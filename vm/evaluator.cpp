/*
 * The scan evaluator
 */

#include "scan.h"

namespace scan {


  /**************************************************************
   * SUBR
   *
   * Subrs are the basic building block of a SIOD program. They
   * represent executable code in compiled C. */

  const _TCHAR *subr_kind_str(subr_arity_t n)
  {
    switch (n)
      {
      case SUBR_0:     return _T("subr-0");
      case SUBR_1:     return _T("subr-1");
      case SUBR_2:     return _T("subr-2");
      case SUBR_2N:    return _T("subr-2n");
      case SUBR_3:     return _T("subr-3");
      case SUBR_4:     return _T("subr-4");
      case SUBR_5:     return _T("subr-5");
      case SUBR_6:     return _T("subr-6");
      case SUBR_N:     return _T("subr-n");
      case SUBR_ARGC:  return _T("subr-argc");
      case SUBR_F:     return _T("subr-f");
      case SUBR_MACRO: return _T("subr-macro");
      default:         return _T("???");
      }
  }

  LRef lsubr_kind(LRef subr)
  {
    if (!SUBRP(subr))
      vmerror_wrong_type(1, subr);

    return keyword_intern(subr_kind_str(SUBR_TYPE(subr)));
  }

  LRef lsubr_name(LRef subr)
  {
    assert(SUBRP(subr) || NULLP(subr));

    LRef name_cell = NIL;

    if (SUBRP(subr))
      name_cell = lassq(interp.sym_name, SUBR_PROPERTY_LIST(subr));

    if (CONSP(name_cell))
      return lcdr(name_cell);
    else
      return strcons(_T("<unknown>"));
  }

  LRef subrcons(subr_arity_t type, LRef name, void *implementation)
  {
    LRef z = new_cell(TC_SUBR);

    SET_SUBR_TYPE(z, type);
    SET_SUBR_PROPERTY_LIST(z, lcons(lcons(interp.sym_name, name), NIL));
    SET_SUBR_CODE(z, implementation);

    return (z);
  }

  /*
   * This group of functions is responsible for registering subrs
   * with the current package
   */

  void register_subr(const _TCHAR *name, subr_arity_t arity, void *implementation)
  {
    if (implementation == NULL)
      dscwritef(";;;; NULL SUBR IMPLEMENTATION: \"~cs\"! (Any attempt to use this subr will fail!)\n", name);

    assert(name != NULL);

    LRef subr = subrcons(arity, strcons(name), implementation);

    lhash_set(SYMBOL_VCELL(interp.sym_subr_table), strcons(name), subr);
  }

  LRef find_subr_by_name(LRef subr_name)
  {
    LRef subr_table = SYMBOL_VCELL(interp.sym_subr_table);

    assert(STRINGP(subr_name));
    assert(HASHP(subr_table)); // REVISIT: Lisp-visible: rebind *subr-table* and invoke the fasl loader

    return lhash_ref(subr_table, subr_name, NIL);
  }

  /**************************************************************
   * Closures
   */


  LRef lclosurecons(LRef env, LRef code, LRef property_list)
  {
    LRef z = new_cell(TC_CLOSURE);

    if (!(CONSP(code) || NULLP(code)))
      vmerror_wrong_type(2, code);

    SET_CLOSURE_ENV(z, env);
    SET_CLOSURE_CODE(z, code);
    SET_CLOSURE_PROPERTY_LIST(z, property_list);

    return z;
  }

  LRef lcompiled_closurecons(LRef env, LRef consts, LRef property_list)
  {
    LRef z = new_cell(TC_COMPILED_CLOSURE);

    if (!(VECTORP(consts)))
      vmerror_wrong_type(2, consts);

    SET_CLOSURE_ENV(z, env);
    SET_CLOSURE_CODE(z, consts);
    SET_CLOSURE_PROPERTY_LIST(z, property_list);

    return z;
  }

  LRef lset_closure_code(LRef exp, LRef code)
  {
    if (!CLOSUREP(exp))
      vmerror_wrong_type(exp);

    SET_CLOSURE_CODE(exp, code);

    return exp;
  }

  LRef lclosure_code(LRef exp)
  {
    if (!CLOSUREP(exp))
      return boolcons(false);
    else
      return (CLOSURE_CODE(exp));
  }

  LRef lset_closure_env(LRef exp, LRef env)
  {
    if (!CLOSUREP(exp))
      vmerror_wrong_type(exp);

    SET_CLOSURE_ENV(exp, env);

    return exp;
  }

  LRef lclosure_env(LRef exp)
  {
    if (!CLOSUREP(exp))
      return boolcons(false);
    else
      return (CLOSURE_ENV(exp));
  }

  LRef lset_property_list(LRef exp, LRef property_list)
  {
    if (CLOSUREP(exp))
      SET_CLOSURE_PROPERTY_LIST(exp, property_list);
    else if (SUBRP(exp))
      SET_SUBR_PROPERTY_LIST(exp, property_list);
    else if (SYMBOLP(exp))
      SET_SYMBOL_PROPS(exp, property_list);
    else
      {
        vmerror_wrong_type(1, exp);
        return NIL; // unreached.
      }

    return property_list;
  }

  LRef lproperty_list(LRef exp)
  {
    if (CLOSUREP(exp))
      return CLOSURE_PROPERTY_LIST(exp);
    else if (SUBRP(exp))
      return SUBR_PROPERTY_LIST(exp);
    else if (SYMBOLP(exp))
      return SYMBOL_PROPS(exp);
    else
      return NIL;
  }

  LRef lprimitivep(LRef obj)
  {
    if (SUBRP(obj))
      return obj;
    else
      return boolcons(false);
  }

  LRef lclosurep(LRef obj)
  {
    if (CLOSUREP(obj))
      return obj;
    else
      return boolcons(false);
  }

  LRef lcompiled_closurep(LRef obj)
  {
    if (COMPILEDP(obj))
      return obj;
    else
      return boolcons(false);
  }

  LRef lprocedurep(LRef exp)
  {
    if (PROCEDUREP(exp))
      return exp;
    else
      return boolcons(false);
  }

  /**************************************************************
   * Frame manager
   *
   * Frames are basically annotations on the dynamic stack. Each
   * frame has an "frame record" stored in an auto variable
   * local to a newly created scope. When the frame is entered,
   * the frame's frame record is registered on a global stack.
   * When the frame is left, the frame record is popped off of
   * the stack.
   */



  /* __frame_set_top(f)
   *
   * Sets the top frame of the frame stack.
   *
   * Parameters:
   *   f - New top of the frame stack.
   *
   * Return value:
   *
   * Notes:
   *   The passed in frame is expected to be somewhere on the
   *   current frame stack.
   */
  void __frame_set_top(frame_record_t *f)
  {
#ifdef _DEBUG
    frame_record_t *loc = thread.frame_stack;

    while(loc)
      {
        if (loc == f) break;
        loc = loc->previous;
      }

    assert(loc); // The frame ought to be on the stack already.
#endif

    thread.frame_stack = f;
  }

  /* __frame_find(pred, info)
   *
   * Finds a frame satisfying the inpassed predicate.
   *
   * Parameters:
   *   pred - A C predicate function.
   *   info - A uptr passed, along with the current frame, into the
   *    predicate function.
   *
   * Return value:
   *   A pointer to the first (topmost) frame satisfying the predicate.
   *   NULL, if none found.
   */
  frame_record_t *__frame_find(frame_predicate pred, uptr info)
  {
    frame_record_t *loc = thread.frame_stack;

    while(loc)
      {
        if (pred(loc, info))
          return loc;

        loc = loc->previous;
      }

    return loc;
  }

  LRef ldebug_backtrace()
  {
    get_current_frames(0, CURRENT_DEBUG_PORT);

    return NIL;
  }

  LRef get_current_frames(fixnum_t skip_count, LRef dump_to_port_while_gathering)
  {
    LRef frame_obj;
    LRef source_loc;
    LRef l = NIL;
    frame_record_t *loc = TOP_FRAME;
    fixnum_t frame_count = 0;



    while(loc)
      {
        source_loc = lcons(strcons(loc->filename), fixcons(loc->line));

        frame_count++;

        bool printing = (frame_count >= skip_count) && !NULLP(dump_to_port_while_gathering);

        switch(loc->type)
          {
          case FRAME_EVAL:
            frame_obj   = listn(4, keyword_intern(_T("eval")),
                                source_loc,
                                loc->frame_as.eval.expr,
                                loc->frame_as.eval.env);

            if (printing)
              scwritef(_T("eval > ~s\n"), dump_to_port_while_gathering,
                       loc->frame_as.eval.expr);
            break;

          case FRAME_EX_TRY:
            frame_obj   = listn(3, keyword_intern(_T("dynamic-escape-try")),
                                source_loc,
                                loc->frame_as.dynamic_escape.tag);

            if (printing)
              scwritef(_T("try > ~s\n"), dump_to_port_while_gathering,
                       loc->frame_as.dynamic_escape.tag);

            break;

          case FRAME_EX_GUARD:
            frame_obj   = listn(2, keyword_intern(_T("dynamic-escape-guard")),
                                source_loc);
            break;

          case FRAME_EX_UNWIND:
            frame_obj   = listn(2, keyword_intern(_T("dynamic-escape-unwind-protect")),
                                source_loc);
            break;

          case FRAME_PRIMITIVE:
            frame_obj   = listn(3, keyword_intern(_T("primitive")),
                                source_loc,
                                loc->frame_as.primitive.function);

            if (printing)
              scwritef(_T("primitive > ~s\n"), dump_to_port_while_gathering,
                       loc->frame_as.primitive.function);

            break;

          default:
            frame_obj = keyword_intern(_T("invalid"));
            break;
          }

        if (frame_count >= skip_count)
          l = lcons(frame_obj, l);

        loc = loc->previous;
      }

    return l;
  }

  LRef lget_current_frames(LRef skip_count)
  {
    return get_current_frames(get_c_fixnum(skip_count), NIL);
  }

  /**************************************************************
   * Stack limit checking
   */

  LRef lset_stack_limit(LRef amount)
  {
    size_t new_size_limit = 0;

    if (!NULLP(amount) && !FALSEP(amount))
      new_size_limit = get_c_long(amount);

    void *new_limit_obj = sys_set_thread_stack_limit(new_size_limit);

    if (!new_size_limit)
      {
        info("stack limit disabled!");
        return boolcons(false);
      }

    info("stack_size = ~cd bytes, [~c&,~c&]\n",
         new_size_limit,
         new_limit_obj,
         sys_get_stack_start());

    return fixcons(new_size_limit);
  }

  LRef lset_interrupt_mask(LRef new_mask)
  {
    if (!BOOLP(new_mask))
      vmerror_wrong_type(1, new_mask);

    bool previous_mask = interp.interrupts_masked;

    interp.interrupts_masked = BOOLV(new_mask);

    return boolcons(previous_mask);
  }

  /**************************************************************
   * The Evaluator
   */

  LRef arg_list_from_buffer(size_t argc, LRef argv[]) {
    LRef result = NIL;

    for(size_t ii = argc; ii > 0; ii--)
        result = lcons(argv[ii - 1], result);

    interp.gc_total_environment_cells_allocated += argc;

    return result;
  }

  size_t evaluate_arguments_to_buffer(LRef l, LRef env, size_t max_argc, LRef argv[])
  {
    size_t argc = 0;
    LRef args = l;

    while(CONSP(args))
      {
        if (argc >= max_argc)
          {
            vmerror("too many actual arguments: ~s", l);
            break;
          }

        argv[argc] = leval(CAR(args), env);

        args = CDR(args);
        argc++;
      }

    if (!NULLP(args))
      vmerror("bad syntax argument list: ~s", l);

    return argc;
  }

  LRef evaluate_arguments(LRef l, LRef env)
  {
    if (NULLP (l))
      return (NIL);

    LRef argv[ARG_BUF_LEN];

    size_t argc = evaluate_arguments_to_buffer(l, env, ARG_BUF_LEN, argv);

    return arg_list_from_buffer(argc, argv);
  }

  static LRef extend_env (LRef actuals, LRef formals, LRef env)
  {
    if (SYMBOLP (formals))
      {
        interp.gc_total_environment_cells_allocated += 4;
        return lcons(lcons(lcons(formals, NIL), lcons(actuals, NIL)), env);
      }
    else
      {
        interp.gc_total_environment_cells_allocated += 2;
        return lcons(lcons(formals, actuals), env);
      }
  }

#define ENVLOOKUP_TRICK 1

  LRef lenvlookup (LRef var, LRef env)
  {
    LRef frame, al, fl, tmp;

#ifdef ENVLOOKUP_STATS
    interp.total_env_lookups++;
#endif

    for (frame = env; CONSP (frame); frame = CDR (frame)) {

#ifdef ENVLOOKUP_STATS
      interp.env_lookup_frames++;
#endif

      tmp = CAR (frame);

      if (!CONSP (tmp))
        vmerror("damaged frame", tmp);

      for (fl = CAR (tmp), al = CDR (tmp); CONSP (fl);
           fl = CDR (fl), al = CDR (al))
        {
          if (!CONSP (al))
            vmerror("too few arguments", tmp);

          if (EQ (CAR (fl), var))
            return (al);
        }
      /* suggested by a user. It works for reference (although conses)
         but doesn't allow for set! to work properly... */
#if (ENVLOOKUP_TRICK)
      if (SYMBOLP (fl) && EQ (fl, var))
        {
          return lcons(al, NIL);
        }
#endif
    }
    if (!NULLP (frame))
      vmerror("damaged env", env);

#ifdef ENVLOOKUP_STATS
    interp.global_env_lookups++;
#endif

    return (NIL);
  }

#ifdef ENVLOOKUP_STATS
  LRef lshow_env_lookup_stats()
  {
    LRef obj = hashcons(true);

    lhash_set(obj, keyword_intern(_T("total-lookups")), fixcons(interp.total_env_lookups));
    lhash_set(obj, keyword_intern(_T("global-lookups")), fixcons(interp.global_env_lookups));
    lhash_set(obj, keyword_intern(_T("lookup-frames")), fixcons(interp.env_lookup_frames));

    return obj;
  }
#endif

  LRef arglchk (LRef x)
  {
#if (!ENVLOOKUP_TRICK)
    LRef l;

    if (SYMBOLP (x))
      return (x);

    for (l = x; CONSP (l); l = CDR (l)) ;

    if (!NULLP (l))
      error("improper formal argument list", x);

#endif
    return (x);
  }


  void signal_break()
  {
    interp.break_pending = true;
  }

  void signal_timer()
  {
    interp.timer_event_pending = true;
  }


  /**************************************************************
   * leval(x, env, eval_option)
   *
   * Evaluates a Scheme form.
   *
   * Parameters:
   *   form - The form to be evaluated.
   *   env - The environment in which the form will be evaluated
   *
   * Options:
   *
   * Return value:
   *   The result of the evaluation
   *
   * Implementation notes:
   *   A basic idiom of this evaluator is that all the object-specific
   *   callbacks return a bool value. The value returned is TRUE in
   *   the case that the invoked object performed a tail-call, and needs
   *   to avoid returning.
   */

  static void process_break_event()
  {
    interp.break_pending = false;

    if (!CLOSUREP(CURRENT_USER_BREAK_HANDLER))
      panic("Bad user break handler");

    LRef val = NIL;
    LRef tag = NIL;

    if (call_lisp_procedure(CURRENT_USER_BREAK_HANDLER, &val, &tag, 0))
      {
        THROW_ESCAPE(tag, val);
      }
  }

  static void process_timer_event()
  {
    interp.timer_event_pending = false;

    if (!CLOSUREP(CURRENT_TIMER_EVENT_HANDLER))
      panic("Bad timer event handler");

    if (call_lisp_procedure(CURRENT_TIMER_EVENT_HANDLER, NULL, NULL, 0))
      panic(_T("Error evaluating timer event handler"));
  }

  /* REVISIT interrupt processing rewrite
   *
   * There are a number of things that need to be done here:
   *
   * 1. Verify that performance impact of processing interrupts in an
   *    inline function isn't too terrible.
   * 2. Allow selective masking of individual interrupts.
   * 3. Switch both the interrupt flags and the mask to a usys bitmap,
   *    one bit per interrupt.
   * 4. Confirm that it really makes sense to process interrupts seperately
   *    from the normal signal handling mechanism.
   */
  INLINE void _process_interrupts()
  {
    if (interp.interrupts_masked)
      return;

    if (interp.break_pending)
      process_break_event();

    if (interp.timer_event_pending)
      process_timer_event();
  }

  void process_interrupts() { _process_interrupts(); }


  LRef vmerror_unbound(LRef v)
  {
    return vmerror("unbound variable: ~s", v);
  }

#define _ARGV(index) ((index >= argc) ? NIL : argv[index])

  INLINE LRef subr_apply(LRef function, size_t argc, LRef argv[], LRef *env, LRef *retval)
  {
    UNREFERENCED(env);

    LRef arg1 = NIL;
    LRef args = NIL;

    ENTER_PRIMITIVE_FRAME(function)
      {
        switch(SUBR_TYPE(function))
          {
          case SUBR_0:
            *retval = (SUBR_F0(function)());
            break;

          case SUBR_1:
            *retval = (SUBR_F1(function)(_ARGV(0)));
            break;

          case SUBR_2:
            *retval = (SUBR_F2(function)(_ARGV(0), _ARGV(1)));
            break;

          case SUBR_3:
            *retval = (SUBR_F3(function)(_ARGV(0), _ARGV(1), _ARGV(2)));
            break;

          case SUBR_4:
            *retval = (SUBR_F4(function)(_ARGV(0), _ARGV(1), _ARGV(2), _ARGV(3)));
            break;

          case SUBR_5:
            *retval = (SUBR_F5(function)(_ARGV(0), _ARGV(1), _ARGV(2), _ARGV(3), _ARGV(4)));
            break;

          case SUBR_6:
            *retval = (SUBR_F6(function)(_ARGV(0), _ARGV(1), _ARGV(2), _ARGV(3), _ARGV(4), _ARGV(5)));
            break;

          case SUBR_2N:
            arg1 = _ARGV(0);

            arg1 = SUBR_F2(function) (arg1, _ARGV(1));

            for (size_t ii = 2; ii < argc; ii++)
              arg1 = SUBR_F2(function)(arg1, leval(_ARGV(ii), *env));

            *retval = arg1;
            break;

          case SUBR_ARGC:
            *retval = (SUBR_FARGC(function)(argc, argv));
            break;

          case SUBR_N:
            args = arg_list_from_buffer(argc, argv);
            *retval = (SUBR_F1(function)(args));
            break;

          case SUBR_F:
          case SUBR_MACRO:
            vmerror("Can't apply special forms", function);
          }
      } LEAVE_FRAME();


    return NIL;
  }


  INLINE LRef closure_apply(LRef function, LRef args, LRef *env, LRef *retval)
  {
    UNREFERENCED(retval);

    LRef c_code = CLOSURE_CODE(function);
    LRef c_env  = CLOSURE_ENV(function);


    if (NULLP(c_code)) // true for uncompiled functions
      {
        if (!CLOSUREP(CURRENT_UNCOMPILED_FUNCTION_HANDLER))
          vmerror("Cannot evaluate function, bad *uncompiled-function-handler*",
                  CURRENT_UNCOMPILED_FUNCTION_HANDLER);

        LRef compiled_code  = NIL;

        if(call_lisp_procedure(CURRENT_UNCOMPILED_FUNCTION_HANDLER, &compiled_code, NULL,
                               1, function))
          panic(_T("Error compiling code at runtime"));

        SET_CLOSURE_CODE(function, compiled_code);
        c_code                 = compiled_code;
      }

    assert(CONSP(c_code));

    *env = extend_env(args, CAR(c_code), c_env);

    return CDR(c_code); // tail call
  }

  INLINE LRef apply(LRef function, size_t argc, LRef argv[], LRef *env, LRef *retval)
  {
    typecode_t type = TYPE(function);

    // NIL signals "no tail recursion", what happens when the actual form is NIL?

    if (type == TC_SUBR)
      return subr_apply(function, argc, argv, env, retval);

    LRef args = arg_list_from_buffer(argc, argv);

    if (type == TC_CLOSURE)
      return closure_apply(function, args, env, retval);
    else if (!NULLP(CURRENT_BAD_APPLY_HANDLER))
      {
        if (!CLOSUREP(CURRENT_BAD_APPLY_HANDLER))
          vmerror("Invalid bad-apply handler", CURRENT_BAD_APPLY_HANDLER);

        if (call_lisp_procedure(CURRENT_BAD_APPLY_HANDLER, retval, NULL, 2, function, args))
          panic(_T("Error evaluating bad apply handler"));

        return NIL;
      }
    else
      vmerror("Cannot apply: ~s", function);

    return NIL; // avoid a warning, since the error case returns nothing.
  }

  LRef leval(LRef form, LRef env)
  {
    typecode_t type;
    LRef retval = NIL;
    LRef args = NIL;
    LRef function = NIL;

    STACK_CHECK(&form);

#ifdef _DEBUG
    ENTER_EVAL_FRAME(form, env)
#endif
      {
      loop:
        interp.forms_evaluated++;

        _process_interrupts();

        type = TYPE(form);

        if (type == TC_FAST_OP)
          {
            LRef sym = FAST_OP_ARG1(form);
            LRef val;

            LRef global_binding;

            switch (FAST_OP_OPCODE(form))
              {
              case FOP_GLOBAL_REF:
                assert(SYMBOLP(sym));
                global_binding = SYMBOL_VCELL(sym);

                if (UNBOUND_MARKER_P(global_binding))
                  vmerror_unbound(sym);

                retval = global_binding;
                break;

              case FOP_GLOBAL_SET:
                assert(SYMBOLP(sym));
                assert(SYMBOL_HOME(sym) != interp.keyword_package);

                val = leval(FAST_OP_ARG2(form), env);

                SET_SYMBOL_VCELL(sym, val);
                retval = val;
                break;

              default: vmerror("Unsupported fast-op: ~s", form);
              }
          }
        else if (type == TC_SYMBOL)
          {
            LRef local_binding = lenvlookup(form, env);

            if(NULLP(local_binding))
              retval = SYMBOL_VCELL(form);
            else
              retval = CAR(local_binding);

            if (UNBOUND_MARKER_P(retval))
              vmerror_unbound(form);
          }
        else if (type == TC_CONS)
          {
            // Split up the form into function and arguments
            function = leval(CAR(form), env);
            args = CDR(form);

            if (SUBRP(function) && (SUBR_TYPE(function) == SUBR_F))
              {
                retval = (SUBR_FF(function)(args, env));
              }
            else if (SUBRP(function) && (SUBR_TYPE(function) == SUBR_MACRO))
              {
                if (NULLP(SUBR_FM (function) (&form, &env)))
                  retval = form;
                else
                  goto loop;
              }
            else
              {
                LRef argv[ARG_BUF_LEN];
                size_t argc = evaluate_arguments_to_buffer(lcdr(form), env, ARG_BUF_LEN, argv);

                form = apply(function, argc, argv, &env, &retval);

                if  (!NULLP(form))
                  goto loop;
              }
          }
        else
          retval = form;
      }
#ifdef _DEBUG
    LEAVE_FRAME();
#endif

    return retval;
  }

  // REVISIT: lapply should be tail recursive
  LRef lapply(size_t argc, LRef argv[])
  {
    size_t fn_argc = 0;
    LRef fn_argv[ARG_BUF_LEN];

    if (argc == 0)
      vmerror("apply requires a function to apply.", NIL);

    LRef fn = argv[0];

    for(size_t ii = 1; ii < argc - 1; ii++)
      {
        if (fn_argc >= ARG_BUF_LEN)
          break;

        fn_argv[fn_argc] = argv[ii];
        fn_argc++;
      }

    LRef args = (argc > 1) ? argv[argc - 1] : NIL;
    while(CONSP(args))
      {
        if (fn_argc >= ARG_BUF_LEN)
          break;

        fn_argv[fn_argc] = CAR(args);
        fn_argc++;

        args = CDR(args);
      }

    if (!NULLP(args))
      vmerror("bad argument list in call to apply: ~s", lcons(fn, args));

    if (fn_argc >= ARG_BUF_LEN)
      vmerror("too many arguments in call to apply: ~s", lcons(fn, NIL));

    LRef retval = NIL;

    STACK_CHECK (&args);

    LRef env = NIL;
    LRef next_form = apply(fn, fn_argc, fn_argv, &env, &retval);

    if (NULLP(next_form))
      return retval;
    else
      return leval(next_form, env);
  }

  LRef lunbind_symbol(LRef var)
  {
    if (!SYMBOLP (var))
      vmerror_wrong_type(1, var);

    SET_SYMBOL_VCELL(var, UNBOUND_MARKER);

    return NIL;
  }

  LRef lsetvar (LRef var, LRef val, LRef lenv, LRef genv)
  {
    LRef tmp;

    if (!SYMBOLP (var))
      vmerror_wrong_type(1, var);

    tmp = lenvlookup (var, lenv);

    if (NULLP(tmp)) {
      LRef old_genv = interp.global_env;

      if (TRUEP(genv) && !NULLP(genv))
        set_global_env(genv);

      if (UNBOUND_MARKER_P(SYMBOL_VCELL(var)))
        vmerror("undefined symbol: ~s", var);

      if (SYMBOL_HOME(var) == interp.keyword_package)
        vmerror("Cannot rebind keywords: ~s", var);

      SET_SYMBOL_VCELL(var, val);

      interp.global_env = old_genv;

      return val;
    }

    SET_CAR(tmp, val);
    return val;
  }

  LRef napply(LRef closure, size_t argc, ...)
  {
    va_list args;

    LRef argv[ARG_BUF_LEN];

    argv[0] = closure;

    va_start(args, argc);

    for(size_t ii = 0; ii < argc; ii++)
      {
        assert(ii + 1 < ARG_BUF_LEN);

        argv[ii + 1] = va_arg(args, LRef);
      }

    va_end(args);

    return lapply(argc + 1, argv);
  }

  bool call_lisp_procedurev(LRef closure, LRef *out_retval, LRef *out_escape_tag, LRef leading_args, size_t n, va_list args)
  {
    LRef dummy_form = leading_args;

    if (!CLOSUREP(closure))
      vmerror_wrong_type(closure);

    if (n)
      {
        LRef trailing_args = make_list(n, NIL);

        LRef loc = trailing_args;
        for (size_t jj = 0; jj < n; ++jj)
          {
            lsetcar (loc, va_arg(args, LRef));
            loc = CDR(loc);
          }


        LRef argv[2];
        argv[0] = dummy_form;
        argv[1] = trailing_args;

        dummy_form = lappend(2, argv);
      }

    bool failed = true;

    LRef retval = NIL;

    ENTER_TRY(NULL)
      {
        retval = napply(closure, 1, dummy_form);
        failed = false;
      }
    ON_ERROR()
      {
        retval = ERROR_RETVAL();
        if (out_escape_tag) *out_escape_tag = ERROR_TAG();
      }
    LEAVE_GUARD();


    if (out_retval)     *out_retval     = retval;

    return failed;
  }

  bool call_lisp_procedure(LRef closure, LRef *out_retval, LRef *out_escape_tag, size_t n, ...)
  {
    va_list args;

    va_start(args, n);

    bool failed = call_lisp_procedurev(closure, out_retval, out_escape_tag, NIL, n, args);

    va_end(args);

    return failed;
  }

  /**************************************************************
   * The evaluator inner functions
   *
   * These implement particular special forms within the evaluator
   */

  LRef lsetq (LRef args, LRef env)
  {
    LRef retval = NIL;

    while (!NULLP(args))
      {
        LRef var = lcar(args);

        if (NULLP(lcdr(args)))
          vmerror("Missing value form", args);

        LRef val = leval(lcar(lcdr(args)), env);

        retval = lsetvar(var, val, env, NIL);

        args = lcdr(lcdr(args));
      };

    return retval;
  }


  static void check_global_environment_size()
  {
    if (interp.last_global_env_entry >= VECTOR_DIM(interp.global_env))
      interp.global_env =
        vector_reallocate_in_place(interp.global_env,
                                   VECTOR_DIM(interp.global_env) + GLOBAL_ENV_BLOCK_SIZE,
                                   UNBOUND_MARKER);
  }

  static void extend_global_environment(LRef sym)
  {
    assert(SYMBOLP(sym));
    assert(SYMBOL_INDEX(sym) == 0);

    interp.last_global_env_entry++;

    check_global_environment_size();

    SET_SYMBOL_INDEX(sym, interp.last_global_env_entry);
  }

  LRef lidefine_global(LRef var, LRef val, LRef genv)
  {
    assert(SYMBOLP(var));

    LRef old_genv = interp.global_env;

    if (TRUEP(genv) && !NULLP(genv))
      set_global_env(genv);

    dscwritef(DF_SHOW_GLOBAL_DEFINES,
              _T("; DEBUG: globally defining ~a in ~c& ~s\n"),
              var, interp.global_env,
              VECTOR_ELEM(interp.global_env, 0));

    if (SYMBOL_INDEX(var) == 0)
      extend_global_environment(var);

    SET_SYMBOL_VCELL(var, val);

    if (!NULLP(interp.sym_global_define_hook)
        && !NULLP(CURRENT_GLOBAL_DEFINE_HOOK))
      {
        if (!CLOSUREP(CURRENT_GLOBAL_DEFINE_HOOK))
          panic("Bad *global-define-hook*");

        if(call_lisp_procedure(CURRENT_GLOBAL_DEFINE_HOOK, NULL, NULL, 2, var, val))
          panic(_T("Error in *global-define-hook*"));
      }

    interp.global_env = old_genv;

    return val;
  }

  LRef lidefine(LRef args, LRef env)
  {
    LRef tmp, var, val;

    var = lcar(args);

    if (!SYMBOLP(var))
      vmerror_wrong_type(var);

    val = leval(lcar(lcdr(args)), env);

    // Overwrite the variable, if defined in the lexical environment
    tmp = lenvlookup (var, env);

    if (!NULLP(tmp))
      {
        SET_CAR(tmp, val);
        return val;
      }

    // If we don't have a lexical environment, define the variable globally.
    if (NULLP (env))
      return lidefine_global(var, val, NIL);

    dscwritef(DF_SHOW_LOCAL_DEFINES, _T("; DEBUG: locally defining ~a\n"), var);

    // If we do have a lexical environment add a new binding.
    tmp = lcar(env);
    lsetcar(tmp, lcons(var, lcar(tmp)));
    lsetcdr(tmp, lcons(val, lcdr(tmp)));

    return (val);

    //return lidefine_global(var, val, NIL);
  }

  LRef ldeclare(LRef args, LRef env)
  {
    LRef procedure_name = lcar(args);
    args = lcdr(args);

    LRef procedure = NIL;

    if (SYMBOLP(procedure_name))
      {
        LRef env_frame = lenvlookup(procedure_name, env);

        if (NULLP(env_frame))
          procedure = SYMBOL_VCELL(procedure_name);
        else
          {
            if (UNBOUND_MARKER_P(CAR(env_frame)))
              vmerror_unbound(procedure_name);

            procedure = lcar(env_frame);
          }
      }
    else
      procedure = procedure_name;

    if (!CLOSUREP(procedure) && !SUBRP(procedure))
      vmerror(_T("Argument 1 of a declaration must be a symbol naming a procedure or a procedure."), procedure_name);


    lset_property_list(procedure,
                       lcons(lcons(interp.sym_declare, args),
                             lproperty_list(procedure)));

    return boolcons(false);
  }

  LRef lif (LRef * pform, LRef * penv)
  {
    LRef args, env;
    args = lcdr(*pform);
    env = *penv;

    if (TRUEP(leval(lcar(args), env)))
      *pform = lcar(lcdr (args));
    else
      *pform = lcar(lcdr(lcdr(args)));

    return boolcons(true);
  }

  LRef lilambda(LRef args, LRef env)
  {
    LRef properties = lcar(args);

    args = lcdr(args);

    LRef body_forms = lcdr(args);

    // If no body or args have been specified, create an 'uncompiled' lambda.
    if (NULLP(body_forms))
      return lclosurecons(env, NIL, properties);

    LRef body;

    // lambda code can be composed of exactly one form. If there are
    // multiple forms, then they're wrapped in a (begin ...)
    if (NULLP(lcdr(body_forms)))
      body = lcar(body_forms);
    else
      body = lcons(interp.sym_progn, body_forms);

    LRef new_closure = lclosurecons(env, lcons(arglchk(lcar(args)), body), properties);

    return new_closure;
  }

  LRef lor (LRef * pform, LRef * penv)
  {
    LRef val;

    LRef env = *penv;
    LRef l = lcdr(*pform);
    LRef next = lcdr(l);


    if (NULLP(l))
      {
        *pform = boolcons(false);
        return (NIL);
      }

    while (!NULLP (next))
      {
        val = leval(lcar(l), env);
        if (TRUEP (val))
          {
            *pform = val;
            return NIL;
          }
        l = next;
        next = lcdr(next);
      }

    *pform = lcar(l);
    return boolcons(true);
  }

  LRef land (LRef * pform, LRef * penv)
  {
    LRef env, l, next;

    env = *penv;
    l = lcdr(*pform);
    if (NULLP (l))
      {
        *pform = boolcons(true);
        return (NIL);
      }
    next = lcdr(l);
    while (!NULLP (next))
      {
        if (!TRUEP(leval(lcar (l), env)))
          {
            *pform = boolcons(false);
            return (NIL);
          }
        l = next;
        next = lcdr(next);
      }
    *pform = lcar(l);
    return boolcons(true);
  }

  LRef lquote (LRef args, LRef env)
  {
    UNREFERENCED(env);

    return lcar(args);
  }

  LRef lthe_environment (LRef args, LRef env)
  {
    UNREFERENCED(args);

    return env;
  }

  LRef lwhile (LRef form, LRef env)
  {
    LRef l;

    while (TRUEP(leval(lcar(form), env)))
      for (l = lcdr(form); !NULLP(l); l = lcdr(l))
        leval(lcar(l), env);

    return NIL;
  }

  LRef lrepeat (LRef form, LRef env)
  {
    LRef limit = leval(lcar(form), env);

    if (!NUMBERP(limit))
      vmerror("Expected number for repeat limit", NIL);

    fixnum_t lim = get_c_fixnum(limit);

    while (lim > 0)
      {
        for (LRef l = lcdr(form); !NULLP(l); l = lcdr(l))
          leval(lcar(l), env);

        lim--;
      }

    return (NIL);
  }

  LRef lprogn (LRef * pform, LRef * penv)
  {
    LRef env, l, next;

    env = *penv;
    l = lcdr(*pform);
    next = lcdr(l);
    while (!NULLP (next))
      {
        leval(lcar(l), env);
        l = next;
        next = lcdr(next);
      }
    *pform = lcar(l);
    return boolcons(true);
  }

  LRef ltime (LRef args, LRef env)
  {
    fixnum_t cells      = interp.gc_total_cells_allocated;
    fixnum_t env_cells  = interp.gc_total_environment_cells_allocated;
    fixnum_t c_blocks   = malloc_blocks;
    fixnum_t c_bytes    = malloc_bytes;
    flonum_t t          = sys_runtime();
    flonum_t gc_t       = interp.gc_total_run_time;
    size_t forms        = interp.forms_evaluated;


    LRef retval = NIL;
    for (LRef l = args; !NULLP(l); l = lcdr(l))
      retval = leval(lcar(l), env);

    LRef argv[8];

    argv[0] = retval;
    argv[1] = flocons(sys_runtime() - t);
    argv[2] = flocons(interp.gc_total_run_time - gc_t);
    argv[3] = fixcons(interp.gc_total_cells_allocated - cells);
    argv[4] = fixcons(interp.gc_total_environment_cells_allocated - env_cells);
    argv[5] = fixcons(malloc_blocks - c_blocks);
    argv[6] = fixcons(malloc_bytes - c_bytes);
    argv[7] = fixcons(interp.forms_evaluated - forms);

    return lvector(8, argv);
  }


  LRef lcond (LRef * pform, LRef * penv)
  {
    LRef args, env, clause, value, next;

    args = lcdr(*pform);
    env = *penv;

    /* If we don't have any arguments, we just return NIL */
    if (NULLP (args))
      {
        *pform = NIL;
        return (NIL);
      }

    /* Now that we know we have a list of clauses, we can iterate
     * through each and determine which is the one we'll run.
     *
     * Each clause is of the form:
     *
     * ( <predicate> . ( <form> ... ) )
     */
    next = lcdr(args);
    while (!NULLP(next))
      {
        /* Extract out an individual clause */
        clause = lcar(args);

        /* Evaluate it, and check to see if it is TRUE */
        value = leval(lcar(clause), env);

        if (TRUEP(value))
          {
            /* Check to see if there are any forms to evaluate in
             * this clause... */
            clause = lcdr(clause);
            if (NULLP (clause))
              {
                *pform = value;
                return (NIL);
              }
            else
              {
                /* ... If there are forms, evaluate them all in sequence */
                next = lcdr(clause);
                while (!NULLP (next))
                  {
                    leval(lcar(clause), env);
                    clause = next;
                    next = lcdr(next);
                  }
                *pform = lcar(clause);
                return boolcons(true);
              }
          }

        args = next;
        next = lcdr(next);
      }

    clause = lcar(args);
    next = lcdr(clause);
    if (NULLP (next))
      {
        *pform = lcar(clause);
        return boolcons(true);
      }
    value = leval(lcar(clause), env);
    if (!TRUEP(value))
      {
        *pform = NIL;
        return NIL;
      }
    clause = next;
    next = lcdr(next);
    while (!NULLP (next))
      {
        leval(lcar(clause), env);
        clause = next;
        next = lcdr(next);
      }
    *pform = lcar(clause);

    return boolcons(true);
  }

  /* (case value ((...values...) . code)
   *              ...
   *              #t . code))
   */
  LRef lcase (LRef * pform, LRef * penv)
  {
    LRef form = lcdr(*pform);
    LRef env  = *penv;

    if (NULLP(form))
      {
        *pform = NIL;
        return NIL;
      }

    if (!CONSP(form))
      vmerror("Invalid form, incorrect test value", *pform);

    LRef testValue = leval(lcar(form), env);
    LRef clauses   = lcdr(form);

    if (!(CONSP(clauses) || NULLP(clauses)))
      vmerror("Invalid form, incorrect clauses", clauses);

    LRef runClause = NIL;

    for(; CONSP(clauses); clauses = CDR(clauses))
      {
        LRef clause = CAR(clauses);

        if (!CONSP(clause))
          vmerror("Invalid form, invalid clause", clause);

        if (CONSP(CAR(clause)))
          {
            for(LRef loc = CAR(clause); CONSP(loc); loc = CDR(loc))
              {
                if (EQ(CAR(loc), testValue))
                  {
                    runClause = CDR(clause);
                    break;
                  }
              }
          }
        else if (BOOLP(CAR(clause)) && TRUEP(CAR(clause)))
          {
            runClause = lcdr(clause);
          }
        else
          vmerror("Invalid form, invalid guard", CAR(clause));

        if (!NULLP(runClause))
          break;
      }

    if (!NULLP(runClause))
      {
        while(!NULLP(lcdr(runClause)))
          {
            leval(lcar(runClause), env);

            runClause = lcdr(runClause);
          };
      }

    if (NULLP(runClause))
      {
        *pform = NIL;
        return NIL;
      }
    else
      {
        *pform = CAR(runClause);
        return boolcons(true);
      }
  }

  /* list-let
   *
   * (list_let <vars> <values>
   *       <forms>)
   *
   * Macro function that binds <values> to <vars> on a 1:1 basis
   */
  LRef llist_let (LRef *pform, LRef *penv)
  {
    LRef l      = lcdr(*pform);
    LRef vars   = lcar(l);
    LRef values = lcar(lcdr(l));
    LRef forms  = lcdr(lcdr((l)));
    LRef next;

    values = leval(values, *penv);

    *penv = extend_env(values, vars, *penv);

    next = lcdr(forms);
    while (!NULLP (next))
      {
        leval(lcar(forms), *penv);
        forms = next;
        next = lcdr(next);
      }
    *pform = lcar(forms);

    return boolcons(true);
  }


  /**************************************************************
   * Handler Bindings
   **************************************************************/

  LRef lset_handler_frames(LRef new_frames)
  {
    thread.handler_frames = new_frames;

    return new_frames;
  }

  LRef lhandler_frames()
  {
    return thread.handler_frames;
  }

  /**************************************************************
   * Exception Scheme Bindings
   **************************************************************/

  /* TODO: Refactor implementation of catch/throw:
   *
   * 2) catch should match catch tags on eqv?
   * 3) throw should detect missing catch tag prior to unwinding the stack.
   * 4) catch should not be a special form. (The existing special form can be a macro atop the funcational form.)
   * 5) catch should have optional on-throw thunk that's tail-called if the catch is thrown to.
   */

  LRef lcatch(LRef args, LRef env)
  {
    LRef tag;
    LRef retval = NIL;

    tag = leval(lcar(args), env);

    // tag==#t implies all tags
    if (BOOLP(tag) && TRUEP(tag))
      tag = NULL;

    ENTER_TRY(tag)
      {
        for (LRef l = lcdr(args); !NULLP (l); l = lcdr(l))
          retval = leval(lcar(l), env);
      }
    ON_ERROR()
      {
        dscwritef(DF_SHOW_THROWS,
                  _T("; DEBUG: catch ~a :~a\n"),
                  ERROR_TAG(), ERROR_RETVAL());

        retval = ERROR_RETVAL();
      }
    LEAVE_TRY();

    return retval;
  }

  LRef lunwind_protect(LRef thunk, LRef after)
  {
    LRef rc = NIL;

    if (!CLOSUREP(thunk))
      vmerror_wrong_type(1, thunk);
    if (!CLOSUREP(after))
      vmerror_wrong_type(2, after);

    ENTER_UNWIND_PROTECT()
      {
        rc = napply(thunk, 0);
      }
    ON_UNWIND()
      {
        napply(after, 0);
      }
    LEAVE_UNWIND_PROTECT();


    return rc;
  }


  LRef lthrow (LRef tag, LRef value)
  {
    dscwritef(DF_SHOW_THROWS,
              _T("; DEBUG: throw ~a :~a\n"),
              tag, value);

    THROW_ESCAPE(tag, value);

    return (NIL);
  }



  LRef lfuncall1 (LRef fcn, LRef a1)
  {
    STACK_CHECK (&fcn);

    if ((TYPE(fcn) == TC_SUBR) && (SUBR_TYPE(fcn) == SUBR_1))
      return (SUBR_F1 (fcn) (a1));
    else if (   (TYPE(fcn) == TC_CLOSURE)
                && (SUBRP(CLOSURE_CODE(fcn)))
                && (SUBR_TYPE(CLOSURE_CODE(fcn)) == SUBR_2))
      return (SUBR_F2(CLOSURE_CODE(fcn))(CLOSURE_ENV(fcn), a1));
    else
      {
        return (napply(fcn, 2, a1, NIL));
      }
  }

  LRef lfuncall2 (LRef fcn, LRef a1, LRef a2)
  {
    if (   (TYPE(fcn) == TC_SUBR)
           && (   (SUBR_TYPE(fcn) == SUBR_2)
                  || (SUBR_TYPE(fcn) == SUBR_2N)))
      {
        STACK_CHECK (&fcn);

        return (SUBR_F2 (fcn) (a1, a2));
      }
    else
      {
        return napply(fcn, 3, a1, a2, NIL);
      }
  }


  /**************************************************************
   * Exception Internal Implementation
   *
   * These functions implement the internal exception handling
   * logic. They are intended to be called via the wrapper macros
   * in the header file.
   **************************************************************/


  /**************************************************************
   * __ex_current_catch_retval()
   *
   * Returns the current catch frame's retval.
   *
   * Return value:
   *   The return value of the current stack frame.
   */

  LRef __ex_current_catch_retval()
  {
    assert(TOP_FRAME);

    assert(    (TOP_FRAME->type == FRAME_EX_GUARD)
               || (TOP_FRAME->type == FRAME_EX_TRY)
               || (TOP_FRAME->type == FRAME_EX_UNWIND));

    assert(TOP_FRAME->frame_as.dynamic_escape.pending);

    return TOP_FRAME->frame_as.dynamic_escape.retval;
  }

  LRef __ex_current_catch_tag()
  {
    assert(TOP_FRAME);

    assert(    (TOP_FRAME->type == FRAME_EX_GUARD)
               || (TOP_FRAME->type == FRAME_EX_TRY)
               || (TOP_FRAME->type == FRAME_EX_UNWIND));

    assert(TOP_FRAME->frame_as.dynamic_escape.pending);

    return TOP_FRAME->frame_as.dynamic_escape.tag;
  }

  /**************************************************************
   * __ex_throw_dynamic_escape(tag, retval, already_pending)
   *
   * Searches for the first matching stack frame, popping off
   * stack frames along the way. The matching stack frame
   * is left on the stack, with the return value specified.
   * longjmp is called to restore execution at that execution
   * context:
   *
   * Parameters:
   *   tag - The tag of the exception frame to be thrown to.
   *   retval - The return value for the catch frame
   *   already_pending - True, if the exception is already in the
   *     process of being handled
   *
   * Return value:
   *   None.
   */

  /* These two predicates find the next exception frame in the list
   * of frame handlers.  next_frame_to_catch is used to calculate
   * the next frame that needs to process the current exception, including
   * unwind protection frames that don't have any particular interest in the
   * type of exception being thrown. next_try_frame is used to calculate the next
   * frame that has explicitly requested interest in this kind of exception.
   * This is used to determine if the exception was expected by the programmer.
   */
  bool __ex_matching_frame_1(frame_record_t *rec,
                             uptr tag,
                             bool exclude_unwind_protection)
  {
    if (!exclude_unwind_protection)
      {
        if (rec->type == FRAME_EX_GUARD)
          return TRUE;

        /* If a frame is being unwound, it means that we're executing the
         * unwind clause and any errors thrown belong to an outside exception
         * frame. Therefore it is not a candidate for the current throw. */
        if (   (rec->type == FRAME_EX_UNWIND)
               && !rec->frame_as.dynamic_escape.unwinding)
          return TRUE;
      }

    if (rec->type == FRAME_EX_TRY)
      {
        if (NULLP(rec->frame_as.dynamic_escape.tag))
          return TRUE;
        else
          return EQ(rec->frame_as.dynamic_escape.tag, (LRef)tag);
      }

    return FALSE;
  }

  bool __ex_next_frame_to_catch(frame_record_t *rec, uptr tag)
  {
    return __ex_matching_frame_1(rec, tag, FALSE);
  }

  bool __ex_next_try_frame(frame_record_t *rec, uptr tag)
  {
    return __ex_matching_frame_1(rec, tag, TRUE);
  }


  void __ex_throw_dynamic_escape(LRef tag, LRef retval, bool already_pending)
  {
    UNREFERENCED(already_pending);

    /* Check to see if we have a matching catch block... */
    frame_record_t *next_try = __frame_find(__ex_next_try_frame, (uptr)((LRef)tag));

    /* ...If we do, start unwinding the stack... */
    if (next_try)
      {
        frame_record_t *next_catcher =
          __frame_find(__ex_next_frame_to_catch, (uptr)((LRef)tag));

        next_catcher->frame_as.dynamic_escape.pending   = TRUE;
        next_catcher->frame_as.dynamic_escape.unwinding = TRUE;
        next_catcher->frame_as.dynamic_escape.tag        = tag;
        next_catcher->frame_as.dynamic_escape.retval        = retval;

        __frame_set_top(next_catcher);

        longjmp(next_catcher->frame_as.dynamic_escape.cframe, 1);
      }

    /* ...If we don't, signal the event... */
    vmsignal(_T("uncaught-throw"), 2, tag, retval);

    /* ...if nobody cares, then we start to panic. */
    panic("Uncaught throw!");
  }

  /**************************************************************
   * __ex_rethrow_dynamic_escape
   *
   * Rethrows the exception matching the current catch frame
   */
  void __ex_rethrow_dynamic_escape()
  {
    LRef retval;
    LRef tag;

    assert(TOP_FRAME);
    assert(    (TOP_FRAME->type == FRAME_EX_GUARD)
               || (TOP_FRAME->type == FRAME_EX_TRY)
               || (TOP_FRAME->type == FRAME_EX_UNWIND));
    assert(TOP_FRAME->frame_as.dynamic_escape.pending);

    tag     = TOP_FRAME->frame_as.dynamic_escape.tag;
    retval  = TOP_FRAME->frame_as.dynamic_escape.retval;

    /* Avoid hitting the same exception over and over again... */
    thread.frame_stack = TOP_FRAME;

    __ex_throw_dynamic_escape(tag, retval, TRUE);
  }

  /****************************************************************
   * Values tuples
   */

  LRef lvalues(LRef values)
  {
    LRef z = new_cell(TC_VALUES_TUPLE);

    SET_VALUES_TUPLE_VALUES(z, values);

    return z;
  }

  LRef valuesn(long n, ...)
  {
    va_list args;

    va_start(args, n);

    LRef result = lvalues(listv(n, args));

    va_end(args);

    return result;
  }

  LRef lvalues2list(LRef obj)
  {
    if (VALUES_TUPLE_P(obj))
      return VALUES_TUPLE_VALUES(obj);

    return lcons(obj, NIL);
  }


  LRef lcurrent_global_environment()
  {
    return interp.global_env;
  }


  void set_global_env(LRef genv)
  {
    if (!VECTORP(genv))
      vmerror_wrong_type(genv);

    interp.global_env = genv;
    check_global_environment_size();
  }

  LRef lcall_with_global_environment(LRef fn, LRef new_global_env)
  {
    if(!VECTORP(new_global_env))
      vmerror_wrong_type(new_global_env);

    LRef old_global_env = interp.global_env;
    LRef retval = NIL;

    ENTER_UNWIND_PROTECT() {

      interp.global_env = new_global_env;

      check_global_environment_size();

      retval = napply(fn, 0);

    } ON_UNWIND() {
      interp.global_env = old_global_env;
    } LEAVE_UNWIND_PROTECT();

    return retval;
  }

  LRef fast_op(int opcode, LRef arg1, LRef arg2)
  {
    LRef z = new_cell(TC_FAST_OP);

    SET_FAST_OP_OPCODE(z, opcode);
    SET_FAST_OP_ARG1(z, arg1);
    SET_FAST_OP_ARG2(z, arg2);

    return (z);
  }

  LRef lfast_op(LRef opcode, LRef arg1, LRef arg2)
  {
    if (!FIXNUMP(opcode))
      vmerror_wrong_type(1, opcode);

    return fast_op(FIXNM(opcode), arg1, arg2);
  }

  LRef lfast_op_opcode(LRef fastop)
  {
    if (!FAST_OP_P(fastop))
      vmerror_wrong_type(1, fastop);

    return fixcons(FAST_OP_OPCODE(fastop));
  }

  LRef lfast_op_args(LRef fastop)
  {
    if (!FAST_OP_P(fastop))
      vmerror_wrong_type(1, fastop);

    return listn(2, FAST_OP_ARG1(fastop), FAST_OP_ARG2(fastop));
  }

  bool fast_op_equal(LRef a, LRef b)
  {
    assert(FAST_OP_P(a));
    assert(FAST_OP_P(b));

    if (FAST_OP_OPCODE(a) != FAST_OP_OPCODE(b))
      return false;

    if (FAST_OP_ARG1(a) != FAST_OP_ARG1(b))
      return false;

    if (FAST_OP_ARG2(a) != FAST_OP_ARG2(b))
      return false;

    return true;
  }


} // end namespace scan
