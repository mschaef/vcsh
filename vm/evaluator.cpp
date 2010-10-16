
/*
 * The scan evaluator
 */

#include "scan.h"

BEGIN_NAMESPACE(scan)

/***** subrs *****/

LRef lsubr_type_code(LRef subr)
{
     if (!SUBRP(subr))
          vmerror_wrong_type(1, subr);

     return fixcons(SUBR_TYPE(subr));
}

LRef lsubr_name(LRef subr)
{
     if (!SUBRP(subr))
          vmerror_wrong_type(1, subr);

     return SUBR_NAME(subr);
}

LRef subrcons(subr_arity_t type, LRef name, void *implementation)
{
     LRef z = new_cell(TC_SUBR);

     SET_SUBR_TYPE(z, type);
     SET_SUBR_NAME(z, name);
     SET_SUBR_CODE(z, implementation);

     return (z);
}

/*
 * This group of functions is responsible for registering subrs
 * with the current package
 */

void register_subr(const _TCHAR * name, subr_arity_t arity, void *implementation)
{
     assert(HASHP(interp.subr_table));
     assert(name != NULL);

     if (implementation == NULL)
          dscwritef(";;;; NULL SUBR IMPLEMENTATION: \"~cs\"!\n", name);

     LRef subr_name = strcons(name);

     LRef subr = subrcons(arity, subr_name, implementation);

     lhash_set(interp.subr_table, subr_name, subr);
}

LRef find_subr_by_name(LRef subr_name)
{
     assert(STRINGP(subr_name));
     assert(HASHP(interp.subr_table)); /*  REVISIT: Lisp-visible: rebind *subr-table* and invoke the fasl loader */

     LRef argv[2];
     argv[0] = interp.subr_table;
     argv[1] = subr_name;

     return lhash_ref(2, argv);
}

LRef lisubr_table()
{
     return interp.subr_table;
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
     else if (SYMBOLP(exp))
          SET_SYMBOL_PROPS(exp, property_list);
     else
     {
          vmerror_wrong_type(1, exp);
          return NIL;           /*  unreached. */
     }

     return property_list;
}

LRef lproperty_list(LRef exp)
{
     if (CLOSUREP(exp))
          return CLOSURE_PROPERTY_LIST(exp);
     else if (SUBRP(exp))
          return NIL;
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
void __frame_set_top(frame_record_t * f)
{
#ifdef _DEBUG
     frame_record_t *loc = CURRENT_TIB()->frame_stack;

     while (loc)
     {
          if (loc == f)
               break;
          loc = loc->previous;
     }

     assert(loc);               /*  The frame ought to be on the stack already. */
#endif

     CURRENT_TIB()->frame_stack = f;
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
frame_record_t *__frame_find(frame_predicate pred, uptr_t info)
{
     frame_record_t *loc = CURRENT_TIB()->frame_stack;

     while (loc)
     {
          if (pred(loc, info))
               return loc;

          loc = loc->previous;
     }

     return loc;
}

LRef lget_current_frames(LRef sc)
{
     fixnum_t skip_count = get_c_fixnum(sc);

     LRef frames = NIL;

     fixnum_t frame_count = 0;
     
     for(frame_record_t *loc = TOP_FRAME; loc; loc = loc->previous)
     {
          LRef frame_obj = NIL;

          frame_count++;

          switch (loc->type)
          {
          case FRAME_EVAL:
               frame_obj = listn(3,
                                 *loc->frame_as.eval.form,
                                 loc->frame_as.eval.initial_form,
                                 loc->frame_as.eval.env);
               break;

          case FRAME_EX_TRY:
               frame_obj = listn(1, loc->frame_as.dynamic_escape.tag);
               break;

          case FRAME_EX_UNWIND:
               frame_obj = NIL;
               break;

          case FRAME_PRIMITIVE:
               frame_obj = listn(1, loc->frame_as.primitive.function);
               break;

          case FRAME_MARKER:
               frame_obj = listn(1, loc->frame_as.marker.tag);
               break;

          default:
               panic("invalid frame type.");
               break;
          }

          frame_obj = lcons(fixcons(loc->type), frame_obj);

          if (frame_count >= skip_count)
               frames = lcons(frame_obj, frames);
     }

     return frames;
}

/**************************************************************
 * Stack limit checking
 */

LRef lset_stack_limit(LRef amount)
{
     size_t new_size_limit = 0;

     if (!NULLP(amount) && !FALSEP(amount))
          new_size_limit = get_c_long(amount);

     void *new_limit_obj = sys_set_stack_limit(new_size_limit);

     if (!new_size_limit)
     {
          dscwritef(DF_SHOW_GC, "stack limit disabled!");

          return boolcons(false);
     }

     dscwritef(DF_SHOW_GC, "stack_size = ~cd bytes, [~c&,~c&]\n",
               new_size_limit, new_limit_obj, sys_get_stack_start());

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

/*** Control Fields ***/

static size_t get_control_field_id(LRef control_field_id)
{
     if (!FIXNUMP(control_field_id))
          vmerror_wrong_type(1, control_field_id);

     size_t id = (size_t)FIXNM(control_field_id);

     if ((id < 0) || (id > VMCTRL_LAST))
          vmerror("Invalid control field ID: ~s", control_field_id);

     return id;
}

LRef liset_control_field(LRef control_field_id, LRef new_value)
{
     interp.control_fields[get_control_field_id(control_field_id)] = new_value;

     return new_value;
}

LRef licontrol_field(LRef control_field_id)
{
     return interp.control_fields[get_control_field_id(control_field_id)];
}


/*** Trap handling ***/

static size_t get_trap_id(LRef trap_id)
{
     if (!FIXNUMP(trap_id))
          vmerror_wrong_type(1, trap_id);

     size_t id = (size_t)FIXNM(trap_id);

     if ((id < 0) || (id > TRAP_LAST))
          vmerror("Invalid trap ID: ~s", trap_id);

     return id;
}

LRef liset_trap_handler(LRef trap_id, LRef new_handler)
{
     if (!PROCEDUREP(new_handler))
          vmerror_wrong_type(2, new_handler);

     interp.trap_handlers[get_trap_id(trap_id)] = new_handler;

     return new_handler;
}

LRef litrap_handler(LRef trap_id)
{
     return interp.trap_handlers[get_trap_id(trap_id)];
}

static void vmtrap_panic(trap_type_t trap, const _TCHAR *msg)
{
     _TCHAR buf[STACK_STRBUF_LEN];

     _sntprintf(buf, STACK_STRBUF_LEN, _T("Trap error for %s: %s"), trap_type_name(trap), msg);

     panic(buf);
}

LRef vmtrap(trap_type_t trap, vmt_options_t options, size_t argc, ...)
{
     assert((trap > 0) && (trap <= TRAP_LAST));
     assert(argc < ARG_BUF_LEN);

     dscwritef(DF_SHOW_TRAPS, _T("; DEBUG: trap : ~cS\n"), trap_type_name(trap));
     
     LRef handler = interp.trap_handlers[trap];

     if (!PROCEDUREP(handler))
     {
          if(!NULLP(handler))
               vmtrap_panic(trap, "bad trap handler");

          if (!(options & VMT_OPTIONAL_TRAP))
               vmtrap_panic(trap, "missing trap handler");

          return NIL;
     }

     LRef retval = NIL;
     va_list args;

     va_start(args, argc);

     LRef argv[ARG_BUF_LEN];

     argv[0] = fixcons(trap);
     for (size_t ii = 1; ii < argc + 1; ii++)
          argv[ii] = va_arg(args, LRef);

     va_end(args);

     retval = apply1(handler, argc + 1, argv);

     if (options & VMT_HANDLER_MUST_ESCAPE)
          vmtrap_panic(trap, "trap handler must escape");

     return retval;
}

/**************************************************************
 * The Evaluator
 */

static LRef arg_list_from_buffer(size_t argc, LRef argv[])
{
     LRef result = NIL;

     for (size_t ii = argc; ii > 0; ii--)
          result = lcons(argv[ii - 1], result);

     interp.gc_total_environment_cells_allocated += argc;

     return result;
}

static LRef leval(LRef form, LRef env);

static size_t evaluate_arguments_to_buffer(LRef l, LRef env, size_t max_argc, LRef argv[])
{
     size_t argc = 0;
     LRef args = l;

     while (CONSP(args))
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

static LRef extend_env(LRef actuals, LRef formals, LRef env)
{
     if (SYMBOLP(formals))
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

LRef lenvlookup(LRef var, LRef env)
{
     LRef frame, al, fl, tmp;

     for (frame = env; CONSP(frame); frame = CDR(frame))
     {
          tmp = CAR(frame);

          if (!CONSP(tmp))
               panic("damaged frame");

          for (fl = CAR(tmp), al = CDR(tmp); CONSP(fl); fl = CDR(fl), al = CDR(al))
          {
               if (!CONSP(al))
                    vmerror("too few arguments", tmp);

               if (EQ(CAR(fl), var))
                    return (al);
          }
          /* suggested by a user. It works for reference (although conses)
             but doesn't allow for set! to work properly... */
#if (ENVLOOKUP_TRICK)
          if (SYMBOLP(fl) && EQ(fl, var))
          {
               return lcons(al, NIL);
          }
#endif
     }

     if (!NULLP(frame))
          panic("damaged env");

     return NIL;
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

     vmtrap(TRAP_USER_BREAK, VMT_MANDATORY_TRAP, 0); // REVISIT: really mandatory?
}

static void process_timer_event()
{
     interp.timer_event_pending = false;

     vmtrap(TRAP_TIMER_EVENT, VMT_MANDATORY_TRAP, 0); // REVISIT: really mandatory?
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

void process_interrupts()
{
     _process_interrupts();
}

#define _ARGV(index) ((index >= argc) ? NIL : argv[index])

INLINE LRef subr_apply(LRef function, size_t argc, LRef argv[], LRef * env, LRef * retval)
{
     UNREFERENCED(env);

     LRef arg1 = NIL;
     LRef args = NIL;

     ENTER_PRIMITIVE_FRAME(function)
     {
          switch (SUBR_TYPE(function))
          {
          case SUBR_0:
               *retval = (SUBR_F0(function) ());
               break;

          case SUBR_1:
               *retval = (SUBR_F1(function) (_ARGV(0)));
               break;

          case SUBR_2:
               *retval = (SUBR_F2(function) (_ARGV(0), _ARGV(1)));
               break;

          case SUBR_3:
               *retval = (SUBR_F3(function) (_ARGV(0), _ARGV(1), _ARGV(2)));
               break;

          case SUBR_4:
               *retval = (SUBR_F4(function) (_ARGV(0), _ARGV(1), _ARGV(2), _ARGV(3)));
               break;

          case SUBR_5:
               *retval = (SUBR_F5(function) (_ARGV(0), _ARGV(1), _ARGV(2), _ARGV(3), _ARGV(4)));
               break;

          case SUBR_6:
               *retval =
                   (SUBR_F6(function) (_ARGV(0), _ARGV(1), _ARGV(2), _ARGV(3), _ARGV(4), _ARGV(5)));
               break;

          case SUBR_2N:
               arg1 = _ARGV(0);

               arg1 = SUBR_F2(function) (arg1, _ARGV(1));

               for (size_t ii = 2; ii < argc; ii++)
                    arg1 = SUBR_F2(function) (arg1, _ARGV(ii));

               *retval = arg1;
               break;

          case SUBR_ARGC:
               *retval = (SUBR_FARGC(function) (argc, argv));
               break;

          case SUBR_N:
               args = arg_list_from_buffer(argc, argv);
               *retval = (SUBR_F1(function) (args));
               break;
          }
     }
     LEAVE_FRAME();


     return NIL;
}


INLINE LRef apply(LRef function, size_t argc, LRef argv[], LRef * env, LRef * retval)
{
     typecode_t type = TYPE(function);

     /*  NIL signals "no tail recursion", what happens when the actual form is NIL? */

     if (type == TC_SUBR)
          return subr_apply(function, argc, argv, env, retval);

     if (type == TC_CLOSURE)
     {
          LRef c_code = CLOSURE_CODE(function);

          *env = extend_env(arg_list_from_buffer(argc, argv), CAR(c_code), CLOSURE_ENV(function));

          return CDR(c_code);   /*  tail call */
     }

     vmerror_wrong_type(function);

     return NIL;                /*  avoid a warning, since the error case returns nothing. */
}

static LRef leval(LRef form, LRef env)
{
     LRef retval = NIL;

     STACK_CHECK(&form);

#ifdef _DEBUG
     ENTER_EVAL_FRAME(&form, env)
#endif
   loop:
     interp.forms_evaluated++;

     _process_interrupts();

     checked_assert(TYPE(form) == TC_FAST_OP);

     LRef sym = FAST_OP_ARG1(form);
     LRef val;

     LRef binding;
     size_t argc;
     LRef argv[ARG_BUF_LEN];

     switch (FAST_OP_OPCODE(form))
     {
     case FOP_LITERAL:
          retval = FAST_OP_ARG1(form);
          break;

     case FOP_GLOBAL_REF:
          checked_assert(SYMBOLP(sym));
          checked_assert(SYMBOL_HOME(sym) != interp.keyword_package);

          binding = SYMBOL_VCELL(sym);

          if (UNBOUND_MARKER_P(binding))
               vmerror_unbound(sym);

          retval = binding;
          break;

     case FOP_GLOBAL_SET:
          checked_assert(SYMBOLP(sym));
          checked_assert(SYMBOL_HOME(sym) != interp.keyword_package);

          binding = SYMBOL_VCELL(sym);

          if (UNBOUND_MARKER_P(binding))
               vmerror_unbound(sym);

          val = leval(FAST_OP_ARG2(form), env);

          SET_SYMBOL_VCELL(sym, val);
          retval = val;
          break;

     case FOP_LOCAL_REF:
          checked_assert(SYMBOLP(sym));

          binding = lenvlookup(sym, env);

          if (NULLP(binding))
               vmerror_unbound(sym);

          retval = CAR(binding);
          break;

     case FOP_LOCAL_SET:
          checked_assert(SYMBOLP(sym));

          binding = lenvlookup(sym, env);

          if (NULLP(binding))
               vmerror_unbound(sym);

          val = leval(FAST_OP_ARG2(form), env);
          SET_CAR(binding, val);

          retval = val;
          break;

     case FOP_APPLY:
          argc = evaluate_arguments_to_buffer(FAST_OP_ARG2(form), env, ARG_BUF_LEN, argv);

          form = apply(leval(FAST_OP_ARG1(form), env), argc, argv, &env, &retval);

          if (!NULLP(form))
               goto loop;

          break;

     case FOP_IF_TRUE:
          if (TRUEP(leval(FAST_OP_ARG1(form), env)))
               form = FAST_OP_ARG2(form);
          else
               form = FAST_OP_ARG3(form);
          goto loop;

     case FOP_AND2:
          if (TRUEP(leval(FAST_OP_ARG1(form), env)))
          {
               form = FAST_OP_ARG2(form);
               goto loop;
          }

          retval = boolcons(false);
          break;

     case FOP_OR2:
          val = leval(FAST_OP_ARG1(form), env);

          if (TRUEP(val))
          {
               retval = val;
               break;
          }

          form = FAST_OP_ARG2(form);
          goto loop;

     case FOP_SEQUENCE:
          leval(FAST_OP_ARG1(form), env);

          form = FAST_OP_ARG2(form);
          goto loop;

     case FOP_CLOSE_ENV:
          retval = lclosurecons(env, FAST_OP_ARG1(form), FAST_OP_ARG2(form));
          break;

     case FOP_GET_ENV:
          retval = env;
          break;

     case FOP_GLOBAL_DEF:
          retval = lidefine_global(FAST_OP_ARG1(form), FAST_OP_ARG2(form), FAST_OP_ARG3(form));
          break;

     case FOP_MARK_STACK:
          ENTER_MARKER_FRAME(leval(FAST_OP_ARG1(form), env))
          {
               retval = leval(FAST_OP_ARG2(form), env);
          }
          LEAVE_FRAME();
          break;

     default:
          panic("Unsupported fast-op");
     }

#ifdef _DEBUG
     LEAVE_FRAME();
#endif

     return retval;
}


LRef apply1(LRef fn, size_t argc, LRef argv[])
{
     assert((argc == 0) || (argv != NULL));

     LRef retval = NIL;

     STACK_CHECK(&fn);

     LRef env = NIL;
     LRef next_form = apply(fn, argc, argv, &env, &retval);

     if (NULLP(next_form))
          return retval;
     else
          return leval(next_form, env);
}

/*  REVISIT: lapply should be tail recursive */
LRef lapply(size_t argc, LRef argv[])
{
     size_t fn_argc = 0;
     LRef fn_argv[ARG_BUF_LEN];

     if (argc == 0)
          vmerror("apply requires a function to apply.", NIL);

     LRef fn = argv[0];

     for (size_t ii = 1; ii < argc - 1; ii++)
     {
          if (fn_argc >= ARG_BUF_LEN)
               break;

          fn_argv[fn_argc] = argv[ii];
          fn_argc++;
     }

     LRef args = (argc > 1) ? argv[argc - 1] : NIL;
     while (CONSP(args))
     {
          if (fn_argc >= ARG_BUF_LEN)
               break;

          fn_argv[fn_argc] = CAR(args);
          fn_argc++;

          args = CDR(args);
     }

     if (fn_argc >= ARG_BUF_LEN)
          vmerror("too many arguments in call to apply: ~s", lcons(fn, NIL));

     if (!NULLP(args))
          vmerror("bad argument list in call to apply: ~s", lcons(fn, args));

     return apply1(fn, fn_argc, fn_argv);
}

LRef lunbind_symbol(LRef var)
{
     if (!SYMBOLP(var))
          vmerror_wrong_type(1, var);

     SET_SYMBOL_VCELL(var, UNBOUND_MARKER);

     return NIL;
}

LRef lsetvar(LRef var, LRef val, LRef lenv, LRef genv)
{
     LRef tmp;

     if (!SYMBOLP(var))
          vmerror_wrong_type(1, var);

     tmp = lenvlookup(var, lenv);

     if (NULLP(tmp))
     {
          LRef old_genv = interp.global_env;

          if (TRUEP(genv) && !NULLP(genv))
               set_global_env(genv);

          if (UNBOUND_MARKER_P(SYMBOL_VCELL(var)))
               vmerror_unbound(var);

          if (SYMBOL_HOME(var) == interp.keyword_package)
               vmerror("Cannot rebind keywords: ~s", var);

          SET_SYMBOL_VCELL(var, val);

          interp.global_env = old_genv;

          return val;
     }

     SET_CAR(tmp, val);
     return val;
}

bool call_lisp_procedurev(LRef closure, LRef * out_retval, LRef * out_escape_tag, LRef leading_args,
                          size_t n, va_list args)
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
               lsetcar(loc, va_arg(args, LRef));
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
          LRef argv[1];
          argv[0] = dummy_form;

          retval = apply1(closure, 1, argv);
          failed = false;
     }
     ON_ERROR()
     {
          retval = ERROR_RETVAL();
          if (out_escape_tag)
               *out_escape_tag = ERROR_TAG();
     }
     LEAVE_TRY();


     if (out_retval)
          *out_retval = retval;

     return failed;
}

bool call_lisp_procedure(LRef closure, LRef * out_retval, LRef * out_escape_tag, size_t n, ...)
{
     assert(PROCEDUREP(closure));

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
               var, interp.global_env, VECTOR_ELEM(interp.global_env, 0));

     if (SYMBOL_INDEX(var) == 0)
          extend_global_environment(var);

     SET_SYMBOL_VCELL(var, val);

     vmtrap(TRAP_DEFINE, VMT_OPTIONAL_TRAP, 2, var, val);

     interp.global_env = old_genv;

     return val;
}


LRef ltime_apply0(LRef fn)
{
     if (!PROCEDUREP(fn))
          vmerror_wrong_type(1, fn);

     fixnum_t cells = interp.gc_total_cells_allocated;
     fixnum_t env_cells = interp.gc_total_environment_cells_allocated;
     fixnum_t c_blocks = malloc_blocks;
     fixnum_t c_bytes = malloc_bytes;
     flonum_t t = sys_runtime();
     flonum_t gc_t = interp.gc_total_run_time;
     size_t forms = interp.forms_evaluated;

     LRef argv[8];

     argv[0] = apply1(fn, 0, NULL);

     argv[1] = flocons(sys_runtime() - t);
     argv[2] = flocons(interp.gc_total_run_time - gc_t);
     argv[3] = fixcons(interp.gc_total_cells_allocated - cells);
     argv[4] = fixcons(interp.gc_total_environment_cells_allocated - env_cells);
     argv[5] = fixcons(malloc_blocks - c_blocks);
     argv[6] = fixcons(malloc_bytes - c_bytes);
     argv[7] = fixcons(interp.forms_evaluated - forms);

     return lvector(8, argv);
}

/**************************************************************
 * Handler Bindings
 **************************************************************/

LRef lset_handler_frames(LRef new_frames)
{
     CURRENT_TIB()->handler_frames = new_frames;

     return new_frames;
}

LRef lhandler_frames()
{
     return CURRENT_TIB()->handler_frames;
}

/* TODO: Refactor implementation of catch/throw:
 *
 * 2) catch should match catch tags on eqv?
 * 3) throw should detect missing catch tag prior to unwinding the stack.
 * 5) catch should have optional on-throw thunk that's tail-called if the catch is thrown to.
 */

LRef lcatch_apply0(LRef tag, LRef fn)
{
     LRef retval;

     /*  tag==#t implies all tags */
     if (BOOLP(tag) && TRUEP(tag))
          tag = NULL;

     ENTER_TRY(tag)
     {
          retval = apply1(fn, 0, NULL);
     }
     ON_ERROR()
     {
          dscwritef(DF_SHOW_THROWS, _T("; DEBUG: catch ~a :~a\n"), ERROR_TAG(), ERROR_RETVAL());

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
          rc = apply1(thunk, 0, NULL);
     }
     ON_UNWIND()
     {
          apply1(after, 0, NULL);
     }
     LEAVE_UNWIND_PROTECT();

     return rc;
}


LRef lthrow(LRef tag, LRef value)
{
     dscwritef(DF_SHOW_THROWS, _T("; DEBUG: throw ~a :~a\n"), tag, value);

     THROW_ESCAPE(tag, value);

     return (NIL);
}


LRef lfuncall1(LRef fcn, LRef a1)
{
     LRef argv[1];
     argv[0] = a1;

     return apply1(fcn, 1, argv);
}

LRef lfuncall2(LRef fcn, LRef a1, LRef a2)
{
     LRef argv[2];
     argv[0] = a1;
     argv[1] = a2;

     return apply1(fcn, 2, argv);
}

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

     assert((TOP_FRAME->type == FRAME_EX_TRY) || (TOP_FRAME->type == FRAME_EX_UNWIND));

     assert(TOP_FRAME->frame_as.dynamic_escape.pending);

     return TOP_FRAME->frame_as.dynamic_escape.retval;
}

LRef __ex_current_catch_tag()
{
     assert(TOP_FRAME);

     assert((TOP_FRAME->type == FRAME_EX_TRY) || (TOP_FRAME->type == FRAME_EX_UNWIND));

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
bool __ex_matching_frame_1(frame_record_t * rec, uptr_t tag, bool exclude_unwind_protection)
{
     if (!exclude_unwind_protection)
     {
          /* If a frame is being unwound, it means that we're executing the
           * unwind clause and any errors thrown belong to an outside exception
           * frame. Therefore it is not a candidate for the current throw. */
          if ((rec->type == FRAME_EX_UNWIND) && !rec->frame_as.dynamic_escape.unwinding)
               return TRUE;
     }

     if (rec->type == FRAME_EX_TRY)
     {
          if (NULLP(rec->frame_as.dynamic_escape.tag))
               return TRUE;
          else
               return EQ(rec->frame_as.dynamic_escape.tag, (LRef) tag);
     }

     return FALSE;
}

bool __ex_next_frame_to_catch(frame_record_t * rec, uptr_t tag)
{
     return __ex_matching_frame_1(rec, tag, FALSE);
}

bool __ex_next_try_frame(frame_record_t * rec, uptr_t tag)
{
     return __ex_matching_frame_1(rec, tag, TRUE);
}


void __ex_throw_dynamic_escape(LRef tag, LRef retval, bool already_pending)
{
     UNREFERENCED(already_pending);

     /* Check to see if we have a matching catch block... */
     frame_record_t *next_try = __frame_find(__ex_next_try_frame, (uptr_t) tag);

     /* ...If we do, start unwinding the stack... */
     if (next_try)
     {
          frame_record_t *next_catcher = __frame_find(__ex_next_frame_to_catch, (uptr_t) tag);

          next_catcher->frame_as.dynamic_escape.pending = TRUE;
          next_catcher->frame_as.dynamic_escape.unwinding = TRUE;
          next_catcher->frame_as.dynamic_escape.tag = tag;
          next_catcher->frame_as.dynamic_escape.retval = retval;

          __frame_set_top(next_catcher);

          longjmp(next_catcher->frame_as.dynamic_escape.cframe, 1);
     }

     vmtrap(TRAP_UNCAUGHT_THROW, (vmt_options_t)(VMT_MANDATORY_TRAP | VMT_HANDLER_MUST_ESCAPE),
            2, tag, retval);

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
     assert((TOP_FRAME->type == FRAME_EX_TRY) || (TOP_FRAME->type == FRAME_EX_UNWIND));
     assert(TOP_FRAME->frame_as.dynamic_escape.pending);

     tag = TOP_FRAME->frame_as.dynamic_escape.tag;
     retval = TOP_FRAME->frame_as.dynamic_escape.retval;

     /* Avoid hitting the same exception over and over again... */
     CURRENT_TIB()->frame_stack = TOP_FRAME;

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
     if (!VECTORP(new_global_env))
          vmerror_wrong_type(new_global_env);

     LRef old_global_env = interp.global_env;
     LRef retval = NIL;

     ENTER_UNWIND_PROTECT()
     {

          interp.global_env = new_global_env;

          check_global_environment_size();

          retval = apply1(fn, 0, NULL);

     }
     ON_UNWIND()
     {
          interp.global_env = old_global_env;
     }
     LEAVE_UNWIND_PROTECT();

     return retval;
}

LRef fast_op(int opcode, LRef arg1, LRef arg2, LRef arg3)
{
     LRef z = new_cell(TC_FAST_OP);

     SET_FAST_OP_OPCODE(z, opcode);
     SET_FAST_OP_ARG1(z, arg1);
     SET_FAST_OP_ARG2(z, arg2);
     SET_FAST_OP_ARG3(z, arg3);

     return (z);
}

LRef lfast_op(LRef opcode, LRef arg1, LRef arg2, LRef arg3)
{
     if (!FIXNUMP(opcode))
          vmerror_wrong_type(1, opcode);

     return fast_op(FIXNM(opcode), arg1, arg2, arg3);
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

     return listn(3, FAST_OP_ARG1(fastop), FAST_OP_ARG2(fastop), FAST_OP_ARG3(fastop));
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

     if (FAST_OP_ARG3(a) != FAST_OP_ARG3(b))
          return false;

     return true;
}


END_NAMESPACE
