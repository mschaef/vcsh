/*
 * The scan evaluator
 */

#include "scan.h"

BEGIN_NAMESPACE(scan)


/***** stack limit checking *****/

LRef lset_stack_limit(LRef amount)
{
     size_t new_size_limit = 0;

     if (!NULLP(amount) && !FALSEP(amount))
          new_size_limit = get_c_long(amount);

     void *new_limit_obj = sys_set_stack_limit(new_size_limit);

     if (!new_size_limit)
     {
          dscwritef(DF_SHOW_GC, ("stack limit disabled!"));

          return boolcons(false);
     }

     dscwritef(DF_SHOW_GC, ("stack_size = ~cd bytes, [~c&,~c&]\n", new_size_limit, new_limit_obj, sys_get_stack_start()));

     return fixcons(new_size_limit);
}

/***** Interrupts *****/

LRef lset_interrupt_mask(LRef new_mask)
{
     if (!BOOLP(new_mask))
          vmerror_wrong_type(1, new_mask);

     bool previous_mask = interp.interrupts_masked;

     interp.interrupts_masked = BOOLV(new_mask);

     return boolcons(previous_mask);
}

void signal_interrupt(vminterrupt_t intr)
{
     interp.interrupts_pending = (vminterrupt_t)(interp.interrupts_pending | intr);
}

static void handle_interrupt(vminterrupt_t intr, trap_type_t handler)
{
     interp.interrupts_pending = (vminterrupt_t)(interp.interrupts_pending & ~intr);

     vmtrap(handler, VMT_MANDATORY_TRAP, 0);
}

INLINE void _process_interrupts()
{
     if (!interp.interrupts_pending || interp.interrupts_masked)
          return;

     if (interp.interrupts_pending & VMINTR_BREAK)
          handle_interrupt(VMINTR_BREAK, TRAP_USER_BREAK);

     if (interp.interrupts_pending & VMINTR_TIMER)
          handle_interrupt(VMINTR_TIMER, TRAP_TIMER_EVENT);
}

/***** Trap handling *****/

static size_t get_trap_id(LRef trap_id)
{
     if (!FIXNUMP(trap_id))
          vmerror_wrong_type(1, trap_id);

     size_t id = (size_t)FIXNM(trap_id);

     if ((id < 0) || (id > TRAP_LAST))
          vmerror_arg_out_of_range(trap_id, _T("[0,TRAP_LAST]"));

     return id;
}

LRef liset_trap_handler(LRef trap_id, LRef new_handler)
{
     if (!PROCEDUREP(new_handler))
          vmerror_wrong_type(2, new_handler);

     size_t tid = get_trap_id(trap_id);

     interp.trap_handlers[tid] = new_handler;

     dscwritef(DF_SHOW_TRAPS, (_T("; DEBUG: set-trap-handler : ~cS := ~s\n"), trap_type_name((trap_type_t)tid), new_handler));

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

     dscwritef(DF_SHOW_TRAPS, (_T("; DEBUG: trap : ~cS\n"), trap_type_name(trap)));
     
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

/***** The evaluator *****/

static LRef execute_fast_op(LRef form, LRef env);

static LRef arg_list_from_buffer(size_t argc, LRef argv[])
{
     LRef result = NIL;

     for (size_t ii = argc; ii > 0; ii--)
          result = lcons(argv[ii - 1], result);

     return result;
}

static size_t evaluate_arguments_to_buffer(LRef l, LRef env, size_t max_argc, LRef argv[])
{
     size_t argc = 0;
     LRef args = l;

     while (CONSP(args))
     {
          if (argc >= max_argc)
          {
               vmerror_unsupported(_T("too many actual arguments"));
               break;
          }

          argv[argc] = execute_fast_op(CAR(args), env);

          args = CDR(args);
          argc++;
     }

     if (!NULLP(args))
          vmerror_arg_out_of_range(l, _T("bad formal argument list"));

     return argc;
}

static LRef extend_env(LRef actuals, LRef formals, LRef env)
{
     if (SYMBOLP(formals))
          return lcons(lcons(lcons(formals, NIL), lcons(actuals, NIL)), env);
     else
          return lcons(lcons(formals, actuals), env);
}

LRef lenvlookup(LRef var, LRef env)
{
     LRef frame;

     for (frame = env; CONSP(frame); frame = CDR(frame))
     {
          LRef tmp = CAR(frame);

          if (!CONSP(tmp))
               panic("damaged frame");

          LRef al, fl;
     
          for (fl = CAR(tmp), al = CDR(tmp); CONSP(fl); fl = CDR(fl), al = CDR(al))
          {
               if (!CONSP(al))
                    vmerror_arg_out_of_range(NIL, _T("too few arguments"));

               if (EQ(CAR(fl), var))
                    return al;
          }

          if (SYMBOLP(fl) && EQ(fl, var))
               return lcons(al, NIL);
     }

     if (!NULLP(frame))
          panic("damaged env");

     return NIL;
}

/* Frames and exceptions
 *
 * Frames are basically annotations on the dynamic stack. Each
 * frame has an "frame record" stored in an auto variable
 * local to a newly created scope. When the frame is entered,
 * the frame's frame record is registered on a global stack.
 * When the frame is left, the frame record is popped off of
 * the stack.
 */

INLINE frame_t *enter_frame()
{
     CURRENT_TIB()->fsp++;
     return CURRENT_TIB()->fsp;
}

INLINE void leave_frame()
{
     CURRENT_TIB()->fsp--;
}

#define _ARGV(index) ((index >= argc) ? NIL : argv[index])

INLINE LRef subr_apply(LRef function, size_t argc, LRef argv[], LRef * env, LRef * retval)
{
     UNREFERENCED(env);

     frame_t  *__frame = enter_frame();
     __frame->type              = FRAME_PRIMITIVE; 
     __frame->as.prim.function  = function;

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
      {
           LRef arg1 = _ARGV(0);
          
          arg1 = SUBR_F2(function) (arg1, _ARGV(1));
          
          for (size_t ii = 2; ii < argc; ii++)
               arg1 = SUBR_F2(function) (arg1, _ARGV(ii));
          
          *retval = arg1;
     }
     break;
          
     case SUBR_ARGC:
          *retval = (SUBR_FARGC(function) (argc, argv));
          break;
          
     case SUBR_N:
          {
          LRef args = arg_list_from_buffer(argc, argv);
          *retval = (SUBR_F1(function) (args));
          }
          break;
     }

     leave_frame();

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

static void continue_throw()
{
     assert(CURRENT_TIB()->throw_target != NULL);
     
     for(frame_t *fsp = CURRENT_TIB()->fsp - 1; fsp > CURRENT_TIB()->throw_target; fsp--)
     {
          if (fsp->type != FRAME_EX_UNWIND)
               continue;
          
          dscwritef(DF_SHOW_THROWS, (_T("; DEBUG: setjmp (from fsp=~c&) to unwind-protect frame: ~c&\n"), CURRENT_TIB()->fsp, fsp));

          CURRENT_TIB()->fsp = fsp;
          longjmp(fsp->as.escape.cframe, 1);
     }
     
     dscwritef(DF_SHOW_THROWS, (_T("; DEBUG: setjmp (from fsp=~c&) to target frame: ~c&\n"), CURRENT_TIB()->fsp, CURRENT_TIB()->throw_target));

     CURRENT_TIB()->fsp = CURRENT_TIB()->throw_target;
     CURRENT_TIB()->throw_target = NULL;

     longjmp(CURRENT_TIB()->fsp->as.escape.cframe, 1);
}


static frame_t *find_throw_target(LRef tag)
{
     frame_t *start_at = CURRENT_TIB()->fsp;

     if (CURRENT_TIB()->throw_target != NULL)
          start_at = CURRENT_TIB()->throw_target - 1;

     for(frame_t *fsp = start_at; fsp > &(CURRENT_TIB()->frame_stack[0]); fsp--)
     {
          if (fsp->type != FRAME_EX_TRY)
               continue;
          
          if (NULLP(fsp->as.escape.tag) || EQ(fsp->as.escape.tag, tag))
               return fsp;
     }

     return NULL;
}

static void lthrow(LRef tag, LRef retval)
{
     dscwritef(DF_SHOW_THROWS, (_T("; DEBUG: throw ~a :~a\n"), tag, retval));

     frame_t *target = find_throw_target(tag);

     /* If we don't find a matching catch for the throw, we have a problem and need to invoke a trap. */
     if (target == NULL)
     {
          vmtrap(TRAP_UNCAUGHT_THROW, (vmt_options_t)(VMT_MANDATORY_TRAP | VMT_HANDLER_MUST_ESCAPE),
                 2, tag, retval);
          return;
     }

     dscwritef(DF_SHOW_THROWS, (_T("; DEBUG: updating throw-target: ~c& -> ~c&\n"), CURRENT_TIB()->throw_target, target));

     CURRENT_TIB()->throw_target = target;
     CURRENT_TIB()->throw_value  = retval;

     continue_throw();
}



static LRef execute_fast_op(LRef fop, LRef env)
{
     LRef retval = NIL;

     STACK_CHECK(&fop);
     
     frame_t *__frame = enter_frame();
     __frame->type                 = FRAME_EVAL;
     __frame->as.eval.form         = &fop;
     __frame->as.eval.initial_form = fop;
     __frame->as.eval.env          = env;

loop:
     _process_interrupts();
          
     checked_assert(TYPE(fop) == TC_FAST_OP);
          
          
     switch (FAST_OP_OPCODE(fop))
     {
     case FOP_LITERAL:
          retval = FAST_OP_ARG1(fop);
          break;
               
     case FOP_GLOBAL_REF:
     {
          LRef sym = FAST_OP_ARG1(fop);
               
          checked_assert(SYMBOLP(sym));
          checked_assert(SYMBOL_HOME(sym) != interp.control_fields[VMCTRL_PACKAGE_KEYWORD]);
               
          LRef binding = SYMBOL_VCELL(sym);
               
          if (UNBOUND_MARKER_P(binding))
               vmerror_unbound(sym);
               
          retval = binding;
     }
     break;
          
     case FOP_GLOBAL_SET:
     {
          LRef sym = FAST_OP_ARG1(fop);
               
          checked_assert(SYMBOLP(sym));
          checked_assert(SYMBOL_HOME(sym) != interp.keyword_package);
               
          LRef binding = SYMBOL_VCELL(sym);
               
          if (UNBOUND_MARKER_P(binding))
               vmerror_unbound(sym);
               
          LRef val = execute_fast_op(FAST_OP_ARG2(fop), env);
               
          SET_SYMBOL_VCELL(sym, val);
          retval = val;
     }
     break;
          
     case FOP_LOCAL_REF:
     { 
          LRef sym = FAST_OP_ARG1(fop);
               
          checked_assert(SYMBOLP(sym));
               
          LRef binding = lenvlookup(sym, env);
               
          if (NULLP(binding))
               vmerror_unbound(sym);
               
          retval = CAR(binding);
     }
     break;
          
     case FOP_LOCAL_SET:
     {
          LRef sym = FAST_OP_ARG1(fop);
               
          checked_assert(SYMBOLP(sym));
               
          LRef binding = lenvlookup(sym, env);
               
          if (NULLP(binding))
               vmerror_unbound(sym);
               
          LRef val = execute_fast_op(FAST_OP_ARG2(fop), env);
               
          SET_CAR(binding, val);
               
          retval = val;
     }
     break;
          
     case FOP_APPLY:
     {
          size_t argc;
          LRef argv[ARG_BUF_LEN];
               
          argc = evaluate_arguments_to_buffer(FAST_OP_ARG2(fop), env, ARG_BUF_LEN, argv);
               
          fop = apply(execute_fast_op(FAST_OP_ARG1(fop), env), argc, argv, &env, &retval);
               
          if (!NULLP(fop))
               goto loop;
     }
     break;
          
     case FOP_IF_TRUE:
          if (TRUEP(execute_fast_op(FAST_OP_ARG1(fop), env)))
               fop = FAST_OP_ARG2(fop);
          else
               fop = FAST_OP_ARG3(fop);
          goto loop;
               
     case FOP_AND2:
          if (TRUEP(execute_fast_op(FAST_OP_ARG1(fop), env)))
          {
               fop = FAST_OP_ARG2(fop);
               goto loop;
          }
               
          retval = boolcons(false);
          break;
               
     case FOP_OR2:
     {
          LRef val = execute_fast_op(FAST_OP_ARG1(fop), env);
               
          if (TRUEP(val))
          {
               retval = val;
               break;
          }
               
          fop = FAST_OP_ARG2(fop);
     }
     goto loop;
          
     case FOP_SEQUENCE:
          execute_fast_op(FAST_OP_ARG1(fop), env);
               
          fop = FAST_OP_ARG2(fop);
          goto loop;
               
     case FOP_CATCH_APPLY0:
     {
          LRef tag = execute_fast_op(FAST_OP_ARG1(fop), env);
               
          /* tag==#t implies all tags */
          if (BOOLP(tag) && TRUEP(tag))
               tag = NULL;
               
          frame_t *__frame = enter_frame();
          __frame->type = FRAME_EX_TRY;
          __frame->as.escape.tag = tag;

          if (setjmp(CURRENT_TIB()->fsp->as.escape.cframe) == 0)
          {
               retval = apply1(execute_fast_op(FAST_OP_ARG2(fop), env), 0, NULL);
          }
          else
          {
               dscwritef(DF_SHOW_THROWS, (_T("; DEBUG: catch retval =~a\n"), CURRENT_TIB()->throw_value));
                    
               retval = CURRENT_TIB()->throw_value;
          }
          leave_frame();
     }
     break;
          
     case FOP_THROW:
          lthrow(execute_fast_op(FAST_OP_ARG1(fop), env),
                 execute_fast_op(FAST_OP_ARG2(fop), env));
          break;
               
     case FOP_UNWIND_PROTECT:
     {
          frame_t *__frame = enter_frame();
          __frame->type = FRAME_EX_UNWIND;
          __frame->as.escape.tag = NULL;

          if (setjmp(CURRENT_TIB()->fsp->as.escape.cframe) == 0)
          {
               retval = apply1(execute_fast_op(FAST_OP_ARG1(fop), env), 0, NULL);
          }

          leave_frame();

          apply1(execute_fast_op(FAST_OP_ARG2(fop), env), 0, NULL);

          if (CURRENT_TIB()->throw_target != NULL)
               continue_throw();
     }
     break;

     case FOP_CATCH:
     {
          LRef tag = execute_fast_op(FAST_OP_ARG1(fop), env);
               
          frame_t *__frame = enter_frame();
          __frame->type = FRAME_EX_TRY;
          __frame->as.escape.tag = tag;

          if (setjmp(CURRENT_TIB()->fsp->as.escape.cframe) == 0)
          {
               retval = execute_fast_op(FAST_OP_ARG2(fop), env);
          }
          else
          {
               dscwritef(DF_SHOW_THROWS, (_T("; DEBUG: catch retval =~a\n"), CURRENT_TIB()->throw_value));
                    
               retval = CURRENT_TIB()->throw_value;
          }
          leave_frame();
     }
     break;

     case FOP_WITH_UNWIND_FN:
     {
          frame_t *__frame = enter_frame();
          __frame->type = FRAME_EX_UNWIND;
          __frame->as.escape.tag = NULL;

          if (setjmp(CURRENT_TIB()->fsp->as.escape.cframe) == 0)
               retval = execute_fast_op(FAST_OP_ARG2(fop), env);

          leave_frame();

          apply1(execute_fast_op(FAST_OP_ARG1(fop), env), 0, NULL);

          if (CURRENT_TIB()->throw_target != NULL)
               continue_throw();
     }
     break;

          
     case FOP_CLOSE_ENV:
          retval = lclosurecons(env, FAST_OP_ARG1(fop), FAST_OP_ARG2(fop));
          break;
               
     case FOP_GET_ENV:
          retval = env;
          break;
               
     case FOP_GLOBAL_DEF:
          retval = lidefine_global(FAST_OP_ARG1(fop), FAST_OP_ARG2(fop), FAST_OP_ARG3(fop));
          break;
               
     case FOP_MARK_STACK:
     {
          frame_t *__frame = enter_frame();
          __frame->type = FRAME_MARKER;
          __frame->as.marker.tag = execute_fast_op(FAST_OP_ARG1(fop), env);
          retval = execute_fast_op(FAST_OP_ARG2(fop), env);

          leave_frame();
     }
     break;
               
     case FOP_SET_GENV:
     {
          LRef old_global_env = interp.global_env;

          interp.global_env = execute_fast_op(FAST_OP_ARG1(fop), env);

          assert(GENVP(interp.global_env));

          retval = old_global_env;
     }
     break;
          
     default:
          panic("Unsupported fast-op");
     }


     leave_frame();

     return retval;
}

LRef apply1(LRef fn, size_t argc, LRef argv[])
{
     checked_assert((argc == 0) || (argv != NULL));

     LRef retval = NIL;

     STACK_CHECK(&fn);

     LRef env = NIL;
     LRef next_form = apply(fn, argc, argv, &env, &retval);

     if (NULLP(next_form))
          return retval;
     else
          return execute_fast_op(next_form, env);
}

/*  REVISIT: lapply should be tail recursive */
LRef lapply(size_t argc, LRef argv[])
{
     size_t fn_argc = 0;
     LRef fn_argv[ARG_BUF_LEN];

     LRef fn = (argc > 0) ? argv[0] : NIL;

     if (!PROCEDUREP(fn))
          vmerror_wrong_type(1, fn);

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
          vmerror_unsupported(_T("too many actual arguments in call to apply"));

     if (!NULLP(args))
          vmerror_arg_out_of_range(args, _T("bad formal argument list"));

     return apply1(fn, fn_argc, fn_argv);
}

/***** Handlers *****/

LRef lset_handler_frames(LRef new_frames)
{
     CURRENT_TIB()->handler_frames = new_frames;

     return new_frames;
}

LRef lhandler_frames()
{
     return CURRENT_TIB()->handler_frames;
}

/***** Frame Management *****/

LRef lget_current_frames(LRef sc)
{
     fixnum_t skip_count = get_c_fixnum(sc);

     LRef frames = NIL;

     fixnum_t frame_count = 0;
     
     frame_t *fsp = CURRENT_TIB()->fsp;

     for(; fsp > &(CURRENT_TIB()->frame_stack[0]); fsp--)
     {
          LRef frame_obj = NIL;

          frame_count++;

          switch (fsp->type)
          {
          case FRAME_EVAL:
               frame_obj = listn(3,
                                 *fsp->as.eval.form,
                                 fsp->as.eval.initial_form,
                                 fsp->as.eval.env);
               break;

          case FRAME_EX_TRY:
               frame_obj = listn(1, fsp->as.escape.tag);
               break;

          case FRAME_EX_UNWIND:
               frame_obj = NIL;
               break;

          case FRAME_PRIMITIVE:
               frame_obj = listn(1, fsp->as.prim.function);
               break;

          case FRAME_MARKER:
               frame_obj = listn(1, fsp->as.marker.tag);
               break;

          default:
               panic("invalid frame type.");
               break;
          }

          frame_obj = lcons(fixcons(fsp->type), lcons(fixcons((fixnum_t)fsp), frame_obj));

          if (frame_count >= skip_count)
               frames = lcons(frame_obj, frames);
     }

     return frames;
}

LRef topmost_primitive()
{
     for(frame_t *fsp = CURRENT_TIB()->fsp; fsp > &(CURRENT_TIB()->frame_stack[0]); fsp--)
     {
          if (fsp->type == FRAME_PRIMITIVE)
               return fsp->as.prim.function;
     }

     return NIL;
}

LRef ltopframe() // TODO: REMOVE
{
     return fixcons(0); 
}

END_NAMESPACE
