
/*
 * evaluator.cpp --
 *
 * The Fast-op Evaluator.
 *
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include <stdio.h>

#include "scan-private.h"

BEGIN_NAMESPACE(scan)


#ifdef CHECKED
#  define EVAL_INLINE 
#else
#  define EVAL_INLINE INLINE
#endif

/***** stack limit checking *****/

lref_t lset_stack_limit(lref_t amount)
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

lref_t lset_interrupt_mask(lref_t new_mask)
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

EVAL_INLINE void _process_interrupts()
{
     if (!interp.interrupts_pending || interp.interrupts_masked)
          return;

     if (interp.interrupts_pending & VMINTR_BREAK)
          handle_interrupt(VMINTR_BREAK, TRAP_USER_BREAK);

     if (interp.interrupts_pending & VMINTR_TIMER)
          handle_interrupt(VMINTR_TIMER, TRAP_TIMER_EVENT);
}

/***** Trap handling *****/

static size_t get_trap_id(lref_t trap_id)
{
     if (!FIXNUMP(trap_id))
          vmerror_wrong_type(1, trap_id);

     size_t id = (size_t)FIXNM(trap_id);

     if ((id < 0) || (id > TRAP_LAST))
          vmerror_arg_out_of_range(trap_id, _T("[0,TRAP_LAST]"));

     return id;
}

lref_t liset_trap_handler(lref_t trap_id, lref_t new_handler)
{
     if (!PROCEDUREP(new_handler))
          vmerror_wrong_type(2, new_handler);

     size_t tid = get_trap_id(trap_id);

     interp.trap_handlers[tid] = new_handler;

     dscwritef(DF_SHOW_TRAPS, (_T("; DEBUG: set-trap-handler : ~cS := ~s\n"), trap_type_name((trap_type_t)tid), new_handler));

     return new_handler;
}

lref_t litrap_handler(lref_t trap_id)
{
     return interp.trap_handlers[get_trap_id(trap_id)];
}

static void vmtrap_panic(trap_type_t trap, const _TCHAR *msg)
{
     _TCHAR buf[STACK_STRBUF_LEN];

     _sntprintf(buf, STACK_STRBUF_LEN, _T("Trap error for %s: %s"), trap_type_name(trap), msg);

     panic(buf);
}

lref_t vmtrap(trap_type_t trap, vmt_options_t options, size_t argc, ...)
{
     assert((trap > 0) && (trap <= TRAP_LAST));
     assert(argc < ARG_BUF_LEN);

     dscwritef(DF_SHOW_TRAPS, (_T("; DEBUG: trap : ~cS\n"), trap_type_name(trap)));
     
     lref_t handler = interp.trap_handlers[trap];

     if (!PROCEDUREP(handler))
     {
          if(!NULLP(handler))
               vmtrap_panic(trap, "bad trap handler");

          if (!(options & VMT_OPTIONAL_TRAP))
               vmtrap_panic(trap, "missing trap handler");

          return NIL;
     }

     lref_t retval = NIL;
     va_list args;

     va_start(args, argc);

     lref_t argv[ARG_BUF_LEN];

     argv[0] = fixcons(trap);
     argv[1] = fixcons((fixnum_t)CURRENT_TIB()->fsp);
     for (size_t ii = 2; ii < argc + 2; ii++)
          argv[ii] = va_arg(args, lref_t);

     va_end(args);

     retval = apply1(handler, argc + 2, argv);

     if (options & VMT_HANDLER_MUST_ESCAPE)
          vmtrap_panic(trap, "trap handler must escape");

     return retval;
}

/***** The evaluator *****/

static lref_t execute_fast_op(lref_t form, lref_t env);

static lref_t arg_list_from_buffer(size_t argc, lref_t argv[])
{
     lref_t result = NIL;

     for (size_t ii = argc; ii > 0; ii--)
          result = lcons(argv[ii - 1], result);

     return result;
}

static lref_t extend_env(lref_t actuals, lref_t formals, lref_t env)
{
     if (SYMBOLP(formals))
          return lcons(lcons(lcons(formals, NIL), lcons(actuals, NIL)), env);
     else
          return lcons(lcons(formals, actuals), env);
}

lref_t lenvlookup(lref_t var, lref_t env)
{
     lref_t frame;

     for (frame = env; CONSP(frame); frame = CDR(frame))
     {
          lref_t tmp = CAR(frame);

          if (!CONSP(tmp))
               panic("damaged frame");

          lref_t al, fl;
     
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

EVAL_INLINE frame_t *enter_frame()
{
     CURRENT_TIB()->fsp++;
     return CURRENT_TIB()->fsp;
}

EVAL_INLINE void leave_frame()
{
     CURRENT_TIB()->fsp--;
}

#define _ARGV(index) ((index >= argc) ? NIL : argv[index])

EVAL_INLINE lref_t subr_apply(lref_t function, size_t argc, lref_t argv[], lref_t * env, lref_t * retval)
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
           lref_t arg1 = _ARGV(0);
          
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
          lref_t args = arg_list_from_buffer(argc, argv);
          *retval = (SUBR_F1(function) (args));
          }
          break;
     }

     leave_frame();

     return NIL;
}

EVAL_INLINE lref_t apply(lref_t function, size_t argc, lref_t argv[], lref_t * env, lref_t * retval)
{
     typecode_t type = TYPE(function);

     /*  NIL signals "no tail recursion", what happens when the actual form is NIL? */

     if (type == TC_SUBR)
          return subr_apply(function, argc, argv, env, retval);

     if (type == TC_CLOSURE)
     {
          lref_t c_code = CLOSURE_CODE(function);

          *env = extend_env(arg_list_from_buffer(argc, argv), CAR(c_code), CLOSURE_ENV(function));

          return CDR(c_code);   /*  tail call */
     }

     vmerror_wrong_type(function);

     return NIL;                /*  avoid a warning, since the error case returns nothing. */
}

static frame_t *find_throw_target(frame_t *start_at, lref_t tag)
{
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

void unwind_stack_for_throw()
{
     for(frame_t *fsp = CURRENT_TIB()->fsp; fsp > &(CURRENT_TIB()->frame_stack[0]); fsp--)
     {
          if (fsp->type == FRAME_EX_UNWIND)
          {
               dscwritef(DF_SHOW_THROWS, (_T("; DEBUG: throw invoking unwind : ~c&\n"), fsp));

               apply1(fsp->as.unwind.after, 0, NULL);

               continue;
          }

          if (fsp->type != FRAME_EX_TRY)
               continue;
          
          if (fsp == CURRENT_TIB()->throw_target)
          {
               dscwritef(DF_SHOW_THROWS, (_T("; DEBUG: setjmp (from fsp=~c&) to target frame: ~c&\n"), CURRENT_TIB()->fsp, fsp));

               CURRENT_TIB()->throw_target = NULL;
               CURRENT_TIB()->fsp = fsp;
               
               longjmp(CURRENT_TIB()->fsp->as.escape.cframe, 1);
          }
     }
}

static void lthrow(lref_t tag, lref_t retval)
{
     dscwritef(DF_SHOW_THROWS, (_T("; DEBUG: throw ~a :~a\n"), tag, retval));

     CURRENT_TIB()->throw_target = find_throw_target(CURRENT_TIB()->fsp, tag);
     CURRENT_TIB()->throw_value = retval;

     unwind_stack_for_throw();

     /* If we don't find a matching catch for the throw, we have a problem and need to invoke a trap. */
     vmtrap(TRAP_UNCAUGHT_THROW, (vmt_options_t)(VMT_MANDATORY_TRAP | VMT_HANDLER_MUST_ESCAPE),
            2, tag, retval);
}

static lref_t execute_fast_op(lref_t fop, lref_t env)
{
     lref_t retval = NIL;

     STACK_CHECK(&fop);

     frame_t *__frame = enter_frame();
     __frame->type                 = FRAME_EVAL;
     __frame->as.eval.form         = &fop;
     __frame->as.eval.initial_form = fop;
     __frame->as.eval.env          = env;

loop:
     _process_interrupts();
          
     checked_assert(TYPE(fop) == TC_FAST_OP);
          
#if defined(WITH_FOPLOG_SUPPORT)
     if (CURRENT_TIB()->foplog_enable)
     {
          CURRENT_TIB()->foplog[CURRENT_TIB()->foplog_index] = fop;
          CURRENT_TIB()->foplog_index = (CURRENT_TIB()->foplog_index + 1) % FOPLOG_SIZE;
     }
#endif
         
     switch (FAST_OP_OPCODE(fop))
     {
     case FOP_LITERAL:
          retval = FAST_OP_ARG1(fop);
          break;
               
     case FOP_GLOBAL_REF:
     {
          lref_t sym = FAST_OP_ARG1(fop);
               
          checked_assert(SYMBOLP(sym));
          checked_assert(SYMBOL_HOME(sym) != interp.control_fields[VMCTRL_PACKAGE_KEYWORD]);
               
          lref_t binding = SYMBOL_VCELL(sym);
               
          if (UNBOUND_MARKER_P(binding))
               vmerror_unbound(sym);
               
          retval = binding;
     }
     break;
          
     case FOP_GLOBAL_SET:
     {
          lref_t sym = FAST_OP_ARG1(fop);
               
          checked_assert(SYMBOLP(sym));
          checked_assert(SYMBOL_HOME(sym) != interp.control_fields[VMCTRL_PACKAGE_KEYWORD]);
               
          lref_t binding = SYMBOL_VCELL(sym);
               
          if (UNBOUND_MARKER_P(binding))
               vmerror_unbound(sym);
               
          lref_t val = execute_fast_op(FAST_OP_ARG2(fop), env);
               
          SET_SYMBOL_VCELL(sym, val);
          retval = val;
     }
     break;
          
     case FOP_LOCAL_REF:
     { 
          lref_t sym = FAST_OP_ARG1(fop);
               
          checked_assert(SYMBOLP(sym));
               
          lref_t binding = lenvlookup(sym, env);
               
          if (NULLP(binding))
               vmerror_unbound(sym);
               
          retval = CAR(binding);
     }
     break;
          
     case FOP_LOCAL_SET:
     {
          lref_t sym = FAST_OP_ARG1(fop);
               
          checked_assert(SYMBOLP(sym));
               
          lref_t binding = lenvlookup(sym, env);
               
          if (NULLP(binding))
               vmerror_unbound(sym);
               
          lref_t val = execute_fast_op(FAST_OP_ARG2(fop), env);
               
          SET_CAR(binding, val);
               
          retval = val;
     }
     break;

     case FOP_APPLY_GLOBAL:
     {
          lref_t sym = FAST_OP_ARG1(fop);               
          lref_t fn = SYMBOL_VCELL(sym);
               
          checked_assert(SYMBOLP(sym));
          checked_assert(SYMBOL_HOME(sym) != interp.control_fields[VMCTRL_PACKAGE_KEYWORD]);

          if (UNBOUND_MARKER_P(fn))
               vmerror_unbound(sym);

          size_t argc = 0;
          lref_t argv[ARG_BUF_LEN];
          lref_t args = FAST_OP_ARG2(fop);

          while (CONSP(args))
          {
               if (argc >= ARG_BUF_LEN)
               {
                    vmerror_unsupported(_T("too many actual arguments"));
                    break;
               }

               argv[argc] = execute_fast_op(CAR(args), env);
               
               args = CDR(args);
               argc++;
          }

          if (!NULLP(args))
               vmerror_arg_out_of_range(FAST_OP_ARG2(fop), _T("bad formal argument list"));
               
          fop = apply(fn, argc, argv, &env, &retval);
               
          if (!NULLP(fop))
               goto loop;
     }
     break;
          
     case FOP_APPLY:
     {
          size_t argc = 0;
          lref_t argv[ARG_BUF_LEN];

          lref_t fn = execute_fast_op(FAST_OP_ARG1(fop), env);

          lref_t args = FAST_OP_ARG2(fop);

          while (CONSP(args))
          {
               if (argc >= ARG_BUF_LEN)
               {
                    vmerror_unsupported(_T("too many actual arguments"));
                    break;
               }

               argv[argc] = execute_fast_op(CAR(args), env);
               
               args = CDR(args);
               argc++;
          }

          if (!NULLP(args))
               vmerror_arg_out_of_range(FAST_OP_ARG2(fop), _T("bad formal argument list"));
               
          fop = apply(fn, argc, argv, &env, &retval);
               
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
          lref_t val = execute_fast_op(FAST_OP_ARG1(fop), env);
               
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
          
     case FOP_THROW:
          lthrow(execute_fast_op(FAST_OP_ARG1(fop), env),
                 execute_fast_op(FAST_OP_ARG2(fop), env));
          break;

     case FOP_CATCH:
     {
          lref_t tag = execute_fast_op(FAST_OP_ARG1(fop), env);
               
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
               CURRENT_TIB()->throw_value = NIL;
          }
          leave_frame();
     }
     break;

     case FOP_WITH_UNWIND_FN:
     {
          frame_t *__frame = enter_frame();
          __frame->type = FRAME_EX_UNWIND;
          __frame->as.unwind.after = execute_fast_op(FAST_OP_ARG1(fop), env);

          retval = execute_fast_op(FAST_OP_ARG2(fop), env);

          lref_t after = __frame->as.unwind.after;

          leave_frame();

          apply1(after, 0, NULL);
     }
     break;
          
     case FOP_CLOSURE:
          retval = lclosurecons(env, lcons(FAST_OP_ARG1(fop), FAST_OP_ARG2(fop)), FAST_OP_ARG3(fop));
          break;

     case FOP_GET_ENV:
          retval = env;
          break;
               
     case FOP_GLOBAL_DEF: // three args, third was genv, but currently unused
          retval = lidefine_global(FAST_OP_ARG1(fop), FAST_OP_ARG2(fop));
          break;

     case FOP_GET_FSP:
          retval = fixcons((fixnum_t)CURRENT_TIB()->fsp++);
          break;

     case FOP_GET_HFRAMES:
          retval = CURRENT_TIB()->handler_frames;
          break;

     case FOP_SET_HFRAMES:
          CURRENT_TIB()->handler_frames = execute_fast_op(FAST_OP_ARG1(fop), env);
          break;

     default:
          panic("Unsupported fast-op");
     }


     leave_frame();

     return retval;
}

lref_t apply1(lref_t fn, size_t argc, lref_t argv[])
{
     checked_assert((argc == 0) || (argv != NULL));

     lref_t retval = NIL;

     STACK_CHECK(&fn);

     lref_t env = NIL;
     lref_t next_form = apply(fn, argc, argv, &env, &retval);

     if (NULLP(next_form))
          return retval;
     else
          return execute_fast_op(next_form, env);
}

/*  REVISIT: lapply should be tail recursive */
lref_t lapply(size_t argc, lref_t argv[])
{
     size_t fn_argc = 0;
     lref_t fn_argv[ARG_BUF_LEN];

     lref_t fn = (argc > 0) ? argv[0] : NIL;

     if (!PROCEDUREP(fn))
          vmerror_wrong_type(1, fn);

     for (size_t ii = 1; ii < argc - 1; ii++)
     {
          if (fn_argc >= ARG_BUF_LEN)
               break;

          fn_argv[fn_argc] = argv[ii];
          fn_argc++;
     }

     lref_t args = (argc > 1) ? argv[argc - 1] : NIL;
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

/***** Frame Management *****/

lref_t lget_current_frames(lref_t sc)
{
     fixnum_t skip_count = get_c_fixnum(sc);

     lref_t frames = NIL;

     fixnum_t frame_count = 0;
     
     frame_t *fsp = CURRENT_TIB()->fsp;

     for(; fsp > &(CURRENT_TIB()->frame_stack[0]); fsp--)
     {
          lref_t frame_obj = NIL;

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
               frame_obj = listn(1, fsp->as.unwind.after);
               break;

          case FRAME_PRIMITIVE:
               frame_obj = listn(1, fsp->as.prim.function);
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

lref_t topmost_primitive()
{
     for(frame_t *fsp = CURRENT_TIB()->fsp; fsp > &(CURRENT_TIB()->frame_stack[0]); fsp--)
     {
          if (fsp->type == FRAME_PRIMITIVE)
               return fsp->as.prim.function;
     }

     return NIL;
}

#if defined(WITH_FOPLOG_SUPPORT)
lref_t lifoplog_reset()
{
     for(int ii = 0; ii < FOPLOG_SIZE; ii++)
          CURRENT_TIB()->foplog[ii] = NIL;
     
     CURRENT_TIB()->foplog_index = 0;

     return NIL;
}

lref_t lifoplog_enable(lref_t enablep)
{
     lref_t prev = boolcons(CURRENT_TIB()->foplog_enable);

     CURRENT_TIB()->foplog_enable = TRUEP(enablep);

     return prev;
}

lref_t lifoplog_snapshot()
{
     lref_t result = vectorcons(FOPLOG_SIZE, fixcons(-1));

     for(int ii = 0; ii < FOPLOG_SIZE; ii++)
          SET_VECTOR_ELEM(result, ii, CURRENT_TIB()->foplog[ii]);

     return result;
}
#endif

END_NAMESPACE
