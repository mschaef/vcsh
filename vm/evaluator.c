/*
 * evaluator.c --
 *
 * The Fast-op Evaluator.
 *
 *
 * (C) Copyright 2001-2014 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include <stdio.h>

#include "scan-private.h"

#ifdef CHECKED
#  define EVAL_INLINE static
#else
#  define EVAL_INLINE static INLINE
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

     dscwritef(DF_SHOW_GC, ("stack_size = ~cd bytes, [~c&,~c&]\n",
                            new_size_limit,
                            new_limit_obj,
                            sys_get_stack_start()));

     return fixcons(new_size_limit);
}

/***** Interrupts *****/

lref_t lset_interrupt_mask(lref_t new_mask)
{
     bool previous_mask = interp.intr_masked;

     interp.intr_masked = TRUEP(new_mask);

     return boolcons(previous_mask);
}

void signal_interrupt(enum vminterrupt_t intr)
{
     interp.intr_pending = (enum vminterrupt_t)(interp.intr_pending | intr);
}

static void handle_interrupt(enum vminterrupt_t intr, enum trap_type_t handler)
{
     interp.intr_pending = (enum vminterrupt_t)(interp.intr_pending & ~intr);

     vmtrap(handler, VMT_MANDATORY_TRAP, 0);
}

EVAL_INLINE void _process_interrupts()
{
     if (!interp.intr_pending || interp.intr_masked)
          return;

     if (interp.intr_pending & VMINTR_BREAK)
          handle_interrupt(VMINTR_BREAK, TRAP_USER_BREAK);

     if (interp.intr_pending & VMINTR_TIMER)
          handle_interrupt(VMINTR_TIMER, TRAP_TIMER_EVENT);
}

/***** Trap handling *****/

static size_t get_trap_id(lref_t trap_id)
{
     if (!FIXNUMP(trap_id))
          vmerror_wrong_type_n(1, trap_id);

     size_t id = (size_t)FIXNM(trap_id);

     if (id > TRAP_LAST)
          vmerror_arg_out_of_range(trap_id, _T("[0,TRAP_LAST]"));

     return id;
}

lref_t liset_trap_handler(lref_t trap_id, lref_t new_handler)
{
     if (!PROCEDUREP(new_handler))
          vmerror_wrong_type_n(2, new_handler);

     size_t tid = get_trap_id(trap_id);

     interp.trap_handlers[tid] = new_handler;

     dscwritef(DF_SHOW_TRAPS,
               (_T("; DEBUG: set-trap-handler : ~cS := ~s\n"),
                trap_type_name((enum trap_type_t)tid), new_handler));

     return new_handler;
}

lref_t litrap_handler(lref_t trap_id)
{
     return interp.trap_handlers[get_trap_id(trap_id)];
}

static void vmtrap_panic(enum trap_type_t trap, const _TCHAR *msg)
{
     _TCHAR buf[STACK_STRBUF_LEN];

     _sntprintf(buf, STACK_STRBUF_LEN, _T("Trap error for %s: %s"),
                trap_type_name(trap), msg);

     panic(buf);
}

lref_t vmtrap(enum trap_type_t trap, enum vmt_options_t options, size_t argc, ...)
{
     assert((trap > 0) && (trap <= TRAP_LAST));
     assert(argc < ARG_BUF_LEN);

     dscwritef(DF_SHOW_TRAPS, (_T("; DEBUG: trap : ~cS\n"),
                               trap_type_name(trap)));

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
     argv[1] = fixcons((fixnum_t)CURRENT_TIB()->frame);
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

          for (fl = CAR(tmp), al = CDR(tmp);
               CONSP(fl);
               fl = CDR(fl), al = CDR(al))
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

/* Frame stack
 *
 * The interpreter stack. This is used in a very similar fashion
 * to the traditional C stack, with CURRENT_TIB()->frame and
 * CURRENT_TIB()->fsp serving as base and stack pointers,
 * respectively.
 */
EVAL_INLINE void *fstack_alloca(size_t size)
{
     size = (size / sizeof(lref_t)) + 1;

     CURRENT_TIB()->fsp = CURRENT_TIB()->fsp - size;

     return (void *)(CURRENT_TIB()->fsp);
}

EVAL_INLINE void fstack_push(lref_t val)
{
     *(--CURRENT_TIB()->fsp) = val;
}

EVAL_INLINE void fstack_enter_frame(enum frame_type_t ft)
{
     CURRENT_TIB()->count_enter_frame++;

     fstack_push((lref_t)(CURRENT_TIB()->frame));

     CURRENT_TIB()->frame = CURRENT_TIB()->fsp;

     fstack_push((lref_t)ft);
}

EVAL_INLINE void fstack_leave_frame()
{
     CURRENT_TIB()->fsp = CURRENT_TIB()->frame + 1;

     CURRENT_TIB()->frame = *(lref_t **)(CURRENT_TIB()->frame);
}

EVAL_INLINE lref_t *fstack_prev_frame(lref_t *frame)
{
     return (lref_t *)frame[FOFS_LINK];
}

EVAL_INLINE enum frame_type_t fstack_frame_type(lref_t *frame)
{
     return (enum frame_type_t)((intptr_t)frame[FOFS_FTYPE]);
}

#define _ARGV(index) ((index >= argc) ? NIL : argv[index])

EVAL_INLINE lref_t subr_apply(lref_t function, size_t argc, lref_t argv[], lref_t * env, lref_t * retval)
{
     lref_t arg1;
     lref_t args;
     size_t ii;

     UNREFERENCED(env);

     fstack_enter_frame(FRAME_SUBR);
     fstack_push((lref_t)function);

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

      case SUBR_2N:
           arg1 = _ARGV(0);

           arg1 = SUBR_F2(function) (arg1, _ARGV(1));
           for (ii = 2; ii < argc; ii++)
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

     fstack_leave_frame();

     return NIL;
}

EVAL_INLINE lref_t apply(lref_t function,
                         size_t argc, lref_t argv[],
                         lref_t * env, lref_t * retval)
{
     if (SUBRP(function))
          return subr_apply(function, argc, argv, env, retval);

     if (CLOSUREP(function))
     {
          lref_t c_code = CLOSURE_CODE(function);

          *env = extend_env(arg_list_from_buffer(argc, argv),
                            CAR(c_code),
                            CLOSURE_ENV(function));

          return CDR(c_code);   /*  tail call */
     }

     vmerror_wrong_type(function);

     return NIL;
}

static lref_t *find_matching_escape(lref_t *start_frame, lref_t tag)
{
     if (CURRENT_TIB()->escape_frame != NULL)
          start_frame = fstack_prev_frame(CURRENT_TIB()->escape_frame);

     for(lref_t *frame = start_frame;
         frame != NULL;
         frame = fstack_prev_frame(frame))
     {
          if (fstack_frame_type(frame) != FRAME_ESCAPE)
               continue;

          lref_t ftag = frame[FOFS_ESCAPE_TAG];

          if (NULLP(ftag) || EQ(ftag, tag))
               return frame;
     }

     return NULL;
}

void unwind_stack_for_throw()
{
     for(lref_t *frame = CURRENT_TIB()->frame;
         frame != NULL;
         frame = fstack_prev_frame(frame))
     {
          if (fstack_frame_type(frame) == FRAME_UNWIND)
          {
               dscwritef(DF_SHOW_THROWS, (_T("; DEBUG: throw invoking unwind : ~c&\n"), frame));

               apply1(frame[FOFS_UNWIND_AFTER], 0, NULL);

               continue;
          }

          if (fstack_frame_type(frame) != FRAME_ESCAPE)
               continue;

          if (frame == CURRENT_TIB()->escape_frame)
          {
               dscwritef(DF_SHOW_THROWS, (_T("; DEBUG: setjmp (from fsp=~c&) to target frame: ~c&\n"), CURRENT_TIB()->fsp, frame));

               CURRENT_TIB()->escape_frame = NULL;

               CURRENT_TIB()->frame = (lref_t *)frame[FOFS_ESCAPE_FRAME];
               CURRENT_TIB()->fsp = CURRENT_TIB()->frame + 1;

               longjmp((struct __jmp_buf_tag *)frame[FOFS_ESCAPE_JMPBUF_PTR], 1);
          }
     }
}

static lref_t execute_fast_op(lref_t fop, lref_t env)
{
     lref_t retval = NIL;
     lref_t sym;
     lref_t binding;
     lref_t fn;
     lref_t args;
     size_t argc;
     lref_t argv[ARG_BUF_LEN];
     lref_t after;
     lref_t tag;
     lref_t escape_retval;
     lref_t *jmpbuf_ptr;
     jmp_buf *jmpbuf;

     STACK_CHECK(&fop);

     fstack_enter_frame(FRAME_EVAL);
     fstack_push((lref_t)&fop);
     fstack_push((lref_t)fop);
     fstack_push((lref_t)env);

     while(!NULLP(fop)) {
          CURRENT_TIB()->count_fop++;

          _process_interrupts();

#if defined(WITH_FOPLOG_SUPPORT)
          if (CURRENT_TIB()->foplog_enable) {
               CURRENT_TIB()->foplog[CURRENT_TIB()->foplog_index] = fop;
               CURRENT_TIB()->foplog_index = (CURRENT_TIB()->foplog_index + 1) % FOPLOG_SIZE;
          }
#endif

          switch (FAST_OP_OPCODE(fop))
          {
          case FOP_LITERAL:
               retval = FAST_OP_ARG1(fop);
               fop = FAST_OP_NEXT(fop);
               break;

          case FOP_GLOBAL_REF:
               sym = FAST_OP_ARG1(fop);
               binding = SYMBOL_VCELL(sym);

               if (UNBOUND_MARKER_P(binding))
                    vmerror_unbound(sym);

               retval = binding;

               fop = FAST_OP_NEXT(fop);
               break;

          case FOP_GLOBAL_SET:
               sym = FAST_OP_ARG1(fop);
               binding = SYMBOL_VCELL(sym);

               if (UNBOUND_MARKER_P(binding))
                    vmerror_unbound(sym);

               SET_SYMBOL_VCELL(sym, retval);

               fop = FAST_OP_NEXT(fop);
               break;

          case FOP_LOCAL_REF:
               sym = FAST_OP_ARG1(fop);
               binding = lenvlookup(sym, env);

               retval = CAR(binding);

               fop = FAST_OP_NEXT(fop);
               break;

          case FOP_LOCAL_SET:
               sym = FAST_OP_ARG1(fop);
               binding = lenvlookup(sym, env);

               SET_CAR(binding, retval);

               fop = FAST_OP_NEXT(fop);
               break;

          case FOP_APPLY_GLOBAL:
               sym = FAST_OP_ARG1(fop);
               fn = SYMBOL_VCELL(sym);

               if (UNBOUND_MARKER_P(fn))
                    vmerror_unbound(sym);

               argc = 0;
               args = FAST_OP_ARG2(fop);

               while (CONSP(args)) {
                    if (argc >= ARG_BUF_LEN) {
                         vmerror_unsupported(_T("too many actual arguments"));
                         break;
                    }

                    argv[argc] = execute_fast_op(CAR(args), env);

                    args = CDR(args);
                    argc++;
               }

               if (!NULLP(args))
                    vmerror_arg_out_of_range(FAST_OP_ARG2(fop),
                                             _T("bad formal argument list"));

               fop = apply(fn, argc, argv, &env, &retval);
               break;

          case FOP_APPLY:
               argc = 0;
               fn = execute_fast_op(FAST_OP_ARG1(fop), env);
               args = FAST_OP_ARG2(fop);

               while (CONSP(args)) {
                    if (argc >= ARG_BUF_LEN) {
                         vmerror_unsupported(_T("too many actual arguments"));
                         break;
                    }

                    argv[argc] = execute_fast_op(CAR(args), env);

                    args = CDR(args);
                    argc++;
               }

               if (!NULLP(args))
                    vmerror_arg_out_of_range(FAST_OP_ARG2(fop),
                                             _T("bad formal argument list"));

               fop = apply(fn, argc, argv, &env, &retval);
               break;

          case FOP_IF_TRUE:
               if (TRUEP(retval))
                    fop = FAST_OP_ARG1(fop);
               else
                    fop = FAST_OP_ARG2(fop);
               break;

          case FOP_RETVAL:
               fop = FAST_OP_NEXT(fop);
               break;

          case FOP_SEQUENCE:
               retval = execute_fast_op(FAST_OP_ARG1(fop), env);

               fop = FAST_OP_ARG2(fop);
               break;

          case FOP_THROW:
               tag = execute_fast_op(FAST_OP_ARG1(fop), env);
               escape_retval = execute_fast_op(FAST_OP_ARG2(fop), env);

               dscwritef(DF_SHOW_THROWS, (_T("; DEBUG: throw ~a :~a\n"), tag, escape_retval));

               CURRENT_TIB()->escape_frame = find_matching_escape(CURRENT_TIB()->frame, tag);
               CURRENT_TIB()->escape_value = escape_retval;

               unwind_stack_for_throw();

               /* If we don't find a matching catch for the throw, we have a
                * problem and need to invoke a trap. */
               vmtrap(TRAP_UNCAUGHT_THROW,
                      (enum vmt_options_t)(VMT_MANDATORY_TRAP | VMT_HANDLER_MUST_ESCAPE),
                      2, tag, escape_retval);

               fop = FAST_OP_NEXT(fop);
               break;

          case FOP_CATCH:
               tag = execute_fast_op(FAST_OP_ARG1(fop), env);

               fstack_enter_frame(FRAME_ESCAPE);
               fstack_push((lref_t)tag);
               fstack_push((lref_t)CURRENT_TIB()->frame);
               fstack_push(NIL);

               jmpbuf_ptr = CURRENT_TIB()->fsp;
               jmpbuf = (jmp_buf *)fstack_alloca(sizeof(jmp_buf));
               *(jmpbuf_ptr) = (lref_t)jmpbuf;

               if (setjmp(*jmpbuf) == 0) {
                    retval = execute_fast_op(FAST_OP_ARG2(fop), env);
               } else {
                    dscwritef(DF_SHOW_THROWS, (_T("; DEBUG: catch retval =~a\n"), CURRENT_TIB()->escape_value));

                    retval = CURRENT_TIB()->escape_value;
                    CURRENT_TIB()->escape_value = NIL;
               }

               fstack_leave_frame();

               fop = FAST_OP_NEXT(fop);
               break;

          case FOP_WITH_UNWIND_FN:
               fstack_enter_frame(FRAME_UNWIND);
               fstack_push((lref_t)execute_fast_op(FAST_OP_ARG1(fop), env));

               retval = execute_fast_op(FAST_OP_ARG2(fop), env);

               after = CURRENT_TIB()->frame[FOFS_UNWIND_AFTER];

               fstack_leave_frame();

               apply1(after, 0, NULL);

               fop = FAST_OP_NEXT(fop);
               break;

          case FOP_CLOSURE:
               retval = lclosurecons(env,
                                     lcons(lcar(FAST_OP_ARG1(fop)),
                                           FAST_OP_ARG2(fop)),
                                     lcdr(FAST_OP_ARG1(fop)));
               fop = FAST_OP_NEXT(fop);
               break;

          case FOP_CAR:
               retval = lcar(retval);
               fop = FAST_OP_NEXT(fop);
               break;

          case FOP_CDR:
               retval = lcdr(retval);
               fop = FAST_OP_NEXT(fop);
               break;

          case FOP_NOT:
               retval = boolcons(!TRUEP(retval));
               fop = FAST_OP_NEXT(fop);
               break;

          case FOP_NULLP:
               retval = boolcons(NULLP(retval));
               fop = FAST_OP_NEXT(fop);
               break;

          case FOP_EQP:
               retval = boolcons(EQ(execute_fast_op(FAST_OP_ARG1(fop), env),
                                    execute_fast_op(FAST_OP_ARG2(fop), env)));
               fop = FAST_OP_NEXT(fop);
               break;

          case FOP_GET_ENV:
               retval = env;
               fop = FAST_OP_NEXT(fop);
               break;

          case FOP_GLOBAL_DEF: // three args, third was genv, but currently unused
               retval = lidefine_global(FAST_OP_ARG1(fop), FAST_OP_ARG2(fop));
               fop = FAST_OP_NEXT(fop);
               break;

          case FOP_GET_FSP:
               retval = fixcons((fixnum_t)CURRENT_TIB()->fsp);
               fop = FAST_OP_NEXT(fop);
               break;

          case FOP_GET_FRAME:
               retval = fixcons((fixnum_t)CURRENT_TIB()->frame);
               fop = FAST_OP_NEXT(fop);
               break;

          case FOP_GET_HFRAMES:
               retval = CURRENT_TIB()->handler_frames;
               fop = FAST_OP_NEXT(fop);
               break;

          case FOP_SET_HFRAMES:
               CURRENT_TIB()->handler_frames = execute_fast_op(FAST_OP_ARG1(fop), env);
               fop = FAST_OP_NEXT(fop);
               break;

          case FOP_GLOBAL_PRESERVE_FRAME:
               sym = FAST_OP_ARG1(fop);
               binding = SYMBOL_VCELL(sym);

               if (UNBOUND_MARKER_P(binding))
                    vmerror_unbound(sym);

               SET_SYMBOL_VCELL(sym, fixcons((fixnum_t)CURRENT_TIB()->frame));

               retval = execute_fast_op(FAST_OP_ARG2(fop), env);
               fop = FAST_OP_NEXT(fop);
               break;

          default:
               panic("Unsupported fast-op");
          }
     }

     fstack_leave_frame();

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

lref_t lapply(size_t argc, lref_t argv[])
{
     size_t fn_argc = 0;
     lref_t fn_argv[ARG_BUF_LEN];

     lref_t fn = (argc > 0) ? argv[0] : NIL;

     if (!PROCEDUREP(fn))
          vmerror_wrong_type_n(1, fn);

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
     return NIL;
}

lref_t topmost_primitive()
{
     for(lref_t *frame = CURRENT_TIB()->frame;
         frame != NULL;
         frame = fstack_prev_frame(frame))
     {
          if (fstack_frame_type(frame) == FRAME_SUBR)
               return frame[FOFS_SUBR_SUBR];
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

