
/* slib_err.c
 *
 * SIOD error handling code.
 *
 * This implements a simple exception handling mechanism, reborrowed
 * from the original SIOD. Parallel to the execution stack, a stack
 * of catch frames is maintained. Throwing an error searches back up
 * the catch frame list and longjmps back to that frame's stack frame.
 */

#include "scan.h"

BEGIN_NAMESPACE(scan)

/**************************************************************
 * Error Reporting
 **************************************************************/

/* The basic mechanism for error reporting are the lerror
 * and err calls.
 *
 * To report an error, each prints an error message to
 * (current-error-port) and throws an errobj exception.
 * The in-passed object is stored in the errobj symbol's
 * value binding.
 */
bool primitive_frame(frame_record_t * rec, uptr notused)
{
     UNREFERENCED(notused);

     return (rec->type == FRAME_PRIMITIVE);
}

LRef topmost_primitive()
{
     frame_record_t *f;

     f = __frame_find(primitive_frame, (uptr) NIL);

     return f ? f->frame_as.primitive.function : NIL;
}


static bool in_stack_overflow = false;

void vmerror_stack_overflow(u8 * obj)
{
     UNREFERENCED(obj);

     if (in_stack_overflow)
          return;

     in_stack_overflow = true;

     if (DEBUGGING_BUILD)
          dump_current_frames(CURRENT_DEBUG_PORT());

     panic("Stack Overflow!");

     /* TODO: Stack overflow should throw out to a catch block and then
      * invoke a overflow handler.
      * 
      * REVISIT: Should the user be allowed to continue after an overflow? */

     THROW_ESCAPE(interp.sym_stack_overflow, NIL);
}

bool infop()                    /* REVISIT: still used? */
{
     assert(SYMBOLP(interp.sym_msglvl_info));

     return TRUEP(SYMBOL_VCELL(interp.sym_msglvl_info));
}


void info(const _TCHAR * message, ...)
{
     va_list arglist;
     va_start(arglist, message);

     if (infop())               /*  this is the call in info (for find in files) */
     {
          WRITE_TEXT_CONSTANT(_T("; Info: "), CURRENT_ERROR_PORT());
          scvwritef(message, CURRENT_ERROR_PORT(), arglist);
          lnewline(CURRENT_ERROR_PORT());
     }
}

LRef vmsignal(const _TCHAR * signal_name, long n, ...)
{
     va_list args;
     va_start(args, n);

     LRef signal_args = listv(n, args);

     va_end(args);

     signal_args = lcons(simple_intern(signal_name, interp.scheme_package), signal_args);

     dscwritef(DF_SHOW_VMSIGNALS, _T("; DEBUG: vm-signal :~cS : ~s\n"), signal_name, signal_args);

     if (NULLP(CURRENT_VM_SIGNAL_HANDLER()))
          panic("VM condition signaled without registered handler.\n");

     if (!PROCEDUREP(CURRENT_VM_SIGNAL_HANDLER()))
          panic("Invalid VM signal handler, must be a procedure.\n");

     return napply(CURRENT_VM_SIGNAL_HANDLER(), 1, signal_args);
}

/*  REVISIT: lots of errors could be improved by adding ~s to print the error object */
LRef vmerror(const _TCHAR * message, LRef new_errobj)
{
     assert(message);

     LRef err_primitive = topmost_primitive();

     dscwritef(DF_SHOW_VMERRORS,
               _T("; DEBUG: runtime error: ~cS : errobj=~s\n"), message, new_errobj);

     if (NULLP(CURRENT_VM_RUNTIME_ERROR_HANDLER()))
          panic("VM runtime error without registered handler.\n");

     if (!CLOSUREP(CURRENT_VM_RUNTIME_ERROR_HANDLER()) && !SUBRP(CURRENT_VM_RUNTIME_ERROR_HANDLER()))
          panic("Invalid VM error handler, must be a procedure.\n");

     napply(CURRENT_VM_RUNTIME_ERROR_HANDLER(), 4, strcons(message), err_primitive, new_errobj, NIL);

     /* Execution should never get to this point...
      *
      * Every extant call to error (at the beginnning of 2006) was written
      * with the assumption that it would abort the current primitive.
      * However, with the signal-based error handling logic, if the
      * CURRENT_VM_RUNTIME_ERROR_HANDLER does not ultimately result in
      * an escape, this assumption will be violated. Thusly, we panic
      * if that ever happens.
      */
     panic("VM errors must result in a non-local escape out of the signaling primitive.");

     return NIL;
}

LRef vmerror_wrong_type(LRef new_errobj)
{

     return vmerror_wrong_type(-1, new_errobj);
}

LRef vmerror_wrong_type(int which_argument, LRef new_errobj)
{
     _TCHAR buffer[STACK_STRBUF_LEN];

     const _TCHAR *msg = NULL;

     switch (which_argument)
     {
     case 1:
          msg = _T(", 1st argument");
          break;
     case 2:
          msg = _T(", 2nd argument");
          break;
     case 3:
          msg = _T(", 3rd argument");
          break;
     case 4:
          msg = _T(", 4th argument");
          break;
     case 5:
          msg = _T(", 5th argument");
          break;
     default:
          msg = _T("");
          break;
     }

     _sntprintf(buffer, STACK_STRBUF_LEN, "Argument type mismatch%s", msg);

     return vmerror(buffer, new_errobj);
}

LRef lpanic(LRef msg)           /*  If everything goes to hell, call this... */
{
     if (STRINGP(msg))
          panic(get_c_string(msg));
     else
          panic("Invalid string passed to %panic!\n");

     return NIL;
}


END_NAMESPACE
