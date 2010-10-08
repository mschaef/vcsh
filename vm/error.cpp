
/* error.cpp
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
bool primitive_frame(frame_record_t * rec, uptr_t notused)
{
     UNREFERENCED(notused);

     return (rec->type == FRAME_PRIMITIVE);
}

LRef topmost_primitive()
{
     frame_record_t *f;

     f = __frame_find(primitive_frame, (uptr_t) NIL);

     return f ? f->frame_as.primitive.function : NIL;
}


void vmerror_stack_overflow(u8_t * obj)
{
     UNREFERENCED(obj);

     panic("Stack Overflow!");

     /* TODO: Stack overflow should throw out to a catch block and then
      * invoke a overflow handler.
      * 
      * REVISIT: Should the user be allowed to continue after an overflow? */
}



void panic_on_bad_trap_handler(trap_type_t trap)
{
     _TCHAR buf[STACK_STRBUF_LEN];

     _sntprintf(buf, STACK_STRBUF_LEN, _T("Trap with bad handler: %s"), trap_type_name(trap));

     panic(buf);
}

LRef invoke_trap_handler(trap_type_t trap, bool allow_empty_handler, size_t argc, ...)
{
     assert((trap > 0) && (trap <= TRAP_LAST));

     dscwritef(DF_SHOW_TRAPS, _T("; DEBUG: trap : ~cS\n"), trap_type_name(trap));

     va_list args;
     va_start(args, argc);
     
     LRef handler = interp.trap_handlers[trap];
     LRef retval = NIL;

     if (!PROCEDUREP(handler))
     {
          if (!(NULLP(handler) && allow_empty_handler))
          {
               va_end(args);
               panic_on_bad_trap_handler(trap);
          }
     }

     if (!NULLP(handler))
          retval = napplyv(handler, argc, args);

     va_end(args);

     return retval;
}

LRef vmsignal(const _TCHAR * signal_name, long n, ...)
{
     va_list args;
     va_start(args, n);

     LRef signal_args = listv(n, args);

     va_end(args);

     signal_args = lcons(simple_intern(signal_name, interp.scheme_package), signal_args);

     return invoke_trap_handler(TRAP_SIGNAL, false, 2, signal_args, NIL);
}

/*  REVISIT: lots of errors could be improved by adding ~s to print the error object */
LRef vmerror(const _TCHAR * message, LRef new_errobj)
{
     assert(message);

     LRef err_primitive = topmost_primitive();

     dscwritef(DF_SHOW_VMERRORS,
               _T("; DEBUG: runtime error: ~cS : errobj=~s\n"), message, new_errobj);


     invoke_trap_handler(TRAP_RUNTIME_ERROR, false, 4, strcons(message), err_primitive, new_errobj,  NIL);

     /* Execution should never get to this point...
      *
      * Every extant call to error (at the beginnning of 2006) was written
      * with the assumption that it would abort the current primitive.
      * However, with the signal-based error handling logic, if the
      * runtime error trap handler does not ultimately result in
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
