
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

     return f ? f->as.prim.function : NIL;
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

void vmerror_wrong_type(LRef new_errobj)
{
     vmerror_wrong_type(-1, new_errobj);
}

void vmerror_wrong_type(int which_argument, LRef new_errobj)
{
     vmtrap(TRAP_WRONG_TYPE, (vmt_options_t)(VMT_MANDATORY_TRAP | VMT_HANDLER_MUST_ESCAPE),
            3, topmost_primitive(), fixcons(which_argument), new_errobj);
}

void vmerror_unbound(LRef v)
{
     vmtrap(TRAP_UNBOUND_GLOBAL, (vmt_options_t)(VMT_MANDATORY_TRAP | VMT_HANDLER_MUST_ESCAPE),
            2, topmost_primitive(), v);
}

void vmerror_index_out_of_bounds(LRef index, LRef obj)
{
     vmtrap(TRAP_INDEX_OUT_OF_BOUNDS, (vmt_options_t)(VMT_MANDATORY_TRAP | VMT_HANDLER_MUST_ESCAPE),
            3, topmost_primitive(), index, obj);
}

void vmerror_arg_out_of_range(LRef arg, const _TCHAR *range_desc /* = NULL */)
{
     LRef range = NIL;

     if (range_desc)
          range = strcons(range_desc);

     vmtrap(TRAP_ARG_OUT_OF_RANGE, (vmt_options_t)(VMT_MANDATORY_TRAP | VMT_HANDLER_MUST_ESCAPE),
            3, topmost_primitive(), arg, range);
}

void vmerror_unsupported(const _TCHAR *desc)
{
     vmtrap(TRAP_UNSUPPORTED, (vmt_options_t)(VMT_MANDATORY_TRAP | VMT_HANDLER_MUST_ESCAPE),
            2,  topmost_primitive(),  strcons(desc));
}

void vmerror_unimplemented(const _TCHAR *desc)
{
     vmtrap(TRAP_UNIMPLEMENTED, (vmt_options_t)(VMT_MANDATORY_TRAP | VMT_HANDLER_MUST_ESCAPE),
            2, topmost_primitive(), strcons(desc));
}

void vmerror_divide_by_zero()
{
     vmtrap(TRAP_DIVIDE_BY_ZERO, (vmt_options_t)(VMT_MANDATORY_TRAP | VMT_HANDLER_MUST_ESCAPE),
            1, topmost_primitive());
}

void vmerror_io_error(const _TCHAR *desc, LRef info)
{
     vmtrap(TRAP_IO_ERROR, (vmt_options_t)(VMT_MANDATORY_TRAP | VMT_HANDLER_MUST_ESCAPE),
            3, topmost_primitive(), strcons(desc), info);
}

void fast_read_error(const _TCHAR * message, LRef port, LRef details /* = NIL */)
{
     /*  REVISIT: fast_read_errors don't always show valid port locations */
     assert(PORTP(port));

     LRef location = lport_location(port);

     vmtrap(TRAP_FAST_READ_ERROR, (vmt_options_t)(VMT_MANDATORY_TRAP | VMT_HANDLER_MUST_ESCAPE),
            5, topmost_primitive(), strcons(message), port, location, details);
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
