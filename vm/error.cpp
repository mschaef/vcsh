
/*
 * error.cpp --
 *
 * Entry points for standard kinds of VM errors.
 *
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include "scan-private.h"

BEGIN_NAMESPACE(scan)

/*** Interpreter Panic ***/

static panic_handler_t current_panic_handler = NULL;

panic_handler_t set_panic_handler(panic_handler_t new_handler)
{
     panic_handler_t old_handler = current_panic_handler;

     current_panic_handler = new_handler;

     return old_handler;
}

static bool in_panic = false;

void _panic(const _TCHAR * str, const _TCHAR * filename, long lineno)
{
     _TCHAR buf[DEBUG_MESSAGE_BUF_SIZE];

     _sntprintf(buf, DEBUG_MESSAGE_BUF_SIZE,
                in_panic ? "Double Panic, Aborting: %s @ (%s:%ld)\n" : "Panic: %s @ (%s:%ld)\n",
                str, filename, lineno);

     sys_output_debug_string(buf);

     if (!in_panic && (current_panic_handler != NULL))
     {
          in_panic = true;
          current_panic_handler();
     }

     sys_abnormally_terminate_vm(1);
}

/*** Default Error Handlers ***/

void vmerror_stack_overflow(uint8_t * obj)
{
     UNREFERENCED(obj);

     panic("Stack Overflow!");

     /* TODO: Stack overflow should throw out to a catch block and then
      * invoke a overflow handler.
      *
      * REVISIT: Should the user be allowed to continue after an overflow? */
}

void vmerror_wrong_type(lref_t new_errobj)
{
     vmerror_wrong_type(-1, new_errobj);
}

void vmerror_wrong_type(int which_argument, lref_t new_errobj)
{
     vmtrap(TRAP_WRONG_TYPE, (vmt_options_t)(VMT_MANDATORY_TRAP | VMT_HANDLER_MUST_ESCAPE),
            3, topmost_primitive(), fixcons(which_argument), new_errobj);
}

void vmerror_unbound(lref_t v)
{
     vmtrap(TRAP_UNBOUND_GLOBAL, (vmt_options_t)(VMT_MANDATORY_TRAP | VMT_HANDLER_MUST_ESCAPE),
            2, topmost_primitive(), v);
}

void vmerror_index_out_of_bounds(lref_t index, lref_t obj)
{
     vmtrap(TRAP_INDEX_OUT_OF_BOUNDS, (vmt_options_t)(VMT_MANDATORY_TRAP | VMT_HANDLER_MUST_ESCAPE),
            3, topmost_primitive(), index, obj);
}

void vmerror_arg_out_of_range(lref_t arg, const _TCHAR *range_desc /* = NULL */)
{
     lref_t range = NIL;

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

void vmerror_io_error(const _TCHAR *desc, lref_t info)
{
     vmtrap(TRAP_IO_ERROR, (vmt_options_t)(VMT_MANDATORY_TRAP | VMT_HANDLER_MUST_ESCAPE),
            3, topmost_primitive(), strcons(desc), info);
}

void vmerror_fast_read(const _TCHAR * message, lref_t reader, lref_t details /* = NIL */)
{
     /*  REVISIT: vmerror_fast_read don't always show valid port locations */
     assert(FASL_READER_P(reader));

     lref_t location = lport_location(FASL_READER_PORT(reader));

     vmtrap(TRAP_FAST_READ_ERROR, (vmt_options_t)(VMT_MANDATORY_TRAP | VMT_HANDLER_MUST_ESCAPE),
            5, topmost_primitive(), strcons(message), reader, location, details);
}


lref_t lpanic(lref_t msg)           /*  If everything goes to hell, call this... */
{
     if (STRINGP(msg))
          panic(get_c_string(msg));
     else
          panic("Invalid string passed to %panic!\n");

     return NIL;
}

END_NAMESPACE
