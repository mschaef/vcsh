
/*
 * io-string.cpp --
 *
 * String port. Allows for input from and output to strings.
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

INLINE void SET_PORT_STRING(lref_t port, lref_t string)
{
     assert(STRINGP(string));

     SET_PORT_USER_OBJECT(port, string);
}

INLINE lref_t PORT_STRING(lref_t port)
{
     return PORT_USER_OBJECT(port);
}
size_t string_port_read(void *buf, size_t size, size_t count, lref_t obj)
{
     size_t bytes_read;

     /*  REVISIT: This is a pretty bad string_port_read */

     assert(size == sizeof(_TCHAR));

     _TCHAR *tbuf = (_TCHAR *) buf;

     for (bytes_read = 0; bytes_read < count; bytes_read++)
     {
          int ch = str_next_character(PORT_STRING(obj));

          if (ch == EOF)
               break;

          tbuf[bytes_read] = (_TCHAR) ch;
     }

     return bytes_read;
}

size_t string_port_write(const void *buf, size_t size, size_t count, lref_t obj)
{
     assert(size == sizeof(_TCHAR));

     if (NULLP(PORT_STRING(obj)))
          SET_PORT_STRING(obj, strcons(count, (_TCHAR *) buf)); /*  REVISIT: fails if buf has embedded nulls */
     else
          str_append_str(PORT_STRING(obj), (_TCHAR *) buf, count);

     return count;
}

size_t string_port_length(lref_t port)
{
     if (NULLP(PORT_STRING(port)))
          return 0;
     else
          return STRING_DIM(PORT_STRING(port));
}

port_class_t string_port_class = {
     _T("STRING"),

     NULL,                // open
     string_port_read,    // read_bytes
     string_port_write,   // write_bytes
     NULL,                // rich_write
     NULL,                // flush
     NULL,                // close
     NULL,                // gc_free
     string_port_length,  // length
};


lref_t lopen_input_string(lref_t string)
{
     if (!STRINGP(string))
          vmerror_wrong_type(1, string);

     /*  REVISIT: open-input-string can avoid duplicating incoming strings */
     /*  REVISIT: open-input-string take string input port argument */

     return portcons(&string_port_class, NIL, PORT_INPUT, strcons(string), NULL);
}

lref_t lopen_output_string()      /*  REVISIT: default string/length in oos? */
{
     return portcons(&string_port_class, NIL, PORT_OUTPUT, NIL, NULL);
}

lref_t lget_output_string(lref_t port)
{
     if (!PORTP(port))
          vmerror_wrong_type(1, port);

     lflush_port(port);

     if (NULLP(PORT_STRING(port)))
          return strcons("");
     else
          return PORT_STRING(port);
}

END_NAMESPACE
