
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

INLINE void SET_PORT_STRING(lref_t port, lref_t string)
{
     assert(STRINGP(string));

     SET_PORT_USER_OBJECT(port, string);
}

INLINE lref_t PORT_STRING(lref_t port)
{
     return PORT_USER_OBJECT(port);
}

size_t string_port_read_bytes(lref_t port, void *buf, size_t size)
{
     size_t bytes_read;

     _TCHAR *tbuf = (_TCHAR *)buf;

     for (bytes_read = 0; bytes_read < size; bytes_read++)
     {
          int ch = str_next_character(PORT_STRING(port));

          if (ch == EOF)
               break;

          tbuf[bytes_read] = (_TCHAR)ch;
     }

     return bytes_read;
}

size_t string_port_write_bytes(lref_t port, const void *buf, size_t size)
{
     if (NULLP(PORT_STRING(port)))
          SET_PORT_STRING(port, strconsbufn(size, (_TCHAR *)buf)); 
     else
          string_appendd(PORT_STRING(port), (_TCHAR *) buf, size);

     return size;
}

size_t string_port_length(lref_t port)
{
     if (NULLP(PORT_STRING(port)))
          return 0;
     else
          return STRING_DIM(PORT_STRING(port));
}

struct port_class_t string_port_class = {
     _T("STRING"),

     NULL,                    // open
     string_port_read_bytes,  // read_bytes
     string_port_write_bytes, // write_bytes
     NULL,                    // rich_write
     NULL,                    // flush
     NULL,                    // close
     NULL,                    // gc_free
     string_port_length,      // length
};


struct port_text_info_t *allocate_text_info();

lref_t lopen_input_string(lref_t string)
{
     if (!STRINGP(string))
          vmerror_wrong_type_n(1, string);

     lref_t port =
          portcons(&string_port_class, NIL, PORT_INPUT, strconsdup(string), NULL);

     SET_PORT_TEXT_INFO(port, allocate_text_info());

     return port;
}

lref_t lopen_output_string()
{
     lref_t port = portcons(&string_port_class, NIL, PORT_OUTPUT, NIL, NULL);

     SET_PORT_TEXT_INFO(port, allocate_text_info());

     return port;
}

lref_t lget_output_string(lref_t port)
{
     if (!PORTP(port))
          vmerror_wrong_type_n(1, port);

     lflush_port(port);

     if (NULLP(PORT_STRING(port)))
          return strconsbuf(_T(""));

     return PORT_STRING(port);
}

