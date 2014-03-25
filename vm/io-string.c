
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

size_t string_port_read_chars(lref_t port, _TCHAR *buf, size_t size)
{
     size_t chars_read;

     lref_t port_str = PORT_STRING(port);
     size_t str_len = STRING_DIM(port_str);
     struct port_text_info_t *pti = PORT_TEXT_INFO(port);

     for (chars_read = 0; chars_read < size; chars_read++)
     {
          if (pti->str_ofs >= str_len)
               break;

          buf[chars_read] = STRING_DATA(port_str)[pti->str_ofs];

          pti->str_ofs++;
     }

     return chars_read;
}

size_t string_port_write_chars(lref_t port, const _TCHAR *buf, size_t size)
{
     if (NULLP(PORT_STRING(port)))
          SET_PORT_STRING(port, strconsbufn(size, buf)); 
     else
          string_appendd(PORT_STRING(port), buf, size);

     return size;
}

size_t string_port_length(lref_t port)
{
     if (NULLP(PORT_STRING(port)))
          return 0;
     else
          return STRING_DIM(PORT_STRING(port));
}

int text_port_peek_char(lref_t port);

struct port_class_t string_port_class = {
     _T("STRING"),

     NULL,                    // open
     NULL,                    // read_bytes
     NULL,                    // write_bytes
     text_port_peek_char,     // peek_char
     string_port_read_chars,  // read_chars
     string_port_write_chars, // write_chars
     NULL,                    // rich_write
     NULL,                    // flush
     NULL,                    // close
     NULL,                    // gc_free
     string_port_length,      // length
};


lref_t lopen_input_string(lref_t string)
{
     if (!STRINGP(string))
          vmerror_wrong_type_n(1, string);

     lref_t port =
          portcons(&string_port_class, NIL, PORT_INPUT, strconsdup(string), NULL);

     struct port_text_info_t *pti = allocate_text_info();

     pti->str_ofs = 0;

     SET_PORT_TEXT_INFO(port, pti);

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

