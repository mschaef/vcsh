/*
 * io-string.c --
 *
 * String port. Allows for input from and output to strings.
 *
 * (C) Copyright 2001-2022 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "LICENSE" for information on usage and redistribution
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

size_t input_string_port_length(lref_t port)
{
     return string_length(PORT_STRING(port));
}

int input_string_port_peek_char(lref_t port)
{
     lref_t port_str = PORT_STRING(port);
     struct port_text_info_t *pti = PORT_TEXT_INFO(port);

     if (pti->str_ofs >= string_length(port_str))
          return -1;

     return string_ref(port_str, pti->str_ofs);
}

size_t input_string_port_read_chars(lref_t port, _TCHAR *buf, size_t size)
{
     size_t chars_read;
     lref_t port_str = PORT_STRING(port);

     struct port_text_info_t *pti = PORT_TEXT_INFO(port);

     for (chars_read = 0; chars_read < size; chars_read++) {
          if (pti->str_ofs >= string_length(port_str))
               break;

          buf[chars_read] = string_ref(port_str, pti->str_ofs);

          pti->str_ofs++;
     }

     return chars_read;
}

struct port_class_t input_string_port_class = {
     _T("STRING-INPUT"),

     NULL,                         // open
     NULL,                         // read_bytes
     NULL,                         // write_bytes
     input_string_port_peek_char,  // peek_char
     input_string_port_read_chars, // read_chars
     NULL,                         // write_chars
     NULL,                         // rich_write
     NULL,                         // flush
     NULL,                         // close
     NULL,                         // gc_free
     input_string_port_length,     // length
};

lref_t lopen_input_string(lref_t string)
{
     if (!STRINGP(string))
          vmerror_wrong_type_n(1, string);

     lref_t port =
          portcons(&input_string_port_class, NIL, PORT_INPUT | PORT_TEXT, strconsdup(string), NULL);

     struct port_text_info_t *pti = allocate_text_info();

     pti->str_ofs = 0;

     SET_PORT_TEXT_INFO(port, pti);

     return port;
}

size_t output_string_port_length(lref_t port)
{
     return string_length(PORT_STRING(port));
}

size_t output_string_port_write_chars(lref_t port, const _TCHAR *buf, size_t size)
{
     for(size_t index = 0; index < size; index++) {
          switch(buf[index]) {
          case _T('\n'):
               PORT_TEXT_INFO(port)->col = 0;
               PORT_TEXT_INFO(port)->row ++;
               break;

          case _T('\r'):
               break;

          default:
               PORT_TEXT_INFO(port)->col++;
          }
     }

     string_appendd(PORT_STRING(port), buf, size);

     return size;
}

struct port_class_t output_string_port_class = {
     _T("STRING-OUTPUT"),

     NULL,                           // open
     NULL,                           // read_bytes
     NULL,                           // write_bytes
     NULL,                           // peek_char
     NULL,                           // read_chars
     output_string_port_write_chars, // write_chars
     NULL,                           // rich_write
     NULL,                           // flush
     NULL,                           // close
     NULL,                           // gc_free
     output_string_port_length,      // length
};

lref_t lopen_output_string()
{
     lref_t port = portcons(&output_string_port_class, NIL, PORT_OUTPUT | PORT_TEXT, NIL, NULL);

     SET_PORT_TEXT_INFO(port, allocate_text_info());
     SET_PORT_STRING(port, strconsbuf(_T("")));

     return port;
}

lref_t lget_output_string(lref_t port)
{
     if (!PORTP(port))
          vmerror_wrong_type_n(1, port);

     lflush_port(port);

     return PORT_STRING(port);
}

