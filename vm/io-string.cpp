
/* io-string.cpp
 *
 * String port. Allows for input from and output to strings.
 */

#include "scan.h"

BEGIN_NAMESPACE(scan)

INLINE void SET_PORT_STRING(LRef port, LRef string)
{
     assert(STRINGP(string));

     PORT_PINFO(port)->_user_object = string;
}

INLINE LRef PORT_STRING(LRef port)
{
     return PORT_PINFO(port)->_user_object;
}

size_t string_port_read(void *buf, size_t size, size_t count, LRef obj)
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

size_t string_port_write(const void *buf, size_t size, size_t count, LRef obj)
{
     assert(size == sizeof(_TCHAR));

     if (NULLP(PORT_STRING(obj)))
          SET_PORT_STRING(obj, strcons(count, (_TCHAR *) buf)); /*  REVISIT: fails if buf has embedded nulls */
     else
          str_append_str(PORT_STRING(obj), (_TCHAR *) buf, count);

     return count;
}

size_t string_port_length(LRef port)
{
     if (NULLP(PORT_STRING(port)))
          return 0;
     else
          return STRING_DIM(PORT_STRING(port));
}

port_class_t string_port_class = {
     _T("STRING"),
     PORT_INPUT_OUTPUT,

     NULL,

     NULL,
     string_port_read,
     string_port_write,
     NULL,

     NULL,
     NULL,
     NULL,

     string_port_length,
};


LRef lopen_input_string(LRef string)
{
     if (!STRINGP(string))
          vmerror_wrong_type(1, string);

     /*  TODO: open-input-string can avoid duplicating incoming strings */
     /*  TODO: open-input-string take string input port argument */

     return portcons(&string_port_class, NIL, PORT_INPUT, strcons(string), NULL);
}

LRef lopen_output_string()      /*  REVISIT: default string/length in oos? */
{
     return portcons(&string_port_class, NIL, PORT_OUTPUT, NIL, NULL);
}

LRef lget_output_string(LRef port)
{
     if (!PORTP(port))
          vmerror_wrong_type(1, port);

     lflush_port(port);

     if (NULLP(PORT_STRING(port)))
          return strcons("");
     else
          return PORT_STRING(port);
}

LRef lread_port_to_string(LRef port)
{
     LRef os = lopen_output_string();

     for (;;)
     {
          int ch = read_char(port);

          if (ch == EOF)
               break;

          write_char(ch, os);
     }

     return lget_output_string(os);
}


END_NAMESPACE
