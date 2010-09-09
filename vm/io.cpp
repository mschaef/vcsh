
/* io.cpp
 *
 * Slib Input/Output code. This file contains the implementation
 * for ports in general, as well as ports for console I/O, string
 * I/O, and file I/O. It should be compliant with the port
 * procdures outlined in R5RS.
 */

#include "scan.h"

BEGIN_NAMESPACE(scan)

/*  REVISIT: It'd be nice to have an 'tee' port to write output to multiple destination ports. */

/* End-of-file object ****************************************
 *
 * This is the unreadable object that the R5RS specification talks
 * about using to mark the end of a file.
 */
LRef lmake_eof()
{
     return LREF2_CONS(LREF2_EOF, 0);
}

LRef leof_objectp(LRef obj)
{
     return EOFP(obj) ? obj : boolcons(false);
}

/* Port object ************************************************
 *
 * This represents a standard input or output port.  */

LRef port_gc_mark(LRef obj)
{
     gc_mark(PORT_PINFO(obj)->_port_name);
     gc_mark(PORT_PINFO(obj)->_user_object);

     for (size_t ii = 0; ii < FAST_LOAD_STACK_DEPTH; ii++)
          gc_mark(PORT_PINFO(obj)->_fasl_stack[ii]);

     return PORT_PINFO(obj)->_fasl_table;
}

void port_gc_free(LRef port)
{
     assert(PORTP(port));
     assert(PORT_CLASS(port));

     if (PORT_CLASS(port)->_close)
          PORT_CLASS(port)->_close(port);

     if (PORT_TEXT_INFO(port))
          safe_free(PORT_TEXT_INFO(port));

     if (PORT_CLASS(port)->_gc_free)
          PORT_CLASS(port)->_gc_free(port);

     safe_free(PORT_PINFO(port));
}

static LRef initialize_port(LRef s,
                            port_class_t * cls,
                            LRef port_name, port_mode_t mode, LRef user_object, void *user_data)
{
     bool binary = (mode & PORT_BINARY) == PORT_BINARY;

     assert(cls != NULL);
     assert(cls->_valid_modes & mode);
     assert(!NULLP(s));

     SET_PORT_PINFO(s, (port_info_t *) safe_malloc(sizeof(port_info_t)));
     SET_PORT_CLASS(s, cls);

     PORT_PINFO(s)->_port_name = port_name;
     PORT_PINFO(s)->_user_data = user_data;
     PORT_PINFO(s)->_user_object = user_object;
     PORT_PINFO(s)->_fasl_table = NIL;

     PORT_PINFO(s)->_fasl_stack_ptr = 0;

     for (size_t ii = 0; ii < FAST_LOAD_STACK_DEPTH; ii++)
          PORT_PINFO(s)->_fasl_stack[ii] = NIL;

     PORT_PINFO(s)->_mode = mode;

     PORT_PINFO(s)->_bytes_read = 0;
     PORT_PINFO(s)->_bytes_written = 0;

     if (binary)
     {
          SET_PORT_TEXT_INFO(s, NULL);
     }
     else
     {
          SET_PORT_TEXT_INFO(s,
                             (port_text_translation_info_t *)
                             safe_malloc(sizeof(port_text_translation_info_t)));

          memset(PORT_TEXT_INFO(s)->_unread_buffer, 0, sizeof(PORT_TEXT_INFO(s)->_unread_buffer));
          PORT_TEXT_INFO(s)->_unread_valid = 0;

          sys_info_t sinf;
          sys_get_info(&sinf);

          PORT_TEXT_INFO(s)->_crlf_translate = (sinf._eoln == SYS_EOLN_CRLF);

          PORT_TEXT_INFO(s)->_needs_lf = FALSE;
          PORT_TEXT_INFO(s)->_column = 0;
          PORT_TEXT_INFO(s)->_row = 1;
          PORT_TEXT_INFO(s)->_previous_line_length = 0;
     }


     if (PORT_CLASS(s)->_open)
          PORT_CLASS(s)->_open(s);

     return (s);
}

LRef portcons(port_class_t * cls, LRef port_name, port_mode_t mode, LRef user_object,
              void *user_data)
{
     LRef s = new_cell(TC_PORT);

     return initialize_port(s, cls, port_name, mode, user_object, user_data);
}


/*
 * Fundamental Port I/O Primatives - All I/O goes through these
 * two functions. They basically write and read raw streams of
 * bytes to and from ports.
 */

size_t write_raw(const void *buf, size_t size, size_t count, LRef port)
{
     if (NULLP(port))
          port = CURRENT_OUTPUT_PORT();

     assert(!NULLP(port));
     assert(PORT_CLASS(port)->_write);

     PORT_PINFO(port)->_bytes_written += (size * count);

     return PORT_CLASS(port)->_write(buf, size, count, port);
}

size_t read_raw(void *buf, size_t size, size_t count, LRef port)
{
     if (NULLP(port))
          port = CURRENT_INPUT_PORT();

     assert(!NULLP(port));
     assert(PORT_CLASS(port)->_read);

     size_t actual_count = PORT_CLASS(port)->_read(buf, size, count, port);

     PORT_PINFO(port)->_bytes_read += (size * actual_count);

     return actual_count;
}



/*
 * Lisp-visible port functions
 */

LRef linput_portp(LRef obj)
{
     if (PORTP(obj) && (PORT_MODE(obj) & PORT_INPUT))
          return obj;
     else
          return boolcons(false);
}

LRef loutput_portp(LRef obj)
{
     if (PORTP(obj) && (PORT_MODE(obj) & PORT_OUTPUT))
          return obj;
     else
          return boolcons(false);
}

LRef lbinary_portp(LRef obj)
{
     if (PORTP(obj) && (PORT_BINARYP(obj)))
          return obj;
     else
          return boolcons(false);
}

LRef lport_mode(LRef obj)
{
     if (!PORTP(obj))
          vmerror_wrong_type(1, obj);

     switch (PORT_MODE(obj) & PORT_DIRECTION)
     {
     case PORT_CLOSED:
          return keyword_intern(_T("closed"));
     case PORT_INPUT:
          return keyword_intern(_T("input"));
     case PORT_OUTPUT:
          return keyword_intern(_T("output"));
     case PORT_INPUT_OUTPUT:
          return keyword_intern(_T("input/output"));
     }

     panic(_T("corrupt port"));

     return NIL;
}


LRef lport_name(LRef port)
{
     if (NULLP(port))
          port = CURRENT_INPUT_PORT();

     if (!PORTP(port))
          vmerror_wrong_type(1, port);

     return PORT_PINFO(port)->_port_name;
}

LRef lport_location(LRef port)
{
     if (NULLP(port))
          port = CURRENT_INPUT_PORT();

     if (!PORTP(port))
          vmerror_wrong_type(1, port);

     if (PORT_BINARYP(port))
          return fixcons(PORT_PINFO(port)->_bytes_read);
     else
          return lcons(fixcons(PORT_TEXT_INFO(port)->_row), fixcons(PORT_TEXT_INFO(port)->_column));
}

LRef lport_translate_mode(LRef port)
{
     if (!PORTP(port))
          vmerror_wrong_type(1, port);

     if (PORT_BINARYP(port))
          return boolcons(false);
     else
          return boolcons(PORT_TEXT_INFO(port)->_crlf_translate);
}

LRef lport_set_translate_mode(LRef port, LRef mode)
{
     if (!PORTP(port))
          vmerror_wrong_type(1, port);
     if (!BOOLP(mode))
          vmerror_wrong_type(2, mode);

     if (PORT_BINARYP(port))
          vmerror("Cannot set translation mode of binary ports!", port);

     lflush_port(port);

     bool old_translate_mode = PORT_TEXT_INFO(port)->_crlf_translate;

     PORT_TEXT_INFO(port)->_crlf_translate = TRUEP(mode);

     return boolcons(old_translate_mode);
}

LRef lport_io_counts(LRef port)
{
     if (!PORTP(port))
          vmerror_wrong_type(1, port);

     return lcons(fixcons(PORT_PINFO(port)->_bytes_read),
                  fixcons(PORT_PINFO(port)->_bytes_written));
}

LRef lclose_port(LRef port)
{
     if (!PORTP(port))
          vmerror_wrong_type(1, port);

     if (PORT_PINFO(port)->_mode & PORT_OUTPUT)
          lflush_port(port);

     if (PORT_CLASS(port)->_close)
          PORT_CLASS(port)->_close(port);

     PORT_PINFO(port)->_mode = PORT_CLOSED;

     return port;
}

LRef lflush_port(LRef port)
{
     if (!PORTP(port))
          vmerror_wrong_type(1, port);

     if ((!PORT_BINARYP(port))
         && (PORT_TEXT_INFO(port)->_crlf_translate) && (PORT_TEXT_INFO(port)->_needs_lf))
     {
          write_char(_T('\n'), port);
     }

     if (PORT_CLASS(port)->_flush)
          PORT_CLASS(port)->_flush(port);

     return port;
}


/* Text I/O ***************************************************
 *
 * The Text I/O translation state machine resides below. */

int read_char(LRef port)
{
     int ch = EOF;

     if (NULLP(port))
          port = CURRENT_INPUT_PORT();

     assert(!NULLP(port));

     if (!(PORT_MODE(port) & PORT_INPUT))
          return ch;

     /* Read the next character, perhaps from the unread buffer... */
     if (!PORT_BINARYP(port) && (PORT_TEXT_INFO(port)->_unread_valid > 0))
     {
          PORT_TEXT_INFO(port)->_unread_valid--;

          ch = PORT_TEXT_INFO(port)->_unread_buffer[PORT_TEXT_INFO(port)->_unread_valid];
     }
     else
     {
          _TCHAR tch;

          if (read_raw(&tch, sizeof(_TCHAR), 1, port) == 0)
               ch = EOF;
          else
               ch = tch;

          /* ... Text ports get special processing. */
          if (!PORT_BINARYP(port))
          {
               /* _crlf_translate mode forces all input newlines (CR, LF, CR+LF) into LF's. */
               if (PORT_TEXT_INFO(port)->_crlf_translate)
               {
                    if (ch == '\r')
                    {
                         ch = '\n';
                         PORT_TEXT_INFO(port)->_needs_lf = TRUE;
                    }
                    else if (PORT_TEXT_INFO(port)->_needs_lf)
                    {
                         PORT_TEXT_INFO(port)->_needs_lf = FALSE;

                         /*  Notice: this _returns_ from read_char, to avoid double
                          *  counting ch in the position counters. */
                         if (ch == '\n')
                              return read_char(port);
                    }
               }
          }
     }

     if (!PORT_BINARYP(port))
     {
          /* Update the text position indicators */
          if (ch == '\n')
          {
               PORT_TEXT_INFO(port)->_row++;
               PORT_TEXT_INFO(port)->_previous_line_length = PORT_TEXT_INFO(port)->_column;
               PORT_TEXT_INFO(port)->_column = 0;
          }
          else
               PORT_TEXT_INFO(port)->_column++;
     }

     return ch;
}

LRef lchar_readyp(LRef port)
{
     if (NULLP(port))
          port = CURRENT_INPUT_PORT();
     else if (!PORTP(port))
          vmerror_wrong_type(1, port);

     if (PORT_CLASS(port)->_read_readyp)
          return boolcons(PORT_CLASS(port)->_read_readyp(port));
     else
     {
          /*  If there's not explicit char-ready? handling provided by the port,
           *  we use this hokey logic that defaults to true unless the port
           *  is at EOF. Because our EOF detection depends on peek_char, we
           *  can't even go that far with binary ports. Fixing this will require
           *  bypassing the C RTL I/O logic, which will have to wait. */

          if (PORT_BINARYP(port))
               vmerror("char-ready? not supported on port", port);

          return boolcons(peek_char(port) != EOF);
     }
}

static int flush_whitespace(LRef port, bool skip_lisp_comments = true)
{
     int c = '\0';

     bool commentp = false;
     bool reading_whitespace = true;

     while (reading_whitespace)
     {
          /*  We can never be in a comment if we're not skipping them... */
          assert(skip_lisp_comments ? true : !commentp);

          c = read_char(port);

          if (c == EOF)
               break;
          else if (commentp)
          {
               if (c == _T('\n'))
                    commentp = FALSE;
          }
          else if ((c == _T(';')) && skip_lisp_comments)
          {
               commentp = TRUE;
          }
          else if (!_istspace(c) && (c != _T('\0')))
               break;
     }

     if (c != EOF)
          unread_char(c, port);

     return c;
}

LRef lflush_whitespace(LRef port, LRef slc)
{
     int ch = EOF;

     if (NULLP(port))
          port = CURRENT_INPUT_PORT();

     if (!PORTP(port))
          vmerror_wrong_type(1, port);

     bool skip_lisp_comments = true;

     if (!NULLP(slc))
     {
          if (!BOOLP(slc))
               vmerror_wrong_type(2, slc);

          skip_lisp_comments = BOOLV(slc);
     }


     assert(!NULLP(port));

     if (PORT_MODE(port) & PORT_INPUT)
          ch = flush_whitespace(port, skip_lisp_comments);

     if (ch == EOF)
          return lmake_eof();
     else
          return charcons((_TCHAR) ch);
}

LRef lread_char(LRef port)
{
     if (NULLP(port))
          port = CURRENT_INPUT_PORT();
     else if (!PORTP(port))
          vmerror_wrong_type(1, port);

     assert(PORTP(port));

     int ch = read_char(port);

     if (ch == EOF)
          return lmake_eof();
     else
          return charcons((_TCHAR) ch);
}



LRef lread_binary_string(LRef l, LRef port)
{
     _TCHAR buf[STACK_STRBUF_LEN];

     if (NULLP(port))
          port = CURRENT_INPUT_PORT();
     else if (!PORTP(port))
          vmerror_wrong_type(2, port);

     assert(PORTP(port));

     if (!PORT_BINARYP(port))
          vmerror(_T("Raw port operations not allowed on text ports."), port);

     if (!NUMBERP(l))
          vmerror_wrong_type(1, l);

     fixnum_t remaining_length = get_c_fixnum(l);

     if (remaining_length <= 0)
          vmerror("Invalid length to read", l);

     LRef new_str = strcons();
     size_t total_read = 0;

     while (remaining_length > 0)
     {
          fixnum_t to_read = remaining_length;

          if (to_read > STACK_STRBUF_LEN)
               to_read = STACK_STRBUF_LEN;

          size_t actual_read = read_raw(buf, sizeof(_TCHAR), (size_t) remaining_length, port);

          if (actual_read <= 0)
               break;

          str_append_str(new_str, buf, actual_read);

          remaining_length -= actual_read;
          total_read += actual_read;
     }

     if (total_read == 0)
          return lmake_eof();

     return new_str;
}

bool read_binary_fixnum(fixnum_t length, bool signedp, LRef port, fixnum_t & result)
{
#ifdef FIXNUM_64BIT
     assert((length == 1) || (length == 2) || (length == 4) || (length == 8));
#else
     assert((length == 1) || (length == 2) || (length == 4));
#endif

     assert(PORTP(port));
     assert(PORT_BINARYP(port));

     u8_t bytes[sizeof(fixnum_t)];
     size_t fixnums_read = read_raw(bytes, (size_t) length, 1, port);

     if (!fixnums_read)
          return false;


     switch (length)
     {
     case 1:
          result = (signedp ? (fixnum_t) (*(i8_t *) bytes) : (fixnum_t) (*(u8_t *) bytes));
          break;
     case 2:
          result = (signedp ? (fixnum_t) (*(i16_t *) bytes) : (fixnum_t) (*(u16_t *) bytes));
          break;
     case 4:
          result = (signedp ? (fixnum_t) (*(i32_t *) bytes) : (fixnum_t) (*(u32_t *) bytes));
          break;
#ifdef FIXNUM_64BIT
     case 8:
          result = (signedp ? (fixnum_t) (*(i64_t *) bytes) : (fixnum_t) (*(u64_t *) bytes));
          break;
#endif
     }

     return true;
}


LRef lread_binary_fixnum(LRef l, LRef sp, LRef port)
{
     if (NULLP(port))
          port = CURRENT_INPUT_PORT();
     else if (!PORTP(port))
          vmerror_wrong_type(3, port);

     if (!PORT_BINARYP(port))
          vmerror(_T("Raw port operations not allowed on text ports."), port);

     if (!NUMBERP(l))
          vmerror_wrong_type(1, l);
     if (!BOOLP(sp))
          vmerror_wrong_type(2, sp);

     fixnum_t length = get_c_fixnum(l);
     bool signedp = BOOLV(sp);

#ifdef FIXNUM_64BIT
     if ((length != 1) && (length != 2) && (length != 4) && (length != 8))
          vmerror("Bad integer length, must be 1, 2, 4, or 8.", l);
#else
     if ((length != 1) && (length != 2) && (length != 4))
          error("Bad integer length, must be 1, 2, or 4.", l);
#endif

     fixnum_t result = 0;

     if (read_binary_fixnum(length, signedp, port, result))
          return fixcons(result);
     else
          return lmake_eof();
}

bool read_binary_flonum(LRef port, flonum_t & result)
{
     assert(PORTP(port));
     assert(PORT_BINARYP(port));

     u8_t bytes[sizeof(flonum_t)];
     size_t flonums_read = read_raw(bytes, sizeof(flonum_t), 1, port);

     if (!flonums_read)
          return false;

     result = *(flonum_t *) bytes;

     return true;
}

LRef lread_binary_flonum(LRef port)
{
     if (NULLP(port))
          port = CURRENT_INPUT_PORT();
     else if (!PORTP(port))
          vmerror_wrong_type(3, port);

     if (!PORT_BINARYP(port))
          vmerror(_T("Raw port operations not allowed on text ports."), port);

     flonum_t result = 0;

     if (read_binary_flonum(port, result))
          return flocons(result);
     else
          return lmake_eof();
}

LRef lread_line(LRef port)
{
     int ch;

     if (NULLP(port))
          port = CURRENT_INPUT_PORT();
     else if (!PORTP(port))
          vmerror_wrong_type(1, port);

     assert(PORTP(port));

     LRef op = lopen_output_string();

     bool read_anything = false;

     for (ch = read_char(port); (ch != EOF) && (ch != _T('\n')); ch = read_char(port))
     {
          read_anything = true;

          write_char(ch, op);
     }

     if (!read_anything && (ch == EOF))
          return lmake_eof();

     return lget_output_string(op);
}

int unread_char(int ch, LRef port)
{
     if (NULLP(port))
          port = CURRENT_INPUT_PORT();

     assert(!NULLP(port));

     if (PORT_BINARYP(port))
          vmerror("Unread not supported on binary ports!", port);

     switch (ch)
     {
     case '\n':
          PORT_TEXT_INFO(port)->_row--;
          PORT_TEXT_INFO(port)->_column = PORT_TEXT_INFO(port)->_previous_line_length;
          break;

     case '\r':
          break;

     default:
          PORT_TEXT_INFO(port)->_column--;
          break;
     }

     if (PORT_TEXT_INFO(port)->_unread_valid >= PORT_UNGET_BUFFER_SIZE)
          vmerror("unget buffer exceeded!", port);

     PORT_TEXT_INFO(port)->_unread_buffer[PORT_TEXT_INFO(port)->_unread_valid] = ch;
     PORT_TEXT_INFO(port)->_unread_valid++;

     return ch;
}

LRef lunread_char(LRef ch, LRef port)
{
     if (NULLP(port))
          port = CURRENT_INPUT_PORT();
     else if (!PORTP(port))
          vmerror_wrong_type(1, port);

     if (!CHARP(ch))
          vmerror_wrong_type(2, ch);

     assert(PORTP(port));

     unread_char(CHARV(ch), port);

     return port;
}

int peek_char(LRef port)
{
     int ch = EOF;

     if (NULLP(port))
          port = CURRENT_INPUT_PORT();

     assert(!NULLP(port));

     ch = read_char(port);
     unread_char(ch, port);

     return ch;
}

LRef lpeek_char(LRef port)
{
     int ch;

     if (NULLP(port))
          port = CURRENT_INPUT_PORT();

     if (!PORTP(port))
          vmerror_wrong_type(1, port);

     ch = peek_char(port);

     if (ch == EOF)
          return lmake_eof();
     else
          return charcons((_TCHAR) ch);
}

void write_char(int ch, LRef port)
{
     _TCHAR tch = (_TCHAR) ch;

     if (NULLP(port))
          port = CURRENT_OUTPUT_PORT();

     assert(!NULLP(port));

     write_text(&tch, 1, port);

     if (!PORT_BINARYP(port) && (tch == _T('\n')))
          lflush_port(port);
}

LRef lwrite_char(LRef ch, LRef port)
{
     if (!CHARP(ch))
          vmerror_wrong_type(1, ch);

     if (NULLP(port))
          port = CURRENT_OUTPUT_PORT();

     if (!PORTP(port))
          vmerror_wrong_type(2, port);

     write_char(CHARV(ch), port);

     return port;
}

LRef lwrite_strings(size_t argc, LRef argv[])
{
     if (argc < 1)
          vmerror(_T("insufficient arguments."), NIL);

     LRef port = argv[0];

     if (!PORTP(port))
          vmerror_wrong_type(1, port);

     for (size_t ii = 1; ii < argc; ii++)
     {
          LRef str = argv[ii];

          if (STRINGP(str))
               write_text(STRING_DATA(str), STRING_DIM(str), port);
          else if (CHARP(str))
          {
               _TCHAR ch = CHARV(str);

               write_text(&ch, 1, port);
          }
          else
               vmerror_wrong_type(ii, str);
     }

     return port;
}

LRef lwrite_binary_string(LRef string, LRef port)
{
     if (NULLP(port))
          port = CURRENT_OUTPUT_PORT();
     else if (!PORTP(port))
          vmerror_wrong_type(2, port);

     assert(PORTP(port));

     if (!PORT_BINARYP(port))
          vmerror(_T("Raw port operations not allowed on text ports."), port);

     if (!STRINGP(string))
          vmerror_wrong_type(1, string);

     size_t length = STRING_DIM(string);
     _TCHAR *strdata = STRING_DATA(string);

     size_t chars_written = write_raw(strdata, sizeof(_TCHAR), length, port);

     if (chars_written < length)
          vmerror("Error writing to port.", port);

     return fixcons(chars_written);
}

LRef lwrite_binary_fixnum(LRef v, LRef l, LRef sp, LRef port)
{
     if (NULLP(port))
          port = CURRENT_OUTPUT_PORT();
     else if (!PORTP(port))
          vmerror_wrong_type(4, port);

     assert(PORTP(port));

     if (!PORT_BINARYP(port))
          vmerror(_T("Raw port operations not allowed on text ports."), port);

     if (!FIXNUMP(v))
          vmerror_wrong_type(1, v);

     if (!NUMBERP(l))
          vmerror_wrong_type(2, l);

     if (!BOOLP(sp))
          vmerror_wrong_type(3, sp);

     fixnum_t length = get_c_fixnum(l);
     bool signedp = BOOLV(sp);

#ifdef FIXNUM_64BIT
     if ((length != 1) && (length != 2) && (length != 4) && (length != 8))
          vmerror("Bad integer length, must be 1, 2, 4, or 8.", l);
#else
     if ((length != 1) && (length != 2) && (length != 4))
          error("Bad integer length, must be 1, 2, or 4.", l);
#endif

     fixnum_t val = FIXNM(v);

     u8_t bytes[sizeof(fixnum_t)];


     bool in_range = false;

     switch (length)
     {
     case 1:
          if (signedp)
          {
               in_range = (val >= I8_MIN) && (val <= I8_MAX);
               *(i8_t *) bytes = (i8_t) val;
          }
          else
          {
               in_range = (val >= U8_MIN) && (val <= U8_MAX);
               *(u8_t *) bytes = (u8_t) val;
          }
          break;

     case 2:
          if (signedp)
          {
               in_range = (val >= I16_MIN) && (val <= I16_MAX);
               *(i16_t *) bytes = (i16_t) val;
          }
          else
          {
               in_range = (val >= U16_MIN) && (val <= U16_MAX);
              *(u16_t *) bytes = (u16_t) val;
          }
          break;

     case 4:
          if (signedp)
          {
               in_range = (val >= I32_MIN) && (val <= I32_MAX);
               *(i32_t *) bytes = (i32_t) val;
          }
          else
          {
               in_range = (val >= U32_MIN) && (val <= U32_MAX);
               *(u32_t *) bytes = (u32_t) val;
          }
          break;

#ifdef FIXNUM_64BIT
     case 8:
          if (signedp)
          {
               in_range = (val >= I64_MIN) && (val <= I64_MAX);
               *(i64_t *) bytes = (i64_t) val;
          }
          else
          {
               in_range = ((u64_t) val >= U64_MIN) && ((u64_t) val <= U64_MAX);
               *(u64_t *) bytes = (u64_t) val;
          }
          break;
#endif
     }

     if (!in_range)
          vmerror("Range check violation writing binary integer.", v);

     size_t fixnums_written = write_raw(bytes, (size_t) length, 1, port);

     if (fixnums_written < 1)
          vmerror("Error writing to port.", port);

     return fixcons(fixnums_written);
}

LRef lbinary_write_flonum(LRef v, LRef port)
{
     if (NULLP(port))
          port = CURRENT_OUTPUT_PORT();
     else if (!PORTP(port))
          vmerror_wrong_type(4, port);

     assert(PORTP(port));

     if (!PORT_BINARYP(port))
          vmerror(_T("Raw port operations not allowed on text ports."), port);

     if (!NUMBERP(v))
          vmerror_wrong_type(1, v);

     flonum_t val = get_c_double(v);

     u8_t bytes[sizeof(flonum_t)];

     *(flonum_t *) bytes = val;

     size_t flonums_written = write_raw(bytes, sizeof(flonum_t), 1, port);

     if (flonums_written < 1)
          vmerror("Error writing to port.", port);

     return fixcons(flonums_written);
}

size_t write_text(const _TCHAR * buf, size_t count, LRef port)
{
     if (NULLP(port))
          port = CURRENT_OUTPUT_PORT();

     assert(PORTP(port));

     if ((PORT_PINFO(port)->_mode & PORT_OUTPUT) != PORT_OUTPUT)
          return 0;

     if (PORT_BINARYP(port))
     {
          return write_raw(buf, sizeof(_TCHAR), count, port);
     }
     else if (!PORT_TEXT_INFO(port)->_crlf_translate)
     {
          for (size_t ii = 0; ii < count; ii++)
          {
               if (buf[ii] == _T('\n'))
               {
                    PORT_TEXT_INFO(port)->_row++;
                    PORT_TEXT_INFO(port)->_column = 0;
               }
               else
                    PORT_TEXT_INFO(port)->_column++;
          }

          return write_raw(buf, sizeof(_TCHAR), count, port);
     }
     else
     {
          /* This code divides the text to be written into blocks seperated
           * by line seperators. raw_write is called for each block to
           * actually do the write, and line seperators are correctly
           * translated to CR+LF pairs. */

          for (size_t next_char_to_write = 0; next_char_to_write < count;)
          {
               unsigned int c = _T('\0');
               size_t next_eoln_char;

               /* Scan for the next eoln character, it ends the block... */
               for (next_eoln_char = next_char_to_write; (next_eoln_char < count); next_eoln_char++)
               {
                    c = buf[next_eoln_char];

                    if ((c == '\n') || (c == '\r') || PORT_TEXT_INFO(port)->_needs_lf)
                         break;
               }

               if (PORT_TEXT_INFO(port)->_needs_lf)
               {
                    assert(next_eoln_char - next_char_to_write == 0);

                    if (buf[next_eoln_char] == _T('\n'))
                         next_eoln_char++;

                    write_raw(_T("\n"), sizeof(_TCHAR), 1, port);

                    PORT_TEXT_INFO(port)->_needs_lf = false;
                    PORT_TEXT_INFO(port)->_row++;
               }
               else if (next_eoln_char - next_char_to_write == 0)
               {
                    switch (c)
                    {
                    case _T('\n'):
                         write_raw(_T("\r\n"), sizeof(_TCHAR), 2, port);
                         PORT_TEXT_INFO(port)->_column = 0;
                         PORT_TEXT_INFO(port)->_row++;
                         break;

                    case _T('\r'):
                         write_raw(_T("\r"), sizeof(_TCHAR), 1, port);
                         PORT_TEXT_INFO(port)->_column = 0;
                         PORT_TEXT_INFO(port)->_needs_lf = true;
                         break;

                    default:
                         panic("Invalid case in write_text");
                    }

                    next_eoln_char++;
               }
               else
               {
                    PORT_TEXT_INFO(port)->_column += (next_eoln_char - next_char_to_write);
                    write_raw(&(buf[next_char_to_write]), sizeof(_TCHAR),
                              next_eoln_char - next_char_to_write, port);
               }

               next_char_to_write = next_eoln_char;
          }
     }

     return count;
}

LRef lrich_write(LRef obj, LRef machine_readable, LRef port)
{
     if (NULLP(port))
          port = CURRENT_OUTPUT_PORT();

     if (!PORTP(port))
          vmerror_wrong_type(3, port);

     if (PORT_CLASS(port)->_rich_write == NULL)
          return boolcons(false);

     if (PORT_CLASS(port)->_rich_write(obj, TRUEP(machine_readable), port))
          return port;

     return boolcons(false);
}


LRef lnewline(LRef port)
{
     if (NULLP(port))
          port = CURRENT_OUTPUT_PORT();

     if (!PORTP(port))
          vmerror_wrong_type(1, port);

     write_char(_T('\n'), port);

     return port;
}

LRef lfresh_line(LRef port)
{
     if (NULLP(port))
          port = CURRENT_OUTPUT_PORT();

     if (!PORTP(port))
          vmerror_wrong_type(1, port);

     if (PORT_BINARYP(port)
         || ((PORT_TEXT_INFO(port)->_column != 0) && !PORT_TEXT_INFO(port)->_needs_lf))
     {
          lnewline(port);
          return boolcons(true);
     }

     return boolcons(false);
}

size_t port_length(LRef port)
{
     assert(PORTP(port));

     if (PORT_CLASS(port)->_length)
          return PORT_CLASS(port)->_length(port);

     return 0;
}

/* C File Port ************************************************
 *
 * All the groovy stuff to specialize a port into a port capable
 * of talking to a file is contained herein. Thanks to the fact
 * that the C standard library's file I/O functions also talk
 * to STDIN/OUT/ERR, this also contains simple port definitions
 * for those devices, as well.
 *
 * state = FILE *
 * extended_state = Scheme object containing file name
 */

INLINE void SET_PORT_FILE(LRef port, FILE * file)
{
     PORT_PINFO(port)->_user_data = file;
}

INLINE FILE *PORT_FILE(LRef port)
{
     return (FILE *) (PORT_PINFO(port)->_user_data);
}

LRef fileportcons(port_class_t * cls, port_mode_t mode, LRef filename)
{
     assert(STRINGP(filename) || NULLP(filename));

     return portcons(cls, filename, mode, NIL, NULL);
}


void file_port_open(LRef obj)
{
     FILE *f = NULL;


     assert(STRINGP(PORT_PINFO(obj)->_port_name));

     if (PORT_MODE(obj) & PORT_OUTPUT)
          f = fopen(get_c_string(PORT_PINFO(obj)->_port_name), "wb");
     else if (PORT_MODE(obj) & PORT_INPUT)
          f = fopen(get_c_string(PORT_PINFO(obj)->_port_name), "rb");
     else
          panic("File port open for closed port");

     if (f)
     {

          SET_PORT_FILE(obj, f);
     }
     else
     {
          SET_PORT_MODE(obj, PORT_CLOSED);
          vmerror("Cannot open file", PORT_PINFO(obj)->_port_name);
     }
}


size_t file_port_read(void *buf, size_t size, size_t count, LRef obj)
{
     FILE *f = PORT_FILE(obj);

     assert(f);

     return fread(buf, size, count, f);
}

size_t file_port_write(const void *buf, size_t size, size_t count, LRef obj)
{
     FILE *f = PORT_FILE(obj);

     assert(f);

     return (int) fwrite(buf, size, count, f);
}

int file_port_flush(LRef obj)
{
     FILE *f = PORT_FILE(obj);

     assert(f);

     return fflush(f);
}

void file_port_close(LRef obj)
{
     FILE *f = PORT_FILE(obj);

     if (f == NULL)
          return;

     fclose(f);
     SET_PORT_FILE(obj, NULL);
}


port_class_t file_port_class = {
     _T("STANDARD-FILE"),
     PORT_INPUT_OUTPUT,

     file_port_open,

     NULL,
     file_port_read,
     file_port_write,
     NULL,
     file_port_flush,

     file_port_close,
     NULL,

     NULL,
};

bool get_c_port_mode(LRef mode)
{
     if (NULLP(mode))
          return false;

     if (!SYMBOLP(mode))
          vmerror_wrong_type(2, mode);

     if (keyword_intern(_T("binary")) == mode)
          return true;
     else if (keyword_intern(_T("text")) == mode)
          return false;
     else
          vmerror("Invalid file mode.", mode);

     return false;
}

LRef lopen_input_file(LRef filename, LRef mode)
{
     bool binary = get_c_port_mode(mode);

     if (!STRINGP(filename))
          vmerror_wrong_type(1, filename);

     return fileportcons(&file_port_class, (port_mode_t) (PORT_INPUT | (binary ? PORT_BINARY : 0)),
                         filename);
}

LRef lopen_output_file(LRef filename, LRef mode)
{
     bool binary = get_c_port_mode(mode);

     if (!STRINGP(filename))
          vmerror_wrong_type(1, filename);

     return fileportcons(&file_port_class, (port_mode_t) (PORT_OUTPUT | (binary ? PORT_BINARY : 0)),
                         filename);
}

/* Standard I/O ***********************************************
 *
 * These ports depend on some code from the C file ports to
 * do their thing.
 */

void stdio_port_close(LRef obj)
{
     SET_PORT_FILE(obj, NULL);
}

void stdin_port_open(LRef obj)
{
     SET_PORT_FILE(obj, stdin);
}

port_class_t stdin_port_class = {
     _T("STANDARD-INPUT"),
     PORT_INPUT,

     stdin_port_open,

     NULL,
     file_port_read,
     NULL,
     NULL,

     NULL,
     stdio_port_close,
     NULL,

     NULL,
};

void stdout_port_open(LRef obj)
{
     SET_PORT_FILE(obj, stdout);
}

port_class_t stdout_port_class = {
     _T("STANDARD-OUTPUT"),
     PORT_OUTPUT,

     stdout_port_open,

     NULL,
     NULL,
     file_port_write,
     NULL,
     file_port_flush,

     stdio_port_close,
     NULL,

     NULL,
};

void stderr_port_open(LRef obj)
{
     SET_PORT_FILE(obj, stderr);
}

port_class_t stderr_port_class = {
     _T("STANDARD-ERROR"),
     PORT_OUTPUT,

     stderr_port_open,

     NULL,
     NULL,
     file_port_write,
     NULL,
     file_port_flush,

     stdio_port_close,
     NULL,

     NULL,
};

/*
 * The standard debug port.
 */

size_t debug_port_write(const void *buf, size_t size, size_t count, LRef obj)
{
     UNREFERENCED(obj);
     assert(PORTP(obj));

     _TCHAR *cbuf = (_TCHAR *) buf;

     _TCHAR block[DEBUG_PORT_BLOCK_SIZE + 1];

     size_t buf_loc = 0;
     size_t block_loc = 0;

     size_t len = size * count;

     /* Filter nulls out of the input string, and ensure that the
      * buffer we pass to OutputDebugString has as terminating
      * null. */
     while (len > 0)
     {
          for (block_loc = 0;
               (block_loc < DEBUG_PORT_BLOCK_SIZE) && (len > 0); block_loc++, buf_loc++, len--)
          {
               if (cbuf[buf_loc] == '\0')
                    block[block_loc] = '~';
               else
                    block[block_loc] = cbuf[buf_loc];
          }

          block[block_loc] = '\0';

          output_debug_string(block);
     }

     return len;
}

port_class_t debug_port_class = {
     _T("STANDARD-DEBUG"),
     PORT_OUTPUT,

     NULL,

     NULL,
     NULL,
     debug_port_write,
     NULL,

     NULL,
     stdio_port_close,
     NULL,

     NULL,
};


LRef ldebug_write(LRef form)
{
     debug_print_object(form, CURRENT_DEBUG_PORT(), true);

     return lnewline(CURRENT_DEBUG_PORT());
}

LRef lopen_debug_port()
{
     return &interp.debugger_output;
}


/* Null port **************************************************
 *
 * Input port - Always reads out EOF.
 * Output port - Accepts all writes. */

/*
 * Input port
 */

size_t null_port_read(void *buf, size_t size, size_t count, LRef obj)
{
     UNREFERENCED(buf);
     UNREFERENCED(size);
     UNREFERENCED(count);
     UNREFERENCED(obj);

     return 0;
}

size_t null_port_write(const void *buf, size_t size, size_t count, LRef obj)
{
     UNREFERENCED(buf);
     UNREFERENCED(size);
     UNREFERENCED(obj);

     return count;
}

port_class_t null_port_class = {
     _T("NULL"),
     PORT_INPUT_OUTPUT,

     NULL,

     NULL,
     null_port_read,
     null_port_write,
     NULL,

     NULL,
     NULL,
     NULL,

     NULL,
};


LRef lopen_null_port()
{
     return portcons(&null_port_class, NIL, (scan::port_mode_t) (PORT_INPUT_OUTPUT | PORT_BINARY),
                     NIL, NULL);
}

/* String port ************************************************
 *
 * This port supports Input from string objects
 *
 * state_info = Current char * pointer into the string
 * extended_state_info = Current string object
 **/

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


/*  REVISIT: Do we need to restrict bootup NULL I/O */

/*  REVISIT: lots of logic supports default ports if port==NULL. Move to scheme? */


/* C Data Stream **********************************************
 *
 * This port class allows data to be written to compiled
 * C-code. As an out port, the C-data port acts as a filter,
 * writing C code to initialize an array of unsigned chars.
 * Conveniently, in input mode, the port allows reads to be
 * made from an array of unsigned chars.
 *
 * (open-c-data-output <dest-stream> <var-name>)
 */

struct c_data_port_state
{
     size_t _bytes_transferred;
     unsigned char *_input_buffer;
     size_t _input_buffer_bytes;
};


size_t c_data_port_read(void *buf, size_t size, size_t count, LRef obj)
{
     if (!(PORT_MODE(obj) & PORT_INPUT))
          return 0;

     c_data_port_state *ps = (c_data_port_state *) (PORT_PINFO(obj)->_user_data);

     size_t bytes = size * count;
     size_t bytes_remaining = ps->_input_buffer_bytes - ps->_bytes_transferred;

     if (bytes > bytes_remaining)
          bytes = (bytes_remaining / size) * size;

     memcpy(buf, &(ps->_input_buffer[ps->_bytes_transferred]), bytes);

     ps->_bytes_transferred += bytes;

     return bytes / size;
}

void c_data_port_gc_free(LRef obj)
{
     assert(PORT_PINFO(obj)->_user_data);

     safe_free(PORT_PINFO(obj)->_user_data);
     PORT_PINFO(obj)->_user_data = NULL;
}

size_t c_data_port_length(LRef obj)
{
     if (!(PORT_MODE(obj) & PORT_INPUT))
          return 0;

     c_data_port_state *ps = (c_data_port_state *) (PORT_PINFO(obj)->_user_data);

     return ps->_input_buffer_bytes - ps->_bytes_transferred;
}

port_class_t c_data_port_class = {
     _T("C-DATA"),
     PORT_INPUT,

     NULL,

     NULL,
     c_data_port_read,
     NULL,
     NULL,

     NULL,
     NULL,
     NULL,

     c_data_port_length,
};

LRef lopen_c_data_output(LRef destination, LRef var_name, LRef mode)
{
     bool binary = get_c_port_mode(mode);

     if (!PORTP(destination))
          vmerror_wrong_type(1, destination);

     if (!STRINGP(var_name))
          vmerror_wrong_type(2, var_name);

     if (!TRUEP(loutput_portp(destination)))
          vmerror("Cannot write c-data to input-only ports", destination);

     if (TRUEP(lbinary_portp(destination)))
          vmerror("Cannot write c-data to binary ports", destination);

     c_data_port_state *ps = (c_data_port_state *) safe_malloc(sizeof(c_data_port_state));

     ps->_bytes_transferred = 0;
     ps->_input_buffer = NULL;
     ps->_input_buffer_bytes = 0;

     return portcons(&c_data_port_class, var_name,
                     (port_mode_t) (PORT_OUTPUT | (binary ? PORT_BINARY : 0)), destination, ps);
}

void register_internal_file(const _TCHAR * filename, bool binary_data, unsigned char *data,
                            size_t bytes)
{
     LRef file_record = lcons(strcons(filename), open_c_data_input(binary_data, data, bytes));

     SET_SYMBOL_VCELL(interp.sym_internal_files,
                      lcons(file_record, SYMBOL_VCELL(interp.sym_internal_files)));
}

LRef open_c_data_input(bool binary_data, unsigned char *source, size_t bytes)
{
     c_data_port_state *ps = (c_data_port_state *) safe_malloc(sizeof(c_data_port_state));

     ps->_bytes_transferred = 0;
     ps->_input_buffer = source;
     ps->_input_buffer_bytes = bytes;

     return portcons(&c_data_port_class, NIL,
                     (port_mode_t) (PORT_INPUT | (binary_data ? PORT_BINARY : 0)), NIL, ps);
}

LRef lclone_c_data_port(LRef port)
{
     if (!PORTP(port))
          vmerror_wrong_type(1, port);

     if (!(PORT_MODE(port) & PORT_INPUT) || (PORT_CLASS(port) != &c_data_port_class))
          vmerror("Cannot clone any kind of port other than a c-data input port.", port);

     c_data_port_state *old_ps = (c_data_port_state *) (PORT_PINFO(port)->_user_data);
     c_data_port_state *new_ps = (c_data_port_state *) safe_malloc(sizeof(c_data_port_state));

     new_ps->_bytes_transferred = old_ps->_bytes_transferred;
     new_ps->_input_buffer = old_ps->_input_buffer;
     new_ps->_input_buffer_bytes = old_ps->_input_buffer_bytes;

     return portcons(&c_data_port_class, NIL, PORT_MODE(port), NIL, new_ps);
}



/* Blocking Input Port ****************************************
 *
 * This port class allows data to be read from a blocking_inputg input:
 * an input that doesn't always have available data. To read
 * from its input, blocking_inputg input ports call a read_input function
 * that waits for input and passes it back to the port. While
 * waiting for available input the read function is free to do
 * as it wishes.
 *
 * Note, that there's nothing that makes it impossible to implement
 * this kind of functionality with a standard port class. What this
 * really does is abstract away a lot of the hairy buffer management
 * logic away from the client C code.
 */

size_t blocking_input_port_read(void *buf, size_t size, size_t count, LRef port);
void blocking_input_port_close(LRef port);
bool blocking_input_read_readyp(LRef port);

port_class_t blocking_input_port_class = {
     _T("BLOCKING-INPUT"),
     PORT_INPUT,

     NULL,

     blocking_input_read_readyp,
     blocking_input_port_read,
     NULL,
     NULL,

     NULL,
     blocking_input_port_close,
     NULL,

     NULL,
};

struct blocking_input_port_state
{
     blocking_input_read_data_fn_t _read_data;
     blocking_input_close_port_fn_t _close_port;
     size_t _buffer_size;
     size_t _buffer_pos;
     u8_t *_buffer;
     void *_userdata;
     bool _more_data;
};

bool blocking_input_read_readyp(LRef port)
{
     return blocking_input_is_data_available(port);
}

size_t blocking_input_port_read(void *buf, size_t size, size_t count, LRef port)
{
     assert(PORTP(port) && (PORT_CLASS(port) == &blocking_input_port_class));

     if (!(PORT_MODE(port) & PORT_INPUT))
          return 0;

     blocking_input_port_state *ps = (blocking_input_port_state *) (PORT_PINFO(port)->_user_data);

     size_t bytes_to_read = size * count;
     size_t bytes_read = 0;

     while (bytes_to_read)
     {
          size_t bytes_available = ps->_buffer_size - ps->_buffer_pos;

          if (bytes_available == 0)
          {
               if (!ps->_more_data)
                    break;

               if (!ps->_read_data(port, ps->_userdata))
                    ps->_more_data = false;

               /*  Handle the case where the read callback actually closed the port on us. */
               if (PORT_MODE(port) == PORT_CLOSED)
                    break;

               bytes_available = ps->_buffer_size - ps->_buffer_pos;
          }

          bytes_available = MIN2(bytes_available, bytes_to_read);

          if (bytes_available)
          {
               memcpy(&((u8_t *) buf)[bytes_read], &((u8_t *) ps->_buffer)[ps->_buffer_pos],
                      bytes_available);

               bytes_read += bytes_available;
               bytes_to_read -= bytes_available;
               ps->_buffer_pos += bytes_available;
          }
     }

     return bytes_read / size;
}

void blocking_input_port_close(LRef port)
{
     assert(PORTP(port) && (PORT_CLASS(port) == &blocking_input_port_class));

     blocking_input_port_state *ps = (blocking_input_port_state *) (PORT_PINFO(port)->_user_data);

     if (ps->_close_port)
          ps->_close_port(port, ps->_userdata);

     if (ps->_buffer)
          safe_free(ps->_buffer);

     safe_free(ps);

     PORT_PINFO(port)->_user_data = NULL;

     SET_PORT_MODE(port, PORT_CLOSED);
}

void blocking_input_post_data(LRef port, void *data, size_t size)
{
     assert(PORTP(port) && (PORT_CLASS(port) == &blocking_input_port_class));
     assert(!blocking_input_is_data_available(port));   /*  TODO: we really should allow this case */

     blocking_input_port_state *ps = (blocking_input_port_state *) (PORT_PINFO(port)->_user_data);

     assert(ps->_more_data);

     if (ps->_buffer)
     {
          safe_free(ps->_buffer);

          ps->_buffer = NULL;
     }

     ps->_buffer = (u8_t *) safe_malloc(size);

     memcpy(ps->_buffer, data, size);

     ps->_buffer_pos = 0;
     ps->_buffer_size = size;
}

void blocking_input_post_eof(LRef port)
{
     assert(PORTP(port) && (PORT_CLASS(port) == &blocking_input_port_class));

     blocking_input_port_state *ps = (blocking_input_port_state *) (PORT_PINFO(port)->_user_data);

     ps->_more_data = false;
}

bool blocking_input_is_data_available(LRef port)
{
     assert(PORTP(port) && (PORT_CLASS(port) == &blocking_input_port_class));

     blocking_input_port_state *ps = (blocking_input_port_state *) (PORT_PINFO(port)->_user_data);

     return (ps != NULL)
         && (ps->_buffer != NULL) && (ps->_buffer_size > 0) && (ps->_buffer_pos < ps->_buffer_size);
}

LRef blocking_input_cons(const _TCHAR * port_name, bool binary,
                         blocking_input_read_data_fn_t read_fn,
                         blocking_input_close_port_fn_t close_fn, void *userdata)
{
     blocking_input_port_state *ps =
         (blocking_input_port_state *) safe_malloc(sizeof(blocking_input_port_state));

     ps->_read_data = read_fn;
     ps->_close_port = close_fn;
     ps->_buffer_size = 0;
     ps->_buffer_pos = 0;
     ps->_buffer = NULL;
     ps->_userdata = userdata;
     ps->_more_data = true;

     return portcons(&blocking_input_port_class,
                     strcons(port_name),
                     binary ? (port_mode_t) (PORT_INPUT | PORT_BINARY) : PORT_INPUT, NIL, ps);

}

/* A C function to do Lisp-style Formatted I/O ******************
 *
 * REVISIT: scvwritef's ~a and ~s should be changed to ~da and ~ds, signifying that they invoke the debug printer
 * ~s - write the lisp object
 * ~a - display the lisp object
 * REVISIT: remove scvwritef ~u in favor of some kind of print_unreadable_object call
 * ~u - display the lisp object in unprintable fashion (ie. <type@addr...>
 *
 * ~cs - display the C string
 * ~cS - display the C string/arglist with a recursive call to scvwritef
 * ~cd - display the C integer
 * ~cf - display the C flonum
 * ~c& - display the C pointer
 * ~cc - display the C character
 * ~cC - display the C integer as an octal character constant
 * ~cB - display the C integer as a byte
 *
 * Prefixing a format code with a #\! (ie. ~!L) causes the corresponding
 * value to be returned from the function as a Lisp object.
 */
LRef scvwritef(const _TCHAR * format_str, LRef port, va_list arglist)
{
     char ch;

     if (NULLP(port))
          port = CURRENT_OUTPUT_PORT();

     assert(PORTP(port));


     _TCHAR buf[STACK_STRBUF_LEN];


     LRef lisp_arg_value = NULL;
     _TCHAR *str_arg_value = NULL;
     _TCHAR char_arg_value = _T('\0');
     long int long_arg_value = 0;
     unsigned long int ulong_arg_value = 0;
     flonum_t flonum_arg_value = 0.0;

     LRef unprintable_object = NIL;
     LRef return_value = NIL;

     for (;;)
     {
          ch = *format_str;

          if (ch == '\0')
               break;

          bool return_next_value = false;

          format_str++;

          if (ch != '~')
          {
               write_char(ch, port);

               continue;
          }

          ch = *format_str;
          format_str++;

          if (ch == '!')
          {
               ch = *format_str;
               format_str++;

               return_next_value = true;
          }

          switch (ch)
          {
          case 's':
               lisp_arg_value = va_arg(arglist, LRef);

               if (return_next_value)
                    return_value = lisp_arg_value;

               debug_print_object(lisp_arg_value, port, true);
               break;

          case 'a':
               lisp_arg_value = va_arg(arglist, LRef);

               if (return_next_value)
                    return_value = lisp_arg_value;

               debug_print_object(lisp_arg_value, port, false);
               break;

          case 'u':
               unprintable_object = va_arg(arglist, LRef);

               if (return_next_value)
                    return_value = unprintable_object;

               if (DEBUG_FLAG(DF_PRINT_FOR_DIFF))
                    scwritef("#<~a@(no-addr)", port, lrepresentation_of(unprintable_object));
               else
                    scwritef("#<~a@~c&", port,
                             lrepresentation_of(unprintable_object), unprintable_object);
               break;

          case '~':
               write_char('~', port);
               break;

          case 'c':            /*  C object prefix */

               ch = *format_str;        /*  read the next format character */
               format_str++;

               switch (ch)
               {

               case 's':
                    str_arg_value = va_arg(arglist, _TCHAR *);

                    if (return_next_value)
                         return_value = strcons(str_arg_value);

                    if (str_arg_value)
                         write_text(str_arg_value, _tcslen(str_arg_value), port);
                    else
                         WRITE_TEXT_CONSTANT(_T("<null>"), port);
                    break;

               case 'S':
                    str_arg_value = va_arg(arglist, _TCHAR *);

                    if (return_next_value)
                         return_value = scvwritef(str_arg_value, port, arglist);
                    else
                         scvwritef(str_arg_value, port, arglist);
                    break;

               case 'd':
                    long_arg_value = va_arg(arglist, long int);

                    if (return_next_value)
                         return_value = fixcons(long_arg_value);

                    _sntprintf(buf, STACK_STRBUF_LEN, _T("%d"), (int) long_arg_value);

                    write_text(buf, _tcslen(buf), port);
                    break;

               case 'x':
                    long_arg_value = va_arg(arglist, long int);

                    if (return_next_value)
                         return_value = fixcons(long_arg_value);

                    _sntprintf(buf, STACK_STRBUF_LEN, _T("%08lx"), long_arg_value);

                    write_text(buf, _tcslen(buf), port);
                    break;

               case 'f':
                    flonum_arg_value = va_arg(arglist, flonum_t);

                    if (return_next_value)
                         return_value = flocons(flonum_arg_value);

                    _sntprintf(buf, STACK_STRBUF_LEN, _T("%f"), flonum_arg_value);

                    write_text(buf, _tcslen(buf), port);
                    break;

               case '&':
                    _sntprintf(buf, STACK_STRBUF_LEN, _T("%p"), (void *) va_arg(arglist, void *));

                    if (return_next_value)
                         return_value = strcons(buf);

                    write_text(buf, _tcslen(buf), port);
                    break;

               case 'c':
                    ulong_arg_value = va_arg(arglist, unsigned long int);

                    if (return_next_value)
                         return_value = fixcons(ulong_arg_value);

                    char_arg_value = (_TCHAR) ulong_arg_value;

                    write_text(&char_arg_value, 1, port);
                    break;

               case 'C':
                    ulong_arg_value = va_arg(arglist, unsigned long int);

                    if (return_next_value)
                         return_value = fixcons(ulong_arg_value);

                    _sntprintf(buf, STACK_STRBUF_LEN, _T("%03o"), (u32_t) ulong_arg_value);
                    write_text(buf, _tcslen(buf), port);
                    break;

               case 'B':
                    ulong_arg_value = va_arg(arglist, unsigned long int);

                    if (return_next_value)
                         return_value = fixcons(ulong_arg_value);

                    _sntprintf(buf, STACK_STRBUF_LEN, _T("0x%02x"), (u32_t) ulong_arg_value);
                    write_text(buf, _tcslen(buf), port);
                    break;

               default:
                    vmerror(_T("Invalid C object format character in scwritef"), charcons(ch));
                    break;
               };
               break;

          default:
               vmerror(_T("Invalid format character in scwritef"), charcons(ch));
               break;
          }

          return_next_value = false;
     }
     va_end(arglist);

     if (!NULLP(unprintable_object))
          scwritef(">", port);

     return return_value;
}

void scwritef(const _TCHAR * format_str, LRef port, ...)
{
     va_list arglist;

     va_start(arglist, port);

     scvwritef(format_str, port, arglist);
}

void dscwritef(const _TCHAR * format_str, ...)
{
     va_list arglist;

     va_start(arglist, format_str);

     scvwritef(format_str, VM_DEBUG_PORT(), arglist);
     lflush_port(VM_DEBUG_PORT());
}

void dscwritef(debug_flag_t flag, const _TCHAR * format_str, ...)
{
     if (DEBUG_FLAG(flag))
     {
          va_list arglist;

          va_start(arglist, format_str);

          scvwritef(format_str, VM_DEBUG_PORT(), arglist);
          lflush_port(VM_DEBUG_PORT());
     }
}

/*  TODO: add char-ready? */

void init_debugger_output()
{
     SET_TYPE(&interp.debugger_output, TC_PORT);

     initialize_port(&interp.debugger_output,
                     DEBUG_FLAG(DF_DEBUGGER_TO_ODS) ? &debug_port_class : &stderr_port_class,
                     NIL, PORT_OUTPUT, NIL, NULL);
}

void init_stdio_ports()
{
     lidefine_global(interp.sym_port_current_in, fileportcons(&stdin_port_class, PORT_INPUT, NIL),
                     NIL);
     lidefine_global(interp.sym_port_current_out,
                     fileportcons(&stdout_port_class, PORT_OUTPUT, NIL), NIL);

     LRef stderr_port = fileportcons(&stderr_port_class, PORT_OUTPUT, NIL);

     lidefine_global(interp.sym_port_current_err, stderr_port, NIL);
     lidefine_global(interp.sym_port_debug, stderr_port, NIL);
}

END_NAMESPACE
