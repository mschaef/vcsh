
 /*
  * io.cpp --
  *
  * The I/O subsystem. This tries to be as R5RS compliant as possible.
  *
  * (C) Copyright 2001-2011 East Coast Toolworks Inc.
  * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
  *
  * See the file "license.terms" for information on usage and redistribution
  * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
  */

 #include <ctype.h>
 #include <memory.h>
 #include <stdio.h>

 #include "scan-private.h"

 BEGIN_NAMESPACE(scan)

 /*  REVISIT: It'd be nice to have an 'tee' port to write output to multiple destination ports. */

 /*  REVISIT: Do we need to restrict bootup NULL I/O */

 /*  REVISIT: lots of logic supports default ports if port==NULL. Move to scheme? */

 /*** End-of-file object ***/
 lref_t lmake_eof()
 {
      return LREF2_CONS(LREF2_EOF, 0);
 }

 lref_t leof_objectp(lref_t obj)
 {
      return EOFP(obj) ? obj : boolcons(false);
 }

 /*** Port object ***/

 lref_t port_gc_mark(lref_t obj)
 {
      gc_mark(PORT_PINFO(obj)->_port_name);
      gc_mark(PORT_PINFO(obj)->_user_object);

      for (size_t ii = 0; ii < FAST_LOAD_STACK_DEPTH; ii++)
           gc_mark(PORT_PINFO(obj)->_fasl_stack[ii]);

      gc_mark(PORT_PINFO(obj)->_fasl_accum);

      return PORT_PINFO(obj)->_fasl_table;
 }

 void port_gc_free(lref_t port)
 {
      assert(PORTP(port));
      assert(PORT_CLASS(port));

      if (PORT_CLASS(port)->_close)
           PORT_CLASS(port)->_close(port);

      if (PORT_TEXT_INFO(port))
           gc_free(PORT_TEXT_INFO(port));

      if (PORT_CLASS(port)->_gc_free)
           PORT_CLASS(port)->_gc_free(port);

      gc_free(PORT_PINFO(port));
 }

 lref_t initialize_port(lref_t s,
                      port_class_t * cls,
                      lref_t port_name, port_mode_t mode, lref_t user_object, void *user_data)
 {
      bool binary = (mode & PORT_BINARY) == PORT_BINARY;

      assert(cls != NULL);
      assert(cls->_valid_modes & mode);
      assert(!NULLP(s));

      SET_PORT_PINFO(s, (port_info_t *) gc_malloc(sizeof(port_info_t)));
      SET_PORT_CLASS(s, cls);

      PORT_PINFO(s)->_port_name = port_name;
      PORT_PINFO(s)->_user_data = user_data;
      PORT_PINFO(s)->_user_object = user_object;
      PORT_PINFO(s)->_fasl_table = NIL;
      PORT_PINFO(s)->_fasl_accum = NIL;

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
                              gc_malloc(sizeof(port_text_translation_info_t)));

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

 size_t port_length(lref_t port)
 {
      assert(PORTP(port));

      if (PORT_CLASS(port)->_length)
           return PORT_CLASS(port)->_length(port);

      return 0;
 }

 size_t write_raw(const void *buf, size_t size, size_t count, lref_t port)
 {
      if (NULLP(port))
           port = CURRENT_OUTPUT_PORT();

      assert(!NULLP(port));
      assert(PORT_CLASS(port)->_write);

      PORT_PINFO(port)->_bytes_written += (size * count);

      return PORT_CLASS(port)->_write(buf, size, count, port);
 }

 size_t read_raw(void *buf, size_t size, size_t count, lref_t port)
 {
      if (NULLP(port))
           port = CURRENT_INPUT_PORT();

      assert(!NULLP(port));
      assert(PORT_CLASS(port)->_read);

      size_t actual_count = PORT_CLASS(port)->_read(buf, size, count, port);

      PORT_PINFO(port)->_bytes_read += (size * actual_count);

      return actual_count;
 }

 lref_t portcons(port_class_t * cls, lref_t port_name, port_mode_t mode, lref_t user_object,
               void *user_data)
 {
      lref_t s = new_cell(TC_PORT);

      return initialize_port(s, cls, port_name, mode, user_object, user_data);
 }

 /***** C I/O functions *****/

 int read_char(lref_t port)
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

 int unread_char(int ch, lref_t port)
 {
      if (NULLP(port))
           port = CURRENT_INPUT_PORT();

      assert(!NULLP(port));

      if (PORT_BINARYP(port))
           vmerror_unsupported(_T("cannot unread on binary ports."));

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
           vmerror_io_error(_T("unget buffer exceeded."), port);

      PORT_TEXT_INFO(port)->_unread_buffer[PORT_TEXT_INFO(port)->_unread_valid] = ch;
      PORT_TEXT_INFO(port)->_unread_valid++;

      return ch;
 }

 int peek_char(lref_t port)
 {
      int ch = EOF;

      if (NULLP(port))
           port = CURRENT_INPUT_PORT();

      assert(!NULLP(port));

      ch = read_char(port);
      unread_char(ch, port);

      return ch;
 }

 void write_char(int ch, lref_t port)
 {
      _TCHAR tch = (_TCHAR) ch;

      if (NULLP(port))
           port = CURRENT_OUTPUT_PORT();

      assert(!NULLP(port));

      write_text(&tch, 1, port);

      if (!PORT_BINARYP(port) && (tch == _T('\n')))
           lflush_port(port);
 }

 size_t write_text(const _TCHAR * buf, size_t count, lref_t port)
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

 static int flush_whitespace(lref_t port, bool skip_lisp_comments = true)
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

 bool read_binary_fixnum(fixnum_t length, bool signedp, lref_t port, fixnum_t & result)
 {
 #ifdef SCAN_64BIT_FIXNUMS
      assert((length == 1) || (length == 2) || (length == 4) || (length == 8));
 #else
      assert((length == 1) || (length == 2) || (length == 4));
 #endif

      assert(PORTP(port));
      assert(PORT_BINARYP(port));

      uint8_t bytes[sizeof(fixnum_t)];
      size_t fixnums_read = read_raw(bytes, (size_t) length, 1, port);

      if (!fixnums_read)
           return false;


      switch (length)
      {
      case 1:
           result = (signedp ? (fixnum_t) (*(int8_t *) bytes) : (fixnum_t) (*(uint8_t *) bytes));
           break;
      case 2:
           result = (signedp ? (fixnum_t) (*(int16_t *) bytes) : (fixnum_t) (*(uint16_t *) bytes));
           break;
      case 4:
           result = (signedp ? (fixnum_t) (*(int32_t *) bytes) : (fixnum_t) (*(uint32_t *) bytes));
           break;
 #ifdef SCAN_64BIT_FIXNUMS
      case 8:
           result = (signedp ? (fixnum_t) (*(int64_t *) bytes) : (fixnum_t) (*(uint64_t *) bytes));
           break;
 #endif
      }

      return true;
 }


 bool read_binary_flonum(lref_t port, flonum_t & result)
 {
      assert(PORTP(port));
      assert(PORT_BINARYP(port));

      uint8_t bytes[sizeof(flonum_t)];
      size_t flonums_read = read_raw(bytes, sizeof(flonum_t), 1, port);

      if (!flonums_read)
           return false;

      result = *(flonum_t *) bytes;

      return true;
 }


 /***** Lisp-visible port functions *****/

 lref_t lportp(lref_t obj)
 {
      return boolcons(PORTP(obj));
 }

 lref_t linput_portp(lref_t obj)
 {
      if (PORTP(obj) && (PORT_MODE(obj) & PORT_INPUT))
           return obj;
      else
           return boolcons(false);
 }

 lref_t loutput_portp(lref_t obj)
 {
      if (PORTP(obj) && (PORT_MODE(obj) & PORT_OUTPUT))
           return obj;
      else
           return boolcons(false);
 }

 lref_t lbinary_portp(lref_t obj)
 {
      if (PORTP(obj) && (PORT_BINARYP(obj)))
           return obj;
      else
           return boolcons(false);
 }

 lref_t lport_mode(lref_t obj)
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


 lref_t lport_name(lref_t port)
 {
      if (NULLP(port))
           port = CURRENT_INPUT_PORT();

      if (!PORTP(port))
           vmerror_wrong_type(1, port);

      return PORT_PINFO(port)->_port_name;
 }

 lref_t lport_location(lref_t port)
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

 lref_t lport_translate_mode(lref_t port)
 {
      if (!PORTP(port))
           vmerror_wrong_type(1, port);

      if (PORT_BINARYP(port))
           return boolcons(false);
      else
           return boolcons(PORT_TEXT_INFO(port)->_crlf_translate);
 }

 lref_t lport_set_translate_mode(lref_t port, lref_t mode)
 {
      if (!PORTP(port))
           vmerror_wrong_type(1, port);
      if (!BOOLP(mode))
           vmerror_wrong_type(2, mode);

      if (PORT_BINARYP(port))
           vmerror_unsupported(_T("cannot set translation mode of binary ports"));

      lflush_port(port);

      bool old_translate_mode = PORT_TEXT_INFO(port)->_crlf_translate;

      PORT_TEXT_INFO(port)->_crlf_translate = TRUEP(mode);

      return boolcons(old_translate_mode);
 }

 lref_t lport_io_counts(lref_t port)
 {
      if (!PORTP(port))
           vmerror_wrong_type(1, port);

      return lcons(fixcons(PORT_PINFO(port)->_bytes_read),
                   fixcons(PORT_PINFO(port)->_bytes_written));
 }

 lref_t lclose_port(lref_t port)
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

 lref_t lflush_port(lref_t port)
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



 lref_t lread_char(lref_t port)
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

 lref_t lunread_char(lref_t ch, lref_t port)
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


 lref_t lpeek_char(lref_t port)
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

lref_t lwrite_char(lref_t ch, lref_t port)
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


lref_t lwrite_strings(size_t argc, lref_t argv[])
{
     lref_t port = (argc < 1) ? NIL : argv[0];

     if (!PORTP(port))
          vmerror_wrong_type(1, port);

     for (size_t ii = 1; ii < argc; ii++)
     {
          lref_t str = argv[ii];

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


lref_t lchar_readyp(lref_t port)
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
               vmerror_unsupported(_T("char-ready? not supported on binary ports"));

          return boolcons(peek_char(port) != EOF);
     }
}


lref_t lflush_whitespace(lref_t port, lref_t slc)
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


lref_t lread_binary_string(lref_t l, lref_t port)
{
     _TCHAR buf[STACK_STRBUF_LEN];

     if (NULLP(port))
          port = CURRENT_INPUT_PORT();
     else if (!PORTP(port))
          vmerror_wrong_type(2, port);

     assert(PORTP(port));

     if (!PORT_BINARYP(port))
          vmerror_unsupported(_T("raw port operations not supported on text ports"));

     if (!NUMBERP(l))
          vmerror_wrong_type(1, l);

     fixnum_t remaining_length = get_c_fixnum(l);

     if (remaining_length <= 0)
          vmerror_arg_out_of_range(l, _T(">0"));

     lref_t new_str = strcons();
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


lref_t lread_binary_fixnum(lref_t l, lref_t sp, lref_t port)
{
     if (NULLP(port))
          port = CURRENT_INPUT_PORT();
     else if (!PORTP(port))
          vmerror_wrong_type(3, port);

     if (!PORT_BINARYP(port))
          vmerror_unsupported(_T("raw port operations not supported on text ports"));

     if (!NUMBERP(l))
          vmerror_wrong_type(1, l);
     if (!BOOLP(sp))
          vmerror_wrong_type(2, sp);

     fixnum_t length = get_c_fixnum(l);
     bool signedp = BOOLV(sp);

#ifdef SCAN_64BIT_FIXNUMS
     if ((length != 1) && (length != 2) && (length != 4) && (length != 8))
          vmerror_arg_out_of_range(l, _T("1, 2, 4, or 8"));
#else
     if ((length != 1) && (length != 2) && (length != 4))
          vmerror_arg_out_of_range(l, _T("1, 2, or 4"));
#endif

     fixnum_t result = 0;

     if (read_binary_fixnum(length, signedp, port, result))
          return fixcons(result);
     else
          return lmake_eof();
}

lref_t lread_binary_flonum(lref_t port)
{
     if (NULLP(port))
          port = CURRENT_INPUT_PORT();
     else if (!PORTP(port))
          vmerror_wrong_type(3, port);

     if (!PORT_BINARYP(port))
          vmerror_unsupported(_T("raw port operations not supported on text ports"));

     flonum_t result = 0;

     if (read_binary_flonum(port, result))
          return flocons(result);
     else
          return lmake_eof();
}

lref_t lread_line(lref_t port)
{
     int ch;

     if (NULLP(port))
          port = CURRENT_INPUT_PORT();
     else if (!PORTP(port))
          vmerror_wrong_type(1, port);

     assert(PORTP(port));

     lref_t op = lopen_output_string();

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

lref_t lwrite_binary_string(lref_t string, lref_t port)
{
     if (NULLP(port))
          port = CURRENT_OUTPUT_PORT();
     else if (!PORTP(port))
          vmerror_wrong_type(2, port);

     assert(PORTP(port));

     if (!PORT_BINARYP(port))
          vmerror_unsupported(_T("raw port operations not supported on text ports"));

     if (!STRINGP(string))
          vmerror_wrong_type(1, string);

     size_t length = STRING_DIM(string);
     _TCHAR *strdata = STRING_DATA(string);

     size_t chars_written = write_raw(strdata, sizeof(_TCHAR), length, port);

     if (chars_written < length)
          vmerror_io_error(_T("error writing to port."), port);

     return fixcons(chars_written);
}

lref_t lwrite_binary_fixnum(lref_t v, lref_t l, lref_t sp, lref_t port)
{
     if (NULLP(port))
          port = CURRENT_OUTPUT_PORT();
     else if (!PORTP(port))
          vmerror_wrong_type(4, port);

     assert(PORTP(port));

     if (!PORT_BINARYP(port))
          vmerror_unsupported(_T("raw port operations not supported on text ports"));

     if (!FIXNUMP(v))
          vmerror_wrong_type(1, v);

     if (!NUMBERP(l))
          vmerror_wrong_type(2, l);

     if (!BOOLP(sp))
          vmerror_wrong_type(3, sp);

     fixnum_t length = get_c_fixnum(l);
     bool signedp = BOOLV(sp);

#ifdef SCAN_64BIT_FIXNUMS
     if ((length != 1) && (length != 2) && (length != 4) && (length != 8))
          vmerror_arg_out_of_range(l, _T("1, 2, 4, or 8"));
#else
     if ((length != 1) && (length != 2) && (length != 4))
          vmerror_arg_out_of_range(l, _T("1, 2, or 4"));
#endif

     fixnum_t val = FIXNM(v);

     uint8_t bytes[sizeof(fixnum_t)];

     switch (length)
     {
     case 1:
          if (signedp)
               *(int8_t *) bytes = (int8_t) val;
          else
               *(uint8_t *) bytes = (uint8_t) val;
          break;

     case 2:
          if (signedp)
               *(int16_t *) bytes = (int16_t) val;
          else
               *(uint16_t *) bytes = (uint16_t) val;
          break;

     case 4:
          if (signedp)
               *(int32_t *) bytes = (int32_t) val;
          else
               *(uint32_t *) bytes = (uint32_t) val;
          break;

#ifdef SCAN_64BIT_FIXNUMS
     case 8:
          if (signedp)
               *(int64_t *) bytes = (int64_t) val;
          else
               *(uint64_t *) bytes = (uint64_t) val;
          break;
#endif
     }

     size_t fixnums_written = write_raw(bytes, (size_t) length, 1, port);

     if (fixnums_written < 1)
          vmerror_io_error(_T("error writing to port."), port);

     return fixcons(fixnums_written);
}

lref_t lbinary_write_flonum(lref_t v, lref_t port)
{
     if (NULLP(port))
          port = CURRENT_OUTPUT_PORT();
     else if (!PORTP(port))
          vmerror_wrong_type(4, port);

     assert(PORTP(port));

     if (!PORT_BINARYP(port))
          vmerror_unsupported(_T("raw port operations not supported on text ports"));

     if (!NUMBERP(v))
          vmerror_wrong_type(1, v);

     flonum_t val = get_c_double(v);

     uint8_t bytes[sizeof(flonum_t)];

     *(flonum_t *) bytes = val;

     size_t flonums_written = write_raw(bytes, sizeof(flonum_t), 1, port);

     if (flonums_written < 1)
          vmerror_io_error(_T("error writing to port."), port);

     return fixcons(flonums_written);
}


lref_t lrich_write(lref_t obj, lref_t machine_readable, lref_t port)
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


lref_t lnewline(lref_t port)
{
     if (NULLP(port))
          port = CURRENT_OUTPUT_PORT();

     if (!PORTP(port))
          vmerror_wrong_type(1, port);

     write_char(_T('\n'), port);

     return port;
}

lref_t lfresh_line(lref_t port)
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

/* Null port
 *
 * Input port - Always reads out EOF.
 * Output port - Accepts all writes.
 */

size_t null_port_read(void *buf, size_t size, size_t count, lref_t obj)
{
     UNREFERENCED(buf);
     UNREFERENCED(size);
     UNREFERENCED(count);
     UNREFERENCED(obj);

     return 0;
}

size_t null_port_write(const void *buf, size_t size, size_t count, lref_t obj)
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


lref_t lopen_null_input_port()
{
     return portcons(&null_port_class, NIL, (port_mode_t) (PORT_INPUT | PORT_BINARY),
                     NIL, NULL);
}

lref_t lopen_null_output_port()
{
     return portcons(&null_port_class, NIL, (port_mode_t) (PORT_OUTPUT | PORT_BINARY),
                     NIL, NULL);
}

END_NAMESPACE
