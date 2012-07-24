
 /*
  * io-text.cpp --
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

 /*** C I/O functions ***/

 int read_char(lref_t port)
 {
      int ch = EOF;

      if (NULLP(port))
           port = CURRENT_INPUT_PORT();

      assert(!NULLP(port));

      if (!PORT_INPUTP(port))
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

      if (!PORT_OUTPUTP(port))
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
            * by line seperators. write_raw is called for each block to
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

/*** Lisp I/O function ***/

 lref_t lport_column(lref_t port)
 {
      if (NULLP(port))
           port = CURRENT_INPUT_PORT();

      if (!PORTP(port))
           vmerror_wrong_type(1, port);

      if (PORT_BINARYP(port))
           vmerror_unsupported(_T("cannot get column of binary ports"));

      return fixcons(PORT_TEXT_INFO(port)->_column);
 }

 lref_t lport_row(lref_t port)
 {
      if (NULLP(port))
           port = CURRENT_INPUT_PORT();

      if (!PORTP(port))
           vmerror_wrong_type(1, port);

      if (PORT_BINARYP(port))
           vmerror_unsupported(_T("cannot get row of binary ports"));

      return fixcons(PORT_TEXT_INFO(port)->_row);
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

     if (PORT_INPUTP(port))
          ch = flush_whitespace(port, skip_lisp_comments);

     if (ch == EOF)
          return lmake_eof();
     else
          return charcons((_TCHAR) ch);
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


/*** Text port object ***/

size_t text_port_read(void *buf, size_t size, size_t count, lref_t obj)
{
     assert(PORTP(PORT_USER_OBJECT(obj)));

     return read_raw(buf, size, count, PORT_USER_OBJECT(obj));
}

size_t text_port_write(const void *buf, size_t size, size_t count, lref_t obj)
{
     assert(PORTP(PORT_USER_OBJECT(obj)));

     return write_raw(buf, size, count, PORT_USER_OBJECT(obj));
}

void text_port_flush(lref_t obj)
{
     assert(PORTP(PORT_USER_OBJECT(obj)));

     lflush_port(PORT_USER_OBJECT(obj));
}

void text_port_close(lref_t obj)
{
     assert(PORTP(PORT_USER_OBJECT(obj)));

     lclose_port(PORT_USER_OBJECT(obj));
}

port_class_t text_port_class = {
     _T("TEXT"),

     NULL,             // open
     text_port_read,   // read
     text_port_write,  // write
     NULL,             // rich_write
     text_port_flush,  // flush
     text_port_close,  // close
     NULL,             // gc_free
     NULL              // length
};

lref_t lopen_text_input_port(lref_t underlying)
{
     if (!PORTP(underlying))
          vmerror_wrong_type(1, underlying);

     if (!PORT_BINARYP(underlying))
          vmerror_unsupported(_T("cannot open text input on text port"));

     return portcons(&text_port_class,
                     lport_name(underlying),
                     PORT_INPUT,
                     underlying,
                     NULL);
}

lref_t lopen_text_output_port(lref_t underlying)
{
     if (!PORTP(underlying))
          vmerror_wrong_type(1, underlying);

     if (!PORT_BINARYP(underlying))
          vmerror_unsupported(_T("cannot open text output on text port"));

     return portcons(&text_port_class,
                     lport_name(underlying),
                     PORT_OUTPUT,
                     underlying,
                     NULL);
}

END_NAMESPACE
