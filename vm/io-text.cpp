
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

static int read_one_char(lref_t port)
{
     _TCHAR tch;

     if (read_bytes(port, &tch, sizeof(_TCHAR)) == 0)
          return EOF;

     return (int)tch;
 }

int read_char(lref_t port)
{
     int ch = EOF;

     if (NULLP(port))
          port = CURRENT_INPUT_PORT();

     assert(!NULLP(port));

     if (!PORT_INPUTP(port))
          return ch;

     if (PORT_BINARYP(port))
          ch = read_one_char(port);
     else
     {
          /* Read the next character, perhaps from the unread buffer... */
          if (PORT_TEXT_INFO(port)->unread_valid > 0)
          {
               PORT_TEXT_INFO(port)->unread_valid--;

               ch = PORT_TEXT_INFO(port)->unread_buffer[PORT_TEXT_INFO(port)->unread_valid];
          }
          else
          {
               ch = read_one_char(port);

               /* _crlf_translate mode forces all input newlines (CR, LF, CR+LF) into LF's. */
               if (PORT_TEXT_INFO(port)->crlf_translate)
               {
                    if (ch == '\r')
                    {
                         ch = '\n';
                         PORT_TEXT_INFO(port)->needs_lf = TRUE;
                    }
                    else if (PORT_TEXT_INFO(port)->needs_lf)
                    {
                         PORT_TEXT_INFO(port)->needs_lf = FALSE;

                         /*  Notice: this _returns_ from read_char, to avoid double
                          *  counting ch in the position counters. */
                         if (ch == '\n')
                              return read_char(port);
                    }
               }
          }

          /* Update the text position indicators */
          if (ch == '\n')
          {
               PORT_TEXT_INFO(port)->row++;
               PORT_TEXT_INFO(port)->previous_line_length = PORT_TEXT_INFO(port)->column;
               PORT_TEXT_INFO(port)->column = 0;
          }
          else
               PORT_TEXT_INFO(port)->column++;
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
           PORT_TEXT_INFO(port)->row--;
           PORT_TEXT_INFO(port)->column = PORT_TEXT_INFO(port)->previous_line_length;
           break;

      case '\r':
           break;

      default:
           PORT_TEXT_INFO(port)->column--;
           break;
      }

      if (PORT_TEXT_INFO(port)->unread_valid >= PORT_UNGET_BUFFER_SIZE)
           vmerror_io_error(_T("unget buffer exceeded."), port);

      PORT_TEXT_INFO(port)->unread_buffer[PORT_TEXT_INFO(port)->unread_valid] = ch;
      PORT_TEXT_INFO(port)->unread_valid++;

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
          return write_bytes(port, buf, count * sizeof(_TCHAR));

     if (!PORT_TEXT_INFO(port)->crlf_translate)
     {
          for (size_t ii = 0; ii < count; ii++)
          {
               if (buf[ii] == _T('\n'))
               {
                    PORT_TEXT_INFO(port)->row++;
                    PORT_TEXT_INFO(port)->column = 0;
               }
               else
                    PORT_TEXT_INFO(port)->column++;
          }

          return write_bytes(port, buf, count * sizeof(_TCHAR));
     }

     /* This code divides the text to be written into blocks seperated
      * by line seperators. write_bytes is called for each block to
      * actually do the write, and line seperators are correctly
      * translated to CR+LF pairs. */

     for (size_t pos = 0; pos < count;)
     {
          unsigned int c = _T('\0');
          size_t eoln_pos;

          /* Scan for the next eoln character, it ends the block... */
          for (eoln_pos = pos; (eoln_pos < count); eoln_pos++)
          {
               c = buf[eoln_pos];

               if ((c == '\n') || (c == '\r') || PORT_TEXT_INFO(port)->needs_lf)
                    break;
          }

          if (PORT_TEXT_INFO(port)->needs_lf)
          {
               assert(eoln_pos - pos == 0);

               if (buf[eoln_pos] == _T('\n'))
                    eoln_pos++;

               write_bytes(port, _T("\n"), sizeof(_TCHAR));

               PORT_TEXT_INFO(port)->needs_lf = false;
               PORT_TEXT_INFO(port)->row++;
          }
          else if (eoln_pos - pos == 0)
          {
               switch (c)
               {
               case _T('\n'):
                    write_bytes(port, _T("\r\n"), 2 * sizeof(_TCHAR));
                    PORT_TEXT_INFO(port)->column = 0;
                    PORT_TEXT_INFO(port)->row++;
                    break;

               case _T('\r'):
                    write_bytes(port, _T("\r"), sizeof(_TCHAR));
                    PORT_TEXT_INFO(port)->column = 0;
                    PORT_TEXT_INFO(port)->needs_lf = true;
                    break;

               default:
                    panic("Invalid case in write_text");
               }

               eoln_pos++;
          }
          else
          {
               size_t seg_len = eoln_pos - pos;

               PORT_TEXT_INFO(port)->column += seg_len;

               write_bytes(port, &(buf[pos]), seg_len * sizeof(_TCHAR));
          }

          pos = eoln_pos;
     }

     return count;
}

 static int flush_whitespace(lref_t port, bool skip_lisp_comments = true)
 {
      int c = '\0';

      bool commentp = false;

      for(;;)
      {
           /*  We can never be in a comment if we're not skipping them... */
           assert(skip_lisp_comments ? true : !commentp);

           c = read_char(port);

           if (c == EOF)
                break;

           if (commentp)
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

      return fixcons(PORT_TEXT_INFO(port)->column);
 }

 lref_t lport_row(lref_t port)
 {
      if (NULLP(port))
           port = CURRENT_INPUT_PORT();

      if (!PORTP(port))
           vmerror_wrong_type(1, port);

      if (PORT_BINARYP(port))
           vmerror_unsupported(_T("cannot get row of binary ports"));

      return fixcons(PORT_TEXT_INFO(port)->row);
 }

 lref_t lport_translate_mode(lref_t port)
 {
      if (!PORTP(port))
           vmerror_wrong_type(1, port);

      if (PORT_BINARYP(port))
           return boolcons(false);
      else
           return boolcons(PORT_TEXT_INFO(port)->crlf_translate);
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

      bool old_translate_mode = PORT_TEXT_INFO(port)->crlf_translate;

      PORT_TEXT_INFO(port)->crlf_translate = TRUEP(mode);

      return boolcons(old_translate_mode);
 }



lref_t lrich_write(lref_t obj, lref_t machine_readable, lref_t port)
{
     if (NULLP(port))
          port = CURRENT_OUTPUT_PORT();

     if (!PORTP(port))
          vmerror_wrong_type(3, port);

     if (PORT_CLASS(port)->rich_write == NULL)
          return boolcons(false);

     if (PORT_CLASS(port)->rich_write(port, obj, TRUEP(machine_readable)))
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
          skip_lisp_comments = TRUEP(slc);

     if (PORT_INPUTP(port))
          ch = flush_whitespace(port, skip_lisp_comments);

     if (ch == EOF)
          return lmake_eof();

     return charcons((_TCHAR) ch);
}


lref_t lread_line(lref_t port)
{
     int ch;

     if (NULLP(port))
          port = CURRENT_INPUT_PORT();

     if (!PORTP(port))
          vmerror_wrong_type(1, port);

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
         || ((PORT_TEXT_INFO(port)->column != 0) && !PORT_TEXT_INFO(port)->needs_lf))
     {
          lnewline(port);
          return boolcons(true);
     }

     return boolcons(false);
}


/*** Text port object ***/

size_t text_port_read_bytes(lref_t port, void *buf, size_t size)
{
     lref_t underlying = PORT_USER_OBJECT(port);

     assert(PORTP(underlying));

     return read_bytes(underlying, buf, size);
}

size_t text_port_write_bytes(lref_t port, const void *buf, size_t size)
{
     lref_t underlying = PORT_USER_OBJECT(port);

     assert(PORTP(underlying));

     return write_bytes(underlying, buf, size);
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

     NULL,                  // open
     text_port_read_bytes,  // read_bytes
     text_port_write_bytes, // write_bytes
     NULL,                  // rich_write
     text_port_flush,       // flush
     text_port_close,       // close
     NULL,                  // gc_free
     NULL                   // length
};

port_text_info_t *allocate_text_info()
{
     port_text_info_t *tinfo =
          (port_text_info_t *)gc_malloc(sizeof(port_text_info_t));

     memset(tinfo->unread_buffer, 0, sizeof(tinfo->unread_buffer));
     tinfo->unread_valid = 0;

     sys_info_t sinf;
     sys_get_info(&sinf);

     tinfo->crlf_translate = (sinf._eoln == SYS_EOLN_CRLF);
     tinfo->needs_lf = FALSE;
     tinfo->column = 0;
     tinfo->row = 1;
     tinfo->previous_line_length = 0;

     return tinfo;
}

lref_t lopen_text_input_port(lref_t underlying)
{
     if (!PORTP(underlying))
          vmerror_wrong_type(1, underlying);

     if (!PORT_BINARYP(underlying))
          vmerror_unsupported(_T("cannot open text input on text port"));

     lref_t port = portcons(&text_port_class,
                            lport_name(underlying),
                            PORT_INPUT,
                            underlying,
                            NULL);

     SET_PORT_TEXT_INFO(port, allocate_text_info());

     return port;
}

lref_t lopen_text_output_port(lref_t underlying)
{
     if (!PORTP(underlying))
          vmerror_wrong_type(1, underlying);

     if (!PORT_BINARYP(underlying))
          vmerror_unsupported(_T("cannot open text output on text port"));

     lref_t port = portcons(&text_port_class,
                            lport_name(underlying),
                            PORT_OUTPUT,
                            underlying,
                            NULL);

     SET_PORT_TEXT_INFO(port, allocate_text_info());

     return port;
}

END_NAMESPACE
