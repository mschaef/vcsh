
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

INLINE lref_t PORT_UNDERLYING(lref_t text_port)
{
     lref_t underlying = PORT_USER_OBJECT(text_port);

     assert(PORTP(underlying));

     return underlying;
}

/*** C I/O functions ***/
int read_char(lref_t port)
{
     assert(TEXT_PORTP(port) && PORT_INPUTP(port));

     /* Text port case below. */

     int ch = EOF;

     if (PORT_TEXT_INFO(port)->pbuf_valid)          
     {
          /* Unread buffer */
          PORT_TEXT_INFO(port)->pbuf_valid = false;

          ch = PORT_TEXT_INFO(port)->pbuf;
     }
     else
     {
          /* Specific string read handling. */
          _TCHAR tch;

          if (PORT_CLASS(port)->read_chars(port, &tch, 1) > 0)
               ch = (tch);
     }

     /* Update the text position indicators */
     if (ch == '\n')
     {
          PORT_TEXT_INFO(port)->pline_mcol = PORT_TEXT_INFO(port)->col;
          PORT_TEXT_INFO(port)->col = 0;
          PORT_TEXT_INFO(port)->row++;
     }
     else
          PORT_TEXT_INFO(port)->col++;

     return ch;
}

int peek_char(lref_t port)
{
     assert(TEXT_PORTP(port) && PORT_INPUTP(port));

     int ch = read_char(port);

     if (ch == EOF)
          return ch;

     /* Update unread buffer. */
     switch (ch)
     {
     case '\n':
          PORT_TEXT_INFO(port)->col = PORT_TEXT_INFO(port)->pline_mcol;
          PORT_TEXT_INFO(port)->row--;
          break;

     case '\r':
          break;

     default:
          PORT_TEXT_INFO(port)->col--;
          break;
     }

     assert(!PORT_TEXT_INFO(port)->pbuf_valid);

     PORT_TEXT_INFO(port)->pbuf = ch;
     PORT_TEXT_INFO(port)->pbuf_valid = true;

     return ch;
}

void write_char(lref_t port, int ch)
{
     assert(TEXT_PORTP(port) && PORT_OUTPUTP(port));

     _TCHAR tch = (_TCHAR) ch;

     write_text(port, &tch, 1);

     if (tch == _T('\n'))
          lflush_port(port);
}

size_t write_text(lref_t port, const _TCHAR * buf, size_t count)
{
     assert(TEXT_PORTP(port) && PORT_OUTPUTP(port));

     return PORT_CLASS(port)->write_chars(port, buf, count);
}

/*** Lisp I/O function ***/

lref_t lport_column(lref_t port)
{
     if (NULLP(port))
          port = CURRENT_INPUT_PORT();

     if (!TEXT_PORTP(port))
          vmerror_wrong_type_n(1, port);

     return fixcons(PORT_TEXT_INFO(port)->col);
}

lref_t lport_row(lref_t port)
{
     if (NULLP(port))
          port = CURRENT_INPUT_PORT();

     if (!TEXT_PORTP(port))
          vmerror_wrong_type_n(1, port);

     return fixcons(PORT_TEXT_INFO(port)->row);
}

lref_t lport_translate_mode(lref_t port)
{
     if (!TEXT_PORTP(port))
          vmerror_wrong_type_n(1, port);

     return boolcons(PORT_TEXT_INFO(port)->translate);
}

lref_t lport_set_translate_mode(lref_t port, lref_t mode)
{
     if (!TEXT_PORTP(port))
          vmerror_wrong_type_n(1, port);

     if (!BOOLP(mode))
          vmerror_wrong_type_n(2, mode);

     lflush_port(port);

     bool old_translate_mode = PORT_TEXT_INFO(port)->translate;

     PORT_TEXT_INFO(port)->translate = TRUEP(mode);

     return boolcons(old_translate_mode);
}


/*** Text Input ***/

lref_t lread_char(lref_t port)
{
     if (NULLP(port))
          port = CURRENT_INPUT_PORT();
      
     if (!TEXT_PORTP(port))
          vmerror_wrong_type_n(1, port);

     int ch = read_char(port);

     if (ch == EOF)
          return lmake_eof();
     else
          return charcons((_TCHAR) ch);
}


lref_t lpeek_char(lref_t port)
{
     int ch;

     if (NULLP(port))
          port = CURRENT_INPUT_PORT();

     if (!TEXT_PORTP(port))
          vmerror_wrong_type_n(1, port);

     ch = peek_char(port);

     if (ch == EOF)
          return lmake_eof();
     else
          return charcons((_TCHAR) ch);
}

static void flush_lisp_comment(lref_t port)
{
     int c = '\0';

     while((c != EOF) && (c != _T('\n')))
          c = read_char(port);
}

static int flush_whitespace(lref_t port, bool skip_lisp_comments)
{
     int c = '\0';

     while(c != EOF)
     {
          c = peek_char(port);
          
          if ((c == _T(';')) && skip_lisp_comments) {
               flush_lisp_comment(port);
               continue;
          }
                 
          if (!_istspace(c) && (c != _T('\0')))
               return c;

          read_char(port);
     }
}


/*** Rich Output ***/

lref_t lrich_write(lref_t obj, lref_t machine_readable, lref_t port)
{
     if (NULLP(port))
          port = CURRENT_OUTPUT_PORT();

     if (!PORTP(port))
          vmerror_wrong_type_n(3, port);

     if (PORT_INPUTP(port))
          vmerror_unsupported(_T("cannot rich-write to input ports"));

     if (PORT_CLASS(port)->rich_write == NULL)
          return boolcons(false);

     if (PORT_CLASS(port)->rich_write(port, obj, TRUEP(machine_readable)))
          return port;

     return boolcons(false);
}

/*** Text Output ***/

lref_t lwrite_char(lref_t ch, lref_t port)
{
     if (NULLP(port))
          port = CURRENT_OUTPUT_PORT();

     if (!TEXT_PORTP(port))
          vmerror_wrong_type_n(2, port);

     if (PORT_INPUTP(port))
          vmerror_unsupported(_T("cannot write-char to input ports"));

     if (!CHARP(ch))
          vmerror_wrong_type_n(1, ch);

     write_char(port, CHARV(ch));

     return port;
}


lref_t lwrite_strings(size_t argc, lref_t argv[])
{
     lref_t port = (argc < 1) ? NIL : argv[0];

     if (NULLP(port))
          port = CURRENT_OUTPUT_PORT();

     if (!TEXT_PORTP(port))
          vmerror_wrong_type_n(1, port);

     if (PORT_INPUTP(port))
          vmerror_unsupported(_T("cannot write-strings to input ports"));

     for (size_t ii = 1; ii < argc; ii++)
     {
          lref_t str = argv[ii];

          if (STRINGP(str))
               write_text(port, STRING_DATA(str), STRING_DIM(str));
          else if (CHARP(str))
          {
               _TCHAR ch = CHARV(str);

               write_text(port, &ch, 1);
          }
          else
               vmerror_wrong_type_n(ii, str);
     }

     return port;
}

lref_t lflush_whitespace(lref_t port, lref_t slc)
{
     int ch = EOF;

     if (NULLP(port))
          port = CURRENT_INPUT_PORT();

     if (!TEXT_PORTP(port))
          vmerror_wrong_type_n(1, port);

     if (PORT_OUTPUTP(port))
          vmerror_unsupported(_T("cannot flush-whitespace output ports"));

     bool skip_lisp_comments = true;

     if (!NULLP(slc))
          skip_lisp_comments = TRUEP(slc);

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

     if (!TEXT_PORTP(port))
          vmerror_wrong_type_n(1, port);

     if (PORT_OUTPUTP(port))
          vmerror_unsupported(_T("cannot read-line from output ports"));

     lref_t op = lopen_output_string();

     bool read_anything = false;

     for (ch = read_char(port); (ch != EOF) && (ch != _T('\n')); ch = read_char(port))
     {
          read_anything = true;

          write_char(op, ch);
     }

     if (!read_anything && (ch == EOF))
          return lmake_eof();

     return lget_output_string(op);
}

lref_t lnewline(lref_t port)
{
     if (NULLP(port))
          port = CURRENT_OUTPUT_PORT();

     if (!TEXT_PORTP(port))
          vmerror_wrong_type_n(1, port);

     if (PORT_INPUTP(port))
          vmerror_unsupported(_T("cannot newline to input ports"));

     write_char(port, _T('\n'));

     return port;
}

lref_t lfresh_line(lref_t port)
{
     if (NULLP(port))
          port = CURRENT_OUTPUT_PORT();

     if (!TEXT_PORTP(port))
          vmerror_wrong_type_n(1, port);


     if (PORT_INPUTP(port))
          vmerror_unsupported(_T("cannot fresh-line to input ports"));

     if ((PORT_TEXT_INFO(port)->col != 0) && !PORT_TEXT_INFO(port)->needs_lf)
     {
          lnewline(port);
          return boolcons(true);
     }

     return boolcons(false);
}


/*** Text port object ***/


struct port_text_info_t *allocate_text_info()
{
     struct port_text_info_t *tinfo = gc_malloc(sizeof(*tinfo));

     tinfo->pbuf = 0;
     tinfo->pbuf_valid = false;

     tinfo->translate = (sys_get_eoln_convention() == SYS_EOLN_CRLF);
     tinfo->needs_lf = FALSE;
     tinfo->col = 0;
     tinfo->row = 1;
     tinfo->pline_mcol = 0;

     tinfo->str_ofs = -1;

     return tinfo;
}

void text_port_open(lref_t port)
{
     SET_PORT_TEXT_INFO(port, allocate_text_info());
}

size_t text_port_read_chars(lref_t port, _TCHAR *buf, size_t size)
{
     size_t chars_read = 0;

     while(chars_read < size)
     {
          _TCHAR ch;

          if (read_bytes(PORT_UNDERLYING(port), &ch, sizeof(_TCHAR)) == 0)
               break;

          /* translation mode forces all input newlines (CR, LF,
           * CR+LF) into LF's.
           */
          if (PORT_TEXT_INFO(port)->translate)
          {
               if (ch == '\r')
               {
                    ch = '\n';
                    PORT_TEXT_INFO(port)->needs_lf = TRUE;
               }
               else if (PORT_TEXT_INFO(port)->needs_lf)
               {
                    PORT_TEXT_INFO(port)->needs_lf = FALSE;

                    /*  Avoid double counting newline. */
                    if (ch == '\n')
                         continue;
               }
          }

          buf[chars_read++] = ch;
     }

     return chars_read;
}

size_t text_port_write_chars(lref_t port, const _TCHAR *buf, size_t count)
{
     /* This code divides the text to be written into blocks seperated
      * by line seperators. write_bytes is called for each block to
      * actually do the write, and line seperators are correctly
      * translated to CR+LF pairs. */
     for (size_t pos = 0; pos < count;)
     {
          unsigned int c = _T('\0');
          /* Emit a needed LF, if necessary. */
          if (PORT_TEXT_INFO(port)->needs_lf)
          {
               if (buf[pos] == _T('\n'))
                    pos++;

               write_bytes(PORT_UNDERLYING(port), _T("\n"), sizeof(_TCHAR));

               PORT_TEXT_INFO(port)->needs_lf = false;
               PORT_TEXT_INFO(port)->row++;

               continue;
          }

          /* Scan for the next eoln character, it ends the block... */
          size_t eoln_pos;

          for (eoln_pos = pos; (eoln_pos < count); eoln_pos++)
          {
               c = buf[eoln_pos];

               if ((c == '\n') || (c == '\r'))
                    break;
          }

          size_t seg_len = eoln_pos - pos;

          if (seg_len  == 0)
          {
               switch (c)
               {
               case _T('\n'):
                    if (PORT_TEXT_INFO(port)->translate)
                         write_bytes(PORT_UNDERLYING(port), _T("\r\n"), 2 * sizeof(_TCHAR));
                    else
                         write_bytes(PORT_UNDERLYING(port), _T("\n"), sizeof(_TCHAR));
                    PORT_TEXT_INFO(port)->col = 0;
                    PORT_TEXT_INFO(port)->row++;
                    break;

               case _T('\r'):
                    write_bytes(PORT_UNDERLYING(port), _T("\r"), sizeof(_TCHAR));
                    PORT_TEXT_INFO(port)->col = 0;
                    PORT_TEXT_INFO(port)->needs_lf = PORT_TEXT_INFO(port)->translate;
                    break;

               default:
                    panic("Invalid case in write_text");
               }

               eoln_pos++;
          }
          else
          {
               PORT_TEXT_INFO(port)->col += seg_len;

               write_bytes(PORT_UNDERLYING(port), &(buf[pos]), seg_len * sizeof(_TCHAR));
          }

          pos = eoln_pos;
     }

     return count;
}

void text_port_flush(lref_t obj)
{
     lflush_port(PORT_UNDERLYING(obj));
}

void text_port_close(lref_t obj)
{
     lclose_port(PORT_UNDERLYING(obj));
}

struct port_class_t text_port_class = {
     _T("TEXT"),

     text_port_open,        // open
     NULL,                  // read_bytes
     NULL,                  // write_bytes
     NULL,                  // peek_char
     text_port_read_chars,  // read_chars
     text_port_write_chars, // write_chars
     NULL,                  // rich_write
     text_port_flush,       // flush
     text_port_close,       // close
     NULL,                  // gc_free
     NULL                   // length
};

lref_t lopen_text_input_port(lref_t underlying)
{
     if (!PORTP(underlying))
          vmerror_wrong_type_n(1, underlying);

     if (!BINARY_PORTP(underlying))
          vmerror_unsupported(_T("cannot open text input on binary port"));

     return portcons(&text_port_class,
                     lport_name(underlying),
                     PORT_INPUT,
                     underlying,
                     NULL);
}

lref_t lopen_text_output_port(lref_t underlying)
{
     if (!PORTP(underlying))
          vmerror_wrong_type_n(1, underlying);

     if (!BINARY_PORTP(underlying))
          vmerror_unsupported(_T("cannot open text output on binary port"));

     return portcons(&text_port_class,
                     lport_name(underlying),
                     PORT_OUTPUT,
                     underlying,
                     NULL);
}

