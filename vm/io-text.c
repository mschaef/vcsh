/*
 * io-text.c --
 *
 * The I/O subsystem. This tries to be as R5RS compliant as possible.
 *
 * (C) Copyright 2001-2022 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "LICENSE" for information on usage and redistribution
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

     if (PORT_TEXT_INFO(port)->pbuf_valid) {
          /* Unread buffer */
          PORT_TEXT_INFO(port)->pbuf_valid = false;

          ch = PORT_TEXT_INFO(port)->pbuf;
     } else {
          /* Specific string read handling. */
          _TCHAR tch;

          if (PORT_CLASS(port)->read_chars(port, &tch, 1) > 0)
               ch = (tch);
     }

     /* Update the text position indicators */
     if (ch == '\n') {
          PORT_TEXT_INFO(port)->pline_mcol = PORT_TEXT_INFO(port)->col;
          PORT_TEXT_INFO(port)->col = 0;
          PORT_TEXT_INFO(port)->row++;
     } else
          PORT_TEXT_INFO(port)->col++;

     return ch;
}

int peek_char(lref_t port)
{
     assert(TEXT_PORTP(port) && PORT_INPUTP(port));

     if (PORT_CLASS(port)->peek_char == NULL)
          vmerror_unsupported(_T("Peek not supported on this port."));

     return PORT_CLASS(port)->peek_char(port);
}

void write_char(lref_t port, _TCHAR ch)
{
     assert(TEXT_PORTP(port) && PORT_OUTPUTP(port));

     write_text(port, &ch, 1);

     if (ch == _T('\n'))
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

     while(c != EOF) {
          c = peek_char(port);

          if ((c == _T(';')) && skip_lisp_comments) {
               flush_lisp_comment(port);
               continue;
          }

          if (!_istspace(c) && (c != _T('\0')))
               break;

          read_char(port);
     }

     return c;
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
     if (argc < 1)
          vmerror_unsupported(_T("Must specify port to write-strings"));

     lref_t port = argv[0];

     if (!TEXT_PORTP(port))
          vmerror_wrong_type_n(1, port);

     if (PORT_INPUTP(port))
          vmerror_unsupported(_T("cannot write-strings to input ports"));

     for (size_t ii = 1; ii < argc; ii++) {
          lref_t str = argv[ii];

          if (STRINGP(str)) {
               write_text(port, str->as.string.data, str->as.string.dim);
          } else if (CHARP(str)) {
               _TCHAR ch = CHARV(str);

               write_text(port, &ch, 1);
          } else
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

     /* A line ends at LF or CR+LF, and the terminator isn't returned. A CR
      * is held back until the next character shows whether it's part
      * of a CR+LF; any other CR is an ordinary character. */
     bool pending_cr = false;

     for (ch = read_char(port); (ch != EOF) && (ch != _T('\n')); ch = read_char(port)) {
          read_anything = true;

          if (pending_cr)
               write_char(op, _T('\r'));

          pending_cr = (ch == _T('\r'));

          if (!pending_cr)
               write_char(op, ch);
     }

     if (pending_cr && (ch == EOF))
          write_char(op, _T('\r'));

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

     if (PORT_TEXT_INFO(port)->col != 0) {
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

int text_port_peek_char(lref_t port)
{
     int ch = read_char(port);

     if (ch == EOF)
          return ch;

     assert(!PORT_TEXT_INFO(port)->pbuf_valid);

     /* Undo read_char's position update; the character will be
      * counted again when it is actually read. */
     if (ch == '\n') {
          PORT_TEXT_INFO(port)->col = PORT_TEXT_INFO(port)->pline_mcol;
          PORT_TEXT_INFO(port)->row--;
     } else
          PORT_TEXT_INFO(port)->col--;

     /* Update unread buffer. */
     PORT_TEXT_INFO(port)->pbuf = ch;
     PORT_TEXT_INFO(port)->pbuf_valid = true;

     return ch;
}

size_t text_port_read_chars(lref_t port, _TCHAR *buf, size_t size)
{
     size_t chars_read = 0;

     while(chars_read < size) {
          _TCHAR ch;

          if (read_bytes(PORT_UNDERLYING(port), &ch, sizeof(_TCHAR)) == 0)
               break;

          buf[chars_read++] = ch;
     }

     return chars_read;
}

size_t text_port_write_chars(lref_t port, const _TCHAR *buf, size_t count)
{
     /* Line endings are written as given. LF is the only line break for
      * position tracking; every other character, CR included, counts as
      * one column. */
     for (size_t pos = 0; pos < count; pos++) {
          if (buf[pos] == _T('\n')) {
               PORT_TEXT_INFO(port)->col = 0;
               PORT_TEXT_INFO(port)->row++;
          } else
               PORT_TEXT_INFO(port)->col++;
     }

     write_bytes(PORT_UNDERLYING(port), buf, count * sizeof(_TCHAR));

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
     text_port_peek_char,   // peek_char
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
                     PORT_INPUT | PORT_TEXT,
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
                     PORT_OUTPUT | PORT_TEXT,
                     underlying,
                     NULL);
}

