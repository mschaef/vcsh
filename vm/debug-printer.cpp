
/* debug-printer.cpp
 *
 * The debug printer. This is the printer used to print Lisp objects from C code,
 * and it's supposed to work, even if the lisp interpreter is in an odd state and
 * cannot be trusted.
 */

#include "scan.h"

BEGIN_NAMESPACE(scan)
static void debug_print_flonum(LRef object, LRef port, bool machine_readable)
{
     _TCHAR buf[STACK_STRBUF_LEN];

     UNREFERENCED(machine_readable);
     assert(FLONUMP(object));

     if (isnan(FLONM(object)))
     {
          _sntprintf(buf, STACK_STRBUF_LEN, _T("#inan"));
     }
     else if (!finite(FLONM(object)))
     {
          if (FLONM(object) > 0)
               _sntprintf(buf, STACK_STRBUF_LEN, _T("#iposinf"));
          else
               _sntprintf(buf, STACK_STRBUF_LEN, _T("#ineginf"));
     }
     else
     {
          int digits = DEBUG_FLONUM_PRINT_PRECISION;

          assert((digits >= 0) && (digits <= 16));

          /* Nothing is as easy as it seems...
           *
           * The sprintf 'g' format code will drop the decimal
           * point if all following digits are zero. That causes
           * the reader to read such numbers as exact, rather than
           * inexact. As a result, we need to implement our own
           * switching between scientific and conventional notation.
           */
          double scale = 0.0;

          if (FLONM(object) != 0.0)
               scale = log10(fabs(FLONM(object)));

          if (fabs(scale) >= digits)
               _sntprintf(buf, STACK_STRBUF_LEN, _T("%.*e"), digits, FLONM(object));
          else
          {
               /* Prevent numbers on the left of the decimal point from
                * adding to the number of digits we print. */
               if ((scale > 0) && (scale <= digits))
                    digits -= (int) scale;

               _sntprintf(buf, STACK_STRBUF_LEN, _T("%.*f"), digits, FLONM(object));
          }
     }

     write_text(buf, _tcslen(buf), port);

     if (COMPLEXP(object))
     {
          if (CMPLXIM(object) >= 0.0)
               write_text(_T("+"), 1, port);

          debug_print_flonum(FLOIM(object), port, machine_readable);

          write_text(_T("i"), 1, port);
     }
}


static void debug_print_string(LRef obj, LRef port, bool machine_readable)
{
     assert(STRINGP(obj));

     if (!machine_readable)
     {
          write_text(STRING_DATA(obj), STRING_DIM(obj), port);
          return;
     }

     WRITE_TEXT_CONSTANT(_T("\""), port);

     size_t next_char_to_write = 0;

     _TCHAR cbuff[2];

     /* To write strings more efficiently, this code scans for the longest
      * block of characters that don't need special encoding, and then
      * passes those blocks on to write_raw. */
     while (next_char_to_write < STRING_DIM(obj))
     {
          unsigned int c;
          size_t next_special_char;

          /* Scan for the next special character, it ends the block... */
          for (next_special_char = next_char_to_write;
               next_special_char < STRING_DIM(obj); next_special_char++)
          {
               c = STRING_DATA(obj)[next_special_char];

               if ((c == '\\') || (c == '"') || (c == '\n') || (c == '\r')
                   || (c == '\t') || (c == '\0') || (c < 32) || (c >= 127))
                    break;
          }

          /* ...which then gets written out. */
          if (next_special_char - next_char_to_write > 0)
               write_text(&(STRING_DATA(obj)[next_char_to_write]),
                          next_special_char - next_char_to_write, port);

          if (next_special_char >= STRING_DIM(obj))
               break;

          c = STRING_DATA(obj)[next_special_char];

          /* Write the next special character. */
          switch (c)
          {
          case '\\':
          case '"':
               cbuff[0] = _T('\\');
               cbuff[1] = (_TCHAR) c;

               write_text(cbuff, 2, port);
               break;

          case '\n':
               WRITE_TEXT_CONSTANT(_T("\\n"), port);
               break;
          case '\r':
               WRITE_TEXT_CONSTANT(_T("\\r"), port);
               break;
          case '\t':
               WRITE_TEXT_CONSTANT(_T("\\t"), port);
               break;
          case '\0':
               WRITE_TEXT_CONSTANT(_T("\\000"), port);
               break;
          default:
               /* This assert will only fail when the special character scanner
                * breaks on a character that the special character writer
                * does not know how to handle. */
               assert((c < 32) || (c >= 127));
               scwritef(_T("\\~cC"), port, (unsigned long) c);
          }

          next_char_to_write = next_special_char + 1;
     }

     WRITE_TEXT_CONSTANT(_T("\""), port);
}


static void debug_print_hash_elements(LRef obj, LRef port, bool machine_readable)
{
     assert(HASHP(obj));

     fixnum_t count = 0;

     LRef key, val;

     hash_iter_t ii;
     hash_iter_begin(obj, &ii);
     while (hash_iter_next(obj, &ii, &key, &val))
     {
          count++;

          write_char(_T(' '), port);

          debug_print_object(key, port, machine_readable);
          write_char(_T(' '), port);
          debug_print_object(val, port, machine_readable);
     }
}

static void debug_print_hash(LRef obj, LRef port, bool machine_readable)
{
     assert(HASHP(obj));

     WRITE_TEXT_CONSTANT(_T("#h("), port);

     debug_print_object(lhash_type(obj), port, machine_readable);

     debug_print_hash_elements(obj, port, machine_readable);

     write_char(')', port);
}

static void debug_print_instance(LRef obj, LRef port, bool machine_readable)
{
     assert(INSTANCEP(obj));

     WRITE_TEXT_CONSTANT(_T("#<instance("), port);

     debug_print_object(INSTANCE_PROTO(obj), port, machine_readable);

     WRITE_TEXT_CONSTANT(_T(")"), port);

     for (size_t ii = 0; ii < INSTANCE_DIM(obj); ii++)
     {
          WRITE_TEXT_CONSTANT(_T(" "), port);
          debug_print_object(INSTANCE_ELEM(obj, ii), port, machine_readable);
     }

     write_char('>', port);
}

const _TCHAR *charnames[] = {
     _T("nul"), _T("soh"), _T("stx"), _T("etx"),
     _T("eot"), _T("eng"), _T("ack"), _T("bel"),
     _T("bs"), _T("tab"), _T("newline"), _T("vtab"),
     _T("formfeed"), _T("cr"), _T("so"), _T("si"),
     _T("dle"), _T("dc1"), _T("dc2"), _T("dc3"),
     _T("dc4"), _T("nak"), _T("syn"), _T("etb"),
     _T("can"), _T("em"), _T("sub"), _T("esc"),
     _T("fs"), _T("gs"), _T("rs"), _T("us"),
     _T("space"), NULL
};

LRef debug_print_object(LRef obj, LRef port, bool machine_readable)
{
     _TCHAR buf[STACK_STRBUF_LEN];

     STACK_CHECK(&obj);

     if (DEBUG_FLAG(DF_PRINT_ADDRESSES))
          scwritef("#@~c&=", port, obj);

     LRef tmp;
     size_t ii;
     LRef slots;
     const _TCHAR *fast_op_name;

     switch (TYPE(obj))
     {
     case TC_NIL:
          WRITE_TEXT_CONSTANT(_T("()"), port);
          break;

     case TC_BOOLEAN:
          if (BOOLV(obj))
          {
               WRITE_TEXT_CONSTANT(_T("#t"), port);
          }
          else
          {
               WRITE_TEXT_CONSTANT(_T("#f"), port);
          }
          break;

     case TC_CONS:
          write_char(_T('('), port);
          debug_print_object(lcar(obj), port, machine_readable);

          for (tmp = lcdr(obj); CONSP(tmp); tmp = lcdr(tmp))
          {
               write_char(_T(' '), port);
               debug_print_object(lcar(tmp), port, machine_readable);
          }

          if (!NULLP(tmp))
          {
               WRITE_TEXT_CONSTANT(_T(" . "), port);
               debug_print_object(tmp, port, machine_readable);
          }

          write_char(_T(')'), port);
          break;

     case TC_FIXNUM:
          _sntprintf(buf, STACK_STRBUF_LEN, _T(FIXNUM_PRINTF_PREFIX "i"), FIXNM(obj));
          write_text(buf, _tcslen(buf), port);
          break;

     case TC_FLONUM:
          debug_print_flonum(obj, port, machine_readable);
          break;

     case TC_CHARACTER:
          if (machine_readable)
          {
               if (CHARV(obj) < CHARNAMECOUNT)
                    scwritef(_T("#\\~cs"), port, charnames[(size_t) CHARV(obj)]);
               else if (CHARV(obj) >= CHAREXTENDED - 1)
                    scwritef(_T("#\\<~cd>"), port, (int) CHARV(obj));
               else
                    scwritef(_T("#\\~cc"), port, (int) CHARV(obj));
          }
          else
               scwritef(_T("~cc"), port, (int) CHARV(obj));
          break;

     case TC_SYMBOL:
          if (NULLP(SYMBOL_HOME(obj)))
          {
               if (DEBUG_FLAG(DF_PRINT_FOR_DIFF))
                    scwritef("#:<uninterned-symbol>", port);
               else
                    scwritef("#:~a@~c&", port, SYMBOL_PNAME(obj), obj);
          }
          else if (SYMBOL_HOME(obj) == interp.keyword_package)
               scwritef(":~a", port, SYMBOL_PNAME(obj));
          else
          {
               /*  With only a minimal c-level package implementation, we just assume
                *  every symbol is private. */
               scwritef("~a::~a", port, PACKAGE_NAME(SYMBOL_HOME(obj)), SYMBOL_PNAME(obj));
          }
          break;

     case TC_VECTOR:
          WRITE_TEXT_CONSTANT(_T("#("), port);

          for (ii = 0; ii < VECTOR_DIM(obj); ii++)
          {
               debug_print_object(VECTOR_ELEM(obj, ii), port, true);

               if (ii + 1 < VECTOR_DIM(obj))
                    write_char(_T(' '), port);
          }

          write_char(_T(')'), port);
          break;

     case TC_STRUCTURE:
          WRITE_TEXT_CONSTANT(_T("#S("), port);

          debug_print_object(CAR(STRUCTURE_LAYOUT(obj)), port, true);

          for (ii = 0, slots = CAR(CDR(STRUCTURE_LAYOUT(obj)));
               ii < STRUCTURE_DIM(obj); ii++, slots = CDR(slots))
          {
               WRITE_TEXT_CONSTANT(_T(" "), port);
               debug_print_object(CAR(CAR(slots)), port, true);
               WRITE_TEXT_CONSTANT(_T(" "), port);
               debug_print_object(STRUCTURE_ELEM(obj, ii), port, true);
          }

          WRITE_TEXT_CONSTANT(_T(")"), port);
          break;

     case TC_STRING:
          debug_print_string(obj, port, machine_readable);
          break;
     case TC_HASH:
          debug_print_hash(obj, port, machine_readable);
          break;
     case TC_INSTANCE:
          debug_print_instance(obj, port, machine_readable);
          break;
     case TC_PACKAGE:
          scwritef("~u ~a", port, (LRef) obj, PACKAGE_NAME(obj));
          break;

     case TC_SUBR:
          scwritef("~u,~cd:~a", port, (LRef) obj, SUBR_TYPE(obj), SUBR_NAME(obj));
          break;

     case TC_CLOSURE:
          if (DEBUG_FLAG(DF_PRINT_CLOSURE_CODE))
               scwritef("~u\n\tcode:~s\n\tenv:~s\n\tp-list:~s", port,
                        (LRef) obj, CLOSURE_CODE(obj), CLOSURE_ENV(obj),
                        CLOSURE_PROPERTY_LIST(obj));

          else
               scwritef("~u", port, (LRef) obj);
          break;

     case TC_VALUES_TUPLE:
          scwritef("~u ~s", port, (LRef) obj, VALUES_TUPLE_VALUES(obj));
          break;

     case TC_MACRO:
          if (DEBUG_FLAG(DF_PRINT_CLOSURE_CODE))
               scwritef("~u ~s", port, (LRef) obj, MACRO_TRANSFORMER(obj));
          else
               scwritef("~u", port, (LRef) obj);
          break;

     case TC_END_OF_FILE:
          scwritef("~u", port, (LRef) obj);
          break;

     case TC_PORT:
          scwritef(_T("~u~s~cs ~cs ~s"), port,
                   obj,
                   lport_mode(obj),
                   (PORT_MODE(obj) & PORT_BINARY) ? "(binary)" : "",
                   PORT_CLASS(obj)->_name, PORT_PINFO(obj)->_port_name);
          break;

     case TC_FAST_OP:
          fast_op_name = fast_op_opcode_name((fast_op_opcode_t)FAST_OP_OPCODE(obj));

          if (fast_op_name)
               scwritef("#<FOP@~c&:~cs ~s ~s>", port, (LRef) obj,
                        fast_op_name, FAST_OP_ARG1(obj), FAST_OP_ARG2(obj), FAST_OP_ARG3(obj));
          else
               scwritef("#<FOP@~c&:~cd ~s ~s>", port, (LRef) obj,
                        FAST_OP_OPCODE(obj), FAST_OP_ARG1(obj), FAST_OP_ARG2(obj), FAST_OP_ARG3(obj));
          break;

     case TC_UNBOUND_MARKER:
          scwritef("#<UNBOUND-MARKER>", port);
          break;

     case TC_FREE_CELL:
          scwritef("#<FREE CELL -- Forget a call to gc_mark? ~c&>", port, obj);
          break;

     default:
          scwritef("#<INVALID OBJECT - UNKNOWN TYPE ~c&>", port, obj);
     }

     return port;
}

LRef lidebug_printer(LRef obj, LRef port, LRef machine_readable_p)
{
     if (!PORTP(port))
          vmerror_wrong_type(2, port);

     return debug_print_object(obj, port, TRUEP(machine_readable_p));
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
                    scwritef("#<~cs@(no-addr)", port, typecode_name(TYPE(unprintable_object)));
               else
                    scwritef("#<~cs@~c&", port,
                             typecode_name(TYPE(unprintable_object)), unprintable_object);
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
                    panic(_T("Invalid C object format character in scwritef"));
                    break;
               };
               break;

          default:
               panic(_T("Invalid format character in scwritef"));
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
     if (!DEBUG_FLAG(flag))
          return;

     va_list arglist;

     va_start(arglist, format_str);
     
     scvwritef(format_str, VM_DEBUG_PORT(), arglist);
     lflush_port(VM_DEBUG_PORT());
}


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
     NULL,
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

void init_debugger_output()
{
     SET_TYPE(&interp.debugger_output, TC_PORT);

     initialize_port(&interp.debugger_output,
                     DEBUG_FLAG(DF_DEBUGGER_TO_ODS) ? &debug_port_class : &stderr_port_class,
                     NIL, PORT_OUTPUT, NIL, NULL);
}

END_NAMESPACE
