/* debug-tools.cpp
 *
 * Code for debugging tools built into the VM.
 */

#include "scan.h"

BEGIN_NAMESPACE(scan)

  /* ldump_heap_state
   *
   * Traverse the heap segments, writing the FASL code of each cell to
   * a file of the specified name.
   */

  LRef ldump_heap_state(LRef port)
  {
    for (size_t heap_no = 0; heap_no < interp.gc_max_heap_segments; heap_no++)
      {
        if (interp.gc_heap_segments[heap_no])
          {
            LRef obj;
            LRef org = interp.gc_heap_segments[heap_no];
            LRef end = org + interp.gc_heap_segment_size;
            fixnum_t ii;

            for (obj = org, ii = 0; obj < end; obj++, ii++)
              {
                if (ii % 256 == 0)
                  {
                    lnewline(port);
                    ii = 0;
                  }

                scwritef(_T("~cd, "), port, TYPE(obj));
              }
          }
      }

    return NIL;
  }


  /* lshow_type_stats
   *
   * Traverse the heap segments, counting each instance of each type.
   * Returns a list with a node for each type containing a count
   * of the number of instances, by type.
   */

  LRef lshow_type_stats ()
  {
    LRef obj, org, end;
    typecode_t type;
    LRef result = NIL;

    size_t internal_type_counts[LAST_INTERNAL_TYPEC + 1];

    for(size_t ii = 0; ii < LAST_INTERNAL_TYPEC + 1; ii++)
      internal_type_counts[ii] = 0;

    /* Traverse the heaps, counting each type */
    for (size_t heap_no = 0; heap_no < interp.gc_max_heap_segments; heap_no++)
      {
        if (interp.gc_heap_segments[heap_no])
          {
            org = interp.gc_heap_segments[heap_no];
            end = org + interp.gc_heap_segment_size;

            for (obj = org; obj < end; ++obj)
              {
                type = TYPE(obj);

                internal_type_counts[type]++;
              }
          }
      }

    /* Build the result list */
    result = NIL;

    for(int i = 0; i <= LAST_INTERNAL_TYPEC; i++)
      result = lcons(lcons(make_type_name((typecode_t)i), fixcons(internal_type_counts[i])), result);

    return result;
  }

  LRef lmemref_byte (LRef addr)
  {
    size_t baseaddr = (size_t)get_c_long(addr);

    u8 *obj = (u8 *)baseaddr;

    return fixcons(*obj);
  }


  LRef lstress_lisp_heap(LRef c)
  {
    if (!FIXNUMP(c))
      vmerror_wrong_type(1, c);

    fixnum_t count = FIXNM(c);

    for(fixnum_t i = 0; i < count; i++)
      lcons(NIL, NIL);

    return NIL;
  }


  LRef lstress_c_heap(LRef c, LRef s)
  {
    if (!FIXNUMP(c))
      vmerror_wrong_type(1, c);

    if (!FIXNUMP(s))
      vmerror_wrong_type(2, s);

    fixnum_t count = FIXNM(c);
    fixnum_t size  = FIXNM(s);

    for(fixnum_t i = 0; i < count; i++)
      vectorcons(size, NIL);

    return NIL;
  }

     LRef lsysob(LRef addr) /* address->object */
  {
    return (LRef)get_c_long(addr);
  };

     LRef lobaddr(LRef object) /* object->address */
  {
    return fixcons((fixnum_t)object);
  }

  LRef lset_debug_flags(LRef v)
  {
    if (!FIXNUMP(v))
      vmerror_wrong_type(1, v);

    fixnum_t old_flags = interp.debug_flags;

    interp.debug_flags = (debug_flag_t)FIXNM(v);


    return fixcons(old_flags);
  }

  LRef ldebug_flags()
  {
    return fixcons(interp.debug_flags);
  }

  /* Blocking input test port. This provides a fixed-length string of bytes
   * from 0 to 255 repeating back to zero. The size of the blocks reported
   * back to the port can be varied to allow tests to be written to exercise
   * all of the code paths. (port_block > request_block, request_block > port_block,
   * port_block == request_block). */

  struct test_blocking_input_info_t {
    fixnum_t _block_size;
    fixnum_t _length;
    u8 _current;
  };

  enum { BLOCKIN_MAX_BLOCK_SIZE = 256 };

  bool test_blocking_input_read(LRef port, void *userdata)
  {
    u8 buf[BLOCKIN_MAX_BLOCK_SIZE];

    test_blocking_input_info_t *info = (test_blocking_input_info_t *)userdata;

    fixnum_t bytes_to_provide = MIN2(info->_block_size, info->_length);

    info->_length -= bytes_to_provide;

    assert(info->_length >= 0);

    for(fixnum_t i = 0; i < bytes_to_provide; i++)
      buf[i] = info->_current++;

    blocking_input_post_data(port, buf, (size_t)bytes_to_provide);

    return info->_length > 0;
  }

  void test_blocking_input_close(LRef port, void *userdata)
  {
    UNREFERENCED(port);

    assert(userdata);

    safe_free(userdata);
  }


  LRef ltest_blocking_input(LRef block_size, LRef length, LRef binary)
  {
    if (!FIXNUMP(block_size))
      vmerror_wrong_type(1, block_size);

    if (!FIXNUMP(length))
      vmerror_wrong_type(1, length);

    if (FIXNM(block_size) > BLOCKIN_MAX_BLOCK_SIZE)
      vmerror("Block size out of range", block_size);

    if (FIXNM(length) < 0)
      vmerror("Length out of range", length);

    if (NULLP(binary)) binary = boolcons(false);

    test_blocking_input_info_t *info = (test_blocking_input_info_t *)safe_malloc(sizeof(test_blocking_input_info_t));

    info->_block_size   = FIXNM(block_size);
    info->_length       = FIXNM(length);
    info->_current      = 0;

    return blocking_input_cons(_T("BLOCKING-TEST-PORT"), BOOLV(binary),
                               test_blocking_input_read, test_blocking_input_close, info);
  }

  /* GC Trip wire support.
   *
   * GC trip wires pretty much do what they sound like, they blow up when the garbage
   * collector touches (attempts to free) them. They are used in tests to verify that
   * the GC is picking up all object references.
   */
  LRef ligc_trip_wire()
  {
    return new_cell(TC_GC_TRIP_WIRE);
  }

  LRef liarm_gc_trip_wires(LRef f)
  {
    bool new_state = TRUEP(f);

    interp.gc_trip_wires_armed = new_state;

    return boolcons(new_state);
  }

  static struct {
    const char  *df_env_name;
    debug_flag_t df;
  } debug_flag_env_names[] = {
    { "show-load-forms"      , DF_SHOW_LOAD_FORMS       },
    { "show-global-defines"  , DF_SHOW_GLOBAL_DEFINES   },
    { "show-local-defines"   , DF_SHOW_LOCAL_DEFINES    },
    { "show-throws"          , DF_SHOW_THROWS           },
    { "show-vmsignals"       , DF_SHOW_VMSIGNALS        },
    { "show-vmerrors"        , DF_SHOW_VMERRORS         },
    { "show-gc"              , DF_SHOW_GC               },
    { "show-gc-details"      , DF_SHOW_GC_DETAILS       },
    { "print-for-diff"       , DF_PRINT_FOR_DIFF        },
    { "print-closure-code"   , DF_PRINT_CLOSURE_CODE    },
    { "print-symbol-packages", DF_PRINT_SYMBOL_PACKAGES },
    { "print-addresses"      , DF_PRINT_ADDRESSES       },
    { "debugger-to-ods"      , DF_DEBUGGER_TO_ODS       },
    { "fasl-show-opcodes"    , DF_FASL_SHOW_OPCODES     },
    { "show-fast-load-forms" , DF_SHOW_FAST_LOAD_FORMS  },
    { "show-fast-load-units" , DF_SHOW_FAST_LOAD_UNITS  },
    { "temp"                 , DF_TEMP                  },
    { "no-startup"           , DF_NO_STARTUP            },
    { "all"                  , DF_ALL                   },
    { NULL                   , DF_ALL                   },
  };

  static void show_debug_flags() {
    dscwritef("Available debug flags:\n\n");

    for(size_t ii = 0; debug_flag_env_names[ii].df_env_name; ii++)
      dscwritef("* ~cs\n", debug_flag_env_names[ii].df_env_name);
  }

#if defined(__CYGWIN__)
  static char *strchrnul(char *string, int c)
  {
    for(; *string; string++)
      if (*string == c)
        break;

    return string;
  }
#endif

  debug_flag_t debug_flags_from_string(debug_flag_t initial, const _TCHAR *source_name, const _TCHAR *str)
  {
    debug_flag_t rc = initial;

    while(str) {
      if (*str == _T('\0'))
        break;

      const char *envtokend = strchrnul(str , ',');

      bool found = false;
      bool remove = false;

      if ((str[0] == '-') || (str[0] == '+'))
        {
          remove = (str[0] == '-');
          str++;
        }

      for(size_t ii = 0; debug_flag_env_names[ii].df_env_name; ii++)
        {
          if (strncasecmp(str, debug_flag_env_names[ii].df_env_name,
                          envtokend - str) == 0)
            {
              found = true;

              if (remove)
                rc = (debug_flag_t)(rc & ~debug_flag_env_names[ii].df);
              else
                rc = (debug_flag_t)(rc | debug_flag_env_names[ii].df);

              break;
            }
      }

      if (!found)
        {
          dscwritef("Unknown debug flag while parsing ~cs, starting here: ~cs\n", source_name, str);
          show_debug_flags();
          panic("Aborting Run");
        }

      str = (char *)((*envtokend == '\0') ? NULL :  envtokend + 1);
    }

    return rc;
  }

  debug_flag_t debug_flags_from_environment(debug_flag_t initial)
  {
    return debug_flags_from_string(initial, _T("VCSH_DEBUG_FLAGS"), getenv("VCSH_DEBUG_FLAGS"));
  }

  /*
   * The debug printer. This is the printer used to print Lisp objects from C code, and it's
   * supposed to work, even if the lisp interpreter is in an odd state and cannot be trusted.
   */


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
          _sntprintf (buf, STACK_STRBUF_LEN,
                      _T("%.*e"), digits, FLONM (object));
        else
          {
            /* Prevent numbers on the left of the decimal point from
             * adding to the number of digits we print. */
            if ((scale > 0) && (scale <= digits))
              digits -= (int)scale;

            _sntprintf(buf, STACK_STRBUF_LEN, _T("%.*f"), digits, FLONM (object));
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
	while(next_char_to_write < STRING_DIM(obj))
	  {
	    unsigned int c;
	    size_t next_special_char;

	    /* Scan for the next special character, it ends the block... */
	    for(next_special_char = next_char_to_write;
            next_special_char < STRING_DIM(obj);
            next_special_char++)
	      {
            c = STRING_DATA(obj)[next_special_char];

            if (   (c == '\\') || (c == '"')  || (c == '\n') || (c == '\r')
                   || (c == '\t') || (c == '\0') || (c < 32)    || (c >= 127))
              break;
	      }

	    /* ...which then gets written out. */
	    if (next_special_char - next_char_to_write > 0)
	      write_text(&(STRING_DATA(obj)[next_char_to_write]),
                     next_special_char - next_char_to_write,
                     port);

	    if (next_special_char >= STRING_DIM(obj))
	      break;

	    c = STRING_DATA(obj)[next_special_char];

	    /* Write the next special character. */
	    switch(c)
	      {
	      case '\\':
	      case '"':
            cbuff[0] = _T('\\');
            cbuff[1] = (_TCHAR)c;

            write_text(cbuff, 2, port);
            break;

	      case '\n': WRITE_TEXT_CONSTANT(_T("\\n"),   port); break;
	      case '\r': WRITE_TEXT_CONSTANT(_T("\\r"),   port); break;
	      case '\t': WRITE_TEXT_CONSTANT(_T("\\t"),   port); break;
	      case '\0': WRITE_TEXT_CONSTANT(_T("\\000"), port); break;
	      default:
            /* This assert will only fail when the special character scanner
             * breaks on a character that the special character writer
             * does not know how to handle. */
            assert ((c < 32) || (c >= 127));
            scwritef(_T("\\~cC"), port, (unsigned long)c);
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
    while(hash_iter_next(obj, &ii, &key, &val))
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

    for(size_t ii = 0; ii < INSTANCE_DIM(obj); ii++)
      {
        WRITE_TEXT_CONSTANT(_T(" "), port);
        debug_print_object(INSTANCE_ELEM(obj, ii), port, machine_readable);
      }

    write_char('>', port);
  }

  const _TCHAR *charnames[] = {
    _T("nul"),      _T("soh"),      _T("stx"),      _T("etx"),
    _T("eot"),      _T("eng"),      _T("ack"),      _T("bel"),
    _T("bs"),       _T("tab"),      _T("newline"),  _T("vtab"),
    _T("formfeed"), _T("cr"),       _T("so"),       _T("si"),
    _T("dle"),      _T("dc1"),      _T("dc2"),      _T("dc3"),
    _T("dc4"),      _T("nak"),      _T("syn"),      _T("etb"),
    _T("can"),      _T("em"),       _T("sub"),      _T("esc"),
    _T("fs"),       _T("gs"),       _T("rs"),       _T("us"),
    _T("space"),    NULL
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

    switch (TYPE(obj)) {
    case TC_NIL:
      WRITE_TEXT_CONSTANT(_T("()"), port);
      break;

    case TC_BOOLEAN:
      if (BOOLV(obj)) {
        WRITE_TEXT_CONSTANT(_T("#t"), port);
      } else {
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

      if (!NULLP (tmp))
        {
          WRITE_TEXT_CONSTANT(_T(" . "), port);
          debug_print_object (tmp, port, machine_readable);
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
            scwritef(_T("#\\~cs"), port, charnames[(size_t)CHARV(obj)]);
          else if (CHARV(obj) >= CHAREXTENDED - 1)
            scwritef(_T("#\\<~cd>"), port, (int)CHARV(obj));
          else
            scwritef(_T("#\\~cc"), port, (int)CHARV(obj));
        }
      else
        scwritef(_T("~cc"), port, (int)CHARV(obj));
      break;

    case TC_SYMBOL:
      if (NULLP(SYMBOL_HOME(obj))) {
        if (DEBUG_FLAG(DF_PRINT_FOR_DIFF))
          scwritef("#:<uninterned-symbol>", port);
        else
          scwritef("#:~a@~c&", port, SYMBOL_PNAME(obj), obj);
      } else if (SYMBOL_HOME(obj) == interp.keyword_package)
        scwritef(":~a", port, SYMBOL_PNAME(obj));
      else
        {
             /*  With only a minimal c-level package implementation, we just assume
              *  every symbol is private. */
          scwritef("~a::~a", port,
                   PACKAGE_NAME(SYMBOL_HOME(obj)),
                   SYMBOL_PNAME(obj));
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

      for(ii = 0, slots = CAR(CDR(STRUCTURE_LAYOUT(obj)));
          ii < STRUCTURE_DIM(obj);
          ii++, slots = CDR(slots))
        {
          WRITE_TEXT_CONSTANT(_T(" "), port);
          debug_print_object(CAR(CAR(slots)), port, true);
          WRITE_TEXT_CONSTANT(_T(" "), port);
          debug_print_object(STRUCTURE_ELEM(obj, ii), port, true);
        }

      WRITE_TEXT_CONSTANT(_T(")"), port);
      break;

    case TC_STRING:    debug_print_string(obj, port, machine_readable);    break;
    case TC_HASH:      debug_print_hash(obj, port, machine_readable);      break;
    case TC_INSTANCE:  debug_print_instance(obj, port, machine_readable);  break;
    case TC_PACKAGE:   scwritef("~u ~a", port, (LRef)obj, PACKAGE_NAME(obj));  break;

    case TC_SUBR:
      scwritef("~u,~cs:~a", port, (LRef)obj, subr_kind_str(SUBR_TYPE(obj)), lsubr_name(obj));
      break;

    case TC_CLOSURE:
      if (DEBUG_FLAG(DF_PRINT_CLOSURE_CODE))
        scwritef("~u\n\tcode:~s\n\tenv:~s\n\tp-list:~s", port,
                 (LRef)obj,
                 CLOSURE_CODE(obj),
                 CLOSURE_ENV(obj),
                 CLOSURE_PROPERTY_LIST(obj));

      else
        scwritef("~u", port, (LRef)obj);
      break;

    case TC_BYTE_VECTOR:
      scwritef("~u", port, (LRef)obj);
      break;

    case TC_VALUES_TUPLE:
      scwritef("~u ~s", port, (LRef)obj, VALUES_TUPLE_VALUES(obj));
      break;

    case TC_MACRO:
      if (DEBUG_FLAG(DF_PRINT_CLOSURE_CODE))
        scwritef("~u ~s", port, (LRef)obj, MACRO_TRANSFORMER(obj));
      else
        scwritef("~u", port, (LRef)obj);
      break;

    case TC_END_OF_FILE:
      scwritef("~u", port, (LRef)obj);
      break;

    case TC_EXTERNAL:
      scwritef("~u, points-to=~c& is-a=~a ",
               port,
               (LRef)obj,
               EXTERNAL_DATA(obj),
               EXTERNAL_DESC(obj));
      break;

    case TC_PORT:
      scwritef(_T("~u~s~cs ~cs ~s"), port,
               obj,
               lport_mode(obj),
               (PORT_MODE(obj) & PORT_BINARY) ? "(binary)" : "",
               PORT_CLASS(obj)->_name,

               PORT_PINFO(obj)->_port_name
               );
      break;

    case TC_FAST_OP:
      scwritef("#<FOP@~c&:~cd ~s ~s>", port, (LRef)obj,
               FAST_OP_OPCODE(obj),
               FAST_OP_ARG1(obj),
               FAST_OP_ARG2(obj),
               FAST_OP_ARG3(obj));
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
END_NAMESPACE


