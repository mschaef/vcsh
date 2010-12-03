
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
          if (interp.gc_heap_segments[heap_no] == NULL)
               continue;

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

     return NIL;
}


void scan_postmortem_dump()
{
     LRef oport = CURRENT_DEBUG_PORT();

     frame_record_t *loc = CURRENT_TIB()->frame_stack;

     while (loc)
     {
          switch (loc->type)
          {
          case FRAME_EVAL:
               scwritef(_T("eval > ~s in ~s\n"), oport, *loc->frame_as.eval.form,
                        loc->frame_as.eval.initial_form);
               break;

          case FRAME_EX_TRY:
               scwritef(_T("try > ~s\n"), oport, loc->frame_as.dynamic_escape.tag);
               break;

          case FRAME_EX_UNWIND:
               scwritef(_T("unwind-protect >\n"), oport);
               break;

          case FRAME_PRIMITIVE:
               scwritef(_T("primitive > ~s\n"), oport, loc->frame_as.primitive.function);
               break;

          case FRAME_MARKER:
               scwritef(_T("marker > ~s\n"), oport, loc->frame_as.marker.tag);
               break;

          default:
               scwritef(_T("<< INVALID-FRAME-TYPE >>\n"), oport);
               break;
          }

          lflush_port(oport);

          loc = loc->previous;
     }
}

LRef lheap_cell_count_by_typecode()
{
     LRef obj, org, end;
     typecode_t type;
     LRef result = NIL;

     size_t internal_type_counts[LAST_INTERNAL_TYPEC + 1];

     for (size_t ii = 0; ii < LAST_INTERNAL_TYPEC + 1; ii++)
          internal_type_counts[ii] = 0;

     /* Traverse the heaps, counting each type */
     for (size_t heap_no = 0; heap_no < interp.gc_max_heap_segments; heap_no++)
     {
          if (interp.gc_heap_segments[heap_no] == NULL)
               continue;

          org = interp.gc_heap_segments[heap_no];
          end = org + interp.gc_heap_segment_size;

          for (obj = org; obj < end; ++obj)
          {
               type = TYPE(obj);

               internal_type_counts[type]++;
          }
     }

     /* Build the result list */
     result = vectorcons(LAST_INTERNAL_TYPEC + 1, NIL);

     for (int i = 0; i <= LAST_INTERNAL_TYPEC; i++)
          SET_VECTOR_ELEM(result, i, fixcons(internal_type_counts[i]));

     return result;
}

LRef lmemref_byte(LRef addr)
{
     size_t baseaddr = (size_t) get_c_long(addr);

     u8_t *obj = (u8_t *) baseaddr;

     return fixcons(*obj);
}


LRef lstress_lisp_heap(LRef c)
{
     if (!FIXNUMP(c))
          vmerror_wrong_type(1, c);

     fixnum_t count = FIXNM(c);

     for (fixnum_t i = 0; i < count; i++)
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
     fixnum_t size = FIXNM(s);

     for (fixnum_t i = 0; i < count; i++)
          vectorcons(size, NIL);

     return NIL;
}

LRef lsysob(LRef addr)          /* address->object */
{
     return (LRef) get_c_long(addr);
};

LRef lobaddr(LRef object)       /* object->address */
{
     return fixcons((fixnum_t) object);
}

LRef lset_debug_flags(LRef v)
{
     if (!FIXNUMP(v))
          vmerror_wrong_type(1, v);

     fixnum_t old_flags = interp.debug_flags;

     interp.debug_flags = (debug_flag_t) FIXNM(v);


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

struct test_blocking_input_info_t
{
     fixnum_t _block_size;
     fixnum_t _length;
     u8_t _current;
};

enum
{
     BLOCKIN_MAX_BLOCK_SIZE = 256
};

bool test_blocking_input_read(LRef port, void *userdata)
{
     u8_t buf[BLOCKIN_MAX_BLOCK_SIZE];

     test_blocking_input_info_t *info = (test_blocking_input_info_t *) userdata;

     fixnum_t bytes_to_provide = MIN2(info->_block_size, info->_length);

     info->_length -= bytes_to_provide;

     assert(info->_length >= 0);

     for (fixnum_t i = 0; i < bytes_to_provide; i++)
          buf[i] = info->_current++;

     blocking_input_post_data(port, buf, (size_t) bytes_to_provide);

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
          vmerror_arg_out_of_range(block_size);

     if (FIXNM(length) < 0)
          vmerror_arg_out_of_range(length, _T(">0"));

     if (NULLP(binary))
          binary = boolcons(false);

     test_blocking_input_info_t *info =
         (test_blocking_input_info_t *) safe_malloc(sizeof(test_blocking_input_info_t));

     info->_block_size = FIXNM(block_size);
     info->_length = FIXNM(length);
     info->_current = 0;

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

/* *INDENT-OFF* */
static struct
{
     const char *df_env_name;
     debug_flag_t df;
} debug_flag_env_names[] = {
     { "show-load-forms", DF_SHOW_LOAD_FORMS},
     { "show-global-defines", DF_SHOW_GLOBAL_DEFINES},
     { "show-local-defines", DF_SHOW_LOCAL_DEFINES},
     { "show-throws", DF_SHOW_THROWS},
     { "show-traps", DF_SHOW_TRAPS},
     { "show-vmerrors", DF_SHOW_VMERRORS},
     { "show-gc", DF_SHOW_GC},
     { "show-gc-details", DF_SHOW_GC_DETAILS},
     { "print-for-diff", DF_PRINT_FOR_DIFF},
     { "print-closure-code", DF_PRINT_CLOSURE_CODE},
     { "print-symbol-packages", DF_PRINT_SYMBOL_PACKAGES},
     { "print-addresses", DF_PRINT_ADDRESSES},
     { "debugger-to-ods", DF_DEBUGGER_TO_ODS},
     { "fasl-show-opcodes", DF_FASL_SHOW_OPCODES},
     { "show-fast-load-forms", DF_SHOW_FAST_LOAD_FORMS},
     { "show-fast-load-units", DF_SHOW_FAST_LOAD_UNITS},
     { "temp", DF_TEMP},
     { "no-startup", DF_NO_STARTUP},
     { "all", DF_ALL},
     { NULL, DF_ALL},
};
/* *INDENT-ON* */

static void show_debug_flags()
{
     dscwritef("Available debug flags:\n\n");

     for (size_t ii = 0; debug_flag_env_names[ii].df_env_name; ii++)
          dscwritef("* ~cs\n", debug_flag_env_names[ii].df_env_name);
}

extern "C" const _TCHAR *strchrnul(const _TCHAR * string, int c)
{
     for (; *string; string++)
          if (*string == c)
               break;

     return string;
}

debug_flag_t debug_flags_from_string(debug_flag_t initial, const _TCHAR * source_name,
                                     const _TCHAR * str)
{
     debug_flag_t rc = initial;

     while (str)
     {
          if (*str == _T('\0'))
               break;

          const char *envtokend = strchrnul(str, ',');

          bool found = false;
          bool remove = false;

          if ((str[0] == '-') || (str[0] == '+'))
          {
               remove = (str[0] == '-');
               str++;
          }

          for (size_t ii = 0; debug_flag_env_names[ii].df_env_name; ii++)
          {
               if (strncasecmp(str, debug_flag_env_names[ii].df_env_name, envtokend - str) == 0)
               {
                    found = true;

                    if (remove)
                         rc = (debug_flag_t) (rc & ~debug_flag_env_names[ii].df);
                    else
                         rc = (debug_flag_t) (rc | debug_flag_env_names[ii].df);

                    break;
               }
          }

          if (!found)
          {
               dscwritef("Unknown debug flag while parsing ~cs, starting here: ~cs\n", source_name,
                         str);
               show_debug_flags();
               panic("Aborting Run");
          }

          str = (char *) ((*envtokend == '\0') ? NULL : envtokend + 1);
     }

     return rc;
}

debug_flag_t debug_flags_from_environment(debug_flag_t initial)
{
     return debug_flags_from_string(initial, _T("VCSH_DEBUG_FLAGS"), getenv("VCSH_DEBUG_FLAGS"));
}


LRef ltime_apply0(LRef fn)
{
     if (!PROCEDUREP(fn))
          vmerror_wrong_type(1, fn);

     fixnum_t cells = interp.gc_total_cells_allocated;
     fixnum_t c_blocks = malloc_blocks;
     fixnum_t c_bytes = malloc_bytes;
     flonum_t t = sys_runtime();
     flonum_t gc_t = interp.gc_total_run_time;

     LRef argv[6];

     argv[0] = apply1(fn, 0, NULL);

     argv[1] = flocons(sys_runtime() - t);
     argv[2] = flocons(interp.gc_total_run_time - gc_t);
     argv[3] = fixcons(interp.gc_total_cells_allocated - cells);
     argv[4] = fixcons(malloc_blocks - c_blocks);
     argv[5] = fixcons(malloc_bytes - c_bytes);

     return lvector(6, argv);
}

END_NAMESPACE
