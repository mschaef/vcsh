
/*
 * debug-tools.cpp --
 *
 * Some debugging tools built into the VM.
 *
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include "scan.h"

BEGIN_NAMESPACE(scan)

/* ldump_heap_state
 *
 * Traverse the heap segments, writing the FASL code of each cell to
 * a file of the specified name.
 */
lref_t ldump_heap_state(lref_t port)
{
     for (size_t heap_no = 0; heap_no < interp.gc_max_heap_segments; heap_no++)
     {
          if (interp.gc_heap_segments[heap_no] == NULL)
               continue;

          lref_t obj;
          lref_t org = interp.gc_heap_segments[heap_no];
          lref_t end = org + interp.gc_heap_segment_size;
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
     lref_t oport = CURRENT_DEBUG_PORT();

     for(frame_t *fsp = CURRENT_TIB()->fsp; fsp > &(CURRENT_TIB()->frame_stack[0]); fsp--)
     {
          scwritef(_T("\n*** FSP=~cd: "), oport, fsp);

          switch (fsp->type)
          {
          case FRAME_EVAL:
               scwritef(_T("eval > ~s in ~s\n"), oport, *fsp->as.eval.form,
                        fsp->as.eval.initial_form);
               break;

          case FRAME_EX_TRY:
               scwritef(_T("try > ~s\n"), oport, fsp->as.escape.tag);
               break;

          case FRAME_EX_UNWIND:
               scwritef(_T("unwind-protect >\n"), oport);
               break;

          case FRAME_PRIMITIVE:
               scwritef(_T("primitive > ~s\n"), oport, fsp->as.prim.function);
               break;

          default:
               scwritef(_T("<< INVALID-FRAME-TYPE >>\n"), oport);
               break;
          }

          lflush_port(oport);
     }
}

lref_t lheap_cell_count_by_typecode()
{
     lref_t obj, org, end;
     typecode_t type;
     lref_t result = NIL;

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

lref_t lmemref_byte(lref_t addr)
{
     size_t baseaddr = (size_t) get_c_long(addr);

     uint8_t *obj = (uint8_t *) baseaddr;

     return fixcons(*obj);
}


lref_t lstress_lisp_heap(lref_t c)
{
     if (!FIXNUMP(c))
          vmerror_wrong_type(1, c);

     fixnum_t count = FIXNM(c);

     for (fixnum_t i = 0; i < count; i++)
          lcons(NIL, NIL);

     return NIL;
}


lref_t lstress_c_heap(lref_t c, lref_t s)
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

lref_t lsysob(lref_t addr)          /* address->object */
{
     return (lref_t) get_c_long(addr);
};

lref_t lobaddr(lref_t object)       /* object->address */
{
     return fixcons((fixnum_t) object);
}

lref_t lset_debug_flags(lref_t v)
{
     if (!FIXNUMP(v))
          vmerror_wrong_type(1, v);

     fixnum_t old_flags = interp.debug_flags;

     interp.debug_flags = (debug_flag_t) FIXNM(v);


     return fixcons(old_flags);
}

lref_t ldebug_flags()
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
     uint8_t _current;
};

enum
{
     BLOCKIN_MAX_BLOCK_SIZE = 256
};

bool test_blocking_input_read(lref_t port, void *userdata)
{
     uint8_t buf[BLOCKIN_MAX_BLOCK_SIZE];

     test_blocking_input_info_t *info = (test_blocking_input_info_t *) userdata;

     fixnum_t bytes_to_provide = MIN2(info->_block_size, info->_length);

     info->_length -= bytes_to_provide;

     assert(info->_length >= 0);

     for (fixnum_t i = 0; i < bytes_to_provide; i++)
          buf[i] = info->_current++;

     blocking_input_post_data(port, buf, (size_t) bytes_to_provide);

     return info->_length > 0;
}

void test_blocking_input_close(lref_t port, void *userdata)
{
     UNREFERENCED(port);

     assert(userdata);

     safe_free(userdata);
}


lref_t ltest_blocking_input(lref_t block_size, lref_t length, lref_t binary)
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
#if defined(WITH_FOPLOG_SUPPORT)
     { "startup-foplog", DF_STARTUP_FOPLOG},
#endif
     { "no-startup", DF_NO_STARTUP},
     { "all", DF_ALL},
     { NULL, DF_ALL},
};
/* *INDENT-ON* */

static void show_debug_flags()
{
     dscwritef(DF_ALWAYS, ("Available debug flags:\n\n"));

     for (size_t ii = 0; debug_flag_env_names[ii].df_env_name; ii++)
          dscwritef(DF_ALWAYS, ("* ~cs\n", debug_flag_env_names[ii].df_env_name));
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
               dscwritef(DF_ALWAYS, ("Unknown debug flag while parsing ~cs, starting here: ~cs\n", source_name,
                                     str));
               show_debug_flags();
               panic("Aborting Run");
          }

          str = (char *) ((*envtokend == '\0') ? NULL : envtokend + 1);
     }

     return rc;
}

debug_flag_t debug_flags_from_environment(debug_flag_t initial)
{
     return debug_flags_from_string(initial,
                                    _T("VCSH_DEBUG_FLAGS"),
                                    getenv("VCSH_DEBUG_FLAGS"));
}


lref_t ltime_apply0(lref_t fn)
{
     if (!PROCEDUREP(fn))
          vmerror_wrong_type(1, fn);

     size_t cells = interp.gc_total_cells_allocated;
     size_t c_blocks = interp.gc_malloc_blocks;
     size_t c_bytes = interp.gc_malloc_bytes;
     flonum_t t = sys_runtime();
     flonum_t gc_t = interp.gc_total_run_time;

     lref_t argv[6];

     argv[0] = apply1(fn, 0, NULL);

     argv[1] = flocons(sys_runtime() - t);
     argv[2] = flocons(interp.gc_total_run_time - gc_t);
     argv[3] = fixcons(interp.gc_total_cells_allocated - cells);
     argv[4] = fixcons(interp.gc_malloc_blocks - c_blocks);
     argv[5] = fixcons(interp.gc_malloc_bytes - c_bytes);

     return lvector(6, argv);
}

END_NAMESPACE
