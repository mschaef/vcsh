
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

#include "scan-private.h"

#include <stdlib.h>

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
#if 0
     lref_t oport = CURRENT_DEBUG_PORT();

     for(frame_t *frame = CURRENT_TIB()->frame;
         frame != NULL;
         frame = frame->prev_frame)
     {
          scwritef(_T("\n*** FRAME=~cd: "), oport, frame);

          switch (frame->type)
          {
          case FRAME_EVAL:
               scwritef(_T("eval > ~s in ~s\n"), oport, *frame->as.eval.form,
                        frame->as.eval.initial_form);
               break;

          case FRAME_ESCAPE:
               scwritef(_T("try > ~s\n"), oport, frame->as.escape.tag);
               break;

          case FRAME_UNWIND:
               scwritef(_T("unwind-protect >\n"), oport);
               break;

          case FRAME_SUBR:
               scwritef(_T("subr > ~s\n"), oport, frame->as.subr.subr);
               break;

          default:
               scwritef(_T("<< INVALID-FRAME-TYPE >>\n"), oport);
               break;
          }

          lflush_port(oport);
     }
#endif
}

lref_t lheap_cell_count_by_typecode()
{
     lref_t obj, org, end;
     enum typecode_t type;
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

lref_t lmemref(lref_t addr)
{
     size_t baseaddr = (size_t) get_c_long(addr);

     intptr_t *obj = (intptr_t *) baseaddr;

     return fixcons((fixnum_t)*obj);
}

lref_t lstress_lisp_heap(lref_t c)
{
     if (!FIXNUMP(c))
          vmerror_wrong_type_n(1, c);

     fixnum_t count = FIXNM(c);

     for (fixnum_t i = 0; i < count; i++)
          lcons(NIL, NIL);

     return NIL;
}


lref_t lstress_c_heap(lref_t c, lref_t s)
{
     if (!FIXNUMP(c))
          vmerror_wrong_type_n(1, c);

     if (!FIXNUMP(s))
          vmerror_wrong_type_n(2, s);

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
          vmerror_wrong_type_n(1, v);

     fixnum_t old_flags = interp.debug_flags;

     interp.debug_flags = (enum debug_flag_t) FIXNM(v);


     return fixcons(old_flags);
}

lref_t ldebug_flags()
{
     return fixcons(interp.debug_flags);
}


/* *INDENT-OFF* */
static struct
{
     const char *df_env_name;
     enum debug_flag_t df;
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

const _TCHAR *strchrnul(const _TCHAR * string, int c)
{
     for (; *string; string++)
          if (*string == c)
               break;

     return string;
}

enum debug_flag_t debug_flags_from_string(enum debug_flag_t initial,
                                          const _TCHAR * source_name,
                                          const _TCHAR * str)
{
     enum debug_flag_t rc = initial;

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
                         rc = (enum debug_flag_t) (rc & ~debug_flag_env_names[ii].df);
                    else
                         rc = (enum debug_flag_t) (rc | debug_flag_env_names[ii].df);

                    break;
               }
          }

          if (!found)
          {
               dscwritef(DF_ALWAYS, ("Unknown debug flag while parsing ~cs, starting here: ~cs\n", source_name, str));
               show_debug_flags();
               panic("Aborting Run");
          }

          str = (char *) ((*envtokend == '\0') ? NULL : envtokend + 1);
     }

     return rc;
}

enum debug_flag_t debug_flags_from_environment(enum debug_flag_t initial)
{
     return debug_flags_from_string(initial,
                                    _T("VCSH_DEBUG_FLAGS"),
                                    getenv("VCSH_DEBUG_FLAGS"));
}

bool is_debug_flag_set(enum debug_flag_t flag)
{
     return DEBUG_FLAG(flag);
}

lref_t ltime_apply0(lref_t fn)
{
     if (!PROCEDUREP(fn))
          vmerror_wrong_type_n(1, fn);

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

