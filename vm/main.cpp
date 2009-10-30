/* slib.c
 *
 * The core scan interpreter
 */

#include <float.h>

#include "scan.h"

namespace scan {

  /**************************************************************
   * Interpreter globals
   */

  interpreter_t interp;
  SCAN_THREAD_LOCAL interpreter_thread_t thread;

  LRef liimmediate_p(LRef obj)
  {
    return boolcons(LREF_IMMEDIATE_P(obj) || NULLP(obj));
  }

  /**************************************************************
   * Boolean
   */
  LRef boolcons(bool val)
  {
    return LREF2_CONS(LREF2_BOOL, val ? 1 : 0);
  }

  LRef lbooleanp(LRef x)
  {
    return boolcons(BOOLP(x));
  }

  LRef lnotp(LRef x)
  {
    return boolcons(!TRUEP(x));
  }

  /**************************************************************
   * C-Pointers
   */

  LRef externalcons(void *data, LRef desc, external_meta_t *meta /* = NULL*/)
  {
    LRef z = new_cell(TC_EXTERNAL);

    assert((meta == NULL) || (meta->_name != NULL));

    SET_EXTERNAL_DATA(z, data);
    SET_EXTERNAL_DESC(z, desc);
    SET_EXTERNAL_META(z, meta);

    return z;
  }

  LRef lexternal_data(LRef x)
  {
    if (!EXTERNALP(x))
      vmerror_wrong_type(1, x);

    return fixcons((uptr)EXTERNAL_DATA(x));
  }

  LRef lexternal_desc(LRef x)
  {
    if (!EXTERNALP(x))
      vmerror_wrong_type(1, x);

    return EXTERNAL_DESC(x);
  }

  LRef lexternalp(LRef x)
  {
    if (EXTERNALP(x))
      return x;
    else
      return boolcons(false);
  }

  LRef lexternal_type_name(LRef obj)
  {
    if (!EXTERNALP(obj))
      return boolcons(false);

    if (EXTERNAL_META(obj))
      return strcons(_T(EXTERNAL_META(obj)->_name));

    return boolcons(true);
  }

  LRef lprint_external_details(LRef obj, LRef port)
  {
    if (!EXTERNALP(obj))
      vmerror_wrong_type(1, obj);

    if (EXTERNAL_META(obj) && (EXTERNAL_META(obj)->_print_details))
      return EXTERNAL_META(obj)->_print_details(obj, port);

    return boolcons(false);
  }

  /**************************************************************
   * Equality tests
   */

  LRef leq(LRef x, LRef y)
  {
    return boolcons(EQ(x, y));
  }

  LRef leql(LRef x, LRef y)
  {
    bool rc = false;

    if (EQ(x, y))
      rc = true;
    else if (!NUMBERP(x) || !NUMBERP(y))
      rc = false;
    else if (FLONUMP(x) && FLONUMP(y))
      rc = (FLONM (x) == FLONM (y));
    else if (FIXNUMP(x) && FIXNUMP(y))
      rc = (FIXNM (x) == FIXNM (y));

    return boolcons(rc);
  }

  bool equalp(LRef a, LRef b)
  {
    typecode_t atype;

    STACK_CHECK (&a);

    if (EQ (a, b))
      return true;

    atype = TYPE(a);

    if (atype != TYPE(b))
      return false;

    switch (atype) {
    case TC_CONS:
      for(;;)
        {
          if (equalp(lcar(a), lcar(b)))
            {
              a = lcdr(a);
              b = lcdr(b);

              if (!CONSP(a) || !CONSP(b))
                return equalp(a, b);
            }
          else
            return false;
        }
      break;

    case TC_FIXNUM:
      return (FIXNM(a) == FIXNM(b));

    case  TC_FLONUM:
      // equal? considers NaN to be equal to itself. This is different
      // from =, which uses the more mathematical approach that NaN
      // is equal to nothing.
      if (isnan(FLONM(a)) && isnan(FLONM(b)))
        return equalp(FLOIM(a), FLOIM(b));
      else
        return (FLONM(a) == FLONM(b)) && equalp(FLOIM(a), FLOIM(b));

    case TC_SYMBOL:     return a == b;
    case TC_VECTOR:     return vector_equal(a, b);
    case TC_STRUCTURE:  return structure_equal(a, b);
    case TC_STRING:     return string_equal(a, b);
    case TC_HASH:       return hash_equal(a, b);
    case TC_INSTANCE:   return instance_equal(a, b);
    case TC_FAST_OP:    return fast_op_equal(a, b);
    }

    return false;
  }

  LRef lequal(LRef a, LRef b)
  {
    return boolcons(equalp(a, b));
  }

  LRef lnullp(LRef x)
  {
    return boolcons(NULLP(x));
  }

  LRef lrepresentation_of(LRef obj)
  {
    if (COMPLEXP(obj))
      return simple_intern(_T("complex"), interp.scheme_package);

    if (INSTANCEP(obj))
      return simple_intern(_T("instance"), interp.scheme_package);

    return make_type_name(TYPE(obj));
  }

  /**** Default panic handler
   */

  static panic_handler_t previous_panic_handler = NULL;

  static bool in_panic_handler = false;

  static void scan_panic_handler()
  {
    in_panic_handler = true;

    if (DEBUGGING_BUILD && !in_panic_handler)
      get_current_frames(0, VM_DEBUG_PORT);

    if (previous_panic_handler)
      previous_panic_handler();
  }

  /* Command line argument handling. Arguments are divided into two classes:
   *
   * 1) VM arguments are handled before the VM gets set up and are not passed
   *    into the interpreted program. These kinds of arguments are used to set
   *    boot-time paramaters  that _can't_ be set once the interpreter is run.
   *    These are always of the form -X<name>[=<value>]
   *
   * 2) Everything else gets stuck in a list bound to system::*args0*, and is
   *    handled by interpreted code.
   */
  static bool is_vm_argument(_TCHAR *arg)
  {
    return (arg != NULL)
      && (arg[0] == _T('-'))
      && (arg[1] == _T('X'));
  }

  static size_t process_vm_int_argument_value(_TCHAR *arg_name, _TCHAR *arg_value)
  {
    size_t rc = 0;

    _TCHAR *endobj = NULL;

    rc = strtol(arg_value, &endobj, 10);

    switch(*endobj)
      {
      case 'k': case 'K': endobj++; rc *= 1024; break;
      case 'm': case 'M': endobj++; rc *= (1024 * 1024); break;
      case 'g': case 'G': endobj++; rc *= (1024 * 1024 * 1024); break;
      }

    if (*endobj != _T('\0'))
      {
        dscwritef("Invalid numeric value (\"~cs\") for VM argument \"~cs\".", arg_value, arg_name);
        panic("Aborting Run");
      }

    return rc;
  }

  static void process_vm_arg_debug_flags(_TCHAR *arg_name, _TCHAR *arg_value)
  {
    UNREFERENCED(arg_name);

    interp.debug_flags = debug_flags_from_string(interp.debug_flags, "command line argument", arg_value);
  }

  static void process_vm_arg_heap_segment_size(_TCHAR *arg_name, _TCHAR *arg_value)
  {
    interp.gc_heap_segment_size = process_vm_int_argument_value(arg_name, arg_value) / sizeof(LObject);
  }

  static void process_vm_arg_max_heap_segments(_TCHAR *arg_name, _TCHAR *arg_value)
  {
    interp.gc_max_heap_segments = process_vm_int_argument_value(arg_name, arg_value);
  }

  static struct {
    const _TCHAR *vm_arg_name;
    void (* vm_arg_handler)(_TCHAR *arg_name, _TCHAR *arg_value);
  } vm_arg_names[] = {
    { "debug-flags",       process_vm_arg_debug_flags },
    { "heap-segment-size", process_vm_arg_heap_segment_size },
    { "max-heap-segments", process_vm_arg_max_heap_segments },
    { NULL, NULL }
  };

  static void show_vm_args() {
    dscwritef("Available VM arguments:\n\n");

    for(size_t ii = 0; vm_arg_names[ii].vm_arg_name; ii++)
      dscwritef("* ~cs\n", vm_arg_names[ii].vm_arg_name);
  }


  static void process_vm_argument(_TCHAR *arg_name, _TCHAR *arg_value)
  {
    for(size_t ii = 0; vm_arg_names[ii].vm_arg_name; ii++)
      {
        if (_tcscmp(arg_name, vm_arg_names[ii].vm_arg_name) == 0)
          {
            vm_arg_names[ii].vm_arg_handler(arg_name, arg_value);
            return;
          }
      }

    dscwritef("Unknown VM argument ~cs\n", arg_name);
    show_vm_args();
    panic("Aborting Run");
  }

  static void process_vm_arguments(int argc, _TCHAR *argv[])
  {
    for(int ii = 0; ii < argc; ii++) {

      if (!is_vm_argument(argv[ii]))
        continue;

      _TCHAR *arg_text = argv[ii] + 2;

      _TCHAR arg_name_buf[STACK_STRBUF_LEN];
      memset(arg_name_buf, 0, STACK_STRBUF_LEN);

      _TCHAR *arg_value_loc = strchrnul(arg_text, '=');

      _tcsncpy(arg_name_buf, arg_text,
               MIN2(arg_value_loc - arg_text, STACK_STRBUF_LEN - 1));

      if (*arg_value_loc == _T('='))
        arg_value_loc++;

      process_vm_argument(arg_name_buf, arg_value_loc);
    }
  }

  static void accept_command_line_arguments(int argc, _TCHAR *argv[])
  {
    LRef arg_list = NIL;
    LRef arg_list_bud = NIL;

    for(int ii = 0; ii < argc; ii++) {

      if (is_vm_argument(argv[ii]))
        continue;

      LRef new_cell = lcons(strcons(argv[ii]), NIL);

      if (NULLP(arg_list_bud)) {
        arg_list = arg_list_bud = new_cell;
      } else {
        SET_CDR(arg_list_bud, new_cell);
        arg_list_bud = new_cell;
      }
    }

    lidefine_global(interp.sym_args0, arg_list, NIL);
  }

  const _TCHAR *system_type_names[LAST_INTERNAL_TYPEC + 1] = {
    _T("free-cell"),
    _T("nil"),
    _T("boolean"),
    _T("cons"),

    _T("fixnum"),
    _T("flonum"),
    _T("character"),
    _T("symbol"),

    _T("package"),
    _T("subr"),
    _T("closure"),
    _T("compiled-closure"),

    _T("macro"),
    _T("byte-vector"),
    _T("string"),
    _T("vector"),

    _T("structure"),
    _T("hash"),
    _T("port"),
    _T("end-of-file"),

    _T("external"),
    _T("values-tuple"),
    _T("instance"),
    _T("unbound-marker"),

    _T("gc-trip-wire"),
    _T("fast-op"),
  };

  /**** Interpreter Initialization and Shutdown
   */
  static void init_base_scheme_objects(void)
  {
    gc_protect(_T("type-name-symbols"),
               interp.syms_internal_type_names,
               LAST_INTERNAL_TYPEC);

    for(size_t ii = 0; ii < LAST_INTERNAL_TYPEC + 1; ii++)
      interp.syms_internal_type_names[ii] =
        simple_intern(system_type_names[ii], interp.system_package);

    // !! These package paramaters should be explicit
    LRef nil_sym = simple_intern(_T("nil"), interp.system_package);
    lidefine_global(nil_sym, NIL, NIL);

    gc_protect_sym(&interp.sym_after_gc, _T("*after-gc*"), interp.system_package);
    lidefine_global(interp.sym_after_gc, NIL, NIL);

    gc_protect_sym(&interp.sym_msglvl_info, _T("*info*"), interp.system_package);
    // Info messages are too slow when we're GC'ing with every new_cell
    lidefine_global(interp.sym_msglvl_info, boolcons(!ALWAYS_GC), NIL);

    gc_protect_sym(&interp.sym_msglvl_errors, _T("*error*"), interp.system_package);
    lidefine_global(interp.sym_msglvl_errors, boolcons(true), NIL);

    gc_protect_sym(&interp.sym_args0, _T("*args0*"), interp.system_package);
    gc_protect_sym(&interp.sym_args, _T("*args*"), interp.system_package);

    gc_protect_sym(&interp.sym_port_current_in, _T("*current-input-port*"), interp.system_package);
    lidefine_global(interp.sym_port_current_in, NIL, NIL);

    gc_protect_sym(&interp.sym_port_current_out, _T("*current-output-port*"), interp.system_package);
    lidefine_global(interp.sym_port_current_out, NIL, NIL);

    gc_protect_sym(&interp.sym_port_current_err, _T("*current-error-port*"), interp.system_package);
    lidefine_global(interp.sym_port_current_err, NIL, NIL);

    gc_protect_sym(&interp.sym_port_debug, _T("*current-debug-port*"), interp.system_package);
    lidefine_global(interp.sym_port_debug, NIL, NIL);

    gc_protect_sym(&interp.sym_declare, _T("declare"), interp.system_package);
    lidefine_global(interp.sym_declare, NIL, NIL);

    gc_protect_sym(&interp.sym_name, _T("name"), interp.system_package);
    lidefine_global(interp.sym_name, NIL, NIL);

    gc_protect_sym(&interp.sym_documentation, _T("documentation"), interp.system_package);
    lidefine_global(interp.sym_documentation, NIL, NIL);

    gc_protect(_T("sym-do-not-understand"), &interp.sym_do_not_understand, 1);
    interp.sym_do_not_understand = keyword_intern(_T("do-not-understand"));

    gc_protect_sym (&interp.sym_errobj, _T("errobj"), interp.system_package);
    lidefine_global(interp.sym_errobj, NIL, NIL);

    gc_protect_sym(&interp.sym_global_bad_apply_handler, _T("*global-bad-apply-handler*"), interp.system_package);
    lidefine_global(interp.sym_global_bad_apply_handler, NIL, NIL);

    gc_protect_sym(&interp.sym_uncompiled_function_handler, _T("*uncompiled-function-handler*"), interp.system_package);
    lidefine_global(interp.sym_uncompiled_function_handler, NIL, NIL);

    gc_protect_sym(&interp.sym_global_define_hook, _T("*global-define-hook*"), interp.system_package);
    lidefine_global(interp.sym_global_define_hook, NIL, NIL);

    gc_protect_sym(&interp.sym_internal_files, _T("*internal-files*"), interp.system_package);
    lidefine_global(interp.sym_internal_files, NIL, NIL);

    gc_protect_sym(&interp.sym_vm_runtime_error_handler, _T("*vm-runtime-error-handler*"), interp.system_package);
    lidefine_global(interp.sym_vm_runtime_error_handler, NIL, NIL);

    gc_protect_sym(&interp.sym_vm_signal_handler, _T("*vm-signal-handler*"), interp.system_package);
    lidefine_global(interp.sym_vm_signal_handler, NIL, NIL);

    gc_protect_sym(&interp.sym_stack_overflow, _T("stack-overflow-escape"), interp.system_package);

    gc_protect_sym(&interp.sym_timer_event_handler, _T("*timer-event-handler*"), interp.system_package);
    lidefine_global(interp.sym_timer_event_handler, NIL, NIL);

    gc_protect_sym(&interp.sym_user_break_handler, _T("*user-break-handler*"), interp.system_package);
    lidefine_global(interp.sym_user_break_handler, NIL, NIL);

    gc_protect_sym(&interp.sym_subr_table, _T("*subr-table*"), interp.system_package);
    lidefine_global(interp.sym_subr_table, hashcons(false), NIL);

    LRef temp_sym;

    temp_sym = simple_intern(_T("*pi*"), interp.system_package);
    lidefine_global(temp_sym, flocons (atan (1.0) * 4), NIL);

    gc_protect_sym (&interp.sym_progn, _T("begin"), interp.system_package);
  }


  static void register_main_subrs()
  {
    register_subr(_T("%call-with-global-environment"),    SUBR_2,     (void*)lcall_with_global_environment       );
    register_subr(_T("%closure"),                         SUBR_3,     (void*)lclosurecons                        );
    register_subr(_T("%closure-code"),                    SUBR_1,     (void*)lclosure_code                       );
    register_subr(_T("%closure-env"),                     SUBR_1,     (void*)lclosure_env                        );
    register_subr(_T("%compiled-closure"),                SUBR_3,     (void*)lcompiled_closurecons               );
    register_subr(_T("%copy-structure"),                  SUBR_1,     (void*)lcopy_structure                     );
    register_subr(_T("%current-global-environment"),      SUBR_0,     (void*)lcurrent_global_environment         );
    register_subr(_T("%debug-flags"),                     SUBR_0,     (void*)ldebug_flags                        );
    register_subr(_T("%define"),                          SUBR_F,     (void*)lidefine                            );
    register_subr(_T("%define-global"),                   SUBR_3,     (void*)lidefine_global                     );
    register_subr(_T("%directory"),                       SUBR_2,     (void*)lidirectory                         );
    register_subr(_T("%dump-heap-state"),                 SUBR_1,     (void*)ldump_heap_state                    );
    register_subr(_T("%extend-env"),                      SUBR_MACRO, (void*)lextend_env                         );
    register_subr(_T("%fast-op-cons"),                    SUBR_3,     (void*)lfast_op                            );
    register_subr(_T("%fast-op"),                         SUBR_3,     (void*)lfast_op                            );

    register_subr(_T("%get-current-frames"),              SUBR_1,     (void*)lget_current_frames                 );
    register_subr(_T("%handler-frames"),                  SUBR_0,     (void*)lhandler_frames                     );
    register_subr(_T("%hash-binding-vector"),             SUBR_1,     (void*)lihash_binding_vector               );
    register_subr(_T("%immediate?"),                      SUBR_1,     (void*)liimmediate_p                       );
    register_subr(_T("%instance-map"),                    SUBR_1,     (void*)liinstance_map                      );
    register_subr(_T("%instance-proto"),                  SUBR_1,     (void*)liinstance_proto                    );
    register_subr(_T("%instance-slots"),                  SUBR_1,     (void*)liinstance_slots                    );
    register_subr(_T("%lambda"),                          SUBR_F,     (void*)lilambda                            );
    register_subr(_T("%macro-transformer"),               SUBR_1,     (void*)lmacro_transformer                  );
    register_subr(_T("%make-eof"),                        SUBR_0,     (void*)lmake_eof                           );
    register_subr(_T("%macrocons"),                       SUBR_1,     (void*)limacrocons                         );
    register_subr(_T("%memref"),                          SUBR_1,     (void*)lmemref_byte                        );
    register_subr(_T("%obaddr"),                          SUBR_1,     (void*)lobaddr                             );
    register_subr(_T("%package-bindings"),                SUBR_1,     (void*)lpackage_bindings                   );
    register_subr(_T("%package-use-list"),                SUBR_1,     (void*)lpackage_use_list                   );
    register_subr(_T("%packagecons"),                     SUBR_3,     (void*)lipackagecons                       );
    register_subr(_T("%panic"),                           SUBR_1,     (void*)lpanic                              );
    register_subr(_T("%primitive-kind"),                  SUBR_1,     (void*)lsubr_kind                          );
    register_subr(_T("%property-list"),                   SUBR_1,     (void*)lproperty_list                      );
    register_subr(_T("%representation-of"),               SUBR_1,     (void*)lrepresentation_of                  );
    register_subr(_T("%set-closure-code"),                SUBR_2,     (void*)lset_closure_code                   );
    register_subr(_T("%set-closure-env"),                 SUBR_2,     (void*)lset_closure_env                    );
    register_subr(_T("%set-debug-flags"),                 SUBR_1,     (void*)lset_debug_flags                    );
    register_subr(_T("%set-handler-frames"),              SUBR_1,     (void*)lset_handler_frames                 );
    register_subr(_T("%set-instance-proto!"),             SUBR_2,     (void*)liset_instance_proto                );
    register_subr(_T("%set-interrupt-mask!"),             SUBR_1,     (void*)lset_interrupt_mask                 );
    register_subr(_T("%set-package-name"),                SUBR_2,     (void*)lset_package_name                   );
    register_subr(_T("%set-package-use-list!"),           SUBR_2,     (void*)lset_package_use_list               );
    register_subr(_T("%set-property-list!"),              SUBR_2,     (void*)lset_property_list                  );
    register_subr(_T("%set-stack-limit"),                 SUBR_1,     (void*)lset_stack_limit                    );
    register_subr(_T("%show-type-stats"),                 SUBR_0,     (void*)lshow_type_stats                    );
    register_subr(_T("%fast-op-opcode"),                  SUBR_1,     (void*)lfast_op_opcode                     );
    register_subr(_T("%fast-op-args"),                    SUBR_1,     (void*)lfast_op_args                       );
    register_subr(_T("%stress-c-heap"),                   SUBR_2,     (void*)lstress_c_heap                      );
    register_subr(_T("%stress-lisp-heap"),                SUBR_1,     (void*)lstress_lisp_heap                   );
    register_subr(_T("%lisp-heap-stress-thread"),         SUBR_3,     (void*)llisp_heap_stress_thread            );
    register_subr(_T("%structure-meta")  ,                SUBR_1,     (void*)lstructure_layout                   );
    register_subr(_T("%structure-layout"),                SUBR_1,     (void*)lstructure_layout                   );
    register_subr(_T("%structure-length"),                SUBR_1,     (void*)lstructure_length                   );
    register_subr(_T("%structure-ref"),                   SUBR_2,     (void*)lstructure_ref                      );
    register_subr(_T("%structure-set!"),                  SUBR_3,     (void*)lstructure_set                      );
    register_subr(_T("%structure?"),                      SUBR_2,     (void*)lstructurep                         );
    register_subr(_T("%structurecons"),                   SUBR_2,     (void*)lstructurecons                      );
    register_subr(_T("%symbol-value"),                    SUBR_3,     (void*)lisymbol_value                      );
    register_subr(_T("%sysob"),                           SUBR_1,     (void*)lsysob                              );
    register_subr(_T("%test-blocking-input"),             SUBR_3,     (void*)ltest_blocking_input                );
    register_subr(_T("%gc-trip-wire"),                    SUBR_0,     (void*)ligc_trip_wire                      );
    register_subr(_T("%arm-gc-trip-wires"),               SUBR_1,     (void*)liarm_gc_trip_wires                 );

    register_subr(_T("%time"),                            SUBR_F,     (void*)ltime                               );
    register_subr(_T("%unbound-marker"),                  SUBR_0,     (void*)lunbound_marker                     );
    register_subr(_T("*"),                                SUBR_2N,    (void*)lmultiply                           );
    register_subr(_T("+"),                                SUBR_2N,    (void*)ladd                                );
    register_subr(_T("-"),                                SUBR_2N,    (void*)lsubtract                           );
    register_subr(_T("->ieee-754-bits"),                  SUBR_1,     (void*)lto_ieee754_bits                    );
    register_subr(_T("/"),                                SUBR_2N,    (void*)ldivide                             );
    register_subr(_T("<"),                                SUBR_ARGC,  (void*)lnum_lt                             );
    register_subr(_T("<="),                               SUBR_ARGC,  (void*)lnum_le                             );
    register_subr(_T("="),                                SUBR_ARGC,  (void*)lnum_eq                             );
    register_subr(_T(">"),                                SUBR_ARGC,  (void*)lnum_gt                             );
    register_subr(_T(">="),                               SUBR_ARGC,  (void*)lnum_ge                             );
    register_subr(_T("acos"),                             SUBR_1,     (void*)lacos                               );
    register_subr(_T("and"),                              SUBR_MACRO, (void*)land                                );
    register_subr(_T("angle"),                            SUBR_1,     (void*)langle                              );
    register_subr(_T("append!"),                          SUBR_ARGC,  (void*)lappendd                            );
    register_subr(_T("append"),                           SUBR_ARGC,  (void*)lappend                             );
    register_subr(_T("apply"),                            SUBR_ARGC,  (void*)lapply                              );
    register_subr(_T("asin"),                             SUBR_1,     (void*)lasin                               );
    register_subr(_T("ass"),                              SUBR_3,     (void*)lass                                );
    register_subr(_T("assoc"),                            SUBR_2,     (void*)lassoc                              );
    register_subr(_T("assq"),                             SUBR_2,     (void*)lassq                               );
    register_subr(_T("assv"),                             SUBR_2,     (void*)lassv                               );
    register_subr(_T("atan"),                             SUBR_2,     (void*)latan                               );
    register_subr(_T("add-symbol-to-package"),            SUBR_2,     (void*)ladd_symbol_to_package              );
    register_subr(_T("begin"),                            SUBR_MACRO, (void*)lprogn                              );
    register_subr(_T("binary-port?"),                     SUBR_1,     (void*)lbinary_portp                       );
    register_subr(_T("bitwise-and"),                      SUBR_2N,    (void*)lbitwise_and                        );
    register_subr(_T("bitwise-arithmatic-shift-right"),   SUBR_2,     (void*)lbitwise_ashr                       );
    register_subr(_T("bitwise-not"),                      SUBR_1,     (void*)lbitwise_not                        );
    register_subr(_T("bitwise-or"),                       SUBR_2N,    (void*)lbitwise_or                         );
    register_subr(_T("bitwise-shift-left"),               SUBR_2,     (void*)lbitwise_shl                        );
    register_subr(_T("bitwise-shift-right"),              SUBR_2,     (void*)lbitwise_shr                        );
    register_subr(_T("bitwise-xor"),                      SUBR_2N,    (void*)lbitwise_xor                        );
    register_subr(_T("boolean?"),                         SUBR_1,     (void*)lbooleanp                           );
    register_subr(_T("byte-vector->vector"),              SUBR_1,     (void*)lbyte_vector2vector                 );
    register_subr(_T("byte-vector?"),                     SUBR_1,     (void*)lbyte_vector_p                      );
    register_subr(_T("external-data"),                    SUBR_1,     (void*)lexternal_data                      );
    register_subr(_T("external-desc"),                    SUBR_1,     (void*)lexternal_desc                      );
    register_subr(_T("external?"),                        SUBR_1,     (void*)lexternalp                          );
    register_subr(_T("external-type-name"),               SUBR_1,     (void*)lexternal_type_name                 );
    register_subr(_T("car"),                              SUBR_1,     (void*)lcar                                );
    register_subr(_T("case"),                             SUBR_MACRO, (void*)lcase                               );
    register_subr(_T("catch"),                            SUBR_F,     (void*)lcatch                              );
    register_subr(_T("cdr"),                              SUBR_1,     (void*)lcdr                                );
    register_subr(_T("ceiling"),                          SUBR_1,     (void*)lceiling                            );
    register_subr(_T("char->integer"),                    SUBR_1,     (void*)lchar2integer                       );
    register_subr(_T("char-ready?"),                      SUBR_1,     (void*)lchar_readyp                        );
    register_subr(_T("char?"),                            SUBR_1,     (void*)lcharp                              );
    register_subr(_T("character->string"),                SUBR_1,     (void*)lcharacter2string                   );
    register_subr(_T("clone-c-data-port"),                SUBR_1,     (void*)lclone_c_data_port                  );
    register_subr(_T("clone-instance"),                   SUBR_1,     (void*)lclone_instance                     );
    register_subr(_T("close-port"),                       SUBR_1,     (void*)lclose_port                         );
    register_subr(_T("closure?"),                         SUBR_1,     (void*)lclosurep                           );
    register_subr(_T("compiled-closure?"),                SUBR_1,     (void*)lcompiled_closurep                  );
    register_subr(_T("complex?"),                         SUBR_1,     (void*)lcomplexp                           );
    register_subr(_T("cond"),                             SUBR_MACRO, (void*)lcond                               );
    register_subr(_T("cons"),                             SUBR_2,     (void*)lcons                               );
    register_subr(_T("cos"),                              SUBR_1,     (void*)lcos                                );
    register_subr(_T("debug-backtrace"),                  SUBR_0,     (void*)ldebug_backtrace                    );
    register_subr(_T("debug-write"),                      SUBR_1,     (void*)ldebug_write                        );
    register_subr(_T("declare"),                          SUBR_F,     (void*)ldeclare                            );
    register_subr(_T("delete-file"),                      SUBR_1,     (void*)ldelete_file                        );
    register_subr(_T("delq"),                             SUBR_2,     (void*)ldelq                               );
    register_subr(_T("enlarge-heap"),                     SUBR_1,     (void*)lenlarge_heap                       );
    register_subr(_T("env-lookup"),                       SUBR_2,     (void*)lenvlookup                          );
#ifdef ENVLOOKUP_STATS
    register_subr(_T("%show-env-lookup-stats"),           SUBR_0,     (void*)lshow_env_lookup_stats              );
#endif
    register_subr(_T("environment"),                      SUBR_0,     (void*)lenvironment                        );
    register_subr(_T("eof-object?"),                      SUBR_1,     (void*)leof_objectp                        );
    register_subr(_T("eq?"),                              SUBR_2,     (void*)leq                                 );
    register_subr(_T("equal?"),                           SUBR_2,     (void*)lequal                              );
    register_subr(_T("eqv?"),                             SUBR_2,     (void*)leql                                );
    register_subr(_T("%eval"),                            SUBR_2,     (void*)leval                               );
    register_subr(_T("exact->inexact"),                   SUBR_1,     (void*)lexact2inexact                      );
    register_subr(_T("exact?"),                           SUBR_1,     (void*)lexactp                             );
    register_subr(_T("exp"),                              SUBR_1,     (void*)lexp                                );
    register_subr(_T("expt"),                             SUBR_2,     (void*)lexpt                               );
    register_subr(_T("%fasl-load"),                       SUBR_1,     (void*)lifasl_load                         );
    register_subr(_T("fast-read"),                        SUBR_1,     (void*)lfast_read                          );
    register_subr(_T("%file-details"),                    SUBR_2,     (void*)lifile_details                      );
    register_subr(_T("find-package"),                     SUBR_1,     (void*)lfind_package                       );
    register_subr(_T("floor"),                            SUBR_1,     (void*)lfloor                              );
    register_subr(_T("flush-port"),                       SUBR_1,     (void*)lflush_port                         );
    register_subr(_T("flush-whitespace"),                 SUBR_2,     (void*)lflush_whitespace                   );
    register_subr(_T("for-each"),                         SUBR_ARGC,  (void*)lforeach                            );
    register_subr(_T("fresh-line"),                       SUBR_1,     (void*)lfresh_line                         );
    register_subr(_T("gc"),                               SUBR_0,     (void*)lgc                                 );
    register_subr(_T("gc-info"),                          SUBR_0,     (void*)lgc_info                            );
    register_subr(_T("gc-status"),                        SUBR_1,     (void*)lgc_status                          );
    register_subr(_T("gc-runtime"),                       SUBR_0,     (void*)lgc_runtime                         );
    register_subr(_T("get-output-string"),                SUBR_1,     (void*)lget_output_string                  );
    register_subr(_T("has-slot?"),                        SUBR_2,     (void*)lhas_slotp                          );
    register_subr(_T("hash->a-list"),                     SUBR_1,     (void*)lhash2alist                         );
    register_subr(_T("hash->list"),                       SUBR_1,     (void*)lhash2list                          );
    register_subr(_T("hash-clear!"),                      SUBR_1,     (void*)lhash_clear                         );
    register_subr(_T("hash-copy"),                        SUBR_1,     (void*)lhash_copy                          );
    register_subr(_T("hash-for-each"),                    SUBR_2,     (void*)lhash_foreach                       );
    register_subr(_T("hash-has?"),                        SUBR_2,     (void*)lhash_hasp                          );
    register_subr(_T("sxhash"),                           SUBR_2,     (void*)lsxhash                             );
    register_subr(_T("hash-ref"),                         SUBR_3,     (void*)lhash_ref                           );
    register_subr(_T("hash-ref*"),                        SUBR_2,     (void*)lhash_refs                          );
    register_subr(_T("hash-remove!"),                     SUBR_2,     (void*)lhash_remove                        );
    register_subr(_T("hash-set!"),                        SUBR_3,     (void*)lhash_set                           );
    register_subr(_T("hash-type"),                        SUBR_1,     (void*)lhash_type                          );
    register_subr(_T("hash?"),                            SUBR_1,     (void*)lhashp                              );
    register_subr(_T("ieee-754-bits->"),                  SUBR_1,     (void*)lieee754_bits_to                    );
    register_subr(_T("if"),                               SUBR_MACRO, (void*)lif                                 );
    register_subr(_T("imag-part"),                        SUBR_1,     (void*)limag_part                          );
    register_subr(_T("inexact->display-string"),          SUBR_4,     (void*)linexact2display_string             );
    register_subr(_T("inexact->exact"),                   SUBR_1,     (void*)linexact2exact                      );
    register_subr(_T("inexact?"),                         SUBR_1,     (void*)linexactp                           );
    register_subr(_T("infinite?"),                        SUBR_1,     (void*)linfinitep                          );
    register_subr(_T("input-port?"),                      SUBR_1,     (void*)linput_portp                        );
    register_subr(_T("instance?"),                        SUBR_1,     (void*)linstancep                          );
    register_subr(_T("integer->char"),                    SUBR_1,     (void*)linteger2char                       );
    register_subr(_T("integer?"),                         SUBR_1,     (void*)lintegerp                           );
    register_subr(_T("keyword?"),                         SUBR_1,     (void*)lkeywordp                           );
    register_subr(_T("last-pair"),                        SUBR_1,     (void*)llast_pair                          );
    register_subr(_T("length"),                           SUBR_1,     (void*)llength                             );
    register_subr(_T("list->hash"),                       SUBR_1,     (void*)llist2hash                          );
    register_subr(_T("list->vector"),                     SUBR_1,     (void*)llist2vector                        );
    register_subr(_T("list-copy"),                        SUBR_1,     (void*)llist_copy                          );
    register_subr(_T("list-let"),                         SUBR_MACRO, (void*)llist_let                           );
    register_subr(_T("log"),                              SUBR_1,     (void*)llog                                );
    register_subr(_T("macro?"),                           SUBR_1,     (void*)lmacrop                             );
    register_subr(_T("magnitude"),                        SUBR_1,     (void*)lmagnitude                          );
    register_subr(_T("make-hash"),                        SUBR_1,     (void*)lmake_hash                          );
    register_subr(_T("make-instance"),                    SUBR_N,     (void*)lmake_instance                      );
    register_subr(_T("make-list"),                        SUBR_2,     (void*)lmake_list                          );
    register_subr(_T("make-package!"),                    SUBR_1,     (void*)lmake_package                       );
    register_subr(_T("make-polar"),                       SUBR_2,     (void*)lmake_polar                         );
    register_subr(_T("make-rectangular"),                 SUBR_2,     (void*)lmake_rectangular                   );
    register_subr(_T("make-vector"),                      SUBR_2,     (void*)lmake_vector                        );
    register_subr(_T("map"),                              SUBR_ARGC,  (void*)lmap                                );
    register_subr(_T("map-pair"),                         SUBR_ARGC,  (void*)lmap_pair                           );
    register_subr(_T("modulo"),                           SUBR_2,     (void*)lmodulo                             );
    register_subr(_T("nan?"),                             SUBR_1,     (void*)lnanp                               );
    register_subr(_T("newline"),                          SUBR_1,     (void*)lnewline                            );
    register_subr(_T("not"),                              SUBR_1,     (void*)lnotp                               );
    register_subr(_T("null?"),                            SUBR_1,     (void*)lnullp                              );
    register_subr(_T("number->string"),                   SUBR_3,     (void*)lnumber2string                      );
    register_subr(_T("number?"),                          SUBR_1,     (void*)lnumberp                            );
    register_subr(_T("open-c-data-output"),               SUBR_3,     (void*)lopen_c_data_output                 );
    register_subr(_T("open-debug-port"),                  SUBR_0,     (void*)lopen_debug_port                    );
    register_subr(_T("open-input-file"),                  SUBR_2,     (void*)lopen_input_file                    );
    register_subr(_T("open-input-string"),                SUBR_1,     (void*)lopen_input_string                  );
    register_subr(_T("open-null-port"),                   SUBR_0,     (void*)lopen_null_port                     );
    register_subr(_T("open-output-file"),                 SUBR_2,     (void*)lopen_output_file                   );
    register_subr(_T("open-output-string"),               SUBR_0,     (void*)lopen_output_string                 );
    register_subr(_T("or"),                               SUBR_MACRO, (void*)lor                                 );
    register_subr(_T("output-port?"),                     SUBR_1,     (void*)loutput_portp                       );
    register_subr(_T("package-name"),                     SUBR_1,     (void*)lpackage_name                       );
    register_subr(_T("package?"),                         SUBR_1,     (void*)lpackagep                           );
    register_subr(_T("pair?"),                            SUBR_1,     (void*)lconsp                              );
    register_subr(_T("peek-char"),                        SUBR_1,     (void*)lpeek_char                          );
    register_subr(_T("port-io-counts"),                   SUBR_1,     (void*)lport_io_counts                     );
    register_subr(_T("port-location"),                    SUBR_1,     (void*)lport_location                      );
    register_subr(_T("port-mode"),                        SUBR_1,     (void*)lport_mode                          );
    register_subr(_T("port-name"),                        SUBR_1,     (void*)lport_name                          );
    register_subr(_T("port-translate-mode"),              SUBR_1,     (void*)lport_translate_mode                );
    register_subr(_T("primitive?"),                       SUBR_1,     (void*)lprimitivep                         );
    register_subr(_T("print-external-details"),           SUBR_2,     (void*)lprint_external_details             );
    register_subr(_T("procedure?"),                       SUBR_1,     (void*)lprocedurep                         );
    register_subr(_T("prog1"),                            SUBR_F,     (void*)lprog1                              );
    register_subr(_T("quote"),                            SUBR_F,     (void*)lquote                              );
    register_subr(_T("quotient"),                         SUBR_2,     (void*)lquotient                           );
    register_subr(_T("random"),                           SUBR_1,     (void*)lrandom                             );
    register_subr(_T("rational?"),                        SUBR_1,     (void*)lrationalp                          );
    register_subr(_T("read-binary-fixnum"),               SUBR_3,     (void*)lread_binary_fixnum                 );
    register_subr(_T("read-binary-flonum"),               SUBR_1,     (void*)lread_binary_flonum                 );
    register_subr(_T("read-binary-string"),               SUBR_2,     (void*)lread_binary_string                 );
    register_subr(_T("read-char"),                        SUBR_1,     (void*)lread_char                          );
    register_subr(_T("read-line"),                        SUBR_1,     (void*)lread_line                          );
    register_subr(_T("real-part"),                        SUBR_1,     (void*)lreal_part                          );
    register_subr(_T("real?"),                            SUBR_1,     (void*)lrealp                              );
    register_subr(_T("realtime"),                         SUBR_0,     (void*)lrealtime                           );
    register_subr(_T("realtime-time-zone-offset"),        SUBR_0,     (void*)lrealtime_time_zone_offset          );
    register_subr(_T("remainder"),                        SUBR_2,     (void*)lremainder                          );
    register_subr(_T("repeat"),                           SUBR_F,     (void*)lrepeat                             );
    register_subr(_T("rich-write"),                       SUBR_3,     (void*)lrich_write                         );
    register_subr(_T("round"),                            SUBR_1,     (void*)lround                              );
    register_subr(_T("sleep"),                            SUBR_1,     (void*)lsleep                              );
    register_subr(_T("runtime"),                          SUBR_0,     (void*)lruntime                            );
    register_subr(_T("send"),                             SUBR_N,     (void*)lsend                               );
    register_subr(_T("set!"),                             SUBR_F,     (void*)lsetq                               );
    register_subr(_T("set-car!"),                         SUBR_2,     (void*)lsetcar                             );
    register_subr(_T("set-cdr!"),                         SUBR_2,     (void*)lsetcdr                             );
    register_subr(_T("set-environment-variable!"),        SUBR_2,     (void*)lset_environment_variable           );
    register_subr(_T("set-port-translate-mode!"),         SUBR_2,     (void*)lport_set_translate_mode            );
    register_subr(_T("set-random-seed!"),                 SUBR_1,     (void*)lset_random_seed                    );
    register_subr(_T("set-symbol-value!"),                SUBR_4,     (void*)lsetvar                             );
    register_subr(_T("sin"),                              SUBR_1,     (void*)lsin                                );
    register_subr(_T("%slot-ref"),                        SUBR_2,     (void*)lislot_ref                          );
    register_subr(_T("%slot-set!"),                       SUBR_3,     (void*)lislot_set                          );
    register_subr(_T("sqrt"),                             SUBR_1,     (void*)lsqrt                               );
    register_subr(_T("strcmp"),                           SUBR_2,     (void*)lisp_strcmp                         );
    register_subr(_T("string->number"),                   SUBR_2,     (void*)lstring2number                      );
    register_subr(_T("string->uninterned-symbol"),        SUBR_1,     (void*)lstring2uninterned_symbol           );
    register_subr(_T("string-append"),                    SUBR_ARGC,  (void*)lstring_append                      );
    register_subr(_T("string-copy"),                      SUBR_1,     (void*)lstring_copy                        );
    register_subr(_T("string-downcase!"),                 SUBR_1,     (void*)lstring_downcased                   );
    register_subr(_T("string-downcase"),                  SUBR_1,     (void*)lstring_downcase                    );
    register_subr(_T("string-first-character"),           SUBR_3,     (void*)lstring_first_char                  );
    register_subr(_T("string-first-substring"),           SUBR_3,     (void*)lstring_first_substring             );
    register_subr(_T("string-fold"),                      SUBR_3,     (void*)lstring_fold                        );
    register_subr(_T("string-length"),                    SUBR_1,     (void*)lstring_length                      );
    register_subr(_T("string-ref"),                       SUBR_2,     (void*)lstring_ref                         );
    register_subr(_T("string-search"),                    SUBR_3,     (void*)lstring_search                      );
    register_subr(_T("string-search-from-right"),         SUBR_3,     (void*)lstring_search_from_right           );
    register_subr(_T("string-set!"),                      SUBR_3,     (void*)lstring_set                         );
    register_subr(_T("string-trim"),                      SUBR_2,     (void*)lstring_trim                        );
    register_subr(_T("string-trim-left"),                 SUBR_2,     (void*)lstring_trim_left                   );
    register_subr(_T("string-trim-right"),                SUBR_2,     (void*)lstring_trim_right                  );
    register_subr(_T("string-upcase!"),                   SUBR_1,     (void*)lstring_upcased                     );
    register_subr(_T("string-upcase"),                    SUBR_1,     (void*)lstring_upcase                      );
    register_subr(_T("string?"),                          SUBR_1,     (void*)lstringp                            );
    register_subr(_T("substring"),                        SUBR_3,     (void*)lsubstring                          );
    register_subr(_T("symbol-bound?"),                    SUBR_3,     (void*)lsymbol_boundp                      );
    register_subr(_T("symbol-name"),                      SUBR_1,     (void*)lsymbol_name                        );
    register_subr(_T("symbol-package"),                   SUBR_1,     (void*)lsymbol_package                     );
    register_subr(_T("set-symbol-package!"),              SUBR_2,     (void*)lset_symbol_package                 );
    register_subr(_T("symbol-value"),                     SUBR_3,     (void*)lsymbol_value                       );
    register_subr(_T("symbol?"),                          SUBR_1,     (void*)lsymbolp                            );
    register_subr(_T("system"),                           SUBR_ARGC,  (void*)lsystem                             );
    register_subr(_T("system-info"),                      SUBR_0,     (void*)lsystem_info                        );
    register_subr(_T("tan"),                              SUBR_1,     (void*)ltan                                );
    register_subr(_T("temporary-file-name"),              SUBR_1,     (void*)ltemporary_file_name                );
    register_subr(_T("the-environment"),                  SUBR_F,     (void*)lthe_environment                    );
    register_subr(_T("throw"),                            SUBR_2,     (void*)lthrow                              );
    register_subr(_T("truncate"),                         SUBR_1,     (void*)ltruncate                           );
    register_subr(_T("unbind-symbol!"),                   SUBR_1,     (void*)lunbind_symbol                      );
    register_subr(_T("unread-char"),                      SUBR_2,     (void*)lunread_char                        );
    register_subr(_T("unwind-protect"),                   SUBR_2,     (void*)lunwind_protect                     );
    register_subr(_T("vector"),                           SUBR_ARGC,  (void*)lvector                             );
    register_subr(_T("vector->byte-vector"),              SUBR_1,     (void*)lvector2byte_vector                 );
    register_subr(_T("vector->list"),                     SUBR_1,     (void*)lvector2list                        );
    register_subr(_T("vector-copy"),                      SUBR_1,     (void*)lvector_copy                        );
    register_subr(_T("vector-fill!"),                     SUBR_2,     (void*)lvector_fill                        );
    register_subr(_T("%values"),                          SUBR_N,     (void*)lvalues                             );
    register_subr(_T("%values->list"),                    SUBR_3,     (void*)lvalues2list                        );
    register_subr(_T("vector-ref"),                       SUBR_3,     (void*)lvector_ref                         );
    register_subr(_T("vector-resize!"),                   SUBR_3,     (void*)lvector_resized                     );
    register_subr(_T("vector-resize"),                    SUBR_3,     (void*)lvector_resize                      );
    register_subr(_T("vector-set!"),                      SUBR_3,     (void*)lvector_set                         );
    register_subr(_T("vector?"),                          SUBR_1,     (void*)lvectorp                            );
    register_subr(_T("while"),                            SUBR_F,     (void*)lwhile                              );
    register_subr(_T("while"),                            SUBR_F,     (void*)lwhile                              );
    register_subr(_T("%debug-printer"),                   SUBR_3,     (void*)lidebug_printer                     );
    register_subr(_T("write-binary-fixnum"),              SUBR_4,     (void*)lwrite_binary_fixnum                );
    register_subr(_T("write-binary-flonum"),              SUBR_2,     (void*)lbinary_write_flonum                );
    register_subr(_T("write-strings"),                    SUBR_ARGC,  (void*)lwrite_strings                      );
    register_subr(_T("write-binary-string"),              SUBR_2,     (void*)lwrite_binary_string                );
    register_subr(_T("write-char"),                       SUBR_2,     (void*)lwrite_char                         );
  }

  static void global_environment_asserts()
  {
    void *stack_start = sys_get_stack_start();

    assert(&stack_start < stack_start); // The stack grows downwards, so the stack_start
    // variable should be lower in memory than the
    // start of the stack

    assert(sizeof(LObject) == 4 * sizeof(LRef));
  }

  // !! Init needs a way to receive standard output ports, for non-console uses of scan
  void init0(int argc, _TCHAR *argv[], debug_flag_t initial_debug_flags)
  {
    global_environment_asserts();

    previous_panic_handler = set_panic_handler(scan_panic_handler);

    /** Initialize the interpreter globals */
    memset(&interp, 0, sizeof(interp));

    // We need the debug flags pretty early on, so that we know how
    // to set up debugger I/O.
    interp.debug_flags                             = debug_flags_from_environment(initial_debug_flags);

    init_debugger_output();

    interp.break_pending                           = false;
    interp.timer_event_pending                     = false;
    interp.interrupts_masked                       = false;
    interp.gc_trip_wires_armed                     = false;

    interp.shutting_down                           = false;

    interp.gc_heap_segment_size                    = DEFAULT_HEAP_SEGMENT_SIZE;
    interp.gc_max_heap_segments                    = DEFAULT_MAX_HEAP_SEGMENTS;
    interp.gc_current_heap_segments                = 0;
    interp.gc_heap_segments                        = NULL;

    interp.gc_status_flag                          = 0;

    interp.launch_realtime                         = sys_runtime();

    interp.sym_package_list                        = NIL;

    interp.system_package                          = NIL;
    interp.scheme_package                          = NIL;
    interp.keyword_package                         = NIL;

    // Standard symbols
    interp.sym_after_gc                            = NIL;
    interp.sym_msglvl_info                         = NIL;
    interp.sym_msglvl_errors                       = NIL;
    interp.sym_args0                               = NIL;
    interp.sym_args                                = NIL;
    interp.sym_current_package                     = NIL;
    interp.sym_progn                               = NIL;
    interp.sym_errobj                              = NIL;
    interp.sym_uncompiled_function_handler         = NIL;
    interp.sym_global_define_hook                  = NIL;

    // Statistics Counters
    interp.forms_evaluated                         = 0;
    interp.gc_total_cells_allocated                = 0;
    interp.gc_total_environment_cells_allocated    = 0;
    interp.gc_cells_allocated                      = 0;
    interp.gc_cells_collected                      = 0;

    interp.malloc_bytes_at_last_gc                 = 0;
    interp.malloc_blocks_at_last_gc                = 0;
    interp.c_bytes_gc_threshold                    = (sizeof(LObject) * interp.gc_heap_segment_size);

    interp.gc_total_run_time                       = 0.0;
    interp.gc_run_time                             = 0.0;
    interp.gc_count                                = 0;

#ifdef ENVLOOKUP_STATS
    interp.total_env_lookups                       = 0;
    interp.global_env_lookups                      = 0;
    interp.env_lookup_frames                       = 0;
#endif

    process_vm_arguments(argc, argv);

    if (interp.debug_flags != DF_NONE)
      dscwritef("; DEBUG: debug_flags=0x~cx\n", interp.debug_flags);

    /*** Create the gc heap and populate it with the standard objects */
    create_gc_heap();
    create_initial_packages();
    init_base_scheme_objects();
    init_stdio_ports();

    register_main_subrs();

    gc_protect(_T("handler-frames"), &thread.handler_frames, 1);

    SET_VECTOR_ELEM(interp.global_env, 0, keyword_intern(_T("global-environment")));

    accept_command_line_arguments(argc, argv);

    interp.gc_status_flag = 1;
  }


  flonum_t time_since_launch()
  {
    return sys_runtime() - interp.launch_realtime;
  }

  LRef load_files_from_args0()
  {
    LRef retval = NIL;

    // Skip the first argument... it typically specifies the launch executable
    if (CONSP(SYMBOL_VCELL(interp.sym_args0)))
      SET_SYMBOL_VCELL(interp.sym_args0, CDR(SYMBOL_VCELL(interp.sym_args0)));

    // While there are still arguments in *args0*, load them. Note
    // that this leaves open the possibility that one of the loaded
    // files will take over argument processing and set *args0* to
    // null itself...
    while(CONSP(SYMBOL_VCELL(interp.sym_args0)))
      {
        LRef fname = CAR(SYMBOL_VCELL(interp.sym_args0));

        if (STRINGP(fname))
          {
            scwritef("; Boot Loading ~a...\n", DEFAULT_PORT, CAR(SYMBOL_VCELL(interp.sym_args0)));

            retval = lifasl_load(fname);
          }

        /* This check keeps us from crashing if the loaded file alters
         * *arg0*. This can happen if one of the loaded scheme files
         * decides to start taking over the boot process itself. */
        if (CONSP(SYMBOL_VCELL(interp.sym_args0)))
          SET_SYMBOL_VCELL(interp.sym_args0, CDR(SYMBOL_VCELL(interp.sym_args0)));
      }

    return retval;
  }

  LRef run()
  {
    LRef retval = NIL;

    LRef run0_proc = lisymbol_value(simple_intern(_T("%run0"), interp.scheme_package), NIL, NIL);

    if (NULLP(run0_proc))
      panic("No bootstrap procedure found in scheme::%run0.");

    if (!PROCEDUREP(run0_proc))
      panic("Invalid bootstrap procedure found in scheme::%run0. (failed PROCEDUREP).");

    if(call_lisp_procedure(run0_proc, &retval, NULL, 0))
      panic("Failure during interprer launch.");

    return retval;
  }

  void shutdown()
  {
    interp.shutting_down = TRUE;

    free_gc_heap();
  }

  const _TCHAR *build_id_string()
  {
    return (SCAN_VERSION " - " __DATE__ " " __TIME__);
  }

} // end namespace scan
