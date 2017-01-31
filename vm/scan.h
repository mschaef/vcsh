/*
 * scan.h --
 *
 * The primary interpreter header file.
 *
 * (C) Copyright 2001-2014 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#ifndef __SCAN_H
#define __SCAN_H

#include <setjmp.h>
#include <stdarg.h>
#include <stdio.h>

#include "scan-base.h"
#include "scan-internal-file.h"
#include "scan-constants.h"
#include "scan-types.h"
#include "scan-sys.h"

/**** Startup/Shutdown ****/

void init0(int argc, _TCHAR * argv[], enum debug_flag_t initial_debug_flags);
void init(int argc, _TCHAR * argv[], enum debug_flag_t initial_debug_flags);

lref_t run();

void signal_interrupt(enum vminterrupt_t intr);

void shutdown();

const _TCHAR *scan_vm_build_id_string();

/**** Fatal Errors ****/

typedef void (*panic_handler_t) (void);

panic_handler_t set_panic_handler(panic_handler_t new_handler);

void _panic(const _TCHAR * str, const _TCHAR * filename, long lineno);

#define panic(str) _panic(str, __FILE__, __LINE__)

/**** Custom Extensions ****/

void register_subr(const _TCHAR * name, enum subr_arity_t arity, void *implementation);

/*** The evaluator ***/

lref_t apply1(lref_t fn, size_t argc, lref_t argv[]);

/**** Vector Constuctor ****/

lref_t vectorcons(fixnum_t n, lref_t initial /* = NIL */);

/**** Numbers ****/

#define fixabs labs

lref_t fixcons(fixnum_t x);

lref_t flocons(double x);
lref_t cmplxcons(flonum_t re, flonum_t im);

fixnum_t get_c_fixnum(lref_t x);
long get_c_long(lref_t x);
flonum_t get_c_flonum(lref_t x);

bool get_c_complex(lref_t x, flonum_t *re, flonum_t *im);

lref_t charcons(_TCHAR ch);

/**** Strings ****/

lref_t symcons(lref_t pname, lref_t home);

lref_t simple_intern(lref_t name, lref_t package);

lref_t intern(lref_t name, lref_t package);
lref_t keyword_intern(const _TCHAR * name);

/**** Strings ****/

lref_t strcons();
lref_t strconsch(_TCHAR ch);
lref_t strconsbuf(const _TCHAR * buffer);
lref_t strconsbuf1(const _TCHAR * buffer, _TCHAR trailing);
lref_t strconsdup(lref_t str);
lref_t strconsbufn(size_t length, const _TCHAR * buffer);
lref_t strcons_transfer_buffer(size_t length, _TCHAR * buffer);

_TCHAR *get_c_string(lref_t x);
_TCHAR *get_c_string_dim(lref_t x, size_t *);
_TCHAR *try_get_c_string(lref_t x);

/**** Hash Tables ****/

lref_t hashcons(bool shallow);

bool hash_ref(lref_t table, lref_t key, lref_t *result);

typedef size_t hash_iter_t;
void hash_iter_begin(lref_t hash, hash_iter_t * iter);
bool hash_iter_next(lref_t hash, hash_iter_t * iter, lref_t * key, lref_t * val);

/**** Port I/O ****/

lref_t portcons(struct port_class_t * cls,
                lref_t port_name,
                enum port_mode_t mode,
                lref_t user_object,
                void *user_data);

size_t read_bytes(lref_t port, void *buf, size_t size);
size_t write_bytes(lref_t port, const void *buf, size_t size);

void write_char(lref_t port, _TCHAR ch);
size_t write_text(lref_t port, const _TCHAR * buf, size_t count);

int read_char(lref_t port);
int peek_char(lref_t port);

bool read_binary_fixnum_uint8(lref_t port, fixnum_t *result);
bool read_binary_fixnum_int8(lref_t port, fixnum_t *result);
bool read_binary_fixnum_uint16(lref_t port, fixnum_t *result);
bool read_binary_fixnum_int16(lref_t port, fixnum_t *result);
bool read_binary_fixnum_uint32(lref_t port, fixnum_t *result);
bool read_binary_fixnum_int32(lref_t port, fixnum_t *result);
bool read_binary_fixnum_uint64(lref_t port, fixnum_t *result);
bool read_binary_fixnum_int64(lref_t port, fixnum_t *result);

bool read_binary_flonum(lref_t port, flonum_t *result);

/**** Internal Files and Blocking Input Ports ****/

void register_internal_file(struct internal_file_t *data);

lref_t open_c_data_input(struct internal_file_t *data);

/**** Formatted and Debug I/O ****/

void scwritef(const _TCHAR * format_str, lref_t port, ...);

void dscwritef_impl(const _TCHAR * format_str, ...);

bool is_debug_flag_set(enum debug_flag_t flag);

#define dscwritef(flag, args) \
     do { if (is_debug_flag_set(flag)) dscwritef_impl args; } while(0);

lref_t debug_print_object(lref_t exp, lref_t port, bool machine_readable);

/**** Error handling and control ****/

enum vmt_options_t {
     /* Use one of these... */
     VMT_MANDATORY_TRAP      = 0x0,
     VMT_OPTIONAL_TRAP       = 0x1,

     /* ...and optionally this. */
     VMT_HANDLER_MUST_ESCAPE = 0x2
};

lref_t vmtrap(enum trap_type_t trap, enum vmt_options_t options, size_t argc, ...);

void vmerror_wrong_type(lref_t new_errobj);
void vmerror_wrong_type_n(int which_argument, lref_t new_errobj);
void vmerror_unbound(lref_t v);
void vmerror_index_out_of_bounds(lref_t index, lref_t obj);
void vmerror_arg_out_of_range(lref_t arg, const _TCHAR *range_desc /* = NULL */);
void vmerror_unsupported(const _TCHAR *desc);
void vmerror_unimplemented(const _TCHAR *desc);
void vmerror_divide_by_zero();
void vmerror_io_error(const _TCHAR *desc, lref_t info);
void vmerror_fast_read(const _TCHAR * message, lref_t port, lref_t details /* = NIL */);
void vmerror_stack_overflow(uint8_t * obj);

/***** Prototypes for C Primitives *****/

lref_t lacos(lref_t x);
lref_t ladd(lref_t x, lref_t y);
lref_t ladd_symbol_to_package(lref_t symbol, lref_t package);
lref_t langle(lref_t cmplx);
lref_t lapply(size_t argc, lref_t argv[]);
lref_t lasin(lref_t x);
lref_t latan(lref_t x, lref_t y);
lref_t lbinary_portp(lref_t obj);
lref_t lbinary_write_flonum(lref_t v, lref_t port);
lref_t lbitwise_and(lref_t x, lref_t y);
lref_t lbitwise_ashr(lref_t x, lref_t n);
lref_t lbitwise_not(lref_t x);
lref_t lbitwise_or(lref_t x, lref_t y);
lref_t lbitwise_shl(lref_t x, lref_t n);
lref_t lbitwise_shr(lref_t x, lref_t n);
lref_t lbitwise_xor(lref_t x, lref_t y);
lref_t lbooleanp(lref_t x);
lref_t lcar(lref_t x);
lref_t lcdr(lref_t x);
lref_t lcdrs(lref_t x);
lref_t lceiling(lref_t x);
lref_t lchar2integer(lref_t s);
lref_t lcharacter2string(lref_t obj);
lref_t lcharp(lref_t x);
lref_t lclone_c_data_port(lref_t port);
lref_t lclose_port(lref_t port);
lref_t lclosure_code(lref_t exp);
lref_t lclosure_env(lref_t exp);
lref_t lclosurecons(lref_t env, lref_t code, lref_t property_list);
lref_t lclosurep(lref_t obj);
lref_t lcomplexp(lref_t x);
lref_t lconsp(lref_t x);
lref_t lcopy_structure(lref_t st);
lref_t lcos(lref_t x);
lref_t ldebug_flags();
lref_t ldebug_write(lref_t form);
lref_t ldelete_file(lref_t filename);
lref_t ldisplay_to_string(lref_t exp);
lref_t ldivide(lref_t x, lref_t y);
lref_t ldo_external_symbols(lref_t args, lref_t env);
lref_t ldo_symbols(lref_t args, lref_t env);
lref_t ldump_heap_state(lref_t port);
lref_t lenvironment();
lref_t lenvlookup(lref_t var, lref_t env);
lref_t leof_objectp(lref_t obj);
lref_t leq(lref_t x, lref_t y);
lref_t leql(lref_t x, lref_t y);
lref_t lequal(lref_t, lref_t);
lref_t lexact2inexact(lref_t x);
lref_t lexactp(lref_t x);
lref_t lexp(lref_t x);
lref_t lexpt(lref_t x, lref_t y);
lref_t lfast_op(lref_t opcode, lref_t arg1, lref_t arg2, lref_t next);
lref_t lfast_op_args(lref_t fast_op);
lref_t lfast_op_opcode(lref_t fast_op);
lref_t lfast_op_next(lref_t fast_op);
lref_t lfast_read(lref_t port);
lref_t lfile_sha1_digest(lref_t fn);
lref_t lfloor(lref_t x);
lref_t lflush_port(lref_t port);
lref_t lflush_whitespace(lref_t port, lref_t slc);
lref_t lfresh_line(lref_t port);
lref_t lgc();
lref_t lgc_info();
lref_t lgc_runtime();
lref_t lgc_status(lref_t new_gc_status);
lref_t lget_output_string(lref_t port);
lref_t lhandler_frames();
lref_t lhash2alist(lref_t hash);
lref_t lhash2list(lref_t hash);
lref_t lhash_clear(lref_t hash);
lref_t lhash_copy(lref_t hash);
lref_t lhash_hasp(lref_t table, lref_t key);
lref_t lhash_key(lref_t obj);
lref_t lhash_ref(size_t argc, lref_t argv[]);
lref_t lhash_refs(lref_t table, lref_t key);
lref_t lhash_remove(lref_t table, lref_t key);
lref_t lhash_set(lref_t table, lref_t key, lref_t value);
lref_t lhash_set_multiple(lref_t hash, lref_t bindings);
lref_t lhashp(lref_t obj);
lref_t lheap_cell_count_by_typecode();
lref_t licontrol_field(lref_t control_field_id);
lref_t lidebug_printer(lref_t obj, lref_t port, lref_t machine_readable_p);
lref_t lidefine_global(lref_t var, lref_t val);
lref_t lidirectory(lref_t dirname, lref_t mode);
lref_t lidentity_hash_p(lref_t obj);
lref_t lifile_details(lref_t path, lref_t existance_onlyp);
lref_t lihash_binding_vector(lref_t hash);
lref_t liifasl_load(lref_t port);
lref_t liimmediate_p(lref_t obj);
lref_t liinternal_files();
lref_t liload(lref_t fname);
lref_t limacrocons(lref_t t);
lref_t limag_part(lref_t cmplx);
lref_t linexact2display_string(lref_t n, lref_t sf, lref_t sci, lref_t s);
lref_t linexact2exact(lref_t x);
lref_t linexactp(lref_t x);
lref_t linfinitep(lref_t x);
lref_t linput_portp(lref_t obj);
lref_t linteger2char(lref_t s);
lref_t lintegerp(lref_t x);
lref_t lipackagecons(lref_t name);
lref_t lirequest_heap_size(lref_t c);
lref_t liset_control_field(lref_t control_field_id, lref_t new_value);
lref_t liset_trap_handler(lref_t trap_id, lref_t new_handler);
lref_t lisp_strcmp(lref_t string_1, lref_t string_2);
lref_t lisp_stricmp(lref_t string_1, lref_t string_2);
lref_t listartup_args();
lref_t lisubr_table();
lref_t lisymbol_globally_boundp(lref_t sym);
lref_t lisymbol_index(lref_t symbol);
lref_t litrap_handler(lref_t trap_id);
lref_t litypecode(lref_t obj);
lref_t lkeywordp(lref_t x);
lref_t llast(lref_t);
lref_t llength(lref_t obj);
lref_t llisp_heap_stress_thread(lref_t t, lref_t c, lref_t s);
lref_t llist(lref_t l);
lref_t llist2vector(lref_t l);
lref_t llog(lref_t x);
lref_t lmacro_transformer(lref_t mac);
lref_t lmacrop(lref_t obj);
lref_t lmagnitude(lref_t cmplx);
lref_t lmake_eof();
lref_t lmake_fasl_reader(lref_t port);
lref_t lmake_hash();
lref_t lmake_identity_hash();
lref_t lmake_polar(lref_t r, lref_t theta);
lref_t lmake_rectangular(lref_t re, lref_t im);
lref_t lmake_vector(lref_t dim, lref_t initial);
lref_t lmemref(lref_t addr);
lref_t lmodulo(lref_t x, lref_t y);
lref_t lmultiply(lref_t x, lref_t y);
lref_t lnanp(lref_t x);
lref_t lnewline(lref_t);
lref_t lnotp(lref_t x);
lref_t lnullp(lref_t x);
lref_t lnum_eq(size_t argc, lref_t argv[]);
lref_t lnum_ge(size_t argc, lref_t argv[]);
lref_t lnum_gt(size_t argc, lref_t argv[]);
lref_t lnum_le(size_t argc, lref_t argv[]);
lref_t lnum_lt(size_t argc, lref_t argv[]);
lref_t lnumber2string(lref_t x, lref_t r, lref_t s, lref_t p);
lref_t lnumberp(lref_t x);
lref_t lobaddr(lref_t object);
lref_t lopen_debug_port();
lref_t lopen_input_string(lref_t string);
lref_t lopen_null_port();
lref_t lopen_output_string();
lref_t lopen_raw_input_file(lref_t filename);
lref_t lopen_raw_output_file(lref_t filename);
lref_t lopen_text_input_port(lref_t underlying);
lref_t lopen_text_output_port(lref_t underlying);
lref_t loutput_portp(lref_t obj);
lref_t lpackagcons(lref_t name);
lref_t lpackage_bindings(lref_t p);
lref_t lpackage_name(lref_t p);
lref_t lpackage_use_list(lref_t p);
lref_t lpackagep(lref_t x);
lref_t lpanic(lref_t msg);
lref_t lpeek_char(lref_t port);
lref_t lportp(lref_t port);
lref_t lport_closedp(lref_t obj);
lref_t lport_column(lref_t port);
lref_t lport_row(lref_t port);
lref_t lport_name(lref_t port);
lref_t lport_openp(lref_t obj);
lref_t lport_set_translate_mode(lref_t port, lref_t mode);
lref_t lport_translate_mode(lref_t port);
lref_t lprimitivep(lref_t obj);
lref_t lprocedurep(lref_t exp);
lref_t lproperty_list(lref_t exp);
lref_t lqsort(lref_t l, lref_t f, lref_t g);
lref_t lquotient(lref_t x, lref_t y);
lref_t lrandom(lref_t n);
lref_t lrationalp(lref_t x);
lref_t lread_binary_fixnum_uint8(lref_t port);
lref_t lread_binary_fixnum_int8(lref_t port);
lref_t lread_binary_fixnum_uint16(lref_t port);
lref_t lread_binary_fixnum_int16(lref_t port);
lref_t lread_binary_fixnum_uint32(lref_t port);
lref_t lread_binary_fixnum_int32(lref_t port);
lref_t lread_binary_fixnum_uint64(lref_t port);
lref_t lread_binary_fixnum_int64(lref_t port);
lref_t lread_binary_flonum(lref_t port);
lref_t lread_binary_string(lref_t l, lref_t port);
lref_t lread_char(lref_t port);
lref_t lread_line(lref_t port);
lref_t lread_port_to_string(lref_t port);
lref_t lreal_part(lref_t cmplx);
lref_t lrealp(lref_t x);
lref_t lrealtime(void);
lref_t lrealtime_time_zone_offset();
lref_t lremainder(lref_t x, lref_t y);
lref_t lrepresentation_of(lref_t obj);
lref_t lrich_write(lref_t obj, lref_t machine_readable, lref_t port);
lref_t lroundnum(lref_t x);
lref_t lruntime(void);
lref_t lset_closure_code(lref_t exp, lref_t code);
lref_t lset_closure_env(lref_t exp, lref_t env);
lref_t lset_debug_flags(lref_t c);
lref_t lset_environment_variable(lref_t varname, lref_t value);
lref_t lset_fasl_package_list(lref_t packages);
lref_t lset_handler_frames(lref_t new_frames);
lref_t lset_interrupt_mask(lref_t new_mask);
lref_t lset_package_name(lref_t p, lref_t new_name);
lref_t lset_package_use_list(lref_t p, lref_t use_list);
lref_t lset_property_list(lref_t exp, lref_t property_list);
lref_t lset_random_seed(lref_t s);
lref_t lset_stack_limit(lref_t);
lref_t lset_symbol_package(lref_t sym, lref_t package);
lref_t lset_symbol_vcell(lref_t sym, lref_t val);
lref_t lsetcar(lref_t cell, lref_t value);
lref_t lsetcdr(lref_t cell, lref_t value);
lref_t lsin(lref_t x);
lref_t lsleep(lref_t ms);
lref_t lsqrt(lref_t x);
lref_t lstress_c_heap(lref_t c, lref_t s);
lref_t lstress_lisp_heap(lref_t c);
lref_t lstring2number(lref_t, lref_t);
lref_t lstring2uninterned_symbol(lref_t str);
lref_t lstring_append(size_t argc, lref_t argv[]);
lref_t lstring_copy(lref_t string);
lref_t lstring_downcase(lref_t);
lref_t lstring_downcased(lref_t);
lref_t lstring_first_char(lref_t string, lref_t char_set, lref_t initial_ofs);
lref_t lstring_first_substring(lref_t string, lref_t char_set, lref_t initial_ofs);
lref_t lstring_length(lref_t string);
lref_t lstring_ref(lref_t a, lref_t i);
lref_t lstring_search(lref_t token, lref_t str, lref_t maybe_from);
lref_t lstring_search_from_right(lref_t tok, lref_t str, lref_t maybe_from);
lref_t lstring_set(lref_t a, lref_t i, lref_t v);
lref_t lstring_trim(lref_t, lref_t);
lref_t lstring_trim_left(lref_t, lref_t);
lref_t lstring_trim_right(lref_t, lref_t);
lref_t lstring_upcase(lref_t);
lref_t lstring_upcased(lref_t);
lref_t lstringp(lref_t x);
lref_t lstructure_layout(lref_t st);
lref_t lstructure_length(lref_t st);
lref_t lstructure_ref(lref_t st, lref_t index);
lref_t lstructure_set(lref_t st, lref_t index, lref_t value);
lref_t lstructurecons(lref_t slots, lref_t layout);
lref_t lstructurep(lref_t st, lref_t expected_layout);
lref_t lsubr_name(lref_t subr);
lref_t lsubr_type_code(lref_t subr);
lref_t lsubset(lref_t fcn, lref_t l);
lref_t lsubstring(lref_t, lref_t, lref_t);
lref_t lsubtract(lref_t x, lref_t y);
lref_t lsxhash(lref_t obj, lref_t hash);
lref_t lsymbol_name(lref_t sym);
lref_t lsymbol_package(lref_t sym);
lref_t lsymbol_vcell(lref_t sym);
lref_t lsymbolp(lref_t x);
lref_t lsysob(lref_t addr);
lref_t lsystem(size_t argc, lref_t argv[]);
lref_t lsystem_info();
lref_t ltan(lref_t x);
lref_t ltemporary_file_name(lref_t prefix);
lref_t ltime_apply0(lref_t fn);
lref_t ltruncate(lref_t x);
lref_t lunbound_marker();
lref_t lvalues(lref_t values);
lref_t lvalues2list(lref_t obj);
lref_t lvector(size_t argc, lref_t argv[]);
lref_t lvector2list(lref_t vec);
lref_t lvector_copy(lref_t vec);
lref_t lvector_fill(lref_t vec, lref_t v);
lref_t lvector_ref(lref_t a, lref_t i, lref_t d);
lref_t lvector_resize(lref_t vec, lref_t new_size, lref_t new_element);
lref_t lvector_set(lref_t a, lref_t i, lref_t v);
lref_t lvectorp(lref_t obj);
lref_t lwrite_binary_fixnum_uint8(lref_t v, lref_t port);
lref_t lwrite_binary_fixnum_int8(lref_t v, lref_t port);
lref_t lwrite_binary_fixnum_uint16(lref_t v, lref_t port);
lref_t lwrite_binary_fixnum_int16(lref_t v, lref_t port);
lref_t lwrite_binary_fixnum_uint32(lref_t v, lref_t port);
lref_t lwrite_binary_fixnum_int32(lref_t v, lref_t port);
lref_t lwrite_binary_fixnum_uint64(lref_t v, lref_t port);
lref_t lwrite_binary_fixnum_int64(lref_t v, lref_t port);
lref_t lwrite_binary_string(lref_t string, lref_t port);
lref_t lwrite_char(lref_t ch, lref_t port);
lref_t lwrite_strings(size_t argc, lref_t argv[]);
lref_t lwrite_to_string(lref_t exp);

#endif
