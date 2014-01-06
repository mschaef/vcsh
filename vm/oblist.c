
/*
 * oblist.cpp --
 *
 * Package and symbol code.
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include <string.h>

#include "scan-private.h"


/* This is an oblist implementation that provides a subset of
 * Common Lisp-style package semantics.
 *
 * The core package tuple contains three fields:
 *
 * - name - The print name of the package.
 *
 * - symbol_bindings - A hash table mapping names to
 *   cons cells describing symbols. These cons cells
 *   have the following format: ( <symbol> . <exported?> )
 *
 * - use_list - A list of child packages used by the current
 *   package.
 */

/*** Utility Functions ***/
static bool list_of_packages_p(lref_t pkgs)
{
     lref_t ii;

     for (ii = pkgs; CONSP(ii); ii = CDR(ii))
          if (!PACKAGEP(CAR(ii)))
               return false;

     if (!NULLP(ii))
          return false;

     return true;
}

/*** package constructor and accessors ***/

lref_t packagecons0(lref_t name, lref_t bindings, lref_t use_list)
{
     assert(STRINGP(name));

     lref_t new_package = new_cell(TC_PACKAGE);

     SET_PACKAGE_NAME(new_package, name);
     SET_PACKAGE_BINDINGS(new_package, bindings);
     SET_PACKAGE_USE_LIST(new_package, use_list);

     return new_package;
}

lref_t packagecons(lref_t name)
{
     assert(STRINGP(name));

     return packagecons0(name, hashcons(false), NIL);
}

lref_t lipackagecons(lref_t name)
{
     if (!STRINGP(name))
          vmerror_wrong_type_n(1, name);

     return packagecons(name);
}

lref_t lpackagep(lref_t x)
{
     if (PACKAGEP(x))
          return x;
     else
          return boolcons(false);
}

lref_t lpackage_name(lref_t p)      /*  ONLY SCHEME */
{
     if (!PACKAGEP(p))
          vmerror_wrong_type_n(1, p);

     return PACKAGE_NAME(p);
}

lref_t lset_package_name(lref_t p, lref_t new_name)   /*  ONLY SCHEME */
{
     if (!PACKAGEP(p))
          vmerror_wrong_type_n(1, p);

     if (!STRINGP(new_name))
          vmerror_wrong_type_n(2, new_name);

     SET_PACKAGE_NAME(p, new_name);

     return p;
}

lref_t lpackage_bindings(lref_t p)  /*  ONLY SCHEME */
{
     if (!PACKAGEP(p))
          vmerror_wrong_type_n(1, p);

     return PACKAGE_BINDINGS(p);
}

lref_t lpackage_use_list(lref_t p)  /*  ONLY SCHEME */
{
     if (!PACKAGEP(p))
          vmerror_wrong_type_n(1, p);

     return PACKAGE_USE_LIST(p);
}

lref_t lset_package_use_list(lref_t p, lref_t use_list)
{
     if (!PACKAGEP(p))
          vmerror_wrong_type_n(1, p);

     if (!list_of_packages_p(use_list))
          vmerror_arg_out_of_range(use_list, _T("bad use list"));

     SET_PACKAGE_USE_LIST(p, use_list);

     return p;
}

/*** support primitives for symbols and name mappings  ***/

/* Find a symbol record local to the specified package. */
static lref_t find_direct_symbol_record(lref_t sym_spec, lref_t package)
{
     lref_t sym_rec;
     lref_t sym_name;

     assert(PACKAGEP(package));

     if (SYMBOLP(sym_spec))
          sym_name = SYMBOL_PNAME(sym_spec);
     else
          sym_name = sym_spec;

     assert(STRINGP(sym_name));

     if (!hash_ref(PACKAGE_BINDINGS(package), sym_name, &sym_rec))
          return NIL;

     /*  If we find a different symbol of the same name in this package, then
      *  we haven't found the requested symbol. Also, we've proven ourselves
      *  not to contain the requested symbol. */
     if (SYMBOLP(sym_spec) && (sym_spec != CAR(sym_rec)))
          return NIL;

     return sym_rec;
}

lref_t ladd_symbol_to_package(lref_t symbol, lref_t package)
{
     if (!SYMBOLP(symbol))
          vmerror_wrong_type_n(1, symbol);
     if (!PACKAGEP(package))
          vmerror_wrong_type_n(2, package);

     /* keyword symbols are created with the external flag set to #t. */
     bool is_keyword = (package == interp.control_fields[VMCTRL_PACKAGE_KEYWORD]);

     lref_t symbol_record = lcons(symbol, boolcons(is_keyword));

     lhash_set(PACKAGE_BINDINGS(package), SYMBOL_PNAME(symbol), symbol_record);

     return NIL;
}


/*** symbol constructor and accessors ***/

lref_t symcons(lref_t pname, lref_t home)
{
     assert(STRINGP(pname));
     assert(NULLP(home) || PACKAGEP(home));

     lref_t z = new_cell(TC_SYMBOL);

     SET_SYMBOL_PNAME(z, pname);
     SET_SYMBOL_VCELL(z, UNBOUND_MARKER);
     SET_SYMBOL_HOME(z, home);

     return z;
}

lref_t lsymbolp(lref_t x)
{
     if (SYMBOLP(x))
          return x;
     else
          return boolcons(false);
}


lref_t lkeywordp(lref_t x)
{
     if (SYMBOLP(x) && (SYMBOL_HOME(x) == interp.control_fields[VMCTRL_PACKAGE_KEYWORD]))
          return x;
     else
          return boolcons(false);
}

/* A simpler variant of intern that does not honor use lists
 * and reports errors via return value. */
lref_t simple_intern(lref_t print_name, lref_t package)
{
     if (!STRINGP(print_name) || !PACKAGEP(package))
          return NIL;

     if (STRING_DIM(print_name) <= 0)
          return NIL;

     lref_t sym_rec = find_direct_symbol_record(print_name, package);

     if (!NULLP(sym_rec))
          return lcar(sym_rec);

     lref_t sym = symcons(print_name, package);

     ladd_symbol_to_package(sym, package);

     return sym;
}


lref_t keyword_intern(const _TCHAR * name)
{
     return simple_intern(strconsbuf(name),
                          interp.control_fields[VMCTRL_PACKAGE_KEYWORD]);
}

/*** Symbol primitives ***/

lref_t lsymbol_package(lref_t sym)
{
     if (!SYMBOLP(sym))
          vmerror_wrong_type_n(1, sym);

     if (NULLP(SYMBOL_HOME(sym)))
          return boolcons(false);
     else
          return SYMBOL_HOME(sym);
}

lref_t lset_symbol_package(lref_t sym, lref_t package)
{
     if (!SYMBOLP(sym))
          vmerror_wrong_type_n(1, sym);
     if (!(PACKAGEP(package) || NULLP(package)))
          vmerror_wrong_type_n(2, package);

     SET_SYMBOL_HOME(sym, package);

     return sym;
}

lref_t lisymbol_index(lref_t symbol)
{
     if (!SYMBOLP(symbol))
          vmerror_wrong_type_n(1, symbol);

     return fixcons(-1);
}


lref_t lsymbol_name(lref_t sym)
{
     if (!SYMBOLP(sym))
          vmerror_wrong_type_n(1, sym);

     return SYMBOL_PNAME(sym);
}

lref_t lstring2uninterned_symbol(lref_t str)
{
     if (!STRINGP(str))
          vmerror_wrong_type_n(1, str);

     if (STRING_DIM(str) <= 0)
          vmerror_arg_out_of_range(str, _T("length > 0"));

     return symcons(str, NIL);
}

/**** Initialization code ****/

void create_initial_packages()
{
     interp.control_fields[VMCTRL_PACKAGE_SYSTEM] = packagecons(strconsbuf("system"));
     interp.control_fields[VMCTRL_PACKAGE_SCHEME] = packagecons(strconsbuf("scheme"));
     interp.control_fields[VMCTRL_PACKAGE_KEYWORD] = packagecons(strconsbuf("keyword"));

     interp.fasl_package_list = listn(3,
                                      interp.control_fields[VMCTRL_PACKAGE_SCHEME],
                                      interp.control_fields[VMCTRL_PACKAGE_SYSTEM],
                                      interp.control_fields[VMCTRL_PACKAGE_KEYWORD]);
}

