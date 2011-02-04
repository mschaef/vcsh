
/* oblist.cpp
 *
 * Package and symbol code.
 *
 * This is an oblist implementation that provides a subset of
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

#include <string.h>

#include "scan.h"

BEGIN_NAMESPACE(scan)

/*** Utility Functions ***/
static bool list_of_packages_p(LRef pkgs)
{
     LRef ii;

     for (ii = pkgs; CONSP(ii); ii = CDR(ii))
          if (!PACKAGEP(CAR(ii)))
               return false;

     if (!NULLP(ii))
          return false;

     return true;
}

/*** package constructor and accessors ***/

LRef packagecons(LRef name, LRef bindings, LRef use_list)
{
     assert(STRINGP(name));

     LRef new_package = new_cell(TC_PACKAGE);

     SET_PACKAGE_NAME(new_package, name);
     SET_PACKAGE_BINDINGS(new_package, bindings);
     SET_PACKAGE_USE_LIST(new_package, use_list);

     return new_package;
}

LRef packagecons(LRef name)
{
     assert(STRINGP(name));

     return packagecons(name, hashcons(false), NIL);
}

LRef lipackagecons(LRef name)
{
     if (!STRINGP(name))
          vmerror_wrong_type(1, name);

     return packagecons(name);
}

LRef lpackagep(LRef x)
{
     if (PACKAGEP(x))
          return x;
     else
          return boolcons(false);
}

LRef lpackage_name(LRef p)      /*  ONLY SCHEME */
{
     if (!PACKAGEP(p))
          vmerror_wrong_type(1, p);

     return PACKAGE_NAME(p);
}

LRef lset_package_name(LRef p, LRef new_name)   /*  ONLY SCHEME */
{
     if (!PACKAGEP(p))
          vmerror_wrong_type(1, p);

     if (!STRINGP(new_name))
          vmerror_wrong_type(2, new_name);

     SET_PACKAGE_NAME(p, new_name);

     return p;
}

LRef lpackage_bindings(LRef p)  /*  ONLY SCHEME */
{
     if (!PACKAGEP(p))
          vmerror_wrong_type(1, p);

     return PACKAGE_BINDINGS(p);
}

LRef lpackage_use_list(LRef p)  /*  ONLY SCHEME */
{
     if (!PACKAGEP(p))
          vmerror_wrong_type(1, p);

     return PACKAGE_USE_LIST(p);
}

LRef lset_package_use_list(LRef p, LRef use_list)
{
     if (!PACKAGEP(p))
          vmerror_wrong_type(1, p);

     if (!list_of_packages_p(use_list))
          vmerror_arg_out_of_range(use_list, _T("bad use list"));

     SET_PACKAGE_USE_LIST(p, use_list);

     return p;
}

/*** support primitives for symbols and name mappings  ***/

/* Find a symbol record local to the specified package. */
static LRef find_direct_symbol_record(LRef sym_spec, LRef package)
{
     LRef sym_rec;
     LRef sym_name;

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

LRef ladd_symbol_to_package(LRef symbol, LRef package)
{
     if (!SYMBOLP(symbol))
          vmerror_wrong_type(1, symbol);
     if (!PACKAGEP(package))
          vmerror_wrong_type(2, package);

     /* keyword symbols are created with the external flag set to #t. */
     bool is_keyword = (package == interp.control_fields[VMCTRL_PACKAGE_KEYWORD]);

     LRef symbol_record = lcons(symbol, boolcons(is_keyword));

     lhash_set(PACKAGE_BINDINGS(package), SYMBOL_PNAME(symbol), symbol_record);

     return NIL;
}


/*** symbol constructor and accessors ***/

LRef symcons(_TCHAR * pname, LRef home)
{
     assert(pname != NULL);
     assert(_tcslen(pname) > 0);

     return symcons(strcons(pname), home);
}

LRef symcons(LRef pname, LRef home)
{
     assert(STRINGP(pname));
     assert(NULLP(home) || PACKAGEP(home));

     LRef z = new_cell(TC_SYMBOL);

     SET_SYMBOL_PNAME(z, pname);
     SET_SYMBOL_VCELL(z, UNBOUND_MARKER);
     SET_SYMBOL_HOME(z, home);

     return z;
}

LRef lsymbolp(LRef x)
{
     if (SYMBOLP(x))
          return x;
     else
          return boolcons(false);
}


LRef lkeywordp(LRef x)
{
     if (SYMBOLP(x) && (SYMBOL_HOME(x) == interp.control_fields[VMCTRL_PACKAGE_KEYWORD]))
          return x;
     else
          return boolcons(false);
}

/* A simpler variant of intern that does not honor use lists
 * and reports errors via return value. */
LRef simple_intern(LRef print_name, LRef package)
{
     if (!STRINGP(print_name) || !PACKAGEP(package))
          return NIL;

     if (STRING_DIM(print_name) <= 0)
          return NIL;

     LRef sym_rec = find_direct_symbol_record(print_name, package);

     if (!NULLP(sym_rec))
          return lcar(sym_rec);

     LRef sym = symcons(print_name, package);

     ladd_symbol_to_package(sym, package);

     return sym;
}

LRef simple_intern(const _TCHAR * name, LRef package)
{
     return simple_intern(strcons(name), package);
}

LRef keyword_intern(const _TCHAR * name)
{
     return simple_intern(strcons(name), interp.control_fields[VMCTRL_PACKAGE_KEYWORD]);
}

/*** Symbol primitives ***/

LRef lsymbol_package(LRef sym)  /*   REVISIT: fix split between _home and _package */
{
     if (!SYMBOLP(sym))
          vmerror_wrong_type(1, sym);

     if (NULLP(SYMBOL_HOME(sym)))
          return boolcons(false);
     else
          return SYMBOL_HOME(sym);
}

LRef lset_symbol_package(LRef sym, LRef package)
{
     if (!SYMBOLP(sym))
          vmerror_wrong_type(1, sym);
     if (!(PACKAGEP(package) || NULLP(package)))
          vmerror_wrong_type(2, package);

     SET_SYMBOL_HOME(sym, package);

     return sym;
}

LRef lisymbol_index(LRef symbol)
{
     if (!SYMBOLP(symbol))
          vmerror_wrong_type(1, symbol);

     return fixcons(-1);
}


LRef lsymbol_name(LRef sym)
{
     if (!SYMBOLP(sym))
          vmerror_wrong_type(1, sym);

     return SYMBOL_PNAME(sym);
}

LRef lstring2uninterned_symbol(LRef str)
{
     if (!STRINGP(str))
          vmerror_wrong_type(1, str);

     if (STRING_DIM(str) <= 0)
          vmerror_arg_out_of_range(str, _T("length > 0"));

     _TCHAR *sname = get_c_string(str);

     return symcons(sname, NIL);
}

/**** Initialization code ****/

void create_initial_packages()
{
     interp.control_fields[VMCTRL_PACKAGE_SYSTEM] = packagecons(strcons("system"));
     interp.control_fields[VMCTRL_PACKAGE_SCHEME] = packagecons(strcons("scheme"));
     interp.control_fields[VMCTRL_PACKAGE_KEYWORD] = packagecons(strcons("keyword"));

     interp.fasl_package_list = listn(3,
                                      interp.control_fields[VMCTRL_PACKAGE_SCHEME],
                                      interp.control_fields[VMCTRL_PACKAGE_SYSTEM],
                                      interp.control_fields[VMCTRL_PACKAGE_KEYWORD]);
}

END_NAMESPACE
