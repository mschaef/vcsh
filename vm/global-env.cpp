/*
 * The global environment.
 */

#include "scan.h"

BEGIN_NAMESPACE(scan)

LRef genvcons(size_t dim /* = GLOBAL_ENV_BLOCK_SIZE */)
{
     LRef genv = new_cell(TC_GENV);

     SET_GENV_DIM(genv, dim);
     SET_GENV_DATA(genv, (LRef *) safe_malloc(dim * sizeof(LRef)));

     for (fixnum_t ii = 0; ii < GLOBAL_ENV_BLOCK_SIZE; ii++)
          SET_GENV_ELEM(genv, ii, UNBOUND_MARKER);

     return genv;
}

LRef lcopy_global_environment(LRef genv)
{
     if (!GENVP(genv))
          vmerror_wrong_type(1, genv);

     LRef new_genv = genvcons(GENV_DIM(genv));

     for (size_t ii = 0; ii < GENV_DIM(genv); ii++)
          SET_GENV_ELEM(new_genv, ii, GENV_ELEM(genv, ii));

     return new_genv;
}

LRef lglobal_environmentp(LRef genv)
{
     if (GENVP(genv))
          return genv;
     else
          return boolcons(false);
}

void genv_enlarge(LRef genv, size_t new_size)
{
     assert(GENVP(genv));
     assert(new_size >= GENV_DIM(genv));

     if (new_size == GENV_DIM(genv))
         return;

     LRef *old_genv_data = GENV_DATA(genv);
     LRef *new_genv_data = (LRef *) safe_malloc(new_size * sizeof(LRef));

     for (size_t ii = 0; ii < new_size; ii++)
          new_genv_data[ii] = (ii < GENV_DIM(genv)) ? GENV_ELEM(genv, ii) : UNBOUND_MARKER;

     safe_free(old_genv_data);

     SET_GENV_DIM(genv, new_size);
     SET_GENV_DATA(genv, new_genv_data);
}

void check_global_environment_size()
{
     if (interp.last_global_env_entry < GENV_DIM(interp.global_env))
          return;
     
     genv_enlarge(interp.global_env, GENV_DIM(interp.global_env) + GLOBAL_ENV_BLOCK_SIZE);
}

static void extend_global_environment(LRef sym)
{
     assert(SYMBOLP(sym));
     assert(SYMBOL_INDEX(sym) == 0);

     interp.last_global_env_entry++;

     check_global_environment_size();

     SET_SYMBOL_INDEX(sym, interp.last_global_env_entry);
}

LRef lidefine_global(LRef var, LRef val, LRef genv)
{
     assert(SYMBOLP(var));

     LRef old_genv = interp.global_env;

     if (TRUEP(genv) && !NULLP(genv))
          set_global_env(genv);

     dscwritef(DF_SHOW_GLOBAL_DEFINES,
               _T("; DEBUG: globally defining ~a in ~c& ~s\n"),
               var, interp.global_env, GENV_ELEM(interp.global_env, 0));

     if (SYMBOL_INDEX(var) == 0)
          extend_global_environment(var);

     SET_SYMBOL_VCELL(var, val);

     vmtrap(TRAP_DEFINE, VMT_OPTIONAL_TRAP, 2, var, val);

     interp.global_env = old_genv;

     return val;
}

LRef lunbind_symbol(LRef var)
{
     if (!SYMBOLP(var))
          vmerror_wrong_type(1, var);

     SET_SYMBOL_VCELL(var, UNBOUND_MARKER);

     return NIL;
}

LRef lsetvar(LRef var, LRef val, LRef lenv, LRef genv)
{
     LRef tmp;

     if (!SYMBOLP(var))
          vmerror_wrong_type(1, var);

     tmp = lenvlookup(var, lenv);

     if (NULLP(tmp))
     {
          LRef old_genv = interp.global_env;

          if (TRUEP(genv) && !NULLP(genv))
               set_global_env(genv);

          if (UNBOUND_MARKER_P(SYMBOL_VCELL(var)))
               vmerror_unbound(var);

          if (SYMBOL_HOME(var) == interp.keyword_package)
               vmerror_arg_out_of_range(var, _T("cannot rebind keywords"));

          SET_SYMBOL_VCELL(var, val);

          interp.global_env = old_genv;

          return val;
     }

     SET_CAR(tmp, val);
     return val;
}

LRef lcurrent_global_environment()
{
     return interp.global_env;
}

void set_global_env(LRef genv)
{
     if (!GENVP(genv))
          vmerror_wrong_type(genv);

     interp.global_env = genv;
     check_global_environment_size();
}

LRef lcall_with_global_environment(LRef fn, LRef new_global_env)
{
     if (!GENVP(new_global_env))
          vmerror_wrong_type(new_global_env);

     LRef old_global_env = interp.global_env;
     LRef retval = NIL;

     ENTER_UNWIND_PROTECT()
     {
          interp.global_env = new_global_env;

          check_global_environment_size();

          retval = apply1(fn, 0, NULL);
     }
     ON_UNWIND()
     {
          interp.global_env = old_global_env;
     }
     LEAVE_UNWIND_PROTECT();

     return retval;
}

END_NAMESPACE
