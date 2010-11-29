/*
 * The global environment.
 */

#include "scan.h"

BEGIN_NAMESPACE(scan)

LRef genvcons(size_t dim, LRef name)
{
     LRef genv = new_cell(TC_GENV);

     SET_GENV_DIM(genv, dim);
     SET_GENV_DATA(genv, (LRef *) safe_malloc(dim * sizeof(LRef)));
     SET_GENV_NAME(genv, name);

     for (size_t ii = 0; ii < dim; ii++)
          SET_GENV_ELEM(genv, ii, UNBOUND_MARKER);

     return genv;
}

LRef lcopy_global_environment(LRef genv, LRef name)
{
     if (!GENVP(genv))
          vmerror_wrong_type(1, genv);

     LRef new_genv = genvcons(GENV_DIM(genv), name);

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

LRef lglobal_environment_name(LRef genv)
{
     if (!GENVP(genv))
          vmerror_wrong_type(1, genv);
     
     return GENV_NAME(genv);
}

LRef lglobal_environment_ref(LRef genv, LRef i)
{
     if (!GENVP(genv))
          vmerror_wrong_type(1, genv);
     
     size_t index = get_c_fixnum(i);

     if ((index >= 0) && (index < GENV_DIM(genv)))
          return GENV_ELEM(genv, index);

     vmerror_index_out_of_bounds(i, genv);

     return NIL;
}

LRef lglobal_environment_set(LRef genv, LRef i, LRef v)
{
     if (!GENVP(genv))
          vmerror_wrong_type(1, genv);

     size_t index = get_c_fixnum(i);

     if ((index >= 0) && (index < GENV_DIM(genv)))
     {
          SET_GENV_ELEM(genv, index, v);
          return genv;
     }

     vmerror_index_out_of_bounds(i, genv);

     return NIL; // unreached
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
     assert(GENVP(interp.global_env));

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
               _T("; DEBUG: globally defining ~a in ~s\n"),
               var,interp.global_env);

     if (SYMBOL_INDEX(var) == 0)
          extend_global_environment(var);

     SET_SYMBOL_VCELL(var, val);

     vmtrap(TRAP_DEFINE, VMT_OPTIONAL_TRAP, 2, var, val);

     interp.global_env = old_genv;

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

END_NAMESPACE
