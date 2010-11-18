/*
 * The global environment.
 */

#include "scan.h"

BEGIN_NAMESPACE(scan)

static void check_global_environment_size()
{
     if (interp.last_global_env_entry >= VECTOR_DIM(interp.global_env))
          interp.global_env =
              vector_reallocate_in_place(interp.global_env,
                                         VECTOR_DIM(interp.global_env) + GLOBAL_ENV_BLOCK_SIZE,
                                         UNBOUND_MARKER);
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
               var, interp.global_env, VECTOR_ELEM(interp.global_env, 0));

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
     if (!VECTORP(genv))
          vmerror_wrong_type(genv);

     interp.global_env = genv;
     check_global_environment_size();
}

LRef lcall_with_global_environment(LRef fn, LRef new_global_env)
{
     if (!VECTORP(new_global_env))
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
