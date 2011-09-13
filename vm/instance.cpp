
/*
 * instance.cpp --
 *
 * A simple prototype based object system. This should be considered
 * to be experimental.
 *
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include "scan-private.h"

BEGIN_NAMESPACE(scan)

lref_t liinstancecons(lref_t proto)
{
     if (!(INSTANCEP(proto) || SYMBOLP(proto) || FALSEP(proto)))
          vmerror_wrong_type(1, proto);

     lref_t new_instance = new_cell(TC_INSTANCE);

     SET_INSTANCE_PROTO(new_instance, proto);
     SET_INSTANCE_SLOTS(new_instance, hashcons(true));

     return new_instance;
}

bool instance_equal(lref_t a, lref_t b)
{
     assert(INSTANCEP(a));
     assert(TYPE(a) == TYPE(b));

     if (INSTANCE_PROTO(a) != INSTANCE_PROTO(b))
          return false;

     if (!equalp(INSTANCE_SLOTS(a), INSTANCE_SLOTS(b)))
          return false;

     return true;
}

lref_t linstancep(lref_t inst)
{
     if (INSTANCEP(inst))
          return inst;
     else
          return boolcons(false);
}

lref_t liinstance_proto(lref_t inst)
{
     if (!INSTANCEP(inst))
          vmerror_wrong_type(1, inst);

     return INSTANCE_PROTO(inst);
}

lref_t liinstance_slots(lref_t inst)
{
     if (!INSTANCEP(inst))
          vmerror_wrong_type(1, inst);

     return INSTANCE_SLOTS(inst);
}

lref_t liset_instance_proto(lref_t inst, lref_t new_proto)
{
     if (!INSTANCEP(inst))
          vmerror_wrong_type(1, inst);

     if (!(INSTANCEP(new_proto) || SYMBOLP(new_proto)))
          vmerror_wrong_type(1, new_proto);

     SET_INSTANCE_PROTO(inst, new_proto);

     return inst;
}

lref_t lislot_ref(lref_t inst, lref_t key)
{
     if (!INSTANCEP(inst))
          vmerror_wrong_type(1, inst);

     lref_t val = NIL;

     if (!hash_ref(INSTANCE_SLOTS(inst), key, &val))
          return boolcons(false);

     return val;
}

lref_t lhas_slotp(lref_t this_inst, lref_t key)
{
     if (!INSTANCEP(this_inst))
          vmerror_wrong_type(1, this_inst);

     lref_t val = NIL;

     bool has_slot = hash_ref(INSTANCE_SLOTS(this_inst), key, &val);

     return boolcons(has_slot);
}

lref_t lislot_set(lref_t inst, lref_t key, lref_t value)
{
     if (!INSTANCEP(inst))
          vmerror_wrong_type(1, inst);

     lhash_set(INSTANCE_SLOTS(inst), key, value);

     return inst;
}

bool init_slots(lref_t obj, lref_t initargs, bool names_must_be_symbols)    /*  REVISIT: should true really mean fail? */
{
     /* initargs takes the form of a property list:
      *
      * ( name1 value1 name2 value2 ...)
      */
     while (!NULLP(initargs))
     {
          if (!CONSP(initargs))
               return true;

          if (!CONSP(CDR(initargs)))
               return true;

          lref_t name = CAR(initargs);
          lref_t value = CAR(CDR(initargs));

          if (names_must_be_symbols && !SYMBOLP(name))
               return true;

          if (INSTANCEP(obj))
               lislot_set(obj, name, value);
          else if (HASHP(obj))
               lhash_set(obj, name, value);
          else
               vmerror_wrong_type(1, obj);

          initargs = CDR(CDR(initargs));
     }

     return false;
}

END_NAMESPACE
