
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
static void instance_reallocate_in_place(lref_t inst, size_t new_size)
{
     lref_t *new_data = NULL;

     if (new_size > 0)
          new_data = (lref_t *) gc_malloc(new_size * sizeof(lref_t *));

     if (INSTANCE_DATA(inst))
     {
          for (size_t ii = 0; ii < MIN2(INSTANCE_DIM(inst), new_size); ii++)
               new_data[ii] = INSTANCE_ELEM(inst, ii);

          gc_free(INSTANCE_DATA(inst));
     }

     SET_INSTANCE_DIM(inst, new_size);
     SET_INSTANCE_DATA(inst, new_data);
}

lref_t liinstancecons(lref_t proto)
{
     lref_t z = new_cell(TC_INSTANCE);

     SET_INSTANCE_DIM(z, 0);
     SET_INSTANCE_DATA(z, NULL);

     instance_reallocate_in_place(z, 1);

     SET_INSTANCE_PROTO(z, proto);

     SET_INSTANCE_MAP(z, hashcons(true));

     return z;
}

static lref_t instance_map(lref_t inst)
{
     assert(INSTANCEP(inst));

     assert(INSTANCE_DIM(inst) > 0);

     lref_t map = INSTANCE_MAP(inst);

     assert(HASHP(map));

     return map;
}

static bool instance_map_ref(lref_t inst, lref_t key, lref_t & value)
{
     return hash_ref(instance_map(inst), key, &value);
}

static lref_t ensure_unique_map(lref_t inst)
{
     lref_t map = lhash_copy(instance_map(inst));

     SET_INSTANCE_MAP(inst, map);

     return map;
}

static void instance_map_set(lref_t inst, lref_t key, lref_t new_value)
{
     lref_t map = ensure_unique_map(inst);

     lhash_set(map, key, new_value);
}

bool instance_equal(lref_t a, lref_t b)
{
     assert(INSTANCEP(a));
     assert(TYPE(a) == TYPE(b));

     if (INSTANCE_PROTO(a) != INSTANCE_PROTO(b))
          return false;

     if (INSTANCE_DIM(a) != INSTANCE_DIM(b))
          return false;

     lref_t slot_name;
     hash_iter_t ii;
     lref_t amap = instance_map(a);
     hash_iter_begin(amap, &ii);
     while (hash_iter_next(amap, &ii, &slot_name, NULL))
     {

          if (!TRUEP(lhas_slotp(b, slot_name))
              || !equalp(lislot_ref(a, slot_name), lislot_ref(b, slot_name)))
               return false;
     }

     return true;
}

lref_t linstancep(lref_t inst)
{
     if (INSTANCEP(inst))
          return inst;
     else
          return boolcons(false);
}

lref_t lmake_instance(lref_t args)
{
     lref_t proto = boolcons(false);

     if (!NULLP(args))
     {
          proto = CAR(args);
          args = CDR(args);
     }

     if (!(FALSEP(proto) || INSTANCEP(proto) || SYMBOLP(proto)))
          vmerror_wrong_type(1, proto);

     lref_t new_instance = liinstancecons(proto);

     if (init_slots(new_instance, args, true))
          vmerror_arg_out_of_range(args, _T("bad instance initialization list"));

     return new_instance;
}

lref_t lclone_instance(lref_t inst)
{
     if (!INSTANCEP(inst))
          vmerror_wrong_type(1, inst);

     lref_t z = new_cell(TC_INSTANCE);

     SET_INSTANCE_DIM(z, 0);
     SET_INSTANCE_DATA(z, NULL);

     instance_reallocate_in_place(z, INSTANCE_DIM(inst));

     SET_INSTANCE_MAP(z, INSTANCE_MAP(inst));

     for (size_t ii = 0; ii < INSTANCE_DIM(inst); ii++)
          SET_INSTANCE_ELEM(z, ii, INSTANCE_ELEM(inst, ii));

     return z;
}

lref_t liinstance_map(lref_t inst)
{
     if (!INSTANCEP(inst))
          vmerror_wrong_type(1, inst);

     return instance_map(inst);
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

     lref_t slots = vectorcons(INSTANCE_DIM(inst), NIL);

     for (size_t ii = 0; ii < INSTANCE_DIM(inst); ii++)
          SET_VECTOR_ELEM(slots, ii, INSTANCE_ELEM(inst, ii));

     return slots;
}

lref_t liset_instance_proto(lref_t inst, lref_t new_proto)
{
     if (!INSTANCEP(inst))
          vmerror_wrong_type(1, inst);

     if (!(INSTANCEP(new_proto) || SYMBOLP(new_proto)))
          vmerror_wrong_type(1, new_proto);

     if (new_proto != INSTANCE_PROTO(inst))
     {
          ensure_unique_map(inst);

          SET_INSTANCE_PROTO(inst, new_proto);
     }

     return inst;
}

static bool try_slot_ref(lref_t inst, lref_t key, lref_t * val)
{
     if (!INSTANCEP(inst))
          vmerror_wrong_type(1, inst);

     for (; INSTANCEP(inst); inst = INSTANCE_PROTO(inst))
     {
          lref_t slot_map_index;

          if (instance_map_ref(inst, key, slot_map_index))
          {
               assert(FIXNUMP(slot_map_index));

               *val = INSTANCE_ELEM(inst, get_c_long(slot_map_index));

               return true;
          }
     }

     return false;
}

lref_t lislot_ref(lref_t inst, lref_t key)
{
     lref_t val = NIL;

     if (!try_slot_ref(inst, key, &val))
          vmerror_arg_out_of_range(lcons(inst, key), _T("slot not found"));

     return val;
}

lref_t lhas_slotp(lref_t this_inst, lref_t key)
{
     if (!INSTANCEP(this_inst))
          vmerror_wrong_type(1, this_inst);

     for (lref_t inst = this_inst; INSTANCEP(inst); inst = INSTANCE_PROTO(inst))
     {
          lref_t unused;

          if (instance_map_ref(inst, key, unused))
          {
               if (inst == this_inst)
                    return keyword_intern(_T("local"));
               else
                    return keyword_intern(_T("inherited"));
          }
     }

     return boolcons(false);
}

static size_t enrich_instance(lref_t inst, lref_t key)
{
     size_t new_index = INSTANCE_DIM(inst);

     instance_reallocate_in_place(inst, INSTANCE_DIM(inst) + 1);

     instance_map_set(inst, key, fixcons(new_index));

     return new_index;
}

lref_t lislot_set(lref_t inst, lref_t key, lref_t value)
{
     if (!INSTANCEP(inst))
          vmerror_wrong_type(1, inst);

     lref_t slot_map_index;
     size_t index;

     if (instance_map_ref(inst, key, slot_map_index))
     {
          assert(FIXNUMP(slot_map_index));
          index = get_c_long(slot_map_index);
     }
     else
          index = enrich_instance(inst, key);

     SET_INSTANCE_ELEM(inst, index, value);

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

static lref_t llookup_message_handler(lref_t lookup_context, lref_t message)
{
     lref_t handler = NIL;

     if (try_slot_ref(lookup_context, message, &handler))
          return handler;

     return boolcons(false);
}

static lref_t lsend_message(lref_t self, lref_t lookup_ctx_inst, lref_t message_name, lref_t args)
{
     if (!INSTANCEP(lookup_ctx_inst))
          vmerror_arg_out_of_range(lookup_ctx_inst, _T("bad message lookup context"));

     if (!SYMBOLP(message_name))
          vmerror_arg_out_of_range(message_name, _T("bad message name"));

     lref_t message_handler = llookup_message_handler(lookup_ctx_inst, message_name);

     if (!TRUEP(message_handler))
     {
          message_handler = vmtrap(TRAP_MSG_NOT_UNDERSTOOD, VMT_MANDATORY_TRAP,
                                   3, self, lookup_ctx_inst, message_name);

          args = lcons(message_name, args);
     }

     if (!TRUEP(message_handler))
          vmerror_arg_out_of_range(lcons(self, args), _T("message not understood"));

     if (!PROCEDUREP(message_handler))
          vmerror_arg_out_of_range(lcons(lookup_ctx_inst, message_name), _T("bad message name"));

     lref_t argv[2];
     argv[0] = self;
     argv[1] = args;

     return apply1(message_handler, 2, argv);
}

lref_t lsend(lref_t args)
{
     if (!CONSP(args))
          vmerror_wrong_type(1, NIL);

     lref_t self = CAR(args);

     lref_t lookup_ctx = self;

     if (!INSTANCEP(lookup_ctx))
          lookup_ctx = vmtrap(TRAP_PRIMITIVE_INSTANCE, VMT_MANDATORY_TRAP, 1, self);

     args = CDR(args);

     assert(CONSP(args));

     lref_t message_name = CAR(args);

     args = CDR(args);

     return lsend_message(self, lookup_ctx, message_name, args);
}


END_NAMESPACE
