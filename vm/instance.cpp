
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

#include "scan.h"

BEGIN_NAMESPACE(scan)
static void instance_reallocate_in_place(LRef inst, size_t new_size)
{
     LRef *new_data = NULL;

     if (new_size > 0)
          new_data = (LRef *) safe_malloc(new_size * sizeof(LRef *));

     if (INSTANCE_DATA(inst))
     {
          for (size_t ii = 0; ii < MIN2(INSTANCE_DIM(inst), new_size); ii++)
               new_data[ii] = INSTANCE_ELEM(inst, ii);

          safe_free(INSTANCE_DATA(inst));
     }

     SET_INSTANCE_DIM(inst, new_size);
     SET_INSTANCE_DATA(inst, new_data);
}

LRef instancecons(LRef proto)
{
     LRef z = new_cell(TC_INSTANCE);

     SET_INSTANCE_DIM(z, 0);
     SET_INSTANCE_DATA(z, NULL);

     instance_reallocate_in_place(z, 1);

     SET_INSTANCE_PROTO(z, proto);

     SET_INSTANCE_MAP(z, hashcons(true));

     return z;
}

static LRef instance_map(LRef inst)
{
     assert(INSTANCEP(inst));

     assert(INSTANCE_DIM(inst) > 0);

     LRef map = INSTANCE_MAP(inst);

     assert(HASHP(map));

     return map;
}

static bool instance_map_ref(LRef inst, LRef key, LRef & value)
{
     return hash_ref(instance_map(inst), key, &value);
}

static LRef ensure_unique_map(LRef inst)
{
     LRef map = lhash_copy(instance_map(inst));

     SET_INSTANCE_MAP(inst, map);

     return map;
}

static void instance_map_set(LRef inst, LRef key, LRef new_value)
{
     LRef map = ensure_unique_map(inst);

     lhash_set(map, key, new_value);
}

bool instance_equal(LRef a, LRef b)
{
     assert(INSTANCEP(a));
     assert(TYPE(a) == TYPE(b));

     if (INSTANCE_PROTO(a) != INSTANCE_PROTO(b))
          return false;

     if (INSTANCE_DIM(a) != INSTANCE_DIM(b))
          return false;

     LRef slot_name;
     hash_iter_t ii;
     LRef amap = instance_map(a);
     hash_iter_begin(amap, &ii);
     while (hash_iter_next(amap, &ii, &slot_name, NULL))
     {

          if (!TRUEP(lhas_slotp(b, slot_name))
              || !equalp(lislot_ref(a, slot_name), lislot_ref(b, slot_name)))
               return false;
     }

     return true;
}

LRef linstancep(LRef inst)
{
     if (INSTANCEP(inst))
          return inst;
     else
          return boolcons(false);
}

LRef lmake_instance(LRef args)
{
     LRef proto = boolcons(false);

     if (!NULLP(args))
     {
          proto = CAR(args);
          args = CDR(args);
     }

     if (!(FALSEP(proto) || INSTANCEP(proto) || SYMBOLP(proto)))
          vmerror_wrong_type(1, proto);

     LRef new_instance = instancecons(proto);

     if (init_slots(new_instance, args, true))
          vmerror_arg_out_of_range(args, _T("bad instance initialization list"));

     return new_instance;
}

LRef lclone_instance(LRef inst)
{
     if (!INSTANCEP(inst))
          vmerror_wrong_type(1, inst);

     LRef z = new_cell(TC_INSTANCE);

     SET_INSTANCE_DIM(z, 0);
     SET_INSTANCE_DATA(z, NULL);

     instance_reallocate_in_place(z, INSTANCE_DIM(inst));

     SET_INSTANCE_MAP(z, INSTANCE_MAP(inst));

     for (size_t ii = 0; ii < INSTANCE_DIM(inst); ii++)
          SET_INSTANCE_ELEM(z, ii, INSTANCE_ELEM(inst, ii));

     return z;
}

LRef liinstance_map(LRef inst)
{
     if (!INSTANCEP(inst))
          vmerror_wrong_type(1, inst);

     return instance_map(inst);
}

LRef liinstance_proto(LRef inst)
{
     if (!INSTANCEP(inst))
          vmerror_wrong_type(1, inst);

     return INSTANCE_PROTO(inst);
}

LRef liinstance_slots(LRef inst)
{
     if (!INSTANCEP(inst))
          vmerror_wrong_type(1, inst);

     LRef slots = vectorcons(INSTANCE_DIM(inst), NIL);

     for (size_t ii = 0; ii < INSTANCE_DIM(inst); ii++)
          SET_VECTOR_ELEM(slots, ii, INSTANCE_ELEM(inst, ii));

     return slots;
}

LRef liset_instance_proto(LRef inst, LRef new_proto)
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

static bool try_slot_ref(LRef inst, LRef key, LRef * val)
{
     if (!INSTANCEP(inst))
          vmerror_wrong_type(1, inst);

     for (; INSTANCEP(inst); inst = INSTANCE_PROTO(inst))
     {
          LRef slot_map_index;

          if (instance_map_ref(inst, key, slot_map_index))
          {
               assert(FIXNUMP(slot_map_index));

               *val = INSTANCE_ELEM(inst, get_c_long(slot_map_index));

               return true;
          }
     }

     return false;
}

LRef lislot_ref(LRef inst, LRef key)
{
     LRef val = NIL;

     if (!try_slot_ref(inst, key, &val))
          vmerror_arg_out_of_range(lcons(inst, key), _T("slot not found"));

     return val;
}

LRef lhas_slotp(LRef this_inst, LRef key)
{
     if (!INSTANCEP(this_inst))
          vmerror_wrong_type(1, this_inst);

     for (LRef inst = this_inst; INSTANCEP(inst); inst = INSTANCE_PROTO(inst))
     {
          LRef unused;

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

static size_t enrich_instance(LRef inst, LRef key)
{
     size_t new_index = INSTANCE_DIM(inst);

     instance_reallocate_in_place(inst, INSTANCE_DIM(inst) + 1);

     instance_map_set(inst, key, fixcons(new_index));

     return new_index;
}

LRef lislot_set(LRef inst, LRef key, LRef value)
{
     if (!INSTANCEP(inst))
          vmerror_wrong_type(1, inst);

     LRef slot_map_index;
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

bool init_slots(LRef obj, LRef initargs, bool names_must_be_symbols)    /*  REVISIT: should true really mean fail? */
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

          LRef name = CAR(initargs);
          LRef value = CAR(CDR(initargs));

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

static LRef llookup_message_handler(LRef lookup_context, LRef message)
{
     LRef handler = NIL;

     if (try_slot_ref(lookup_context, message, &handler))
          return handler;

     return boolcons(false);
}

static LRef lsend_message(LRef self, LRef lookup_ctx_inst, LRef message_name, LRef args)
{
     if (!INSTANCEP(lookup_ctx_inst))
          vmerror_arg_out_of_range(lookup_ctx_inst, _T("bad message lookup context"));

     if (!SYMBOLP(message_name))
          vmerror_arg_out_of_range(message_name, _T("bad message name"));

     LRef message_handler = llookup_message_handler(lookup_ctx_inst, message_name);

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

     LRef argv[2];
     argv[0] = self;
     argv[1] = args;

     return apply1(message_handler, 2, argv);
}

LRef lsend(LRef args)
{
     if (!CONSP(args))
          vmerror_wrong_type(1, NIL);

     LRef self = CAR(args);

     LRef lookup_ctx = self;

     if (!INSTANCEP(lookup_ctx))
          lookup_ctx = vmtrap(TRAP_PRIMITIVE_INSTANCE, VMT_MANDATORY_TRAP, 1, self);

     args = CDR(args);

     assert(CONSP(args));

     LRef message_name = CAR(args);

     args = CDR(args);

     return lsend_message(self, lookup_ctx, message_name, args);
}


END_NAMESPACE
