/* instance.cpp
 *
 * A simple prototype based object system. All slot references
 * on an instance search up the prototype chain to find their
 * referent. Slot sets always take place in the instance for
 * which they were called.  The proto fields an have one of
 * several possible values:
 *
 * #f - Signals a base instance
 * symbol - A reference to a symbol whose value is the prototype
 * hash - A litereal prototype instance.
 */

#include "scan.h"

namespace scan {

  static void instance_reallocate_in_place(LRef inst, size_t new_size)
  {
    LRef *new_data = NULL;

    if (new_size > 0)
      new_data = (LRef *)safe_malloc(new_size * sizeof(LRef *));

    if (INSTANCE_DATA(inst))
      {
        for(size_t ii = 0; ii < MIN2(INSTANCE_DIM(inst), new_size); ii++)
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

     static bool instance_map_ref(LRef inst, LRef key, LRef &value) /*  TODO: ref->value */
  {
    return hash_ref(instance_map(inst), key, value);
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

  /* 'equal?' support */

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
    while(hash_iter_next(amap, &ii, &slot_name, NULL))
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
      vmerror("expected instance, symbol, or #f for proto", proto);

    LRef new_instance = instancecons(proto);

    if (init_slots(new_instance, args, true))
      vmerror("Error initializing instance, malformed initialization list", args);

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

    for(size_t ii = 0; ii < INSTANCE_DIM(inst); ii++)
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

    for(size_t ii = 0; ii < INSTANCE_DIM(inst); ii++)
      SET_VECTOR_ELEM(slots, ii, INSTANCE_ELEM(inst, ii));

    return slots;
  }

  LRef liset_instance_proto(LRef inst, LRef new_proto)
  {
    if (!INSTANCEP(inst))
      vmerror_wrong_type(1, inst );

    if (!(INSTANCEP(new_proto) || SYMBOLP(new_proto)))
      vmerror_wrong_type(1, new_proto);

    if (new_proto != INSTANCE_PROTO(inst))
      {
        ensure_unique_map(inst);

        SET_INSTANCE_PROTO(inst, new_proto);
      }

    return inst;
  }

  static bool try_slot_ref (LRef inst, LRef key, LRef *val)
  {
    if (!INSTANCEP(inst))
      vmerror_wrong_type(1, inst);

    for(;;) {
      if (!INSTANCEP(inst))
        break;

      LRef slot_map_index;

      if (instance_map_ref(inst, key, slot_map_index))
        {
          assert(FIXNUMP(slot_map_index));

          *val = INSTANCE_ELEM(inst, get_c_long(slot_map_index));

          return true;
        }

      inst = INSTANCE_PROTO(inst);

      if (FALSEP(inst) || NULLP(inst))
        break;

      if (SYMBOLP(inst))
        inst = lsymbol_value(inst, NIL, NIL);

      /* If our prototype is not an instance, we abort the search and
       * don't find the slot, rather than fail with an error. This is to
       * ensure that instances remain accessible even if they have prototype
       * symbols erroneously rebound to non-instances. This should ideally
       * be caught by some kind of user space 'helper' facility that watches
       * symbol redefinitions for problems.
       */
      if (!(INSTANCEP(inst) || NULLP(inst)))
        break;
    }

    return false;
  }

  LRef lislot_ref (LRef inst, LRef key)
  {
    LRef val = NIL;

    if (!try_slot_ref(inst, key, &val))
      vmerror("Slot not found in instance. ~s", lcons(inst, key));

    return val;
  }

  LRef lhas_slotp(LRef this_inst, LRef key)
  {
    if (!INSTANCEP(this_inst))
      vmerror_wrong_type(1, this_inst);

    for(LRef inst = this_inst;;) {
      if (!INSTANCEP(inst))
        break;

      LRef unused;

      if (instance_map_ref(inst, key, unused))
        {
          if (inst == this_inst)
            return keyword_intern(_T("local"));
          else
            return keyword_intern(_T("inherited"));
        }

      inst = INSTANCE_PROTO(inst);

      if (FALSEP(inst) || NULLP(inst))
        break;

      if (SYMBOLP(inst))
        inst = lsymbol_value(inst, NIL, NIL);

      /* If our prototype is not an instance, we abort the search and
       * don't find the slot, rather than fail with an error. This is to
       * ensure that instances remain accessible even if they have prototype
       * symbols erroneously rebound to non-instances. This should ideally
       * be caught by some kind of user space 'helper' facility that watches
       * symbol redefinitions for problems.
       */
      if (!(INSTANCEP(inst) || NULLP(inst)))
        break;
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

  LRef lislot_set (LRef inst, LRef key, LRef value)
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

    SET_INSTANCE_ELEM(inst,index, value);

    return inst;
  }

     bool init_slots(LRef obj, LRef initargs, bool names_must_be_symbols) /*  REVISIT: should true really mean fail? */
  {
    /* initargs takes the form of a property list:
     *
     * ( name1 value1 name2 value2 ...)
     */
    while(!NULLP(initargs))
      {
        if (!CONSP(initargs))
          return true;

        if (!CONSP(CDR(initargs)))
	  return true;

        LRef name  = CAR(initargs);
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
      vmerror("Invalid message lookup context: ~s", lookup_ctx_inst);

    if (!SYMBOLP(message_name))
      vmerror("Invalid message: ~s", message_name);

    LRef message_handler = llookup_message_handler(lookup_ctx_inst, message_name);

    if (!TRUEP(message_handler))
      {
        message_handler = llookup_message_handler(lookup_ctx_inst, interp.sym_do_not_understand);

        args = lcons(message_name, args);
      }

    if (!TRUEP(message_handler))
      vmerror("Message not understood. (instance . message) = ~s", lcons(self, args));

    if (!PROCEDUREP(message_handler))
      vmerror("Improper message handler in instance.", lcons(lookup_ctx_inst, message_name));

    return napply(message_handler, 2, self, args);
  }

  LRef lsend(LRef args)
  {
    if (NULLP(args))
      vmerror("insufficient arguments", NIL);

    assert(CONSP(args));

    LRef self = CAR(args);

    LRef lookup_ctx = self;

    if (!INSTANCEP(lookup_ctx))
      lookup_ctx = SYMBOL_VCELL(lrepresentation_of(self));

    args = CDR(args);

    assert(CONSP(args));

    LRef message_name = CAR(args);

    args = CDR(args);

    return lsend_message(self, lookup_ctx, message_name, args);
  }


} /*  end namespace scan */
