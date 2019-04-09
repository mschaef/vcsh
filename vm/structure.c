/*
 * structure.c --
 *
 * The primitive implementation of structures.
 *
 * (C) Copyright 2001-2014 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include "scan-private.h"

lref_t lmake_slayout(lref_t name, lref_t slots)
{
     lref_t sl = new_cell(TC_SLAYOUT);

     SET_SLAYOUT_NAME(sl, name);
     SET_SLAYOUT_SLOTS(sl, slots);
     
     return sl;
}

lref_t lslayoutp(lref_t obj)
{
     if (SLAYOUTP(obj))
          return obj;

     return boolcons(false);
}

lref_t lset_slayout_name(lref_t sl, lref_t name)
{
     if (!SLAYOUTP(sl))
          vmerror_wrong_type_n(1, sl);

     SET_SLAYOUT_NAME(sl, name);
     
     return sl;
}

lref_t lslayout_name(lref_t sl)
{
     if (!SLAYOUTP(sl))
          vmerror_wrong_type_n(1, sl);

     return SLAYOUT_NAME(sl);
}

lref_t lslayout_slots(lref_t sl)
{
     if (!SLAYOUTP(sl))
          vmerror_wrong_type_n(1, sl);

     return SLAYOUT_SLOTS(sl);
}

lref_t lstructurecons(lref_t slots, lref_t layout)
{
     if (!VECTORP(slots))
          vmerror_wrong_type_n(1, slots);

     size_t len = slots->as.vector.dim;

     lref_t st = new_cell(TC_STRUCTURE);

     SET_STRUCTURE_DIM(st, len);
     SET_STRUCTURE_LAYOUT(st, layout);
     SET_STRUCTURE_DATA(st, (lref_t *) gc_malloc(len * sizeof(lref_t)));

     for (size_t ii = 0; ii < len; ii++)
          SET_STRUCTURE_ELEM(st, ii, slots->as.vector.data[ii]);

     return st;
}

lref_t lcopy_structure(lref_t st, lref_t expected_layout)
{
     if (!STRUCTUREP(st))
          vmerror_wrong_type_n(1, st);

     if (!NULLP(expected_layout) && (expected_layout != STRUCTURE_LAYOUT(st)))
          vmerror_wrong_type_n(1, st);

     lref_t new_st = new_cell(TC_STRUCTURE);

     size_t len = STRUCTURE_DIM(st);;

     SET_STRUCTURE_DIM(new_st, len);
     SET_STRUCTURE_LAYOUT(new_st, STRUCTURE_LAYOUT(st));
     SET_STRUCTURE_DATA(new_st, (lref_t *) gc_malloc(len * sizeof(lref_t)));

     for (size_t ii = 0; ii < len; ii++)
          SET_STRUCTURE_ELEM(new_st, ii, STRUCTURE_ELEM(st, ii));

     return new_st;
}

lref_t lstructurep(lref_t st, lref_t expected_layout)
{
     if (!STRUCTUREP(st))
          return boolcons(false);

     if (!NULLP(expected_layout) && (expected_layout != STRUCTURE_LAYOUT(st)))
          return boolcons(false);

     return boolcons(true);
}

lref_t lstructure_layout(lref_t st)
{
     if (!STRUCTUREP(st))
          vmerror_wrong_type_n(1, st);

     return STRUCTURE_LAYOUT(st);
}

lref_t lstructure_length(lref_t st)
{
     if (!STRUCTUREP(st))
           vmerror_wrong_type_n(1, st);

     return fixcons(STRUCTURE_DIM(st));
}

lref_t lstructure_ref(lref_t st, lref_t index, lref_t expected_layout)
{
     if (!STRUCTUREP(st))
          vmerror_wrong_type_n(1, st);

     if (!NULLP(expected_layout) && (expected_layout != STRUCTURE_LAYOUT(st)))
          vmerror_wrong_type_n(1, st);
     
     if (!FIXNUMP(index))
          vmerror_wrong_type_n(2, index);

     fixnum_t idx = get_c_fixnum(index);

     if ((idx >= 0) && ((size_t) idx < STRUCTURE_DIM(st)))
          return STRUCTURE_ELEM(st, idx);

     vmerror_index_out_of_bounds(index, st);

     return NIL; // unreached
}

lref_t lstructure_set(lref_t st, lref_t index, lref_t value, lref_t expected_layout)
{
     if (!STRUCTUREP(st))
          vmerror_wrong_type_n(1, st);

     if (!NULLP(expected_layout) && (expected_layout != STRUCTURE_LAYOUT(st)))
          vmerror_wrong_type_n(1, st);

     if (!FIXNUMP(index))
          vmerror_wrong_type_n(2, index);

     fixnum_t idx = get_c_fixnum(index);

     if ((idx >= 0) && ((size_t) idx < STRUCTURE_DIM(st)))
     {
          SET_STRUCTURE_ELEM(st, idx, value);

          return st;
     }

     vmerror_index_out_of_bounds(index, st);

     return NIL;
}

bool structure_equal(lref_t sta, lref_t stb)
{
     assert(STRUCTUREP(sta));
     assert(STRUCTUREP(stb));

     if (STRUCTURE_LAYOUT(sta) != STRUCTURE_LAYOUT(stb))
          return false;

     if (STRUCTURE_DIM(sta) != STRUCTURE_DIM(stb))
          return false;

     for (size_t ii = 0; ii < STRUCTURE_DIM(sta); ii++)
          if (!equalp(STRUCTURE_ELEM(sta, ii), STRUCTURE_ELEM(stb, ii)))
               return false;

     return true;
}

/// Slots

lref_t lslot_ref(size_t argc, lref_t argv[]) {

     lref_t obj = NIL;
     lref_t slot_name = NIL;
     lref_t default_val = boolcons(false);
     lref_t val;

     if (argc > 0)
          obj = argv[0];

     if (argc > 1)
          slot_name = argv[1];

     if (argc > 2)
          default_val = argv[2];
          
     if (STRUCTUREP(obj)) {
          if (hash_ref(CDR(STRUCTURE_LAYOUT(obj)), slot_name, &val)) {
               return STRUCTURE_ELEM(obj, FIXNM(val));
          } else if (argc > 2) {
               return default_val;
          } else {
               vmerror_index_out_of_bounds(slot_name, obj);                    
          }          
     } else if (HASHP(obj)) {
          if (hash_ref(obj, slot_name, &val)) {
               return val;
          } else {
               return default_val;
          }
     } else {
          vmerror_wrong_type_n(1, obj);          
     }
}

lref_t lslot_set(lref_t obj, lref_t slot_name, lref_t new_val) {
     if (STRUCTUREP(obj)) {
          lref_t idx;

          if (hash_ref(CDR(STRUCTURE_LAYOUT(obj)), slot_name, &idx)) {
               SET_STRUCTURE_ELEM(obj, FIXNM(idx), new_val);
          } else {
               vmerror_index_out_of_bounds(slot_name, obj);
          }
     } else if (HASHP(obj)) {
          lhash_set(obj, slot_name, new_val);
     } else {
          vmerror_wrong_type_n(1, obj);
     }

     return obj;
}

