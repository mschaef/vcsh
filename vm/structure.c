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

lref_t lcopy_structure(lref_t st)
{
     if (!STRUCTUREP(st))
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

static void validate_structure_layout(size_t slots, lref_t layout)
{
     if (!CONSP(layout))
          vmerror_wrong_type_n(2, layout);

     size_t len = (size_t) get_c_long(llength(layout));

     if (len != 2)
          vmerror_arg_out_of_range(layout, _T("bad structure layout, length<>2"));

     lref_t slot_layout = CAR(CDR(layout));

     if (get_c_long(llength(slot_layout)) != (long) slots)
          vmerror_arg_out_of_range(lcons(slot_layout, fixcons(slots)),
                                   _T("bad structure layout, wrong number of slots"));

     for (; CONSP(slot_layout); slot_layout = CDR(slot_layout))
     {
          if (!CONSP(CAR(slot_layout)))
               vmerror_arg_out_of_range(lcons(slot_layout, layout),
                                        _T("bad structure layout, bad slot layout"));

          if (!SYMBOLP(CAR(CAR(slot_layout))))
               vmerror_arg_out_of_range(layout,
                                        _T("bad structure layout, missing slot name"));
     }
}

lref_t lstructurecons(lref_t slots, lref_t layout)
{
     if (!VECTORP(slots))
          vmerror_wrong_type_n(1, slots);

     size_t len = slots->as.vector.dim;

     validate_structure_layout(len, layout);

     lref_t st = new_cell(TC_STRUCTURE);

     SET_STRUCTURE_DIM(st, len);
     SET_STRUCTURE_LAYOUT(st, layout);
     SET_STRUCTURE_DATA(st, (lref_t *) gc_malloc(len * sizeof(lref_t)));

     for (size_t ii = 0; ii < len; ii++)
          SET_STRUCTURE_ELEM(st, ii, slots->as.vector.data[ii]);

     return st;
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

lref_t lstructure_ref(lref_t st, lref_t index)
{
     if (!STRUCTUREP(st))
          vmerror_wrong_type_n(1, st);

     if (!FIXNUMP(index))
          vmerror_wrong_type_n(2, index);

     fixnum_t idx = get_c_fixnum(index);

     if ((idx >= 0) && ((size_t) idx < STRUCTURE_DIM(st)))
          return STRUCTURE_ELEM(st, idx);

     vmerror_index_out_of_bounds(index, st);

     return NIL; // unreached
}

lref_t lstructure_set(lref_t st, lref_t index, lref_t value)
{
     if (!STRUCTUREP(st))
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

