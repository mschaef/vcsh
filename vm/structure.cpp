/* structure.cpp
 *
 * SIOD struture code. This contains the implementations for
 * structures
 */

#include "scan.h"

namespace scan {

     /*  REVISIT %structure-become */

     LRef lcopy_structure(LRef st) /*  REVISIT how much of this can be shared with lstructurecons? */
  {
    if (!STRUCTUREP(st))
      return vmerror_wrong_type(1, st);

    LRef new_st = new_cell(TC_STRUCTURE);

    size_t len = STRUCTURE_DIM(st);;

    SET_STRUCTURE_DIM(new_st, len);
    SET_STRUCTURE_LAYOUT(new_st, STRUCTURE_LAYOUT(st));
    SET_STRUCTURE_DATA(new_st, (LRef *)safe_malloc(len * sizeof (LRef)));

    for (size_t ii = 0; ii < len; ii++)
      SET_STRUCTURE_ELEM(new_st, ii, STRUCTURE_ELEM(st, ii));

    return new_st;
  }

  static void validate_structure_layout(size_t slots, LRef layout)
  {
    if (!CONSP(layout))
      vmerror_wrong_type(2, layout);

    size_t len = (size_t)get_c_long(llength(layout));

    if (len != 2)
      vmerror("Malformed structure layout, bad length: ~s", layout);

    LRef slot_layout = CAR(CDR(layout));

    if (get_c_long(llength(slot_layout)) != (long)slots)
      vmerror("Wrong number of slots in structure layout: ~s", lcons(slot_layout, fixcons(slots)));

    for(; CONSP(slot_layout); slot_layout = CDR(slot_layout))
      {
        if (!CONSP(CAR(slot_layout)))
          vmerror("Bad slot layout in structure layout", lcons(slot_layout, layout));

        if (!SYMBOLP(CAR(CAR(slot_layout))))
          vmerror("Missing slot name from structure layout", layout);
      }
  }

  LRef lstructurecons(LRef slots, LRef layout)
  {
    if (!VECTORP(slots))
      vmerror_wrong_type(1, slots);

    size_t len = VECTOR_DIM(slots);

    validate_structure_layout(len, layout);

    LRef st = new_cell(TC_STRUCTURE);

    SET_STRUCTURE_DIM(st, len);
    SET_STRUCTURE_LAYOUT(st, layout);
    SET_STRUCTURE_DATA(st, (LRef *)safe_malloc(len * sizeof (LRef)));

    for (size_t ii = 0; ii < len; ii++)
      SET_STRUCTURE_ELEM(st, ii, VECTOR_ELEM(slots, ii));

    return st;
  }

  LRef lstructurep(LRef st, LRef expected_layout)
  {
    if (!STRUCTUREP(st))
      return boolcons(false);

    if (!NULLP(expected_layout) && (expected_layout != STRUCTURE_LAYOUT(st)))
      return boolcons(false);

    return boolcons(true);
  }

  LRef lstructure_layout(LRef st)
  {
    if (!STRUCTUREP(st))
      return vmerror_wrong_type(1, st);

    return STRUCTURE_LAYOUT(st);
  }

  LRef lstructure_length(LRef st)
  {
    if (!STRUCTUREP(st))
      return vmerror_wrong_type(1, st);

    return fixcons(STRUCTURE_DIM(st));
  }

  LRef lstructure_ref(LRef st, LRef index)
  {
    if (!STRUCTUREP(st))
      vmerror_wrong_type(1, st);

    if (!FIXNUMP(index))
      vmerror_wrong_type(2, index);

    fixnum_t idx = get_c_fixnum(index);

    if ((idx >= 0) && ((size_t)idx < STRUCTURE_DIM(st)))
      return STRUCTURE_ELEM(st, idx);

    return vmerror("Structure index out of bounds.", lcons(st, index));
  }

  LRef lstructure_set(LRef st, LRef index, LRef value)
  {
    if (!STRUCTUREP(st))
      vmerror_wrong_type(1, st);

    if (!FIXNUMP(index))
      vmerror_wrong_type(2, index);

    fixnum_t idx = get_c_fixnum(index);

    if ((idx >= 0) && ((size_t)idx < STRUCTURE_DIM(st)))
      {
        SET_STRUCTURE_ELEM(st, idx, value);

        return st;
      }

    return vmerror("Structure index out of bounds.", lcons(st, index));
  }

  bool structure_equal(LRef sta, LRef stb)
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

} /*  end namespace scan */
