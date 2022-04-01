/*
 * structure.c --
 *
 * The primitive implementation of structures.
 *
 * (C) Copyright 2001-2022 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "LICENSE" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include "scan-private.h"

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

     if (HASHP(obj)) {
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
     if (HASHP(obj)) {
          lhash_set(obj, slot_name, new_val);
     } else {
          vmerror_wrong_type_n(1, obj);
     }

     return obj;
}
