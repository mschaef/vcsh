/*
 * list.c --
 *
 * Internal list primitives.
 *
 * (C) Copyright 2001-2022 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "LICENSE" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include "scan-private.h"

lref_t lcons(lref_t x, lref_t y)
{
     lref_t z = new_cell(TC_CONS);

     SET_CAR(z, x);
     SET_CDR(z, y);

     return z;
}

lref_t lconsp(lref_t x)
{
     if (CONSP(x))
          return x;
     else
          return boolcons(false);
}


lref_t lcar(lref_t x)
{
     if (NULLP(x))
          return NIL;

     if (!CONSP(x))
          vmerror_wrong_type_n(1, x);

     return CAR(x);
};

lref_t lcdr(lref_t x)
{
     if (NULLP(x))
          return NIL;

     if (!CONSP(x))
          vmerror_wrong_type_n(1, x);

     return CDR(x);
};

lref_t lcdrs(lref_t x)
{
     if (NULLP(x) || !CONSP(x))
          return NIL;

     return CDR(x);
};

lref_t lsetcar(lref_t cell, lref_t value)
{
     if (!CONSP(cell))
          vmerror_wrong_type_n(1, cell);

     SET_CAR(cell, value);
     return value;
}

lref_t lsetcdr(lref_t cell, lref_t value)
{
     if (!CONSP(cell))
          vmerror_wrong_type_n(1, cell);

     SET_CDR(cell, value);
     return value;
}


static size_t list_length(lref_t xs)
{
     size_t len = 0;

     while (CONSP(xs))
     {
          xs = CDR(xs);
          len++;
     }

     return len;
}

size_t object_length(lref_t obj)  /*  REVISIT: Is it really necessary to be this generic in C? */
{
     switch (TYPE(obj))
     {
     case TC_NIL:
          return 0;
     case TC_CONS:
          return list_length(obj);
     case TC_STRING:
          return obj->as.string.dim;
     case TC_VECTOR:
          return obj->as.vector.dim;
     case TC_HASH:
          return hash_length(obj);
     case TC_PORT:
          return port_length(obj);
     default:
          return 0;
     }
}

lref_t llength(lref_t obj)
{
     return fixcons(object_length(obj));
}

