/* list.cpp
 *
 * SIOD list support
 */


#include "scan.h"

BEGIN_NAMESPACE(scan)

/**** Cons Cell */
  LRef lcons(LRef x, LRef y) {
    LRef z = new_cell(TC_CONS);

    SET_CAR(z, x);
    SET_CDR(z, y);

    return z;
  }

  LRef lconsp(LRef x)
  {
    if (CONSP(x))
      return x;
    else
      return boolcons(false);
  }


   LRef lcar(LRef x) {
    if (NULLP(x))
      return NIL;
    else if (CONSP(x))
      return CAR(x);

    return vmerror_wrong_type(1, x);
  };

  LRef lcdr(LRef x) {
    if (NULLP(x))
      return NIL;
    else if (CONSP(x))
      return CDR(x);

    return vmerror_wrong_type(1, x);
  };


  LRef lsetcar(LRef cell, LRef value)
  {
    if (!CONSP (cell))
      vmerror_wrong_type(1, cell);

    SET_CAR(cell, value);
    return value;
  }

  LRef lsetcdr(LRef cell, LRef value)
  {
    if (!CONSP (cell))
      vmerror_wrong_type(1, cell);

    SET_CDR(cell, value);
    return value;
  }

  /**** list library functions */
  LRef lmap1(LRef fcn,LRef in)
  {
    LRef res, obj, l;

    if (NULLP(in))
      return NIL;

    res = obj = lcons(lfuncall1(fcn, lcar(in)), NIL);

    for (l = lcdr(in); CONSP(l); l = CDR(l))
      {
        LRef next_obj = lcons(lfuncall1(fcn, CAR(l)), CDR(obj));

        SET_CDR(obj, next_obj);

        obj = next_obj;
      }

    return res;
  }

  LRef lmap2(LRef fcn, LRef in1, LRef in2)
  {
    LRef res, obj, l1, l2;

    if (NULLP(in1) || NULLP (in2))
      return NIL;

    res = obj = lcons(lfuncall2(fcn, lcar(in1), lcar(in2)), NIL);

    for (l1 = lcdr(in1), l2 = lcdr(in2);
         CONSP(l1) && CONSP(l2);
         l1 = CDR(l1), l2 = CDR(l2))
      {
        LRef next_obj = lcons(lfuncall2(fcn, CAR(l1), CAR(l2)), CDR(obj));

        SET_CDR(obj, next_obj);

        obj = next_obj;
      }

    return res;
  }

  LRef lmap(size_t argc, LRef argv[])
  {
    switch (argc)
      {
      case 2:  return lmap1(argv[0], argv[1]);
      case 3:  return lmap2(argv[0], argv[1], argv[2]);
      default: return vmerror("map case not handled", fixcons(argc));
      }
  }


  LRef lmap_pair1(LRef fcn,LRef in)
  {
    LRef res, obj, l;

    if (NULLP(in))
      return NIL;

    res = obj = lcons(lfuncall1(fcn, in), NIL);

    for (l = lcdr(in); CONSP(l); l = CDR(l))
      {
        LRef next_obj = lcons(lfuncall1(fcn, l), CDR(obj));

        SET_CDR(obj, next_obj);
        obj = next_obj;
      }

    return res;
  }

  LRef lmap_pair2(LRef fcn, LRef in1, LRef in2)
  {
    LRef res, obj, l1, l2;

    if (NULLP (in1) || NULLP (in2))
      return (NIL);

    res = obj = lcons(lfuncall2(fcn, in1, in2), NIL);

    for (l1 = lcdr(in1), l2 = lcdr(in2);
         CONSP(l1) && CONSP(l2);
         l1 = CDR(l1), l2 = CDR(l2))
      {
        LRef next_obj = lcons(lfuncall2(fcn, l1, l2), CDR(obj));

        SET_CDR(obj, next_obj);

        obj = next_obj;
      }

    return (res);
  }

  LRef lmap_pair(size_t argc, LRef argv[])
  {
    switch (argc)
      {
      case 2:  return lmap_pair1(argv[0], argv[1]);
      case 3:  return lmap_pair2(argv[0], argv[1], argv[2]);
      default: return vmerror("map-pair case not handled", fixcons(argc));
      }
  }

  LRef lforeach1(LRef fcn, LRef in)
  {

    if (VECTORP(in))
      {
        for(size_t ii = 0; ii < VECTOR_DIM(in); ii++)
          lfuncall1(fcn, VECTOR_ELEM(in, ii));
      }
    else
      for(LRef l = in; CONSP(l); l = CDR(l))
        lfuncall1 (fcn, CAR (l));

    return boolcons(false);
  }

  LRef lforeach2(LRef fcn, LRef in1, LRef in2)
  {
    LRef l1, l2;

    if (VECTORP(in1) && VECTORP(in2))
      {
        for(size_t ii = 0; (ii < VECTOR_DIM(in1)) && (ii < VECTOR_DIM(in2)); ii++)
          lfuncall2(fcn, VECTOR_ELEM(in1, ii), VECTOR_ELEM(in2, ii));
      }
    else
      for(l1 = in1, l2 = in2; CONSP(l1) && CONSP(l2); l1 = CDR(l1), l2 = CDR(l2))
        lfuncall2(fcn, CAR (l1), CAR(l2));

    return boolcons(false);
  }

  LRef lforeach(size_t argc, LRef argv[])
  {
    switch (argc)
      {
      case 2:  return lforeach1(argv[0], argv[1]);
      case 3:  return lforeach2(argv[0], argv[1], argv[2]);
      default: return vmerror("foreach case not handled", fixcons(argc));
      }
  }

  LRef lass(LRef x, LRef alist, LRef fcn)
  {
    LRef l, tmp;

    for (l = alist; CONSP(l); l = CDR(l))
      {
        tmp = CAR(l);
        if (CONSP(tmp) && TRUEP(lfuncall2(fcn, CAR(tmp), x)))
          return tmp;
      }

    if (NULLP(l))
      return NIL;
    else
      return vmerror("improper list to ass", alist);
  }


  LRef lassq(LRef x, LRef alist)
  {
    LRef l, tmp;

    for (l = alist; CONSP (l); l = CDR (l))
      {
        tmp = CAR (l);

        if (CONSP (tmp) && EQ (CAR (tmp), x))
          return (tmp);
      }

    if (NULLP(l))
      return boolcons(false);

    return vmerror("improper list to assq", alist);
  }

  LRef ldelq(LRef elem, LRef l)
  {
    if (NULLP(l))
      return l;

    STACK_CHECK(&elem);

    if (EQ(elem, lcar(l)))
      return (ldelq(elem, lcdr(l)));
    else {
      lsetcdr(l, ldelq(elem, lcdr(l)));

      return l;
    }
  }

  LRef lassoc(LRef x, LRef alist)
  {
    LRef l, tmp;

    for (l = alist; CONSP (l); l = CDR (l))
      {
        tmp = CAR (l);

        if (CONSP (tmp) && equalp(CAR (tmp), x))
          return (tmp);
      }

    if (NULLP(l))
      return boolcons(false);

    return vmerror("improper list to assoc", alist);
  }

  LRef lassv(LRef x, LRef alist)
  {
    LRef l,tmp;

    for (l = alist; CONSP (l); l = CDR (l))
      {
        tmp = CAR (l);

        if (CONSP (tmp) && TRUEP(leql(CAR (tmp), x)))
          return (tmp);
      }

    if (NULLP(l))
      return boolcons(false);

    return vmerror("improper list to assv", alist);
  }


  LRef llast_pair(LRef xs)
  {
    while (CONSP(xs) && CONSP(lcdr(xs)))
      xs = lcdr(xs);

    return xs;
  }

  LRef llist_copy(LRef xs)
  {
    LRef new_list = NIL;
    LRef list_tail = NIL;
    LRef xs_pos = xs;

    while(CONSP(xs_pos))
      {
        if (NULLP(new_list))
          {
            new_list = lcons(CAR(xs_pos), CDR(xs_pos));
            list_tail = new_list;
          }
        else
          {
            LRef temp_cell = lcons(CAR(xs_pos), CDR(xs_pos));
            SET_CDR(list_tail, temp_cell);
            list_tail = temp_cell;
          }

        xs_pos = CDR(xs_pos);
      }

    if (NULLP(list_tail))
      return xs_pos;

    assert(CONSP(list_tail));
    SET_CDR(list_tail, xs_pos);

    return new_list;
  }

  LRef lappend(size_t argc, LRef argv[])
  {
    LRef result = NIL;     /* The accumulated result list */
    LRef result_bud = NIL; /* The "bud" of the result, new growth. */

    for(size_t ii = 0; ii < argc; ii++)
      {
        LRef next_list = argv[ii];

        if (NULLP(next_list))
          continue;

        if (ii < argc - 1)
          {
            if (!CONSP(next_list))
              vmerror_wrong_type(next_list);

            next_list = llist_copy(next_list);
          }

        if (!NULLP(result))
          {
            if (!(NULLP(result_bud) || CONSP(result_bud)))
              vmerror("Bad args to append: ~s", lista(argc, argv));

            SET_CDR(result_bud, next_list);
          }
        else
          {
            result = next_list;
          }

        result_bud = llast_pair(next_list);
      }

    return result;
  }


  LRef lappendd(size_t argc, LRef argv[])
  {
    LRef result = NIL;     /* The accumulated result list */
    LRef result_bud = NIL; /* The "bud" of the result, new growth. */

    for(size_t ii = 0; ii < argc; ii++)
      {
        if (NULLP(argv[ii]))
          continue;

        if (!CONSP(argv[ii]))
          vmerror_wrong_type(argv[ii]);

        if (!NULLP(result))
          {
            if (!(NULLP(result_bud) || CONSP(result_bud)))
              vmerror("Bad args to append: ~s", lista(argc, argv));

            SET_CDR(result_bud, argv[ii]);
          }
        else
          {
            result = argv[ii];
          }

        result_bud = llast_pair(argv[ii]);
      }

    return result;
  }

     static size_t list_length(LRef xs) /*  REVISIT: extend to tolerate circularity? */
  {
    size_t len = 0;

    while(CONSP(xs))
      {
        xs = CDR(xs);
        len++;
      }

    return len;
  }

     size_t object_length(LRef obj) /*  REVISIT: Is it really necessary to be this generic in C? */
  {
    switch (TYPE(obj)) {
    case TC_NIL:    return 0;
    case TC_CONS:   return list_length(obj);
    case TC_STRING: return STRING_DIM(obj);
    case TC_VECTOR: return VECTOR_DIM(obj);
    case TC_HASH:   return hash_length(obj);
    case TC_PORT:   return port_length(obj);
    default:        return 0;
    }
  }

  LRef llength(LRef obj)
  {
    return fixcons(object_length(obj));
  }

  LRef make_list(size_t dim, LRef initial)
  {
    LRef l = NIL;

    for(size_t n = 0; n < dim; n++)
      l = lcons(initial, l);

    return l;
  }

  LRef lmake_list(LRef d, LRef initial)
  {
    if (!NUMBERP(d))
      return vmerror_wrong_type(1, d);

    fixnum_t dim = get_c_fixnum(d);

    if (dim < 0)
      return vmerror("bad dimension to make-list", d);

    return make_list((size_t)dim, initial);
  }

  LRef listn(long n, ...)
  {
    va_list args;

    va_start(args, n);

    LRef result = listv(n, args);

    va_end(args);

    return result;
  }

  LRef listv(long n, va_list args)
  {
    LRef result, obj;
    long jj;

    for(jj = 0, result = NIL; jj < n; ++jj)
      result = lcons(NIL, result);

    for(jj = 0, obj = result; jj < n; obj = lcdr(obj), ++jj)
      lsetcar(obj, va_arg(args, LRef));

    return result;
  }

  LRef lista(size_t n, LRef args[])
  {
    LRef result, obj;
    size_t jj;

    for(jj = 0, result = NIL; jj < n; ++jj)
      result = lcons(NIL, result);

    for(jj = 0, obj = result; jj < n; obj = lcdr(obj), ++jj)
      lsetcar(obj, args[jj]);

    return result;
  }

END_NAMESPACE
