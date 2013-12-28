
/*
 * hash-table.cpp --
 *
 * Scheme-visible hash tables.
 *
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include "scan-private.h"

/*  REVISIT: add explicit 'no value' hash value to allow keys to be members without values. (a way to use hashes as sets) */
INLINE fixnum_t HASH_COMBINE(fixnum_t _h1, fixnum_t _h2)
{
     return (_h1 * 17 + 1) ^ _h2;
}

INLINE void SET_HASH_SHALLOW(lref_t hash, bool shallow_keys)
{
     assert(HASHP(hash));

     hash->storage_as.hash.info.shallow_keys = shallow_keys;
}

INLINE bool HASH_SHALLOW(lref_t hash)
{
     assert(HASHP(hash));

     return hash->storage_as.hash.info.shallow_keys;
}

INLINE void SET_HASH_COUNT(lref_t hash, unsigned int count)
{
     assert(HASHP(hash));

     hash->storage_as.hash.info.count = count;
}

INLINE unsigned int HASH_COUNT(lref_t hash)
{
     assert(HASHP(hash));

     return hash->storage_as.hash.info.count;
}

static void init_hash_entry(hash_entry_t * entry)
{
     entry->key = UNBOUND_MARKER;
     entry->val = UNBOUND_MARKER;
}

static void delete_hash_entry(hash_entry_t * entry)
{
     entry->key = UNBOUND_MARKER;
     entry->val = NULL;
}

static bool hash_entry_used_p(hash_entry_t * entry)
{
     return !UNBOUND_MARKER_P(entry->key);
}

static bool hash_entry_unused_p(hash_entry_t * entry)
{
     return UNBOUND_MARKER_P(entry->key);
}

static bool hash_entry_deleted_p(hash_entry_t * entry)
{
     return UNBOUND_MARKER_P(entry->key) && NULLP(entry->val);
}

void hash_iter_begin(lref_t hash, hash_iter_t * iter)
{
     assert(HASHP(hash));

     *iter = 0;
}

bool hash_iter_next(lref_t hash, hash_iter_t * iter, lref_t * key, lref_t * val)
{
     assert(HASHP(hash));

     while (*iter < HASH_SIZE(hash))
     {
          if (hash_entry_used_p(&HASH_DATA(hash)[*iter]))
          {
               if (key)
                    *key = HASH_DATA(hash)[*iter].key;

               if (val)
                    *val = HASH_DATA(hash)[*iter].val;

               *iter = *iter + 1;

               return true;
          }

          *iter = *iter + 1;
     }

     return false;
}

fixnum_t sxhash_eq(lref_t obj)
{
     /* Slice off the tag bits, assuming that hashes will be mostly
      * homogeneous. */

     if (LREF1_TAG(obj) == LREF1_SPECIAL)
          return ((uintptr_t) obj) >> LREF2_TAG_SHIFT;
     else
          return ((uintptr_t) obj) >> LREF1_TAG_SHIFT;
}

fixnum_t sxhash(lref_t obj)
{
     STACK_CHECK(&obj);

     fixnum_t hash = 0;

     if (NULLP(obj))
          return 0;

     lref_t tmp;
     size_t ii;

     switch (TYPE(obj))
     {
     case TC_CONS:
          hash = sxhash(CAR(obj));

          for (tmp = CDR(obj); CONSP(tmp); tmp = CDR(tmp))
               hash = HASH_COMBINE(hash, sxhash(CAR(tmp)));

          hash = HASH_COMBINE(hash, sxhash(tmp));
          break;

     case TC_FIXNUM:
          hash = get_c_fixnum(obj);
          break;

     case TC_FLONUM:
          hash = get_c_fixnum(obj);
          break;

     case TC_SYMBOL:
          hash = sxhash(SYMBOL_PNAME(obj));
          break;

     case TC_SUBR:
          hash = (fixnum_t)SUBR_CODE(obj);
          break;

     case TC_STRING:
          for (ii = 0; ii < STRING_DIM(obj); ii++)
               hash = (hash << 5) - hash + STRING_DATA(obj)[ii];
          break;

     case TC_VECTOR:
          for (ii = 0; ii < VECTOR_DIM(obj); ii++)
               hash = HASH_COMBINE(hash, sxhash(VECTOR_ELEM(obj, ii)));
          break;

     case TC_STRUCTURE:
          hash = HASH_COMBINE(hash, sxhash(STRUCTURE_LAYOUT(obj)));

          for (ii = 0; ii < STRUCTURE_DIM(obj); ++ii)
               hash = HASH_COMBINE(hash, sxhash(STRUCTURE_ELEM(obj, ii)));
          break;

     case TC_INSTANCE:
          hash = HASH_COMBINE(hash, sxhash(INSTANCE_PROTO(obj)));
          hash = HASH_COMBINE(hash, sxhash(INSTANCE_SLOTS(obj)));
          break;

     case TC_HASH:
          for (ii = 0; ii < HASH_SIZE(obj); ii++)
          {
               hash = HASH_COMBINE(hash, sxhash(HASH_DATA(obj)[ii].key));
               hash = HASH_COMBINE(hash, sxhash(HASH_DATA(obj)[ii].val));
          }
          break;

     default:
          hash = 0;
     }

     if (hash < 0)
          hash = -hash;         /*  REVISIT: still needed? */

     return hash;
}

lref_t lsxhash(lref_t obj, lref_t hash)       /*  if hash is bound, lsxhash matches its hash function */
{
     bool shallow = false;

     if (!NULLP(hash))
     {
          if (!HASHP(hash))
               vmerror_wrong_type_n(2, hash);

          shallow = HASH_SHALLOW(hash);
     }

     fixnum_t hashed;

     if (shallow)
          hashed = sxhash_eq(obj);
     else
          hashed = sxhash(obj);

     if (!NULLP(hash))
     {
          assert(HASHP(hash));

          hashed = hashed & HASH_MASK(hash);
     }

     return fixcons(hashed);

}

static size_t round_up_to_power_of_two(size_t val)
{
     size_t rounded = 1;

     while (rounded < val)
     {
          rounded <<= 1;

          if (rounded <= 0)
               return 0;        /*  REVISIT: correct overflow retval? */
     }

     return rounded;
}

static void clear_hash_data(hash_entry_t * entries, size_t size)
{
     for (size_t ii = 0; ii < size; ii++)
          init_hash_entry(&entries[ii]);
}

static hash_entry_t *allocate_hash_data(size_t size)
{
     hash_entry_t *data = (hash_entry_t *) gc_malloc(size * sizeof(hash_entry_t));

     clear_hash_data(data, size);

     return data;
}

lref_t hashcons(bool shallow, size_t size)
{
     lref_t hash = new_cell(TC_HASH);

     size = round_up_to_power_of_two(size);

     SET_HASH_MASK(hash, size - 1);
     SET_HASH_DATA(hash, allocate_hash_data(size));
     SET_HASH_SHALLOW(hash, shallow);
     SET_HASH_COUNT(hash, 0);

     return hash;
}

bool hash_equal(lref_t a, lref_t b)
{
     assert(HASHP(a));
     assert(TYPE(a) == TYPE(b));

     if (HASH_SHALLOW(a) != HASH_SHALLOW(b))
          return false;

     if (HASH_COUNT(a) != HASH_COUNT(b))
          return false;

     lref_t key, val;

     hash_iter_t ii;
     hash_iter_begin(a, &ii);
     while (hash_iter_next(a, &ii, &key, &val))
     {
          lref_t other_item_value;

          if (!hash_ref(b, key, &other_item_value))
               return false;

          if (!equalp(val, other_item_value))
               return false;
     }

     return true;
}

lref_t lmake_hash(lref_t key_type)
{
     bool shallow = false;

     if (NULLP(key_type))
          key_type = keyword_intern(_T("equal"));

     if (key_type == keyword_intern(_T("equal")))
          shallow = false;
     else if (key_type == keyword_intern(_T("eq")))
          shallow = true;
     else
          vmerror_arg_out_of_range(key_type, _T(":equal or :eq"));

     return hashcons(shallow);
}

lref_t lhashp(lref_t obj)
{
     if (HASHP(obj))
          return obj;
     else
          return boolcons(false);
}

static fixnum_t href_index(bool shallow_p, size_t mask, lref_t key)
{
     fixnum_t index;

     if (shallow_p)
          index = sxhash_eq(key);
     else
          index = sxhash(key);

     return index & mask;
}

static fixnum_t href_next_index(size_t mask, fixnum_t index)
{
     return (index + 1) & mask;
}

lref_t hash_set(lref_t table, lref_t key, lref_t value, bool check_for_expand);

static bool enlarge_hash(lref_t hash)
{
     assert(HASHP(hash));

     size_t current_size = HASH_SIZE(hash);
     size_t new_size;

     if (HASH_COUNT(hash) > HASH_SMALL_ENLARGE_THRESHOLD)
          new_size = current_size * HASH_SMALL_ENLARGE_FACTOR;
     else
          new_size = current_size * HASH_LARGE_ENLARGE_FACTOR;

     if (new_size < current_size)
          return false;

     hash_entry_t *new_data = allocate_hash_data(new_size);

     lref_t key, val;

     hash_iter_t ii;
     hash_iter_begin(hash, &ii);
     while (hash_iter_next(hash, &ii, &key, &val))
     {
          for (fixnum_t index = href_index(HASH_SHALLOW(hash), new_size - 1, key);;
               index = href_next_index(new_size - 1, index))
          {
               hash_entry_t *entry = &(new_data)[index];

               if (hash_entry_unused_p(entry))
               {
                    entry->key = key;
                    entry->val = val;

                    break;
               }
          }
     }

     gc_free(HASH_DATA(hash));

     SET_HASH_MASK(hash, new_size - 1);
     SET_HASH_DATA(hash, new_data);

     return true;
}

static hash_entry_t *hash_lookup_entry(lref_t hash, lref_t key)
{
     assert(HASHP(hash));

     for (fixnum_t index = href_index(HASH_SHALLOW(hash), HASH_MASK(hash), key);;
          index = href_next_index(HASH_MASK(hash), index))
     {
          hash_entry_t *entry = &HASH_DATA(hash)[index];

          if (hash_entry_deleted_p(entry))
               continue;

          if (hash_entry_unused_p(entry))
               break;

          if (HASH_SHALLOW(hash))
          {
               if (EQ(key, entry->key))
                    return entry;
          }
          else
          {
               if (equalp(key, entry->key))
                    return entry;
          }

          /*  REVISIT: termination criteria, if unused entries. (which shouldn't happen) */
     }

     return NULL;
}

bool hash_ref(lref_t hash, lref_t key, lref_t *value_result)
{
     hash_entry_t *entry = hash_lookup_entry(hash, key);

     if (entry == NULL)
          return false;

     *value_result = entry->val;

     return true;
}

lref_t lhash_refs(lref_t hash, lref_t key)
{
     if (!HASHP(hash))
          vmerror_wrong_type_n(1, hash);

     hash_entry_t *entry = hash_lookup_entry(hash, key);

     if (entry == NULL)
          return boolcons(false);

     return lcons(entry->key, entry->val);
}


lref_t lhash_ref(size_t argc, lref_t argv[])
{
     lref_t hash = NIL;
     lref_t key = NIL;
     lref_t defaultValue = boolcons(false);

     if (argc > 0)
          hash = argv[0];

     if (argc > 1)
          key = argv[1];

     if (argc > 2)
          defaultValue = argv[2];

     if (!HASHP(hash))
          vmerror_wrong_type_n(1, hash);

     hash_entry_t *entry = hash_lookup_entry(hash, key);

     if (entry == NULL)
          return defaultValue;
     else
          return entry->val;
}

lref_t lhash_hasp(lref_t hash, lref_t key)
{
     if (!HASHP(hash))
          vmerror_wrong_type_n(1, hash);

     if (hash_lookup_entry(hash, key) == NULL)
          return boolcons(false);
     else
          return hash;
}

lref_t hash_set(lref_t hash, lref_t key, lref_t value, bool check_for_expand)
{
     assert(HASHP(hash));

     hash_entry_t *entry = hash_lookup_entry(hash, key);        /*  REVISIT: double lookup/hash */

     if (entry != NULL)
     {
          entry->val = value;
     }
     else
     {
          for (fixnum_t index = href_index(HASH_SHALLOW(hash), HASH_MASK(hash), key);;
               index = href_next_index(HASH_MASK(hash), index))
          {
               hash_entry_t *entry = &HASH_DATA(hash)[index];

               if (hash_entry_unused_p(entry))
               {
                    entry->key = key;
                    entry->val = value;

                    SET_HASH_COUNT(hash, HASH_COUNT(hash) + 1);

                    break;
               }
          }
     }

     if (check_for_expand)
     {
          if (HASH_COUNT(hash) > HASH_SIZE(hash) * (HASH_MAX_LOAD_FACTOR / 100.0))
               enlarge_hash(hash);
     }

     return hash;
}

lref_t lhash_set(lref_t hash, lref_t key, lref_t value)
{
     if (!HASHP(hash))
          vmerror_wrong_type_n(1, hash);

     return hash_set(hash, key, value, true);
}

lref_t lhash_remove(lref_t hash, lref_t key)
{
     if (!HASHP(hash))
          vmerror_wrong_type_n(1, hash);

     hash_entry_t *entry = hash_lookup_entry(hash, key);

     if (entry != NULL)
     {
          delete_hash_entry(entry);
          SET_HASH_COUNT(hash, HASH_COUNT(hash) - 1);
     }

     return hash;
};

lref_t lhash_clear(lref_t hash)
{
     if (!HASHP(hash))
          vmerror_wrong_type_n(1, hash);

     SET_HASH_COUNT(hash, 0);
     clear_hash_data(HASH_DATA(hash), HASH_SIZE(hash));

     return hash;
}

/* This returns a 'binding table', a term I'm using to refer to a
 * vector with an element for each element in the hash table's array
 * of entries.  The element is null for a completely unused entry,
 * #f for a deleted entry, and a key/value pair for a bound entry.
 *
 * This is intended to make it possible to write scheme functions
 * analyzing hash table performance.
 */
lref_t lihash_binding_vector(lref_t hash)
{
     if (!HASHP(hash))
          vmerror_wrong_type_n(1, hash);

     size_t hash_size = HASH_SIZE(hash);

     lref_t btable = vectorcons(hash_size);

     for (size_t ii = 0; ii < hash_size; ii++)
     {
          hash_entry_t *entry = &HASH_DATA(hash)[ii];

          if (hash_entry_deleted_p(entry))
               SET_VECTOR_ELEM(btable, ii, boolcons(false));
          else if (hash_entry_unused_p(entry))
               SET_VECTOR_ELEM(btable, ii, NIL);
          else
               SET_VECTOR_ELEM(btable, ii, lcons(entry->key, entry->val));
     }

     return btable;
}


static bool init_slots(lref_t obj, lref_t initargs)
{
     if (!HASHP(obj))
          vmerror_wrong_type_n(1, obj);

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

          lhash_set(obj, CAR(initargs), CAR(CDR(initargs)));

          initargs = CDR(CDR(initargs));
     }

     return false;
}

lref_t llist2hash(lref_t obj)
{
     if (!(CONSP(obj) || NULLP(obj)))
          vmerror_wrong_type_n(1, obj);

     lref_t key_type = lcar(obj);
     lref_t bindings = lcdr(obj);

     lref_t hash = lmake_hash(key_type);

     if (init_slots(hash, bindings))
          vmerror_arg_out_of_range(bindings, _T("Invalid hash binding"));

     return hash;
}


lref_t lhash2alist(lref_t hash)
{
     if (!HASHP(hash))
          vmerror_wrong_type_n(1, hash);

     lref_t a_list = NIL;

     lref_t key, val;

     hash_iter_t ii;
     hash_iter_begin(hash, &ii);
     while (hash_iter_next(hash, &ii, &key, &val))
          a_list = lcons(lcons(key, val), a_list);

     return a_list;
}

lref_t lhash2list(lref_t hash)
{
     if (!HASHP(hash))
          vmerror_wrong_type_n(1, hash);

     lref_t new_list = NIL;

     lref_t key, val;

     hash_iter_t ii;
     hash_iter_begin(hash, &ii);
     while (hash_iter_next(hash, &ii, &key, &val))
          new_list = lcons(key, lcons(val, new_list));

     return lcons(lhash_type(hash), new_list);
}

lref_t lhash_type(lref_t hash)
{
     if (!HASHP(hash))
          return boolcons(false);

     if (HASH_SHALLOW(hash))
          return keyword_intern(_T("eq"));
     else
          return keyword_intern(_T("equal"));
}

lref_t lhash_copy(lref_t hash)
{
     if (!HASHP(hash))
          vmerror_wrong_type_n(hash);

     lref_t target_hash = hashcons(HASH_SHALLOW(hash));

     lref_t key, val;

     hash_iter_t ii;
     hash_iter_begin(hash, &ii);
     while (hash_iter_next(hash, &ii, &key, &val))
          lhash_set(target_hash, key, val);

     return target_hash;
}

size_t hash_length(lref_t hash)
{
     assert(HASHP(hash));

     return HASH_COUNT(hash);
}

