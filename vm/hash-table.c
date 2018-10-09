/*
 * hash-table.c --
 *
 * Scheme-visible hash tables.
 *
 *
 * (C) Copyright 2001-2014 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include "scan-private.h"

INLINE fixnum_t HASH_COMBINE(fixnum_t _h1, fixnum_t _h2)
{
     return (_h1 * 17 + 1) ^ _h2;
}

INLINE fixnum_t HASH_COMBINE_COMMUTE(fixnum_t _h1, fixnum_t _h2)
{
     return _h1 ^ _h2;
}

INLINE size_t HASH_MASK(lref_t obj)
{
     checked_assert(HASHP(obj));
     return obj->as.hash.table->mask;
}

INLINE void SET_HASH_MASK(lref_t obj, size_t mask)
{
     checked_assert(HASHP(obj));
     obj->as.hash.table->mask = mask;
}

INLINE size_t HASH_SIZE(lref_t obj)
{
     return HASH_MASK(obj) + 1;
}


INLINE struct hash_entry_t *HASH_ENTRY(lref_t hash, size_t index)
{
     assert(HASHP(hash));

     return &(hash->as.hash.table->data[index]);
}

INLINE void SET_HASH_SHALLOW(lref_t hash, bool is_shallow)
{
     assert(HASHP(hash));

     hash->as.hash.table->is_shallow = is_shallow;
}

INLINE bool HASH_SHALLOW(lref_t hash)
{
     assert(HASHP(hash));

     return hash->as.hash.table->is_shallow;
}

INLINE void SET_HASH_COUNT(lref_t hash, unsigned int count)
{
     assert(HASHP(hash));

     hash->as.hash.table->count = count;
}

INLINE unsigned int HASH_COUNT(lref_t hash)
{
     assert(HASHP(hash));

     return hash->as.hash.table->count;
}

static void init_hash_entry(struct hash_entry_t * entry)
{
     entry->key = UNBOUND_MARKER;
     entry->val = UNBOUND_MARKER;
}

static void delete_hash_entry(struct hash_entry_t * entry)
{
     entry->key = UNBOUND_MARKER;
     entry->val = NULL;
}

static bool hash_entry_used_p(struct hash_entry_t * entry)
{
     return !UNBOUND_MARKER_P(entry->key);
}

static bool hash_entry_unused_p(struct hash_entry_t * entry)
{
     return UNBOUND_MARKER_P(entry->key);
}

static bool hash_entry_deleted_p(struct hash_entry_t * entry)
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
          if (hash_entry_used_p(HASH_ENTRY(hash, *iter)))
          {
               if (key)
                    *key = HASH_ENTRY(hash, *iter)->key;

               if (val)
                    *val = HASH_ENTRY(hash, *iter)->val;

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
          for (ii = 0; ii < obj->as.string.dim; ii++)
               hash = (hash << 5) - hash + obj->as.string.data[ii];
          break;

     case TC_VECTOR:
          for (ii = 0; ii < obj->as.vector.dim; ii++)
               hash = HASH_COMBINE(hash, sxhash(obj->as.vector.data[ii]));
          break;

     case TC_STRUCTURE:
          hash = HASH_COMBINE(hash, sxhash(STRUCTURE_LAYOUT(obj)));

          for (ii = 0; ii < STRUCTURE_DIM(obj); ii++)
               hash = HASH_COMBINE(hash, sxhash(STRUCTURE_ELEM(obj, ii)));
          break;

     case TC_HASH:
          for (ii = 0; ii < HASH_SIZE(obj); ii++)
          {
               hash = HASH_COMBINE_COMMUTE(hash,
                                           HASH_COMBINE(sxhash(HASH_ENTRY(obj, ii)->key),
                                                        sxhash(HASH_ENTRY(obj, ii)->val)));
          }
          break;

     default:
          hash = 0;
     }

     return hash & FIXNUM_MAX;
}

lref_t lsxhash(lref_t obj)
{
     return fixcons(sxhash(obj));
}

lref_t lsxhash_identity(lref_t obj)
{
     return fixcons(sxhash_eq(obj));
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

static void clear_hash_data(struct hash_table_t *table)
{
     for (size_t ii = 0; ii < table->mask + 1; ii++)
          init_hash_entry(&table->data[ii]);
}

static struct hash_table_t *allocate_hash_data(size_t size)
{
     struct hash_table_t *table =
          gc_malloc(sizeof(struct hash_table_t)
                    + size * sizeof(struct hash_entry_t));

     table->mask = size - 1;

     clear_hash_data(table);

     return table;
}

lref_t hashcons(bool shallow)
{
     lref_t hash = new_cell(TC_HASH);

     size_t size = round_up_to_power_of_two(HASH_DEFAULT_INITIAL_SIZE);

     hash->as.hash.table = allocate_hash_data(size);

     SET_HASH_MASK(hash, size - 1);
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

lref_t lmake_hash()
{
     return hashcons(false);
}

lref_t lmake_identity_hash()
{
     return hashcons(true);
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

     struct hash_table_t *new_data = allocate_hash_data(new_size);

     new_data->mask = new_size - 1;
     new_data->is_shallow = HASH_SHALLOW(hash);
     new_data->count = HASH_COUNT(hash);

     lref_t key, val;

     hash_iter_t ii;
     hash_iter_begin(hash, &ii);
     while (hash_iter_next(hash, &ii, &key, &val))
     {
          for (fixnum_t index = href_index(HASH_SHALLOW(hash), new_size - 1, key);;
               index = href_next_index(new_size - 1, index))
          {
               struct hash_entry_t *entry = &(new_data->data[index]);

               if (hash_entry_unused_p(entry))
               {
                    entry->key = key;
                    entry->val = val;

                    break;
               }
          }
     }

     gc_free(hash->as.hash.table);

     hash->as.hash.table = new_data;

     return true;
}

static struct hash_entry_t *hash_lookup_entry(lref_t hash, lref_t key)
{
     assert(HASHP(hash));

     for (fixnum_t index = href_index(HASH_SHALLOW(hash), HASH_MASK(hash), key);;
          index = href_next_index(HASH_MASK(hash), index))
     {
          struct hash_entry_t *entry = HASH_ENTRY(hash, index);

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
     }

     return NULL;
}

bool hash_ref(lref_t hash, lref_t key, lref_t *value_result)
{
     struct hash_entry_t *entry = hash_lookup_entry(hash, key);

     if (entry == NULL)
          return false;

     *value_result = entry->val;

     return true;
}

lref_t lhash_refs(lref_t hash, lref_t key)
{
     if (!HASHP(hash))
          vmerror_wrong_type_n(1, hash);

     struct hash_entry_t *entry = hash_lookup_entry(hash, key);

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

     struct hash_entry_t *entry = hash_lookup_entry(hash, key);

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

     struct hash_entry_t *entry = hash_lookup_entry(hash, key);

     if (entry != NULL)
     {
          entry->val = value;
     }
     else
     {
          for (fixnum_t index = href_index(HASH_SHALLOW(hash), HASH_MASK(hash), key);;
               index = href_next_index(HASH_MASK(hash), index))
          {
               struct hash_entry_t *entry = HASH_ENTRY(hash, index);

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

lref_t lhash_set_multiple(lref_t hash, lref_t bindings)
{
     if (!HASHP(hash))
          vmerror_wrong_type_n(1, hash);

     lref_t pos = bindings;

     while (!NULLP(pos))
     {
          if (!CONSP(pos) || !CONSP(CDR(pos))) {
               vmerror_arg_out_of_range(pos, _T("Invalid hash binding"));
          }

          lhash_set(hash, CAR(pos), CAR(CDR(pos)));

          pos = CDR(CDR(pos));
     }

     return hash;
}

lref_t lhash_remove(lref_t hash, lref_t key)
{
     if (!HASHP(hash))
          vmerror_wrong_type_n(1, hash);

     struct hash_entry_t *entry = hash_lookup_entry(hash, key);

     if (entry != NULL)
     {
          delete_hash_entry(entry);
          SET_HASH_COUNT(hash, HASH_COUNT(hash) - 1);
     }

     return hash;
}

lref_t lhash_clear(lref_t hash)
{
     if (!HASHP(hash))
          vmerror_wrong_type_n(1, hash);

     SET_HASH_COUNT(hash, 0);
     clear_hash_data(hash->as.hash.table);

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

     lref_t btable = vectorcons(hash_size, NIL);

     for (size_t ii = 0; ii < hash_size; ii++)
     {
          struct hash_entry_t *entry = HASH_ENTRY(hash, ii);

          lref_t btelem;

          if (hash_entry_deleted_p(entry))
               btelem = boolcons(false);
          else if (hash_entry_unused_p(entry))
               btelem = NIL;
          else
               btelem = lcons(entry->key, entry->val);

          btable->as.vector.data[ii] = btelem;
     }

     return btable;
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

     return new_list;
}

lref_t lidentity_hash_p(lref_t obj)
{
     return boolcons(HASHP(obj) && HASH_SHALLOW(obj));
}

lref_t lhash_copy(lref_t hash)
{
     if (!HASHP(hash))
          vmerror_wrong_type_n(1, hash);

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

