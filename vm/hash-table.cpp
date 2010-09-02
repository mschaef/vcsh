
/* hash-table.cpp
 *
 * SIOD hash table code.
 */

#include "scan.h"

BEGIN_NAMESPACE(scan)

/*  REVISIT: add explicit 'no value' hash value to allow keys to be members without values. (a way to use hashes as sets) */
#define HASH_COMBINE(_h1,_h2) ((((_h1) * 17 + 1) ^ (_h2)))

INLINE void SET_HASH_SHALLOW(LRef hash, bool shallow_keys)
{
     assert(HASHP(hash));

     hash->storage_as.hash.info.shallow_keys = shallow_keys;
}

INLINE bool HASH_SHALLOW(LRef hash)
{
     assert(HASHP(hash));

     return hash->storage_as.hash.info.shallow_keys;
}

INLINE void SET_HASH_COUNT(LRef hash, unsigned int count)
{
     assert(HASHP(hash));

     hash->storage_as.hash.info.count = count;
}

INLINE unsigned int HASH_COUNT(LRef hash)
{
     assert(HASHP(hash));

     return hash->storage_as.hash.info.count;
}

static void init_hash_entry(hash_entry_t * entry)
{
     entry->_key = UNBOUND_MARKER;
     entry->_val = UNBOUND_MARKER;
}

static void delete_hash_entry(hash_entry_t * entry)
{
     entry->_key = UNBOUND_MARKER;
     entry->_val = NULL;
}

static bool hash_entry_used_p(hash_entry_t * entry)
{
     return !UNBOUND_MARKER_P(entry->_key);
}

static bool hash_entry_unused_p(hash_entry_t * entry)
{
     return UNBOUND_MARKER_P(entry->_key);
}

static bool hash_entry_deleted_p(hash_entry_t * entry)
{
     return UNBOUND_MARKER_P(entry->_key) && NULLP(entry->_val);
}

void hash_iter_begin(LRef hash, hash_iter_t * iter)
{
     assert(HASHP(hash));

     *iter = 0;
}

bool hash_iter_next(LRef hash, hash_iter_t * iter, LRef * key, LRef * val)
{
     assert(HASHP(hash));

     while (*iter < HASH_SIZE(hash))
     {
          if (hash_entry_used_p(&HASH_DATA(hash)[*iter]))
          {
               if (key)
                    *key = HASH_DATA(hash)[*iter]._key;
               if (val)
                    *val = HASH_DATA(hash)[*iter]._val;

               *iter = *iter + 1;

               return true;
          }

          *iter = *iter + 1;
     }

     return false;
}

/**************************************************************
 * Hashing
 */

fixnum_t sxhash_eq(LRef obj)
{
     /* Slice off the tag bits, assuming that hashes will be mostly
      * homogeneous. */

     if (LREF1_TAG(obj) == LREF1_SPECIAL)
          return ((uptr) obj) >> LREF2_TAG_SHIFT;
     else
          return ((uptr) obj) >> LREF1_TAG_SHIFT;
}

fixnum_t sxhash(LRef obj)
{

     STACK_CHECK(&obj);

     fixnum_t hash = 0;

     /*  ADD_TYPE */

     if (NULLP(obj))
          return 0;

     LRef tmp;
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
          hash = sxhash(lsubr_name(obj));
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
          for (ii = 0; ii < INSTANCE_DIM(obj); ii++)
               hash = HASH_COMBINE(hash, sxhash(INSTANCE_ELEM(obj, ii)));
          break;

     case TC_HASH:
          for (ii = 0; ii < HASH_SIZE(obj); ii++)
          {
               hash = HASH_COMBINE(hash, sxhash(HASH_DATA(obj)[ii]._key));
               hash = HASH_COMBINE(hash, sxhash(HASH_DATA(obj)[ii]._val));
          }
          break;

     default:
          hash = 0;
     }

     if (hash < 0)
          hash = -hash;         /*  REVISIT: still needed? */

     return hash;
}

LRef lsxhash(LRef obj, LRef hash)       /*  if hash is bound, lsxhash matches its hash function */
{
     bool shallow = false;

     if (!NULLP(hash))
     {
          if (!HASHP(hash))
               vmerror_wrong_type(2, hash);

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

/**************************************************************
 * Hashes
 */

/* Constructor */

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
     hash_entry_t *data = (hash_entry_t *) safe_malloc(size * sizeof(hash_entry_t));

     clear_hash_data(data, size);

     return data;
}

LRef hashcons(bool shallow, size_t size)
{
     LRef hash = new_cell(TC_HASH);

     size = round_up_to_power_of_two(size);

     SET_HASH_MASK(hash, size - 1);
     SET_HASH_DATA(hash, allocate_hash_data(size));
     SET_HASH_SHALLOW(hash, shallow);
     SET_HASH_COUNT(hash, 0);

     return hash;
}

/* 'equal?' support */

bool hash_equal(LRef a, LRef b)
{
     assert(HASHP(a));
     assert(TYPE(a) == TYPE(b));

     if (HASH_SHALLOW(a) != HASH_SHALLOW(b))
          return false;

     if (HASH_COUNT(a) != HASH_COUNT(b))
          return false;

     LRef key, val;

     hash_iter_t ii;
     hash_iter_begin(a, &ii);
     while (hash_iter_next(a, &ii, &key, &val))
     {
          LRef other_item_value;

          if (!hash_ref(b, key, other_item_value))
               return false;

          if (!equalp(val, other_item_value))
               return false;
     }

     return true;
}

/* Hashing */

/* R5RS Hash Functions ****************************************/

LRef lmake_hash(LRef key_type)
{
     bool shallow = false;

     if (NULLP(key_type))
          key_type = keyword_intern(_T("equal"));

     if (key_type == keyword_intern(_T("equal")))
          shallow = false;
     else if (key_type == keyword_intern(_T("eq")))
          shallow = true;
     else
          vmerror("invalid hash key type", key_type);

     return hashcons(shallow);
}

LRef lhashp(LRef obj)
{
     if (HASHP(obj))
          return obj;
     else
          return boolcons(false);
}

/**************************************************************
 * Hash Tables
 **************************************************************/

static fixnum_t href_index(bool shallow_p, size_t mask, LRef key)
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

LRef hash_set(LRef table, LRef key, LRef value, bool check_for_expand);

static bool enlarge_hash(LRef hash)
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

     LRef key, val;

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
                    entry->_key = key;
                    entry->_val = val;

                    break;
               }
          }
     }

     safe_free(HASH_DATA(hash));

     SET_HASH_MASK(hash, new_size - 1);
     SET_HASH_DATA(hash, new_data);

     return true;
}

static hash_entry_t *hash_lookup_entry(LRef hash, LRef key)
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
               if (EQ(key, entry->_key))
                    return entry;
          }
          else
          {
               if (equalp(key, entry->_key))
                    return entry;
          }

          /*  REVISIT: termination criteria, if unused entries. (which shouldn't happen) */
     }

     return NULL;
}

bool hash_ref(LRef table, LRef key, LRef & value_result)
{
     hash_entry_t *entry = hash_lookup_entry(table, key);

     if (entry == NULL)
          return false;

     value_result = entry->_val;

     return true;
}

LRef lhash_refs(LRef table, LRef key)
{
     if (!HASHP(table))
          vmerror_wrong_type(1, table);

     hash_entry_t *entry = hash_lookup_entry(table, key);

     if (entry == NULL)
          return boolcons(false);

     return lcons(entry->_key, entry->_val);
}

/*  TODO: Switch lhash_ref to SUBR_ARGS, so that it can return () for a lookup failure. */
LRef lhash_ref(LRef table, LRef key, LRef defaultValue)
{
     if (NULLP(defaultValue))
          defaultValue = boolcons(false);

     if (!HASHP(table))
          vmerror_wrong_type(1, table);

     hash_entry_t *entry = hash_lookup_entry(table, key);

     if (entry == NULL)
          return defaultValue;
     else
          return entry->_val;
}

LRef lhash_hasp(LRef table, LRef key)
{
     if (!HASHP(table))
          vmerror_wrong_type(1, table);

     if (hash_lookup_entry(table, key) == NULL)
          return boolcons(false);
     else
          return table;
}

LRef hash_set(LRef table, LRef key, LRef value, bool check_for_expand)
{
     assert(HASHP(table));

     hash_entry_t *entry = hash_lookup_entry(table, key);       /*  REVISIT: double lookup/hash */

     if (entry != NULL)
     {
          entry->_val = value;
     }
     else
     {
          for (fixnum_t index = href_index(HASH_SHALLOW(table), HASH_MASK(table), key);;
               index = href_next_index(HASH_MASK(table), index))
          {
               hash_entry_t *entry = &HASH_DATA(table)[index];

               if (hash_entry_unused_p(entry))
               {
                    entry->_key = key;
                    entry->_val = value;

                    SET_HASH_COUNT(table, HASH_COUNT(table) + 1);

                    break;
               }
          }
     }

     if (check_for_expand)
     {
          if (HASH_COUNT(table) > HASH_SIZE(table) * (HASH_MAX_LOAD_FACTOR / 100.0))
               enlarge_hash(table);
     }

     return table;
}

LRef lhash_set(LRef table, LRef key, LRef value)
{
     if (!HASHP(table))
          vmerror_wrong_type(1, table);

     return hash_set(table, key, value, true);
}

LRef lhash_remove(LRef table, LRef key)
{
     if (!HASHP(table))
          vmerror_wrong_type(1, table);

     hash_entry_t *entry = hash_lookup_entry(table, key);

     if (entry != NULL)
     {
          delete_hash_entry(entry);
          SET_HASH_COUNT(table, HASH_COUNT(table) - 1);
     }

     return table;
};

LRef lhash_clear(LRef hash)
{
     if (!HASHP(hash))
          vmerror_wrong_type(1, hash);

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
LRef lihash_binding_vector(LRef hash)
{
     if (!HASHP(hash))
          vmerror_wrong_type(1, hash);

     size_t hash_size = HASH_SIZE(hash);

     LRef btable = vectorcons(hash_size);

     for (size_t ii = 0; ii < hash_size; ii++)
     {
          hash_entry_t *entry = &HASH_DATA(hash)[ii];

          if (hash_entry_deleted_p(entry))
               SET_VECTOR_ELEM(btable, ii, boolcons(false));
          else if (hash_entry_unused_p(entry))
               SET_VECTOR_ELEM(btable, ii, NIL);
          else
               SET_VECTOR_ELEM(btable, ii, lcons(entry->_key, entry->_val));
     }

     return btable;
}

LRef llist2hash(LRef obj)
{
     if (!(CONSP(obj) || NULLP(obj)))
          vmerror_wrong_type(1, obj);

     LRef key_type = lcar(obj);
     LRef bindings = lcdr(obj);

     LRef hash = lmake_hash(key_type);

     if (init_slots(hash, bindings, false))     /*  REVISIT: should this really be init_slots? */
          vmerror("Invalid hash binding", bindings);

     return hash;
}


LRef lhash2alist(LRef hash)
{
     if (!HASHP(hash))
          vmerror_wrong_type(1, hash);

     LRef a_list = NIL;

     LRef key, val;

     hash_iter_t ii;
     hash_iter_begin(hash, &ii);
     while (hash_iter_next(hash, &ii, &key, &val))
          a_list = lcons(lcons(key, val), a_list);

     return a_list;
}

LRef lhash2list(LRef hash)
{
     if (!HASHP(hash))
          vmerror_wrong_type(1, hash);

     LRef new_list = NIL;

     LRef key, val;

     hash_iter_t ii;
     hash_iter_begin(hash, &ii);
     while (hash_iter_next(hash, &ii, &key, &val))
          new_list = lcons(key, lcons(val, new_list));

     return lcons(lhash_type(hash), new_list);
}

LRef lhash_type(LRef hash)
{
     if (!HASHP(hash))
          return boolcons(false);

     if (HASH_SHALLOW(hash))
          return keyword_intern(_T("eq"));
     else
          return keyword_intern(_T("equal"));
}

LRef lhash_copy(LRef hash)
{
     if (!HASHP(hash))
          vmerror_wrong_type(hash);

     LRef target_hash = hashcons(HASH_SHALLOW(hash));

     LRef key, val;

     hash_iter_t ii;
     hash_iter_begin(hash, &ii);
     while (hash_iter_next(hash, &ii, &key, &val))
          lhash_set(target_hash, key, val);

     return target_hash;
}

LRef lhash_foreach(LRef closure, LRef hash)
{
     if (!CLOSUREP(closure))
          vmerror_wrong_type(1, closure);

     if (!HASHP(hash))
          vmerror_wrong_type(2, hash);

     LRef key, val;

     hash_iter_t ii;
     hash_iter_begin(hash, &ii);
     while (hash_iter_next(hash, &ii, &key, &val))
          lfuncall2(closure, key, val);

     return boolcons(false);
}

size_t hash_length(LRef hash)
{
     assert(HASHP(hash));

     return HASH_COUNT(hash);
}

END_NAMESPACE
