/*
 * constants.c --
 *
 * The source file for the constant tables.
 *
 * (C) Copyright 2001-2022 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "LICENSE" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include <string.h>

#include "scan-sys.h"

#define CONST_C_IMPL
#include "scan-constants.i"
#undef CONST_C_IMPL

/* FNV-1a, used for the VM interface hash written into FASL headers. */

uint64_t abi_hash_bytes(uint64_t h, const void *data, size_t len)
{
     const uint8_t *bytes = (const uint8_t *)data;

     for (size_t ii = 0; ii < len; ii++) {
          h ^= bytes[ii];
          h *= UINT64_C(0x100000001b3);
     }

     return h;
}

uint64_t abi_hash_string(uint64_t h, const char *str)
{
     /* Include the terminator, so adjacent strings can't run together. */
     return abi_hash_bytes(h, str, strlen(str) + 1);
}

uint64_t abi_hash_int(uint64_t h, int64_t value)
{
     uint8_t bytes[8];

     for (size_t ii = 0; ii < 8; ii++)
          bytes[ii] = (uint8_t)((uint64_t)value >> (8 * ii));

     return abi_hash_bytes(h, bytes, sizeof(bytes));
}

uint64_t vm_constants_abi_hash(uint64_t h)
{
#define CONST_C_HASH
#include "scan-constants.i"
#undef CONST_C_HASH

     return h;
}

