/*
 * vm-tests.c
 *
 * Tests for the VM.
 *
 * (C) Copyright 2022 East Coast Toolworks Inc.
 *
 * See the file "LICENSE" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include <ctype.h>
#include <memory.h>
#include <stdio.h>

#include "scan-private.h"

static size_t assert_fail_count = 0;
static _TCHAR *test_name = NULL;

static void invoke_test(void (* test_fn)(), _TCHAR *test_fn_name)
{
     assert(test_name == NULL);

     test_name = test_fn_name;

     fprintf(stderr, "* %s\n", test_name);

     test_fn();

     test_name = NULL;
}

static bool test_assert(bool condition, _TCHAR *condition_name)
{
     if (condition)
          return false;

     fprintf(stderr, "** FAIL (%s): %s\n", test_name, condition_name);

     assert_fail_count++;

     return true;
}

#define INVOKE_TEST(test_fn_name) invoke_test(test_fn_name, _T(#test_fn_name))

#define TEST_ASSERT(condition) test_assert(condition, _T(#condition))

static void test_encode_decode_int(void (* encode)(uint8_t *buf, fixnum_t num),
                                   fixnum_t (* decode)(uint8_t *buf))
{
     fixnum_t ii;
     fixnum_t ii2;
     uint8_t buf[8];

     for(ii = -4; ii < 5; ii++) {
          encode(buf, ii);
          ii2 = decode(buf);

          if (TEST_ASSERT(ii == ii2))
               fprintf(stderr, _T("%" SCAN_PRIiFIXNUM " != %" SCAN_PRIiFIXNUM "\n"), ii, ii2);
     }
}

static void test_encode_decode_uint(void (* encode)(uint8_t *buf, unsigned_fixnum_t num),
                                    unsigned_fixnum_t (* decode)(uint8_t *buf))
{
     fixnum_t ii;
     fixnum_t ii2;
     uint8_t buf[8];

     for(ii = 0; ii < 5; ii++) {
          encode(buf, ii);
          ii2 = decode(buf);

          if (TEST_ASSERT(ii == ii2))
               fprintf(stderr, _T("%" SCAN_PRIiFIXNUM "i != %" SCAN_PRIiFIXNUM "i\n"), ii, ii2);
     }
}

static void test_encdec_int8 () { test_encode_decode_int(io_encode_int8 , io_decode_int8);  }
static void test_encdec_uint8 () { test_encode_decode_uint(io_encode_uint8 , io_decode_uint8);  }

static void test_encdec_int16() { test_encode_decode_int(io_encode_int16, io_decode_int16); }
static void test_encdec_uint16() { test_encode_decode_uint(io_encode_uint16, io_decode_uint16); }

static void test_encdec_int32() { test_encode_decode_int(io_encode_int32, io_decode_int32); }
static void test_encdec_uint32() { test_encode_decode_uint(io_encode_uint32, io_decode_uint32); }

static void test_encdec_int64() { test_encode_decode_int(io_encode_int64, io_decode_int64); }
static void test_encdec_uint64() { test_encode_decode_uint(io_encode_uint64, io_decode_uint64); }

size_t execute_vm_tests()
{
     INVOKE_TEST(test_encdec_int8  );
     INVOKE_TEST(test_encdec_uint8 );
     INVOKE_TEST(test_encdec_int16 );
     INVOKE_TEST(test_encdec_uint16);
     INVOKE_TEST(test_encdec_int32 );
     INVOKE_TEST(test_encdec_uint32);
     INVOKE_TEST(test_encdec_int64 );
     INVOKE_TEST(test_encdec_uint64);

     if (assert_fail_count > 0)
          fprintf(stderr, "%d ASSERT%s FAILED.\n",
                  (int)assert_fail_count, ((assert_fail_count == 1) ? "" : "S"));
     else
          fprintf(stderr, "All tests passed.\n");

     return assert_fail_count;
}
