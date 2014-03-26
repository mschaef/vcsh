/*
 * vm-tests.c
 *
 * Tests for the VM.
 *
 * (C) Copyright 2014 East Coast Toolworks Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
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

static void test_encode_decode_s(void (* encode)(uint8_t *buf, fixnum_t num),
                               fixnum_t (* decode)(uint8_t *buf))
{
     fixnum_t ii;
     fixnum_t ii2;
     uint8_t buf[8];

     for(ii = -4; ii < 5; ii++) {
          encode(buf, ii);
          ii2 = decode(buf);

          if (TEST_ASSERT(ii == ii2))
               fprintf(stderr, _T("%" PRINTF_PREFIX_FIXNUM "i != %" PRINTF_PREFIX_FIXNUM "i\n"), ii, ii2);
     }
}

static void test_encode_decode_u(void (* encode)(uint8_t *buf, unsigned_fixnum_t num),
                                 unsigned_fixnum_t (* decode)(uint8_t *buf))
{
     fixnum_t ii;
     fixnum_t ii2;
     uint8_t buf[8];

     for(ii = 0; ii < 5; ii++) {
          encode(buf, ii);
          ii2 = decode(buf);

          if (TEST_ASSERT(ii == ii2))
               fprintf(stderr, _T("%" PRINTF_PREFIX_FIXNUM "i != %" PRINTF_PREFIX_FIXNUM "i\n"), ii, ii2);
     }
}

static void test_encdec_s8 () { test_encode_decode_s(io_encode_fixnum_s8 , io_decode_fixnum_s8);  }
static void test_encdec_u8 () { test_encode_decode_u(io_encode_fixnum_u8 , io_decode_fixnum_u8);  }

static void test_encdec_s16() { test_encode_decode_s(io_encode_fixnum_s16, io_decode_fixnum_s16); }
static void test_encdec_u16() { test_encode_decode_u(io_encode_fixnum_u16, io_decode_fixnum_u16); }

static void test_encdec_s32() { test_encode_decode_s(io_encode_fixnum_s32, io_decode_fixnum_s32); }
static void test_encdec_u32() { test_encode_decode_u(io_encode_fixnum_u32, io_decode_fixnum_u32); }

static void test_encdec_s64() { test_encode_decode_s(io_encode_fixnum_s64, io_decode_fixnum_s64); }
static void test_encdec_u64() { test_encode_decode_u(io_encode_fixnum_u64, io_decode_fixnum_u64); }

size_t execute_vm_tests()
{
     INVOKE_TEST(test_encdec_s8 );
     INVOKE_TEST(test_encdec_u8 );
     INVOKE_TEST(test_encdec_s16);
     INVOKE_TEST(test_encdec_u16);
     INVOKE_TEST(test_encdec_s32);
     INVOKE_TEST(test_encdec_u32);
     INVOKE_TEST(test_encdec_s64);
     INVOKE_TEST(test_encdec_u64);

     if (assert_fail_count > 0)
          fprintf(stderr, "%d ASSERT%s FAILED.\n",
                  (int)assert_fail_count, ((assert_fail_count == 1) ? "" : "S"));
     else
          fprintf(stderr, "All tests passed.\n");

     return assert_fail_count;
}
