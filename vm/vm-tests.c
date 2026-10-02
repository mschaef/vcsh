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
#include <math.h>
#include <float.h>

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

/* Immediate flonums */

static bool flonum_bits_equal(double a, double b)
{
     return FLONUM_TO_BITS(a) == FLONUM_TO_BITS(b);
}

/* Check one double: it must round trip bit-for-bit through flocons and
 * FLONM, and be immediate exactly when expected. */
static void check_flonum_round_trip(double d, int expect_immediate)
{
     lref_t imm = NIL;
     bool is_imm = FLONUM_IMMEDIATE(d, &imm);

     if (is_imm) {
          if (TEST_ASSERT(LREF1_TAG(imm) == LREF1_FLONUM))
               fprintf(stderr, "bad tag for %a\n", d);

          if (TEST_ASSERT(flonum_bits_equal(d, FLONUM_IMMEDIATE_VALUE(imm))))
               fprintf(stderr, "immediate %a decodes to %a\n", d, FLONUM_IMMEDIATE_VALUE(imm));
     }

     if ((expect_immediate >= 0) && TEST_ASSERT(is_imm == (bool)expect_immediate))
          fprintf(stderr, "%a: immediate = %d, expected %d\n", d, is_imm, expect_immediate);

     lref_t x = flocons(d);

     if (TEST_ASSERT(FLONUMP(x) && (TYPE(x) == TC_FLONUM) && !COMPLEXP(x)))
          fprintf(stderr, "%a: not a flonum after flocons\n", d);

     if (TEST_ASSERT(LREF_IMMEDIATE_P(x) == is_imm))
          fprintf(stderr, "%a: flocons immediacy mismatch\n", d);

     if (TEST_ASSERT(flonum_bits_equal(d, FLONM(x))))
          fprintf(stderr, "%a: flocons round trip gave %a\n", d, FLONM(x));
}

static void test_flonum_immediate_values()
{
     /* Immediate */
     check_flonum_round_trip(0.0, 1);
     check_flonum_round_trip(1.0, 1);
     check_flonum_round_trip(-1.0, 1);
     check_flonum_round_trip(0.1, 1);
     check_flonum_round_trip(-3.25, 1);
     check_flonum_round_trip(1e10, 1);
     check_flonum_round_trip(6.02214076e23, 1);
     check_flonum_round_trip(1e-70, 1);
     check_flonum_round_trip(1e77, 1);
     check_flonum_round_trip(-1e77, 1);
     check_flonum_round_trip(nextafter(ldexp(1.0, -255), 1.0), 1);  /* smallest immediate */
     check_flonum_round_trip(-ldexp(1.0, -255), 1);                 /* sign bit avoids the collision */
     check_flonum_round_trip(ldexp(1.0, 256), 1);
     check_flonum_round_trip(nextafter(ldexp(1.0, 257), 0.0), 1);   /* largest immediate */

     /* Boxed */
     check_flonum_round_trip(-0.0, 0);
     check_flonum_round_trip(ldexp(1.0, -255), 0);  /* 0x3000000000000000, would collide with 0.0 */
     check_flonum_round_trip(nextafter(ldexp(1.0, -255), 0.0), 0);
     check_flonum_round_trip(ldexp(1.0, 257), 0);
     check_flonum_round_trip(1e-300, 0);
     check_flonum_round_trip(1e300, 0);
     check_flonum_round_trip(DBL_MIN, 0);
     check_flonum_round_trip(DBL_MAX, 0);
     check_flonum_round_trip(DBL_TRUE_MIN, 0);
     check_flonum_round_trip(INFINITY, 0);
     check_flonum_round_trip(-INFINITY, 0);
     check_flonum_round_trip(NAN, 0);
     check_flonum_round_trip(BITS_TO_FLONUM(0xfff0000000000123ULL), 0); /* NaN with payload */

     /* The zero encoding must not collide with any other immediate. */
     lref_t zero = NIL;
     FLONUM_IMMEDIATE(0.0, &zero);
     TEST_ASSERT((uint64_t)(uintptr_t)zero == FLONUM_IMMEDIATE_ZERO);
}

static void test_flonum_immediate_sweep()
{
     /* Pseudo-random bit patterns, plus every exponent with a few
      * mantissas, checked bit-for-bit. A cheap LCG keeps this
      * deterministic. */
     uint64_t state = 0x9e3779b97f4a7c15ULL;

     for (size_t ii = 0; ii < 2000000; ii++) {
          state = state * 6364136223846793005ULL + 1442695040888963407ULL;
          check_flonum_round_trip(BITS_TO_FLONUM(state), -1);
     }

     for (uint64_t sign = 0; sign < 2; sign++)
          for (uint64_t exp = 0; exp < 2048; exp++) {
               uint64_t base = (sign << 63) | (exp << 52);

               check_flonum_round_trip(BITS_TO_FLONUM(base), -1);
               check_flonum_round_trip(BITS_TO_FLONUM(base | 1), -1);
               check_flonum_round_trip(BITS_TO_FLONUM(base | 0x000fffffffffffffULL), -1);
               check_flonum_round_trip(BITS_TO_FLONUM(base | 0x0008000000000000ULL), -1);
          }
}

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
     INVOKE_TEST(test_flonum_immediate_values);
     INVOKE_TEST(test_flonum_immediate_sweep);

     if (assert_fail_count > 0)
          fprintf(stderr, "%d ASSERT%s FAILED.\n",
                  (int)assert_fail_count, ((assert_fail_count == 1) ? "" : "S"));
     else
          fprintf(stderr, "All tests passed.\n");

     return assert_fail_count;
}
