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

static size_t test_fail_count = 0;
static _TCHAR *test_name = NULL;

static void invoke_test(void (* test_fn)(), _TCHAR *test_fn_name)
{
     assert(test_name == NULL);

     test_name = test_fn_name;

     fprintf(stderr, "Executing VM Test: %s\n", test_name);

     test_fn();

     test_name = NULL;
}

static void test_assert(bool condition, _TCHAR *condition_name)
{
     if (condition)
          return;

     fprintf(stderr, "Failure in %s: %s\n", test_name, condition_name);

     test_fail_count++;
}

#define INVOKE_TEST(test_fn_name) invoke_test(test_fn_name, _T(#test_fn_name))

#define TEST_ASSERT(condition) test_assert(condition, _T(#condition));

static void test_encdec_i8()
{
     TEST_ASSERT(false);
}

static void dotest()
{
     fixnum_t ii;
     fixnum_t ii2;
     uint8_t buf[8];

     for(ii = -4; ii < 5; ii++) {
          io_encode_fixnum_s16(buf, ii);
          ii2 = io_decode_fixnum_s16(buf);

          
          fprintf(stderr, _T("%" PRINTF_PREFIX_FIXNUM "i == %" PRINTF_PREFIX_FIXNUM "i\n"), ii, ii2);
     }
}

size_t execute_vm_tests()
{
     INVOKE_TEST(test_encdec_i8);

     if (test_fail_count > 0)
          fprintf(stderr, "%d TEST%s FAILED.\n",
                  (int)test_fail_count, ((test_fail_count == 1) ? "" : "S"));
     else
          fprintf(stderr, "All tests passed.\n");

     return test_fail_count;
}
