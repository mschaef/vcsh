
/*
 * mt19937.cpp --
 *
 * Mersenne Twister RNG, from the official site:
 *
 * Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
 *  All rights reserved.
 */

#ifndef __MT19937_H
#define __MT19937_H

#include "../util/base-types.h"

void init_mt19937(unsigned long s);
void init_mt19937_by_array(unsigned long init_key[], int key_length);

unsigned long mt19937_int32(void);      /* generates a random number on [0,0xffffffff]-interval */
uint64_t mt19937_int64(void);      /* generates a random number on [0,0xffffffffffffffff]-interval */

long mt19937_int31(void);       /* generates a random number on [0,0x7fffffff]-interval */

double mt19937_real1(void);     /* generates a random number on [0,1]-real-interval */
double mt19937_real2(void);     /* generates a random number on [0,1)-real-interval */
double mt19937_real3(void);     /* generates a random number on (0,1)-real-interval */
double mt19937_res53(void);     /* generates a random number on [0,1) with 53-bit resolution */


#endif                          /*  __MT19937_H */
