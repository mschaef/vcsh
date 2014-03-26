/*
 * io-encdec.cpp --
 *
 * Binary I/O encoding.
 *
 * (C) Copyright 2014 East Coast Toolworks Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */


#include <ctype.h>
#include <memory.h>

#include "scan-private.h"

/***** Signed 64 bit *****/

void io_encode_fixnum_s64(uint8_t *buf, fixnum_t num)
{
     buf[0] = (uint8_t)(num >> 56);
     buf[1] = (uint8_t)(num >> 48);
     buf[2] = (uint8_t)(num >> 40);
     buf[3] = (uint8_t)(num >> 32);
     buf[4] = (uint8_t)(num >> 24);
     buf[5] = (uint8_t)(num >> 16);
     buf[6] = (uint8_t)(num >>  8);
     buf[7] = (uint8_t)(num >>  0);
}

fixnum_t io_decode_fixnum_s64(uint8_t *buf)
{
     return (((fixnum_t)(buf[0] & 255) << 56) +
             ((fixnum_t)(buf[1] & 255) << 48) +
             ((fixnum_t)(buf[2] & 255) << 40) +
             ((fixnum_t)(buf[3] & 255) << 32) +
             ((fixnum_t)(buf[4] & 255) << 24) +
             ((fixnum_t)(buf[5] & 255) << 16) +
             ((fixnum_t)(buf[6] & 255) <<  8) +
             ((fixnum_t)(buf[7] & 255) <<  0));
}

/***** Unsigned 64 bit *****/

void io_encode_fixnum_u64(uint8_t *buf, fixnum_t num)
{
     unsigned_fixnum_t unum = (unsigned_fixnum_t) num;

     buf[0] = (uint8_t)(unum >> 56);
     buf[1] = (uint8_t)(unum >> 48);
     buf[2] = (uint8_t)(unum >> 40);
     buf[3] = (uint8_t)(unum >> 32);
     buf[4] = (uint8_t)(unum >> 24);
     buf[5] = (uint8_t)(unum >> 16);
     buf[6] = (uint8_t)(unum >>  8);
     buf[7] = (uint8_t)(unum >>  0);
}

fixnum_t io_decode_fixnum_u64(uint8_t *buf)
{
     return (((fixnum_t)buf[0] << 56) +
             ((fixnum_t)buf[1] << 48) +
             ((fixnum_t)buf[2] << 40) +
             ((fixnum_t)buf[3] << 32) +
             ((fixnum_t)buf[4] << 24) +
             ((fixnum_t)buf[5] << 16) +
             ((fixnum_t)buf[6] <<  8) +
             ((fixnum_t)buf[7] <<  0));
}

/***** Signed 32 bit *****/

void io_encode_fixnum_s32(uint8_t *buf, fixnum_t num)
{
     buf[0] = (uint8_t)(num >> 24);
     buf[1] = (uint8_t)(num >> 16);
     buf[2] = (uint8_t)(num >>  8);
     buf[3] = (uint8_t)(num >>  0);
}

fixnum_t io_decode_fixnum_s32(uint8_t *buf)
{
     return (((int32_t)(buf[0] << 24)) +
             ((int32_t)(buf[1] << 16)) +
             ((int32_t)(buf[2] <<  8)) +
             ((int32_t)(buf[3] <<  0)));
}

/***** Unsigned 32 bit *****/

void io_encode_fixnum_u32(uint8_t *buf, unsigned_fixnum_t num)
{
     buf[0] = (uint8_t)(num >> 24);
     buf[1] = (uint8_t)(num >> 16);
     buf[2] = (uint8_t)(num >>  8);
     buf[3] = (uint8_t)(num >>  0);
}

unsigned_fixnum_t io_decode_fixnum_u32(uint8_t *buf)
{
     return (((uint32_t)buf[0] << 24) +
             ((uint32_t)buf[1] << 16) +
             ((uint32_t)buf[2] << 8) +
             ((uint32_t)buf[3] << 0));
}
/***** Signed 16 bit *****/

void io_encode_fixnum_s16(uint8_t *buf, fixnum_t num)
{
     buf[0] = (uint8_t)(num >>  8);
     buf[1] = (uint8_t)(num >>  0);
}

fixnum_t io_decode_fixnum_s16(uint8_t *buf)
{
     return (((int16_t)buf[0] << 8) +
             ((int16_t)buf[1] << 0));
}

/***** Unsigned 16 bit *****/

void io_encode_fixnum_u16(uint8_t *buf, unsigned_fixnum_t num)
{
     buf[0] = (uint8_t)(num >>  8);
     buf[1] = (uint8_t)(num >>  0);
}

unsigned_fixnum_t io_decode_fixnum_u16(uint8_t *buf)
{
     return (((uint16_t)(buf[0] & 255) <<  8) +
             ((uint16_t)(buf[1] & 255) <<  0));
}

/***** Signed 8 bit *****/

void io_encode_fixnum_s8(uint8_t *buf, fixnum_t num)
{
     buf[0] = (uint8_t)(num >>  0);
}

fixnum_t io_decode_fixnum_s8(uint8_t *buf)
{
     return ((int8_t)(buf[0] & 255) <<  0);
}

/***** Unsigned 8 bit *****/

void io_encode_fixnum_u8(uint8_t *buf, unsigned_fixnum_t num)
{
     buf[0] = (uint8_t)(num >>  0);
}

unsigned_fixnum_t io_decode_fixnum_u8(uint8_t *buf)
{
     return ((uint8_t)(buf[0] & 255) <<  0);
}






