/*
 * io-encdec.c --
 *
 * Binary I/O encoding.
 *
 * (C) Copyright 2022 East Coast Toolworks Inc.
 *
 * See the file "LICENSE" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include <ctype.h>
#include <memory.h>

#include "scan-private.h"


/***** Unsigned 8 bit *****/

void io_encode_uint8(uint8_t *buf, unsigned_fixnum_t num)
{
     buf[0] = (uint8_t)num;
}

unsigned_fixnum_t io_decode_uint8(uint8_t *buf)
{
     return (uint8_t)buf[0];
}

/***** Signed 8 bit *****/

void io_encode_int8(uint8_t *buf, fixnum_t num)
{
     buf[0] = (uint8_t)num;
}

fixnum_t io_decode_int8(uint8_t *buf)
{
     return (int8_t)buf[0];
}

/***** Unsigned 16 bit *****/

void io_encode_uint16(uint8_t *buf, unsigned_fixnum_t num)
{
     buf[0] = (uint8_t)(num >>  8);
     buf[1] = (uint8_t)num;
}

unsigned_fixnum_t io_decode_uint16(uint8_t *buf)
{
     return (((uint16_t)buf[0] <<  8) +
             ((uint16_t)buf[1]));
}

/***** Signed 16 bit *****/

void io_encode_int16(uint8_t *buf, fixnum_t num)
{
     buf[0] = (uint8_t)(num >>  8);
     buf[1] = (uint8_t)num;
}

fixnum_t io_decode_int16(uint8_t *buf)
{
     return ((int16_t)(buf[0] << 8) +
             (int16_t)buf[1]);
}

/***** Unsigned 32 bit *****/

void io_encode_uint32(uint8_t *buf, unsigned_fixnum_t num)
{
     buf[0] = (uint8_t)(num >> 24);
     buf[1] = (uint8_t)(num >> 16);
     buf[2] = (uint8_t)(num >>  8);
     buf[3] = (uint8_t)num;
}

unsigned_fixnum_t io_decode_uint32(uint8_t *buf)
{
     return (((uint32_t)buf[0] << 24) +
             ((uint32_t)buf[1] << 16) +
             ((uint32_t)buf[2] << 8) +
             ((uint32_t)buf[3]));
}

/***** Signed 32 bit *****/

void io_encode_int32(uint8_t *buf, fixnum_t num)
{
     buf[0] = (uint8_t)(num >> 24);
     buf[1] = (uint8_t)(num >> 16);
     buf[2] = (uint8_t)(num >>  8);
     buf[3] = (uint8_t)num;
}

fixnum_t io_decode_int32(uint8_t *buf)
{
     return (((int32_t)(buf[0] << 24)) +
             ((int32_t)(buf[1] << 16)) +
             ((int32_t)(buf[2] <<  8)) +
             ((int32_t)buf[3]));
}

/***** Unsigned 64 bit *****/

void io_encode_uint64(uint8_t *buf, unsigned_fixnum_t num)
{
     buf[0] = (uint8_t)(num >> 56);
     buf[1] = (uint8_t)(num >> 48);
     buf[2] = (uint8_t)(num >> 40);
     buf[3] = (uint8_t)(num >> 32);
     buf[4] = (uint8_t)(num >> 24);
     buf[5] = (uint8_t)(num >> 16);
     buf[6] = (uint8_t)(num >>  8);
     buf[7] = (uint8_t)num;
}

unsigned_fixnum_t io_decode_uint64(uint8_t *buf)
{
     return (((unsigned_fixnum_t)buf[0] << 56) +
             ((unsigned_fixnum_t)buf[1] << 48) +
             ((unsigned_fixnum_t)buf[2] << 40) +
             ((unsigned_fixnum_t)buf[3] << 32) +
             ((unsigned_fixnum_t)buf[4] << 24) +
             ((unsigned_fixnum_t)buf[5] << 16) +
             ((unsigned_fixnum_t)buf[6] <<  8) +
             ((unsigned_fixnum_t)buf[7] <<  0));
}

/***** Signed 64 bit *****/

void io_encode_int64(uint8_t *buf, fixnum_t num)
{
     buf[0] = (uint8_t)(num >> 56);
     buf[1] = (uint8_t)(num >> 48);
     buf[2] = (uint8_t)(num >> 40);
     buf[3] = (uint8_t)(num >> 32);
     buf[4] = (uint8_t)(num >> 24);
     buf[5] = (uint8_t)(num >> 16);
     buf[6] = (uint8_t)(num >>  8);
     buf[7] = (uint8_t)num;
}

fixnum_t io_decode_int64(uint8_t *buf)
{
     return (((fixnum_t)buf[0] << 56) +
             ((fixnum_t)buf[1] << 48) +
             ((fixnum_t)buf[2] << 40) +
             ((fixnum_t)buf[3] << 32) +
             ((fixnum_t)buf[4] << 24) +
             ((fixnum_t)buf[5] << 16) +
             ((fixnum_t)buf[6] <<  8) +
             ((fixnum_t)buf[7]));
}

/***** Flonum *****/

void io_encode_flonum(uint8_t *buf, flonum_t num)
{
     memcpy(buf, &num, sizeof(flonum_t));
}

flonum_t io_decode_flonum(uint8_t *buf)
{
     flonum_t value;

     memcpy(&value, buf, sizeof(flonum_t));

     return value;
}

