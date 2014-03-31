/*
 * io.cpp --
 *
 * The I/O subsystem. This tries to be as R5RS compliant as possible.
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include <ctype.h>
#include <memory.h>
#include <stdio.h>

#include "scan-private.h"

/*** End-of-file object ***/

lref_t lmake_eof()
{
     return MAKE_LREF2(LREF2_EOF, 0);
}

lref_t leof_objectp(lref_t obj)
{
     if (EOFP(obj))
          return obj;

     return boolcons(false);
}

/*** Port object ***/

lref_t port_gc_mark(lref_t obj)
{
     gc_mark(PORT_PINFO(obj)->port_name);

     return PORT_USER_OBJECT(obj);
}

void port_gc_free(lref_t port)
{
     assert(PORTP(port));
     assert(PORT_CLASS(port));

     if (PORT_CLASS(port)->close)
          PORT_CLASS(port)->close(port);

     if (PORT_TEXT_INFO(port))
          gc_free(PORT_TEXT_INFO(port));

     if (PORT_CLASS(port)->gc_free)
          PORT_CLASS(port)->gc_free(port);

     gc_free(PORT_PINFO(port));
}


lref_t initialize_port(lref_t port,
                       struct port_class_t * cls,
                       lref_t port_name,
                       enum port_mode_t mode,
                       lref_t user_object,
                       void *user_data)
{
     assert(cls != NULL);
     assert(!NULLP(port));

     SET_PORT_PINFO(port, gc_malloc(sizeof(struct port_info_t)));
     SET_PORT_CLASS(port, cls);

     PORT_PINFO(port)->port_name = port_name;
     PORT_PINFO(port)->user_data = user_data;
     PORT_PINFO(port)->user_object = user_object;
     PORT_PINFO(port)->mode = mode;

     SET_PORT_TEXT_INFO(port, NULL);;

     if (PORT_CLASS(port)->open)
          PORT_CLASS(port)->open(port);

     return port;
}

size_t port_length(lref_t port)
{
     assert(PORTP(port));

     if (PORT_CLASS(port)->length)
          return PORT_CLASS(port)->length(port);

     return 0;
}

size_t write_bytes(lref_t port, const void *buf, size_t size)
{
     assert(!NULLP(port));
     assert(PORT_CLASS(port)->write_bytes);

     return PORT_CLASS(port)->write_bytes(port, buf, size);
}

size_t read_bytes(lref_t port, void *buf, size_t size)
{
     assert(!NULLP(port));
     assert(PORT_CLASS(port)->read_bytes);

     size_t actual_count =
          PORT_CLASS(port)->read_bytes(port, buf, size);

     PORT_PINFO(port)->bytes_read += actual_count;

     return actual_count;
}

lref_t portcons(struct port_class_t * cls,
                lref_t port_name,
                enum port_mode_t mode,
                lref_t user_object,
                void *user_data)
{
     return initialize_port(new_cell(TC_PORT),
                            cls, port_name, mode, user_object, user_data);
}

/***** C I/O functions *****/


bool read_binary_fixnum(fixnum_t length, bool signedp, lref_t port, fixnum_t *result)
{
     assert(BINARY_PORTP(port));

     uint8_t bytes[sizeof(fixnum_t)];

     if (read_bytes(port, bytes, (size_t)length) <= 0)
          return false;

     switch (length)
     {
     case 1:
          if (signedp)
               *result = io_decode_int8(bytes);
          else
               *result = io_decode_uint8(bytes);
          break;
     case 2:
          if (signedp)
               *result = io_decode_int16(bytes);
          else
               *result = io_decode_uint16(bytes);
          break;
     case 4:
          if (signedp)
               *result = io_decode_int32(bytes);
          else
               *result = io_decode_uint32(bytes);
          break;
#ifdef SCAN_64BIT
     case 8:
          if (signedp)
               *result = io_decode_int64(bytes);
          else
               *result = io_decode_uint64(bytes);
          break;
#endif
     default:
          assert(!"Invalid length in read_binary_fixnum"); 
     }

     return true;
}

 

bool read_binary_flonum(lref_t port, flonum_t *result)
{
     assert(BINARY_PORTP(port));

     uint8_t bytes[sizeof(flonum_t)];
     size_t flonums_read = read_bytes(port, bytes, sizeof(flonum_t));

     if (!flonums_read)
          return false;

     *result = *(flonum_t *) bytes;

     return true;
}


/***** Lisp-visible port functions *****/

lref_t lportp(lref_t obj)
{
     return boolcons(PORTP(obj));
}

lref_t linput_portp(lref_t obj)
{
     if (PORTP(obj) && PORT_INPUTP(obj))
          return obj;

     return boolcons(false);
}

lref_t loutput_portp(lref_t obj)
{
     if (PORTP(obj) && PORT_OUTPUTP(obj))
          return obj;

     return boolcons(false);
}

lref_t lbinary_portp(lref_t obj)
{
     if (BINARY_PORTP(obj))
          return obj;

     return boolcons(false);
}

lref_t lport_closedp(lref_t obj)
{
     if (!PORTP(obj))
          vmerror_wrong_type_n(1, obj);

     return boolcons(PORT_CLOSEDP(obj));
}

lref_t lport_openp(lref_t obj)
{
     if (!PORTP(obj))
          vmerror_wrong_type_n(1, obj);

     return boolcons(!PORT_CLOSEDP(obj));
}


lref_t lport_name(lref_t port)
{
     if (NULLP(port))
          port = CURRENT_INPUT_PORT();

     if (!PORTP(port))
          vmerror_wrong_type_n(1, port);

     return PORT_PINFO(port)->port_name;
}

lref_t lclose_port(lref_t port)
{
     if (!PORTP(port))
          vmerror_wrong_type_n(1, port);

     if (PORT_OUTPUTP(port))
          lflush_port(port);

     if (PORT_CLASS(port)->close)
          PORT_CLASS(port)->close(port);

     PORT_PINFO(port)->mode = PORT_CLOSED;

     return port;
}

lref_t lflush_port(lref_t port)
{
     if (!PORTP(port))
          vmerror_wrong_type_n(1, port);

     if (TEXT_PORTP(port)
         && PORT_TEXT_INFO(port)->translate
         && PORT_TEXT_INFO(port)->needs_lf)
     {
          write_char(port, _T('\n'));
     }

     if (PORT_CLASS(port)->flush)
          PORT_CLASS(port)->flush(port);

     return port;
}


lref_t lread_binary_string(lref_t l, lref_t port)
{
     _TCHAR buf[STACK_STRBUF_LEN];

     if (!BINARY_PORTP(port))
          vmerror_wrong_type_n(2, port);

     if (!NUMBERP(l))
          vmerror_wrong_type_n(1, l);

     fixnum_t remaining_length = get_c_fixnum(l);

     if (remaining_length <= 0)
          vmerror_arg_out_of_range(l, _T(">0"));

     lref_t new_str = strcons();
     size_t total_read = 0;

     while (remaining_length > 0)
     {
          fixnum_t to_read = remaining_length;

          if (to_read > STACK_STRBUF_LEN)
               to_read = STACK_STRBUF_LEN;

          size_t actual_read = read_bytes(port, buf, (size_t)(remaining_length * sizeof(_TCHAR)));

          if (actual_read <= 0)
               break;

          string_appendd(new_str, buf, actual_read);

          remaining_length -= actual_read;
          total_read += actual_read;
     }

     if (total_read == 0)
          return lmake_eof();

     return new_str;
}


INLINE lref_t lread_binary_fixnum_0(size_t length, bool signedp, lref_t port)
{
     if (!BINARY_PORTP(port))
          vmerror_wrong_type_n(1, port);

     fixnum_t result = 0;

     if (read_binary_fixnum(length, signedp, port, &result))
          return fixcons(result);

     return lmake_eof();
}

lref_t lread_binary_fixnum_u8(lref_t port)
{
     return lread_binary_fixnum_0(1, false, port);
}

lref_t lread_binary_fixnum_s8(lref_t port)
{
     return lread_binary_fixnum_0(1, true, port);
}

lref_t lread_binary_fixnum_u16(lref_t port)
{
     return lread_binary_fixnum_0(2, false, port);
}

lref_t lread_binary_fixnum_s16(lref_t port)
{
     return lread_binary_fixnum_0(2, true, port);
}

lref_t lread_binary_fixnum_u32(lref_t port)
{
     return lread_binary_fixnum_0(4, false, port);
}

lref_t lread_binary_fixnum_s32(lref_t port)
{
     return lread_binary_fixnum_0(4, true, port);
}

lref_t lread_binary_fixnum_u64(lref_t port)
{
     return lread_binary_fixnum_0(8, false, port);
}

lref_t lread_binary_fixnum_s64(lref_t port)
{
     return lread_binary_fixnum_0(8, true, port);
}

lref_t lread_binary_flonum(lref_t port)
{
     if (!BINARY_PORTP(port))
          vmerror_wrong_type_n(1, port);

     flonum_t result = 0;

     if (read_binary_flonum(port, &result))
          return flocons(result);
     else
          return lmake_eof();
}


lref_t lwrite_binary_string(lref_t string, lref_t port)
{
     if (!STRINGP(string))
          vmerror_wrong_type_n(1, string);

     if (!BINARY_PORTP(port))
          vmerror_wrong_type_n(2, port);

     size_t length = STRING_DIM(string);
     _TCHAR *strdata = STRING_DATA(string);

     size_t bytes_written = write_bytes(port, strdata, length * sizeof(_TCHAR));

     if (bytes_written != length * sizeof(_TCHAR))
          vmerror_io_error(_T("error writing to port."), port);

     return port;
}

INLINE lref_t lwrite_binary_fixnum_0(uint8_t bytes[], size_t length, lref_t port)
{
     if (!BINARY_PORTP(port))
          vmerror_wrong_type_n(2, port);

     if (write_bytes(port, bytes, length) != length)
          vmerror_io_error(_T("error writing to port."), port);

     return port;
}


lref_t lwrite_binary_fixnum_u8(lref_t v, lref_t port)
{
     if (!FIXNUMP(v))
          vmerror_wrong_type_n(1, v);

     uint8_t bytes[sizeof(fixnum_t)];

     io_encode_uint8(bytes, FIXNM(v));

     return lwrite_binary_fixnum_0(bytes, 1, port);
}

lref_t lwrite_binary_fixnum_s8(lref_t v, lref_t port)
{
     if (!FIXNUMP(v))
          vmerror_wrong_type_n(1, v);

     uint8_t bytes[sizeof(fixnum_t)];

     io_encode_int8(bytes, FIXNM(v));

     return lwrite_binary_fixnum_0(bytes, 1, port);
}

lref_t lwrite_binary_fixnum_u16(lref_t v, lref_t port)
{
     if (!FIXNUMP(v))
          vmerror_wrong_type_n(1, v);

     uint8_t bytes[sizeof(fixnum_t)];

     io_encode_uint16(bytes, FIXNM(v));

     return lwrite_binary_fixnum_0(bytes, 2, port);
}

lref_t lwrite_binary_fixnum_s16(lref_t v, lref_t port)
{
     if (!FIXNUMP(v))
          vmerror_wrong_type_n(1, v);

     uint8_t bytes[sizeof(fixnum_t)];

     io_encode_int16(bytes, FIXNM(v));

     return lwrite_binary_fixnum_0(bytes, 2, port);
}

lref_t lwrite_binary_fixnum_u32(lref_t v, lref_t port)
{
     if (!FIXNUMP(v))
          vmerror_wrong_type_n(1, v);

     uint8_t bytes[sizeof(fixnum_t)];

     io_encode_uint32(bytes, FIXNM(v));

     return lwrite_binary_fixnum_0(bytes, 4, port);
}

lref_t lwrite_binary_fixnum_s32(lref_t v, lref_t port)
{
     if (!FIXNUMP(v))
          vmerror_wrong_type_n(1, v);

     uint8_t bytes[sizeof(fixnum_t)];

     io_encode_int32(bytes, FIXNM(v));

     return lwrite_binary_fixnum_0(bytes, 4, port);
}

lref_t lwrite_binary_fixnum_u64(lref_t v, lref_t port)
{
     if (!FIXNUMP(v))
          vmerror_wrong_type_n(1, v);

     uint8_t bytes[sizeof(fixnum_t)];

     io_encode_uint64(bytes, FIXNM(v));

     return lwrite_binary_fixnum_0(bytes, 8, port);
}

lref_t lwrite_binary_fixnum_s64(lref_t v, lref_t port)
{
     if (!FIXNUMP(v))
          vmerror_wrong_type_n(1, v);

     uint8_t bytes[sizeof(fixnum_t)];

     io_encode_int64(bytes, FIXNM(v));

     return lwrite_binary_fixnum_0(bytes, 8, port);
}

lref_t lbinary_write_flonum(lref_t v, lref_t port)
{
     if (!NUMBERP(v))
          vmerror_wrong_type_n(1, v);

     if (!BINARY_PORTP(port))
          vmerror_wrong_type_n(2, port);

     uint8_t bytes[sizeof(flonum_t)];

     *(flonum_t *) bytes = get_c_double(v);

     if (write_bytes(port, bytes, sizeof(flonum_t)) != sizeof(flonum_t))
          vmerror_io_error(_T("error writing to port."), port);

     return port;
}

/* Null port
 *
 * Input port - Always reads out EOF.
 * Output port - Accepts all writes.
 */

size_t null_port_read_bytes(lref_t port, void *buf, size_t size)
{
     UNREFERENCED(port);
     UNREFERENCED(buf);
     UNREFERENCED(size);

     return 0;
}

size_t null_port_write_bytes(lref_t port, const void *buf, size_t size)
{
     UNREFERENCED(port);
     UNREFERENCED(buf);
     UNREFERENCED(size);

     return size;
}

struct port_class_t null_port_class = {
     _T("NULL"),

     NULL,                  // open
     null_port_read_bytes,  // read_bytes
     null_port_write_bytes, // write_bytes
     NULL,                  // peek_char
     NULL,                  // read_chars
     NULL,                  // write_chars
     NULL,                  // rich_write
     NULL,                  // flush
     NULL,                  // close
     NULL,                  // gc_free
     NULL,                  // length
};

lref_t lopen_null_port()
{
     return portcons(&null_port_class, NIL, PORT_INPUT | PORT_OUTPUT, NIL, NULL);
}
