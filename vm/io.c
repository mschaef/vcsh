
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

 /*  REVISIT: Do we need to restrict bootup NULL I/O */

 /*  REVISIT: lots of logic supports default ports if port==NULL. Move to scheme? */

 /*** End-of-file object ***/

 lref_t lmake_eof()
 {
      return LREF2_CONS(LREF2_EOF, 0);
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


 lref_t initialize_port(lref_t s,
                        struct port_class_t * cls,
                        lref_t port_name,
                        enum port_mode_t mode,
                        lref_t user_object,
                        void *user_data)
 {
      assert(cls != NULL);
      assert(!NULLP(s));

      SET_PORT_PINFO(s, gc_malloc(sizeof(struct port_info_t)));
      SET_PORT_CLASS(s, cls);

      PORT_PINFO(s)->port_name = port_name;
      PORT_PINFO(s)->user_data = user_data;
      PORT_PINFO(s)->user_object = user_object;
      PORT_PINFO(s)->mode = mode;

      SET_PORT_TEXT_INFO(s, NULL);;

      if (PORT_CLASS(s)->open)
           PORT_CLASS(s)->open(s);

      return s;
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
      if (NULLP(port))
           port = CURRENT_OUTPUT_PORT();

      assert(!NULLP(port));
      assert(PORT_CLASS(port)->write_bytes);

      return PORT_CLASS(port)->write_bytes(port, buf, size);
 }

 size_t read_bytes(lref_t port, void *buf, size_t size)
 {
      if (NULLP(port))
           port = CURRENT_INPUT_PORT();

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
      lref_t s = new_cell(TC_PORT);

      return initialize_port(s, cls, port_name, mode, user_object, user_data);
 }

 /***** C I/O functions *****/

 bool read_binary_fixnum(fixnum_t length, bool signedp, lref_t port, fixnum_t *result)
 {
 #ifdef SCAN_64BIT
      assert((length == 1) || (length == 2) || (length == 4) || (length == 8));
 #else
      assert((length == 1) || (length == 2) || (length == 4));
 #endif

      assert(PORTP(port));
      assert(PORT_BINARYP(port));

      uint8_t bytes[sizeof(fixnum_t)];
      size_t fixnums_read = read_bytes(port, bytes, (size_t)length);

      if (!fixnums_read)
           return false;


      switch (length)
      {
      case 1:
           *result = (signedp ? (fixnum_t) (*(int8_t *) bytes) : (fixnum_t) (*(uint8_t *) bytes));
           break;
      case 2:
           *result = (signedp ? (fixnum_t) (*(int16_t *) bytes) : (fixnum_t) (*(uint16_t *) bytes));
           break;
      case 4:
           *result = (signedp ? (fixnum_t) (*(int32_t *) bytes) : (fixnum_t) (*(uint32_t *) bytes));
           break;
 #ifdef SCAN_64BIT
      case 8:
           *result = (signedp ? (fixnum_t) (*(int64_t *) bytes) : (fixnum_t) (*(uint64_t *) bytes));
           break;
 #endif
      }

      return true;
 }


 bool read_binary_flonum(lref_t port, flonum_t *result)
 {
      assert(PORTP(port));
      assert(PORT_BINARYP(port));

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
      if (PORTP(obj) && PORT_BINARYP(obj))
           return obj;
      else
           return boolcons(false);
 }

lref_t lport_mode(lref_t obj)
{
     if (!PORTP(obj))
          vmerror_wrong_type_n(1, obj);

     if (PORT_CLOSEDP(obj))
          return keyword_intern(_T("closed"));
     else if (PORT_INPUTP(obj))
          return keyword_intern(_T("input"));
     else if (PORT_OUTPUTP(obj))
          return keyword_intern(_T("output"));

     panic(_T("corrupt port"));

     return NIL;
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

      if (!PORT_BINARYP(port)
          && PORT_TEXT_INFO(port)->translate
          && PORT_TEXT_INFO(port)->needs_lf)
      {
           write_char(_T('\n'), port);
      }

      if (PORT_CLASS(port)->flush)
           PORT_CLASS(port)->flush(port);

      return port;
 }


lref_t lread_binary_string(lref_t l, lref_t port)
{
     _TCHAR buf[STACK_STRBUF_LEN];

     if (NULLP(port))
          port = CURRENT_INPUT_PORT();
     else if (!PORTP(port))
          vmerror_wrong_type_n(2, port);

     assert(PORTP(port));

     if (!PORT_BINARYP(port))
          vmerror_unsupported(_T("raw port operations not supported on text ports"));

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

          str_append_str(new_str, buf, actual_read);

          remaining_length -= actual_read;
          total_read += actual_read;
     }

     if (total_read == 0)
          return lmake_eof();

     return new_str;
}


lref_t lread_binary_fixnum(lref_t l, lref_t sp, lref_t port)
{
     if (NULLP(port))
          port = CURRENT_INPUT_PORT();
     else if (!PORTP(port))
          vmerror_wrong_type_n(3, port);

     if (!PORT_BINARYP(port))
          vmerror_unsupported(_T("raw port operations not supported on text ports"));

     if (!NUMBERP(l))
          vmerror_wrong_type_n(1, l);
     if (!BOOLP(sp))
          vmerror_wrong_type_n(2, sp);

     fixnum_t length = get_c_fixnum(l);
     bool signedp = BOOLV(sp);

#ifdef SCAN_64BIT
     if ((length != 1) && (length != 2) && (length != 4) && (length != 8))
          vmerror_arg_out_of_range(l, _T("1, 2, 4, or 8"));
#else
     if ((length != 1) && (length != 2) && (length != 4))
          vmerror_arg_out_of_range(l, _T("1, 2, or 4"));
#endif

     fixnum_t result = 0;

     if (read_binary_fixnum(length, signedp, port, &result))
          return fixcons(result);
     else
          return lmake_eof();
}

lref_t lread_binary_flonum(lref_t port)
{
     if (NULLP(port))
          port = CURRENT_INPUT_PORT();
     else if (!PORTP(port))
          vmerror_wrong_type_n(3, port);

     if (!PORT_BINARYP(port))
          vmerror_unsupported(_T("raw port operations not supported on text ports"));

     flonum_t result = 0;

     if (read_binary_flonum(port, &result))
          return flocons(result);
     else
          return lmake_eof();
}


lref_t lwrite_binary_string(lref_t string, lref_t port)
{
     if (NULLP(port))
          port = CURRENT_OUTPUT_PORT();
     else if (!PORTP(port))
          vmerror_wrong_type_n(2, port);

     assert(PORTP(port));

     if (!PORT_BINARYP(port))
          vmerror_unsupported(_T("raw port operations not supported on text ports"));

     if (!STRINGP(string))
          vmerror_wrong_type_n(1, string);

     size_t length = STRING_DIM(string);
     _TCHAR *strdata = STRING_DATA(string);

     size_t chars_written = write_bytes(port, strdata, length * sizeof(_TCHAR));

     if (chars_written < length)
          vmerror_io_error(_T("error writing to port."), port);

     return fixcons(chars_written);
}

lref_t lwrite_binary_fixnum(lref_t v, lref_t l, lref_t sp, lref_t port)
{
     if (NULLP(port))
          port = CURRENT_OUTPUT_PORT();
     else if (!PORTP(port))
          vmerror_wrong_type_n(4, port);

     assert(PORTP(port));

     if (!PORT_BINARYP(port))
          vmerror_unsupported(_T("raw port operations not supported on text ports"));

     if (!FIXNUMP(v))
          vmerror_wrong_type_n(1, v);

     if (!NUMBERP(l))
          vmerror_wrong_type_n(2, l);

     if (!BOOLP(sp))
          vmerror_wrong_type_n(3, sp);

     fixnum_t length = get_c_fixnum(l);
     bool signedp = BOOLV(sp);

#ifdef SCAN_64BIT
     if ((length != 1) && (length != 2) && (length != 4) && (length != 8))
          vmerror_arg_out_of_range(l, _T("1, 2, 4, or 8"));
#else
     if ((length != 1) && (length != 2) && (length != 4))
          vmerror_arg_out_of_range(l, _T("1, 2, or 4"));
#endif

     fixnum_t val = FIXNM(v);

     uint8_t bytes[sizeof(fixnum_t)];

     switch (length)
     {
     case 1:
          if (signedp)
               *(int8_t *) bytes = (int8_t) val;
          else
               *(uint8_t *) bytes = (uint8_t) val;
          break;

     case 2:
          if (signedp)
               *(int16_t *) bytes = (int16_t) val;
          else
               *(uint16_t *) bytes = (uint16_t) val;
          break;

     case 4:
          if (signedp)
               *(int32_t *) bytes = (int32_t) val;
          else
               *(uint32_t *) bytes = (uint32_t) val;
          break;

#ifdef SCAN_64BIT
     case 8:
          if (signedp)
               *(int64_t *) bytes = (int64_t) val;
          else
               *(uint64_t *) bytes = (uint64_t) val;
          break;
#endif
     }

     size_t fixnums_written = write_bytes(port, bytes, (size_t)length);

     if (fixnums_written < 1)
          vmerror_io_error(_T("error writing to port."), port);

     return fixcons(fixnums_written);
}

lref_t lbinary_write_flonum(lref_t v, lref_t port)
{
     if (NULLP(port))
          port = CURRENT_OUTPUT_PORT();
     else if (!PORTP(port))
          vmerror_wrong_type_n(4, port);

     assert(PORTP(port));

     if (!PORT_BINARYP(port))
          vmerror_unsupported(_T("raw port operations not supported on text ports"));

     if (!NUMBERP(v))
          vmerror_wrong_type_n(1, v);

     flonum_t val = get_c_double(v);

     uint8_t bytes[sizeof(flonum_t)];

     *(flonum_t *) bytes = val;

     size_t flonums_written = write_bytes(port, bytes, sizeof(flonum_t));

     if (flonums_written < 1)
          vmerror_io_error(_T("error writing to port."), port);

     return fixcons(flonums_written);
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
     NULL,                  // rich_write
     NULL,                  // flush
     NULL,                  // close
     NULL,                  // gc_free
     NULL,                  // length
};


lref_t lopen_null_input_port()
{
     return portcons(&null_port_class,
                     NIL,
                     (enum port_mode_t) (PORT_INPUT | PORT_BINARY),
                     NIL,
                     NULL);
}

lref_t lopen_null_output_port()
{
     return portcons(&null_port_class,
                     NIL,
                     (enum port_mode_t) (PORT_OUTPUT | PORT_BINARY),
                     NIL,
                     NULL);
}

