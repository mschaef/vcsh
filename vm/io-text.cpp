
 /*
  * io-text.cpp --
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

 BEGIN_NAMESPACE(scan)

/*** Text port object ***/

size_t text_port_read(void *buf, size_t size, size_t count, lref_t obj)
{
     assert(PORTP(PORT_USER_OBJECT(obj)));

     return read_raw(buf, size, count, PORT_USER_OBJECT(obj));
}

size_t text_port_write(const void *buf, size_t size, size_t count, lref_t obj)
{
     assert(PORTP(PORT_USER_OBJECT(obj)));

     return write_raw(buf, size, count, PORT_USER_OBJECT(obj));
}

void text_port_flush(lref_t obj)
{
     assert(PORTP(PORT_USER_OBJECT(obj)));

     lflush_port(PORT_USER_OBJECT(obj));
}

void text_port_close(lref_t obj)
{
     assert(PORTP(PORT_USER_OBJECT(obj)));

     lclose_port(PORT_USER_OBJECT(obj));
}

port_class_t text_port_class = {
     _T("TEXT"),

     NULL,

     NULL,
     text_port_read,

     text_port_write,
     NULL,

     text_port_flush,
     text_port_close,
     NULL,

     NULL
};

lref_t lopen_text_input_port(lref_t underlying)
{
     if (!PORTP(underlying))
          vmerror_wrong_type(1, underlying);

     if (!PORT_BINARYP(underlying))
          vmerror_unsupported(_T("cannot open text input on text port"));

     return portcons(&text_port_class,
                     lport_name(underlying),
                     PORT_INPUT,
                     underlying,
                     NULL);
}

lref_t lopen_text_output_port(lref_t underlying)
{
     if (!PORTP(underlying))
          vmerror_wrong_type(1, underlying);

     if (!PORT_BINARYP(underlying))
          vmerror_unsupported(_T("cannot open text output on text port"));

     return portcons(&text_port_class,
                     lport_name(underlying),
                     PORT_OUTPUT,
                     underlying,
                     NULL);
}

END_NAMESPACE
