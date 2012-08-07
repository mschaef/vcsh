/* io-internal-file.cpp --
 *
 * An input port used to access 'internal files'... internal files are
 * files that are built into the executable as part of the build process.
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include <memory.h>

#include "scan-private.h"

BEGIN_NAMESPACE(scan)
struct c_data_port_state
{
     unsigned char *buf;
     size_t buf_size;

     size_t buf_pos;
};

size_t c_data_port_read_bytes(lref_t port, void *buf, size_t size)
{
     if (!PORT_INPUTP(port))
          return 0;

     c_data_port_state *ps = (c_data_port_state *) (PORT_PINFO(port)->user_data);

     size_t buf_remain = ps->buf_size - ps->buf_pos;

     if (size > buf_remain)
          size = buf_remain;

     memcpy(buf, &(ps->buf[ps->buf_pos]), size);

     ps->buf_pos += size;

     return size;
}

void c_data_port_gc_free(lref_t obj)
{
     assert(PORT_PINFO(obj)->user_data);

     gc_free(PORT_PINFO(obj)->user_data);

     PORT_PINFO(obj)->user_data = NULL;
}

size_t c_data_port_length(lref_t obj)
{
     if (!PORT_INPUTP(obj))
          return 0;

     c_data_port_state *ps = (c_data_port_state *) (PORT_PINFO(obj)->user_data);

     return ps->buf_size - ps->buf_pos;
}

port_class_t c_data_port_class = {
     _T("C-DATA"),

     NULL,                   // open
     c_data_port_read_bytes, // read_bytes
     NULL,                   // write_bytes
     NULL,                   // rich_write
     NULL,                   // flush
     NULL,                   // close
     NULL,                   // gc_free
     c_data_port_length,     // length
};


lref_t liinternal_files()
{
     return interp.internal_files;
}

lref_t open_c_data_input(internal_file_t *data)
{
     c_data_port_state *ps = (c_data_port_state *) gc_malloc(sizeof(c_data_port_state));

     ps->buf      = data->_bytes;
     ps->buf_size = data->_length;
     ps->buf_pos  = 0;

     return portcons(&c_data_port_class,
                     NIL,
                     (port_mode_t) (PORT_INPUT | PORT_BINARY),
                     NIL,
                     ps);
}

void register_internal_file(internal_file_t *data)
{
     lref_t file_record = lcons(strcons(data->_name), open_c_data_input(data));

     interp.internal_files = lcons(file_record, interp.internal_files);
}

lref_t lclone_c_data_port(lref_t port)
{
     if (!PORTP(port))
          vmerror_wrong_type(1, port);

     if (PORT_CLASS(port) != &c_data_port_class)
          vmerror_unsupported(_T("only c-data ports may be cloned"));

     if (!PORT_INPUTP(port))
          vmerror_unsupported(_T("only input ports may be cloned"));

     c_data_port_state *old_ps = (c_data_port_state *) (PORT_PINFO(port)->user_data);
     c_data_port_state *new_ps = (c_data_port_state *) gc_malloc(sizeof(c_data_port_state));

     new_ps->buf      = old_ps->buf;
     new_ps->buf_size = old_ps->buf_size;
     new_ps->buf_pos  = old_ps->buf_pos;

     return portcons(&c_data_port_class, NIL, PORT_MODE(port), NIL, new_ps);
}


END_NAMESPACE
