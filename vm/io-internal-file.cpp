
/*
 * io-internal-file.cpp --
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

#include "scan.h"

BEGIN_NAMESPACE(scan)
struct c_data_port_state
{
     size_t _bytes_transferred;
     unsigned char *_input_buffer;
     size_t _input_buffer_bytes;
};


size_t c_data_port_read(void *buf, size_t size, size_t count, lref_t obj)
{
     if (!(PORT_MODE(obj) & PORT_INPUT))
          return 0;

     c_data_port_state *ps = (c_data_port_state *) (PORT_PINFO(obj)->_user_data);

     size_t bytes = size * count;
     size_t bytes_remaining = ps->_input_buffer_bytes - ps->_bytes_transferred;

     if (bytes > bytes_remaining)
          bytes = (bytes_remaining / size) * size;

     memcpy(buf, &(ps->_input_buffer[ps->_bytes_transferred]), bytes);

     ps->_bytes_transferred += bytes;

     return bytes / size;
}

void c_data_port_gc_free(lref_t obj)
{
     assert(PORT_PINFO(obj)->_user_data);

     gc_free(PORT_PINFO(obj)->_user_data);
     PORT_PINFO(obj)->_user_data = NULL;
}

size_t c_data_port_length(lref_t obj)
{
     if (!(PORT_MODE(obj) & PORT_INPUT))
          return 0;

     c_data_port_state *ps = (c_data_port_state *) (PORT_PINFO(obj)->_user_data);

     return ps->_input_buffer_bytes - ps->_bytes_transferred;
}

port_class_t c_data_port_class = {
     _T("C-DATA"),
     PORT_INPUT,

     NULL,

     NULL,
     c_data_port_read,
     NULL,
     NULL,

     NULL,
     NULL,
     NULL,

     c_data_port_length,
};


lref_t liinternal_files()
{
     return interp.internal_files;
}

lref_t open_c_data_input(internal_file_t *data)
{
     c_data_port_state *ps = (c_data_port_state *) gc_malloc(sizeof(c_data_port_state));

     ps->_bytes_transferred  = 0;
     ps->_input_buffer       = data->_bytes;
     ps->_input_buffer_bytes = data->_length;

     return portcons(&c_data_port_class, NIL,
                     (port_mode_t) (PORT_INPUT | PORT_BINARY), NIL, ps);
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

     if (!(PORT_MODE(port) & PORT_INPUT) || (PORT_CLASS(port) != &c_data_port_class))
          vmerror_unsupported(_T("only c-data ports may be cloned"));

     c_data_port_state *old_ps = (c_data_port_state *) (PORT_PINFO(port)->_user_data);
     c_data_port_state *new_ps = (c_data_port_state *) gc_malloc(sizeof(c_data_port_state));

     new_ps->_bytes_transferred = old_ps->_bytes_transferred;
     new_ps->_input_buffer = old_ps->_input_buffer;
     new_ps->_input_buffer_bytes = old_ps->_input_buffer_bytes;

     return portcons(&c_data_port_class, NIL, PORT_MODE(port), NIL, new_ps);
}


END_NAMESPACE
