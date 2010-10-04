
/* io-internal-file.cpp
 *
 * An input port used to access 'internal files'... internal files are
 * files that are built into the executable as part of the build process.
 */

#include "scan.h"

BEGIN_NAMESPACE(scan)
struct c_data_port_state
{
     size_t _bytes_transferred;
     unsigned char *_input_buffer;
     size_t _input_buffer_bytes;
};


size_t c_data_port_read(void *buf, size_t size, size_t count, LRef obj)
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

void c_data_port_gc_free(LRef obj)
{
     assert(PORT_PINFO(obj)->_user_data);

     safe_free(PORT_PINFO(obj)->_user_data);
     PORT_PINFO(obj)->_user_data = NULL;
}

size_t c_data_port_length(LRef obj)
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

LRef lopen_c_data_output(LRef destination, LRef var_name, LRef mode)
{
     bool binary = get_c_port_mode(mode);

     if (!PORTP(destination))
          vmerror_wrong_type(1, destination);

     if (!STRINGP(var_name))
          vmerror_wrong_type(2, var_name);

     if (!TRUEP(loutput_portp(destination)))
          vmerror("Cannot write c-data to input-only ports", destination);

     if (TRUEP(lbinary_portp(destination)))
          vmerror("Cannot write c-data to binary ports", destination);

     c_data_port_state *ps = (c_data_port_state *) safe_malloc(sizeof(c_data_port_state));

     ps->_bytes_transferred = 0;
     ps->_input_buffer = NULL;
     ps->_input_buffer_bytes = 0;

     return portcons(&c_data_port_class, var_name,
                     (port_mode_t) (PORT_OUTPUT | (binary ? PORT_BINARY : 0)), destination, ps);
}

void register_internal_file(const _TCHAR * filename, bool binary_data, data_block_t *data)
{
     LRef file_record = lcons(strcons(filename), open_c_data_input(binary_data, data));

     interp.internal_files = lcons(file_record, interp.internal_files);
                     
}

LRef liinternal_files()
{
     return interp.internal_files;
}

LRef open_c_data_input(bool binary_data, data_block_t *data)
{
     c_data_port_state *ps = (c_data_port_state *) safe_malloc(sizeof(c_data_port_state));

     ps->_bytes_transferred  = 0;
     ps->_input_buffer       = data->_bytes;
     ps->_input_buffer_bytes = data->_length;

     return portcons(&c_data_port_class, NIL,
                     (port_mode_t) (PORT_INPUT | (binary_data ? PORT_BINARY : 0)), NIL, ps);
}

LRef lclone_c_data_port(LRef port)
{
     if (!PORTP(port))
          vmerror_wrong_type(1, port);

     if (!(PORT_MODE(port) & PORT_INPUT) || (PORT_CLASS(port) != &c_data_port_class))
          vmerror("Cannot clone any kind of port other than a c-data input port.", port);

     c_data_port_state *old_ps = (c_data_port_state *) (PORT_PINFO(port)->_user_data);
     c_data_port_state *new_ps = (c_data_port_state *) safe_malloc(sizeof(c_data_port_state));

     new_ps->_bytes_transferred = old_ps->_bytes_transferred;
     new_ps->_input_buffer = old_ps->_input_buffer;
     new_ps->_input_buffer_bytes = old_ps->_input_buffer_bytes;

     return portcons(&c_data_port_class, NIL, PORT_MODE(port), NIL, new_ps);
}


END_NAMESPACE
