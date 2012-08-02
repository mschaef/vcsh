
/*
 * io-blocking-input.cpp --
 *
 * The blocking input port class.
 *
 * This port class allows data to be read from a blocking input:
 * an input that doesn't always have available data. To read
 * from its input, blocking input ports call a read_input function
 * that waits for input and passes it back to the port. While
 * waiting for available input the read function is free to do
 * as it wishes.
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

size_t blocking_input_port_read_bytes(lref_t port, void *buf, size_t size, size_t count);
void blocking_input_port_close(lref_t port);

port_class_t blocking_input_port_class = {
     _T("BLOCKING-INPUT"),

     NULL,                           // open
     blocking_input_port_read_bytes, // read_bytes
     NULL,                           // write_bytes
     NULL,                           // rich_write
     NULL,                           // flush
     blocking_input_port_close,      // close
     NULL,                           // gc_free
     NULL,                           // length
};



struct blocking_input_port_state
{
     blocking_input_read_data_fn_t _read_data;
     blocking_input_close_port_fn_t _close_port;

     size_t _buffer_size;
     size_t _buffer_pos;
     uint8_t *_buffer;

     void *_userdata;
     bool _more_data;
};

size_t blocking_input_port_read_bytes(lref_t port, void *buf, size_t size, size_t count)
{
     assert(PORTP(port) && (PORT_CLASS(port) == &blocking_input_port_class));

     if (!PORT_INPUTP(port))
          return 0;

     blocking_input_port_state *ps = (blocking_input_port_state *) (PORT_PINFO(port)->user_data);

     size_t bytes_to_read = size * count;
     size_t bytes_read = 0;

     while (bytes_to_read)
     {
          size_t bytes_available = ps->_buffer_size - ps->_buffer_pos;

          if (bytes_available == 0)
          {
               if (!ps->_more_data)
                    break;

               if (!ps->_read_data(port, ps->_userdata))
                    ps->_more_data = false;

               /*  Handle the case where the read callback actually closed the port on us. */
               if (PORT_CLOSEDP(port))
                    break;

               bytes_available = ps->_buffer_size - ps->_buffer_pos;
          }

          bytes_available = MIN2(bytes_available, bytes_to_read);

          if (bytes_available)
          {
               memcpy(&((uint8_t *) buf)[bytes_read], &((uint8_t *) ps->_buffer)[ps->_buffer_pos],
                      bytes_available);

               bytes_read += bytes_available;
               bytes_to_read -= bytes_available;
               ps->_buffer_pos += bytes_available;
          }
     }

     return bytes_read / size;
}

void blocking_input_port_close(lref_t port)
{
     assert(PORTP(port) && (PORT_CLASS(port) == &blocking_input_port_class));

     blocking_input_port_state *ps = (blocking_input_port_state *) (PORT_PINFO(port)->user_data);

     if (ps->_close_port)
          ps->_close_port(port, ps->_userdata);

     if (ps->_buffer)
          gc_free(ps->_buffer);

     gc_free(ps);

     PORT_PINFO(port)->user_data = NULL;

     SET_PORT_MODE(port, PORT_CLOSED);
}

void blocking_input_post_data(lref_t port, void *data, size_t size)
{
     assert(PORTP(port) && (PORT_CLASS(port) == &blocking_input_port_class));
     assert(!blocking_input_is_data_available(port));   /*  REVISIT: we really should allow this case */

     blocking_input_port_state *ps = (blocking_input_port_state *) (PORT_PINFO(port)->user_data);

     assert(ps->_more_data);

     if (ps->_buffer)
     {
          gc_free(ps->_buffer);

          ps->_buffer = NULL;
     }

     ps->_buffer = (uint8_t *) gc_malloc(size);

     memcpy(ps->_buffer, data, size);

     ps->_buffer_pos = 0;
     ps->_buffer_size = size;
}

void blocking_input_post_eof(lref_t port)
{
     assert(PORTP(port) && (PORT_CLASS(port) == &blocking_input_port_class));

     blocking_input_port_state *ps = (blocking_input_port_state *) (PORT_PINFO(port)->user_data);

     ps->_more_data = false;
}

bool blocking_input_is_data_available(lref_t port)
{
     assert(PORTP(port) && (PORT_CLASS(port) == &blocking_input_port_class));

     blocking_input_port_state *ps = (blocking_input_port_state *) (PORT_PINFO(port)->user_data);

     return (ps != NULL)
         && (ps->_buffer != NULL) && (ps->_buffer_size > 0) && (ps->_buffer_pos < ps->_buffer_size);
}


lref_t blocking_input_cons(const _TCHAR * port_name, bool binary,
                           blocking_input_read_data_fn_t read_fn,
                           blocking_input_close_port_fn_t close_fn, void *userdata)
{
     blocking_input_port_state *ps =
         (blocking_input_port_state *) gc_malloc(sizeof(blocking_input_port_state));

     ps->_read_data = read_fn;
     ps->_close_port = close_fn;
     ps->_buffer_size = 0;
     ps->_buffer_pos = 0;
     ps->_buffer = NULL;
     ps->_userdata = userdata;
     ps->_more_data = true;

     return portcons(&blocking_input_port_class,
                     strcons(port_name),
                     binary ? (port_mode_t) (PORT_INPUT | PORT_BINARY) : PORT_INPUT,
                     NIL,
                     ps);

}

END_NAMESPACE
