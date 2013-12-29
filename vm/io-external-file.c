
/*
 * io-external-file.cpp --
 *
 * An I/O port class used to access external files.
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include <stdio.h>

#include "scan-private.h"

/* C File Port ************************************************
 *
 * All the groovy stuff to specialize a port into a port capable
 * of talking to a file is contained herein. Thanks to the fact
 * that the C standard library's file I/O functions also talk
 * to STDIN/OUT/ERR, this also contains simple port definitions
 * for those devices, as well.
 *
 * state = FILE *
 * extended_state = Scheme object containing file name
 */
INLINE void SET_PORT_FILE(lref_t port, FILE * file)
{
     PORT_PINFO(port)->user_data = file;
}

INLINE FILE *PORT_FILE(lref_t port)
{
     return (FILE *) (PORT_PINFO(port)->user_data);
}

lref_t fileportcons(struct port_class_t * cls, enum port_mode_t mode, lref_t filename)
{
     assert(STRINGP(filename) || NULLP(filename));

     return portcons(cls, filename, mode, NIL, NULL);
}


void file_port_open(lref_t obj)
{
     FILE *f = NULL;

     assert(STRINGP(PORT_PINFO(obj)->port_name));

     if (PORT_OUTPUTP(obj))
          f = fopen(get_c_string(PORT_PINFO(obj)->port_name), "wb");
     else if (PORT_INPUTP(obj))
          f = fopen(get_c_string(PORT_PINFO(obj)->port_name), "rb");
     else
          panic("File port open for closed port");

     if (f)
     {

          SET_PORT_FILE(obj, f);
     }
     else
     {
          SET_PORT_MODE(obj, PORT_CLOSED);
          vmerror_io_error(_T("cannot open file"), PORT_PINFO(obj)->port_name);
     }
}


size_t file_port_read_bytes(lref_t port, void *buf, size_t size)
{
     FILE *f = PORT_FILE(port);

     assert(f);

     return fread(buf, 1, size, f);
}

size_t file_port_write_bytes(lref_t port, const void *buf, size_t size)
{
     FILE *f = PORT_FILE(port);

     assert(f);

     return fwrite(buf, 1, size, f);
}

void file_port_flush(lref_t port)
{
     FILE *f = PORT_FILE(port);

     assert(f);

     fflush(f);
}

void file_port_close(lref_t port)
{
     FILE *f = PORT_FILE(port);;

     if (f == NULL)
          return;

     fclose(f);

     SET_PORT_FILE(port, NULL);
}


struct port_class_t file_port_class = {
     _T("STANDARD-FILE"),

     file_port_open,        // open
     file_port_read_bytes,  // read_bytes
     file_port_write_bytes, // write_bytes
     NULL,                  // rich_write
     file_port_flush,       // flush
     file_port_close,       // close
     NULL,                  // gc_free
     NULL,                  // length
};

bool get_c_port_mode(lref_t mode)
{
     if (NULLP(mode))
          return false;

     if (!SYMBOLP(mode))
          vmerror_wrong_type_n(2, mode);

     if (keyword_intern(_T("binary")) == mode)
          return true;
     else if (keyword_intern(_T("text")) == mode)
          return false;
     else
          vmerror_arg_out_of_range(mode, _T(":binary or :text"));

     return false;
}

lref_t lopen_raw_input_file(lref_t filename)
{
     if (!STRINGP(filename))
          vmerror_wrong_type_n(1, filename);

     return fileportcons(&file_port_class,
                         (enum port_mode_t)(PORT_INPUT | PORT_BINARY),
                         filename);
}

lref_t lopen_raw_output_file(lref_t filename) // TODO: Append Mode
{
     if (!STRINGP(filename))
          vmerror_wrong_type_n(1, filename);

     return fileportcons(&file_port_class,
                         (enum port_mode_t)(PORT_OUTPUT | PORT_BINARY),
                         filename);
}

/* Standard I/O ***********************************************
 *
 * These ports depend on some code from the C file ports to
 * do their thing.
 */

void stdio_port_close(lref_t obj)
{
     SET_PORT_FILE(obj, NULL);
}

void stdin_port_open(lref_t obj)
{
     SET_PORT_FILE(obj, stdin);
}

struct port_class_t stdin_port_class = {
     _T("STANDARD-INPUT"),

     stdin_port_open,        // open
     file_port_read_bytes,   // read_bytes
     NULL,                   // write_bytes
     NULL,                   // rich_write
     NULL,                   // flush
     stdio_port_close,       // close
     NULL,                   // gc_free
     NULL,                   // length
};

void stdout_port_open(lref_t obj)
{
     SET_PORT_FILE(obj, stdout);

     if (!PORT_BINARYP(obj))
          PORT_TEXT_INFO(obj)->translate = false;
}

struct port_class_t stdout_port_class = {
     _T("STANDARD-OUTPUT"),

     stdout_port_open,      // open
     NULL,                  // read_bytes
     file_port_write_bytes, // write_bytes
     NULL,                  // rich_write
     file_port_flush,       // flush
     stdio_port_close,      // close
     NULL,                  // gc_free
     NULL,                  // length
};

void stderr_port_open(lref_t obj)
{
     SET_PORT_FILE(obj, stderr);

     if (!PORT_BINARYP(obj))
          PORT_TEXT_INFO(obj)->translate = false;
}

struct port_class_t stderr_port_class = {
     _T("STANDARD-ERROR"),

     stderr_port_open,      // open
     NULL,                  // read_bytes
     file_port_write_bytes, // write_bytes
     NULL,                  // rich_write
     file_port_flush,       // flusn
     stdio_port_close,      // close
     NULL,                  // gc_free
     NULL,                  // length
};


void init_stdio_ports()
{
     lref_t stdin_port =
          lopen_text_input_port(fileportcons(&stdin_port_class,
                                             (enum port_mode_t)(PORT_INPUT | PORT_BINARY),
                                             strcons(_T("<stdin>"))));

     lref_t stdout_port =
          lopen_text_output_port(fileportcons(&stdout_port_class,
                                              (enum port_mode_t)(PORT_OUTPUT | PORT_BINARY),
p                                              strcons(_T("<stdout>"))));

     lref_t stderr_port =
          lopen_text_output_port(fileportcons(&stderr_port_class,
                                              (enum port_mode_t)(PORT_OUTPUT | PORT_BINARY),
                                              strcons(_T("<stderr>"))));

     interp.control_fields[VMCTRL_CURRENT_INPUT_PORT] = stdin_port;
     interp.control_fields[VMCTRL_CURRENT_OUTPUT_PORT] = stdout_port;
     interp.control_fields[VMCTRL_CURRENT_ERROR_PORT] = stderr_port;
     interp.control_fields[VMCTRL_CURRENT_DEBUG_PORT] = stderr_port;
}


