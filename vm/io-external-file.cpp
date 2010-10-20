
/* io-external-file.cpp
 *
 * An I/O port used to access external files. 
 */

#include "scan.h"

BEGIN_NAMESPACE(scan)

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
INLINE void SET_PORT_FILE(LRef port, FILE * file)
{
     PORT_PINFO(port)->_user_data = file;
}

INLINE FILE *PORT_FILE(LRef port)
{
     return (FILE *) (PORT_PINFO(port)->_user_data);
}

LRef fileportcons(port_class_t * cls, port_mode_t mode, LRef filename)
{
     assert(STRINGP(filename) || NULLP(filename));

     return portcons(cls, filename, mode, NIL, NULL);
}


void file_port_open(LRef obj)
{
     FILE *f = NULL;


     assert(STRINGP(PORT_PINFO(obj)->_port_name));

     if (PORT_MODE(obj) & PORT_OUTPUT)
          f = fopen(get_c_string(PORT_PINFO(obj)->_port_name), "wb");
     else if (PORT_MODE(obj) & PORT_INPUT)
          f = fopen(get_c_string(PORT_PINFO(obj)->_port_name), "rb");
     else
          panic("File port open for closed port");

     if (f)
     {

          SET_PORT_FILE(obj, f);
     }
     else
     {
          SET_PORT_MODE(obj, PORT_CLOSED);
          vmerror_io_error(_T("cannot open file"), PORT_PINFO(obj)->_port_name);
     }
}


size_t file_port_read(void *buf, size_t size, size_t count, LRef obj)
{
     FILE *f = PORT_FILE(obj);

     assert(f);

     return fread(buf, size, count, f);
}

size_t file_port_write(const void *buf, size_t size, size_t count, LRef obj)
{
     FILE *f = PORT_FILE(obj);

     assert(f);

     return (int) fwrite(buf, size, count, f);
}

int file_port_flush(LRef obj)
{
     FILE *f = PORT_FILE(obj);

     assert(f);

     return fflush(f);
}

void file_port_close(LRef obj)
{
     FILE *f = PORT_FILE(obj);

     if (f == NULL)
          return;

     fclose(f);
     SET_PORT_FILE(obj, NULL);
}


port_class_t file_port_class = {
     _T("STANDARD-FILE"),
     PORT_INPUT_OUTPUT,

     file_port_open,

     NULL,
     file_port_read,
     file_port_write,
     NULL,
     file_port_flush,

     file_port_close,
     NULL,

     NULL,
};

bool get_c_port_mode(LRef mode)
{
     if (NULLP(mode))
          return false;

     if (!SYMBOLP(mode))
          vmerror_wrong_type(2, mode);

     if (keyword_intern(_T("binary")) == mode)
          return true;
     else if (keyword_intern(_T("text")) == mode)
          return false;
     else
          vmerror_arg_out_of_range(mode, _T(":binary or :text"));

     return false;
}

LRef lopen_input_file(LRef filename, LRef mode)
{
     bool binary = get_c_port_mode(mode);

     if (!STRINGP(filename))
          vmerror_wrong_type(1, filename);

     return fileportcons(&file_port_class, (port_mode_t) (PORT_INPUT | (binary ? PORT_BINARY : 0)),
                         filename);
}

LRef lopen_output_file(LRef filename, LRef mode)
{
     bool binary = get_c_port_mode(mode);

     if (!STRINGP(filename))
          vmerror_wrong_type(1, filename);

     return fileportcons(&file_port_class, (port_mode_t) (PORT_OUTPUT | (binary ? PORT_BINARY : 0)),
                         filename);
}

/* Standard I/O ***********************************************
 *
 * These ports depend on some code from the C file ports to
 * do their thing.
 */

void stdio_port_close(LRef obj)
{
     SET_PORT_FILE(obj, NULL);
}

void stdin_port_open(LRef obj)
{
     SET_PORT_FILE(obj, stdin);
}

port_class_t stdin_port_class = {
     _T("STANDARD-INPUT"),
     PORT_INPUT,

     stdin_port_open,

     NULL,
     file_port_read,
     NULL,
     NULL,

     NULL,
     stdio_port_close,
     NULL,

     NULL,
};

void stdout_port_open(LRef obj)
{
     SET_PORT_FILE(obj, stdout);
}

port_class_t stdout_port_class = {
     _T("STANDARD-OUTPUT"),
     PORT_OUTPUT,

     stdout_port_open,

     NULL,
     NULL,
     file_port_write,
     NULL,
     file_port_flush,

     stdio_port_close,
     NULL,

     NULL,
};

void stderr_port_open(LRef obj)
{
     SET_PORT_FILE(obj, stderr);
}

port_class_t stderr_port_class = {
     _T("STANDARD-ERROR"),
     PORT_OUTPUT,

     stderr_port_open,

     NULL,
     NULL,
     file_port_write,
     NULL,
     file_port_flush,

     stdio_port_close,
     NULL,

     NULL,
};


void init_stdio_ports()
{
     LRef stdin_port = fileportcons(&stdin_port_class, PORT_INPUT, NIL);
     LRef stdout_port = fileportcons(&stdout_port_class, PORT_OUTPUT, NIL);
     LRef stderr_port = fileportcons(&stderr_port_class, PORT_OUTPUT, NIL);

     interp.control_fields[VMCTRL_CURRENT_INPUT_PORT] = stdin_port;
     interp.control_fields[VMCTRL_CURRENT_OUTPUT_PORT] = stdout_port;
     interp.control_fields[VMCTRL_CURRENT_ERROR_PORT] = stderr_port;
     interp.control_fields[VMCTRL_CURRENT_DEBUG_PORT] = stderr_port;
}


END_NAMESPACE
