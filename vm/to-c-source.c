
/*
 * to-c-source.cpp --
 *
 * A utility for dumping the contents of a file into source text that
 * can be read as a variable declaration by a c compiler.
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include <stdio.h>

#include "scan-base.h"

enum
{
     /* The block size used to read input files. */
     BLOCK_SIZE = 256,

     /* The length of the buffer used to hold the output variable name. */
     NAME_BUF_SIZE = 32,
};

struct write_state
{
     size_t _bytes_transferred;
};

void write_bytes_as_c_source(const void *buf, size_t bytes, struct write_state * ws)
{
     for (size_t ii = 0; ii < bytes; ii++)
     {
          if ((ws->_bytes_transferred % 8) == 0)
               printf("\n          ");
          else
               printf(" ");

          printf("0x%02x,", ((uint8_t *) buf)[ii]);

          ws->_bytes_transferred++;
     }
}

size_t file_length(FILE * in)
{
     uint8_t buf[BLOCK_SIZE];

     size_t total = 0;

     for (;;)
     {
          size_t bytes = fread(buf, 1, BLOCK_SIZE, in);

          if (bytes == 0)
               break;

          total += bytes;
     }

     rewind(in);

     return total;
}

void write_file_as_c_source(FILE * in, _TCHAR * blockname, _TCHAR * varname)
{
     struct write_state s;

     s._bytes_transferred = 0;

     printf("DECL_INTERNAL_FILE %s =\n", varname);
     printf("{\n");
     // TODO: The (_TCHAR *) cast eliminates a gcc warning about a deprecated
     // conversion of string constant into a _TCHAR *. Investigate if there's
     // a better way to do this.
     printf("     ((_TCHAR *)_T(\"%s\")), %" PRINTF_PREFIX_SIZE_T "d,\n",
             blockname, file_length(in));

     printf("     INTERNAL_FILE_DATA_CAST {");

     uint8_t buf[BLOCK_SIZE];

     size_t total = 0;

     for (;;)
     {
          size_t bytes = fread(buf, 1, BLOCK_SIZE, in);

          if (bytes == 0)
               break;

          write_bytes_as_c_source(buf, bytes, &s);

          total += bytes;
     }

     printf("\n     }\n");

     printf("};\n");
     printf("\n");
}

bool valid_variable_character(_TCHAR ch)
{
     return _istalpha(ch)
          || _istdigit(ch)
          || (ch == _T('_'))
          || (ch == _T('\0'));
}

void find_variable_name(_TCHAR *name_buf, _TCHAR *filename, size_t name_buf_len)
{
     _sntprintf(name_buf, name_buf_len, "ifile_%s", filename);

     for(size_t ii = 0; ii < name_buf_len; ii++)
     {
          if (valid_variable_character(name_buf[ii]))
               continue;

          name_buf[ii] = _T('_');
     }
}

int _tmain(int argc, _TCHAR* argv[])
{
     if (argc < 2)
     {
          fprintf(stderr, "Usage: %s <filename-1> ... <filename-n>\n", argv[0]);
          return 1;
     }

     for(int ii = 1; ii < argc; ii++)
     {
          _TCHAR *filename = argv[ii];
          _TCHAR varname_buf[NAME_BUF_SIZE];

          find_variable_name(varname_buf, filename, NAME_BUF_SIZE);

          FILE *f = fopen(filename, "rb");

          if (f == NULL)
          {
               fprintf(stderr, "Error opening file: %s\n", filename);
               return 1;
          }

          write_file_as_c_source(f, filename, varname_buf);

          fclose(f);
     }

     return 0;
}
