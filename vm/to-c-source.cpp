
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

#include "base-types.h"

struct write_state
{
     FILE *_out;
     size_t _bytes_transferred;
};

void write_bytes_as_c_source(const void *buf, size_t bytes, write_state * ws)
{
     for (size_t ii = 0; ii < bytes; ii++)
     {
          if ((ws->_bytes_transferred % 8) == 0)
               fprintf(ws->_out, "\n          ");
          else
               fprintf(ws->_out, " ");

          fprintf(ws->_out, "0x%02x,", ((uint8_t *) buf)[ii]);

          ws->_bytes_transferred++;
     }
}

#define BLOCK_SIZE (256)

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

void write_file_as_c_source(FILE * in, FILE * out, _TCHAR * blockname, _TCHAR * varname)
{
     write_state s;

     s._out = out;
     s._bytes_transferred = 0;

     fprintf(out, "struct scan::internal_file_t %s =\n", varname);
     fprintf(out, "{\n");
     fprintf(out, "     _T(\"%s\"), %" PRINTF_PREFIX_SIZE_T "d,\n",
             blockname, file_length(in));

     fprintf(out, "     INTERNAL_FILE_DATA_CAST {");

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

     fprintf(out, "\n     }\n");

     fprintf(out, "};\n");
}

int main(int argc, char *argv[])        /* REVISIT: Enhance to map n files to 1 source file */
{
     if (argc != 3)
     {
          fprintf(stderr, "Usage: %s <filename> <varname>\n", argv[0]);
          return 1;
     }

     FILE *f = fopen(argv[1], "rb");

     if (f == NULL)
     {
          fprintf(stderr, "Error opening file %s\n", argv[1]);
          return 1;
     }

     write_file_as_c_source(f, stdout, argv[1], argv[2]);

     fclose(f);

     return 0;
}
