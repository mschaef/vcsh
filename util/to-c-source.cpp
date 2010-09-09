
/* to-c-source
 * July 4th, 2007
 *
 * A utility for dumping the contents of a file into something that can
 * be read as a variable declaration by a c compiler.
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
               fprintf(ws->_out, "\n   ");

          fprintf(ws->_out, "0x%02x, ", ((u8 *) buf)[ii]);

          ws->_bytes_transferred++;
     }
}

#define BLOCK_SIZE (256)

void write_file_as_c_source(FILE * in, FILE * out, _TCHAR * varname)
{
     write_state s;

     s._out = out;
     s._bytes_transferred = 0;

     fprintf(out, "unsigned char %s[]= {", varname);

     u8 buf[BLOCK_SIZE];

     size_t total = 0;

     for (;;)
     {
          size_t bytes = fread(buf, 1, BLOCK_SIZE, in);

          if (bytes == 0)
               break;

          write_bytes_as_c_source(buf, bytes, &s);

          total += bytes;
     }

     fprintf(out, "};\n\nunsigned int %s_bytes = %zi;\n", varname, s._bytes_transferred);
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

     write_file_as_c_source(f, stdout, argv[2]);

     fclose(f);

     return 0;
}
