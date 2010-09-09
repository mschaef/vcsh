/* show-retval.cpp
 *
 * Utility to show the value returned by a given command.
 */


#include <stdio.h>
#include <stdlib.h>

#include "base-types.h"

int main(int argc, char *argv[])
{
  if (argc != 2)
    {
      fprintf(stderr, "Usage: %s <cmd-line>\n", argv[0]);
      return 1;
    }

  int retval = system(argv[1]);

  fprintf(stderr, "%d\n", retval);

  return retval;
}
