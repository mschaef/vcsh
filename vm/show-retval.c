/*
 * show-retval.c --
 *
 * Utility to show the value returned by a given command.
 *
 * (C) Copyright 2001-2022 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "LICENSE" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include <stdio.h>
#include <stdlib.h>

#include "scan-base.h"

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
