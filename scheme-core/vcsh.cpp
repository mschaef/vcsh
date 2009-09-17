/* vcsh.cpp
 *
 * The vcalc console's main 'shell' program.
 */

#include "../vm/scan.h"

void sigint_handler(int i)
{
  UNREFERENCED(i);

  scan::signal_break();

  signal(SIGINT, sigint_handler);
};

/* Split a command line, in argstr, into an argc/argv pair. */

int split_argstr(_TCHAR *argstr, _TCHAR **argv[])
{
  _TCHAR *loc;
  int max_argc = 1;
  int argstr_length = 0;

  /* Scan to establish a worst case number of arguments */
  for(loc = argstr; *loc;)
    {
      while (*loc && isspace(*loc)) 
        {
	  loc++;
	  argstr_length++;
        }

      if (*loc) max_argc++;

      while (*loc && !isspace(*loc)) 
        {
	  loc++;         
	  argstr_length++;
        }
    }

  /* Allocate argv - character storage is to the end. */
  assert(argv != NULL);
    
  if (max_argc == 1)
    {    
      *argv = NULL;
      return 0;
    }

  *argv = (_TCHAR **)scan::safe_malloc(  (sizeof(char *) * max_argc) // string pointers
                                         + argstr_length);             // string characters

  /* Start the argv array at the beginning of our block of storage,
   * place the character strings after the argv array. */

  _TCHAR **current_argv     = *argv;
  _TCHAR  *current_argv_str = ((_TCHAR *)(*argv) + (sizeof(_TCHAR *) * max_argc));

  /* Copy arguments into args */
  int argc      = 0;
  bool in_arg   = false;
  bool in_quote = false;
  bool escaped  = false;

  for(loc = argstr; *loc ; loc++)
    {
      if (in_quote)
        {            
	  if ((*loc == '\\') && !escaped)
	    escaped = true;
	  else
            {
	      if ((*loc == '\"') && !escaped)
		in_quote = false;
	      else
		*current_argv_str++ = *loc;

	      escaped = false;
            }

        }
      else if (isspace(*loc))
        {
	  if (in_arg)
	    *current_argv_str++ = '\0';

	  in_arg = false;
        }
      else 
        {
	  // we're not in a string, and haven't encountered a space

	  if (!in_arg)
            {
	      *current_argv = current_argv_str;
	      *current_argv++;
	      argc++;
            }
	  in_arg = true;

	  if (escaped)
            {
	      *current_argv_str++ = *loc;

	      escaped = false;
            }
	  else
            {
	      if (*loc == '\"')
		in_quote = true;
	      else if (*loc == '\\')
		escaped = true;
	      else
		*current_argv_str++ = *loc;
            }
        }
    }

  if (in_arg)
    *current_argv_str = '\0';

  assert(argc <= max_argc);

  return argc;
}

using namespace scan;

int _tmain(int argc, _TCHAR *argv[])
{
  scan::sys_init();
  scan::init(argc, argv, scan::DF_NONE);

  signal(SIGINT, sigint_handler);

  if (CHECKED_BUILD)
    scwritef(_T(";;;; VCSH, CHECKED Build (~cs)\n"), CURRENT_DEBUG_PORT, scan::build_id_string());
  else if (DEBUGGING_BUILD)
    scwritef(_T(";;;; VCSH, Debug Build (~cs)\n"), CURRENT_DEBUG_PORT, scan::build_id_string());

  scan::LRef retval = scan::run();

  long return_status = 0;

  if (scan::NUMBERP(retval))
    return_status = scan::get_c_long(retval);

  if (DEBUGGING_BUILD)
    scwritef(_T(";;;; end run, rc=~cd\n"), CURRENT_DEBUG_PORT, return_status);

  scan::shutdown();

  exit((int)return_status);
}

