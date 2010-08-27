/* scansh0.cpp
 * January 14th, 2006
 *
 * This is the scheme interpreter used during bootstrap. It
 * is as simple as possible: it has no internal scheme code
 * and all it does is load the files named at the command
 * line one after another.
 */

#include "scan.h"

void sigint_handler(int i)
{
  UNREFERENCED(i);

  scan::signal_break();

  signal(SIGINT, sigint_handler);
};


int _tmain(int argc, _TCHAR* argv[])
{
  scan::sys_init();
  scan::init0(argc, argv, scan::DF_NONE);

  signal(SIGINT, sigint_handler);

  fprintf(stderr, ";;; scansh0 - %s\n", scan::build_id_string());

  SET_SYMBOL_VCELL(scan::interp.sym_port_current_err, SYMBOL_VCELL(scan::interp.sym_port_current_out));
  SET_SYMBOL_VCELL(scan::interp.sym_port_debug, SYMBOL_VCELL(scan::interp.sym_port_current_err));

  scan::LRef retval = scan::run();

  long return_status = 0;

  if (scan::NUMBERP(retval))
    return_status = scan::get_c_long(retval);

  int rc = (int)(FIXNUMP(retval) ? get_c_fixnum(retval) : 0);

  scan::shutdown();

  fprintf(stderr, "\n;;; scansh0 return code = %d\n", rc);

  exit(rc);
}

