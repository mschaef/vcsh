
/* vcsh.cpp
 *
 * The vcalc console's main 'shell' program.
 */

#include <signal.h>

#include "../vm/scan.h"

void sigint_handler(int i)
{
     UNREFERENCED(i);

     scan::signal_interrupt(scan::VMINTR_BREAK);

     signal(SIGINT, sigint_handler);
};

int _tmain(int argc, _TCHAR * argv[])
{
     scan::sys_init();
     scan::init(argc, argv, scan::DF_NONE);

     signal(SIGINT, sigint_handler);

     scan::LRef retval = scan::run();

     long return_status = 0;

     if (scan::NUMBERP(retval))
          return_status = scan::get_c_long(retval);

     if (DEBUGGING_BUILD)
          scwritef(_T(";;;; end run, rc=~cd\n"), scan::CURRENT_DEBUG_PORT(), return_status);

     scan::shutdown();

     exit((int) return_status);
}
