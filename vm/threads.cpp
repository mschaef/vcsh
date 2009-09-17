/* threads.cpp
 *
 * Lisp thread support.
 */

#include "scan.h"

namespace scan {


  struct interp_thread_args_t
  {
    thread_entry_t entry;
    void *arglist;

    interpreter_thread_t **thread_table_entry;
  };

  static interpreter_thread_t **allocate_thread_table_entry()
  {
    interpreter_thread_t **table_entry = NULL;

    sys_enter_critical_section(interp.thread_table_crit_sec);
    {
      for(size_t ii = 0; ii < MAX_THREADS; ii++)
        {
          if (interp.thread_table[ii] == NULL)
            {
              table_entry = &(interp.thread_table[ii]);

              *table_entry = THREAD_INITIALIZING;

              break;
            }
        }
    }
    sys_leave_critical_section(interp.thread_table_crit_sec);

    return table_entry;
  }

  void interp_thread_main(void *arglist)
  {
    thread_entry_t entry                      = ((interp_thread_args_t *)arglist)->entry;
    void *actual_arglist                      = ((interp_thread_args_t *)arglist)->arglist;
    interpreter_thread_t **thread_table_entry = ((interp_thread_args_t *)arglist)->thread_table_entry;

    safe_free(arglist);

    assert(*thread_table_entry == THREAD_INITIALIZING);

    dscwritef(DF_SHOW_THREADS, ";;; ENTERING INTERPRETER THREAD, thid=~c&\n", sys_current_thread());

    sys_enter_critical_section(interp.thread_table_crit_sec);
    {
      *thread_table_entry = &thread;

      gc_register_thread(&thread);
    }
    sys_leave_critical_section(interp.thread_table_crit_sec);

    thread.frame_stack = NULL;
    thread.handler_frames = NIL;
    gc_protect(_T("handler-frames"), &thread.handler_frames, 1);

    entry(actual_arglist);

    gc_release_freelist(thread.freelist);
    thread.freelist = NULL;

    *thread_table_entry = NULL; // atomic, so no lock required

    dscwritef(DF_SHOW_THREADS, ";;; LEAVING INTERPRETER THREAD, thid=~c&", sys_current_thread());
  }

  sys_thread_t interp_create_thread(thread_entry_t entry, void *arglist)
  {
    interpreter_thread_t **table_entry = allocate_thread_table_entry();

    if (table_entry == NULL)
      return NULL;

    interp_thread_args_t *args = (interp_thread_args_t *)safe_malloc(sizeof(interp_thread_args_t));

    args->entry              = entry;
    args->arglist            = arglist;
    args->thread_table_entry = table_entry;

    return sys_create_thread(interp_thread_main, THREAD_DEFAULT_STACK_SIZE, args);
  }

}
