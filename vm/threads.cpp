/* threads.cpp
 *
 * Lisp thread support.
 */

#include "scan.h"

namespace scan {

  interpreter_thread_info_block_t *allocate_thread_info_block()
  {
    interpreter_thread_info_block_t *table_entry = NULL;

    sys_enter_critical_section(interp.thread_table_crit_sec);
    {
      for(size_t ii = 0; ii < MAX_THREADS; ii++)
        {
          table_entry = &(interp.thread_table[ii]);

          if (table_entry->state != THREAD_EMPTY)
            continue;

          table_entry->state = THREAD_STARTING;
          break;
        }
    }
    sys_leave_critical_section(interp.thread_table_crit_sec);

    return table_entry;
  }

  void free_thread_info_block(interpreter_thread_info_block_t *tib)
  {
    gc_release_freelist(tib->freelist);
    tib->freelist = NULL;

    tib->state = THREAD_EMPTY;
  }

  void interp_thread_main(void *userdata)
  {
    interpreter_thread_info_block_t *tib = (interpreter_thread_info_block_t *)userdata;

    thread_entry_t entry = tib->entry;
    void *actual_arglist = tib->arglist;

    assert(tib->state == THREAD_STARTING);

    dscwritef(DF_SHOW_THREADS, ";;; ENTERING INTERPRETER THREAD, thid=~c&\n", sys_current_thread());

    gc_register_thread(tib);

    tib->frame_stack = NULL;
    tib->handler_frames = NIL;
    gc_protect(_T("handler-frames"), &(tib->handler_frames), 1);

    entry(actual_arglist);
    
    free_thread_info_block(tib);

    dscwritef(DF_SHOW_THREADS, ";;; LEAVING INTERPRETER THREAD, thid=~c&", sys_current_thread());
  }

  sys_thread_t interp_create_thread(thread_entry_t entry, void *arglist)
  {
    interpreter_thread_info_block_t *tib = allocate_thread_info_block();

    if (tib == NULL)
      return NULL;

    tib->entry              = entry;
    tib->arglist            = arglist;

    sys_thread_t sys_th = sys_create_thread(interp_thread_main, THREAD_DEFAULT_STACK_SIZE, tib);

    if (sys_th == NULL)
      free_thread_info_block(tib);

    return sys_th;
  }
}
