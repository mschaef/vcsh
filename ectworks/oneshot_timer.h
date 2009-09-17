/* oneshot_timer.h
 * April 17th, 2006
 *
 * This is a oneshot timer object. It's used to schedule a function
 * to be called at a set time in the future. The scheduled call can
 * be arbitrarily rescheduled. */

#ifndef __ONESHOT_TIMER_H
#define __ONESHOT_TIMER_H

#include <scan-assert.h>
#include <scan-types.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef void(* oneshot_timer_fn_t)(void *);

struct oneshot_timer_t;

oneshot_timer_t *oneshot_timer_create(oneshot_timer_fn_t timer_elapsed_fn, void *timer_elapsed_fn_data);
void oneshot_timer_set(oneshot_timer_t *oneshot, i64 due_time);

void oneshot_timer_cancel(oneshot_timer_t *oneshot);
void oneshot_timer_destroy(oneshot_timer_t *oneshot);

#ifdef __cplusplus
} 
#endif


#endif // __ONESHOT_TIMER_H
