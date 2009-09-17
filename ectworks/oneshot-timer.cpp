/* oneshot_timer.cpp
 * April 17th, 2006
 *
 * This is a oneshot timer object. It's used to schedule a function
 * to be called at a set time in the future. The scheduled call can
 * be arbitrarily rescheduled. */

#include "stdafx.h"
#include <windows.h>
#include <process.h>
#include "oneshot_timer.h"


typedef void(* oneshot_timer_fn_t)(void *);

struct oneshot_timer_t
{
	LARGE_INTEGER _due_time;

	HANDLE _timer_object;
	HANDLE _destroy_event;
	HANDLE _reset_event;

	oneshot_timer_fn_t _timer_elapsed_fn;
	void *_timer_elapsed_fn_data;
};


void oneshot_timer_thread_fn(void *timer_thread_args)
{
	oneshot_timer_t *oneshot = (oneshot_timer_t *)timer_thread_args;

	HANDLE handles[3];

	handles[0] = oneshot->_timer_object;
	handles[1] = oneshot->_destroy_event;
	handles[2] = oneshot->_reset_event;
	
	bool running = true;

	while(running)
	{
		DWORD status = WaitForMultipleObjects(3, handles, false, INFINITE);

		bool timer_elapsed = false;

		LARGE_INTEGER now;
		GetSystemTimeAsFileTime((LPFILETIME)&now);
	
		switch(status)
		{
		case WAIT_OBJECT_0 + 0: // timer object
			timer_elapsed = true;
			break;

		case WAIT_OBJECT_0 + 1: // shutdown event
			running = false;
			break;

		case WAIT_OBJECT_0 + 2: // reset event
			if (now.QuadPart >= oneshot->_due_time.QuadPart)
				timer_elapsed = true;
			else
				SetWaitableTimer(oneshot->_timer_object, &oneshot->_due_time, 0, NULL, NULL, FALSE);
			break;

		default:
		case WAIT_FAILED:
			assert(!"Wait failed");
			break;
		}

		if (timer_elapsed)
			oneshot->_timer_elapsed_fn(oneshot->_timer_elapsed_fn_data);
	}


	CloseHandle(CreateEvent(NULL, FALSE, FALSE, NULL));
	CloseHandle(CreateEvent(NULL, FALSE, FALSE, NULL));
	CloseHandle(CreateWaitableTimer(NULL, FALSE, NULL));

	free(oneshot);
}

oneshot_timer_t *oneshot_timer_create(oneshot_timer_fn_t timer_elapsed_fn, void *timer_elapsed_fn_data)
{
	oneshot_timer_t *new_timer = NULL;

	new_timer = (oneshot_timer_t *)malloc(sizeof(oneshot_timer_t));

	if (!new_timer)
		return NULL;

	memset(new_timer, 0, sizeof(oneshot_timer_t));

	new_timer->_timer_elapsed_fn		= timer_elapsed_fn;
	new_timer->_timer_elapsed_fn_data	= timer_elapsed_fn_data;

	new_timer->_reset_event		= CreateEvent(NULL, FALSE, FALSE, NULL);
	new_timer->_destroy_event	= CreateEvent(NULL, FALSE, FALSE, NULL);
	new_timer->_timer_object	= CreateWaitableTimer(NULL, FALSE, NULL);

	_beginthread(oneshot_timer_thread_fn, 0, new_timer);

	return new_timer;
}

void oneshot_timer_set(oneshot_timer_t *oneshot, i64 due_time)
{
	oneshot->_due_time.QuadPart = due_time;

	SetEvent(oneshot->_reset_event);
}


void oneshot_timer_cancel(oneshot_timer_t *oneshot)
{
	CancelWaitableTimer(oneshot->_timer_object);
}

void oneshot_timer_destroy(oneshot_timer_t *oneshot)
{
	SetEvent(oneshot->_destroy_event);	
}
