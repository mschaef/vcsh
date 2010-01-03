/* vcalc.h
 * January 2003
 *
 * VCalc application header file
 */

#ifndef __VCALC_H
#define __VCALC_H

#include "../vm/scan.h"

#include <tchar.h>

#include "../util/base-assert.h"

#include "resource.h"		

#include "../ectworks/TimeoutRegistration.h"
#include "../ectworks/CAppBranding.h"

#include "image.h"
#include "console-window.h"

/**************************************************************
 * CVCalcBranding
 */

class CVCalcBranding : public CAppBranding
{
public:
	virtual UINT GetSmallBannerImage();
	virtual UINT GetLargeBannerImage();

	virtual CString GetPurchaseURL(CString serialNumber);
	virtual CString GetContactURL(CString serialNumber);

	virtual CString GetLicenseText();
	virtual CString GetApplicationVersion();
};

/**************************************************************
 * CVCalcApp:
 *
 * See vcalc.cpp for the implementation of this class
 */

class CVCalcApp : public CWinApp
{
	DECLARE_MESSAGE_MAP()

public:
	CVCalcApp();

	CLispConsole m_console;	

	TimeoutRegistration m_registration;
	CVCalcBranding m_branding;

private:
	bool m_fullFeatures;
	bool m_quitMessageReceived;

	bool m_throw_pending;
	scan::LRef m_pending_throw_retval;
	scan::LRef m_pending_throw_tag;

	bool m_busy_flag;

public:
	bool DelegateMessageToLispProcedure(_TCHAR *desc, scan::LRef closure, scan::LRef *retval, long n, ...);

	virtual BOOL InitInstance();
	virtual int ExitInstance();

	virtual int Run();
	virtual bool MessagePump(bool *endLoopFlag, bool exitWhenIdle);
	virtual BOOL OnIdle(LONG lCount);

	void ShowConsole();
	void HideConsole();
	bool GetRunMode();

	bool SetAppBusy(bool busyFlag);
	bool GetAppBusy();

	void SetEventTimer(flonum_t realtime);
	void CancelEventTimer();
};

inline CVCalcApp* VCalcGetApp() { return (CVCalcApp *)AfxGetApp(); }

/**************************************************************
 * This is the big, global instance
 */

extern CVCalcApp theApp;

/**************************************************************
 * Declarations for globally initialized variables containing
 * encrypted data.
 */
extern unsigned char vcInit[];
extern unsigned int vcInit_bytes;

extern unsigned char licenseTxt[];
extern unsigned int licenseTxtSize;

/* Information for managing keyboard events. */

#define NUM_KEYS (256)

extern scan::LRef sym_key[NUM_KEYS];

/* This is the public API */

/* Standard symbols */

extern scan::LRef sym_console_input_port;
extern scan::LRef sym_console_output_port;
extern scan::LRef sym_console_error_port;
extern scan::LRef sym_on_move;
extern scan::LRef sym_on_size;
extern scan::LRef sym_on_edit_keypress;
extern scan::LRef sym_on_keypress;
extern scan::LRef sym_on_command;
extern scan::LRef sym_on_update;
extern scan::LRef sym_on_destroy;
extern scan::LRef sym_surface;
extern scan::LRef sym_bitmap;
extern scan::LRef sym_initial_title;
extern scan::LRef sym_initial_placement;
extern scan::LRef sym_children;
extern scan::LRef sym_parent;
extern scan::LRef sym_window_type;

extern scan::LRef vcalc_package;

/* System utilties */

scan::LRef lsystem_string(scan::LRef keyword);
scan::LRef lchoose_file(scan::LRef parent, scan::LRef load_filep, scan::LRef filter_string, scan::LRef default_extension, scan::LRef default_filename);
scan::LRef lbeep();
scan::LRef lexit_application();
scan::LRef lyes_or_nop(scan::LRef prompt, scan::LRef title);
scan::LRef lmessage(scan::LRef prompt, scan::LRef title);
scan::LRef lget_clipboard_formats();
scan::LRef lset_clipboard_data(scan::LRef s);
scan::LRef lget_clipboard_data(scan::LRef format_name);
scan::LRef lget_user_file_path(scan::LRef filename);

/* Tip of the day window */
scan::LRef lshow_tip(scan::LRef parent, scan::LRef state);

/* Choose Window */

scan::LRef lchoose(scan::LRef parent, scan::LRef choose_list,  scan::LRef title,  scan::LRef description);

/* Config dialog box */

scan::LRef lshow_config_dialog(scan::LRef parent, scan::LRef config_vector, scan::LRef changed_callback);

/* Text editor */
scan::LRef ledit_text(scan::LRef parent, scan::LRef text, scan::LRef reformatter);

/* Window creation */
scan::LRef limake_window(scan::LRef host_instance, scan::LRef parent_peer);

class ILispInstance
{
 public:
  virtual scan::LRef GetInstanceSlotValue(scan::LRef slotName, scan::LRef defaultValue = 0) = 0;
  virtual scan::LRef SendInstanceMessage(scan::LRef message, scan::LRef args = 0) = 0;
};

#endif // __VCALC_H
