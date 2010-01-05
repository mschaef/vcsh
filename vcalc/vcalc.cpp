// vcalc.cpp : Defines the class behaviors for the application.
//


#include "stdafx.h"
#include "vcalc.h"

#include "../ectworks/CAboutBox.h"

#include "image.h"
#include "console-window.h"
#include "VCWindow.h"
#include "VCConfigDialog.h"
#include "VCSystem.h"
#include "VCChooseBox.h"
#include "../ectworks/CIntroDialog.h"
#include "../ectworks/CRegistrationDialog.h"
#include "TipWindow.h"
// TODO: encrypt license text
// #include <util-crypt.h>
#include "drawer-window.h"
#include "VCEditor.h"

#include "../vm/scan.h"

using namespace scan;

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

void vcalc_init_app(void);

/**************************************************************
 * CVCalcBranding
 */

UINT CVCalcBranding::GetSmallBannerImage()
{
  return IDB_SPLASH_IMAGE_SMALL;
}

UINT CVCalcBranding::GetLargeBannerImage()
{
  return IDB_SPLASH_IMAGE;
}

CString CVCalcBranding::GetPurchaseURL(CString serialNumber)
{
  CString url;

  url.FormatMessage(IDS_PURCHASE_URL, serialNumber);

  return url;
}

CString CVCalcBranding::GetContactURL(CString serialNumber)
{
  CString url;

  url.FormatMessage(IDS_CONTACT_URL, serialNumber);

  return url;
}

CString CVCalcBranding::GetLicenseText()
{
  CString licenseText;

  /* Read the license text */

  /* TODO: Get the license text

     u8 shortKey[SHORT_CRYPT_KEY_LENGTH];

     parse_crypt_key(shortKey, RESOURCE_KEYSTRING);

     LRef license_encrypted_port = open_c_data_input(true, licenseTxt, licenseTxtSize);
     LRef license_port = open_des_port(license_encrypted_port, PORT_INPUT, *((fixnum_t *)shortKey), false, false);

     LRef data = lread_port_to_string(license_port);

     if (!NULLP(data))
     licenseText = get_c_string(data);
  */

  return licenseText;
}

CString CVCalcBranding::GetApplicationVersion()
{
  CString version;

  version.LoadString(IDS_VERSION);

  return version;
}


/**************************************************************
 * CVCalcApp
 */

BEGIN_MESSAGE_MAP(CVCalcApp, CWinApp)
END_MESSAGE_MAP()

// CVCalcApp construction

CVCalcApp::CVCalcApp() :
m_registration(_T("Icegiant Software"),        // company name			(do not change)
               _T("IG"),                       // company acronym		(do not change)
               _T("info@ectworks.com"),        // info address			(not even used)
               _T("vCalc1"),                   // product name			(change)
               _T("IG-VCALC1"))                // serial number prefix   (change)
{
  m_quitMessageReceived = false;
  m_busy_flag = false;
}

// The one and only CVCalcApp object

CVCalcApp theApp;

// CVCalcApp initialization

void vcalc_init_vcalc_package();




bool CVCalcApp::DelegateMessageToLispProcedure(_TCHAR *desc, LRef closure, LRef *retval, long n, ...)
{
  UNREFERENCED(desc);


  if (m_throw_pending)
    {
      return true;
    }

  LRef closure_tag	= NIL;
  LRef closure_retval	= NIL;

  va_list args;

  va_start(args, n);

  bool failed = scan::call_lisp_procedurev(closure, &closure_retval, &closure_tag, NIL, n, args);

  va_end(args);

  if (failed) {
    assert(!m_throw_pending); // Only allowed one of these at a time.

    m_throw_pending			= true;
    m_pending_throw_retval	= closure_retval;
    m_pending_throw_tag		= closure_tag;

  } else if (retval)
    *retval = closure_retval;

  return failed;
}

BOOL CVCalcApp::InitInstance()
{	
  flonum_t boot_start_time;
  flonum_t scboot_time;
  flonum_t vcalc_core_boot_time;
  flonum_t vcalc_boot_time;
        
  CWinApp::InitInstance();

  // InitCommonControls() is required on Windows XP if an application
  // manifest specifies use of ComCtl32.dll version 6 or later to enable
  // visual styles.  Otherwise, any window creation will fail.
  InitCommonControls();
  AfxInitRichEdit2();

  // We bring the interpreter up first so that we have Scheme facilities
  // availble during initialization.
  sys_init();
  boot_start_time = sys_runtime();
  init(0, NULL, DF_DEBUGGER_TO_ODS);
  scboot_time  = sys_runtime() - boot_start_time;

  m_registration.loadRegistrationSettings();

  if (m_registration.isRegisteredVersion() != m_registration.isRegisteredVersion())
    {
      CString strTitle;
      CString strContents;

      strTitle.LoadString(IDS_CORRUPT_BINARY_TITLE);
      strContents.LoadString(IDS_CORRUPT_BINARY_CONTENTS);

      MessageBox(0, LPCTSTR(strContents), LPCTSTR(strTitle), MB_OK | MB_ICONEXCLAMATION);
      return FALSE;
    }

  // REVISIT: registration logic needs to be duplicated in Scheme

  m_fullFeatures = TRUE;
	
  if (false) // TODO: REENABLE REGISTRATION!!!!!
    // // if (!m_registration.isRegisteredVersion())
    {
      CIntroDialog dlg;

      if (dlg.UserAgreesToLicense())
        m_fullFeatures = !m_registration.TrialLicenseExpired();
      else
        return FALSE;
    }

  m_pszAppName = _tcsdup(_T("vCalc")); // !!!!! String Literal

  vcalc_init_vcalc_package();

  m_pMainWnd = &m_console;

  m_console.Create(IDD_LISP_CONSOLE);

  vcalc_core_boot_time  = sys_runtime() - boot_start_time;

  ENTER_TRY(NULL)
    {
      dscwritef("loading vcinit\n");

      lifasl_load(open_c_data_input(true, vcInit, vcInit_bytes));

      dscwritef("done loading vcinit\n");
    }
  ON_ERROR()
    {
      dscwritef("Error while loading vcinit\n");

      if (DEBUGGING_BUILD)
        WRITE_TEXT_CONSTANT("Error in vCalc Startup! \n", CURRENT_ERROR_PORT);
      else
        {
          RETHROW_DYNAMIC_ESCAPE();
        }
    }
  LEAVE_TRY();

  vcalc_boot_time  = sys_runtime() - boot_start_time;

  scwritef("; scan Boot time = ~cf sec.\n", NIL, scboot_time);
  scwritef("; vCalc Core Boot time = ~cf sec.\n", NIL, vcalc_core_boot_time);
  scwritef("; vCalc Boot time = ~cf sec.\n", NIL, vcalc_boot_time);

  m_throw_pending = false;
  scan::gc_protect(_T("pending-throw-return-value"), &m_pending_throw_retval, 1);
  scan::gc_protect(_T("pending-throw-tag"), &m_pending_throw_tag, 1);

  scan::run();
  scan::shutdown();

  return FALSE;
}


int CVCalcApp::ExitInstance()
{
  m_console.DestroyWindow();

  return CWinApp::ExitInstance();
}

bool CVCalcApp::MessagePump(bool *endLoopFlag, bool exitWhenIdle)
{
  ASSERT_VALID(this);
  _AFX_THREAD_STATE* pState = AfxGetThreadState();

  bool loop_running = true;

  // acquire and dispatch messages until either a WM_QUIT message is received, or
  // we are signaled to end the loop.
  while (   !m_quitMessageReceived
            && loop_running
            && ((endLoopFlag == NULL) || !*endLoopFlag))

    {
      LONG lIdleCount = 0;

      // Call OnIdle while we have idle work to do.
      while (!::PeekMessage(&(pState->m_msgCur), NULL, NULL, NULL, PM_NOREMOVE))
        {
          if (exitWhenIdle)
            {
              loop_running = false;
              break;
            }
          // REVISIT: this design fires idle processing after each event, which is
          // probably too often.
          else if (!OnIdle(lIdleCount++))
            {
              break;
            }
        }

      // pump message, but quit on WM_QUIT
      if (loop_running && !m_throw_pending && !PumpMessage())
        {
          m_quitMessageReceived = true;
          break; 
        }

      // Throw any escapes we've been told to throw on behalf of a message handler
      if (m_throw_pending)
        {
          LRef tag	= m_pending_throw_tag;
          LRef retval	= m_pending_throw_retval;

          m_throw_pending			= false;
          m_pending_throw_tag		= NIL;
          m_pending_throw_retval	= NIL;

          THROW_ESCAPE(tag, retval);
        }

      scan::process_interrupts();
    }

  return !m_quitMessageReceived;  // If we've received a quit message signal to the caller
  // that execution is not going to continue.
}

int CVCalcApp::Run()
{
  int retcode = 0;

  if (!MessagePump(NULL, false))
    retcode = ExitInstance();

  return 0;
}

LRef sym_on_idle_handler = NIL;


BOOL CVCalcApp::OnIdle(LONG lCount)
{
  LRef idle_retval = NIL;

  LRef idle_fn = SYMBOL_VCELL(sym_on_idle_handler);

  if (!UNBOUND_MARKER_P(idle_fn))
    {
      if (!CLOSUREP(idle_fn))
        panic(_T("Invalid idle hook"));

      if (scan::call_lisp_procedure(idle_fn, &idle_retval, NULL, 1, fixcons(lCount)))
        panic(_T("Error evaluating idle hook"));

      return TRUEP(idle_retval);
    }

  return FALSE;
} 

void CVCalcApp::ShowConsole()
{
  m_console.ShowWindow(SW_SHOW);
}

void CVCalcApp::HideConsole()
{
  m_console.ShowWindow(SW_HIDE);
}

bool CVCalcApp::GetRunMode()
{
  return m_fullFeatures;
}

bool CVCalcApp::SetAppBusy(bool busyFlag)
{
  bool oldBusyFlag = m_busy_flag;

  if (m_busy_flag != busyFlag)
    {
      m_busy_flag = busyFlag;

      if (m_busy_flag)
        BeginWaitCursor();
      else
        EndWaitCursor();
    }

  return oldBusyFlag;
}

bool CVCalcApp::GetAppBusy()
{
  return m_busy_flag;
}

void CVCalcApp::SetEventTimer(flonum_t realtime)
{
  m_console.SetEventTimer(realtime);
}

void CVCalcApp::CancelEventTimer()
{
  m_console.CancelEventTimer();
}


/* The scheme bindings ****************************************/

LRef lshow_console()
{
  VCalcGetApp()->ShowConsole();

  return NIL;
}

LRef lhide_console()
{
  VCalcGetApp()->HideConsole();

  return NIL;
}
LRef lget_run_mode()
{
  return boolcons(VCalcGetApp()->GetRunMode());
}


LRef ldo_register()
{
  CRegistrationDialog dlg(NULL);

  dlg.DoModal();

  return NIL;
}

/* Scheme interface routines **********************************/

LRef lset_application_busy(LRef new_busy)
{
  return boolcons(VCalcGetApp()->SetAppBusy(TRUEP(new_busy)));
}

LRef lpump_messages()
{
  VCalcGetApp()->MessagePump(NULL, true);

  return NIL;
}

LRef lset_timer_event_time(LRef realtime)
{
	
  if (NUMBERP(realtime))
    VCalcGetApp()->SetEventTimer(get_c_flonum(realtime));
  else if (FALSEP(realtime))
    VCalcGetApp()->CancelEventTimer();
  else
    vmerror_wrong_type(1, realtime);

  return NIL;
}

LRef labout_box(LRef parent)
{
  if (!WINDOWP(parent))
    vmerror_wrong_type(1, parent);

  CAboutBox dlg(WNDOB(parent));
                
  dlg.DoModal();

  return boolcons(FALSE);
}

LRef limake_window(LRef host_instance, LRef parent_peer)
{
  if (!INSTANCEP(host_instance))
    vmerror_wrong_type(1, host_instance);
  if (!(WINDOWP(parent_peer) || NULLP(parent_peer)))
    vmerror_wrong_type(2, parent_peer);

  LRef typespec = lislot_ref(host_instance, sym_window_type);


  CLispWnd *window = NULL;

  if (typespec == keyword_intern("window"))
    window = new CLispWnd(host_instance, parent_peer);
  else if (typespec == keyword_intern(_T("vcalc-window")))
    window = new CVCWindow(host_instance, parent_peer);
  else if (typespec == keyword_intern(_T("drawer")))
    {
      if (NULLP(parent_peer))
        vmerror("Drawer windows require parents", NIL);

      window = new CDrawer(host_instance, parent_peer);
    }
  else
    vmerror("Unknown window type:", typespec);

  if (window == NULL)
    vmerror("Could not create window: ~s", host_instance);

  return windowcons(window);
}


/****************************************************************
 * vcalc package initialization
 * 
 * This code creates and initializes the vcalc package to contain 
 * all the wholesome goodness that the vcalc .cpp layer provides
 * Code in scheme handles importing all the baseline scheme code.
 */


static void vcalc_register_subrs()
{
  register_subr(_T("show-console")          , SUBR_0, lshow_console); 
  register_subr(_T("hide-console")          , SUBR_0, lhide_console); 
  register_subr(_T("get-run-mode")          , SUBR_0, lget_run_mode); 
  register_subr(_T("do-register")           , SUBR_0, ldo_register); 
  register_subr(_T("about-box")             , SUBR_1, labout_box); 
  register_subr(_T("set-application-busy!") , SUBR_1, lset_application_busy);
  register_subr(_T("pump-messages")         , SUBR_0, lpump_messages);
  register_subr(_T("%set-timer-event-time") , SUBR_1, lset_timer_event_time );

  register_subr(_T("make-image")            , SUBR_1, lmake_image); 
  register_subr(_T("measure-image")         , SUBR_1, lmeasure_image); 

  register_subr(_T("image-open!")           , SUBR_1, limage_open); 
  register_subr(_T("image-close!")          , SUBR_1, limage_close); 

  register_subr(_T("draw-line")             , SUBR_3, ldraw_line); 
  register_subr(_T("draw-polyline")         , SUBR_2, ldraw_polyline); 
  register_subr(_T("draw-rectangle")        , SUBR_3, ldraw_rectangle); 
  register_subr(_T("fill-rectangle")        , SUBR_3, lfill_rectangle); 
  register_subr(_T("draw-gradient")         , SUBR_4, ldraw_gradient); 
  register_subr(_T("draw-ellipse")          , SUBR_3, ldraw_ellipse); 
  register_subr(_T("fill-ellipse")          , SUBR_3, lfill_ellipse); 
  register_subr(_T("draw-point")            , SUBR_2, ldraw_point);
  register_subr(_T("draw-image")            , SUBR_4, ldraw_image);
  register_subr(_T("copy-image")            , SUBR_1, lcopy_image);
  register_subr(_T("subimage")              , SUBR_3, lsubimage);
  register_subr(_T("draw-text")             , SUBR_3, ldraw_text); 
  register_subr(_T("measure-text")          , SUBR_2, lmeasure_text); 

  register_subr(_T("set-drawing-origin!")   , SUBR_2, lset_drawing_origin); 
  register_subr(_T("update-drawing-origin!"), SUBR_2, lupdate_drawing_origin); 
  register_subr(_T("get-drawing-origin")    , SUBR_1, lget_drawing_origin); 

  register_subr(_T("set-foreground-color!") , SUBR_2, lset_foreground_color); 
  register_subr(_T("set-background-color!") , SUBR_2, lset_background_color); 
  register_subr(_T("set-font!")             , SUBR_2, lset_font); 
  register_subr(_T("point-scale")           , SUBR_3, lpoint_scale);
 
  register_subr(_T("draw-linear-text")      , SUBR_5, ldraw_linear_text); 
  register_subr(_T("measure-linear-text")   , SUBR_3, lmeasure_linear_text); 

  register_subr(_T("font")                  , SUBR_1, lfont); 

  register_subr(_T("rgb-color")             , SUBR_3, lrgb_color); 
  register_subr(_T("color-r")               , SUBR_1, lcolor_r); 
  register_subr(_T("color-g")               , SUBR_1, lcolor_g); 
  register_subr(_T("color-b")               , SUBR_1, lcolor_b); 

  register_subr(_T("beep")                  , SUBR_0, lbeep); 
  register_subr(_T("exit-application")      , SUBR_0, lexit_application); 
  register_subr(_T("system-string")         , SUBR_1, lsystem_string); 
  register_subr(_T("%choose-file")          , SUBR_5, lchoose_file); 
  register_subr(_T("yes-or-no?")            , SUBR_2, lyes_or_nop); 
  register_subr(_T("message")               , SUBR_2, lmessage); 
  register_subr(_T("get-clipboard-formats") , SUBR_0, lget_clipboard_formats);
  register_subr(_T("set-clipboard-data")    , SUBR_N, lset_clipboard_data); 
  register_subr(_T("get-clipboard-data")    , SUBR_N, lget_clipboard_data); 
  register_subr(_T("get-user-file-path")    , SUBR_1, lget_user_file_path); 
  register_subr(_T("%make-window")          , SUBR_2, limake_window); 
  register_subr(_T("show-window")           , SUBR_1, lshow_window); 
  register_subr(_T("hide-window")           , SUBR_1, lhide_window); 
  register_subr(_T("window-size")           , SUBR_1, lget_window_size); 
  register_subr(_T("close-window")          , SUBR_1, lclose_window); 
  register_subr(_T("update-window")         , SUBR_1, lupdate_window); 
  register_subr(_T("flush-window")          , SUBR_1, lflush_window); 
  register_subr(_T("open-editor")           , SUBR_2, lopen_editor); 
  register_subr(_T("close-editor")          , SUBR_1, lclose_editor); 
  register_subr(_T("window-placement")      , SUBR_1, lget_window_placement); 
  register_subr(_T("set-window-placement!") , SUBR_2, lset_window_placement); 
  register_subr(_T("set-status-text!")      , SUBR_3, lset_status_text); 
  register_subr(_T("parent-repositioned")   , SUBR_1, lparent_repositioned); 
  register_subr(_T("key-name->key-id")      , SUBR_1, lkey_name2key_id); 
  register_subr(_T("key-id->key-name")      , SUBR_1, lkey_id2key_name); 
  register_subr(_T("key-id->natural-number"), SUBR_1, lkey_id2natural_number);
  register_subr(_T("key-id->character")     , SUBR_1, lkey_id2character);
  register_subr(_T("show-config-dialog")    , SUBR_3, lshow_config_dialog); 
  register_subr(_T("show-tip")              , SUBR_2, lshow_tip); 
  register_subr(_T("choose")                , SUBR_4, lchoose); 

  register_subr(_T("edit-text")             , SUBR_3, ledit_text); 
  register_subr(_T("show-config-dialog")    , SUBR_3, lshow_config_dialog);
}


#include "keynames.h"

LRef sym_key[NUM_KEYS];
LRef vcalc_package = NIL;

void vcalc_init_vcalc_package()
{
  gc_protect(_T("vcalc-package"), &vcalc_package, 1);
  vcalc_package = lmake_package(strcons( "vcalc"));

  gc_protect_sym(&sym_on_idle_handler, _T("*on-idle-handler*"), vcalc_package);

  vcalc_register_subrs();

  gc_protect(_T("keysym-table"), &sym_key[0], NUM_KEYS);
  for(int ii = 0; ii < NUM_KEYS; ii++)
      sym_key[ii] = keyword_intern(keyinfoTable[ii].name);

  gc_protect_sym(&sym_console_input_port, _T("*console-input-port*"), vcalc_package);
  gc_protect_sym(&sym_console_output_port, _T("*console-output-port*"), vcalc_package);
  gc_protect_sym(&sym_console_error_port, _T("*console-error-port*"), vcalc_package);
  
  gc_protect_sym(&sym_on_move, "on-move", vcalc_package);
  gc_protect_sym(&sym_on_size, "on-size", vcalc_package);
  gc_protect_sym(&sym_on_edit_keypress, "on-edit-keypress", vcalc_package);
  gc_protect_sym(&sym_on_keypress, "on-keypress", vcalc_package);
  gc_protect_sym(&sym_on_command, "on-command", vcalc_package);
  gc_protect_sym(&sym_on_update, "on-update", vcalc_package);
  gc_protect_sym(&sym_on_destroy, "on-destroy", vcalc_package);
  gc_protect_sym(&sym_surface, "surface", vcalc_package);
  gc_protect_sym(&sym_bitmap, "bitmap", vcalc_package);
  gc_protect_sym(&sym_initial_title, "initial-title", vcalc_package);
  gc_protect_sym(&sym_initial_placement, "initial-placement", vcalc_package);
  gc_protect_sym(&sym_children, "children", vcalc_package);
  gc_protect_sym(&sym_parent, "parent", vcalc_package);
  gc_protect_sym(&sym_window_type, "window-type", vcalc_package);

  gc_protect(_T("bold"), &sym_bold, 1);
  sym_bold = keyword_intern(_T("bold"));

  gc_protect(_T("italic"), &sym_italic, 1);
  sym_italic = keyword_intern(_T("italic"));

  gc_protect(_T("strikethrough"), &sym_strikethrough, 1);
  sym_strikethrough = keyword_intern(_T("strikethrough"));

  gc_protect(_T("underline"), &sym_underline, 1);
  sym_underline = keyword_intern(_T("underline"));

  gc_protect_sym(&sym_current_image, _T("*current-image*"), vcalc_package);

  lidefine_global(sym_current_image, NIL, NIL);
}
