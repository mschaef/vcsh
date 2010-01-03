// LispConsole.cpp : implementation file
//

#include "stdafx.h"
#include <RichOle.h>

#include "vcalc.h"
#include "console-window.h"

#include "../vm/scan.h"

using namespace scan;

// !! Multiple console windows
// !! How will non-local escapes be propagated across the message pump?
// !! How will modal dialogs keep our message pump procesing working? 

const _TCHAR *DEFAULT_CONSOLE_FONT = _T("Lucida Console");
const int STROUT_PORT_BLOCK_SIZE   = 256;
const int CONSOLE_PORT_BLOCK_SIZE  = 64;

/*****
 * ImageDataObject
 *
 * Impementation for IDataObject Interface to be used in inserting bitmap to the
 * RichEdit Control.
 *
 * Author : Hani Atassi  (atassi@arabteam2000.com)
 */



void CImageDataObject::InsertBitmap(IRichEditOle* pRichEditOle, HBITMAP hBitmap)
{
  SCODE sc;

  // Get the image data object
  CImageDataObject *pods = new CImageDataObject;
  LPDATAOBJECT lpDataObject;
  pods->QueryInterface(IID_IDataObject, (void **)&lpDataObject);

  pods->SetBitmap(hBitmap);

  // Get the RichEdit container site
  IOleClientSite *pOleClientSite; 
  pRichEditOle->GetClientSite(&pOleClientSite);

  // Initialize a Storage Object
  IStorage *pStorage;     

  LPLOCKBYTES lpLockBytes = NULL;
  sc = ::CreateILockBytesOnHGlobal(NULL, TRUE, &lpLockBytes);
  if (sc != S_OK)
    AfxThrowOleException(sc);
  ASSERT(lpLockBytes != NULL);
        
  sc = ::StgCreateDocfileOnILockBytes(lpLockBytes,
                                      STGM_SHARE_EXCLUSIVE|STGM_CREATE|STGM_READWRITE, 0, &pStorage);
  if (sc != S_OK)
    {
      VERIFY(lpLockBytes->Release() == 0);
      lpLockBytes = NULL;
      AfxThrowOleException(sc);
    }
  ASSERT(pStorage != NULL);

  // The final ole object which will be inserted in the richedit control
  IOleObject *pOleObject; 
  pOleObject = pods->GetOleObject(pOleClientSite, pStorage);

  // all items are "contained" -- this makes our reference to this object
  //  weak -- which is needed for links to embedding silent update.
  OleSetContainedObject(pOleObject, TRUE);

  // Now Add the object to the RichEdit 
  REOBJECT reobject;
  ZeroMemory(&reobject, sizeof(REOBJECT));
  reobject.cbStruct = sizeof(REOBJECT);
        
  CLSID clsid;
  sc = pOleObject->GetUserClassID(&clsid);
  if (sc != S_OK)
    AfxThrowOleException(sc);

  reobject.clsid = clsid;
  reobject.cp = REO_CP_SELECTION;
  reobject.dvaspect = DVASPECT_CONTENT;
  reobject.poleobj = pOleObject;
  reobject.polesite = pOleClientSite;
  reobject.pstg = pStorage;

  // Insert the bitmap at the current location in the richedit control
  pRichEditOle->InsertObject(&reobject);

  // Release all unnecessary interfaces
  pOleObject->Release();
  pOleClientSite->Release();
  pStorage->Release();
  lpDataObject->Release();
}

void CImageDataObject::SetBitmap(HBITMAP hBitmap)
{
  ASSERT(hBitmap);

  STGMEDIUM stgm;
  stgm.tymed = TYMED_GDI;           // Storage medium = HBITMAP handle              
  stgm.hBitmap = hBitmap;
  stgm.pUnkForRelease = NULL;       // Use ReleaseStgMedium

  FORMATETC fm;
  fm.cfFormat = CF_BITMAP;          // Clipboard format = CF_BITMAP
  fm.ptd = NULL;                    // Target Device = Screen
  fm.dwAspect = DVASPECT_CONTENT;   // Level of detail = Full content
  fm.lindex = -1;                   // Index = Not applicaple
  fm.tymed = TYMED_GDI;             // Storage medium = HBITMAP handle

  this->SetData(&fm, &stgm, TRUE);                
}

IOleObject *CImageDataObject::GetOleObject(IOleClientSite *pOleClientSite, IStorage *pStorage)
{
  ASSERT(m_stgmed.hBitmap);

  SCODE sc;
  IOleObject *pOleObject;
  sc = ::OleCreateStaticFromData(this, IID_IOleObject, OLERENDER_FORMAT, 
                                 &m_format, pOleClientSite, pStorage, (void **)&pOleObject);
  if (sc != S_OK)
    AfxThrowOleException(sc);
  return pOleObject;
}

/* Console output port ****************************************
 *
 * This port supports output to the console window
 *
 * state_info = Current CLispConsole pointer to window
 **/

#define PORT_CONSOLE(obj) ((CLispConsole *)PORT_PINFO(obj)->_user_data)

size_t con_port_write(const void *buf, size_t size, size_t count, LRef obj, CLispConsole::OutputClass o)
{
  // !!!! con_port_write is shared with debug_port_write: refactoring opportunity.

  assert(PORTP(obj));

  _TCHAR block[CONSOLE_PORT_BLOCK_SIZE + 1];

  _TCHAR *cbuf = (_TCHAR *)buf;

  size_t buf_loc = 0;
  size_t block_loc = 0;

  size_t len = size * count;

  /* Filter nulls out of the input string, and ensure that the
   * buffer we pass to OutputDebugString has as terminating
   * null. */
  while(len > 0)
    {
      for(block_loc = 0;
          (block_loc < CONSOLE_PORT_BLOCK_SIZE) && (len > 0);
          block_loc++, buf_loc++, len--)
        {
          if (cbuf[buf_loc] == '\0')
            block[block_loc] = '~';
          else
            block[block_loc] = cbuf[buf_loc];
        }

      block[block_loc] = '\0';

      PORT_CONSOLE(obj)->InsertLispOutput(CString(block), o);
    }

  return len;
}


size_t conout_port_write(const void *buf, size_t size, size_t count, LRef obj)
{
  return con_port_write(buf, size, count, obj, CLispConsole::OUTPUT_PORT_TEXT);
}

bool con_port_rich_write(LRef obj, bool machine_readable, LRef port)
{
  UNREFERENCED(machine_readable);

  if (!IMAGEP(obj))
    return false;

  // TODO:  with machine_readable set, drawing surfaces should be rich written with dimensions
  // TODO:  rich write should have a global variable that puts an upper bound on bitmap dimensions
  // TODO:  rich write would be a good place to allow font and color changes in the console
  
  PORT_CONSOLE(port)->InsertDrawingSurface(IMAGE_SURFACE(obj));

  return true;
}

size_t conerr_port_write(const void *buf, size_t size, size_t count, LRef obj)
{
  return con_port_write(buf, size, count, obj, CLispConsole::ERROR_PORT_TEXT);
}

port_class_t console_output_port = {
  _T("CONSOLE-OUTPUT"),
  PORT_OUTPUT,

  NULL,

  NULL,
  NULL,
  conout_port_write,
  con_port_rich_write,

  NULL,
  NULL,
  NULL,

  NULL,
};

port_class_t console_error_port = {
  _T("CONSOLE-ERROR"),
  PORT_OUTPUT,

  NULL,

  NULL,
  NULL,
  conerr_port_write,
  con_port_rich_write,

  NULL,
  NULL,
  NULL,

  NULL,
};

LRef sym_console_input_port;
LRef sym_console_output_port;
LRef sym_console_error_port;

LRef conoutportcons(CLispConsole *c)
{
  return portcons(&console_output_port, NIL, PORT_OUTPUT, NIL, (void *)c);
}

LRef conerrportcons(CLispConsole *c)
{
  return portcons(&console_error_port, NIL, PORT_OUTPUT,  NIL, (void *)c);
}


bool console_input_read(LRef port, void *userdata)
{

  OutputDebugString("console_input_read");

  UNREFERENCED(port);

  CLispConsole *c = (CLispConsole *)userdata;


  bool data_ready = false;

  bool *oldDataReadyFlag = c->m_dataReadyFlag;
  c->m_dataReadyFlag = &data_ready;

  bool more_data = VCalcGetApp()->MessagePump(&data_ready, false);

  c->m_dataReadyFlag = oldDataReadyFlag; // Not exception safe, but MessagePump shouldn't throw errors

  return more_data;
}

LRef coninportcons(CLispConsole *c)
{
  return blocking_input_cons(_T("console-input"), false, console_input_read, NULL, c);
}

/* Console window ********************************************
 *
 * This is the console window itself
 **/

// CLispConsole dialog

IMPLEMENT_DYNAMIC(CLispConsole, CDialog)
CLispConsole::CLispConsole(CWnd* pParent /*=NULL*/)
: CDialog(CLispConsole::IDD, pParent)
{
  m_dialogInitialized = FALSE;
  m_event_time = -1;
  m_dataReadyFlag = NULL;
}

CLispConsole::~CLispConsole()
{
}

void CLispConsole::DoDataExchange(CDataExchange* pDX)
{
  CDialog::DoDataExchange(pDX);
  DDX_Control(pDX, IDC_OUTPUT_PANE, m_OutputPane);
}


BEGIN_MESSAGE_MAP(CLispConsole, CDialog)
ON_NOTIFY(EN_MSGFILTER, IDC_OUTPUT_PANE, OnOutputPaneMessage)
ON_WM_SIZE()
ON_WM_CLOSE()
ON_WM_SETCURSOR()
ON_WM_TIMER()
END_MESSAGE_MAP()


// CLispConsole message handlers


BOOL CLispConsole::OnInitDialog()
{
  CDialog::OnInitDialog();

  m_dialogInitialized = TRUE;

  m_OutputPane.SetEventMask(m_OutputPane.GetEventMask() | ENM_KEYEVENTS);

  m_OutputBufferEndPos = 0;

  CHARFORMAT cf;

  cf.cbSize = sizeof(CHARFORMAT);
  cf.dwMask = CFM_BOLD | CFM_COLOR | CFM_FACE | CFM_SIZE;
  cf.dwEffects = 0;
  cf.crTextColor = RGB(0, 0, 0);
  cf.yHeight = 240;
  strcpy(cf.szFaceName,DEFAULT_CONSOLE_FONT);

  m_OutputPane.SetDefaultCharFormat(cf);

  m_OutputPane.SetTargetDevice(NULL, 0);

  lidefine_global(sym_console_input_port, coninportcons(this), NIL);
  lidefine_global(sym_console_output_port, conoutportcons(this), NIL);
  lidefine_global(sym_console_error_port, conerrportcons(this), NIL);

  return TRUE;  // return TRUE unless you set the focus to a control
  // EXCEPTION: OCX Property Pages should return FALSE
}


void CLispConsole::OnOutputPaneMessage(NMHDR *pNMHDR, LRESULT *pResult)
{
  MSGFILTER *pMsgFilter = reinterpret_cast<MSGFILTER *>(pNMHDR);

  if (pMsgFilter->msg == WM_KEYDOWN)
    {
      if (pMsgFilter->wParam == VK_RETURN) 
        {
          bool control = (GetKeyState(VK_LCONTROL) & 0x80) || (GetKeyState(VK_RCONTROL) & 0x80);

          // Not currently used.
          //
          // bool shift   = (GetKeyState(VK_LSHIFT  ) & 0x80) || (GetKeyState(VK_RSHIFT  ) & 0x80); 

          if (control)
            {
              CString form = GetInputText();

              blocking_input_post_data(SYMBOL_VCELL(sym_console_input_port), 
                                       form.GetBuffer(),
                                       form.GetLength());

              form.ReleaseBuffer();

              assert(m_dataReadyFlag != NULL);

              *m_dataReadyFlag = blocking_input_is_data_available(SYMBOL_VCELL(sym_console_input_port));
            }
        }
    }

  // TODO:  Add your control notification handler code here

  *pResult = 0;
}


void CLispConsole::OnSize(UINT nType, int cx, int cy)
{
  CDialog::OnSize(nType, cx, cy);

  if (m_dialogInitialized)
    m_OutputPane.SetWindowPos(NULL, 0, 0, cx, cy, SWP_NOMOVE);

}

void CLispConsole::OnClose()
{
  CDialog::OnClose();

  AfxPostQuitMessage(0);
}

BOOL CLispConsole::OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message)
{
  if (VCalcGetApp()->GetAppBusy())
    {
      RestoreWaitCursor();
      return TRUE;
    }

  return CDialog::OnSetCursor(pWnd, nHitTest, message);
}

CString CLispConsole::GetInputText()
{
  long beginSel;
  long endSel;
  long finalChar;
  CString form = "";

  m_OutputPane.GetSel(beginSel, endSel);


  if (beginSel > endSel)
    finalChar = beginSel;
  else
    finalChar = endSel;

  if (finalChar > m_OutputBufferEndPos)
    {
      m_OutputPane.SetSel(m_OutputBufferEndPos, finalChar);

      form = m_OutputPane.GetSelText();
    }

  m_OutputBufferEndPos = finalChar;

  m_OutputPane.SetSel(m_OutputBufferEndPos, m_OutputBufferEndPos);

  return form + _T("\n");
}



void CLispConsole::InsertDrawingSurface(vcalc_image_t *surf)
{
  HBITMAP hBitmap = vcalc_image_duplicate_hbitmap(surf);

  IRichEditOle *pIREO = m_OutputPane.GetIRichEditOle();

  CImageDataObject::InsertBitmap(pIREO, hBitmap);
}

void CLispConsole::InsertLispOutput(CString txt, OutputClass txtClass)
{
  CHARFORMAT cf;

  cf.cbSize = sizeof(CHARFORMAT);

  switch (txtClass)
    {
    case USER_TEXT:
      cf.dwMask = CFM_BOLD | CFM_COLOR | CFM_FACE;
      cf.dwEffects = 0;
      cf.crTextColor = RGB(0, 0, 0);
      strcpy(cf.szFaceName,DEFAULT_CONSOLE_FONT);
      break;

    case LISP_TEXT:
      cf.dwMask = CFM_BOLD | CFM_COLOR | CFM_FACE;
      cf.dwEffects = CFE_BOLD;
      cf.crTextColor = RGB(0, 0, 0);
      strcpy(cf.szFaceName,DEFAULT_CONSOLE_FONT);
      break;

    case OUTPUT_PORT_TEXT:
      cf.dwMask = CFM_ITALIC | CFM_COLOR | CFM_FACE;
      cf.dwEffects = CFE_BOLD;
      cf.crTextColor = RGB(0, 0, 0);
      strcpy(cf.szFaceName,DEFAULT_CONSOLE_FONT);
      break;

    case ERROR_TEXT:
      cf.dwMask = CFM_BOLD | CFM_COLOR | CFM_FACE;
      cf.dwEffects = CFE_BOLD;
      cf.crTextColor = RGB(0xFF, 0, 0);
      strcpy(cf.szFaceName,DEFAULT_CONSOLE_FONT);
      break;

    case ERROR_PORT_TEXT:
      cf.dwMask = CFM_ITALIC | CFM_COLOR | CFM_FACE;
      cf.dwEffects = CFE_BOLD;
      cf.crTextColor = RGB(0xFF, 0, 0);
      strcpy(cf.szFaceName,DEFAULT_CONSOLE_FONT);
      break;
    }

  long nStartPos, nEndPos,nTemp;

  // Set selection to end of text
  m_OutputPane.SetSel(LONG_MAX,LONG_MAX);      
  m_OutputPane.GetSel(nStartPos,nTemp);
  m_OutputPane.ReplaceSel(txt);
  m_OutputPane.GetSel(nTemp,nEndPos);
  m_OutputPane.SetSel(nStartPos,nEndPos);

  m_OutputPane.SetSelectionCharFormat(cf);

  m_OutputPane.SetSel(LONG_MAX,LONG_MAX);      

  m_OutputPane.GetSel(nTemp,nEndPos);

  m_OutputBufferEndPos = nEndPos;
}

void CLispConsole::EventTimerElapsed()
{
  m_event_time = -1;

  scan::signal_timer();
}

void CLispConsole::SetEventTimer(flonum_t realtime)
{
  flonum_t now = sys_realtime();

  CancelEventTimer();

  if (now > realtime)
    {
      EventTimerElapsed();
      return;
    }

  flonum_t msec_to_go = (realtime - now) * 1000.0;

  assert(msec_to_go < U32_MAX);

  SetTimer(EVENT_TIMER_ID, (UINT)msec_to_go, NULL);

  m_event_time = realtime;
}

void CLispConsole::CancelEventTimer()
{
  if (m_event_time < 0) 
    return;

  KillTimer(EVENT_TIMER_ID);
  m_event_time = -1;
}

void CLispConsole::OnTimer(UINT nIDEvent)
{
  if (nIDEvent == EVENT_TIMER_ID)
    {
      assert(m_event_time > 0);

      CancelEventTimer();

      EventTimerElapsed();
    }
  else
    CDialog::OnTimer(nIDEvent);
}
