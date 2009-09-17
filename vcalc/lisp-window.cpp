/****************************************************************
 * LispWindow.cpp
 * April 15th, 2004
 *
 * This is the basic functionality on which Lisp windows are built.
 * All windows to be accessed from Lisp code are to be derived from
 * CLispWnd.
 */


#include "stdafx.h"
#include "resource.h"
#include "image.h"
#include "lisp-window.h"
#include "drawer-window.h"
#include "vcalc.h"

/*
 * A few global tables...
 */
#include "cmd_keys.i"

scan::LRef sym_on_move = NIL;
scan::LRef sym_on_size = NIL;
scan::LRef sym_on_edit_keypress = NIL;
scan::LRef sym_on_keypress = NIL;
scan::LRef sym_on_command = NIL;
scan::LRef sym_on_update= NIL;
scan::LRef sym_on_destroy = NIL;
scan::LRef sym_surface = NIL;
scan::LRef sym_bitmap = NIL;
scan::LRef sym_initial_title = NIL;
scan::LRef sym_initial_placement = NIL;
scan::LRef sym_children = NIL;
scan::LRef sym_parent = NIL;
scan::LRef sym_window_type = NIL;

/****************************************************************
 * The CLispWnd Implementation
 */


IMPLEMENT_DYNAMIC(CLispWnd, CWnd)
CLispWnd::CLispWnd(LRef host_instance, LRef parent_peer)
{
  _lisp_instance = host_instance;  
  _parent_peer   = parent_peer;

  m_BackgroundImage.LoadBitmap(IDB_BACKGROUND);
}

CLispWnd::~CLispWnd()
{
  m_BackgroundImage.DeleteObject();
}

/*
 * Message Handlers
 */

BEGIN_MESSAGE_MAP(CLispWnd, CWnd)
    ON_WM_CREATE()
    ON_WM_CLOSE()
    ON_WM_PAINT()
    ON_WM_SIZE()
    ON_WM_MOVE()
    ON_WM_KEYDOWN()
    ON_WM_MOUSEMOVE()
    ON_WM_DROPFILES()
    ON_WM_SETCURSOR()
    ON_WM_DESTROY()
END_MESSAGE_MAP()

int CLispWnd::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
    if (CWnd::OnCreate(lpCreateStruct) == -1)
        return -1;

    m_Icon          = VCalcGetApp()->LoadIcon(IDI_MAIN);
    m_ArrowCursor   = VCalcGetApp()->LoadStandardCursor(IDC_ARROW);

    SetIcon(m_Icon, TRUE);

    return 0;
}

void CLispWnd::OnClose( )
{
  if (TRUEP(DoProcessCommand(simple_intern(_T("cmd-window-close"), vcalc_package), NIL)))
        CWnd::OnClose();
}

void CLispWnd::OnDestroy()
{
  SendInstanceMessage(sym_on_destroy, NIL);
}

void CLispWnd::DoPaint(CDC &dc, int xOrig, int yOrig)
{
    CRect clientRect;
    
    GetClientRect(clientRect);

    LRef surf = GetInstanceSlotValue(sym_surface, NIL);

    if (IMAGEP(surf)) {
      HDC surfaceDC = vcalc_image_add_dc_ref(IMAGE_SURFACE(surf));

      ::BitBlt(dc, 
               0, 0, clientRect.Width(), clientRect.Height(), 
               surfaceDC, xOrig, yOrig, SRCCOPY);

        vcalc_image_release_dc_ref(IMAGE_SURFACE(surf));
    } else {
      DrawEmptyBackground(dc);
    }

    GdiFlush();
}

void CLispWnd::OnPaint()
{
    Invalidate(FALSE);

    CPaintDC dc(this);

    DoPaint(dc, 0, 0);
}



void CLispWnd::OnSize(UINT nType, int cx, int cy)
{
    CWnd::OnSize(nType, cx, cy);

    DoSizeWindow(cx, cy);
}

void CLispWnd::OnMove(int x, int y)
{
    CWnd::OnMove(x, y);

    SendInstanceMessage(sym_on_move, listn(2, fixcons(x), fixcons(y)));
}

void CLispWnd::OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags)
{
    CWnd::OnKeyDown(nChar, nRepCnt, nFlags);

    i32 key_id = current_keypress_key_id(nChar);

    if (key_id != -1)
      SendInstanceMessage(sym_on_keypress, listn(1, fixcons(key_id)));
}

void CLispWnd::OnMouseMove(UINT nFlags, CPoint point)
{
    CWnd::OnMouseMove(nFlags, point);
}

void CLispWnd::OnDropFiles(HDROP hDropInfo)
{
    TCHAR pathBuffer[MAX_PATH + 1];

    int numFiles = DragQueryFile(hDropInfo, (UINT)-1, NULL, 0);

    for(int i = 0; i < numFiles; i++)
    {
        if (DragQueryFile(hDropInfo, i, pathBuffer, MAX_PATH + 1))
          DoProcessCommand(simple_intern(_T("cmd-file-dropped"), vcalc_package), strcons( pathBuffer));
    }
}

BOOL CLispWnd::OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message)
{
    if (VCalcGetApp()->GetAppBusy())
    {
        RestoreWaitCursor();
        return TRUE;
    }
    else if (nHitTest == HTCLIENT)
    {
        ::SetCursor(m_ArrowCursor);
        return TRUE;
    }

    return CWnd::OnSetCursor(pWnd, nHitTest, message);
}


BOOL CLispWnd::OnCommand(WPARAM wParam, LPARAM lParam)
{
    for(int i = 0; commandIDTable[i]._resID != 0; i++)
    {
        if (commandIDTable[i]._resID == wParam)
        {
          DoProcessCommand(simple_intern(commandIDTable[i]._keyword, vcalc_package), NIL);

            break;
        }
    }

    // !! ? Need to worry about unfound commands?

    return CWnd::OnCommand(wParam, lParam);
}


/*
 * Parent message notifications
 *
 * Not needed for all windows, but needed to implement parallel window
 * motion, etc.
 */

void CLispWnd::LParentRepositioned()
{
}

void CLispWnd::DrawEmptyBackground(HDC dc)
{
  BITMAP bmp;
  m_BackgroundImage.GetBitmap(&bmp);

  CRect clientRect;
    
  GetClientRect(clientRect);

  HDC srcDC = ::CreateCompatibleDC(dc);

  assert(srcDC != NULL);

  int last_state = ::SaveDC(srcDC);

  assert(last_state != 0);

  HGDIOBJ last_bitmap = ::SelectObject(srcDC, (HBITMAP)m_BackgroundImage);

  assert(last_bitmap != NULL);

  for(int x = 0; x < clientRect.Width(); x += 20) {
    for(int y = 0; y < clientRect.Height(); y += 20) {
      ::BitBlt(dc, x, y, bmp.bmWidth, bmp.bmHeight, srcDC, 0, 0, SRCCOPY);
    }
  }

  ::RestoreDC(srcDC, last_state);
  ::DeleteDC(srcDC);
}

/*
 * Overridable virtual functions
 */

void CLispWnd::DoSizeWindow(int cx, int cy)
{
  SendInstanceMessage(sym_on_size, listn(2, fixcons(cx), fixcons(cy)));
}

void CLispWnd::DoCreateWindow()
{    
    CString windowClass = AfxRegisterWndClass(CS_VREDRAW | CS_HREDRAW,
                                              NULL,
                                              (HBRUSH) ::GetStockObject(NULL_BRUSH),
                                              NULL);

    LRef initialTitle = GetInstanceSlotValue(sym_initial_title, NIL);

    CString title = STRINGP(initialTitle) ? get_c_string(initialTitle) : "LispWindow: No title specified";

    CWnd *parent = NULLP(_parent_peer) ? NULL : WNDOB(_parent_peer);

    // !!! Figure out how to set window placement from initial-placement
    CreateEx(WS_EX_APPWINDOW,
             windowClass,
             title,
               WS_OVERLAPPED 
             | WS_SIZEBOX
             | WS_MINIMIZEBOX
             | WS_SYSMENU,
             CRect(0, 0, 200, 200),
             parent,
              0);

    SetWindowPos(NULL, 0, 0, 200, 200, SWP_NOMOVE | SWP_HIDEWINDOW);
}



LRef CLispWnd::DoProcessCommand(LRef command, LRef arg)
{
    return SendInstanceMessage(command, listn(1, arg));
}

/*
 * Public Interface FUnctions
 */

void CLispWnd::LClose()
{
  DestroyWindow();
}

void CLispWnd::LUpdate()
{
  SendInstanceMessage(sym_on_update, NIL);
}

void CLispWnd::LFlushDrawSurface()
{
    Invalidate(FALSE);
}

bool CLispWnd::LShowWindow()
{
    return (ShowWindow(SW_SHOW) != 0);
}

bool CLispWnd::LHideWindow()
{
    return (ShowWindow(SW_HIDE) != 0);
}


LRef CLispWnd::GetPlacement()
{
    WINDOWPLACEMENT wp;
    wp.length = sizeof (WINDOWPLACEMENT);
    GetWindowPlacement(&wp);

    return llist2vector(listn(6,
                              fixcons(wp.flags),
                              fixcons(wp.showCmd),
                              fixcons(wp.rcNormalPosition.left),
                              fixcons(wp.rcNormalPosition.top),
                              fixcons(wp.rcNormalPosition.right),
                              fixcons(wp.rcNormalPosition.bottom)));

}

LRef CLispWnd::SetPlacement(LRef placement)
{
    LRef old_placement = GetPlacement();

    WINDOWPLACEMENT wp;
    wp.length = sizeof (WINDOWPLACEMENT);
    GetWindowPlacement(&wp);

    if (!VECTORP(placement)) 
      vmerror_wrong_type(1, placement);

    if (   (VECTOR_LENGTH(placement) != 6)
        || !FIXNUMP(VECTOR_OBJECT(placement, 0))
        || !FIXNUMP(VECTOR_OBJECT(placement, 1))
        || !FIXNUMP(VECTOR_OBJECT(placement, 2))
        || !FIXNUMP(VECTOR_OBJECT(placement, 3))
        || !FIXNUMP(VECTOR_OBJECT(placement, 4))
        || !FIXNUMP(VECTOR_OBJECT(placement, 5)))
        vmerror("invalid placement vector in set-window-placement!", placement);

    wp.flags                    = (UINT)get_c_fixnum(VECTOR_OBJECT(placement, 0));
    wp.showCmd                  = (UINT)get_c_fixnum(VECTOR_OBJECT(placement, 1));
    wp.rcNormalPosition.left    = (LONG)get_c_fixnum(VECTOR_OBJECT(placement, 2));
    wp.rcNormalPosition.top     = (LONG)get_c_fixnum(VECTOR_OBJECT(placement, 3));
    wp.rcNormalPosition.right   = (LONG)get_c_fixnum(VECTOR_OBJECT(placement, 4));
    wp.rcNormalPosition.bottom  = (LONG)get_c_fixnum(VECTOR_OBJECT(placement, 5));

    wp.rcNormalPosition.left = min (wp.rcNormalPosition.left,
                                    ::GetSystemMetrics (SM_CXSCREEN) -
                                    ::GetSystemMetrics (SM_CXICON));
    wp.rcNormalPosition.top = min (wp.rcNormalPosition.top,
                                    ::GetSystemMetrics (SM_CYSCREEN) -
                                    ::GetSystemMetrics (SM_CYICON));

    SetWindowPlacement (&wp);

    return old_placement;
}



LRef CLispWnd::LGetWindowSize()
{
    return lcons(fixcons(_windowWidth), fixcons(_windowHeight));
}

void CLispWnd::OpenEditor(_TCHAR *initial)
{
  UNREFERENCED(initial);

    vmerror("This window doesn't support open-editor", _lisp_instance);
}

LRef CLispWnd::CloseEditor()
{
    vmerror("This window doesn't support close-editor", _lisp_instance);

    return NULL; 
}

void CLispWnd::SetStatusText(long pane, _TCHAR *string)
{
  UNREFERENCED(pane);
  UNREFERENCED(string);

    vmerror("This window doesn't support set-status-text", _lisp_instance);
}

void CLispWnd::GCMark()
{
  gc_mark(_lisp_instance);
  gc_mark(_parent_peer);
}

scan::LRef CLispWnd::GetInstanceSlotValue(scan::LRef slotName, scan::LRef defaultValue /* = NIL */)
{
  if (NULLP(_lisp_instance))
    return defaultValue;

  if (!TRUEP(lhas_slotp(_lisp_instance, slotName)))
    return defaultValue;

  return lislot_ref(_lisp_instance, slotName);
}

scan::LRef CLispWnd::SendInstanceMessage(scan::LRef message, scan::LRef args /* = NIL */)
{
  if (NULLP(_lisp_instance))
    return boolcons(false);

  scan::LRef rc = NIL;

  ENTER_TRY(NULL)
    {
      rc = lsend_message(_lisp_instance, message, args);
    }
  ON_ERROR()
    {
      WRITE_TEXT_CONSTANT("Error sending instance message.", CURRENT_ERROR_PORT);
    }
  LEAVE_GUARD();

  return rc;
}

/****************************************************************
 * The Scheme Object definition
 */

void window_gc_mark_proc(LRef obj)
{
  assert(WINDOWP(obj));

  WNDOB(obj)->GCMark();
}


void window_gc_free_proc(LRef obj)
{
  assert(WINDOWP(obj));

  WNDOB(obj)->DestroyWindow();
      
  delete WNDOB(obj);
}

external_meta_t window_meta =
  {
    _T("window"),

    window_gc_mark_proc,
    window_gc_free_proc,
    NULL
  };

/*
 * The Scheme primitive functions for accessing window features.
 */


LRef lshow_window(LRef window)
{
  if (!WINDOWP(window))
     vmerror_wrong_type(1, window);

   return boolcons(WNDOB(window)->LShowWindow());
}

LRef lhide_window (LRef window)
{
  if (!WINDOWP(window))
     vmerror_wrong_type(1, window);

   return boolcons(WNDOB(window)->LHideWindow());
}

LRef lget_window_size(LRef window)
{
  if (!WINDOWP(window))
      vmerror_wrong_type(1, window);

    return WNDOB(window)->LGetWindowSize();
}

LRef lclose_window(LRef window)
{
  if (!WINDOWP(window))
      vmerror_wrong_type(1, window);

    WNDOB(window)->LClose();

    return NULL;        
}

LRef lupdate_window(LRef window)
{
  if (!WINDOWP(window))
      vmerror_wrong_type(1, window);

    WNDOB(window)->LUpdate();

    return NULL;        
}

LRef lflush_window(LRef window)
{
  if (!WINDOWP(window))
      vmerror_wrong_type(1, window);

    WNDOB(window)->LFlushDrawSurface();

    return NULL;        
}

LRef lget_window_placement(LRef window)
{
  if (!WINDOWP(window))
      vmerror_wrong_type(1, window);

    return WNDOB(window)->GetPlacement();
}

LRef lset_window_placement(LRef window, LRef placement)
{
  if (!WINDOWP(window))
    vmerror_wrong_type(1, window);

    return WNDOB(window)->SetPlacement(placement);
}


LRef lopen_editor(LRef window, LRef initial)
{
  if (!WINDOWP(window))
      vmerror_wrong_type(1, window);
    if (!STRINGP(initial))           
      vmerror_wrong_type(2, initial);
    
    WNDOB(window)->OpenEditor(get_c_string(initial));

    return NULL;
}

LRef lclose_editor(LRef window)
{
  if (!WINDOWP(window))
      vmerror_wrong_type(1, window);

    return WNDOB(window)->CloseEditor();
}

LRef lset_status_text(LRef window, LRef pane, LRef string)
{
  if (!WINDOWP(window))
      vmerror_wrong_type(1, window);
    if (!FIXNUMP(pane))              
      vmerror_wrong_type(2, pane);
    if (!STRINGP(string))            
      vmerror_wrong_type(3, string);

    WNDOB(window)->SetStatusText((long)FIXNM(pane), get_c_string(string));

    return NULL;
}

LRef lparent_repositioned(scan::LRef window)
{
  if (!WINDOWP(window))
      vmerror_wrong_type(1, window);
  
  WNDOB(window)->LParentRepositioned();
    
  return NULL;
}

LRef windowcons(CLispWnd *window)
{
    assert(window);

    LRef wnd_cptr = externalcons(window, NIL, &window_meta);

    window->DoCreateWindow();


    return wnd_cptr;
}
