/****************************************************************
 * DrawerWindow.cpp
 * April 17th, 2004
 */

#include "stdafx.h"
#include "drawer-window.h"

#include <scan-assert.h>

#include "vcalc.h"

/**************************************************************
 * SafeSetWindowTheme(m_hWnd, appName, IDlist)
 *
 * Sets the window theme of a given window safely. That is, it
 * does not statically link to uxtheme.dll and will load on systems
 * that don't support theming. On these systems, it will just
 * report failure by returning ERROR_NOT_SUPPORTED. */

typedef HRESULT (WINAPI *PSWT)(HWND, LPCWSTR, LPCWSTR);
PSWT pSetWindowTheme;

HRESULT SafeSetWindowTheme(HWND hwnd, LPCWSTR pszSubAppName, LPCWSTR pszSubIdList)
{
  /* First, see if we can get the API call we need */
  HMODULE hDLL = LoadLibrary("uxtheme");

  if (hDLL == NULL)
    return ERROR_NOT_SUPPORTED;

  pSetWindowTheme = 
    (PSWT) GetProcAddress(hDLL, "SetWindowTheme");

  if (pSetWindowTheme == NULL)
    return ERROR_NOT_SUPPORTED;

  return pSetWindowTheme(hwnd, pszSubAppName, pszSubIdList);
}

/****************************************************************
 * CDrawer
 */

#define DRAWER_DRAG_HANDLE_SIZE (8)

#define ANIMATE_TIMER    (1)

// Time units in milliseconds
#define ANIMATE_TIMESTEP (12)   
#define ANIMATE_DURATION (120)

IMPLEMENT_DYNAMIC(CDrawer, CLispWnd)

CDrawer::CDrawer(LRef host_instance, LRef parent_peer) 
: CLispWnd(host_instance, parent_peer)
{
  assert(WINDOWP(parent_peer));

  _drawer_parent = WNDOB(parent_peer);

  /* Initial state... */
  _created            = FALSE;    
  _parent_minimized   = FALSE;
    
  _hidden             = FALSE;

  /* Default settings... */
  _side       = RIGHT;    
  _lastSide   = BOTTOM; // Different than _side to force SWP_FRAMECHANGED

  _ofs_low    = 0;
  _ofs_high   = 0;

  _size                   = 200;
  _size_adjust            = -_size;
  _size_step              = 0;
  _options                = DEFAULT;
}

CDrawer::~CDrawer()
{
}

/*
 * Message Handlers
 */

BEGIN_MESSAGE_MAP(CDrawer, CLispWnd)
ON_WM_TIMER()
//    ON_WM_NCHITTEST() - results in odd compiler error
ON_WM_NCCALCSIZE()
ON_WM_NCPAINT()
ON_WM_ACTIVATEAPP()
ON_WM_CREATE()
ON_WM_PAINT()
ON_WM_SIZE()
END_MESSAGE_MAP()


void CDrawer::OnTimer(UINT nIDEvent)
{
  if (nIDEvent == ANIMATE_TIMER)
    {
      _size_adjust += _size_step;

      if (IsClosing())
        {
          if (_size_adjust <= -_size)
            {
              _size_adjust = -_size;
              _size_step = 0;
            }
        }
      else if (IsOpening())
        {
          if (_size_adjust >= 0)
            {
              _size_adjust = 0;
              _size_step = 0;
            }
        }

      UpdateDrawerLocation(); 


      if (!IsOpening() && !IsClosing())
        KillTimer(ANIMATE_TIMER);
    }   

  CLispWnd::OnTimer(nIDEvent);
}

void CDrawer::OnSize(UINT nType, int cx, int cy)
{
  if (!IsOpening() && !IsClosing())
    {
      switch(_side)
        {
        case LEFT:
        case RIGHT:
          _size = cx + DRAWER_DRAG_HANDLE_SIZE;
          break;

        case BOTTOM:
          _size = cy + DRAWER_DRAG_HANDLE_SIZE;
        }
    }

  if (IsOpen()) ResizeContent();

  // NOTE: Skipping CLispWnd's OnSize!
  CWnd::OnSize(nType, cx, cy);
}

UINT CDrawer::OnNcHitTest(CPoint point)
{
  LONG x, y;

  CRect windowRect;
  GetWindowRect(windowRect);

  x = point.x - windowRect.left;
  y = point.y - windowRect.top;

  if (IsOpening() || IsClosing())
    return HTBORDER;

  switch(_actingSide)
    {
    case LEFT:
      if (x <= DRAWER_DRAG_HANDLE_SIZE)
        return HTLEFT;
      else
        return HTCLIENT;
      break;

    case RIGHT:
      if (x >= windowRect.Width() - DRAWER_DRAG_HANDLE_SIZE)
        return HTRIGHT;
      else
        return HTCLIENT;
      break;

    case BOTTOM:
      if (y >= windowRect.Height() - DRAWER_DRAG_HANDLE_SIZE)
        return HTBOTTOM;
      else
        return HTCLIENT;

    default:
      return HTBORDER;
    }

}

void CDrawer::OnNcCalcSize(BOOL bCalcValidRects, NCCALCSIZE_PARAMS* lpncsp)
{
  UNREFERENCED(bCalcValidRects);
  
  switch(_actingSide)
    {
    case LEFT:
      lpncsp->rgrc[0].left += DRAWER_DRAG_HANDLE_SIZE;

      lpncsp->rgrc[0].top     += 2;
      lpncsp->rgrc[0].bottom  -= 2;
      break;
    
    case RIGHT:
      lpncsp->rgrc[0].right -= DRAWER_DRAG_HANDLE_SIZE;

      lpncsp->rgrc[0].top     += 2;
      lpncsp->rgrc[0].bottom  -= 2;
      break;
        
    case BOTTOM:
      lpncsp->rgrc[0].left  += 2;
      lpncsp->rgrc[0].right -= 2;

      lpncsp->rgrc[0].bottom  -= DRAWER_DRAG_HANDLE_SIZE;
      break;
    }
}


void CDrawer::OnNcPaint()
{
  CDC *dc = GetWindowDC();

  CPen nullPen(PS_NULL, 0, RGB(0, 0, 0));

  CBrush faceBrush(GetSysColor(COLOR_3DFACE));
  CPen facePen(PS_SOLID, 0, GetSysColor(COLOR_3DFACE));
  CPen lightPen(PS_SOLID, 0, GetSysColor(COLOR_3DHIGHLIGHT));
  CPen shadowPen(PS_SOLID, 0, GetSysColor(COLOR_3DSHADOW));

  /*
    CBrush faceBrush(RGB(0, 0, 0xFF));
    CPen facePen(PS_SOLID, 0, RGB(0xFF, 0xFF, 0));
    CPen lightPen(PS_SOLID, 0, RGB(0xFF, 0, 0));
    CPen shadowPen(PS_SOLID, 0, RGB(0, 0xFF, 0));
  */

  dc->SaveDC(); 
  {
    CRect windowRect;
    GetWindowRect(windowRect);

    if (_actingSide == BOTTOM)
      {
        dc->SelectObject(lightPen);
        dc->MoveTo(0, 0);
        dc->LineTo(0, windowRect.Height() - 1);

        dc->SelectObject(facePen);
        dc->MoveTo(1, 0);
        dc->LineTo(1, windowRect.Height() - 1);

        dc->MoveTo(windowRect.Width() - 2, 0);
        dc->LineTo(windowRect.Width() - 2, windowRect.Height() - 1);

        dc->SelectObject(shadowPen);
        dc->MoveTo(windowRect.Width() - 1, 0);
        dc->LineTo(windowRect.Width() - 1, windowRect.Height() - 1);


        dc->Draw3dRect(0, windowRect.Height() - DRAWER_DRAG_HANDLE_SIZE, 
                       windowRect.Width(), DRAWER_DRAG_HANDLE_SIZE,
                       GetSysColor(COLOR_3DHIGHLIGHT),
                       GetSysColor(COLOR_3DSHADOW));


        dc->SelectObject(nullPen);
        dc->SelectObject(faceBrush);
        dc->Rectangle(1, windowRect.Height() - DRAWER_DRAG_HANDLE_SIZE + 1, 
                      windowRect.Width(), windowRect.Height() );
      }
    else
      {
        dc->SelectObject(lightPen);
        dc->MoveTo(0, 0);
        dc->LineTo(windowRect.Width() - 1, 0);

        dc->SelectObject(facePen);
        dc->MoveTo(0, 1);
        dc->LineTo(windowRect.Width() - 1, 1);
        dc->MoveTo(0, windowRect.Height() - 2);
        dc->LineTo(windowRect.Width(), windowRect.Height() - 2);

        dc->SelectObject(shadowPen);
        dc->MoveTo(0, windowRect.Height() - 1);
        dc->LineTo(windowRect.Width(), windowRect.Height() - 1);

        if (_actingSide == LEFT)
          {
            dc->Draw3dRect(0, 0, 
                           DRAWER_DRAG_HANDLE_SIZE, windowRect.Height() ,
                           GetSysColor(COLOR_3DHIGHLIGHT),
                           GetSysColor(COLOR_3DSHADOW));


            dc->SelectObject(nullPen);
            dc->SelectObject(faceBrush);
            dc->Rectangle(1, 1, DRAWER_DRAG_HANDLE_SIZE, windowRect.Height() );
          }
        else
          {
            assert(_actingSide == RIGHT);

            dc->Draw3dRect(windowRect.Width() - DRAWER_DRAG_HANDLE_SIZE, 0, 
                           DRAWER_DRAG_HANDLE_SIZE, windowRect.Height() ,
                           GetSysColor(COLOR_3DHIGHLIGHT),
                           GetSysColor(COLOR_3DSHADOW));


            dc->SelectObject(nullPen);
            dc->SelectObject(faceBrush);
            dc->Rectangle(windowRect.Width() - DRAWER_DRAG_HANDLE_SIZE + 1, 1,
                          windowRect.Width(), windowRect.Height() );
          }

      }
  }
  dc->RestoreDC(-1);

  ReleaseDC(dc);
}


int CDrawer::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
  if (CLispWnd::OnCreate(lpCreateStruct) == -1)
    return -1;

  SafeSetWindowTheme(m_hWnd, L" ", L" ");

  return 0;
}

void CDrawer::OnActivateApp(BOOL bActive, DWORD dwThreadID)
{
  CLispWnd::OnActivateApp(bActive, dwThreadID);

  _hidden = !bActive;

  // !!! This causes problems with main window resizing.
  UpdateDrawerLocation();
}

void CDrawer::OnPaint()
{
  Invalidate(FALSE);

  CPaintDC dc(this);

  CPoint origin = GetContentOrigin();

  DoPaint(dc, origin.x, origin.y);
}

/* 
 * Parent window notification functions 
 */

void CDrawer::LParentRepositioned()
{
  UpdateDrawerLocation();
}


/*
 * CLispWnd Overrides
 */

bool CDrawer::LShowWindow()
{
  return OpenDrawer(true);
}

bool CDrawer::LHideWindow()
{
  return OpenDrawer(false);
}


void CDrawer::DoCreateWindow()
{
  bool success;

  CRect dummyLocation(0, 0, 100, 100);

  CString MyClass = AfxRegisterWndClass(CS_VREDRAW | CS_HREDRAW, NULL,
                                        (HBRUSH) ::GetStockObject(NULL_BRUSH), NULL);

  CString windowName("Foo");

  success = ( CreateEx(0, MyClass,  windowName,  WS_THICKFRAME | WS_POPUP,
                       dummyLocation, _drawer_parent, 0) != 0 );
    
  _options = DEFAULT;

  if (success)
    _created = true;
}

/*
 * Drawer implementation functions
 */

void CDrawer::SetOptions(int options)
{
  _options = options;

  UpdateDrawerLocation();
}

int CDrawer::GetOptions()
{
  return _options;
}

void CDrawer::SetOffsets(int low, int high)
{
  _ofs_low  = low;
  _ofs_high = high;
}

void CDrawer::SetSide(Sides side)
{
  _side = side;

  UpdateDrawerLocation();

  SetWindowPos(NULL, 0, 0, 0, 0,
               SWP_FRAMECHANGED | SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER); 
}

LRef CDrawer::GetPlacement()
{
  LRef side_sym = NULL;

  switch (_side)
    {
    case LEFT:      side_sym = keyword_intern(_T("left"));   break;
    case RIGHT:     side_sym = keyword_intern(_T("right"));  break;
    case BOTTOM:    side_sym = keyword_intern(_T("bottom")); break;
    default:        assert("Invalid drawer size");
    }

  return llist2vector(listn(5,
                            side_sym,
                            boolcons(IsClosing() || IsClosed()),
                            fixcons(_size),
                            fixcons(_ofs_low),
                            fixcons(_ofs_high)));
}

LRef CDrawer::SetPlacement(LRef new_placement)
{
  Sides new_side = _side;
  int new_size = _size;
  int new_ofs_low = 0;
  int new_ofs_high = 0;
  bool valid_placement_length = FALSE;

  bool do_open_window = FALSE;
  bool do_close_window = FALSE;
  bool closed = IsClosed() || IsClosing();

  LRef old_placement = GetPlacement();
  LRef temp;

  if (!VECTORP(new_placement)) 
    vmerror_wrong_type(1, new_placement);
  
  // Parse out the drawer side...    
  if (VECTOR_LENGTH(new_placement) >= 1)
    {
      temp = VECTOR_OBJECT(new_placement, 0);

      if (!SYMBOLP(temp))
        vmerror("Invalid side specifier in drawer placement.", temp);

      if (temp == keyword_intern(_T("left")))
        new_side = LEFT;
      else if (temp == keyword_intern(_T("right")))
        new_side = RIGHT;
      else if (temp == keyword_intern(_T("bottom")))
        new_side = BOTTOM;
      else
        vmerror("Invalid drawer side", temp);

      valid_placement_length = TRUE;
    }

  // Parse out the visibility...
  if (VECTOR_LENGTH(new_placement) >= 2)
    {
      temp = VECTOR_OBJECT(new_placement, 1);

      if (!BOOLP(temp))
        vmerror("Invalid visibility in drawer placement.", temp);

      if (TRUEP(temp))
        do_close_window = TRUE;
      else 
        do_open_window = TRUE;

      valid_placement_length = TRUE;
    }

  // Parse out the size...
  if (VECTOR_LENGTH(new_placement) >= 3)
    {
      temp = VECTOR_OBJECT(new_placement, 2);
      if (!FIXNUMP(temp))
        vmerror("Invalid size in drawer placement.", temp);

      new_size = (int)get_c_fixnum(temp);

      valid_placement_length = TRUE;
    }

  // Finally, the offsets...
  if (VECTOR_LENGTH(new_placement) >= 5)
    {
      temp = VECTOR_OBJECT(new_placement, 3);

      if (!FIXNUMP(temp))
        vmerror("Invalid lower offset in drawer placement.", temp);

      new_ofs_low = (int)get_c_fixnum(temp);

      temp = VECTOR_OBJECT(new_placement, 4);

      if (!FIXNUMP(temp))
        vmerror("Invalid upper offset in drawer placement.", temp);

      new_ofs_high = (int)get_c_fixnum(temp);

      valid_placement_length = (VECTOR_LENGTH(new_placement) == 5);
    }

  // If we're here, we've been able to parse the placement vector, 
  // and can commit our work if the placement vector is not too
  // long.
  switch(VECTOR_LENGTH(new_placement))
    {
    case 5:
      _ofs_low    = new_ofs_low;
      _ofs_high   = new_ofs_high;

    case 3:
      _size       = new_size;

    case 2:
    case 1:
      _side       = new_side;
      _actingSide = new_side;
      break;

    default:
      vmerror("Invalid drawer placement vector, bad length", new_placement);
    }

  if (do_open_window)
    closed = FALSE;
  else if (do_close_window)
    closed = TRUE;

  if (closed)
    _size_adjust = -_size;
  else
    _size_adjust = 0;
        
  UpdateDrawerLocation();

  return old_placement;
}

LRef CDrawer::LGetWindowSize()
{
  CSize contents_size = GetContentSize();

  return lcons(fixcons(contents_size.cx), fixcons(contents_size.cy));
}

bool CDrawer::OpenDrawer(bool open)
{
  bool was_open = IsOpen();

  if (was_open != open)
    {            
      if (_options & ANIMATE)
        {
          if (open)
            _size_step = (_size * ANIMATE_TIMESTEP) / ANIMATE_DURATION;                 
          else
            _size_step = -(_size * ANIMATE_TIMESTEP) / ANIMATE_DURATION;                       
                
                
          SetTimer(ANIMATE_TIMER, ANIMATE_TIMESTEP, NULL);
        }
      else
        {
          if (open)
            _size_adjust = 0;                
          else
            _size_adjust = -_size;
        }

      if(open)
        ResizeContent();

      UpdateDrawerLocation();
    }

  return was_open;
}

bool CDrawer::IsOpen()
{
  return (_size_adjust >= 0) && (_size_step == 0);
}

bool CDrawer::IsClosed()
{
  return (_size_adjust <= -_size) && (_size_step == 0);
}

bool CDrawer::IsOpening()
{
  return (_size_step > 0);
}

bool CDrawer::IsClosing()
{
  return (_size_step < 0);
}



void CDrawer::ResizeContent()
{
  CSize content_size = GetContentSize();

  DoSizeWindow(content_size.cx, content_size.cy);
}

CSize CDrawer::GetContentSize()
{
  int height = 0;
  int width = 0;
  CRect parentRect;
  CWnd *parent = _drawer_parent;

  assert(parent);

  parent->GetWindowRect(parentRect);

  switch(_side) // Not worrying about acting side, since it does not impact contents size.
    {
    case LEFT:
    case RIGHT:
      height = parentRect.Height() - _ofs_low - _ofs_high;

      if (_options & NOBORDER)
        height -= (  (2 * GetSystemMetrics(SM_CYSIZEFRAME))
                     + GetSystemMetrics(SM_CYCAPTION));
        
      return CSize(_size, height);
    
    case BOTTOM:
      width = parentRect.Width() - _ofs_low - _ofs_high;

      if (_options & NOBORDER)
        width -= (2 * GetSystemMetrics(SM_CXSIZEFRAME));

      return CSize(width, _size);

    default:
      return CSize(0, 0);
    }
}

CPoint CDrawer::GetContentOrigin()
{
  switch(_actingSide)
    {
    case LEFT:  return CPoint(0, 0);
    case RIGHT: return CPoint(-_size_adjust, 0);        
    case BOTTOM:return CPoint(0, -_size_adjust);
    default:    return CPoint(0, 0);
    }    
}



bool CDrawer::OutOfMonitorBounds(Sides side)
{
  CRect parentRect;

  CWnd *parent = _drawer_parent;
  parent->GetWindowRect(parentRect);

  HMONITOR hMon = MonitorFromWindow(parent->GetSafeHwnd(), MONITOR_DEFAULTTONEAREST);

  if (hMon == NULL)
    return FALSE;

  MONITORINFO mInfo;
  mInfo.cbSize = sizeof(MONITORINFO);
    
  if (!GetMonitorInfo(hMon, &mInfo))
    return FALSE;

  CRect monitorRect(mInfo.rcMonitor);

  switch (side)
    {
    case LEFT:  return (parentRect.left - _size < monitorRect.left);
    case RIGHT: return (parentRect.right + _size > monitorRect.right);
    case BOTTOM:return FALSE;
    }

  return FALSE;
}

void CDrawer::UpdateDrawerLocation()
{
  if (!_created)
    return;

  if (_parent_minimized  || _hidden || IsClosed())            
    {   
      ShowWindow(SW_HIDE);
      return;
    }

  CRect parentRect;
  CWnd *parent = _drawer_parent;

  assert(parent);

  parent->GetWindowRect(parentRect);

  int x, y, cx, cy;

  _actingSide = _side;

  if ((_actingSide == LEFT)  || (_actingSide == RIGHT))
    {
      /* If the drawer fits better on the opposite side
       * of the window, this just moves it over temporarily. 
       *
       * !! In an ideal world, the slide operation would be animated. */
      if (_options & AUTOSLIDE)
        {
          if     (   (_side == LEFT)
                     && (OutOfMonitorBounds(LEFT))
                     && (!OutOfMonitorBounds(RIGHT)))
            _actingSide = RIGHT;
          else if (   (_side == RIGHT)
                      && (OutOfMonitorBounds(RIGHT))
                      && (!OutOfMonitorBounds(LEFT)))
            _actingSide = LEFT;
        }
        

      if (_actingSide == LEFT)
        x = parentRect.left - _size - _size_adjust;
      else 
        x = parentRect.right;

      cx = _size + _size_adjust;


      y  = parentRect.top + _ofs_low;         
      cy = parentRect.Height() - _ofs_low - _ofs_high;

      if (_options & NOBORDER)
        {
          y  += (     GetSystemMetrics(SM_CYSIZEFRAME)  + GetSystemMetrics(SM_CYCAPTION));
          cy -= ((2 * GetSystemMetrics(SM_CYSIZEFRAME)) + GetSystemMetrics(SM_CYCAPTION));
        }
    } 
  else
    {
      assert(_actingSide == BOTTOM);

      x  = parentRect.left;
      cx = parentRect.Width() - _ofs_low - _ofs_high;

      if (_options & NOBORDER)
        {
          x  +=      GetSystemMetrics(SM_CXSIZEFRAME);
          cx -= (2 * GetSystemMetrics(SM_CXSIZEFRAME));
        }

      y  = parentRect.bottom;
      cy = _size + _size_adjust;
    }

  bool updateFrame = (_lastSide != _actingSide);
  _lastSide = _actingSide;

  SetWindowPos(parent, 
               x, y, cx, cy,
               (updateFrame ? SWP_FRAMECHANGED : 0)  | SWP_SHOWWINDOW | SWP_NOZORDER | SWP_NOACTIVATE);
}
