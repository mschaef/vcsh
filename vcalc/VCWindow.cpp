/* VCWindow.cpp
 *
 * This module contains the implementation of the main VCalc window.
 */

#include "stdafx.h"
#include "vcalc.h"
#include "image.h"
#include "VCChooseBox.h"
#include "VCWindow.h"

#include "../vm/scan.h"

/****************************************************************
 * CVCWindow
 */

IMPLEMENT_DYNAMIC(CVCWindow, CLispWnd)
CVCWindow::CVCWindow(LRef host_instance, LRef parent_peer)
: CLispWnd(host_instance, parent_peer)
{
}

CVCWindow::~CVCWindow()
{
}


BEGIN_MESSAGE_MAP(CVCWindow, CLispWnd)
	ON_WM_CREATE()
END_MESSAGE_MAP()



// CVCWindow message handlers

static UINT indicators[] =
{
	ID_SEPARATOR,
	ID_SEPARATOR,
	ID_SEPARATOR,
	ID_SEPARATOR,
    ID_SEPARATOR,
};

int CVCWindow::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	if (CLispWnd::OnCreate(lpCreateStruct) == -1)
		return -1;

	DragAcceptFiles(TRUE);

	if (!m_Editor.CreateEx(WS_EX_CLIENTEDGE, 
		                   "EDIT",
     					    NULL,
		                      WS_CHILD | WS_DISABLED | WS_VISIBLE | WS_TABSTOP | WS_BORDER,
				            CRect(0, 0, 0, 32),
				            this,
				            1,
					        NULL))
	{
		TRACE0("Failed to create m_Editor");
		return -1;
	}

	if (!m_Menu.LoadMenu(IDR_MAINMENU))
	{
		TRACE0("Failed to load m_Menu");
		return -1;
	}


	m_Icon   = VCalcGetApp()->LoadIcon(IDI_MAIN);

	SetIcon(m_Icon, TRUE);

	SetMenu(&m_Menu);	

	if (!m_StatusBar.Create(this))
	{
		
		TRACE0("Failed to create m_StatusBar");
		return -1;
	}

 	m_StatusBar.SetIndicators(indicators, sizeof(indicators)/sizeof(UINT));
	
	
	TEXTMETRIC tm;
	{
		CClientDC dc(NULL);
		HFONT hFont = (HFONT)m_StatusBar.SendMessage(WM_GETFONT);
		HGDIOBJ hOldFont = NULL;
		if (hFont != NULL)
			hOldFont = dc.SelectObject(hFont);
		VERIFY(dc.GetTextMetrics(&tm));
		if (hOldFont != NULL)
			dc.SelectObject(hOldFont);
	}

    
	m_StatusBar.SetPaneInfo(0, 0, SBPS_STRETCH | SBPS_NOBORDERS, 0);
	m_StatusBar.SetPaneInfo(1, 0, 0, tm.tmAveCharWidth * 8);
	m_StatusBar.SetPaneInfo(2, 0, 0, tm.tmAveCharWidth * 8);	
    m_StatusBar.SetPaneInfo(3, 0, 0, tm.tmAveCharWidth * 8);
    m_StatusBar.SetPaneInfo(4, 0, 0, tm.tmAveCharWidth * 16);
    

	return 0;
}


void CVCWindow::DoSizeWindow(int cx, int cy)
{
	if (IsWindow(m_StatusBar))
	{
		CSize s;
		s =		m_StatusBar.CalcFixedLayout(TRUE, TRUE);

		cy -= s.cy;		
		m_StatusBar.SetWindowPos(NULL, 0, cy, cx, s.cy, SWP_NOREDRAW);
	}

	if (IsWindow(m_Editor))
	{
		CRect editorRect;
		m_Editor.GetWindowRect(editorRect);

		cy -= editorRect.Height();
	}

	CRect editorRect;
	m_Editor.GetWindowRect(editorRect);

	m_Editor.SetWindowPos(NULL, 0, cy, cx, editorRect.Height(), 0);

    CLispWnd::DoSizeWindow(cx, cy);
}


void CVCWindow::OnDestroy()
{
	CWnd::OnDestroy();
}

/**************************************************************
 * Utility functions
 */


void CVCWindow::OpenEditor(_TCHAR *initial)
{
  m_Editor.SetParent(this);
  m_Editor.EnableWindow(TRUE);
  m_Editor.SetFocus();
  m_Editor.SetWindowText(CString(initial));
  m_Editor.SetSel(m_Editor.GetWindowTextLength(), m_Editor.GetWindowTextLength());
}

LRef CVCWindow::CloseEditor()
{
	CString text;
	m_Editor.GetWindowText(text);

    LRef retval = strcons( text.GetBuffer());

    m_Editor.EnableWindow(FALSE);
    m_Editor.SetWindowText("");

    SetFocus();

    return retval;
}

void CVCWindow::SetStatusText(long pane, _TCHAR *string)
{
	m_StatusBar.SetPaneText(pane, string);
}

