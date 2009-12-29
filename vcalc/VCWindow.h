
/* VCWindow.h
 *
 * This module contains the implementation of the main VCalc window.
 */

#ifndef __VCWINDOW_H
#define __VCWINDOW_H

#include "../vm/scan.h"
#include "afxwin.h"

#include "lisp-window.h"

#include "EnterNotifyEdit.h"



/* The MFC implementation class */

class CVCWindow : public CLispWnd
{
	DECLARE_DYNAMIC(CVCWindow)

public:
  CVCWindow(scan::LRef host_instance, scan::LRef parent_peer);
	virtual ~CVCWindow();

protected:
	DECLARE_MESSAGE_MAP()

public:
	
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnDestroy();
	afx_msg void OnMove(int x, int y);	
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnClose();

        virtual void DoSizeWindow(int cx, int cy);

	virtual void OpenEditor(_TCHAR *initial);
    virtual LRef CloseEditor();
	virtual void SetStatusText(long pane, _TCHAR *string);

        //    virtual void DoSizeWindow(int cx, int cy);

public:
	CStatusBar m_StatusBar;
	CMenu m_Menu;

    LRef _drawer;    
};


#endif // __VCWINDOW_H 
