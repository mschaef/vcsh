/****************************************************************
 * LispWindow.h
 */

#ifndef __LISP_WINDOW_H
#define __LISP_WINDOW_H

#include "stdafx.h"
#include "KeyboardTranslation.h"
#include "EnterNotifyEdit.h"

/* The MFC implementation of the basic Lisp window. */

class CLispWnd : public CWnd, public ILispInstance
{
public:
  CLispWnd(scan::LRef host_instance, scan::LRef parent_peer);
  virtual ~CLispWnd();
    
public:
    /* Window message handlers */

    DECLARE_MESSAGE_MAP()
    DECLARE_DYNAMIC(CLispWnd)

    afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
    afx_msg void OnClose();
    afx_msg void OnDestroy();
    afx_msg void OnPaint();
    afx_msg void OnSize(UINT nType, int cx, int cy);
    afx_msg void OnMove(int x, int y);
    afx_msg void OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags);
    afx_msg void OnMouseMove(UINT nFlags, CPoint point);
    afx_msg void OnDropFiles(HDROP hDropInfo);
    afx_msg BOOL OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message);

    virtual BOOL OnCommand(WPARAM wParam, LPARAM lParam);
   
public:
    /* Parent window notification functions  */
    virtual void LParentRepositioned();

protected:
    HICON   m_Icon;
    HCURSOR m_ArrowCursor;
    CBitmap m_BackgroundImage;

    void DrawEmptyBackground(HDC dc);
public:
    /* Overridable virtuals */
    virtual void DoSizeWindow(int cx, int cy);
    virtual void DoPaint(CDC &dc, int xOrig, int yOrig);
    virtual void DoCreateWindow();
    virtual scan::LRef DoProcessCommand(scan::LRef command, scan::LRef arg);

public:
    /* The public API */
    virtual void LClose();
    virtual void LUpdate();
    virtual void LFlushDrawSurface();
    virtual bool LShowWindow();
    virtual bool LHideWindow();

    virtual scan::LRef GetPlacement();
    virtual scan::LRef SetPlacement(scan::LRef placement);


    virtual scan::LRef LGetWindowSize();

    virtual void OpenEditor(_TCHAR *initial);
    virtual scan::LRef CloseEditor();
    virtual void SetStatusText(long pane, _TCHAR *string);

    virtual void GCMark();

    virtual scan::LRef GetInstanceSlotValue(scan::LRef slotName, scan::LRef defaultValue = NIL);
    virtual scan::LRef SendInstanceMessage(scan::LRef message, scan::LRef args = NIL);

    scan::LRef _lisp_instance; // A back pointer to the host Lisp object
    scan::LRef _parent_peer;
    
    int _windowWidth, _windowHeight;
    bool _windowBusy;

    CEnterNotifyEdit m_Editor;
};

/* Metaclass implementation functions */

void window_write (scan::LRef ptr, scan::LRef port, bool machine_readable);
void window_gc_free(scan::LRef window);
scan::LRef window_gc_mark (scan::LRef ptr);

/* Window API  - Basic Windows */

extern external_meta_t window_meta;

inline bool WINDOWP(scan::LRef obj) 
{
  return EXTERNALP(obj) && (EXTERNAL_META(obj) == &window_meta);
}

inline CLispWnd *WNDOB(scan::LRef obj)
{
  return ((CLispWnd *)(EXTERNAL_DATA(obj)));
}

scan::LRef windowcons(CLispWnd *window);
scan::LRef lshow_window (scan::LRef window);
scan::LRef lhide_window (scan::LRef window);
scan::LRef lget_window_size(scan::LRef window);
scan::LRef lclose_window(scan::LRef window);
scan::LRef lupdate_window(scan::LRef window);
scan::LRef lflush_window(scan::LRef window);
scan::LRef lget_window_placement(scan::LRef window);
scan::LRef lset_window_placement(scan::LRef window, scan::LRef placement);
scan::LRef lopen_editor(scan::LRef window, scan::LRef initial);
scan::LRef lclose_editor(scan::LRef window);
scan::LRef lset_status_text(scan::LRef window, scan::LRef pane, scan::LRef string);
scan::LRef lparent_repositioned(scan::LRef window);

#endif // __LISP_WINDOW_H
