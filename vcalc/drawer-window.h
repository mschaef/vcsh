/****************************************************************
 * DrawerWindow.h
 * April 17th, 2004
 */

#ifndef __DRAWERWINDOW_H
#define __DRAWERWINDOW_H

#include <scan-types.h>

#include "lisp-window.h"
#include "image.h"

class CDrawer : public CLispWnd
{
public:
  CDrawer(scan::LRef host_instance, scan::LRef parent_peer);
	virtual ~CDrawer();

    DECLARE_DYNAMIC(CDrawer);

public:
    /* Window message handlers */
    DECLARE_MESSAGE_MAP()

	afx_msg void OnTimer(UINT nIDEvent);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg UINT OnNcHitTest(CPoint point);
	afx_msg void OnNcCalcSize(BOOL bCalcValidRects, NCCALCSIZE_PARAMS* lpncsp);
	afx_msg void OnNcPaint();
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnActivateApp(BOOL bActive, DWORD dwThreadID);
    afx_msg void OnPaint();

public:
    /* Parent window notification functions  */
    virtual void LParentRepositioned();

public:
    /* CLispWnd overrides */
    virtual bool LShowWindow();
    virtual bool LHideWindow();
    virtual scan::LRef GetPlacement();
    virtual scan::LRef SetPlacement(scan::LRef placement);

	virtual scan::LRef LGetWindowSize();

	virtual void DoCreateWindow();

protected:
    /* Drawer implementation functions */

    /* Drawer options */	
	enum Options {		
		AUTOSLIDE = 0x00000001, // Keep drawer visible by sliding left<->right
		AUTOHIDE  = 0x00000002, // Hide drawer when parent loses focus
		NOBORDER  = 0x00000004, // Avoid overlap with parent window captions
		ANIMATE   = 0x00000008, // Animate drawer transitions

		DEFAULT   = NOBORDER | ANIMATE,
	};

	void SetOptions(int options);
	int GetOptions();

	/* Drawer Placement */
	enum Sides { LEFT, RIGHT, BOTTOM };

	void SetOffsets(int low, int high); // Offset in pixels from window border 
	void SetSize(int size);             // Amount drawer extends from parent
	void SetSide(Sides side);           // Side of parent window on which drawer resides

	/* Drawer opener/closer */
	bool OpenDrawer(bool open);

	bool IsOpen();
    bool IsClosed();
   	bool IsOpening();
    bool IsClosing();



    /* Drawer sliding support - Allows the drawer contents to slide
     * with the drawer */
    virtual void ResizeContent();
    CSize GetContentSize();
    CPoint GetContentOrigin();

	/* Determine if the drawer, given the proposed side, would
	 * fit within the bounds of the current monitor */
	bool OutOfMonitorBounds(Sides side);

    /* Update the current location of the drawer window */
    void UpdateDrawerLocation();

	
    /* Member variables */
	CWnd *_drawer_parent;
	bool _created;
	bool _parent_minimized;	
	bool _hidden;

	Sides _side;
	Sides _lastSide;
	Sides _actingSide;
	int _ofs_low, _ofs_high;
	int _size;
	int _size_adjust;
	int _size_step;

	int _options;

};

#endif // __DRAWERWINDOW_H
