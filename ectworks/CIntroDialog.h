/* CIntroDialog.cpp 
 * Copyright 2003-2004, East Coast Toolworks, LLC
 * 
 * This is the implementation code for the introduction dialog 
 * box: the dialog that is displayed to the user immediately upon startup.
 */

#ifndef __CINTRODIALOG_H
#define __CINTRODIALOG_H

#include "afxwin.h"
#include "../util/base-types.h"
#include "smartpicturestatic.h"

// CIntroDialog dialog

class CIntroDialog : public CDialog
{
	DECLARE_DYNAMIC(CIntroDialog)

public:
	CIntroDialog(CWnd* pParent = NULL);
	virtual ~CIntroDialog();

protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support

	DECLARE_MESSAGE_MAP()

	enum { STARTUP_TIMER = 1 };

	// Control variables
	CButton m_AgreeCtrl;
	CStatic m_StatusCtrl;
	CStatic m_IntroTextCtrl;
	CEdit m_LicenseCtrl;
	CSmartPictureStatic m_BannerCtrl;
	CStatic m_LicenseInfoCtrl;
	CButton m_EnterRegCtrl;
	CButton m_QuitCtrl;


	// The font we use for the intro text
	CFont m_IntroTextFont;

	// The bitmaps we use for the intro banner
	CBitmap m_LargeBanner;
	CBitmap m_SmallBanner;


	// Overrides and message implementations
	virtual BOOL OnInitDialog();
	afx_msg void OnTimer(UINT nIDEvent);
	virtual void OnOK();
	virtual void OnCancel();

	// Command implementations
	afx_msg void OnBnClickedViewEvaluationLicense();
	afx_msg void OnBnClickedViewOrderingInfo();
	afx_msg void OnBnClickedEnterRegistrationKey();
	afx_msg void OnBnClickedAgree();
	afx_msg void OnBnClickedQuit();

	// Internal state variables
    i32 m_DaysInstalled;
	i32 m_StartupTimeout;
	UINT m_licenseResource;

	void UpdateStatusText();

public:
	BOOL UserAgreesToLicense();		
};

#endif // __CINTRODIALOG_H
