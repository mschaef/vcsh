#pragma once

#include "afxwin.h"
#include "smartpicturestatic.h"

/* VCAboutBox.h
 *
 * VCalc About Box Routines
 */


// CAboutBox dialog

class CAboutBox : public CDialog
{
	DECLARE_DYNAMIC(CAboutBox)

public:
	CAboutBox(CWnd* pParent = NULL);
	virtual ~CAboutBox();

protected:
	// Controls
	CSmartPictureStatic m_BannerCtrl;
	CStatic m_VersionDescCtrl;
	CStatic m_LicenseDescCtrl;
	CEdit m_LicenseTextCtrl;
	CButton m_ViewOrderingInfoCtrl;
	CButton m_RegisterCtrl;

	// The bitmaps we use for the intro banner
	CBitmap m_LargeBanner;
	CBitmap m_SmallBanner;

	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support

	DECLARE_MESSAGE_MAP()

public:
	virtual BOOL OnInitDialog();

	afx_msg void OnBnClickedRegisterVcalc();
	afx_msg void OnBnClickedEmail();
};

