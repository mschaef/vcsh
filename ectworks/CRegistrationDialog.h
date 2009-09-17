#pragma once

#include "afxwin.h"
#include "TimeoutRegistration.h"
#include "CNewKeyDialog.h"

// CRegistrationDialog dialog

class CRegistrationDialog : public CDialog
{
	DECLARE_DYNAMIC(CRegistrationDialog)

public:
	CRegistrationDialog(CWnd* pParent);   // standard constructor
	virtual ~CRegistrationDialog();

protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support

	DECLARE_MESSAGE_MAP()

	CEdit m_SerialNumberCtrl;
	CEdit m_KeyCodeCtrl;
	CStatic m_LicenseTypeCtrl;
	
    CStatic m_KeyTypeDescCtrl;
	CStatic m_KeyTypeIconCtrl;

    HICON m_userKeyIcon;
    HICON m_systemKeyIcon;
    HICON m_noKeyIcon;


	afx_msg void OnBnClickedSetKeyCode();
	afx_msg void OnBnClickedPurchaseKeyCode();

	void UpdateLicenseInfo();

public:
	virtual BOOL OnInitDialog();
};
