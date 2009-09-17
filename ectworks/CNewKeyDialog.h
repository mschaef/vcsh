#pragma once
#include "afxwin.h"


// CNewKeyDialog dialog

class CNewKeyDialog : public CDialog
{
	DECLARE_DYNAMIC(CNewKeyDialog)

public:
	CNewKeyDialog(CWnd* pParent = NULL);   // standard constructor
	virtual ~CNewKeyDialog();

protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support

	DECLARE_MESSAGE_MAP()

	virtual BOOL OnInitDialog();

	CString m_NewKey;
	CEdit m_NewKeyCtrl;

    CStatic m_InstallMethodDescCtrl;
    CStatic m_InstallMethodIconCtrl;

    HICON m_userKeyIcon;
    HICON m_systemKeyIcon;

public:
	CString GetNewKey();
	void SetNewKey(CString newKey);	
};
