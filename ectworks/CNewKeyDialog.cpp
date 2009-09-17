// CNewKeyDialog.cpp : implementation file
//

#include "stdafx.h"

#include "CNewKeyDialog.h"

#include <util-resource.h>

#include "registration.h"

// CNewKeyDialog dialog

IMPLEMENT_DYNAMIC(CNewKeyDialog, CDialog)
CNewKeyDialog::CNewKeyDialog(CWnd* pParent /*=NULL*/)
	: CDialog(IDD_NEW_KEY_DIALOG, pParent)
	, m_NewKey(_T(""))
{
    m_userKeyIcon       = AfxGetApp()->LoadIcon(IDI_USER_ICON);
    m_systemKeyIcon     = AfxGetApp()->LoadIcon(IDI_SYSTEM_ICON);
}

CNewKeyDialog::~CNewKeyDialog()
{
}

void CNewKeyDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	DDX_Text(pDX, IDC_NEW_KEY, m_NewKey);
	DDX_Control(pDX, IDC_NEW_KEY, m_NewKeyCtrl);
    DDX_Control(pDX, IDC_INSTALL_METHOD_DESC, m_InstallMethodDescCtrl);
    DDX_Control(pDX, IDC_INSTALL_METHOD_ICON, m_InstallMethodIconCtrl);
}


BEGIN_MESSAGE_MAP(CNewKeyDialog, CDialog)
END_MESSAGE_MAP()

/**************************************************************
 * Dialog Behavior (Message handlers, implementation, 
 * and virtual function overrides)
 */

BOOL CNewKeyDialog::OnInitDialog()
{
	CDialog::OnInitDialog();

	m_NewKeyCtrl.SetSel(0, -1);

    Registration *reg = Registration::GetRegistrationObject();

    CString msg;

    
    

    if (reg->HaveSystemScopeRights())
    {
        msg.LoadString(IDS_SYSTEM_KEY_INSTALL);
        m_InstallMethodDescCtrl.SetWindowText(msg);
        m_InstallMethodIconCtrl.SetIcon(m_systemKeyIcon);
    }
    else
    {
        msg.LoadString(IDS_USER_KEY_INSTALL);
        m_InstallMethodDescCtrl.SetWindowText(msg);
        m_InstallMethodIconCtrl.SetIcon(m_userKeyIcon);
    }

	return TRUE;
}

/**************************************************************
 * External Interface
 **************************************************************/

void CNewKeyDialog::SetNewKey(CString newKey)
{
	m_NewKey = newKey;
}

CString CNewKeyDialog::GetNewKey()
{
	return m_NewKey;
}
