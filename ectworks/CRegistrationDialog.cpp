// CRegistrationDialog.cpp : implementation file
//

#define OEMRESOURCE
#include "stdafx.h"
#include <scan-assert.h>
#include <util-resource.h>

#include "CAppBranding.h"

#include "CRegistrationDialog.h"

// CRegistrationDialog dialog

IMPLEMENT_DYNAMIC(CRegistrationDialog, CDialog)
CRegistrationDialog::CRegistrationDialog(CWnd* pParent)
	: CDialog(IDD_REGISTRATION_DIALOG, pParent)
{
    m_userKeyIcon       = AfxGetApp()->LoadIcon(IDI_USER_ICON);
    m_systemKeyIcon     = AfxGetApp()->LoadIcon(IDI_SYSTEM_ICON);
    m_noKeyIcon         = AfxGetApp()->LoadOEMIcon(OIC_NOTE);
}

CRegistrationDialog::~CRegistrationDialog()
{
}

void CRegistrationDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	DDX_Control(pDX, IDC_SERIAL_NUMBER, m_SerialNumberCtrl);
	DDX_Control(pDX, IDC_KEY_CODE, m_KeyCodeCtrl);
	DDX_Control(pDX, IDC_LICENSE_TYPE, m_LicenseTypeCtrl);
	DDX_Control(pDX, IDC_KEY_TYPE_ICON, m_KeyTypeIconCtrl);
	DDX_Control(pDX, IDC_KEY_TYPE_DESC, m_KeyTypeDescCtrl);
}


BEGIN_MESSAGE_MAP(CRegistrationDialog, CDialog)
	ON_BN_CLICKED(ID_SET_KEY_CODE, OnBnClickedSetKeyCode)
	ON_BN_CLICKED(ID_PURCHASE_KEY_CODE, OnBnClickedPurchaseKeyCode)
END_MESSAGE_MAP()

/**************************************************************
 * Dialog Behavior (Message handlers, implementation, 
 * and virtual function overrides)
 */

BOOL CRegistrationDialog::OnInitDialog()
{
	CDialog::OnInitDialog();

	Registration *reg = Registration::GetRegistrationObject();

	m_SerialNumberCtrl.SetWindowText(reg->createSn());
	
	CAppBranding *brand = 
		CAppBranding::GetBrandingObject();

	CString appName = brand->GetApplicationName();
	CString tmp;

	tmp.FormatMessage(IDS_REGISTER, appName);
	SetWindowText(tmp);

    UpdateLicenseInfo();

	return TRUE;
}

void CRegistrationDialog::UpdateLicenseInfo()
{
    CString keyDesc;

	Registration *reg = Registration::GetRegistrationObject();

	CString key = reg->getLicenseKey();
    m_KeyCodeCtrl.SetWindowText(key);

    

	if (!key.GetLength())
    {
		key = _T("<no key set>");
        m_KeyTypeIconCtrl.SetIcon(m_noKeyIcon);

        keyDesc.LoadString(IDS_NO_KEY);
    }
    else if (reg->IsCurrentKeySystemScoped())
    {
        m_KeyTypeIconCtrl.SetIcon(m_systemKeyIcon);
        keyDesc.LoadString(IDS_SYSTEM_KEY);
    }
    else
    {        
        m_KeyTypeIconCtrl.SetIcon(m_userKeyIcon);

        if (reg->HaveSystemScopeRights())
            keyDesc.LoadString(IDS_USER_KEY_WITH_RIGHTS);
        else
            keyDesc.LoadString(IDS_USER_KEY);
    }

    m_KeyTypeDescCtrl.SetWindowText(keyDesc);

	m_LicenseTypeCtrl.SetWindowText(reg->GetLicenseDescription());		
}

/**************************************************************
 * Button Command Handlers
 */

void CRegistrationDialog::OnBnClickedSetKeyCode()
{
	CNewKeyDialog dlg;

	Registration *reg = Registration::GetRegistrationObject();

	CString key = reg->getLicenseKey();

	if (!key.GetLength())
		key = "<no key set>";

	dlg.SetNewKey(key);

	if (dlg.DoModal() == IDOK)
	{
		reg->setKey(dlg.GetNewKey());
		reg->saveRegistrationSettings();
		
		m_KeyCodeCtrl.SetWindowText(dlg.GetNewKey());
		UpdateLicenseInfo();
	}
}

void CRegistrationDialog::OnBnClickedPurchaseKeyCode()
{
	Registration *reg = Registration::GetRegistrationObject();

	CAppBranding *brand = 
		CAppBranding::GetBrandingObject();

	CString url = brand->GetPurchaseURL(reg->createSn());


	::ShellExecute(NULL,
		           NULL,
				   LPCTSTR(url),
				   NULL,
				   _T("C:\\"),
				   SW_SHOWNORMAL);
}
