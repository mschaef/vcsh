// CAboutBox.cpp : implementation file
//

#include "stdafx.h"

#include <util-resource.h>

#include "TimeoutRegistration.h"
#include "CRegistrationDialog.h"

#include "CAppBranding.h"

#include "CAboutBox.h"

// CAboutBox dialog

IMPLEMENT_DYNAMIC(CAboutBox, CDialog)
CAboutBox::CAboutBox(CWnd* pParent)
   : CDialog(IDD_ABOUTBOX, pParent)
{
	CAppBranding *brand = 
		CAppBranding::GetBrandingObject();

	m_LargeBanner.LoadBitmap(brand->GetLargeBannerImage());
	m_SmallBanner.LoadBitmap(brand->GetSmallBannerImage());
}

CAboutBox::~CAboutBox()
{
	m_LargeBanner.DeleteObject();
	m_SmallBanner.DeleteObject();
}

void CAboutBox::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	DDX_Control(pDX, IDC_BANNER, m_BannerCtrl);
	DDX_Control(pDX, IDC_VERSION_DESC, m_VersionDescCtrl);
	DDX_Control(pDX, IDC_LICENSE_DESC, m_LicenseDescCtrl);
	DDX_Control(pDX, IDC_LICENSE_TEXT, m_LicenseTextCtrl);
	DDX_Control(pDX, ID_REGISTER_VCALC, m_RegisterCtrl);
	
}


BEGIN_MESSAGE_MAP(CAboutBox, CDialog)
	ON_BN_CLICKED(ID_REGISTER_VCALC, OnBnClickedRegisterVcalc)
	ON_BN_CLICKED(ID_EMAIL, OnBnClickedEmail)
END_MESSAGE_MAP()


// CAboutBox message handlers

BOOL CAboutBox::OnInitDialog()
{
	CDialog::OnInitDialog();

	Registration *reg = Registration::GetRegistrationObject();

	/* Set the license description */
	CString licenseDesc = reg->GetLicenseDescription();

	
	m_LicenseDescCtrl.SetWindowText(licenseDesc);

	CAppBranding *brand = 
		CAppBranding::GetBrandingObject();

	m_VersionDescCtrl.SetWindowText(brand->GetApplicationVersion());

	m_LicenseTextCtrl.SetWindowText(brand->GetLicenseText());

	m_BannerCtrl.AddBitmap(m_LargeBanner);
	m_BannerCtrl.AddBitmap(m_SmallBanner);

	CString appName = brand->GetApplicationName();
	CString tmp;

	tmp.FormatMessage(IDS_ABOUT, appName);
	SetWindowText(tmp);

	tmp.FormatMessage(IDS_REGISTER, appName);
	m_RegisterCtrl.SetWindowText(tmp);


	return TRUE;  
}


void CAboutBox::OnBnClickedRegisterVcalc()
{
	CRegistrationDialog dlg(this);

	dlg.DoModal();
}

void CAboutBox::OnBnClickedEmail()
{
	CAppBranding *brand = 
		CAppBranding::GetBrandingObject();

	Registration *reg = Registration::GetRegistrationObject();

	CString sn(reg->getSn());

	CString url = brand->GetContactURL(sn);
	
	::ShellExecute(NULL,
		           NULL,
				   LPCTSTR(url),
				   NULL,
				   _T("C:\\"),
				   SW_SHOWNORMAL);
}


