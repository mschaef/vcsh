/* CIntroDialog.cpp 
 * Copyright 2003-2004, East Coast Toolworks, LLC
 * 
 * This is the implementation code for the introduction dialog 
 * box: the dialog that is displayed to the user immediately upon startup.
 */

#include "stdafx.h"
#include <scan-assert.h>
#include <util-resource.h>

#include "TimeoutRegistration.h"

#include "CRegistrationDialog.h"

#include "CAppBranding.h"

#include "CIntroDialog.h"

// CIntroDialog dialog

IMPLEMENT_DYNAMIC(CIntroDialog, CDialog)
CIntroDialog::CIntroDialog(CWnd* pParent)
	: CDialog(IDD_INTRO_DIALOG, pParent)
{
    m_DaysInstalled = -1;
	m_StartupTimeout = 0;

	NONCLIENTMETRICS ncm;
	
	ncm.cbSize = sizeof(NONCLIENTMETRICS);

	::SystemParametersInfo(SPI_GETNONCLIENTMETRICS,
		                   ncm.cbSize,
						   &ncm,
						   FALSE);

	ncm.lfMessageFont.lfWeight = FW_BOLD;

	m_IntroTextFont.CreateFontIndirect(&ncm.lfMessageFont);

	CAppBranding *brand = 
		CAppBranding::GetBrandingObject();

	m_LargeBanner.LoadBitmap(brand->GetLargeBannerImage());
	m_SmallBanner.LoadBitmap(brand->GetSmallBannerImage());
}

CIntroDialog::~CIntroDialog()
{
	m_IntroTextFont.DeleteObject();

	m_LargeBanner.DeleteObject();
	m_SmallBanner.DeleteObject();
}

void CIntroDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	DDX_Control(pDX, ID_AGREE, m_AgreeCtrl);
	DDX_Control(pDX, IDC_STATUS, m_StatusCtrl);
	DDX_Control(pDX, IDC_INTRO_TEXT_STATIC, m_IntroTextCtrl);
	DDX_Control(pDX, IDC_LICENSE, m_LicenseCtrl);
	DDX_Control(pDX, IDC_BANNER, m_BannerCtrl);
	DDX_Control(pDX, IDC_LICENSE_INFO, m_LicenseInfoCtrl);
	DDX_Control(pDX, ID_ENTER_REGISTRATION_KEY, m_EnterRegCtrl);
	DDX_Control(pDX, ID_QUIT, m_QuitCtrl);
}


BEGIN_MESSAGE_MAP(CIntroDialog, CDialog)
	ON_BN_CLICKED(ID_ENTER_REGISTRATION_KEY, OnBnClickedEnterRegistrationKey)
	ON_BN_CLICKED(ID_AGREE, OnBnClickedAgree)
	ON_BN_CLICKED(ID_QUIT, OnBnClickedQuit)
	ON_WM_TIMER()
END_MESSAGE_MAP()

/**************************************************************
 * Dialog Behavior (Message handlers, implementation, 
 * and virtual function overrides)
 */

void CIntroDialog::UpdateStatusText()
{
	CString statusText;

	if (m_DaysInstalled != -1)
	{
		Registration *registration = Registration::GetRegistrationObject();

		statusText = registration->GetLicenseDescription();
	}
	
	if (m_StartupTimeout)
		statusText.FormatMessage(IDS_INTRO_TIME_TO_GO, m_StartupTimeout);

	m_StatusCtrl.SetWindowText(statusText);
}

BOOL CIntroDialog::OnInitDialog()
{
	CDialog::OnInitDialog();

	// Bold the introduction text
	m_IntroTextCtrl.SetFont(&m_IntroTextFont);

	// We get a lot of the information we display from the global
	// registration object
	Registration *registration = Registration::GetRegistrationObject();

        UNREFERENCED(registration);

    // No more startup timeout... it's a little too annoying.
    m_StartupTimeout = 0;

	// In the event we're forcing the user to wait, we disable the "I Agree" 
	// button and start a timer, so that we can count down the specified
	// number of seconds.
	if (m_StartupTimeout)
	{
		SetTimer(STARTUP_TIMER, 1000, NULL);
		m_AgreeCtrl.EnableWindow(FALSE);
	}
	else
		m_AgreeCtrl.EnableWindow(TRUE);

	UpdateStatusText();


	CAppBranding *brand = 
		CAppBranding::GetBrandingObject();

	CString appName = brand->GetApplicationName();
	CString tmp;

	tmp.FormatMessage(IDS_WELCOME, appName);
	SetWindowText(tmp);

	tmp.FormatMessage(IDS_INTRO, appName);
	m_IntroTextCtrl.SetWindowText(tmp);

	tmp.FormatMessage(IDS_LICENSE_INFO, appName);
	m_LicenseInfoCtrl.SetWindowText(tmp);

	tmp.FormatMessage(IDS_REGISTER, appName);
	m_EnterRegCtrl.SetWindowText(tmp);

	tmp.FormatMessage(IDS_QUIT, appName);
	m_QuitCtrl.SetWindowText(tmp);

	m_LicenseCtrl.SetWindowText(brand->GetLicenseText());

	m_BannerCtrl.AddBitmap(m_LargeBanner);
	m_BannerCtrl.AddBitmap(m_SmallBanner);

	return TRUE;
}


void CIntroDialog::OnTimer(UINT nIDEvent)
{
	assert(nIDEvent == STARTUP_TIMER);

	m_StartupTimeout--;

	if (!m_StartupTimeout)	
	{
        KillTimer(STARTUP_TIMER);
		m_AgreeCtrl.EnableWindow(TRUE);
		GotoDlgCtrl(&m_AgreeCtrl);	
	}

	UpdateStatusText();

	CDialog::OnTimer(nIDEvent);
}

void CIntroDialog::OnCancel()
{
	EndDialog(ID_QUIT);
}

void CIntroDialog::OnOK()
{	
}

/**************************************************************
 * Button Command Handlers
 */


void CIntroDialog::OnBnClickedEnterRegistrationKey()
{
	CRegistrationDialog dlg(this);

	dlg.DoModal();
}

void CIntroDialog::OnBnClickedAgree()
{
	EndDialog(ID_AGREE);
}

void CIntroDialog::OnBnClickedQuit()
{
	EndDialog(ID_QUIT);
}

/**************************************************************
 * External interface
 **************************************************************/

BOOL CIntroDialog::UserAgreesToLicense()
{
	INT_PTR returnCode = DoModal();

	return (returnCode != ID_QUIT);
}
