#include "StdAfx.h"
#include <util-resource.h>
#include <scan-assert.h>

#include "TimeoutRegistration.h"

#define TRIAL_DAYS_ALLOWED (21)

TimeoutRegistration::TimeoutRegistration(const CString company,
                                         const CString companyAcronym,
                                         const CString companyEmail,
                                         const CString productName,
                                         const CString snPrefix)
    : Registration(company,
                   companyAcronym,
                   companyEmail,
                   productName,
                   snPrefix)
{
}


u32 TimeoutRegistration::DaysSinceInstallation()
{
	char szBuffer[1024];
	WIN32_FILE_ATTRIBUTE_DATA fileInfo;
	CTime now = CTime::GetCurrentTime();
	
	GetModuleFileName(AfxGetInstanceHandle(), szBuffer, sizeof(szBuffer));
	
	// !!!!! Cross check file modification time with registry key
	GetFileAttributesEx(szBuffer,
		                GetFileExInfoStandard,
						&fileInfo);
	CTime modificationTime(fileInfo.ftLastWriteTime);

    CTimeSpan delta = now - modificationTime;

    return (u32)(delta.GetDays());
}


i32 TimeoutRegistration::DaysRemainingOnTrial()
{
	return TRIAL_DAYS_ALLOWED - (i32)DaysSinceInstallation();
}

bool TimeoutRegistration::TrialLicenseExpired()
{
	return DaysRemainingOnTrial() < 0;
}

Registration::LicenseStatus TimeoutRegistration::GetLicenseStatus()
{
    LicenseStatus status = Registration::GetLicenseStatus();

    if (status == Registration::EXPIRED)
    {
        if (TrialLicenseExpired())
            return status;
        else
            return Registration::TRIAL;
    }
    else
        return Registration::REGISTERED;
}

CString TimeoutRegistration::GetLicenseDescription()
{	
    LicenseStatus status = GetLicenseStatus();

    if (status == Registration::TRIAL)
    {
        CString str;

		i32 daysLeft = DaysRemainingOnTrial();

		str.FormatMessage(IDS_TRIAL_VERSION, daysLeft);

	    return str;
    }
    else
        return Registration::GetLicenseDescription();
}
