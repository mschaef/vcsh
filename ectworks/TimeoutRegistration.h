#pragma once

#include "../util/base-types.h"

#include "registration.h"

class TimeoutRegistration :
	public Registration
{
public:
	TimeoutRegistration(const CString company,
                        const CString companyAcronym,
                        const CString companyEmail,
                        const CString productName,
                        const CString snPrefix);

	u32 DaysSinceInstallation();
	i32 DaysRemainingOnTrial();

	bool TrialLicenseExpired();	

	virtual LicenseStatus GetLicenseStatus();
	virtual CString GetLicenseDescription();

};
