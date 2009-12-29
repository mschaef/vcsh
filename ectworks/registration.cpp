/****************************************************************
 * Registration.cpp
 *
 * Copyright 2001-2004, Icegiant Software
 * Portions Copyright 2003-2004, East Coast Toolworks, LLC
 */

#include "stdafx.h"

#include "../util/base-assert.h"

#include "util-resource.h"

#include "registration.h"

CString g_sn;

/****************************************************************
 * Registry Accessors
 */

bool regCompare(const CString& str1, const CString& str2, bool noCase, bool any)
{
	if(any)
	{
		if(noCase)
		{
			CString tempStr1=str1;
			CString tempStr2=str2;

			tempStr1.MakeLower();
			tempStr2.MakeLower();

			return (tempStr1.Find(tempStr2)>=0);
		}

		return (str1.Find(str2,0)>=0);
	}

	if(noCase)
		return (str1.CompareNoCase(str2)==0);

	return (str1.Compare(str2)==0);
}

/****************************************************************
 * The implementation of the non-inline parts of Registration
 */

Registration *Registration::_globalRegistrationObject = NULL;

Registration::Registration(CString company,
                           CString companyAcronym,
                           CString companyEmail,
                           CString productName,
                           CString snPrefix)
	: m_systemAppBinding(_T("registration"), CRegistryBindings::LOCAL_SYSTEM, company, productName),
	  m_systemUniBinding(_T("registration"), CRegistryBindings::LOCAL_SYSTEM, company, "universal"),
	  m_userAppBinding  (_T("registration"), CRegistryBindings::CURRENT_USER, company, productName),
	  m_userUniBinding  (_T("registration"), CRegistryBindings::CURRENT_USER, company, "universal")
{
    // Can't have more than one...
    assert(_globalRegistrationObject == NULL);

    if (_globalRegistrationObject == NULL)
	    Registration::_globalRegistrationObject = this;
    else
    {
        // If we get here without throwing an assertation that means we're
        // in production code and something's fishy.
        Registration::PoisonPill();
        return;
    }

	m_snPrefix		= snPrefix;
	m_company		= company;
	m_companyAcronym= companyAcronym;
	m_companyEmail	= companyEmail;
	m_productName	= productName;

	m_systemAppBinding.Add(new CStringBinding(_systemAppKey		, _T("key"), ""));

	m_systemUniBinding.Add(new CStringBinding(_systemUniKey		, _T("key"), ""));

	m_userAppBinding  .Add(new CStringBinding(_userAppKey		, _T("key")		, ""));
	m_userAppBinding  .Add(new CStringBinding(m_sn				, _T("sn")		, ""));
	m_userAppBinding  .Add(new CStringBinding(m_userName		, _T("name")	, ""));
	m_userAppBinding  .Add(new CStringBinding(m_userCompanyName , _T("company")	, ""));
	m_userAppBinding  .Add(new CStringBinding(m_userCompanyName	, _T("email")	, ""));

	m_userUniBinding  .Add(new CStringBinding(_userUniKey		, _T("key"), ""));

	_systemScopedKeys =    !m_systemAppBinding.IsReadOnly()
	                    && !m_systemUniBinding.IsReadOnly();
}

void Registration::setUserInfo(const CString& name,
							   const CString& companyName,
							   const CString& emailAddress)
{
    m_userName			= name;
    m_userCompanyName	= companyName;
    m_userEmailAddress	= emailAddress;
}

void Registration::saveRegistrationSettings()
{
	ASSERT(m_systemAppBinding.IsReadOnly() == m_systemUniBinding.IsReadOnly());

	if (!m_systemAppBinding.IsReadOnly() && !m_systemUniBinding.IsReadOnly())
	{
		m_systemAppBinding.Write();
		m_systemUniBinding.Write();
	}

	m_userAppBinding.Write();
	m_userUniBinding.Write();
}

void Registration::loadRegistrationSettings()
{
	m_systemAppBinding.ReadFromRegistry();
	m_systemUniBinding.ReadFromRegistry();
	m_userAppBinding  .ReadFromRegistry();
	m_userUniBinding  .ReadFromRegistry();
}

CString Registration::getProductName()
{
    return m_productName;     
}

CString Registration::getCompanyAcronym()
{
    return m_companyAcronym;  
}

CString Registration::getCompanyName()
{ 
    return m_company;       
}

CString Registration::getCompanyEmail()		
{
    return m_companyEmail;    
}

CString Registration::getSn()					
{ 
    return m_sn;				
}

CString Registration::getUserCompanyName()		
{ 
    return m_userCompanyName; 
}

CString Registration::getUserEmailAddress()	
{ 
    return m_userEmailAddress;
}

CString Registration::getUserName()			
{ 
    return m_userName;		
}

Registration::LicenseStatus Registration::GetLicenseStatus()
{
	if (isRegisteredVersion())
		return Registration::REGISTERED;
    else
		return Registration::EXPIRED;
}

CString Registration::GetLicenseDescription()
{	
	CString str;

	switch (GetLicenseStatus())
	{
	case Registration::REGISTERED:
		str.FormatMessage(IDS_REGISTERED_VERSION);
		break;

	case Registration::EXPIRED:
		str.FormatMessage(IDS_UNREGISTERED_VERSION);
		break;

    default:
        PoisonPill();
        str.FormatMessage(IDS_UNREGISTERED_VERSION);
    }

	return str;
}

bool Registration::IsCurrentKeySystemScoped()
{
    CString key;
    KeySource source = currentLicenseKeyWithValidation(key);

    return ((source == SYS_UNI_KEY) || (source == SYS_APP_KEY));
}

bool Registration::HaveSystemScopeRights()
{
    return _systemScopedKeys;
}

CString Registration::loadOSProductID()
{
    LONG rc;
    HKEY productIDKey;
	DWORD type;
	DWORD size;
	CString buf;


	rc = RegOpenKeyEx(HKEY_LOCAL_MACHINE,
                      _T("SOFTWARE\\Microsoft\\Windows\\CurrentVersion"),
                      0,
                      KEY_READ, &productIDKey);	

    if (rc == ERROR_SUCCESS)
    {
        
        rc = RegQueryValueEx(productIDKey, _T("ProductID"), NULL, &type, NULL, &size);

		if (rc == ERROR_SUCCESS)
        {
            ASSERT(type == REG_SZ);

            rc = RegQueryValueEx(productIDKey,
                                 _T("ProductID"),
                                 NULL, 
                                 &type, (LPBYTE)buf.GetBuffer(size/sizeof(TCHAR)), &size);

            buf.ReleaseBuffer();
        }

        rc = RegCloseKey(productIDKey);
    }
    
    ASSERT(rc == ERROR_SUCCESS);

    return buf;
}

