/****************************************************************
 * Registration.h
 *
 * Copyright 2001-2004, Icegiant Software
 * Portions Copyright 2003-2004, East Coast Toolworks, LLC
 */

#ifndef __REGISTRATION_H
#define __REGISTRATION_H

#include "CRegistryBindings.h"

#include "../util/base-types.h"

/* __forceinline is a heavy-duty Microsoft-C++ specific version
 * of the C++ keyword 'inline'.  Like its name implies, it forces
 * inline expansion of the specified function, overriding the
 * compiler heuristics that would be consulted for the 'inline'
 * keyword.
 *
 * The reason this is done is entirely for security reasons. By
 * duplicating this code in multiple places in the executable,
 * life is made significantly more difficult for folks that would
 * want to crack the software's protection mechanism.
 */
 
#define INLINE __forceinline


class Registration
{
public:
	Registration(const CString company,
                 const CString companyAcronym,
                 const CString companyEmail,
                 const CString productName,
                 const CString snPrefix);

	virtual ~Registration()	{}

    INLINE static void PoisonPill();
	INLINE static Registration *GetRegistrationObject();

	void setUserInfo(const CString& name,
                     const CString& companyName,
					 const CString& emailAddress);

    INLINE void setKey(const CString& key);

	INLINE CString createSn();
	INLINE CString createKey(const CString& license,const CString& sn);

	void saveRegistrationSettings();
	void loadRegistrationSettings();

	INLINE bool isRegisteredVersion();
	INLINE bool isValidBasicVersion();

	INLINE void tellMeTheTruth(HWND hwnd,int id);
	INLINE bool isBasicVersion();
	INLINE bool isProfessionalVersion();
	INLINE bool isUniversalVersion();
	INLINE bool isEvaluationVersion();

	CString getProductName();
	CString getCompanyAcronym();
    CString getCompanyName();
    CString getCompanyEmail();
	CString getSn();
	CString getUserCompanyName();
    CString getUserEmailAddress();
	CString getUserName();

    INLINE CString getLicenseKey();

	enum LicenseStatus
	{
		REGISTERED      = 0xD940E23D,
		TRIAL           = 0x67404d65,
		EXPIRED         = 0xA960890F
	};

	virtual LicenseStatus GetLicenseStatus();
	virtual CString GetLicenseDescription();

    bool IsCurrentKeySystemScoped();
    bool HaveSystemScopeRights();

private:
    static Registration *_globalRegistrationObject;

    enum KeySource
    {
        /* These are random numbers to make it slightly more difficult 
         * to reverse engineer. */
        NO_KEY          = 0xAF6FBD49,
        SYS_UNI_KEY     = 0xF3E8491A,
        SYS_APP_KEY     = 0xA8EC98E2,
        USR_UNI_KEY     = 0x0E909A11,
        USR_APP_KEY     = 0xE6A451EB
    };

    INLINE KeySource currentLicenseKey(CString &key);
    INLINE KeySource currentLicenseKeyWithValidation(CString &key);

	INLINE bool isValidKey(const CString& license,const CString& sn,const CString& key);//,int& reallyValid);
	INLINE bool isValidUniKey(const CString& license,const CString& sn,const CString& key);//,int& reallyValid);

    INLINE CString licenseString(const CString& license,const CString& sn);
	INLINE CString formatSn(const CString& dist,const CString& prod,int num);

	INLINE CString getKeyLeadingUniversalString();
	INLINE CString getKeyLeadingString();

	INLINE CString extractKeyLicenseField(CString& key);
	INLINE CString extractKeyKeyField(CString& key);

	INLINE bool testKey(CString registryKey);
	INLINE bool checkRegistrationVersion(CString type);

    CString loadOSProductID();

    bool _systemScopedKeys;

	CRegistryBindings m_systemAppBinding;
	CRegistryBindings m_systemUniBinding;
	CRegistryBindings m_userAppBinding;
	CRegistryBindings m_userUniBinding;

	CString _systemAppKey;
	CString _systemUniKey;
	CString _userAppKey;
	CString _userUniKey;

	CString m_userCompanyName;
	CString m_userEmailAddress;
	CString m_userName;

	CString m_sn;

	CString m_company;
	CString m_companyAcronym;
	CString m_companyEmail;

	CString m_snPrefix;
	CString m_productName;

	bool m_keyChanged;
};

extern CString g_sn;

/****************************************************************
 * Implementations of the inline functions follow
 */

#include "registrationUtil.h"

bool regCompare(const CString& str1,const CString& str2,bool noCase=TRUE,bool any=FALSE);

void Registration::PoisonPill()
{
    Registration::_globalRegistrationObject = NULL;
}

Registration *Registration::GetRegistrationObject()
{
	return Registration::_globalRegistrationObject;
}

void Registration::setKey(const CString& key)
{
	if(key.Find(_T("universal"))>=0)
    {
        if (_systemScopedKeys)
            _systemUniKey = key;
        else
            _userUniKey = key;		
    }
	else
    {
        if (_systemScopedKeys)
            _systemAppKey = key;
        else
            _userAppKey = key;        
    }

    saveRegistrationSettings();
}

CString Registration::createSn()
{
	CString prefix=m_snPrefix;

	CString vol=_T("c:\\"); 

	CString volInfo=getVolumeSerialNumber(vol);

	CString decodedPostfix=volInfo+_T(",")+loadOSProductID();

	CString postfix=getCrc(decodedPostfix);

	CString sn=prefix+_T("-")+postfix;

	g_sn=sn;

	return sn;
}

CString Registration::createKey(const CString& license,const CString& sn) 
{
	CString decodedKey=licenseString(license,sn);

	CString key=getCrc(decodedKey);

	return license+_T("\\")+key;
}

bool Registration::isRegisteredVersion()
{
	CString registryKey;
    KeySource source = currentLicenseKeyWithValidation(registryKey);

    if ((source == NO_KEY) || (registryKey.GetLength() == 0))
        return FALSE;

    if(testKey(registryKey))
		return TRUE;

    return FALSE;
}

bool Registration::isValidBasicVersion()
{
	if(!isBasicVersion())
		return FALSE;

	CString registryKey;

    currentLicenseKey(registryKey);

    return testKey(registryKey);
}

void Registration::tellMeTheTruth(HWND hwnd,int id)
{ 
    PostMessage(hwnd,id,isRegisteredVersion(),0); 
}

bool Registration::isBasicVersion()
{ 
    return checkRegistrationVersion(_T("basic")); 
}

bool Registration::isProfessionalVersion()
{ 
    return checkRegistrationVersion(_T("professional"));
}

bool Registration::isUniversalVersion()
{ 
    return checkRegistrationVersion(_T("universal")); 
}

bool Registration::isEvaluationVersion()
{ 
    return checkRegistrationVersion(_T("evaluation")); 
}

CString Registration::getLicenseKey()
{ 
    CString key; 
    
    currentLicenseKey(key); 
    
    return key; 
}

Registration::KeySource Registration::currentLicenseKey(CString &key)
{
    if (testKey(_systemUniKey))
    {
        key = _systemUniKey;
        return SYS_UNI_KEY;
    }
    else if (testKey(_userUniKey))
    {
        key = _userUniKey;
        return USR_UNI_KEY;
    }
    else if (testKey(_systemAppKey))
    {
        key = _systemAppKey;
        return SYS_APP_KEY;
    }
    else if (testKey(_userAppKey))
    {
        key = _userAppKey;
        return USR_APP_KEY;
    }
    else
    {
        key = "";
        return NO_KEY;
    }
}

Registration::KeySource Registration::currentLicenseKeyWithValidation(CString &key)
{
    CString key1, key2;
    KeySource source;

    /* This is currentLicenseKey with a bunch of validity checks. It
     * extracts/tests the key twice and checks the results to ensure
     * they match. It also runs a series of consistancy checks to ensure
     * that the key matches what we expect it to be, given the keysource.
     */
    source = currentLicenseKey(key1);

    if (source == NO_KEY)
        return NO_KEY;

    if (key1.GetLength() == 0)
        return NO_KEY;


    if ((source == SYS_UNI_KEY) || (source == USR_UNI_KEY))
    {
        if (!(regCompare(extractKeyLicenseField(key1), "universal")))
            return NO_KEY;
    }

    key = key1;

    if (source != currentLicenseKey(key2))
        return NO_KEY;

    if (key1 != key2)
        return NO_KEY;

    return source;
}

bool Registration::isValidKey(const CString& license,const CString& sn,const CString& key)
{
	CString testKey=licenseString(license,sn);

	CString crc=getCrc(testKey);

	int index=key.Find(_T('\\'));
	CString newKey=key;

	if(index>=0)
		newKey=key.Right(key.GetLength()-(index+1));

	if(regCompare(crc,newKey))
          return crc==newKey;

        return false;
}


bool Registration::isValidUniKey(const CString& license,const CString& sn,const CString& key)
{
	CString testKey=licenseString(license,sn);

	if(!testKey.GetLength())
		return FALSE;

	CString crc=getCrc(testKey);

	int index=key.Find(_T('\\'));
	CString newKey=key;

	if(index>=0)
		newKey=key.Right(key.GetLength()-(index+1));

	if(regCompare(crc,newKey))
          return crc==newKey;

        return false;
}

CString Registration::licenseString(const CString& license, const CString& sn)
{
	CString testKey;
	
	if(regCompare(_T("universal"),license))
	{
		int index=sn.ReverseFind(_T('-'));
		CString nsn=sn.Right(sn.GetLength()-index-1);
		testKey=getKeyLeadingUniversalString()+_T("\\")+license+_T("\\")+nsn;
	}
	else
		testKey=getKeyLeadingString()+_T("\\")+license+_T("\\")+sn;

	return testKey;
}

CString Registration::formatSn(const CString& dist,const CString& prod,int num)
{
	CString hexStr;

	hexStr.Format(_T("%04x"),num);
	CString newSn=dist+_T("-")+prod+_T("-")+hexStr;

	return newSn;
}

CString Registration::getKeyLeadingUniversalString()
{
	CString str;

	CString company=getCompanyName();

	str.Format(_T("icegiant\\%s\\universal"), company);

	return str;
}

CString Registration::getKeyLeadingString()
{
	CString str;

	CString company=getCompanyName();
	CString productName=getProductName();

	str.Format( _T("icegiant\\%s\\%s"),company,productName);

	return str;
}


CString Registration::extractKeyLicenseField(CString& key)
{
	int index=key.Find(_T("\\"),0);

	if(index>0)
		return key.Left(index);

	return CString();
}

CString Registration::extractKeyKeyField(CString& key)
{
	int index=key.Find(_T("\\"),0);

	if(index>0)
		return key.Right(key.GetLength()-(index+1));

	return CString();
}


bool Registration::testKey(CString registryKey)
{
	CString license	= extractKeyLicenseField(registryKey);
	CString key		= extractKeyKeyField(registryKey);

	CString sn		= createSn();

	return isValidKey(license,sn,key);
}

bool Registration::checkRegistrationVersion(CString type)
{
	if(!isRegisteredVersion())
		return FALSE;

    CString key;
    CString licenseType;

    currentLicenseKeyWithValidation(key);

	licenseType = extractKeyLicenseField(key);

    /* The actual test */
	if(regCompare(licenseType,type))
	    return TRUE;

    return FALSE;
}

#endif // __REGISTRATION_H
