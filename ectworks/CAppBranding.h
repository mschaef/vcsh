/* CAppBranding.h
 * December 19th, 2003
 * 
 * This class encapsulates a bunch of information relating to
 * the branding of the applicaton
 */

#ifndef __CAPPBRANDING_H
#define __CAPPBRANDING_H

#include "afxwin.h"

class CAppBranding
{
private:
	static CAppBranding *_globalAppBrand;

public:
	CAppBranding();

	static CAppBranding *GetBrandingObject();

	virtual UINT GetSmallBannerImage() = 0;
	virtual UINT GetLargeBannerImage() = 0;

	virtual CString GetPurchaseURL(CString serialNumber) = 0;
	virtual CString GetContactURL(CString serialNumber) = 0;

	virtual CString GetLicenseText() = 0;
	virtual CString GetApplicationName();
	virtual CString GetApplicationVersion() = 0;
};

#endif /* __CAPPBRANDING_H */