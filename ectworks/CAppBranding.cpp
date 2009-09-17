/* CAppBranding.cpp
 * December 19th, 2003
 * 
 * This class encapsulates a bunch of information relating to
 * the branding of the applicaton
 */

#include <scan-assert.h>

#include "CAppBranding.h"

CAppBranding *CAppBranding::_globalAppBrand = NULL;

CAppBranding::CAppBranding()
{
	/* There can't be more than one instance of CAppBranding. */
	assert(CAppBranding::_globalAppBrand == NULL);

	CAppBranding::_globalAppBrand = this;
}

CAppBranding *CAppBranding::GetBrandingObject()
{
	/* Your program needs to derive from CAppBranding and
	 * create an instance thereof to use certain services
	 * provided by the ECTworks utilities library. */
	assert(CAppBranding::_globalAppBrand);

	return CAppBranding::_globalAppBrand;
}

CString CAppBranding::GetApplicationName()
{
	CString appTitle;

	appTitle.LoadString(AFX_IDS_APP_TITLE);

	return appTitle;
}
