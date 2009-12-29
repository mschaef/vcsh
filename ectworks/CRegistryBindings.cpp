/*
 * CRegistryBindings.h
 *
 * Copyright (C) 2003 by Mike Schaeffer (mschaef@mschaef.com)
 * Portions Copyright (C) 1998 by Joerg Dentler (dentler@tpnet.de)
 * All rights reserved
 */
 
/* This work is derived from CRegBinding, by Joerg Dentler. The
 * following message is his copyright.  I like it too, so I'm
 * releasing my modifications under the same terms. If you like
 * this class, have questions, or have used it in an interesting
 * product, please drop me a line at mschaef@mschaef.com. I'm
 * curious to see where this goes.
 *
 ///////////////////////////////////////////////////////////////////
 * Permission is granted to anyone to use this software for any
 * purpose and to redistribute it in any way, subject to the following 
 * restrictions:
 *
 * 1. The author is not responsible for the consequences of use of
 *    this software, no matter how awful, even if they arise
 *    from defects in it.
 *
 * 2. The origin of this software must not be misrepresented, either
 *    by explicit claim or by omission.
 *
 * 3. Altered versions must be plainly marked as such, and must not
 *    be misrepresented (by explicit claim or omission) as being
 *    the original software.
 *
 * 4. This notice must not be removed or altered.
 ///////////////////////////////////////////////////////////////////
 */

// !!! Need to report whether or not a binding is using default values...



#include <afxwin.h>

#include "../util/base-types.h"

#include "CRegistryBindings.h"

/**********************************************************************
 * CBinding Implementations
 */
 
/*** CBinding ***/

void CBinding::Write(IRegistrySection &section)
{
	if (_mode != BINDING_HIDDEN)
		WriteToRegistry(section);
}

void CBinding::Read(IRegistrySection &section)
{
	ReadFromRegistry(section);
}

CBinding::CBinding(CString name, BindingMode mode) :
  _mode(mode), 
  _name(name)
{
}

BOOL CBinding::IsWindowBinding(HWND hWnd)
{
  UNREFERENCED(hWnd);

  return FALSE;
}

BOOL CBinding::SameNameAs(const CBinding *b)
{
	return _name == b->_name;
}

/*** CIntBinding ***/

CIntBinding::CIntBinding(int &value,
                         const char *name,
                         const int def,
						 const CBinding::BindingMode mode) :
  CBinding(name, mode),
  _value(value)
{
	_default = def;
}


void CIntBinding::WriteToRegistry(IRegistrySection &section)
{
	section.WriteInt(_name, _value);	
}

void CIntBinding::ReadFromRegistry(IRegistrySection &section)
{
	_value = section.ReadInt(_name, _default);
}

/*** CDWORDBinding ***/

CDWORDBinding::CDWORDBinding(DWORD &value,
                             const char *name,
                             const DWORD def,
                             const CBinding::BindingMode mode) :
	CBinding(name, mode),
	_value(value)
{
  UNREFERENCED(def);
}


void CDWORDBinding::WriteToRegistry(IRegistrySection &section)
{
	section.WriteInt(_name, _value);	
}

void CDWORDBinding::ReadFromRegistry(IRegistrySection &section)
{
	_value = section.ReadInt(_name, _default);
}

/*** CStringBinding ***/

CStringBinding::CStringBinding(CString &value,
                               const char *name,
                               const char *def,
							   const CBinding::BindingMode mode) :
	CBinding(name, mode),
	_value(value)
{
	_default = def;
}

void CStringBinding::WriteToRegistry(IRegistrySection &section)
{
	section.WriteString(_name, _value);
}

void CStringBinding::ReadFromRegistry(IRegistrySection &section)
{
	_value = section.ReadString(_name, _default);	
}

/*** CByteArrayBinding ***/

CByteArrayBinding::CByteArrayBinding(CByteArray &value,
                                     const char *name,
                                     const CByteArray *def,
									 const CBinding::BindingMode mode) :
	CBinding(name, mode),
	_value(value)
{
	_default = def;
}

void CByteArrayBinding::WriteToRegistry(IRegistrySection &section)
{
	LPBYTE data = (LPBYTE)_value.GetData();
	UINT size	= (UINT)_value.GetSize();

	section.WriteBinary(_name, data, size);
}

void CByteArrayBinding::ReadFromRegistry(IRegistrySection &section)
{
	LPBYTE  data = NULL;
	UINT    bytes;

	section.ReadBinary(_name, &data, &bytes);


	if (bytes > 0) 
	{
		_value.SetAtGrow(bytes - 1, 0);
		BYTE *p = _value.GetData();
		CopyMemory(p, data, bytes);
		delete data;
	}
	else if (_default)
	{
		ASSERT_VALID(_default);
		_value.Copy(*_default);
	}
}

/*** CWndBinding ***/

CWndBinding::CWndBinding(CWnd *value,
                         const char *name,
						 WindowBindingOptions wbo,
						 const CBinding::BindingMode mode) :
	CBinding(name, mode),
	_window(value),
	_wbo(wbo)
{
}

void CWndBinding::WriteToRegistry(IRegistrySection &section)
{
	WINDOWPLACEMENT wp;

	wp.length = sizeof (WINDOWPLACEMENT);
	_window->GetWindowPlacement(&wp);

	section.WriteInt(_name + "_flags", wp.flags);

	if (!(_wbo & WBO_NOSHOW))
		section.WriteInt(_name + "_section", wp.showCmd);

	section.WriteInt(_name + "_l", wp.rcNormalPosition.left);
	section.WriteInt(_name + "_t", wp.rcNormalPosition.top);
	
	if (!(_wbo & WBO_NOSIZE))
	{
		section.WriteInt(_name + "_r", wp.rcNormalPosition.right);
		section.WriteInt(_name + "_b", wp.rcNormalPosition.bottom);
	}
}

void CWndBinding::ReadFromRegistry(IRegistrySection &section)
{
	ASSERT_VALID(_window);
	ASSERT(_window->GetSafeHwnd() != NULL);

	WINDOWPLACEMENT wp;
	wp.length = sizeof (WINDOWPLACEMENT);
	_window->GetWindowPlacement(&wp);

	/* Make a record of the current height and width of the window, to
	 * use if the user has specified WBO_NOSIZE. */
	UINT width		= wp.rcNormalPosition.right - wp.rcNormalPosition.left;
	UINT height		= wp.rcNormalPosition.bottom - wp.rcNormalPosition.top;

	bool successful = TRUE;

	successful = 
		((wp.flags = section.ReadInt(_name + "_flags", -1)) != -1);

	if (!(_wbo & WBO_NOSHOW))
	{
		successful = successful && 
			((wp.showCmd = section.ReadInt(_name + "_showCmd", -1)) != -1);
	}

	successful = successful && 
		((wp.rcNormalPosition.left	
			= section.ReadInt(_name + "_l", -1)) != -1) &&
		((wp.rcNormalPosition.top	
			= section.ReadInt(_name + "_t", -1)) != -1);

	/* Make sure the upper left corner of the window is onscreen */
	wp.rcNormalPosition.left = min (wp.rcNormalPosition.left,
                                    ::GetSystemMetrics (SM_CXSCREEN) - 
                                        ::GetSystemMetrics (SM_CXICON));

	wp.rcNormalPosition.top = min (wp.rcNormalPosition.top,
                                    ::GetSystemMetrics (SM_CYSCREEN) -
                                        ::GetSystemMetrics (SM_CYICON));

	if (!(_wbo & WBO_NOSIZE))
	{
		successful = successful && 
			((wp.rcNormalPosition.right	
				= section.ReadInt(_name + "_r", -1)) != -1) &&
			((wp.rcNormalPosition.bottom
				= section.ReadInt(_name + "_b", -1)) != -1);
	}
	else
	{
		wp.rcNormalPosition.right = wp.rcNormalPosition.left + width;
		wp.rcNormalPosition.bottom = wp.rcNormalPosition.top + height;
	}

	if (successful)
		_window->SetWindowPlacement (&wp);
}


BOOL CWndBinding::IsWindowBinding(HWND hWnd)
{
	return (_window->m_hWnd == hWnd);
}

/**********************************************************************
 * CRegistryBindings Implementation
 */

/* CRegistrySection
 * 
 * This is the class we provide to bindings to allow them access to
 * registry data.
 */
class CRegistrySection : public IRegistrySection
{
public:
	CRegistrySection(HKEY key)
	{
		/* Note: we're allowing NULL handles here, as a sort of
		 * ad hoc way to represent sections that do not exist
		 * and cannot be created. */
		_key = key;
	}

	~CRegistrySection()
	{
		::RegCloseKey(_key);
	}

	virtual UINT ReadInt(CString entry, int defaultValue)
	{
		DWORD type;
		DWORD size = sizeof(DWORD);
		DWORD value;

		LONG rc = RegQueryValueEx(_key, entry, NULL, &type, (LPBYTE)&value, &size);
		
		if (rc != ERROR_SUCCESS)
			return defaultValue;

		ASSERT(type == REG_DWORD);
		ASSERT(size == sizeof(DWORD));

		return (UINT)value;
	}

	virtual void WriteInt(CString entry, UINT value)
	{
		LONG rc = RegSetValueEx(_key, entry, NULL, REG_DWORD, (LPBYTE)&value, sizeof(DWORD));

		if (rc != ERROR_SUCCESS)
		{
			if (rc == ERROR_ACCESS_DENIED)
				throw CRegistryBindings::RegistryAccessDenied();
			else
				throw CRegistryBindings::RegistryError();
		}
	}

	virtual CString ReadString(CString entry, CString defaultValue)
	{
		DWORD type;
		DWORD size;
		CString buf;

		LONG rc = RegQueryValueEx(_key, entry, NULL, &type, NULL, &size);

		if (rc != ERROR_SUCCESS)
			return defaultValue;

		ASSERT(type == REG_SZ);

		rc = RegQueryValueEx(_key, entry, NULL, &type, 
			                 (LPBYTE)buf.GetBuffer(size/sizeof(TCHAR)), &size);
		buf.ReleaseBuffer();

		if (rc != ERROR_SUCCESS)
			return defaultValue;

		ASSERT(type == REG_SZ);

		return buf;
	}

	virtual void WriteString(CString entry, CString value)
	{
		LONG rc;
		const TCHAR *buf = value.GetString();
		
		rc = RegSetValueEx(_key, entry, NULL, REG_SZ,
	                       (LPBYTE)buf, (lstrlen(buf) +1 ) * sizeof(TCHAR));

		if (rc != ERROR_SUCCESS)
		{
			if (rc == ERROR_ACCESS_DENIED)
				throw CRegistryBindings::RegistryAccessDenied();
			else
				throw CRegistryBindings::RegistryError();
		}
	}

	virtual void ReadBinary(CString entry, LPBYTE *buf, UINT *size)
	{
		DWORD type;
		DWORD bytes;

		LONG rc = RegQueryValueEx(_key, entry, NULL, &type, NULL, &bytes);
		*size = bytes;

		if (rc == ERROR_SUCCESS)
		{
			ASSERT(type == REG_BINARY);
			*buf = new BYTE[*size];

			DWORD s;

			rc = RegQueryValueEx(_key, entry, NULL, &type, *buf, &s);

			*size = (UINT)s;
		}

		if (rc == ERROR_SUCCESS)
		{
			ASSERT(type == REG_BINARY);
			return;
		}

		delete [] *buf;
		*buf = NULL;

		if (rc == ERROR_ACCESS_DENIED)
			throw CRegistryBindings::RegistryAccessDenied();
		else
			throw CRegistryBindings::RegistryError();
	}

	virtual void WriteBinary(CString entry, LPBYTE buf, UINT size)
	{
		LONG rc = RegSetValueEx(_key, entry, NULL, REG_BINARY, buf, size);

		if (rc != ERROR_SUCCESS)
		{
			if (rc == ERROR_ACCESS_DENIED)
				throw CRegistryBindings::RegistryAccessDenied();
			else
				throw CRegistryBindings::RegistryError();
		}
	}

private:
	HKEY _key;
};

/* CDefaultValuesRegistrySection
 * 
 * This simulates registry access, but really just provides 
 * default values (and fails on writes). It is used to
 * reinitialize bound variables to default values.
 */
class CDefaultValuesRegistrySection : public IRegistrySection
{
public:
	virtual UINT ReadInt(CString entry, int defaultValue)
	{
		return defaultValue;
	}

	virtual void WriteInt(CString entry, UINT value)
	{
          UNREFERENCED(entry);
          UNREFERENCED(value);

          throw CRegistryBindings::RegistryAccessDenied();
	}

	virtual CString ReadString(CString entry, CString defaultValue)
	{
		return defaultValue;
	}

	virtual void WriteString(CString entry, CString value)
	{
		throw CRegistryBindings::RegistryAccessDenied();
	}

	virtual void ReadBinary(CString entry, LPBYTE *buf, UINT *size)
	{
		*buf = NULL;
		*size = 0;
	}

	virtual void WriteBinary(CString entry, LPBYTE buf, UINT size)
	{
          UNREFERENCED(entry);
          UNREFERENCED(buf);
          UNREFERENCED(size);

		throw CRegistryBindings::RegistryAccessDenied();
	}
};



/* InstanceList
 *
 * We keep an instance list around so that the window deletion hook
 * may be used to remove window bindings from all registry binding
 * sets.
 */

CObList &InstanceList()
{
	static CObList instances;

	return instances;
}

/* This is the guts of CRegistryBindings */

CRegistryBindings::CRegistryBindings(CString sectionName, 
                                     BindingScope scope             /* = CURRENT_USER */,
                                     CString overrideCompanyName    /* = "" */,
                                     CString overrideAppName        /* = "" */)
{
	// As in MFC, we store our registry data underneath:
	//      "\software\<company_name>\<app_name>\<section>"
	_overrideCompanyName    = overrideCompanyName;
	_overrideAppName        = overrideAppName;
	_sectionName            = sectionName;
	_scope                  = scope;
	_readOnly               = FALSE;

	// Add ourselves to the global CREgistryBinding instance list, and set up
	// the hook so that we can detect window closures
	if (InstanceList().IsEmpty())
	{
		m_hHook = ::SetWindowsHookEx(WH_CBT, FilterHook, NULL, ::GetCurrentThreadId());
		if (m_hHook == NULL)
			AfxThrowMemoryException();
	}

	InstanceList().AddTail(this);
}



CRegistryBindings::~CRegistryBindings()
{
  	POSITION i = _bindingList.GetHeadPosition();
	while (i != NULL) 
	{
		CObject *o = _bindingList.GetAt(i);
		ASSERT_VALID(o);    
		delete o;
		_bindingList.SetAt(i, NULL);

		_bindingList.GetNext(i);
	}

	POSITION f = InstanceList().Find(this);
	ASSERT(f != NULL);

	InstanceList().RemoveAt(f);
	if (InstanceList().IsEmpty())
		UnhookWindowsHookEx(m_hHook);
}


void CRegistryBindings::Add(CBinding *b)
{
#ifdef _DEBUG  
	POSITION pos = _bindingList.GetHeadPosition();
	while (pos != NULL) 
	{
		CObject *o = _bindingList.GetNext(pos);
		ASSERT_VALID(o);
		CBinding *lb = (CBinding *)o;

		if (lb->SameNameAs(b))
			ASSERT(!"The same key already exists in this section\n");
	}
#endif

  ASSERT_VALID(b);
  _bindingList.AddTail(b);

  CRegistrySection section(GetKeyWithFallbackToReadOnly());

  b->Read(section);
}

void CRegistryBindings::Write(BOOL del)
{
	POSITION pos1 = _bindingList.GetHeadPosition();
	POSITION pos2;

	while (pos1 != NULL)
	{
		pos2 = pos1;
		CBinding *binding = (CBinding *)_bindingList.GetNext(pos1);
		ASSERT_VALID(binding);

		CRegistrySection section(GetKeyWithFallbackToReadOnly());

		binding->WriteToRegistry(section);

		if (del)
		{
			_bindingList.RemoveAt(pos2);
			delete binding;
		}
	}
}

void CRegistryBindings::Read(IRegistrySection &section)
{
	POSITION pos1 = _bindingList.GetHeadPosition();
	POSITION pos2;

	while (pos1 != NULL)
	{
		pos2 = pos1;
		CBinding *binding = (CBinding *)_bindingList.GetNext(pos1);
		ASSERT_VALID(binding);

		binding->ReadFromRegistry(section);
	}
}

void CRegistryBindings::ReadFromRegistry()
{
	CRegistrySection section(GetKeyWithFallbackToReadOnly());

	Read(section);
}

void CRegistryBindings::ReadFromDefaults()
{
	CDefaultValuesRegistrySection section;

	Read(section);
}

HKEY CRegistryBindings::GetKeyWithFallbackToReadOnly()
{
	// Try to open it up however we are...
	try 
	{
		return GetKey();
	}
	catch (RegistryAccessDenied)
	{
		// ...if that fails, degrade to read only...
		_readOnly = TRUE;
	}
    catch (RegistryError)
    {
        return NULL;
    }

	// ...and try again...
	try
	{
		return GetKey();
	}
	catch (RegistryError)
	{
		// ...if that fails, we'll signal the error with a null HKEY...
	}

	return NULL;
}

HKEY CRegistryBindings::GetKey()
{
	HKEY baseKey		= NULL;
	HKEY softwareKey	= NULL;
	HKEY companyKey		= NULL;
	HKEY appKey			= NULL;
	HKEY sectionKey		= NULL;

	CString companyName;
	CString appName;

	REGSAM regSecurity;
	DWORD disposition;
	LONG rc;

	CWinApp *app = AfxGetApp();
	ASSERT_VALID(app);


	/* Determine the company and application names. If the user hasn't
	 * specified names at construct time, then we fill them in from
	 * the usual MFC sources:
	 *
	 * CompanyName	is	m_pszRegistryKey	which-is-set-by	CWinApp::SetRegistryKey
	 * AppName		is	m_pszAppNAme		whish-is-set-by	CWinApp::CWinApp
	 */
	if (_overrideCompanyName.GetLength())
	{
		companyName = _overrideCompanyName;
	}
	else
	{
		ASSERT(app->m_pszRegistryKey != NULL);

		companyName = app->m_pszRegistryKey;
	}

	if (_overrideAppName.GetLength())
	{
		appName = _overrideAppName;
	}
	else
	{
		ASSERT(app->m_pszAppName != NULL);

		appName = app->m_pszAppName;
	}


	/*  Our base key is derived based on the scope of the registry bindings */
	switch(_scope)
	{
	case LOCAL_SYSTEM: baseKey = HKEY_LOCAL_MACHINE; break;
	case CURRENT_USER: baseKey = HKEY_CURRENT_USER; break;
	default: ASSERT(!"Invalid scope in CRegistryBindings");
	}

	if (IsReadOnly())
		regSecurity = KEY_READ;
	else
		regSecurity = KEY_READ | KEY_WRITE;
	
	/*
	 * Open and/or Create the requested key
	 */

	rc = RegOpenKeyEx(baseKey, _T("software"), 0, regSecurity, &softwareKey);	

	if (rc == ERROR_SUCCESS)
	{
		if (IsReadOnly())
			rc = RegOpenKeyEx(softwareKey, companyName, 0, regSecurity, &companyKey);
		else
			rc = RegCreateKeyEx(softwareKey,  companyName,
								0, REG_NONE, REG_OPTION_NON_VOLATILE, regSecurity, NULL,
								&companyKey,
								&disposition);
	}

	if (rc == ERROR_SUCCESS)
	{
		if (IsReadOnly())
			rc = RegOpenKeyEx(companyKey, appName, 0, regSecurity, &appKey);
		else
			rc = RegCreateKeyEx(companyKey,  appName,
								0, REG_NONE, REG_OPTION_NON_VOLATILE, regSecurity, NULL,
								&appKey,
								&disposition);
	}


	if (rc == ERROR_SUCCESS)
	{
		if (IsReadOnly())
			rc = RegOpenKeyEx(appKey, _sectionName, 0, regSecurity, &sectionKey);
		else
			rc = RegCreateKeyEx(appKey,  _sectionName,
								0, REG_NONE, REG_OPTION_NON_VOLATILE, regSecurity, NULL,
								&sectionKey,
								&disposition);
	}

	/* Close the keys that we do _not_ return. In the event of a failure, we
	 * know that we have no valid sectionKey, so this handles failure too.*/
	if (appKey != NULL)			RegCloseKey(appKey);
	if (companyKey != NULL)		RegCloseKey(companyKey);
	if (softwareKey != NULL)	RegCloseKey(softwareKey);

	if (rc != ERROR_SUCCESS)
	{ 
		if (rc == ERROR_ACCESS_DENIED)
			throw RegistryAccessDenied();
		else;
			throw RegistryError();
	}

	return sectionKey;
}


BOOL CRegistryBindings::IsReadOnly()
{
	return _readOnly;
}


/*** Automatic window deletion ***
 *
 * CRegistryBindings will automatically delete windows from its binding
 * list when the windows are deleted. To do this, it registers an
 * application-wide hook function, and watches for HCBT_DESTROYWND.
 */

HHOOK    CRegistryBindings::m_hHook;

BOOL CRegistryBindings::Hook(const HWND hwnd)
{
	CBinding *e;

	if (FindAndRemoveWindow(e, hwnd))
	{
		CRegistrySection section(GetKeyWithFallbackToReadOnly());

		e->WriteToRegistry(section);		

		delete e;

		return TRUE;
	}

	return FALSE;
}

BOOL CRegistryBindings::FindAndRemoveWindow(CBinding *&e, const HWND hwnd)
{
  POSITION pos = _bindingList.GetHeadPosition(), spos;

	while (pos != NULL) 
	{
		spos = pos;
		CObject *o = _bindingList.GetNext(pos);
		ASSERT_VALID(o);
		e = (CBinding *)o;

		if (e->IsWindowBinding(hwnd))
		{
			_bindingList.RemoveAt(spos);
			return TRUE;
		}
	}
	return FALSE;
}


LRESULT CALLBACK 
CRegistryBindings::FilterHook(int code, WPARAM wParam, LPARAM lParam)
{
	if (code == HCBT_DESTROYWND)
	{
		ASSERT(wParam != NULL); // should be non-NULL HWND

		POSITION pos = InstanceList().GetHeadPosition();

		while (pos != NULL)
		{
			CObject *o = InstanceList().GetNext(pos);
			ASSERT_VALID(o);
			CRegistryBindings *rs = (CRegistryBindings *)o;

			if (rs->Hook((HWND)wParam))
				break;
		}
	}
	return CallNextHookEx(m_hHook, code, wParam, lParam);
}





