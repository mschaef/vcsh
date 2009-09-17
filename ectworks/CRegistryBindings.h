/*
 * CRegistryBindings.h
 *
 * Copyright (C) 2003-4 by Mike Schaeffer (mschaef@mschaef.com)
 * Portions Copyright (C) 1998 by Joerg Dentler (dentler@tpnet.de)
 * All rights reserved
 *
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

#ifndef CREGISTRY_BINDINGS_H
#define CREGISTRY_BINDINGS_H


#include <afx.h>

/* IRegistrySection interface 
 *
 * This encapsulates access to a given section in the registry.
 * References to this interface are passed into binding objects
 * to enable them to read from and write to registry keys in
 * a section determined by the binding collection.
 */

class IRegistrySection
{
public:
	virtual UINT ReadInt(CString entry, int defaultValue = 0) = 0;
	virtual void WriteInt(CString entry, UINT value) = 0;

	virtual CString ReadString(CString entry, CString defaultValue) = 0;
	virtual void WriteString(CString entry, CString value) = 0;

	virtual void ReadBinary(CString entry, LPBYTE *buf, UINT *size) = 0;
	virtual void WriteBinary(CString entry, LPBYTE buf, UINT size) = 0;
};

/* CBinding class 
 *
 * This class (and its subclasses) model bindings between
 * variables and registry keys. Custom binding types can be
 * implemented by subclassing from a class in this hierarchy.
 * The noteworthy virtual methods to override are:
 * 
 * WriteToRegistry - Store the binding's contents into the registry
 * ReadFromRegistry - Read the binding's contents from the registry
 * IsWindowBinding - Return TRUE if this binding is specific to the specified window.
 **/

class CBinding : public CObject
{
public:
	virtual void Write(IRegistrySection &section);
	virtual void Read(IRegistrySection &section);

	virtual void WriteToRegistry(IRegistrySection &section) = 0;
	virtual void ReadFromRegistry(IRegistrySection &section) = 0;

	virtual BOOL IsWindowBinding(HWND hWnd);
	virtual BOOL SameNameAs(const CBinding *b);

protected:

	/* Binding modes determine what happens when the binding doesn't
	 * exist in the registry. If it's normal, the value is always 
	 * written to the Registry. If it's hidden, the value is never 
	 * written to the registry and the default is used unless a value
	 * has been manually entered using the registry editor. */
	enum BindingMode {
		BINDING_NORMAL,
		BINDING_HIDDEN
	};

	BindingMode _mode;
	CString _name;

	CBinding(CString name, BindingMode mode);
};


/* CRegistryBindings
 *
 * This class models a set of registry bindings. Instantiate it,
 * and call Add(CBinding *) to add a series of heap allocated
 * CBindings. These instances will be deleted by CRegistryBindings
 * when the class is destoryed. Adding a binding causes it to be
 * read from the registry.  To write data out to the registry,
 * you must call Write(const BOOL).
 **/

class CRegistryBindings : public CObject
{
public:
	class RegistryError { };
	class RegistryAccessDenied : public RegistryError { };


	enum BindingScope {
		LOCAL_SYSTEM,
		CURRENT_USER
	};

	CRegistryBindings(CString sectionName, 
		              BindingScope scope			= CURRENT_USER,
					  CString overrideCompanyName	= "",
					  CString overrideAppName		= "");
	
	~CRegistryBindings();

	void Add(CBinding *b);
	void Write(const BOOL clear = FALSE);

	
	void ReadFromRegistry();
	void ReadFromDefaults();

	BOOL IsReadOnly();

protected:
	void Read(IRegistrySection &section);
	HKEY GetKeyWithFallbackToReadOnly();
	HKEY GetKey();
	

	BOOL _readOnly;
	BindingScope _scope;
	CObList  _bindingList;

	CString _overrideCompanyName;
	CString _overrideAppName;	

	CString _sectionName;

	BOOL Hook(const HWND hwnd);
	BOOL FindAndRemoveWindow(CBinding *&e, const HWND hwnd);


	static HHOOK    m_hHook;

	static LRESULT CALLBACK FilterHook(int code, WPARAM wParam, LPARAM lParam);
};



/****************************************************************
 * Some standard registry binding objects.
 */

class CIntBinding : public CBinding
{
public:
	CIntBinding(int &value,
		        const _TCHAR *name,
				const int def = 0,
				const CBinding::BindingMode mode = BINDING_NORMAL);

	virtual void WriteToRegistry(IRegistrySection &section);
	virtual void ReadFromRegistry(IRegistrySection &section);

protected:
	int &_value;
	int _default;
};

class CDWORDBinding : public CBinding
{
public:
	CDWORDBinding(DWORD &value,
                  const _TCHAR *name,
                  const DWORD def = 0,
                  const CBinding::BindingMode mode = BINDING_NORMAL);

	virtual void WriteToRegistry(IRegistrySection &section);
	virtual void ReadFromRegistry(IRegistrySection &section);

protected:
	DWORD &_value;
	DWORD _default;
};

class CStringBinding : public CBinding
{
public:
	CStringBinding(CString &value,
                   const _TCHAR *name,
                   const _TCHAR *def = NULL,
                   const CBinding::BindingMode mode = BINDING_NORMAL);

	virtual void WriteToRegistry(IRegistrySection &section);
	virtual void ReadFromRegistry(IRegistrySection &section);

protected:
	CString &_value;
	const _TCHAR *_default;
};

class CByteArrayBinding : public CBinding 
{
public:
	CByteArrayBinding(const CByteArrayBinding &o);
	CByteArrayBinding(CByteArray &value,
                      const _TCHAR *name,
                      const CByteArray *def = NULL,
                      const CBinding::BindingMode t = BINDING_NORMAL);

	virtual void WriteToRegistry(IRegistrySection &section);
	virtual void ReadFromRegistry(IRegistrySection &section);

protected:
	CByteArray &_value;
	const CByteArray *_default;
};

class CWndBinding : public CBinding
{
public:
	enum WindowBindingOptions {
		WBO_NORMAL = 0x00000000,
		WBO_NOSIZE = 0x00000001,
		WBO_NOSHOW = 0x00000002
	};

	CWndBinding(CWnd *value,
                const _TCHAR *name,
				WindowBindingOptions wbo = WBO_NORMAL,
                const CBinding::BindingMode mode = BINDING_NORMAL);

	virtual void WriteToRegistry(IRegistrySection &section);
	virtual void ReadFromRegistry(IRegistrySection &section);
	virtual BOOL IsWindowBinding(HWND hWnd);

protected:
	CWnd *_window;
	WindowBindingOptions _wbo;
};

template<class TYPE>
class CObjectBinding: public CBinding
{
private:
	TYPE **_objVar;

public:	
	CObjectBinding(TYPE **value,
                   const _TCHAR *name,					  
                   const CBinding::BindingMode mode = BINDING_NORMAL)
		: CBinding(name, mode)
	{
		_objVar = value;
	}

	virtual void WriteToRegistry(IRegistrySection &section)
	{
		ASSERT(_objVar);
		ASSERT_VALID(*_objVar);

		CMemFile contents;
		CArchive ar(&contents, CArchive::store);	

		(*_objVar)->Serialize(ar);

		ar.Close();

		UINT length			= (UINT)contents.GetLength();
		LPBYTE memory		= (LPBYTE)contents.Detach();

		contents.Close();

		section.WriteBinary(_name, memory, length);

		delete [] memory;
	}

	virtual void ReadFromRegistry(IRegistrySection &section)
	{
		if (*_objVar == NULL)
			*_objVar = new TYPE;

		LPBYTE data;
		UINT dataLength;

		try {
			section.ReadBinary(_name, &data, &dataLength);

			CMemFile contents(data, dataLength);
			CArchive ar(&contents, CArchive::load);

			(*_objVar)->Serialize(ar);

			ar.Close();

			contents.Detach();
			delete [] data;
		}
		catch (CRegistryBindings::RegistryError)
		{
		}
	}
};


#endif /* CREGISTRY_BINDINGS_H */

