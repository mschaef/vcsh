#ifndef __IDCOMBO_H
#define __IDCOMBO_H

/////////////////////////////////////////////////////////////////////////////
// CAutoSizeCombo window

class CAutoSizeCombo : public CComboBox
{
public:
	CAutoSizeCombo();

    int maxlen; 

protected:
	//{{AFX_MSG(CIDCombo)
	afx_msg void OnDropdown();
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
// CIDCombo window

typedef struct {
	int id;  // IDS_ for string, or 0 for end of table for AddStrings
	INT_PTR val; // value for itemdata
} IDData;

typedef struct {
	int id;  // IDS_ for string, or 0 for end of table for AddStrings
	_TCHAR *val; // value for itemdata
} IDStringData;


class CIDCombo : public CAutoSizeCombo
{

// Attributes
public:

// Operations
public:
	int AddString(IDData * data);
	int AddString(CString &s) { return CComboBox::AddString(s); }
	int AddStrings(IDData * data, int def = 0);
	int AddStrings(IDStringData * data, int def = 0);
	int SelectString(_TCHAR *itemval);
	int Select(DWORD itemval);

	DWORD_PTR GetCurItem();
	_TCHAR *GetCurItemString();
		
public:
    
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
#endif //__IDCOMBO_H