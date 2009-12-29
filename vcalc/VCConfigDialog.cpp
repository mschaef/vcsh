// VCConfigDialog.cpp : implementation file
//

#include "stdafx.h"
#include "vcalc.h"
#include "VCConfigDialog.h"
#include "../ectworks/CRegistrationDialog.h"
#include "lisp-window.h"

// CVConfigDialog dialog

IMPLEMENT_DYNAMIC(CVConfigDialog, CDialog)
CVConfigDialog::CVConfigDialog(CWnd* pParent /*=NULL*/)
	: CDialog(CVConfigDialog::IDD, pParent)
{
	m_OptionChangedCallback = NULL;
}

CVConfigDialog::~CVConfigDialog()
{
}

void CVConfigDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	DDX_Control(pDX, IDC_DEFAULTBASE, m_DefaultBaseCtrl);
	DDX_Control(pDX, IDC_ANGLEMODE, m_AngleModeCtrl);
	DDX_Control(pDX, IDC_INTERESTMODE, m_InterestModeCtrl);
	DDX_Control(pDX, IDC_NUMBERSTYLE, m_NumberStyleCtrl);
	DDX_Control(pDX, IDC_NUMBERDIGITS, m_NumberDigitsCtrl);
	DDX_Control(pDX, IDC_SEPERATORSTYLE, m_NumberSeperatorCtrl);
}


BEGIN_MESSAGE_MAP(CVConfigDialog, CDialog)
	ON_CBN_SELCHANGE(IDC_DEFAULTBASE, OnConfigOptionChanged)
	ON_CBN_SELCHANGE(IDC_ANGLEMODE, OnConfigOptionChanged)
	ON_CBN_SELCHANGE(IDC_INTERESTMODE, OnConfigOptionChanged)
	ON_CBN_SELCHANGE(IDC_NUMBERSTYLE, OnConfigOptionChanged)
	ON_CBN_SELCHANGE(IDC_NUMBERDIGITS, OnConfigOptionChanged)
	ON_CBN_SELCHANGE(IDC_SEPERATORSTYLE, OnConfigOptionChanged)
	ON_BN_CLICKED(IDREGISTER, OnBnClickedRegister)
END_MESSAGE_MAP()


// CVConfigDialog message handlers

IDStringData angleModes[] = {
	{ IDS_DEGREES , "degrees" },
	{ IDS_RADIANS , "radians" },
	{ IDS_GRADIANS, "gradians"},
	{ 0, 0 }
};

IDStringData interestModes[] = {
	{ IDS_BEGIN , "begin" },
	{ IDS_END   , "end"   },
	{ 0, 0 }
};

IDStringData numberSeperators[] = {
	{ IDS_USGROUP   , "us"     },
	{ IDS_EUROGROUP , "euro"   },
	{ IDS_NOGROUP   , "none"   },
	{ 0, 0 }
};

IDStringData numberStyles[] = {
	{ IDS_FIXED     , "fixed"      },
	{ IDS_SCIENTIFIC, "scientific" },
	{ 0, 0 }
};

IDStringData defaultBases[] = {
	{ IDS_BASE10    , "decimal"      },
	{ IDS_BASE16    , "hexadecimal"  },
	{ IDS_BASE8     , "octal"        },
	// { IDS_BASE2     , ":binary"       },
	{ 0, 0 }
};

BOOL CVConfigDialog::OnInitDialog()
{
	CString numberDigitsText;

	CDialog::OnInitDialog();

	m_AngleModeCtrl.AddStrings(angleModes);
	m_AngleModeCtrl.SelectString(m_AngleMode);

	m_NumberSeperatorCtrl.AddStrings(numberSeperators);
	m_NumberSeperatorCtrl.SelectString(m_NumberSeperator);

	m_NumberStyleCtrl.AddStrings(numberStyles);
	m_NumberStyleCtrl.SelectString(m_NumberStyle);

	m_InterestModeCtrl.AddStrings(interestModes);
	m_InterestModeCtrl.SelectString(m_InterestMode);

	m_DefaultBaseCtrl.AddStrings(defaultBases);
	m_DefaultBaseCtrl.SelectString(m_DefaultBase);

	numberDigitsText.Format("%d", m_NumberDigits);
	m_NumberDigitsCtrl.SetWindowText(numberDigitsText);

	return TRUE;  // return TRUE unless you set the focus to a control
	// EXCEPTION: OCX Property Pages should return FALSE
}

void CVConfigDialog::OnOK()
{
	UpdateDialogMembers();

	if ((m_NumberDigits > 15) || (m_NumberDigits < 0))
	{
		
		AfxMessageBox(IDS_ERR_NUMDIGITS_RANGE,
			          MB_ICONEXCLAMATION | MB_OK | MB_APPLMODAL);
		return;
	}

	CDialog::OnOK();
}

void CVConfigDialog::OnBnClickedRegister()
{
	CRegistrationDialog dlg(NULL);
	
	dlg.DoModal();
}


void CVConfigDialog::UpdateDialogMembers()
{
	CString numberDigitsText;

	m_AngleMode       = m_AngleModeCtrl.GetCurItemString();
	m_NumberSeperator = m_NumberSeperatorCtrl.GetCurItemString();
	m_NumberStyle     = m_NumberStyleCtrl.GetCurItemString();
	m_InterestMode    = m_InterestModeCtrl.GetCurItemString();
	m_DefaultBase     = m_DefaultBaseCtrl.GetCurItemString();

	 m_NumberDigitsCtrl.GetWindowText(numberDigitsText);

	/* Assuming that the number is valid, this should be TRUE
	 * if the control itself is in NUMERIC style. */
	m_NumberDigits = atoi(numberDigitsText);

}

bool CVConfigDialog::SetOptionVector(LRef config_vector)
{
	m_AngleMode       = NULL;
	m_NumberSeperator = NULL;
	m_NumberStyle     = NULL;
	m_NumberDigits    = 0;
	m_InterestMode    = NULL;
	m_DefaultBase     = NULL;


	if (VECTORP(config_vector))
	{
		if (VECTOR_DIM(config_vector) != 6)
                  vmerror("Configuration vectors need to be of length 6", config_vector);
		
		/* This code is tolerant of invalid vector members. If something
		 * isn't right, it just defaults to NULL or 0. */
		if (lkeywordp(VECTOR_ELEM(config_vector, 0)))
			m_AngleMode = get_c_string(VECTOR_ELEM(config_vector, 0));

		if (lkeywordp(VECTOR_ELEM(config_vector, 1)))
			m_NumberSeperator = get_c_string(VECTOR_ELEM(config_vector, 1));

		if (lkeywordp(VECTOR_ELEM(config_vector, 2)))
			m_NumberStyle = get_c_string(VECTOR_ELEM(config_vector, 2));

		if (FIXNUMP(VECTOR_ELEM(config_vector, 3)))
			m_NumberDigits = (int)get_c_fixnum(VECTOR_ELEM(config_vector, 3));

		if (lkeywordp(VECTOR_ELEM(config_vector, 4)))
			m_InterestMode = get_c_string(VECTOR_ELEM(config_vector, 4));

		if (lkeywordp(VECTOR_ELEM(config_vector, 5)))
			m_DefaultBase  = get_c_string(VECTOR_ELEM(config_vector, 5));

	} else if (!NULLP(config_vector))
		return TRUE;

	return FALSE;		
}

LRef CVConfigDialog::GetOptionVector()
{
	LRef result_vector = lmake_vector(fixcons(6), boolcons(FALSE));

	if (!NULLP(result_vector))
	{        
		if (m_AngleMode)
                  SET_VECTOR_ELEM(result_vector, 0, keyword_intern(m_AngleMode));

		if (m_NumberSeperator)
                  SET_VECTOR_ELEM(result_vector, 1, keyword_intern(m_NumberSeperator));

		if (m_NumberStyle)
                  SET_VECTOR_ELEM(result_vector, 2, keyword_intern(m_NumberStyle));

		if (m_NumberDigits >= 0)
                  SET_VECTOR_ELEM(result_vector, 3, fixcons(m_NumberDigits));

		if (m_InterestMode)
                  SET_VECTOR_ELEM(result_vector, 4, keyword_intern(m_InterestMode));

		if (m_DefaultBase)
                  SET_VECTOR_ELEM(result_vector, 5, keyword_intern(m_DefaultBase));

		/* Handle the case where keyword_intern or fixcons failed. boolcons
         * will not fail, since it doesn't do any allocation. */
		for(int i = 0; i < 6; i++)
			if (NULLP(VECTOR_ELEM(result_vector, i)))
                          SET_VECTOR_ELEM(result_vector, i, boolcons(FALSE));

		return result_vector;
	}

    return result_vector;
}


void CVConfigDialog::OnConfigOptionChanged()
{
	if (m_OptionChangedCallback)
	{
		UpdateDialogMembers();

		if (VCalcGetApp()->DelegateMessageToLispProcedure(_T("Option Changed Callback"), m_OptionChangedCallback, NULL, 1, GetOptionVector()))
			WRITE_TEXT_CONSTANT("Error in config change callback", CURRENT_ERROR_PORT);
	}
}


/* The scheme bindings ****************************************
 **/

/* show-config-dialog := <config_vector> -> <config_vector>/#f
 *
 * Displays the configuration dialog box with the specified
 * initial settings. 
 *
 * <config_vector) := #( <angle_keyword>
 *                       <number_seperator_keyword>
 *                       <number_style_keyword>
 *                       <number_digits_fixnum>
 *                       <interest_mode_keyword>
 *                       <default_base_keyword> )
 *
 * Return Value:
 *   #f - Cancel
 *   <config_vector> - The user's specified configuration vector
 *
 * Unrecognized keywords result in unselected combo boxes. If
 * the user does not select a value, they are returned as #f
 * in the returned config_vector.
 */

LRef lshow_config_dialog(LRef parent,
						 LRef config_vector, 
						 LRef changed_callback)
{
  if (!WINDOWP(parent))
	{
		vmerror_wrong_type(1, parent);
		return boolcons(FALSE);
	}
	else
	{
		CVConfigDialog dlg(WNDOB(parent));

		if (dlg.SetOptionVector(config_vector))
			vmerror_wrong_type(2, config_vector);

		if (!NULLP(changed_callback) && TRUEP(changed_callback))
		{
			if (lprocedurep(changed_callback))
				dlg.m_OptionChangedCallback = changed_callback;
			else
				vmerror_wrong_type(3, changed_callback);
		}

		if (dlg.DoModal() == IDOK)
			return dlg.GetOptionVector();
		else
			return boolcons(FALSE);
	}
}



