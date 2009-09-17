// VCChooseBox.cpp : implementation file
//

#include "stdafx.h"
#include "vcalc.h"
#include "VCChooseBox.h"
#include "lisp-window.h"

// CVChooseBox dialog

IMPLEMENT_DYNAMIC(CVChooseBox, CDialog)
CVChooseBox::CVChooseBox(CWnd* pParent /*=NULL*/)
	: CDialog(CVChooseBox::IDD, pParent)
{
	m_chooseList = NULL;
	m_chosen = NULL;
	m_chooseListTitle = "";
	m_chooseListDesc = "";
}

CVChooseBox::~CVChooseBox()
{
}

void CVChooseBox::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	DDX_Control(pDX, IDC_CHOOSELIST, m_chooseListCtrl);
	DDX_Control(pDX, IDC_CHOOSEDESC, m_chooseListDescCtrl);
}

BEGIN_MESSAGE_MAP(CVChooseBox, CDialog)
	ON_BN_CLICKED(IDOK, OnBnClickedOk)
END_MESSAGE_MAP()

void CVChooseBox::SetChooseList(LRef cl, _TCHAR *title, _TCHAR *desc)
{
	m_chooseList = cl;

	if (title)
		m_chooseListTitle = title;

	if (desc)
		m_chooseListDesc = desc;
}

LRef CVChooseBox::GetChosenItem()
{
	return m_chosen;
}


// CVChooseBox message handlers

BOOL CVChooseBox::OnInitDialog()
{
	LRef p;

	CDialog::OnInitDialog();

	if (m_chooseListTitle != "")
		SetWindowText(m_chooseListTitle);

	if (m_chooseListDesc != "")
		m_chooseListDescCtrl.SetWindowText(m_chooseListDesc);

	for(p = m_chooseList; CONSP(p); p = CDR(p))
	{
		LRef list_node = lcar(p);

		/* We're assuming that the choose list has been properly
		 * vetted by the caller. Any invalid nodes are bypassed
		 * without error reporting. */
		if (!CONSP(list_node) || (!STRINGP(CAR(list_node))))
			continue;

		int index = m_chooseListCtrl.AddString(get_c_string(lcar(list_node)));
		m_chooseListCtrl.SetItemDataPtr(index, list_node);
	}


	return TRUE;  // return TRUE unless you set the focus to a control
	// EXCEPTION: OCX Property Pages should return FALSE
}

void CVChooseBox::OnBnClickedOk()
{
	int current_selection = 
			m_chooseListCtrl.GetCurSel();

	if (current_selection == LB_ERR)
		m_chosen = NIL;
	else
		m_chosen = (LRef)m_chooseListCtrl.GetItemDataPtr(current_selection);

	OnOK();
}

/* Scheme interface routines **********************************/

LRef lchoose(LRef parent, LRef choose_list,  LRef title, LRef description)
{
  if (!WINDOWP(parent))
	{
		vmerror_wrong_type(1, parent);
		return boolcons(FALSE);
	}
	else
	{
		CVChooseBox dlg(WNDOB(parent));

		LRef rc = NIL;

		dlg.SetChooseList(choose_list,
						  get_c_string(title),
						  get_c_string(description));

		INT_PTR dlgRc = dlg.DoModal();

		switch (dlgRc)
		{
		case -1:
			vmerror("Couldn't create dialog box in choose", NIL);
			break;

		case IDOK:
			rc = dlg.GetChosenItem();
			break;

		case IDCANCEL:
			rc = boolcons(FALSE);
			break;
		}

		return rc;
	}
}

