// TipWindow.cpp : implementation file
//

#include "stdafx.h"
#include "vcalc.h"
#include "TipWindow.h"

#include "../vm/scan.h"

#include "lisp-window.h"

// TipWindow dialog

IMPLEMENT_DYNAMIC(TipWindow, CDialog)
TipWindow::TipWindow(CWnd* pParent /*=NULL*/)
	: CDialog(TipWindow::IDD, pParent)
	, _NoAutoTipDisplay(FALSE)
{
	_currentTipID = 0;
}

TipWindow::~TipWindow()
{	
}

void TipWindow::DoDataExchange(CDataExchange* pDX)
{
    CDialog::DoDataExchange(pDX);
    DDX_Control(pDX, IDC_NOAUTOTIPDISPLAY, m_NoAutoTipDisplayCtrl);
    DDX_Check(pDX, IDC_NOAUTOTIPDISPLAY, _NoAutoTipDisplay);
    DDX_Control(pDX, IDC_TIPTEXT, _TipText);
}


BEGIN_MESSAGE_MAP(TipWindow, CDialog)
	ON_BN_CLICKED(IDPREVIOUSTIP, OnBnClickedPrevioustip)
	ON_BN_CLICKED(IDNEXTTIP, OnBnClickedNexttip)
	ON_WM_CTLCOLOR()
END_MESSAGE_MAP()


// TipWindow message handlers

void TipWindow::OnBnClickedPrevioustip()
{
}

void TipWindow::OnBnClickedNexttip()
{
	_currentTipID++;

	LoadTip();
}

void TipWindow::LoadTip()
{
    CString tipString;

    tipString.LoadString(IDS_TIP0 + _currentTipID);

    if (strlen(tipString) == 0)
    {
        _currentTipID = 0;
        tipString.LoadString(IDS_TIP0 + _currentTipID);
    }
    
    _TipText.SetWindowText(tipString);
};

BOOL TipWindow::OnInitDialog()
{
	CDialog::OnInitDialog();

	OnBnClickedNexttip();

    LoadTip();

	return TRUE;
}

/* Scheme interface routines **********************************/

LRef lshow_tip(LRef parent, LRef state)
{
  if (!WINDOWP(parent))
    vmerror_wrong_type(1, parent);

		TipWindow dlg(WNDOB(parent));

	if (!NULLP(state))
	{
		if (!CONSP(state)) 
                  vmerror_wrong_type(2, state);

		if (!BOOLP(CAR(state)) && !FIXNUMP(CDR(state)))
			vmerror("Invalid tip window state in tip-window", state);

		dlg._NoAutoTipDisplay	= BOOLV(CAR(state));
		dlg._currentTipID		= (int)get_c_fixnum(CDR(state));		
	}	

	dlg.DoModal();

	return lcons(boolcons(dlg._NoAutoTipDisplay), fixcons(dlg._currentTipID));
}


HBRUSH TipWindow::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor)
{
	if (pWnd == &_TipText)
		return (HBRUSH)GetStockObject(WHITE_BRUSH);

	HBRUSH hbr = CDialog::OnCtlColor(pDC, pWnd, nCtlColor);

	return hbr;
}
