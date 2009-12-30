// VCEditor.cpp : implementation file
//

#include "stdafx.h"
#include "vcalc.h"
#include "VCEditor.h"
#include "vcwindow.h"
#include ".\vceditor.h"

// CVCEditor dialog

IMPLEMENT_DYNAMIC(CVCEditor, CDialog)
CVCEditor::CVCEditor(CWnd* pParent, LRef syntax_checker): CDialog(CVCEditor::IDD, pParent)
{
  _syntax_checker = syntax_checker;

  assert(NULLP(_syntax_checker) || CLOSUREP(_syntax_checker));

  m_dialogInitialized = FALSE;
}

CVCEditor::~CVCEditor()
{
}

void CVCEditor::SetEditText(CString &editText)
{
  _editText = editText;
}

CString CVCEditor::GetEditText()
{
  return _editText;
}

void CVCEditor::DoDataExchange(CDataExchange* pDX)
{
  CDialog::DoDataExchange(pDX);
  DDX_Control(pDX, IDC_EDITOR, m_Editor);
  DDX_Control(pDX, IDC_REFORMAT_AND_CHECK_SYNTAX, m_ReformatAndCheckSyntax);
  DDX_Control(pDX, IDOK, m_CmdOK);
  DDX_Control(pDX, IDCANCEL, m_CmdCancel);
}


BEGIN_MESSAGE_MAP(CVCEditor, CDialog)
  ON_BN_CLICKED(IDC_REFORMAT_AND_CHECK_SYNTAX, OnBnClickedReformatAndCheckSyntax) 
  ON_WM_SIZE()
  ON_WM_GETMINMAXINFO()
END_MESSAGE_MAP()


// CVCEditor message handlers

BOOL CVCEditor::OnInitDialog()
{
  CDialog::OnInitDialog();

  m_Editor.SetFocus();

  m_Editor.SetSel(0, LONG_MAX);
  m_Editor.ReplaceSel(_editText);

  CHARFORMAT cf;

  cf.cbSize = sizeof(CHARFORMAT);
  cf.dwMask = CFM_BOLD | CFM_COLOR | CFM_FACE | CFM_SIZE;	
  cf.dwEffects = 0;
  cf.crTextColor = RGB(0, 0, 0);
  cf.yHeight = 180;
  strcpy(cf.szFaceName, "Courier");

  m_Editor.SetDefaultCharFormat(cf);

  if (NULLP(_syntax_checker))
    m_ReformatAndCheckSyntax.ShowWindow(SW_HIDE);

  m_dialogInitialized = TRUE;

  // Compute the margins used to resize the dialog box

  CRect clientRect;
  GetClientRect(clientRect);

  CRect editorRect;
  m_Editor.GetWindowRect(editorRect);
  ScreenToClient(editorRect);
  _editor_y_margin = clientRect.bottom - editorRect.bottom;

  CRect buttonRect;
  m_CmdCancel.GetWindowRect(buttonRect);
  ScreenToClient(buttonRect);
  _button_y_margin = clientRect.bottom - buttonRect.bottom;
  _button_x_margin = clientRect.right - buttonRect.right;
  _button_height = buttonRect.Height();
  _button_width = buttonRect.Width();

  DoSizeWindow();

  return FALSE;  // return TRUE unless you set the focus to a control
  // EXCEPTION: OCX Property Pages should return FALSE
}

void CVCEditor::OnOK()
{
  if (CheckSyntax())
    {
      m_Editor.SetSel(0, LONG_MAX);
      _editText = m_Editor.GetSelText();

      CDialog::OnOK();
    }
}

void CVCEditor::OnBnClickedReformatAndCheckSyntax()
{
  CheckSyntax(true);
}

bool CVCEditor::CheckSyntax(bool reformat_also)
{
  bool correct_syntax = false;

  m_Editor.SetSel(0, LONG_MAX);
  CString currentText = m_Editor.GetSelText();

  assert(NULLP(_syntax_checker) || CLOSUREP(_syntax_checker));

  LRef formatted_text = NIL;

  if (VCalcGetApp()->DelegateMessageToLispProcedure(_T("Editor Syntax Checker"), _syntax_checker, &formatted_text, 2, strcons(currentText.GetBuffer()), boolcons(reformat_also)))
    WRITE_TEXT_CONSTANT("Error in syntax checker callback", CURRENT_ERROR_PORT);
  else if (TRUEP(formatted_text))
    {
      correct_syntax = true;

      if (STRINGP(formatted_text))
        {
          m_Editor.SetSel(0, LONG_MAX);
          m_Editor.ReplaceSel(get_c_string(formatted_text));
        }
      else if (reformat_also)
        WRITE_TEXT_CONSTANT("Invalid syntax checker return value", formatted_text);
    }

  return correct_syntax;
}

LRef ledit_text(LRef parent, LRef text, LRef syntax_checker)
{
  if (!WINDOWP(parent))
    {
      vmerror_wrong_type(1, parent);
      return boolcons(FALSE);
    }

  if (!STRINGP(text))
    vmerror_wrong_type(2, text);

  if (!(NULLP(syntax_checker) || CLOSUREP(syntax_checker)))
    vmerror_wrong_type(3,syntax_checker);

  CVCEditor dlg(WNDOB(parent), syntax_checker);

  dlg.SetEditText(CString(get_c_string(text)));

  if (dlg.DoModal() == IDOK)
    return strcons(dlg.GetEditText().GetBuffer());
  else
    return boolcons(false);
}

void CVCEditor::DoSizeWindow()
{
  if (!m_dialogInitialized)
    return;

  HDWP hdwp = BeginDeferWindowPos(4);

  if (hdwp == NULL)
    return; // Bail on Fail

  CRect clientRect;
  GetClientRect(clientRect);
  int cx = clientRect.Width();
  int cy = clientRect.Height();


  DeferWindowPos(hdwp, m_Editor.m_hWnd, NULL,
                 0, 0, cx, cy - _editor_y_margin,
                 SWP_NOZORDER);

  DeferWindowPos(hdwp, m_ReformatAndCheckSyntax.m_hWnd, NULL,
                 _button_x_margin, cy - _button_x_margin - _button_height, 2 * _button_width, _button_height,
                 SWP_NOZORDER);

  DeferWindowPos(hdwp, m_CmdCancel.m_hWnd, NULL,
                 cx - (_button_x_margin + _button_width), cy - _button_x_margin - _button_height, _button_width, _button_height,
                 SWP_NOZORDER);

  DeferWindowPos(hdwp, m_CmdOK.m_hWnd, NULL,
                 cx - 2 * (_button_x_margin + _button_width), cy - _button_x_margin - _button_height, _button_width, _button_height,
                 SWP_NOZORDER);

  EndDeferWindowPos(hdwp);

}

void CVCEditor::OnSize(UINT nType, int cx, int cy)
{
  CDialog::OnSize(nType, cx, cy);

  DoSizeWindow();
}

void CVCEditor::OnGetMinMaxInfo(MINMAXINFO* lpMMI)
{
  CDialog::OnGetMinMaxInfo(lpMMI);

  if (m_dialogInitialized)
    {
      lpMMI->ptMinTrackSize.y = 3 * (_button_height + _button_y_margin + _editor_y_margin);
      lpMMI->ptMinTrackSize.x = 4 * (_button_width + _button_x_margin);
    }
}
