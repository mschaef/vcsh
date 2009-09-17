/****************************************************************
 * VCEditor.h
 * Copyright 2003-2009, East Coast Toolworks, LLC
 *
 * A pop up editor window.
 */

#ifndef __VCEDITOR_H
#define __VCEDITOR_H

#pragma once
#include "afxcmn.h"
#include "afxwin.h"


// CVCEditor dialog

class CVCEditor : public CDialog
{
  DECLARE_DYNAMIC(CVCEditor)

    public:
  CVCEditor(CWnd* pParent, scan::LRef syntax_checker);   // standard constructor
  virtual ~CVCEditor();

  // Dialog Dat
  enum { IDD = IDD_VCALC_EDITOR };

  void SetEditText(CString &editText);
  CString GetEditText();

 protected:
  virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support

  DECLARE_MESSAGE_MAP()

    virtual void OnOK();
  virtual BOOL OnInitDialog();
  afx_msg void OnBnClickedReformatAndCheckSyntax();
  afx_msg void OnSize(UINT nType, int cx, int cy);

  void DoSizeWindow();

 public:
  CRichEditCtrl m_Editor;
  CButton m_ReformatAndCheckSyntax;
  CButton m_CmdOK;
  CButton m_CmdCancel;

 protected:
  int _editor_y_margin;
  int _button_x_margin;
  int _button_y_margin;
  int _button_height;
  int _button_width;

  BOOL m_dialogInitialized;
  CString _editText;
  scan::LRef _syntax_checker;
  bool CheckSyntax(bool reformat_also = false);

 public:
  afx_msg void OnGetMinMaxInfo(MINMAXINFO* lpMMI);
};

#endif // __VCEDITOR_H
