/****************************************************************
 * VCConfigDialog.h
 * Copyright 2003-2009, East Coast Toolworks, LLC
 *
 * The vCalc main configuration dialog.
 */

#ifndef __VCCONFIGDIALOG_H
#define __VCCONFIGDIALOG_H

#include "afxwin.h"
#include "IDCombo.h"
#include "scan.h"

// CVConfigDialog dialog

class CVConfigDialog : public CDialog
{
  DECLARE_DYNAMIC(CVConfigDialog)

    public:
  CVConfigDialog(CWnd* pParent = NULL);   // standard constructor
  virtual ~CVConfigDialog();

  // Dialog Data
  enum { IDD = IDD_CONFIG_DIALOG };

 protected:
  virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support

  DECLARE_MESSAGE_MAP()

    virtual BOOL OnInitDialog();
  virtual void OnOK();

  CIDCombo m_DefaultBaseCtrl;
  CIDCombo m_AngleModeCtrl;
  CIDCombo m_InterestModeCtrl;
  CIDCombo m_NumberStyleCtrl;
  CEdit m_NumberDigitsCtrl;
  CIDCombo m_NumberSeperatorCtrl;

  void UpdateDialogMembers();

  _TCHAR *m_DefaultBase;
  _TCHAR *m_AngleMode;
  _TCHAR *m_InterestMode;
  int   m_NumberDigits;
  _TCHAR *m_NumberStyle;
  _TCHAR *m_NumberSeperator;	

 public:

  bool SetOptionVector(scan::LRef config_vector);
  scan::LRef GetOptionVector();

  scan::LRef m_OptionChangedCallback;
	
 public:
  afx_msg void OnConfigOptionChanged();
  afx_msg void OnBnClickedRegister();
};

#endif // __VCCONFIG_DIALOG_H
