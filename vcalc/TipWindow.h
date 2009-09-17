/****************************************************************
 * TipWindow.h
 * Copyright 2003-2009, East Coast Toolworks, LLC
 *
 * The tip of the day window.
 */

#ifndef __TIPWINDOW_H
#define __TIPWINDOW_H

#include "afxwin.h"
#include "afxcmn.h"

// TipWindow dialog

class TipWindow : public CDialog
{
  DECLARE_DYNAMIC(TipWindow)

    public:
  TipWindow(CWnd* pParent = NULL);   // standard constructor
  virtual ~TipWindow();

  // Dialog Data
  enum { IDD = IDD_TIPWINDOW };

  int _currentTipID;
  BOOL _NoAutoTipDisplay;

 protected:
  CButton m_NoAutoTipDisplayCtrl;

  virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support

  DECLARE_MESSAGE_MAP()

    virtual BOOL OnInitDialog();

  afx_msg void OnBnClickedPrevioustip();
  afx_msg void OnBnClickedNexttip();

  void LoadTip();
 public:
  CStatic _TipText;
  afx_msg HBRUSH OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor);
};

#endif //__TIPWINDOW_H
