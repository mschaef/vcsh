/****************************************************************
 * VCChooseBox.h
 * Copyright 2003-2009, East Coast Toolworks, LLC
 *
 * A simple box for choosing an item from a list.
 */

#ifndef __VCCHOOSE_BOX
#define __VCCHOOSE_BOX

#include "../vm/scan.h"
#include "afxwin.h"

// CVChooseBox dialog

class CVChooseBox : public CDialog
{
  DECLARE_DYNAMIC(CVChooseBox)

    public:
  CVChooseBox(CWnd* pParent = NULL);   // standard constructor
  virtual ~CVChooseBox();

  // Dialog Data
  enum { IDD = IDD_CHOOSEBOX };

 protected:
  virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support

  scan::LRef m_chooseList;
  scan::LRef m_chosen;

  CListBox m_chooseListCtrl;
  CStatic m_chooseListDescCtrl;

  CString m_chooseListTitle;
  CString m_chooseListDesc;

  DECLARE_MESSAGE_MAP()
    public:
  virtual BOOL OnInitDialog();


  void SetChooseList(scan::LRef cl, _TCHAR *title = NULL, _TCHAR *desc = NULL);
  scan::LRef GetChosenItem();
	
  afx_msg void OnBnClickedOk();
};


#endif // __VCCHOOSE_BOX
