/****************************************************************
 * EnterNotifyEdit.h
 * Copyright 2003-2009, East Coast Toolworks, LLC
 *
 * An edit control that delegates to a Lisp procedure on certain
 * events
 */

#ifndef __ENTERNOTIFYYEDIT_H
#define __ENTERNOTIFYYEDIT_H

#include "scan.h"

#include "vcalc.h"

class CEnterNotifyEdit : public CEdit
{
  DECLARE_DYNAMIC(CEnterNotifyEdit)

  public:
  CEnterNotifyEdit();
  virtual ~CEnterNotifyEdit();

protected:
  DECLARE_MESSAGE_MAP()
    public:
  virtual BOOL PreTranslateMessage(MSG* pMsg);

  void SetParent(ILispInstance *parent);

  ILispInstance *_parent;
};


#endif //__ENTERNOTIFYYEDIT_H
