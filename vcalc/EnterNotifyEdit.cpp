// EnterNotifyEdit.cpp : implementation file
//

#include "stdafx.h"
#include "vcalc.h"
#include "EnterNotifyEdit.h"
#include "KeyboardTranslation.h"

#include <scan.h>

// CEnterNotifyEdit

IMPLEMENT_DYNAMIC(CEnterNotifyEdit, CEdit)
CEnterNotifyEdit::CEnterNotifyEdit()
{
  _parent = NULL;
}

CEnterNotifyEdit::~CEnterNotifyEdit()
{
}


BEGIN_MESSAGE_MAP(CEnterNotifyEdit, CEdit)
END_MESSAGE_MAP()



/* CEnterNotifyEdit message handlers **************************/

/* The keyboard handling for edit controls is a lot less
 * wierd than it was in 1.0. :-)
 *
 * Basically, a Lisp closure is given the chance
 * to intercept each keystroke before the edit control
 * does its own processing. This lisp code takes the
 * same parameters (key-id key-char key-num) as the
 * main window's keypress callback. The return value
 * controls how the keystroke is processed:
 * 
 * #t - process the keystroke normally
 * #f - ignore the keystroke
 * <string> - Insert the string into the edit control at the caret
 */
BOOL CEnterNotifyEdit::PreTranslateMessage(MSG* pMsg)
{
    assert(pMsg->hwnd == GetSafeHwnd());

    if (pMsg->message == WM_KEYDOWN)
    {

    i32 key_id = current_keypress_key_id((u32)(pMsg->wParam));

      LRef key_handling = 
        _parent->SendInstanceMessage(sym_on_edit_keypress,
                                     listn(1, fixcons(current_keypress_key_id(key_id))));

        if (STRINGP(key_handling))
        {
            ReplaceSel(CString(get_c_string(key_handling)));
            return TRUE;
        }
        else if (FALSEP(key_handling))
        {
            return TRUE;
        }
    }

    return CEdit::PreTranslateMessage(pMsg);
}

void CEnterNotifyEdit::SetParent(ILispInstance *parent)
{
  _parent = parent;
}

