/****************************************************************
 * KeyboardTranslation.h
 * April 15th, 2004
 *
 * These functions implement the logic used to map between keyboard
 * id's and key specifications.
 */

#ifndef __KEYBOARDTRANSLATION_H
#define __KEYBOARDTRANSLATION_H

#include "scan-types.h"
#include "scan.h"

using namespace scan;

_TCHAR key_encodes_char(fixnum_t key_id);
scan::LRef lkey_name2key_id(scan::LRef keyspec);
scan::LRef lkey_id2key_name(scan::LRef id);
scan::LRef lkey_id2natural_number(scan::LRef key_id);
scan::LRef lkey_id2character(scan::LRef key_id);

i32 current_keypress_key_id(i32 vkey);

#define KEY_ID_SHIFT         (0x10000000)
#define KEY_ID_CTRL          (0x20000000)
#define KEY_ID_ALT           (0x40000000)
#define KEY_ID_HANKAKU       (0x80000000)
#define KEY_ID_VK_FIELD      (0x00FFFFFF)

#endif // __KEYBOARDTRANSLATION_H
