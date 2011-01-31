/****************************************************************
 * KeyboardTranslation.cpp
 * April 15th, 2004
 *
 * These functions implement the logic used to map between keyboard
 * id's and key specifications. 
 *
 * Keys have several representations
 *
 * Name				Description
 * ---------------	---------------------------------------------------------------------
 * key-id			The Win32 VK + flags indicating shift state (uniquely ID's each key)
 *
 * key-char			The character associated with a key
 *
 * natural-number	The number normally printed on the face of the key
 *
 * key-name			key-name := key | ( keysym . ( modifier* ) )
 *					keysym := character | symbol
 *					modifier := :control | :shift | :alt | :hankuku
 *
 * 
 * The translations required are as follows:
 * - key-name->key-id
 * - key-id->key-name
 * - key-id->natural-number
 * - key-id->character
 */

#include "StdAfx.h"
#include "KeyboardTranslation.h"
#include "vcalc.h"

_TCHAR key_encodes_char(fixnum_t key_id)
{
  BYTE keystate[256];
  WORD buf[3];

  memset(keystate, 0, 256);

  memset(buf, 0, sizeof(buf));

  // Parse out the control bits, and build up the keystate
  if (key_id & KEY_ID_HANKAKU)
    keystate[VK_KANA]       = 0x80;  // REVISIT: Don't know if VK_KANA is right for HANKAKU or not

  if (key_id & KEY_ID_ALT)
    keystate[VK_MENU]      = 0x80;

  /* The control key effects character encoding of letters. We don't
   * want the keyspec to include characters like #\soh and #\newline
   *
   * if (key_id & KEY_ID_CTRL)
   *   keystate[VK_CONTROL]   = 0x80;
   */

  if (key_id & KEY_ID_SHIFT)
    keystate[VK_SHIFT]     = 0x80;

  // Strip off the shift flags
  key_id = key_id & KEY_ID_VK_FIELD;

  if ((ToAscii((UINT)key_id, MapVirtualKey((UINT)key_id, 0), keystate, buf, 0) == 1)
      && !isspace(buf[0])
      && !iscntrl(buf[0]))
    return (_TCHAR)buf[0];
  else
    return  _T('\0');
}

LRef lkey_name2key_id(LRef keyspec)
{
  LRef new_keysym;
  LRef loc;
  LRef remaining_keyspec;

  SHORT scancode;

  int i;

  u32 key_id  = 0;
  LRef keysym  = NIL; 
  bool shift   = FALSE;
  bool alt     = FALSE;
  bool ctrl    = FALSE;
  bool hankaku = FALSE;

  /* Keys defined as raw characters are kept as raw characters. */
  if (CHARP(keyspec))
    return keyspec;

  /* Parse out the keyspec */
  if (SYMBOLP(keyspec))
    {
      keysym = keyspec;
    }
  else if (CONSP(keyspec))
    {
      remaining_keyspec = keyspec;
        
      while (CONSP(remaining_keyspec))
        {
          new_keysym = NIL;
          loc = CAR(remaining_keyspec);

          if (CHARP(loc))
            {
              new_keysym = loc;
            }
          else if (SYMBOLP(loc))
            {
              if (EQ(loc, keyword_intern(_T("shift"))))
                shift = TRUE;
              else if (EQ(loc, keyword_intern(_T("control"))))
                ctrl = TRUE;
              else if (EQ(loc, keyword_intern(_T("alt"))))
                alt = TRUE;
              else if (EQ(loc, keyword_intern(_T("hankaku"))))
                hankaku = TRUE;
              else
                new_keysym = loc;
            }
          else
            vmerror_wrong_type(loc);

          if (!NULLP(new_keysym))
            {
              if (!NULLP(keysym))
                vmerror("multiply specified keysym", new_keysym);

              keysym = new_keysym;
            }

          remaining_keyspec = CDR(remaining_keyspec);
        }
    }
  else
    vmerror_wrong_type(keyspec);

  if (NULLP(keysym))
    vmerror("Incomplete key specification", keyspec);

  /* Make it canonical */

  if (CHARP(keysym))
    {
      scancode = VkKeyScan(CHARV(keysym));

      if (scancode == -1)
        vmerror("Cannot find matching key", keyspec);

        
      key_id = LOBYTE(scancode);

      /* Magic numbers from API documentation for VkKeyScan */
      if (HIBYTE(scancode) & 0x1) shift       = TRUE; 
      if (HIBYTE(scancode) & 0x2) ctrl        = TRUE;
      if (HIBYTE(scancode) & 0x4) alt         = TRUE;
      if (HIBYTE(scancode) & 0x8) hankaku     = TRUE;
    }
  else if (SYMBOLP(keysym))
    {
      for(i = 0; i < NUM_KEYS; i++)
        {
          if (EQ(sym_key[i], keysym))
            {
              key_id = i;
              break;
            }
        }

      if (i == NUM_KEYS)
        vmerror("unexpected keysym", keysym);
    }
  else
    vmerror_wrong_type(keysym);

  if (shift)      
    key_id |= KEY_ID_SHIFT;

  if (ctrl)       
    key_id |= KEY_ID_CTRL;

  if (alt)        
    key_id |= KEY_ID_ALT;

  if (hankaku)    
    key_id |= KEY_ID_HANKAKU;

  return fixcons(key_id);    
}


/* key-id->key-name
 * 
 * This is the inverse of the above transformation. It generates a
 * canonical keyspec from a key id. Canonical keyspecs always take
 * the form:
 *
 * ( :shift? :ctrl? :alt? :hankaku? keysym )
 */
LRef lkey_id2key_name(LRef id)
{
  LRef result = NIL;
	
  // If a long ID is passed, then strip everything but the short id
  if (CONSP(id)) 
    id = CAR(id);

  if (!FIXNUMP(id)) 
    vmerror_wrong_type(1, id);

  fixnum_t key_id = FIXNM(id);

  _TCHAR ch = key_encodes_char(key_id);

  if (ch != _T('\0'))
    {
      result = lcons(charcons(ch), NIL);
    }
  else
    {
      fixnum_t vkid = key_id & KEY_ID_VK_FIELD;

      if ((vkid < 0) || (vkid >= NUM_KEYS))
        vmerror("Invalid key id ~s, out of range", id);

      result = lcons(sym_key[vkid], NIL);
    }

  // Parse out the control bits, and start building the result list
  // REVISIT: Don't know if VK_KANA is right for HANKAKU or not
  if (key_id & KEY_ID_HANKAKU)
    result = lcons(keyword_intern(_T("hankaku")), result);

  if (key_id & KEY_ID_ALT)
    result = lcons(keyword_intern(_T("alt")), result);

  if (key_id & KEY_ID_CTRL)
    result = lcons(keyword_intern(_T("control")), result);

  if (key_id & KEY_ID_SHIFT)
    result = lcons(keyword_intern(_T("shift")), result);   

  return result;
}



LRef lkey_id2natural_number(LRef k)
{
  if (!FIXNUMP(k))
    vmerror_wrong_type(k);

  fixnum_t vkey = FIXNM(k) & KEY_ID_VK_FIELD;

  if ((vkey >= VK_F1) && (vkey <= VK_F24))
    return fixcons(vkey - VK_F1);
  else if ((vkey >= VK_NUMPAD0) && (vkey <= VK_NUMPAD9))
    return fixcons(vkey - VK_NUMPAD0);
  else if ((vkey >= _T('0')) && (vkey <= _T('9')))
    return fixcons(vkey - _T('0'));
	
  return boolcons(false);
}

LRef lkey_id2character(LRef k)
{

  if (CHARP(k))
    return k;

  if (!FIXNUMP(k))
    vmerror_wrong_type(k);

  fixnum_t key_id = FIXNM(k);
	
  /* Any modifiers other than shift preclude the key from 
   * encoding a character. */
  if ((key_id & ~(KEY_ID_VK_FIELD | KEY_ID_SHIFT)) != 0)
    return boolcons(false);

  _TCHAR ch = key_encodes_char(key_id);

  if (ch == _T('\0'))
    return boolcons(false);
  else
    return charcons(ch);
}

u32 make_key_id(UINT vk, bool control, bool shift, bool alt)
{
  u32 key_id = vk;

  if (control) key_id |= KEY_ID_CTRL;
  if (shift)   key_id |= KEY_ID_SHIFT;
  if (alt)     key_id |= KEY_ID_ALT;

  return key_id;
}



static i32 fixup_key_id(i32 key_id)
{
  u32 vkey = key_id & KEY_ID_VK_FIELD;
    
  // We do not process modifier keys directly
  if ((vkey == VK_SHIFT) || (vkey == VK_CONTROL) && (vkey == VK_MENU))
    return -1;


  // Fix up the numeric keypad's key_id's to match with the 
  // main numeric keys'.
  if ((vkey >= VK_NUMPAD0) && (vkey <= VK_NUMPAD9))
    key_id = _T('0') + (vkey - VK_NUMPAD0); // Fixup key_id to match the key id of 'normal' number keys

  return key_id;
}

i32 current_keypress_key_id(i32 vkey)
{
  return fixup_key_id(make_key_id(vkey,
                                  GetKeyState(VK_CONTROL) < 0,
                                  GetKeyState(VK_SHIFT) < 0,
                                  GetKeyState(VK_MENU) < 0));
}



