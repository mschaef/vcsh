/****************************************************************
 * image.cpp
 * Copyright 2003-2009, East Coast Toolworks, LLC
 *
 * A bitmap image surface object.
 */

#include "vcalc.h"
#include "image.h"

#include <windowsx.h>

using namespace scan;

LRef sym_bold = NIL;
LRef sym_italic = NIL;
LRef sym_strikethrough = NIL;
LRef sym_underline = NIL;

LRef sym_current_image = NIL;

// REVISIT - Imaging, 5 of 7: Refactor, Split Surface from Image
// REVISIT - Imaging, 6 of 7: Window drawing surface
// REVISIT - Imaging, 7 of 7: Rewrite vCalc window bitmap management in Scheme


#define CURRENT_IMAGE SYMBOL_VCELL(sym_current_image)
#define TO_SCALED_COLOR(unscaled) ((flonum_t)unscaled / 255.0)
#define TO_UNSCALED_COLOR(scaled) ((long)((flonum_t)scaled * 255.0))

/* Color Object */

external_meta_t color_meta =
  {
    _T("color"),

    NULL,
    NULL,
    NULL
  };

LRef lrgb_color(LRef r, LRef g, LRef b)
{
  if (!NUMBERP(r))
    vmerror_wrong_type(1, r);
  if (!NUMBERP(g)) 
    vmerror_wrong_type(1, g);
  if (!NUMBERP(b)) 
    vmerror_wrong_type(1, b);

  flonum_t red   = get_c_flonum(r);
  flonum_t green = get_c_flonum(g);
  flonum_t blue  = get_c_flonum(b);

  if ((red > 1.0) || (red < 0.0)) 
    vmerror("Red channel out of range [0.0, 1.0]", r);

  if ((green > 1.0) || (green < 0.0)) 
    vmerror("Green channel out of range [0.0, 1.0]", g);

  if ((blue > 1.0) || (blue < 0.0)) 
    vmerror("Blue channel out of range [0.0, 1.0]", b);

  COLORREF cref = RGB(TO_UNSCALED_COLOR(red),
                      TO_UNSCALED_COLOR(green),
                      TO_UNSCALED_COLOR(blue));

  return externalcons((void *)cref, NIL, &color_meta);
}

LRef lcolor_r(LRef color)
{
  if (!COLORP(color))
    vmerror_wrong_type(color);

  return flocons(TO_SCALED_COLOR(GetRValue(COLOR_COLORREF(color))));
}

LRef lcolor_g(LRef color)
{
  if (!COLORP(color))
    vmerror_wrong_type(color);

  return flocons(TO_SCALED_COLOR(GetGValue(COLOR_COLORREF(color))));
}

LRef lcolor_b(LRef color)
{
  if (!COLORP(color))
    vmerror_wrong_type(color);

  return flocons(TO_SCALED_COLOR(GetBValue(COLOR_COLORREF(color))));
}


/* Font Object */

void font_gc_mark(LRef obj)
{
  assert(FONTP(obj));
  
  ::DeleteObject(FONT_HFONT(obj));
  
  delete ((vcalc_font_t *)EXTERNAL_DATA(obj));

  SET_EXTERNAL_DATA(obj, NULL);
}

external_meta_t font_meta =
  {
    _T("font"),

    font_gc_mark,
    NULL,
    NULL
  };

LOGFONT get_win32_font(LRef obj);

LRef lfont(LRef desc)
{
  vcalc_font_t *info = new vcalc_font_t();

  assert(info != NULL);

  LRef new_font = externalcons(info, NIL, &font_meta);

  info->_lf    = get_win32_font(desc); 
  info->_hFont = ::CreateFontIndirect(&info->_lf);

  assert(info->_hFont != NULL);

  return new_font;
}

/* Font Data **************************************************
 *
 * A simple little parser for font data 
 *
 * A font is represented by a list of the following structure:
 *
 * ( <facename> <facesize> . ( <attribute> ... ))
 *
 * <facename> is a string containing the name of the face.
 *
 * <attribute> is one of the following symbols:
 *     bold, italic, strikethrough, underline
 *
 * If a list element is missing, then a default value is used.
 * The empty list is thus the default system font. 
 */

LOGFONT get_win32_font(LRef obj)
{	
  LOGFONT lf;
  memset(&lf, 0, sizeof(LOGFONT));

  /* REVISIT: Add default font information */

  do { // This begins a block we can break out of to abort the computation
    if (!CONSP(obj))
      vmerror("Invalid font specification", obj);

    if (NULLP(obj))
      break;
    else if (STRINGP(CAR(obj)))
      strncpy(lf.lfFaceName, get_c_string(CAR(obj)), LF_FACESIZE);
    else
      vmerror("Font facename needs to be a string", CAR(obj));

    obj = CDR(obj);

    if (NULLP(obj))
      break;
    else if (NUMBERP(CAR(obj)))
      lf.lfHeight = get_c_long(CAR(obj));
    else
      vmerror("Font size needs to be a number", CAR(obj));

    obj = CDR(obj);

    for(; CONSP(obj); obj = CDR(obj))
      {
        if (SYMBOLP(CAR(obj)))
          {
            if (CAR(obj) == sym_bold)
              lf.lfWeight = FW_BOLD;
            else if (CAR(obj) == sym_italic)
              lf.lfItalic = TRUE;
            else if (CAR(obj) == sym_strikethrough)
              lf.lfStrikeOut = TRUE;
            else if (CAR(obj) == sym_underline)
              lf.lfUnderline = TRUE;
            else
              vmerror("Unknown font attribute", obj);
          }
        else
          vmerror("Font attribute needs to be a symbol", obj);
      }

    lf.lfQuality = 5; // CLEARTYPE_QUALITY; TODO: Investigate fallback to ANTIALIASED

  } while(0); // All the breaks in the above block break past this

  return lf;
}

/*
 * A couple graphics utility functions 
 */

/* Create a new screen compatible CBitmap of the specified size. Returns
 * NULL on failure. */
static HBITMAP create_blank_screen_bitmap(long sx, long sy)
{
  HBITMAP hNewBitmap = NULL;

  HDC hScreenDC = ::GetDC(NULL);

  if (hScreenDC)
    {
      hNewBitmap = ::CreateCompatibleBitmap(hScreenDC, sx, sy);

      ::ReleaseDC(NULL, hScreenDC);
    }

  return hNewBitmap;
}

/* Copies an sx by sy sized region from the origin of src to the
 * origin of dest. */
static void copy_bitmap_over_bitmap(HBITMAP dest, HBITMAP src, long sx, long sy) // REVISIT: needs error checking
{

  HDC srcDC = ::CreateCompatibleDC(NULL);
  HDC destDC = ::CreateCompatibleDC(NULL);

  ::SaveDC(srcDC);
  ::SaveDC(destDC);

  ::SelectObject(srcDC, src);
  ::SelectObject(destDC, dest);

  ::BitBlt(destDC, 0, 0, sx, sy, srcDC, 0, 0, SRCCOPY);
				    
  ::RestoreDC(destDC, -1);
  ::RestoreDC(srcDC, -1);		

  ::DeleteDC(destDC);
  ::DeleteDC(srcDC);
}

/* TC_DRAW_SURFACE ********************************************
 *
 * This is a 'drawing surface'. A drawing surface is an offscreen
 * bitmap into which drawing calls can be made. Drawing surfaces
 * are implemented in a C++ class, VCalcImage, that wraps
 * the MFC CBitmap and CDC classes. VCalcImage it itself
 * wrapped by a series of functions that marshall calls between
 * the scheme interpreter and C++. C++ cllients are welcome
 * to use either interface, but the C++ API is encouraged.
 */


HDC vcalc_image_add_dc_ref(vcalc_image_t *img)
{
  assert(img->_bmp);

  if (img->_dc_refcount == 0)
    {
      img->_dc = ::CreateCompatibleDC(NULL);
      ::SaveDC(img->_dc);
      ::SelectObject(img->_dc, img->_bmp);
    }

  img->_dc_refcount++;

  return img->_dc;
}

void vcalc_image_release_dc_ref(vcalc_image_t *img)
{
  assert(img->_dc);
  assert(img->_dc_refcount > 0);

  img->_dc_refcount--;

  if (img->_dc_refcount == 0)
    vcalc_image_close_dc(img);
}

void vcalc_image_close_dc(vcalc_image_t *img)
{
  if (img->_dc == NULL)
    {
      assert(img->_dc_refcount == 0);

      return;
    }

  img->_dc_refcount = 0;

  ::RestoreDC(img->_dc, -1);

  if (img->_currentPen) 
    {
      ::DeletePen(img->_currentPen);
      img->_currentPen = NULL;
    }

  if (img->_currentBrush) 
    {
      ::DeleteBrush(img->_currentBrush);
      img->_currentBrush = NULL;
    }

  if (img->_currentFont) 
    {
      ::DeleteFont(img->_currentFont);
      img->_currentFont = NULL;
    }

  ::DeleteDC(img->_dc);
  img->_dc = NULL;
}


bool vcalc_image_set_size(vcalc_image_t *img, long sx, long sy, bool retain)
{
  assert(img->_dc == NULL);

  HBITMAP new_bitmap = create_blank_screen_bitmap(sx, sy);

  if (new_bitmap == NULL)
    return false;

  // At this point, we know we've successfully allocated the new bitmap.
  // What's left to do is to handle the retain flag. If the retain
  // flag is set and we have an existing bitmap, we copy the existing
  // bitmap into the new bitmap before discarding it.

  if (img->_bmp)
    {
      if (retain)
        {
          long ox = img->_width;
          long oy = img->_height;

          copy_bitmap_over_bitmap(new_bitmap, img->_bmp, (sx < ox) ? sx : ox, (sy < oy) ? sy : oy);
        }

      ::DeleteObject(img->_bmp);
    }

  img->_width = sx;
  img->_height = sy;
  img->_bmp = new_bitmap;

  return true;
}

void vcalc_image_get_size(vcalc_image_t *img, long &sx, long &sy)
{
  sx = img->_width;
  sy = img->_height;
}

HBITMAP vcalc_image_duplicate_hbitmap(vcalc_image_t *img)
{
  assert(img->_bmp);

  HBITMAP new_bitmap = create_blank_screen_bitmap(img->_width, img->_height);

  if (new_bitmap == NULL)
    return NULL;
	
  copy_bitmap_over_bitmap(new_bitmap, img->_bmp, img->_width, img->_height);

  return new_bitmap;
}

/**************************************************************
 * The Scheme API
 */


void get_c_point(LRef pt, flonum_t &x, flonum_t &y) // REVISIT: Some scalar points are treated as x+xi, some are sizes...
{
  if (COMPLEXP(pt)) 
    {
      x = (flonum_t)(CMPLXRE(pt));
      y = (flonum_t)(CMPLXIM(pt));
    } 
  else if (NUMBERP(pt))
    {
      x = y = get_c_flonum(pt);
    }
  else
    vmerror("Bad point [ ~a ], points must be specified as complex or real numbers.", pt);
}

void get_c_point(LRef pt, long &x, long &y) // REVISIT: Some scalar points are treated as x+xi, some are sizes...
{
  flonum_t fx, fy;

  get_c_point(pt, fx, fy);

  x = (long)round(fx);
  y = (long)round(fy);
}


LRef current_surface(LRef s, bool should_be_open /* = true */)
{
  LRef surf = CURRENT_IMAGE;

  if (!NULLP(s))
    surf = s;

  if (!IMAGEP(surf)) 
    vmerror("Expected a drawing surface", surf);

  if (should_be_open && (IMAGE_SURFACE(surf)->_dc == NULL))
    vmerror("drawing surface should be open", surf);

  return surf;
}

LRef lmake_image(LRef size)
{	
  long width, height;
  get_c_point(size, width, height);

  if (width <= 0)  
    vmerror("Image width must be >0", size);
  if (height <= 0) 
    vmerror("Image height must be >0", size);
	
  LRef surface = imagecons(width, height);

  if (NULLP(surface))
    vmerror("Error allocating image!", size);

  return surface;
}

LRef lmeasure_image(LRef s) 
{
  LRef surf = current_surface(s, false);

  long width, height;
  vcalc_image_get_size(IMAGE_SURFACE(surf), width, height);

  return cmplxcons(width, height);
}

LRef limage_open(LRef s)
{
  LRef surf = current_surface(s, false);

  vcalc_image_add_dc_ref(IMAGE_SURFACE(surf));

  return surf;
}

LRef limage_close(LRef s)
{
  LRef surf = current_surface(s);

  vcalc_image_release_dc_ref(IMAGE_SURFACE(surf));

  return surf;
}

/**************************************************************
 * The Scheme Object
 */

void image_gc_free(LRef obj)
{
  assert(IMAGEP(obj));

  vcalc_image_t *img = IMAGE_SURFACE(obj);

  vcalc_image_close_dc(img);

  ::DeleteObject(img->_bmp);
  img->_bmp = NULL;

  delete img;
}

LRef image_print_details(LRef obj, LRef port)
{
  assert(IMAGEP(obj));

  vcalc_image_t *img = IMAGE_SURFACE(obj);

  scwritef(" ~cdX~cd", port, img->_width, img->_height);

  return boolcons(true);
}

external_meta_t image_meta =
  {
    _T("image"),

    NULL,
    image_gc_free,
    image_print_details
  };

LRef imagecons(long width, long height)
{
  vcalc_image_t *image = new vcalc_image_t;

  image->_bmp               = NULL;
  image->_dc_refcount       = 0;
  image->_dc                = NULL;
  image->_foregroundColor   = RGB(0x00, 0x00, 0x00);
  image->_backgroundColor   = RGB(0x00, 0x00, 0x00);
  image->_currentPen        = NULL;
  image->_currentBrush      = NULL;
  image->_currentFont       = NULL;
  image->_width             = 0;
  image->_height            = 0;

  if (!image)
    return NIL;

  LRef new_image = externalcons(image, NIL, &image_meta);

  assert(new_image);

  if (vcalc_image_set_size(image,width,height))
    return new_image;

  return NIL; // The GC will pick up the residual object and the free thunk will delete the VCalcImge
}
