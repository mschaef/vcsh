/****************************************************************
 * image.h
 * Copyright 2003-2006, East Coast Toolworks, LLC
 *
 * Graphics system core.
 */

#ifndef __IMAGE_H
#define __IMAGE_H

#include "../vm/scan.h"

extern scan::LRef sym_bold;
extern scan::LRef sym_italic;
extern scan::LRef sym_strikethrough;
extern scan::LRef sym_underline;

extern scan::LRef sym_current_image;

extern scan::external_meta_t color_meta;
extern scan::external_meta_t font_meta;
extern scan::external_meta_t image_meta;

enum { IMAGE_RESIZE_MIN_DELTA = 32 };

struct vcalc_image_t
{
  /* The bitmap that the drawing surface uses to
   * retain its contents. */
  HBITMAP _bmp;

  /* Open drawing surfaces retain device contexts, in which
   * the surface's bitmap has been selected. */
  int _dc_refcount;
  HDC  _dc;

  COLORREF _foregroundColor;
  COLORREF _backgroundColor;

  HPEN   _currentPen;
  HBRUSH _currentBrush;
  HFONT  _currentFont;

  long _width;
  long _height;
};

HDC vcalc_image_add_dc_ref(vcalc_image_t *img);
void vcalc_image_release_dc_ref(vcalc_image_t *img);
void vcalc_image_close_dc(vcalc_image_t *img);


bool vcalc_image_set_size(vcalc_image_t *img, long sx, long sy, bool retain = FALSE);	
void vcalc_image_get_size(vcalc_image_t *img, long &sx, long &sy);

HBITMAP vcalc_image_duplicate_hbitmap(vcalc_image_t *img);

scan::LRef imagecons(long width, long height);

/* Accessors */

inline bool COLORP(scan::LRef obj) 
{
  return EXTERNALP(obj) && (EXTERNAL_META(obj) == &color_meta);
}

inline COLORREF COLOR_COLORREF(scan::LRef obj)
{
  assert(COLORP(obj));

  return (COLORREF)EXTERNAL_DATA(obj);
}

struct vcalc_font_t
{
  HFONT _hFont;
  LOGFONT _lf;
};

inline bool FONTP(scan::LRef obj) // REVISIT: Is this used anywhere?
{
  return EXTERNALP(obj) && (EXTERNAL_META(obj) == &font_meta);
}

inline HFONT FONT_HFONT(scan::LRef obj)
{
  assert(FONTP(obj));

  return ((vcalc_font_t *)EXTERNAL_DATA(obj))->_hFont;
}

inline LOGFONT *FONT_LOGFONT(scan::LRef obj)
{
  assert(FONTP(obj));

  return &((vcalc_font_t *)EXTERNAL_DATA(obj))->_lf;
}

inline bool IMAGEP(scan::LRef obj) 
{
  return EXTERNALP(obj) && (EXTERNAL_META(obj) == &image_meta);
}

inline vcalc_image_t *IMAGE_SURFACE(scan::LRef obj)
{
  assert(IMAGEP(obj));

  return (vcalc_image_t *)EXTERNAL_DATA(obj);
}

inline HDC IMAGE_DC(scan::LRef obj) 
{
  assert(IMAGEP(obj));

  return IMAGE_SURFACE(obj)->_dc;
}


/* Graphics Primitives */


scan::LRef lfont(scan::LRef desc);

scan::LRef lmake_image(scan::LRef size);
scan::LRef lmeasure_image(scan::LRef s);
scan::LRef limage_open(scan::LRef s);
scan::LRef limage_close(scan::LRef s);

scan::LRef ldraw_line (scan::LRef begin, scan::LRef end, scan::LRef s);
scan::LRef ldraw_polyline(scan::LRef points, scan::LRef s);
scan::LRef ldraw_text (scan::LRef at, scan::LRef string, scan::LRef s);
scan::LRef ldraw_rectangle(scan::LRef ptA, scan::LRef ptB, scan::LRef s);
scan::LRef lfill_rectangle(scan::LRef ptA, scan::LRef ptB, scan::LRef s);
scan::LRef ldraw_gradient(scan::LRef begin, scan::LRef end, scan::LRef colors, scan::LRef s);
scan::LRef ldraw_ellipse(scan::LRef ptA, scan::LRef ptB, scan::LRef s);
scan::LRef lfill_ellipse(scan::LRef ptA, scan::LRef ptB, scan::LRef s);
scan::LRef ldraw_point(scan::LRef pts, scan::LRef s);
scan::LRef ldraw_image(scan::LRef source_image, scan::LRef origin, scan::LRef mode, scan::LRef s);
scan::LRef lcopy_image(scan::LRef source_image);
scan::LRef lsubimage(scan::LRef source_image, scan::LRef origin, scan::LRef sz);
scan::LRef lset_drawing_origin(scan::LRef pos, scan::LRef s);
scan::LRef lupdate_drawing_origin(scan::LRef delta, scan::LRef s);
scan::LRef lget_drawing_origin(scan::LRef s);
scan::LRef lset_foreground_color(scan::LRef color, scan::LRef s);
scan::LRef lset_background_color(scan::LRef color, scan::LRef s);
scan::LRef lset_font(scan::LRef font, scan::LRef s);
scan::LRef lpoint_scale(scan::LRef point, scan::LRef scale_mul, scan::LRef scale_div);
scan::LRef lmeasure_text(scan::LRef string, scan::LRef s);

scan::LRef ldraw_linear_text(scan::LRef at, scan::LRef l_text, scan::LRef wrap_limit, scan::LRef s);
scan::LRef lmeasure_linear_text(scan::LRef l_text, scan::LRef wrap_limit, scan::LRef s);

scan::LRef lrgb_color(scan::LRef r, scan::LRef g, scan::LRef b);
scan::LRef lcolor_r(scan::LRef color);
scan::LRef lcolor_g(scan::LRef color);
scan::LRef lcolor_b(scan::LRef color);

inline bool POINTP(scan::LRef x) { return scan::COMPLEXP(x) || scan::NUMBERP(x); }

LOGFONT get_win32_font(scan::LRef obj);
void get_c_point(scan::LRef pt, flonum_t &x, flonum_t &y);
void get_c_point(scan::LRef pt, long &x, long &y);
scan::LRef current_surface(scan::LRef s, bool should_be_open = true);

#endif // __IMAGE_H
