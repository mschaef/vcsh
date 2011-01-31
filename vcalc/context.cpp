/****************************************************************
 * context.cpp
 * Copyright 2003-2009, East Coast Toolworks, LLC
 *
 * A drawing context.
 */

#include "vcalc.h"
#include "image.h"

#include <windowsx.h>

using namespace scan;

LRef ldraw_line(LRef begin, LRef end, LRef s)
{
  LRef surf = current_surface(s);

  long x, y, x2, y2;
  get_c_point(begin, x, y);
  get_c_point(end, x2, y2);

  HDC dc = IMAGE_DC(surf);

  ::MoveToEx(dc, x, y, NULL);
  ::LineTo(dc, x2, y2);
	
  return boolcons(TRUE);
}

LRef ldraw_polyline(LRef points, LRef s)
{
  LRef surf = current_surface(s);
  HDC dc = IMAGE_DC(surf);

  bool first = true;

  while (!NULLP(points))
    {
      if (!CONSP(points)) 
        vmerror("Bad list of points: ~s", points);
		
      long x, y;

      if (FALSEP(CAR(points))) // #f signals a segment break, a discontinuity in the polyline
        first = true;
      else
        {
          get_c_point(CAR(points), x, y);

          if (first)
            ::MoveToEx(dc, x, y, NULL);
          else
            ::LineTo(dc, x, y);

          first = false;
        }
		

      points = CDR(points);
    }
	
  return boolcons(TRUE);
}


static LRef draw_rectangle_1(LRef ptA, LRef ptB, LRef s, bool fill)
{
  HBRUSH oldBrush = NULL;

  LRef surf = current_surface(s);
  HDC dc = IMAGE_DC(surf);

  long x, y, x2, y2;

  get_c_point(ptA, x, y);
  get_c_point(ptB, x2, y2);

  if (!fill) 
    oldBrush = (HBRUSH)::SelectObject(dc, ::GetStockObject(HOLLOW_BRUSH));

  ::Rectangle(dc, x, y, x2, y2);

  if (!fill) 
    ::SelectObject(dc, oldBrush);

  return boolcons(TRUE);
}

LRef ldraw_rectangle(LRef ptA, LRef ptB, LRef s) { return draw_rectangle_1(ptA, ptB, s, false); }
LRef lfill_rectangle(LRef ptA, LRef ptB, LRef s) { return draw_rectangle_1(ptA, ptB, s, true); }

LRef ldraw_gradient(LRef begin, LRef end, LRef colors, LRef s)
{
  LRef surf = current_surface(s);

  long x, y, x2, y2;
  get_c_point(begin, x, y);
  get_c_point(end, x2, y2);

  LRef lfrom = lcar(colors);
  LRef lto = lcar(lcdr(colors));

  if (!COLORP(lfrom)) 
    vmerror_wrong_type(lfrom);
  if (!COLORP(lto)) 
    vmerror_wrong_type(lto);

  COLORREF from = COLOR_COLORREF(lfrom);
  COLORREF to   = COLOR_COLORREF(lto);
    
  TRIVERTEX vertices[2];

  vertices[0].x     = x;
  vertices[0].y     = y;
  vertices[0].Red   = GetRValue(from) << 8;
  vertices[0].Green = GetGValue(from) << 8;
  vertices[0].Blue  = GetBValue(from) << 8;
  vertices[0].Alpha = 0;

  vertices[1].x     = x2;
  vertices[1].y     = y2;
  vertices[1].Red   = GetRValue(to) << 8;
  vertices[1].Green = GetGValue(to) << 8;
  vertices[1].Blue  = GetBValue(to) << 8;
  vertices[1].Alpha = 0;

  GRADIENT_RECT mesh[1];

  mesh[0].UpperLeft  = 0;
  mesh[0].LowerRight = 1;

  HDC dc = IMAGE_DC(surf);

  ::GradientFill(dc, vertices, 2, mesh, 1, TRUEP(lcar(lcdr(lcdr(colors)))) ? GRADIENT_FILL_RECT_H : GRADIENT_FILL_RECT_V);

  return boolcons(TRUE);
}

static LRef draw_ellipse_1(LRef ptA, LRef ptB, LRef s, bool fill)
{
  HBRUSH oldBrush = NULL;

  LRef surf = current_surface(s);
  HDC dc = IMAGE_DC(surf);

  long x, y, x2, y2;
  get_c_point(ptA, x, y);
  get_c_point(ptB, x2, y2);

  if (!fill) 
    oldBrush = (HBRUSH)::SelectObject(dc, ::GetStockObject(HOLLOW_BRUSH));

  ::Ellipse(dc, x, y, x2, y2);

  if (!fill) 
    ::SelectObject(dc, oldBrush);

  return boolcons(TRUE);
}

LRef ldraw_ellipse(LRef ptA, LRef ptB, LRef s) { return draw_ellipse_1(ptA, ptB, s, false); }
LRef lfill_ellipse(LRef ptA, LRef ptB, LRef s) { return draw_ellipse_1(ptA, ptB, s, true ); }

LRef ldraw_point(LRef pts, LRef s)
{
  LRef surf = current_surface(s);
  HDC dc = IMAGE_DC(surf);

  while(!NULLP(pts))
    {
      long x, y;

      if (POINTP(pts))
        {
          get_c_point(pts, x, y);
          pts = NIL;
        }
      else if (CONSP(pts))
        {
          LRef point = lcar(pts);

          if (!POINTP(point))
            break; // Error case, pts is now the list starting from the bad point

          get_c_point(point, x, y);

          pts = lcdr(pts);
        }
      else
        break; // Error case, pts is now the bad point

      ::SetPixel(dc, x, y, IMAGE_SURFACE(surf)->_foregroundColor);
    }

  if (!NULLP(pts)) 
    vmerror_wrong_type(1, pts);

  return boolcons(true);
}

LRef ldraw_image(LRef source_image, LRef origin, LRef mode, LRef s)
{
  LRef dest_image = current_surface(s, false);

  if (!IMAGEP(source_image)) 
    vmerror("Expected a image", source_image);

  DWORD blt_mode = SRCCOPY;
  double alpha = -1.0; // No alpha by default

  if(!NULLP(mode))
    {
      if (FALSEP(mode))
        blt_mode = BLACKNESS;
      else if (BOOLP(mode))
        blt_mode = WHITENESS;
      else if (FLONUMP(mode))
        {
          alpha = FLONM(mode);

          if ((alpha < 0.0) || (alpha > 1.0))
            vmerror(_T("Alpha out of range, should be in [0.0, 1.0]"), mode);
        }
      else if (EQ(mode, keyword_intern(_T("and"))))
        blt_mode = SRCAND;
      else if (EQ(mode, keyword_intern(_T("invert"))))
        blt_mode = SRCINVERT;
      else if (EQ(mode, keyword_intern(_T("paint"))))
        blt_mode = SRCPAINT;
      else if (EQ(mode, keyword_intern(_T("erase"))))
        blt_mode = SRCERASE;
      else if (EQ(mode, keyword_intern(_T("not-draw"))))
        blt_mode = NOTSRCCOPY;
      else if (EQ(mode, keyword_intern(_T("not-erase"))))
        blt_mode = NOTSRCERASE;
      else 
        vmerror(_T("Invalid image drawing mode, expected :and, :invert, paint, :erase, :not-draw, :not-erase, #t, #f, (), or [0.0,1.0]"), mode);
    }


  long originX, originY;
  get_c_point(origin, originX, originY);


  long dest_width, dest_height;
  vcalc_image_get_size(IMAGE_SURFACE(dest_image), dest_width, dest_height);

  long source_width, source_height;
  vcalc_image_get_size(IMAGE_SURFACE(source_image), source_width, source_height);

  HDC destDC = vcalc_image_add_dc_ref(IMAGE_SURFACE(dest_image));
  HDC srcDC  = vcalc_image_add_dc_ref(IMAGE_SURFACE(source_image));
	
  if (alpha < 0.0)
    ::BitBlt(destDC, originX, originY, dest_width, dest_height, srcDC, 0, 0, blt_mode);
  else
    {
      BLENDFUNCTION bf;

      memset(&bf, 0, sizeof(BLENDFUNCTION));
      bf.SourceConstantAlpha	= (BYTE)(alpha * 255.0);
      bf.BlendOp				= AC_SRC_OVER;
      bf.AlphaFormat			= 0; // No Alpha Channel

      ::AlphaBlend(destDC, originX, originY, dest_width, dest_height, 
                   srcDC, 0, 0, source_width, source_height,
                   bf);
    }


  vcalc_image_release_dc_ref(IMAGE_SURFACE(source_image));
  vcalc_image_release_dc_ref(IMAGE_SURFACE(dest_image));

  return dest_image;
}

LRef lcopy_image(LRef source_image)
{
  if (!IMAGEP(source_image)) 
    vmerror_wrong_type(1, source_image);

  long width, height;
  vcalc_image_get_size(IMAGE_SURFACE(source_image), width, height);

  LRef new_image = imagecons(width, height);

  if (!NULLP(new_image))
    {
      HDC destDC = vcalc_image_add_dc_ref(IMAGE_SURFACE(new_image));
      HDC srcDC  = vcalc_image_add_dc_ref(IMAGE_SURFACE(source_image));
		
      ::BitBlt(destDC, 0, 0, width, height, srcDC, 0, 0, SRCCOPY);

      vcalc_image_release_dc_ref(IMAGE_SURFACE(source_image));
      vcalc_image_release_dc_ref(IMAGE_SURFACE(new_image));
    }

  return new_image;    
}

LRef lsubimage(LRef source_image, LRef origin, LRef sz)
{

  if (!IMAGEP(source_image)) 
    vmerror_wrong_type(1, source_image);

  long width, height;
  get_c_point(sz, width, height);

  long originX, originY;
  get_c_point(origin, originX, originY);

  LRef new_image = imagecons(width, height);

  if (!NULLP(new_image))
    {
      HDC destDC = vcalc_image_add_dc_ref(IMAGE_SURFACE(new_image));
      HDC srcDC  = vcalc_image_add_dc_ref(IMAGE_SURFACE(source_image));
		
      ::BitBlt(destDC, 0, 0, width, height, srcDC, originX, originY, SRCCOPY);

      vcalc_image_release_dc_ref(IMAGE_SURFACE(source_image));
      vcalc_image_release_dc_ref(IMAGE_SURFACE(new_image));
    }

  return new_image;
}


LRef ldraw_text(LRef at, LRef string, LRef s)
{
  LRef surf = current_surface(s);

  long x, y;
  get_c_point(at, x, y);
	
  if (!STRINGP(string)) 
    vmerror_wrong_type(3, string);

  HDC dc = IMAGE_DC(surf);

  TEXTMETRIC metrics;
  int baselineAdjust = 0;

  if (::GetTextMetrics(dc, &metrics))
    baselineAdjust = metrics.tmDescent;

  long yc = y;

  yc += baselineAdjust;	
	
  int oldBkMode = ::SetBkMode(dc, TRANSPARENT);

  size_t len = 0;
  _TCHAR *str  = get_c_string_dim(string, len);

  ::TextOut(dc, x, yc, str, (int)len);
  ::SetBkMode(dc, oldBkMode);

  return fixcons(baselineAdjust);
}



LRef lset_drawing_origin (LRef pos, LRef s)
{
  LRef surf = current_surface(s);

  long x, y;
  get_c_point(pos, x, y);

  HDC dc = IMAGE_DC(surf);

  LRef oldOrigin = lget_drawing_origin(s);

  ::SetWindowOrgEx(dc, -x, -y, NULL);

  return oldOrigin;
}

LRef lupdate_drawing_origin (LRef delta, LRef s)
{
  long dx, dy;
  get_c_point(delta, dx, dy);

  LRef origin = lget_drawing_origin(s); // TODO: This was marked with '!!!!!!!!'. Why?
  long xo = get_c_long(lcar (origin));
  long yo = get_c_long(lcar(lcdr(origin)));

  return lset_drawing_origin(cmplxcons(xo + dx, yo + dy), s);
}

LRef lget_drawing_origin(LRef s)
{
  LRef surf = current_surface(s);

  HDC dc = IMAGE_DC(surf);

  POINT pt;

  ::GetWindowOrgEx(dc, &pt);

  return lcons(fixcons(pt.x), fixcons(pt.y));
}

LRef lset_foreground_color(LRef c, LRef s)
{	
  LRef surf = current_surface(s);

  if (!COLORP(c)) 
    vmerror_wrong_type(1, c);

  COLORREF color = COLOR_COLORREF(c);

  IMAGE_SURFACE(surf)->_foregroundColor = color;

  HPEN newPen = ::CreatePen(PS_SOLID, 0, color);

  if (newPen)
    {
      HDC dc = IMAGE_DC(surf);

      ::SetTextColor(dc, color);
      HPEN oldPen = (HPEN)::SelectObject(dc, newPen);
        
      if (IMAGE_SURFACE(surf)->_currentPen)
        {
          assert(oldPen == IMAGE_SURFACE(surf)->_currentPen);

          DeletePen(IMAGE_SURFACE(surf)->_currentPen);
        }

      IMAGE_SURFACE(surf)->_currentPen = newPen;
    }
  else
    vmerror("Failure to set the foreground color", NIL);

  return NIL;    
}

LRef lset_background_color(LRef color, LRef s)
{
  LRef surf = current_surface(s);

  if (!COLORP(color)) 
    vmerror_wrong_type(1, color);

  HBRUSH newBrush = ::CreateSolidBrush(COLOR_COLORREF(color));

  IMAGE_SURFACE(surf)->_backgroundColor = COLOR_COLORREF(color);

  if (newBrush)
    {
      HDC dc = IMAGE_DC(surf);

      HBRUSH oldBrush = (HBRUSH)::SelectObject(dc, newBrush);

      if (IMAGE_SURFACE(surf)->_currentBrush)
        {
          assert(oldBrush == IMAGE_SURFACE(surf)->_currentBrush);

          DeleteBrush(IMAGE_SURFACE(surf)->_currentBrush);
        }

      IMAGE_SURFACE(surf)->_currentBrush = newBrush;
    }
  else
    vmerror("Failure to set the background color", NIL);
	
	

  return NIL;
}

LRef lset_font(LRef font, LRef s)
{
  LOGFONT lf;

  LRef surf = current_surface(s);

  lf = get_win32_font(font);

  HFONT newFont = ::CreateFontIndirect(&lf);

  HDC dc = IMAGE_DC(surf);

  if (newFont)
    {
      HFONT oldFont = (HFONT)::SelectObject(dc, newFont);

      if (IMAGE_SURFACE(surf)->_currentFont)
        {
          assert(oldFont == IMAGE_SURFACE(surf)->_currentFont);

          DeleteObject(IMAGE_SURFACE(surf)->_currentFont);
        }

      IMAGE_SURFACE(surf)->_currentFont = newFont;
    }
  else
    vmerror("Failure to set the font", NIL);

  return NIL;
}

LRef lpoint_scale(LRef point, LRef scale_mul, LRef scale_div)
{
  flonum_t x, y;
  get_c_point(point, x, y);

  if (!NULLP(scale_mul))
    {
      flonum_t scale_x, scale_y;
      get_c_point(scale_mul, scale_x, scale_y);

      x *= scale_x;
      y *= scale_y;
    }

  if (!NULLP(scale_div))
    {
      flonum_t scale_div_x, scale_div_y;
      get_c_point(scale_div, scale_div_x, scale_div_y);

      x /= scale_div_x;
      y /= scale_div_y;
    }


  return cmplxcons(x, y);
}

LRef lmeasure_text(LRef string, LRef s)
{
  LRef surf = current_surface(s);

  if (!STRINGP(string))  
    vmerror_wrong_type(1, string);

  HDC dc = IMAGE_DC(surf);

  TEXTMETRIC metrics;
  long ba = 0;

  if (::GetTextMetrics(dc, &metrics))
    ba = metrics.tmDescent;

  size_t len = 0;
  _TCHAR *str = get_c_string_dim(string, len);
  SIZE extent;

  ::GetTextExtentPoint32(dc, str, (int)len, &extent);

  return listn(3, fixcons(extent.cx), fixcons(extent.cy), fixcons(ba));
}


/* Linear text inner loop functions ***************************
 * 
 * These functions are functions translated from Scheme in the
 * name of efficiency.
 */

LRef ldraw_linear_text(LRef at, LRef l_text,  LRef wrap_limit, LRef s)
{
  LRef surf = current_surface(s);

  long x, y;
  get_c_point(at, x, y);

  if (!CONSP(l_text)) 
    vmerror_wrong_type(3, l_text);

  long xofs = x;
  long xc = 0;
  long yc = y;

  long wl = LONG_MAX;

  if (FIXNUMP(wrap_limit))
    wl = (long)get_c_fixnum(wrap_limit);
  else if (!NULLP(wrap_limit))
    vmerror_wrong_type(4, wrap_limit);

  for(; CONSP(l_text); l_text = CDR(l_text))
    {
      LRef t = CAR(l_text);

      lset_font(CAR(t), surf);

      if (!STRINGP(CDR(t)))
        vmerror_wrong_type(CDR(t));

      LRef measurements = lmeasure_text(CDR(t), surf);
      long sx = get_c_long(lcar  (measurements));
      long sy = get_c_long(lcar(lcdr(measurements)));

      // Baseline adjustment currently unused 
      //
      // long ba = get_c_long(lcar(lcdr(lcdr(measurements))));

      // If a single block is larger than the h_extent, we allow
      // it to be rendered on a line by itself.
      if (!(   ((xc == 0) && (sx > wl)) 
               || (xc + sx < wl)))
        break;


      ldraw_text(cmplxcons(xofs + xc, yc - sy), CDR(t), surf);

      xc += sx;
    }

  return boolcons(FALSE);
}

LRef lmeasure_linear_text(LRef l_text, LRef wrap_limit, LRef s)
{
  long h_extent = 0;
  long v_extent = 0;
  long baseline_adj = 0;

  long wl = LONG_MAX;

  if (FIXNUMP(wrap_limit))
    wl = (long)get_c_fixnum(wrap_limit);
  else if (!NULLP(wrap_limit))
    vmerror_wrong_type(2, wrap_limit);

  LRef surf = current_surface(s);

  if (!CONSP(l_text))  
    vmerror_wrong_type(1, l_text);

  for(; CONSP(l_text); l_text = CDR(l_text))
    {
      LRef t = CAR(l_text);

      lset_font(CAR(t), surf);

      if (!STRINGP(CDR(t))) 
        vmerror_wrong_type(CDR(t));

      LRef measurements = lmeasure_text(CDR(t), surf);
      long sx = get_c_long(lcar  (measurements));
      long sy = get_c_long(lcar (lcdr(measurements)));
      long ba = get_c_long(lcar(lcdr(lcdr(measurements))));

      // If a single block is larger than the h_extent, we allow
      // it to b e rendered on a line by itself.
      if (!(   ((h_extent == 0) && (sx > wl)) 
               || (h_extent + sx < wl)))
        break;

      h_extent += sx;

      if (sy > v_extent) v_extent = sy;
      if (ba > baseline_adj) baseline_adj = ba;
    }

  return listn(4, l_text,
               fixcons(h_extent),
               fixcons(v_extent),
               fixcons(baseline_adj));
}


