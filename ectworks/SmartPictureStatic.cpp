// SmartPictureStatic.cpp : implementation file
//

#include "stdafx.h"
#include <scan-assert.h>

#include "SmartPictureStatic.h"

// CSmartPictureStatic

IMPLEMENT_DYNAMIC(CSmartPictureStatic, CStatic)

CSmartPictureStatic::CSmartPictureStatic()
{
	for(int i = 0; i < SMART_PICTURE_BITMAP_COUNT; i++)
		bitmaps[i].bmp = 0;
}

CSmartPictureStatic::~CSmartPictureStatic()
{
}


BEGIN_MESSAGE_MAP(CSmartPictureStatic, CStatic)
	ON_WM_PAINT()
END_MESSAGE_MAP()

void CSmartPictureStatic::AddBitmap(CBitmap &bmp)
{
	BITMAP size;
	bmp.GetObject(sizeof(BITMAP), &size);

	for(int i = 0; i < SMART_PICTURE_BITMAP_COUNT; i++)
	{
		if (bitmaps[i].bmp == 0)
		{
			bitmaps[i].bmp = bmp;
			bitmaps[i].size = CSize(size.bmWidth, size.bmHeight);
			return;
		}
	}

	assert(!"Too many bitmaps in CSmartPictureStatic!");

	return;
}


void CSmartPictureStatic::OnPaint()
{	
	CPaintDC dc(this);

	CRect clientRect;
	int clientArea;

	int bitmapToDraw = -1;
	int currentAreaDifference = INT_MAX;

	GetClientRect(clientRect);

	clientArea = clientRect.Width() * clientRect.Height();

    for(int i = 0; i < SMART_PICTURE_BITMAP_COUNT; i++)
	{
		/* Skip empty bitmap list entries */
		if(!bitmaps[i].bmp)
			continue;

		/* Skip bitmaps that are too big */
		if (   (clientRect.Width() < bitmaps[i].size.cx)
			|| (clientRect.Height() < bitmaps[i].size.cy))
			continue;

		/* Look for the closest fitting bitmap, by area. */
		int bitmapArea = bitmaps[i].size.cx * bitmaps[i].size.cy;

		if (clientArea - bitmapArea < currentAreaDifference)
		{
			currentAreaDifference = clientArea - bitmapArea;
			bitmapToDraw = i;
		}
	}

	if (bitmapToDraw == -1)
	{
		assert(!"No matching bitmaps found for CSmartPictureStatic");
		return;
	}

    /* Display the bitmap centered in the control's client rect */
	int offsetFromLeft;
	int offsetFromTop;

    offsetFromLeft = (clientRect.Width() - bitmaps[bitmapToDraw].size.cx) / 2;
	offsetFromTop = (clientRect.Height() - bitmaps[bitmapToDraw].size.cy) / 2;

	{
		CDC bmpDC;
		bmpDC.CreateCompatibleDC(NULL);
		bmpDC.SaveDC();

		bmpDC.SelectObject(bitmaps[bitmapToDraw].bmp);

		dc.BitBlt(offsetFromLeft, offsetFromTop,
				bitmaps[bitmapToDraw].size.cx, bitmaps[bitmapToDraw].size.cy,
				&bmpDC,
				0, 0,
				SRCCOPY);

		bmpDC.RestoreDC(-1);
		bmpDC.DeleteDC();
	}
}
