#pragma once


// CSmartPictureStatic

class CSmartPictureStatic : public CStatic
{
	DECLARE_DYNAMIC(CSmartPictureStatic)

public:
	CSmartPictureStatic();
	virtual ~CSmartPictureStatic();

protected:
	DECLARE_MESSAGE_MAP()

	enum { SMART_PICTURE_BITMAP_COUNT = 4 };

	struct BitmapInfo {
		HBITMAP bmp;
		CSize size;
	};

	BitmapInfo bitmaps[SMART_PICTURE_BITMAP_COUNT];

public:
	void AddBitmap(CBitmap &bmp);

	afx_msg void OnPaint();
};


