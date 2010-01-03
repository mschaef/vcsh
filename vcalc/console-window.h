/****************************************************************
 * console-window.h
 * Copyright 2003-2009, East Coast Toolworks, LLC
 *
 * The lisp console window.
 */

#ifndef __CONSOLE_WINDOW_H
#define __CONSOLE_WINDOW_H

#include "afxcmn.h"
#include "resource.h"

#include "../util/base-types.h"

#include "image.h"

/*****
 * ImageDataObject
 *
 *
 * Impementation for IDataObject Interface to be used in inserting bitmap to the
 * RichEdit Control.
 *
 * Author : Hani Atassi  (atassi@arabteam2000.com)
 *
 */

#if !defined(AFX_IMAGEDATAOBJECT_H__7E162227_62B8_49E3_A35B_FEC3F241A78F__INCLUDED_)
#define AFX_IMAGEDATAOBJECT_H__7E162227_62B8_49E3_A35B_FEC3F241A78F__INCLUDED_

class CImageDataObject : IDataObject
{
 public:
  static void InsertBitmap(IRichEditOle *pRichEditOle, HBITMAP hBitmap);

 private:
  ULONG m_ulRefCnt;
  BOOL  m_bRelease;

  // The data being passed to the richedit
  STGMEDIUM m_stgmed;
  FORMATETC m_format;

 public:
 CImageDataObject() : m_ulRefCnt(0)
    {
      m_bRelease = FALSE;
    }

  ~CImageDataObject()
    {
      if (m_bRelease)
        ::ReleaseStgMedium(&m_stgmed);
    }

  // Methods of the IUnknown interface
  STDMETHOD(QueryInterface)(REFIID iid, void ** ppvObject)
  {
    if (iid == IID_IUnknown || iid == IID_IDataObject)
      {
        *ppvObject = this;
        AddRef();

        return S_OK;
      }
    else
      return E_NOINTERFACE;
  }

  STDMETHOD_(ULONG, AddRef)(void)
    {
      m_ulRefCnt++;

      return m_ulRefCnt;
    }

  STDMETHOD_(ULONG, Release)(void)
    {
      if (--m_ulRefCnt == 0)
        {
          delete this;

          return 0;
        }

      return m_ulRefCnt;
    }

  // Methods of the IDataObject Interface
  //

  STDMETHOD(GetData)(FORMATETC *pformatetcIn, STGMEDIUM *pmedium) 
  {
    UNREFERENCED(pformatetcIn);

    HANDLE hDst;
    hDst = ::OleDuplicateData(m_stgmed.hBitmap, CF_BITMAP, NULL);

    if (hDst == NULL)
      return E_HANDLE;

    pmedium->tymed = TYMED_GDI;
    pmedium->hBitmap = (HBITMAP)hDst;
    pmedium->pUnkForRelease = NULL;

    return S_OK;
  }

  STDMETHOD(GetDataHere)(FORMATETC *pformatetc, STGMEDIUM *pmedium) 
  {
    UNREFERENCED(pformatetc);
    UNREFERENCED(pmedium);

    return E_NOTIMPL;
  }

  STDMETHOD(QueryGetData)(FORMATETC *pformatetc) 
  {
    UNREFERENCED(pformatetc);
    return E_NOTIMPL;
  }

  STDMETHOD(GetCanonicalFormatEtc)(FORMATETC *pformatetcIn, FORMATETC *pformatetcOut)
  {
    UNREFERENCED(pformatetcIn);
    UNREFERENCED(pformatetcOut);

    return E_NOTIMPL;
  }

  STDMETHOD(SetData)(FORMATETC *pformatetc, STGMEDIUM *pmedium, BOOL fRelease)
  {
    UNREFERENCED(fRelease);

    m_format = *pformatetc;
    m_stgmed = *pmedium;

    return S_OK;
  }

  STDMETHOD(EnumFormatEtc)(DWORD dwDirection, IEnumFORMATETC **ppenumFormatEtc) 
  {
    UNREFERENCED(dwDirection);
    UNREFERENCED(ppenumFormatEtc);

    return E_NOTIMPL;
  }

  STDMETHOD(DAdvise)(FORMATETC *pformatetc, DWORD advf, IAdviseSink *pAdvSink, DWORD *pdwConnection) 
  {
    UNREFERENCED(pformatetc);
    UNREFERENCED(advf);
    UNREFERENCED(pAdvSink);
    UNREFERENCED(pdwConnection);

    return E_NOTIMPL;
  }

  STDMETHOD(DUnadvise)(DWORD dwConnection) 
  {
    UNREFERENCED(dwConnection);

    return E_NOTIMPL;
  }

  STDMETHOD(EnumDAdvise)(IEnumSTATDATA **ppenumAdvice) 
  {
    UNREFERENCED(ppenumAdvice);

    return E_NOTIMPL;
  }

  // Some Other helper functions
  //
  void SetBitmap(HBITMAP hBitmap);
  IOleObject *GetOleObject(IOleClientSite *pOleClientSite, IStorage *pStorage);

};

#endif // !defined(AFX_IMAGEDATAOBJECT_H__7E162227_62B8_49E3_A35B_FEC3F241A78F__INCLUDED_)

// CLispConsole dialog

class CLispConsole : public CDialog
{
  DECLARE_DYNAMIC(CLispConsole)

    public:
  CLispConsole(CWnd* pParent = NULL);   // standard constructor
  virtual ~CLispConsole();

  // Dialog Data
  enum { IDD = IDD_LISP_CONSOLE };

  enum OutputClass {
    USER_TEXT,
    LISP_TEXT,
    ERROR_TEXT,
    OUTPUT_PORT_TEXT,
    ERROR_PORT_TEXT,
  };

  long m_OutputBufferEndPos;

  bool m_dialogInitialized;
  bool *m_dataReadyFlag;

	

 protected:
  virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support

  DECLARE_MESSAGE_MAP()
    public:
  CRichEditCtrl m_OutputPane;

  virtual BOOL OnInitDialog();
  afx_msg void OnOutputPaneMessage(NMHDR *pNMHDR, LRESULT *pResult);

 public:
  CString GetInputText();

  void InsertBitmap(CBitmap *bmp);
  void InsertDrawingSurface(vcalc_image_t *bmp);
  void InsertLispOutput(CString txt, OutputClass txtClass);
	
  afx_msg void OnSize(UINT nType, int cx, int cy);
  afx_msg void OnClose();
  afx_msg BOOL OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message);

  enum { EVENT_TIMER_ID = 0x123 };

  flonum_t m_event_time;
  void EventTimerElapsed();
  void SetEventTimer(flonum_t realtime);
  void CancelEventTimer();
  afx_msg void OnTimer(UINT nIDEvent);
};

#endif // _CONSOLE_WINDOW_H
