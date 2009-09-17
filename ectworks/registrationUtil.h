#pragma once

CString bfe(CString& in, CString& key);
CString bfd(CString& in, CString& key);

//CString getResource(const CString& path, int id);
CString getCrc(const CString &data);
CString getVolumeSerialNumber(const CString& vol);

int getCRCFile(const CString &filename, DWORD dwSize);

extern CString _this;