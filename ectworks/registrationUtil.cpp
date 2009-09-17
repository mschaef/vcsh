#include "stdafx.h"
// MAS Modified
//#include "util.h"
// End MAS Modified
#include "registrationUtil.h"


ULONG crc32_table[256]; // Lookup table array

ULONG reflect(ULONG ref, char ch)
{// Used only by Init_CRC32_Table()

	ULONG value(0);

	// Swap bit 0 for bit 7
	// bit 1 for bit 6, etc.
	for(int i = 1; i < (ch + 1); i++)
	{
		if(ref & 1)
			value |= 1 << (ch - i);
		ref >>= 1;
	}
	return value;
}

void initCrcTable()
{// Called by OnInitDialog()

	// This is the official polynomial used by CRC-32 
	// in PKZip, WinZip and Ethernet. 
	ULONG ulPolynomial = 0x04c11db7;

	// 256 values representing ASCII character codes.
	for(int i = 0; i <= 0xFF; i++)
	{
		crc32_table[i]=reflect(i, 8) << 24;
		for (int j = 0; j < 8; j++)
			crc32_table[i] = (crc32_table[i] << 1) ^ (crc32_table[i] & (1 << 31) ? ulPolynomial : 0);
		crc32_table[i] = reflect(crc32_table[i], 32);
	}
}


// MAS Modified
//#include <fstream.h>
#include <fstream>
using namespace std;
// End MAS Modified



// This function uses the crc32_table lookup table
// to generate a CRC for csData
int getCRCFile(const CString &path, DWORD dwSize)
{
	// Be sure to use unsigned variables,
	// because negative values introduce high bits
	// where zero bits are required.

	initCrcTable();

	ULONG  crc(0xffffffff);
	int len;
	unsigned char* buffer;

	len = dwSize;
	// Save the text in the buffer.
	// Perform the algorithm on each character
	// in the string, using the lookup table values.

	if(!path.GetLength())
		return 0;

	fstream f;

	// MAS Modified
	//f.open(path,ios::in|ios::nocreate,0);//filebuf::sh_read|filebuf::sh_write);
	// ios:nocreate is no longer supported, and is now the default
	f.open(path,ios::in);	
	// End MAS Modified

	unsigned char buf[1000];
	memset(buf,0,sizeof(TCHAR)*1000);

	while(f.good())
	{
		// MAS Modified
		//f.read(buf,1000);
		f.read((char *)buf,1000);
		// End MAS Modified

		len=1000;
		if(!len)
			break;

		buffer=buf;

		while(len--)
			crc = (crc >> 8) ^ crc32_table[(crc & 0xFF) ^ *buffer++];

		dwSize-=len;

		if(dwSize<=0)
			break;
	}

	f.close();

	// Exclusive OR the result with the beginning value.
	return crc^0xffffffff;
}


// This function uses the crc32_table lookup table
// to generate a CRC for csData
int getCRC(const CString &csData, DWORD dwSize)
{
	// Be sure to use unsigned variables,
	// because negative values introduce high bits
	// where zero bits are required.

	initCrcTable();

	ULONG  crc(0xffffffff);
	int len;
	unsigned char* buffer;

	len = dwSize;
	// Save the text in the buffer.
	buffer = (unsigned char*)(LPCTSTR)csData;
	// Perform the algorithm on each character
	// in the string, using the lookup table values.
	while(len--)
		crc = (crc >> 8) ^ crc32_table[(crc & 0xFF) ^ *buffer++];
	// Exclusive OR the result with the beginning value.
	return crc^0xffffffff;
}

CString getCrc(const CString &data)
{
	initCrcTable();

	int nCRC = getCRC(data, data.GetLength());
	// Convert the returned integer into a character string.
	char ch[20];
	itoa(nCRC, ch, 16);  // Note that the integer is a 16 bit hex
	// Send the CRC string to the dialog.
	return ch;//m_csCRCtext = ch;
	// If the file path is likely to be too long to display shorten it.
//	int nMax = 60;
//	if(m_csFileName.GetLength() > nMax)
//		m_csFileName = m_csFileName.Left(3) + "..." + m_csFileName.Right(nMax -6);
}

//////////////////////////////////////////////////////////

CString getVolumeSerialNumber(const CString& vol)
{
	TCHAR name[255];
	TCHAR fstype[255];
	unsigned long sn,cl,fs;
	sn=cl=fs=0;

	BOOL found=GetVolumeInformation(
		LPCTSTR(vol),						// root directory
		name,        // volume name buffer
		255,            // length of name buffer
		&sn,     // volume serial number
		&cl, // maximum file name length
		&fs,        // file system options
		fstype,    // file system name buffer
		255         // length of file system name buffer
	);
	CString snStr;
	snStr.Format(_T("%x"),sn);

//	getResource(_T("fd.dll"),ID_SN)

	return snStr;
}


