/* VCSystem.cpp
 *
 * VCalc System Interface Routines
 */

#include "stdafx.h"
#include "VCSystem.h"
#include "shlobj.h"
#include "shlwapi.h"
#include "lisp-window.h"

/* String Table ***********************************************/

#include "resource.h"
#include "str_keys.i"

/* system-string := <keyword> -> <string>/#f
 *
 * Input: String keyword names
 * Output: String (#f if the string was not found)
 *
 * Looks up the specified string in the system stringtable.
 * Strings are specified by keywords similar to the resource
 * constants used by the C++ side. The exact mapping is
 * specified in ids_exp.awk.
 */

LRef lsystem_string(LRef keyword)
{
	if (!lkeywordp(keyword)) 
          vmerror_wrong_type(1, keyword);

	_TCHAR *keyword_str = get_c_string(keyword);

	for(int i = 0; stringIDTable[i]._keyword != NULL; i++)
	{
		if (_tcscmp(keyword_str, stringIDTable[i]._keyword) == 0)
		{
			CString str;
			
			str.LoadString(stringIDTable[i]._resID);
			
			/* This little bit of grotesqueness is due to the
			 * fact that the scan code base does not have
			 * const declerations. */
			return strcons( const_cast<LPTSTR>(LPCTSTR(str)));
		}		
	}

	return boolcons(FALSE);
}

/* choose-file ************************************************/

/* choose-file <parent> <caption>
 * 
 * Input: <open_file?>
 * Output: <filename>/#f
 *
 * Displays a file-selection dialog box with the specified
 * caption. Returns #f if the user selected cancel.
 */

LRef lchoose_file(LRef parent,
				  LRef load_filep,
				  LRef filter_string,
				  LRef default_extension,
				  LRef default_filename)
{
  if (!WINDOWP(parent))
          vmerror_wrong_type(1, parent);

	bool loading_file = true;

	if (!NULLP(load_filep))
	{
		if (!BOOLP(load_filep)) 
                  vmerror_wrong_type(2, load_filep);

		loading_file = BOOLV(load_filep);
	}


	CString filter;
	filter.LoadString(IDS_CHOOSE_FILE_FILTER);

	if (!NULLP(filter_string))
	{
		if (!STRINGP(filter_string)) 
                  vmerror_wrong_type(3, filter_string);

		filter = get_c_string(filter_string);
	}

	CString extension("vcx");
	if (!NULLP(default_extension))
	{
		if (!STRINGP(default_extension)) 
                  vmerror_wrong_type(4, default_extension);

		extension = get_c_string(default_extension);
	}


	CString filename("*.vcx");
	if (!NULLP(default_filename))
	{
		if (!STRINGP(default_filename)) 
                  vmerror_wrong_type(5, default_filename);

		filename = get_c_string(default_filename);
	}
	

	CFileDialog dlg(loading_file,
					extension,
					filename,
					OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
					filter,
					WNDOB(parent));


	/* This little bit of grotesqueness is due to the
     * fact that the scan code base does not have
     * const declerations. */
	if (dlg.DoModal() == IDOK)
		return strcons( const_cast<LPTSTR>(LPCTSTR(dlg.GetPathName())));

	return boolcons(FALSE);
}


/* Beep *******************************************************/

/* beep
 * 
 * Input: 
 * Output:
 *
 * Beeps. (Hey, what did you expect??? :-) )
 */

LRef lbeep()
{
	MessageBeep(0);

	return boolcons(FALSE);
}

/* exit-application *******************************************/

/* exit-application 
 * 
 * Input: 
 * Output:
 *
 * Starts the application exit process.
 */

LRef lexit_application()
{
	AfxGetMainWnd()->PostMessage(WM_CLOSE);

	return boolcons(FALSE);
}


/***************************************************************
 * Message/Question functions
 */

/* message_1
 *
 * Implementation for lyes_or_nop and lmessage. Most of the
 * operation is described there. */
LRef message_1(LRef prompt, LRef title, bool questionp)
{
	CString title_string;
	
	title_string.LoadString(AFX_IDS_APP_TITLE);

	if (!STRINGP(prompt)) 
          vmerror_wrong_type(1, prompt);

	if (!NULLP(title))
	{
		if (!STRINGP(title))
			vmerror_wrong_type(2, title);
		else
		{
			title_string += ": ";
			title_string += get_c_string(title);
		}
	}

    int id = MessageBox(NULL,
						get_c_string(prompt),
						title_string, 
						questionp ? 
						    MB_YESNO | MB_TASKMODAL | MB_ICONQUESTION
						  : MB_OK | MB_ICONINFORMATION);
						         
	
	if (questionp)
		return boolcons(id == IDYES);
	else
		return boolcons(FALSE);
}


/*
 * yes-or-no? <prompt> [<title>] 
 *
 * Input: <prompt> - Prompt containing yes or no question
 *        <title> - Optional string specifying window title
 * Output: #t/#f - User's yes/no (#t/#f) answer
 *
 * Asks the user a yes or no question
 */

LRef lyes_or_nop(LRef prompt, LRef title)
{
	return message_1(prompt, title, TRUE);	
}

/*
 * message <prompt> [<title>] 
 *
 * Input: <prompt> - Prompt containing yes or no question
 *        <title> - Optional string specifying window title
 * Output: #f
 *
 * Displays a message to the user in the most convenient way.
 */

LRef lmessage(LRef prompt, LRef title)
{
	return message_1(prompt, title, FALSE);	
}

/**************************************************************
 * Clipboard access
 */

struct {
	UINT   _format;
	LPTSTR _name;
} stock_formats[] = {
	{ CF_TEXT            , _T("CF_TEXT")            },
	{ CF_BITMAP          , _T("CF_BITMAP")          },
	{ CF_METAFILEPICT    , _T("CF_METAFILEPICT")    },
	{ CF_SYLK            , _T("CF_SYLK")            },
	{ CF_DIF             , _T("CF_DIF")             },
	{ CF_TIFF            , _T("CF_TIFF")            },
	{ CF_OEMTEXT         , _T("CF_OEMTEXT")         },
	{ CF_DIB             , _T("CF_DIB")             },
	{ CF_PALETTE         , _T("CF_PALETTE")         },
	{ CF_PENDATA         , _T("CF_PENDATA")         },
	{ CF_RIFF            , _T("CF_RIFF")            },
	{ CF_WAVE            , _T("CF_WAVE")            },
	{ CF_UNICODETEXT     , _T("CF_UNICODETEXT")     },
	{ CF_ENHMETAFILE     , _T("CF_ENHMETAFILE")     },
	{ CF_HDROP           , _T("CF_HDROP")           },
	{ CF_LOCALE          , _T("CF_LOCALE")          },
	{ CF_DIBV5           , _T("CF_DIBV5")           },
	{ CF_OWNERDISPLAY    , _T("CF_OWNERDISPLAY")    },
	{ CF_DSPTEXT         , _T("CF_DSPTEXT")         },
	{ CF_DSPBITMAP       , _T("CF_DSPBITMAP")       },
	{ CF_DSPMETAFILEPICT , _T("CF_DSPMETAFILEPICT") },
	{ 0                  , NULL                     }
};

// !!! Add support for integer clipboard format 'names'
// !!! Add support for delayed rendering of clipboard data

LRef clipboard_format_name(UINT format)
{
	for(int i = 0; stock_formats[i]._name; i++)
		if (stock_formats[i]._format == format)
			return keyword_intern(stock_formats[i]._name);

	_TCHAR buf[STACK_STRBUF_LEN];

	GetClipboardFormatName(format, buf, STACK_STRBUF_LEN); // !!!!! Error checking

	return strcons(buf);
}

UINT named_clipboard_format(LRef n)
{
	_TCHAR *name = get_c_string(n);

	for(int i = 0; stock_formats[i]._name; i++)
		if (_tcsicmp(stock_formats[i]._name, name) == 0)
			return stock_formats[i]._format;

	return RegisterClipboardFormat(name);
}

LRef lget_clipboard_formats()
{
	CWnd *window = AfxGetMainWnd();

	if (!window->OpenClipboard()) 
		vmerror(_T("Error opening clipboard."), NIL);

	LRef formats = NIL;

	UINT format = EnumClipboardFormats(0);
	while (format)
	{
		formats = lcons(clipboard_format_name(format), formats);

		format = EnumClipboardFormats(format);
	}

	CloseClipboard();
	
	return formats;
}

LRef lset_clipboard_data(LRef args)
{ 
	LRef retval = boolcons(true);

	/* Parameter validation */
	for(LRef loc = args; CONSP(loc); loc = lcdr(lcdr(loc)))
	{
		if (named_clipboard_format(lcar(loc)) == 0)
			vmerror("Bad clipboard format", lcar(loc));

		if (!CONSP(lcdr(loc)))
			vmerror("Expected even number of arguments", args);

		if (!STRINGP(lcar(lcdr(loc)))) // !!!! Generalize to c-pointer
                  vmerror("Clipboard data must be of type string", lcar(lcdr(loc)));
	}


	/* The copy operation. */

	CWnd *window = AfxGetMainWnd();

	if (window->OpenClipboard()) 
	{
		EmptyClipboard(); 

		while(CONSP(args))
		{
                  LRef data = lcar(lcdr(args));
			UINT format = named_clipboard_format(lcar(args));

			size_t string_len = 0;
			_TCHAR *string = get_c_string_dim(data, string_len);

			HGLOBAL global_storage = GlobalAlloc(GMEM_MOVEABLE, string_len + 1);

			if (global_storage == NULL)
			{
				retval = boolcons(false);
				break;
			}

			_TCHAR *global_storage_ptr = (_TCHAR *)GlobalLock(global_storage); 
			memcpy(global_storage_ptr, string, string_len);
			global_storage_ptr[string_len] = 0;
			GlobalUnlock(global_storage); 

			SetClipboardData(format, (LPVOID)global_storage_ptr); 

			args = lcdr(lcdr(args));
		}

		CloseClipboard(); 
	}

	return retval; 
} 


LRef lget_clipboard_data(LRef format_names)
{
	LRef retval = boolcons(FALSE);

	CWnd *window = AfxGetMainWnd();

	while (!NULLP(format_names))
	{
		assert(CONSP(format_names));

		LRef format_name = CAR(format_names);
		format_names = CDR(format_names);

		UINT format = named_clipboard_format(format_name);

		if (!IsClipboardFormatAvailable(format))
			continue;

		if (!window->OpenClipboard())
			vmerror("Error opening clipboard", NIL);

		HGLOBAL global_storage = GetClipboardData(format); 
		if (global_storage != NULL) 
		{ 
			_TCHAR *global_storage_ptr = (_TCHAR *)GlobalLock(global_storage); 
			SIZE_T len = GlobalSize(global_storage);

			if (global_storage_ptr != NULL) 
			{ 
				retval = lcons(format_name, strcons(len, global_storage_ptr));
				GlobalUnlock(global_storage); 
			} 
		} 
		CloseClipboard();		

		if (!NULLP(retval))
			break;
	}

	return retval;
}

/*
 * get-user-file-path
 *
 * Input: <filename>
 * Output: <full-filename>/#f
 *
 * Given a filename, creates a fully qualified path to the 
 * file of the given name in the user's application
 * settings directory.  The default algorithm tries to
 * place files in the nominal "Application Data" directory,
 * and if that fails it tries "My Documents". In theory,
 * this failure should only happen on Windows 95 systems
 * that are running ancient versions of MSIE. If none of
 * this works, the function just returns FALSE. 
 */

LRef lget_user_file_path(LRef filename)
{
	if (!STRINGP(filename)) 
          vmerror_wrong_type(1, filename);

	TCHAR pathBuffer[MAX_PATH + 1];

	LPMALLOC shellMalloc;
	if (SHGetMalloc(&shellMalloc) != NOERROR)
	{
		assert("SHGetMalloc Failed");
		return boolcons(FALSE);
	}

	LPITEMIDLIST folderIDL;
	bool idlValid = FALSE;

	if (    (SHGetSpecialFolderLocation(0, CSIDL_LOCAL_APPDATA, &folderIDL) == NOERROR)
		|| 	(SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, &folderIDL) == NOERROR))
	{
		idlValid = TRUE;
	}

	LRef retval = boolcons(FALSE);

	if (idlValid)
	{
		if (SHGetPathFromIDList(folderIDL, pathBuffer))
		{
			strncat(pathBuffer, "\\", MAX_PATH + 1);
			strncat(pathBuffer, get_c_string(filename), MAX_PATH + 1);
			retval = strcons( pathBuffer);
		}

		shellMalloc->Free(folderIDL);
	}

	shellMalloc->Release();

	return retval;	
}

/* Initialize System Subrs ************************************/

