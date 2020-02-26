/*
 * system.c --
 *
 * Generic code for access to OS level services.
 *
 * (C) Copyright 2001-2014 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include <float.h>
#include <string.h>

#include <sys/stat.h>
#include <stdlib.h>

#ifdef OSX
#include <CommonCrypto/CommonDigest.h>
#endif

#include "scan-private.h"

lref_t lsystem(size_t argc, lref_t argv[])
{
     _TCHAR buf[STACK_STRBUF_LEN];

     lref_t command_line = lstring_append(argc, argv);

     int len = get_c_string(command_line, STACK_STRBUF_LEN, buf);

     if (len == 0)
          vmerror_arg_out_of_range(NIL, _T("empty command line"));

     if (len < 0)
          vmerror_arg_out_of_range(command_line, _T("command line length too long"));

     return fixcons(system(buf));
}

lref_t lenvironment()
{
     _TCHAR **env_strings = sys_get_env_vars();

     lref_t vars = NIL;

     for (size_t i = 0; env_strings[i] != NULL; i++) {
          lref_t var_name = NIL;
          lref_t var_value = NIL;

          _TCHAR *loc = env_strings[i];

          _TCHAR *current_var = loc;

          while (*loc && (*loc != '='))
               loc++;

          var_name = strconsbufn(loc - current_var, current_var);

          if (*loc) {
               loc++;

               _TCHAR *current_value = loc;

               while (*loc)
                    loc++;

               var_value = strconsbufn(loc - current_value, current_value);
          }


          vars = lcons(lcons(var_name, var_value), vars);

          loc++;
     }

     return vars;
}

lref_t lset_environment_variable(lref_t varname, lref_t value)
{
     if (!STRINGP(varname) && !SYMBOLP(varname))
          vmerror_wrong_type_n(1, varname);

     if (!STRINGP(value))
          vmerror_wrong_type_n(2, value);

     _TCHAR namebuf[STACK_STRBUF_LEN];
     _TCHAR valbuf[STACK_STRBUF_LEN];

     if(get_c_string(varname, STACK_STRBUF_LEN, namebuf) < 0)
          vmerror_arg_out_of_range(varname, _T("environment variable name too long"));

     if(get_c_string(value, STACK_STRBUF_LEN, valbuf) < 0)
          vmerror_arg_out_of_range(value, _T("environment variable value too long"));

     enum sys_retcode_t rc = sys_setenv(namebuf, valbuf);

     if (rc != SYS_OK)
          vmerror_io_error(_T("Error setting environment variable"), varname);

     return boolcons(true);
}

lref_t ltemporary_file_name(lref_t p)
{
     _TCHAR *prefix = _T("vcsh-temp");
     _TCHAR prefixbuf[STACK_STRBUF_LEN];

     if (!NULLP(p)) {
          if (get_c_string(p, STACK_STRBUF_LEN, prefixbuf) < 0) {
               vmerror_arg_out_of_range(p, _T("temporary filename prefix too long"));
          }

          prefix = prefixbuf;
     }

     _TCHAR buf[STACK_STRBUF_LEN];

     enum sys_retcode_t rc = sys_temporary_filename(prefix, buf, STACK_STRBUF_LEN);

     if (rc == SYS_OK)
          return strconsbuf(buf);

     vmerror_io_error(_T("Could not allocate temporary file name"), p);

     return NIL;
}

lref_t ldelete_file(lref_t filename)
{
     _TCHAR filenamebuf[STACK_STRBUF_LEN];

     if(get_c_string(filename, STACK_STRBUF_LEN, filenamebuf) < 0) {
          vmerror_arg_out_of_range(filename, _T("filename too long"));
     }

     enum sys_retcode_t rc = sys_delete_file(filenamebuf);

     if (rc == SYS_OK)
          return boolcons(true);

     vmerror_io_error(_T("Error deleting file"), filename);

     return NIL;
}

lref_t file_details_object(_TCHAR * filename, struct sys_stat_t * info)
{
     lref_t obj = hashcons(false, boolcons(false));

     lref_t file_type = NULL;

     switch (info->_filetype) {
     case SYS_FT_REG:
          file_type = keyword_intern(_T("file"));
          break;
     case SYS_FT_DIR:
          file_type = keyword_intern(_T("directory"));
          break;
     case SYS_FT_CHR:
          file_type = keyword_intern(_T("character-device"));
          break;
     case SYS_FT_BLK:
          file_type = keyword_intern(_T("block-device"));
          break;
     case SYS_FT_FIFO:
          file_type = keyword_intern(_T("fifo"));
          break;
     case SYS_FT_LNK:
          file_type = keyword_intern(_T("symbolic-link"));
          break;
     case SYS_FT_SOCK:
          file_type = keyword_intern(_T("socket"));
          break;

     case SYS_FT_UNKNOWN:
          /* fall-through */
     default:
          file_type = keyword_intern(_T("unknown"));
     };

     lhash_set(obj, keyword_intern(_T("file-type")), file_type);

     lref_t attrs = NIL;

     if (info->_attrs & SYS_FATTR_TEMPORARY)
          attrs = lcons(keyword_intern(_T("temporary")), attrs);
     if (info->_attrs & SYS_FATTR_ARCHIVE)
          attrs = lcons(keyword_intern(_T("archive")), attrs);
     if (info->_attrs & SYS_FATTR_OFFLINE)
          attrs = lcons(keyword_intern(_T("offline")), attrs);
     if (info->_attrs & SYS_FATTR_COMPRESSED)
          attrs = lcons(keyword_intern(_T("compressed")), attrs);
     if (info->_attrs & SYS_FATTR_ENCRYPTED)
          attrs = lcons(keyword_intern(_T("encrypted")), attrs);
     if (info->_attrs & SYS_FATTR_HIDDEN)
          attrs = lcons(keyword_intern(_T("hidden")), attrs);

     lhash_set(obj, keyword_intern(_T("attributes")), attrs);

     lhash_set(obj, keyword_intern(_T("mode")), fixcons(info->_mode & ~S_IFMT));

     lhash_set(obj, keyword_intern(_T("create-time")), fixcons(info->_ctime));
     lhash_set(obj, keyword_intern(_T("access-time")), fixcons(info->_atime));
     lhash_set(obj, keyword_intern(_T("write-time")), fixcons(info->_mtime));

     lhash_set(obj, keyword_intern(_T("size")), fixcons(info->_size));

     lhash_set(obj, keyword_intern(_T("filename")), strconsbuf(filename));

     return obj;
}

lref_t lifile_details(lref_t path, lref_t existance_onlyp)
{
     struct sys_stat_t file_info;
     _TCHAR pathbuf[STACK_STRBUF_LEN];

     if (!(NULLP(existance_onlyp) || BOOLP(existance_onlyp)))
          vmerror_wrong_type_n(2, existance_onlyp);

     if(get_c_string(path, STACK_STRBUF_LEN, pathbuf) < 0) {
          vmerror_arg_out_of_range(path, _T("path too long"));
     }

     /*  If stat fails, we assume the file does not exist and return false. */
     if (sys_stat(pathbuf, &file_info))
          return boolcons(false);

     if (!NULLP(existance_onlyp) && TRUEP(existance_onlyp))
          return boolcons(true);

     return file_details_object(pathbuf, &file_info);
}

lref_t lidirectory(lref_t dn, lref_t m)
{
     bool include_dirs = true;
     bool include_files = true;

     if (!STRINGP(dn))
          vmerror_wrong_type_n(1, dn);

     if (SYMBOLP(m)) {
          if (keyword_intern("directories") == m) {
               include_dirs = true;
               include_files = false;
          } else if (keyword_intern("files") == m) {
               include_dirs = false;
               include_files = true;
          } else if (keyword_intern("all") != m)
               vmerror_arg_out_of_range(m, _T(":directories, :files, or :all"));
     }
     else if (!NULLP(m))
          vmerror_wrong_type_n(2, m);

     _TCHAR dirname[STACK_STRBUF_LEN];
     if (get_c_string(dn, STACK_STRBUF_LEN, dirname) < 0)
          vmerror_arg_out_of_range(dn, _T("dirname string too long"));

     if (_tcslen(dirname) == 0)
          _tcsncpy(dirname, _T("./"), STACK_STRBUF_LEN);

     struct sys_dir_t *d;
     enum sys_retcode_t rc;

     if ((rc = sys_opendir(dirname, &d)) != SYS_OK) {
          vmerror_io_error(_T("Error opening directory"), dn);
          return NIL;
     }

     lref_t filenames = NULL;

     bool done = false;

     for (;;) {
          struct sys_dirent_t entry;

          rc = sys_readdir(d, &entry, &done);   /* rc preserved 'til end of loop */

          if (done)
               break;

          if (entry._type == SYS_FT_DIR) {
               if (!include_dirs)
                    continue;

               filenames = lcons(strconsbuf1(entry._name, _T('/')), filenames);

          } else {
               if (!include_files)
                    continue;

               filenames = lcons(strconsbuf(entry._name), filenames);
          }
     }

     enum sys_retcode_t cdrc = sys_closedir(d);

     if (rc != SYS_OK) {         /* rc from sys_readdir, to report errors from scan. */
          vmerror_io_error(_T("Error reading directory"), dn);
          return NIL;
     }

     if (cdrc != SYS_OK) {
          vmerror_io_error(_T("Error closing directory"), dn);
     }

     return filenames;
}


lref_t lsleep(lref_t msec)
{
     if (!NUMBERP(msec))
          vmerror_wrong_type_n(1, msec);

     sys_sleep(get_c_fixnum(msec));

     return NIL;
}

lref_t lruntime(void)
{
     return flocons(sys_runtime());
}

lref_t lrealtime(void)
{
     return flocons(sys_realtime());
}

lref_t lgc_runtime(void)
{
     return flocons(interp.gc_total_run_time);
}

lref_t lrealtime_time_zone_offset()
{
     return flocons(sys_timezone_offset());
}

lref_t lsystem_info()
{
     lref_t obj = hashcons(false, boolcons(false));

     lref_t eoln = boolcons(false);

     switch (sys_get_eoln_convention())
     {
     case SYS_EOLN_CRLF:
          eoln = keyword_intern(_T("crlf"));
          break;
     case SYS_EOLN_CR:
          eoln = keyword_intern(_T("cr"));
          break;
     case SYS_EOLN_LF:
          eoln = keyword_intern(_T("lf"));
          break;
     }

     lhash_set(obj, keyword_intern(_T("eoln-convention")), eoln);

     lhash_set(obj, keyword_intern(_T("fs-case-sensitive?")),
               boolcons(sys_get_fs_names_case_sensitive()));


     _TCHAR system_name[STACK_STRBUF_LEN];

     lref_t name = boolcons(false);

     if (sys_gethostname(system_name, STACK_STRBUF_LEN) == SYS_OK)
          name = strconsbuf(system_name);

     lhash_set(obj, keyword_intern(_T("system-name")), name);

     lref_t build_keyword = NIL;

     if (CHECKED_BUILD)
          build_keyword = keyword_intern(_T("checked"));
     else if (DEBUGGING_BUILD)
          build_keyword = keyword_intern(_T("debug"));
     else
          build_keyword = keyword_intern(_T("release"));

     lhash_set(obj, keyword_intern(_T("build-type")), build_keyword);

     lhash_set(obj, keyword_intern(_T("vm-build-id")), strconsbuf(scan_vm_build_id_string()));

     lhash_set(obj, keyword_intern(_T("platform-name")),
               keyword_intern(sys_get_platform_name()));

     lhash_set(obj, keyword_intern(_T("most-positive-flonum")), flocons(FLONUM_MAX));
     lhash_set(obj, keyword_intern(_T("most-negative-flonum")), flocons(FLONUM_MIN));
     lhash_set(obj, keyword_intern(_T("flonum-epsilon")), flocons(FLONUM_EPSILON));
     lhash_set(obj, keyword_intern(_T("most-positive-fixnum")), fixcons(FIXNUM_MAX));
     lhash_set(obj, keyword_intern(_T("most-negative-fixnum")), fixcons(FIXNUM_MIN));
     lhash_set(obj, keyword_intern(_T("most-positive-character")), charcons(_TCHAR_MAX));

     lhash_set(obj, keyword_intern(_T("runtime-resolution")), flocons(1.0 / sys_time_resolution()));

     lhash_set(obj, keyword_intern(_T("maximum-name-length")), fixcons(SYS_NAME_MAX));
     lhash_set(obj, keyword_intern(_T("maximum-path-length")), fixcons(SYS_PATH_MAX));

     lhash_set(obj, keyword_intern(_T("size-of-lobject")), fixcons(sizeof(struct lobject_t)));
     lhash_set(obj, keyword_intern(_T("size-of-fixnum")), fixcons(sizeof(fixnum_t)));
     lhash_set(obj, keyword_intern(_T("size-of-flonum")), fixcons(sizeof(flonum_t)));
     lhash_set(obj, keyword_intern(_T("size-of-lref")), fixcons(sizeof(lref_t)));


     lhash_set(obj, keyword_intern(_T("heap-segment-size")),
               fixcons(interp.gc_heap_segment_size * sizeof(struct lobject_t)));
     lhash_set(obj, keyword_intern(_T("current-heap-segments")),
               fixcons(interp.gc_current_heap_segments));
     lhash_set(obj, keyword_intern(_T("maximum-heap-segments")),
               fixcons(interp.gc_max_heap_segments));
     lhash_set(obj, keyword_intern(_T("argument-buffer-len")), fixcons(ARG_BUF_LEN));
     lhash_set(obj, keyword_intern(_T("most-postive-character")), charcons(_TCHAR_MAX));

     lhash_set(obj, keyword_intern(_T("interpreter-state-size")), fixcons(sizeof(struct interpreter_t)));

     return obj;
}

#ifdef OSX
static void sha1_find_digest(FILE *infile, uint8_t *digest)
{
     CC_SHA1_CTX ctx;
     CC_SHA1_Init(&ctx);

     for(;;)
     {
          uint8_t buf[SHA1_BUF_SIZE];

          size_t bytes_read = fread(buf, 1, SHA1_BUF_SIZE, infile);

          if (bytes_read == 0)
               break;

          CC_SHA1_Update(&ctx, buf, bytes_read);
     }

     CC_SHA1_Final(digest, &ctx);
}

static _TCHAR hexchar(int i)
{
     if ((i >= 0) && (i <= 9))
          return _T('0') + i;
     else
          return _T('a') + (i - 10);
}

static lref_t sha1_encode_digest(uint8_t *digest)
{
     _TCHAR encoded_digest[CC_SHA1_DIGEST_LENGTH * 2];

     for(size_t ii = 0; ii < CC_SHA1_DIGEST_LENGTH; ii++) {
          encoded_digest[ii * 2 + 0] = hexchar((digest[ii] >> 4) & 0xF);
          encoded_digest[ii * 2 + 1] = hexchar((digest[ii] >> 0) & 0xF);
     }

     return strconsbufn(CC_SHA1_DIGEST_LENGTH * 2, encoded_digest);
}

lref_t lfile_sha1_digest(lref_t fn)
{
     if (!STRINGP(fn))
          vmerror_wrong_type_n(1, fn);

     _TCHAR *filename = get_c_string(fn);

     FILE *f = fopen(filename, "rb");

     if (f == NULL)
          vmerror_io_error(_T("cannot open file"), fn);

     uint8_t sha1_digest[CC_SHA1_DIGEST_LENGTH];

     sha1_find_digest(f, sha1_digest);

     fclose(f);

     return sha1_encode_digest(sha1_digest);
}
#endif


