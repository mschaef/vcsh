
/*
 * system.cpp --
 *
 * Generic code for access to OS level services.
 *
 * (C) Copyright 2001-2011 East Coast Toolworks Inc.
 * (C) Portions Copyright 1988-1994 Paradigm Associates Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include <float.h>
#include <string.h>

#include <sys/stat.h>

#include <CommonCrypto/CommonDigest.h>

#include "scan-private.h"

BEGIN_NAMESPACE(scan)
lref_t lsystem(size_t argc, lref_t argv[])
{
     size_t len = 0;
     _TCHAR *command_line = get_c_string_dim(lstring_append(argc, argv), &len);

     if (len == 0)
          vmerror_arg_out_of_range(NIL, _T("empty command line"));

     return fixcons(system(command_line));
}

lref_t lenvironment()
{
     _TCHAR **env_strings = sys_get_env_vars();

     lref_t vars = NIL;

     for (size_t i = 0; env_strings[i] != NULL; i++)
     {
          lref_t var_name = NIL;
          lref_t var_value = NIL;

          _TCHAR *loc = env_strings[i];

          _TCHAR *current_var = loc;

          while (*loc && (*loc != '='))
               loc++;

          var_name = strcons(loc - current_var, current_var);

          if (*loc)
          {
               loc++;

               _TCHAR *current_value = loc;

               while (*loc)
                    loc++;

               var_value = strcons(loc - current_value, current_value);
          }


          vars = lcons(lcons(var_name, var_value), vars);

          loc++;
     }

     return vars;
}

lref_t lset_environment_variable(lref_t varname, lref_t value)
{
     if (!STRINGP(varname) && !SYMBOLP(varname))
          vmerror_wrong_type(1, varname);

     if (!STRINGP(value))
          vmerror_wrong_type(2, value);

     sys_retcode_t rc = sys_setenv(get_c_string(varname), get_c_string(value));

     if (rc == SYS_OK)
          return boolcons(true);

     vmerror_io_error(_T("Error setting environment variable"), varname);

     return NIL;
}

lref_t ltemporary_file_name(lref_t p)       /*  REVISIT: This is a generally bad way to create temp files */
{
     if (!(STRINGP(p) || NULLP(p)))
          vmerror_wrong_type(1, p);

     _TCHAR *prefix = NULLP(p) ? (_TCHAR *) "vcsh-temp" : get_c_string(p);

     _TCHAR buf[STACK_STRBUF_LEN];

     sys_retcode_t rc = sys_temporary_filename(prefix, buf, STACK_STRBUF_LEN);

     if (rc == SYS_OK)
          return strcons(buf);

     vmerror_io_error(_T("Could not allocate temporary file name"), p);

     return NIL;
}

lref_t ldelete_file(lref_t filename)
{
     if (!STRINGP(filename))
          vmerror_wrong_type(1, filename);

     sys_retcode_t rc = sys_delete_file(get_c_string(filename));

     if (rc == SYS_OK)
          return boolcons(true);

     vmerror_io_error(_T("Error deleting file"), filename);

     return NIL;

     /*  REVISIT: delete_file should detect directories and call RemoveDirectory */
}

lref_t file_details_object(_TCHAR * filename, struct sys_stat_t * info)
{
     lref_t obj = hashcons(true);

     lref_t file_type = NULL;

     switch (info->_filetype)
     {
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

     lhash_set(obj, keyword_intern(_T("filename")), strcons(filename));

     return obj;
}

lref_t lifile_details(lref_t path, lref_t existance_onlyp)
{
     if (!STRINGP(path))
          vmerror_wrong_type(1, path);

     if (!(NULLP(existance_onlyp) || BOOLP(existance_onlyp)))
          vmerror_wrong_type(2, existance_onlyp);

     sys_stat_t file_info;

     /*  If stat fails, we assume the file does not exist and return false. */
     if (sys_stat(get_c_string(path), &file_info))
          return boolcons(false);

     if (!NULLP(existance_onlyp) && TRUEP(existance_onlyp))
          return boolcons(true);

     return file_details_object(get_c_string(path), &file_info);
}

/*  REVISIT: directory should be able to take lists of filename specifiers in addition to single specifiers. */

lref_t lidirectory(lref_t dn, lref_t m)
{
     bool include_dirs = true;
     bool include_files = true;

     if (!STRINGP(dn))
          vmerror_wrong_type(1, dn);

     if (SYMBOLP(m))
     {
          if (keyword_intern("directories") == m)
          {
               include_dirs = true;
               include_files = false;
          }
          else if (keyword_intern("files") == m)
          {
               include_dirs = false;
               include_files = true;
          }
          else if (keyword_intern("all") != m)
               vmerror_arg_out_of_range(m, _T(":directories, :files, or :all"));
     }
     else if (!NULLP(m))
          vmerror_wrong_type(2, m);

     const _TCHAR *dirname = get_c_string(dn);

     if (_tcslen(dirname) == 0)
          dirname = _T("./");

     sys_dir_t *d;
     sys_retcode_t rc;

     if ((rc = sys_opendir(dirname, &d)) != SYS_OK)
     {
          vmerror_io_error(_T("Error opening directory"), dn);
          return NIL;
     }

     lref_t filenames = NULL;

     bool done = false;

     for (;;)
     {
          sys_dirent_t entry;

          rc = sys_readdir(d, &entry, &done);   /* rc preserved 'til end of loop */

          if (done)
               break;

          if (entry._type == SYS_FT_DIR)
          {
               if (!include_dirs)
                    continue;

               filenames = lcons(strcons(entry._name, _T('/')), filenames);

          }
          else
          {
               if (!include_files)
                    continue;

               filenames = lcons(strcons(entry._name), filenames);
          }
     }

     sys_retcode_t cdrc = sys_closedir(d);

     if (rc != SYS_OK)          /* rc from sys_readdir, to report errors from scan. */
     {
          vmerror_io_error(_T("Error reading directory"), dn);
          return NIL;
     }

     if (cdrc != SYS_OK)
     {
          vmerror_io_error(_T("Error closing directory"), dn);
     }

     return filenames;
}


lref_t lsleep(lref_t ms)
{
     if (!NUMBERP(ms))
          vmerror_wrong_type(1, ms);

     double msec = get_c_double(ms);

     sys_sleep((uintptr_t) msec);

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
     sys_info_t info;

     sys_get_info(&info);

     lref_t obj = hashcons(true);

     lref_t eoln = boolcons(false);

     switch (info._eoln)
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
               boolcons(info._fs_names_case_sensitive));


     _TCHAR system_name[STACK_STRBUF_LEN];

     lref_t name = boolcons(false);

     if (sys_gethostname(system_name, STACK_STRBUF_LEN) == SYS_OK)
          name = strcons(system_name);

     lhash_set(obj, keyword_intern(_T("system-name")), name);

     lref_t build_keyword = NIL;

     if (CHECKED_BUILD)
          build_keyword = keyword_intern(_T("checked"));
     else if (DEBUGGING_BUILD)
          build_keyword = keyword_intern(_T("debug"));
     else
          build_keyword = keyword_intern(_T("release"));

     lhash_set(obj, keyword_intern(_T("build-type")), build_keyword);

     lhash_set(obj, keyword_intern(_T("vm-build-id")), strcons(build_id_string()));

     lhash_set(obj, keyword_intern(_T("platform-name")), keyword_intern(info._platform_name));

     lhash_set(obj, keyword_intern(_T("most-positive-flonum")), flocons(FLONUM_MAX));
     lhash_set(obj, keyword_intern(_T("most-negative-flonum")), flocons(FLONUM_MIN));
     lhash_set(obj, keyword_intern(_T("flonum-epsilon")), flocons(FLONUM_EPSILON));
     lhash_set(obj, keyword_intern(_T("most-positive-fixnum")), fixcons(FIXNUM_MAX));
     lhash_set(obj, keyword_intern(_T("most-negative-fixnum")), fixcons(FIXNUM_MIN));
     lhash_set(obj, keyword_intern(_T("most-positive-character")), charcons(_TCHAR_MAX));

     lhash_set(obj, keyword_intern(_T("runtime-resolution")), flocons(1.0 / sys_time_resolution()));

     lhash_set(obj, keyword_intern(_T("maximum-name-length")), fixcons(SYS_NAME_MAX));
     lhash_set(obj, keyword_intern(_T("maximum-path-length")), fixcons(SYS_PATH_MAX));

     lhash_set(obj, keyword_intern(_T("size-of-lobject")), fixcons(sizeof(lobject_t)));
     lhash_set(obj, keyword_intern(_T("size-of-fixnum")), fixcons(sizeof(fixnum_t)));
     lhash_set(obj, keyword_intern(_T("size-of-flonum")), fixcons(sizeof(flonum_t)));
     lhash_set(obj, keyword_intern(_T("size-of-lref")), fixcons(sizeof(lref_t)));


     lhash_set(obj, keyword_intern(_T("heap-segment-size")),
               fixcons(interp.gc_heap_segment_size * sizeof(lobject_t)));
     lhash_set(obj, keyword_intern(_T("current-heap-segments")),
               fixcons(interp.gc_current_heap_segments));
     lhash_set(obj, keyword_intern(_T("maximum-heap-segments")),
               fixcons(interp.gc_max_heap_segments));
     lhash_set(obj, keyword_intern(_T("argument-buffer-len")), fixcons(ARG_BUF_LEN));
     lhash_set(obj, keyword_intern(_T("most-postive-character")), charcons(_TCHAR_MAX));

     lhash_set(obj, keyword_intern(_T("interpreter-state-size")), fixcons(sizeof(interpreter_t)));

     return obj;
}

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

static _TCHAR hexchar(int i) // REVISIT: standard way to do this?
{
     if ((i >= 0) && (i <= 9))
          return _T('0') + i;
     else
          return _T('a') + (i - 10);
}

static lref_t sha1_encode_digest(uint8_t *digest)
{
     _TCHAR encoded_digest[CC_SHA1_DIGEST_LENGTH * 2];

     for(size_t ii = 0; ii < CC_SHA1_DIGEST_LENGTH; ii++)
     {
          encoded_digest[ii * 2 + 0] = hexchar((digest[ii] >> 4) & 0xF);
          encoded_digest[ii * 2 + 1] = hexchar((digest[ii] >> 0) & 0xF);
     }

     return strcons(CC_SHA1_DIGEST_LENGTH * 2, encoded_digest);
}

lref_t lfile_sha1_digest(lref_t fn)
{
     if (!STRINGP(fn))
          vmerror_wrong_type(1, fn);

     _TCHAR *filename = get_c_string(fn);

     FILE *f = fopen(filename, "rb");

     if (f == NULL)
          vmerror_io_error(_T("cannot open file"), fn);

     uint8_t sha1_digest[CC_SHA1_DIGEST_LENGTH];

     sha1_find_digest(f, sha1_digest);

     fclose(f);

     return sha1_encode_digest(sha1_digest);
}

END_NAMESPACE
