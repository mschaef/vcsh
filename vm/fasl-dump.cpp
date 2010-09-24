/* fasl-dump.cpp
 * Jume 26th, 2007
 *
 * A utility for dumping the contents of a FASL file in readable form.
 */

#include "scan.h"

/*  REVISIT: It would be nice if this emitted something directly loadable via the scheme reader */

/*  REVISIT: There are a few places where size_t's are printed with %08zx, which truncates on */
/*  64-bit sizes. I never want to have to use fasl-dump on a >4GB file, but it is not strictly */
/*  correct to truncate. */

BEGIN_NAMESPACE(scan)

enum
{
     MAX_READER_DEFINITIONS = 200000, /*  REVISIT: MAX_READER_DEFINITIONS should be dynamic */
     STRBUF_SIZE = 256,
};

fasl_opcode_t g_reader_definition_ops[MAX_READER_DEFINITIONS];
fixnum_t g_reader_definition_fixnums[MAX_READER_DEFINITIONS];

FILE *g_file = NULL;
size_t g_current_ofs = 0;
size_t g_nesting_level = 0;

#define FIXNUM_OP_P(op)     \
  ((op == FASL_OP_FIX8)     \
   || (op == FASL_OP_FIX16) \
   || (op == FASL_OP_FIX32) \
   || (op == FASL_OP_FIX64))

void newline()
{
  printf("\n");
}

void indent()
{
  for(size_t ii = 0; ii < g_nesting_level - 1; ii++)
    printf(" ");
}

size_t last_definition_offset = 0;

void show_opcode(size_t offset, fasl_opcode_t opcode, const _TCHAR *desc)
{
  const _TCHAR *opcode_name = fasl_opcode_name(opcode);

  newline();

  if ((opcode == FASL_OP_LOADER_DEFINEQ) || (opcode == FASL_OP_LOADER_DEFINEA0))
    {
      last_definition_offset = offset;
      newline();
    }

  printf(" 0x%08zx (D+0x%08zx) ", offset, offset - last_definition_offset);
  indent();

  if (desc)
    printf(" %s=", desc);

  if (opcode_name)
    printf("%s", opcode_name);
  else
    printf(_T("<INVALID>"));

  printf(":");
}

size_t read_binary(void *buf, size_t size, size_t count, size_t *ofs = NULL)
{
  size_t blocks_read;

  if (ofs != NULL)
    *ofs = g_current_ofs;

  blocks_read = fread(buf, size, count, g_file);

  g_current_ofs += (size * blocks_read);

  return blocks_read;
}

bool read_binary_fixnum(fixnum_t length, bool signedp, fixnum_t &result, size_t *ofs = NULL)
{
#ifdef FIXNUM_64BIT
  assert ((length == 1) || (length == 2) || (length == 4) || (length == 8));
#else
  assert ((length == 1) || (length == 2) || (length == 4));
#endif

  u8_t bytes[sizeof(fixnum_t)];
  size_t fixnums_read = read_binary(bytes, (size_t)length, 1, ofs);

  if (!fixnums_read)
    return false;

  switch(length)
    {
    case 1: result = (signedp ? (fixnum_t)(*(i8_t *)bytes) : (fixnum_t)(*(u8_t  *)bytes)); break;
    case 2: result = (signedp ? (fixnum_t)(*(i16_t *)bytes) : (fixnum_t)(*(u16_t *)bytes)); break;
    case 4: result = (signedp ? (fixnum_t)(*(i32_t *)bytes) : (fixnum_t)(*(u32_t *)bytes)); break;
#ifdef FIXNUM_64BIT
    case 8: result = (signedp ? (fixnum_t)(*(i64_t *)bytes) : (fixnum_t)(*(u64_t *)bytes)); break;
#endif
    }

  return true;
}

bool read_binary_flonum(flonum_t &result)
{
  u8_t bytes[sizeof(flonum_t)];
  size_t flonums_read = read_binary(bytes, sizeof(flonum_t), 1);

  if (!flonums_read)
    return false;

  result = *(flonum_t *)bytes;

  return true;
}


/* This code depends on using an output paramater, rather than a
 * function return code. For self-referential data structures, it's
 * essential that the reader definition table be updated before
 * the definition is referenced. However, if a return paramater
 * was used, the definition table wouldn't be updated until the
 o * object read was complete. The use of an output paramater
 * allows the FASL_OP_READER_xDEFINITION opcode to arrange for
 * its recursive call to dump to update the definition table
 * prior to returning. This means that embedded instances of
 * FASL_OP_READER_REFERENCE can refer to the object itself. This
 * has two implications for you:
 *
 * 1. Do not change this code to use ordinary function return values.
 * 2. When writing code that reads a composite object, be sure
 *    to update the return value prior to reading any component objects.
 */

static fasl_opcode_t dump_next_object(const _TCHAR *desc = NULL, fixnum_t *fixnum_value = NULL);

static void dump_error(const _TCHAR *message)
{
  _TCHAR buf[STACK_STRBUF_LEN];
  _sntprintf(buf, STACK_STRBUF_LEN, "Error Reading FASL File: %s @ 0x%08zx.", message, g_current_ofs);

  panic(buf);
}

static fasl_opcode_t fast_read_opcode(size_t *ofs = NULL)
{
  fixnum_t opcode = FASL_OP_EOF;

  if (read_binary_fixnum(1, false, opcode, ofs))
    return (fasl_opcode_t)opcode;
  else
    return FASL_OP_EOF;
}

static void dump_list(bool read_listd)
{
  fixnum_t length;
  _TCHAR buf[STRBUF_SIZE];

  fasl_opcode_t op = dump_next_object(_T("length"), &length);

  if (!FIXNUM_OP_P(op))
    dump_error("lists must have a fixnum length");

  for(fixnum_t ii = 0; ii < length; ii++) {
      _sntprintf(buf, STRBUF_SIZE, _T("item["FIXNUM_PRINTF_PREFIX "i]"), ii);

    op = dump_next_object(buf);

    if (op == FASL_OP_EOF)
      dump_error("incomplete list definition");
  }

  if (read_listd) {
    op = dump_next_object(_T("item[cdr]"));

    if (op == FASL_OP_EOF)
      dump_error("incomplete list definition, missing cdr");
  }
}

static void dump_character()
{
  fixnum_t data = 0;

  if (read_binary_fixnum(1, false, data))
    {
      putchar((int)data);
    }
  else
    dump_error("EOF while reading character");
}


static void dump_fixnum(size_t length, fixnum_t *fixnum_value = NULL)
{
  fixnum_t buf;

  if (!read_binary_fixnum(length, true, buf))
    dump_error("expected fixnum not found");

  if (fixnum_value != NULL)
    *fixnum_value = buf;

  _tprintf(_T(FIXNUM_PRINTF_PREFIX "i"), buf);
}

static void dump_flonum(bool complexp)
{
  flonum_t real_part = 0.0;
  flonum_t imag_part = 0.0;

  if (read_binary_flonum(real_part))
    {
      if (complexp)
	{
	  if (read_binary_flonum(imag_part))
	    printf("%f+%fi", real_part, imag_part);
	  else
	    dump_error("EOF during complex number");
	}
      else
	printf("%f", real_part);
    }
  else
    dump_error("EOF during flonum");
}


static void dump_string()
{
  fixnum_t length;

  fasl_opcode_t op = dump_next_object(_T("length"), &length);

  if (!FIXNUM_OP_P(op))
    dump_error("strings must have a fixnum length");

  printf(" \"");

  _TCHAR ch = _T('\0');

  for(fixnum_t ii = 0; ii < length; ii++) {

    if (read_binary(&ch, sizeof(_TCHAR), 1) == 0)
      dump_error("EOF during string data");

    putchar(ch);
  }

  printf("\"");
}

static void dump_package()
{
  fasl_opcode_t op = dump_next_object(_T("package-name"));

  if (op != FASL_OP_STRING)
    dump_error("packages must have string names");
}

static void dump_symbol()
{
  fasl_opcode_t op = dump_next_object(_T("pname"));

  if (op != FASL_OP_STRING)
    dump_error("symbols must have string print names");

  op = dump_next_object(_T("package"));

  if ((op != FASL_OP_PACKAGE) && (op != FASL_OP_NIL) && (op != FASL_OP_FALSE))
    dump_error("a symbol must either have a package or NIL for home");
}

static void dump_subr()
{
  fasl_opcode_t op = dump_next_object(_T("name"));

  if (op != FASL_OP_STRING)
    dump_error("subrs must have string print names");
}

static void dump_vector()
{
  fixnum_t length;
  _TCHAR buf[STRBUF_SIZE];

  fasl_opcode_t op = dump_next_object(_T("length"), &length);

  if (!FIXNUM_OP_P(op))
    dump_error("Expected fixnum for vector length");

  for(fixnum_t ii = 0; ii < length; ii++)
    {
      _sntprintf(buf, STRBUF_SIZE, _T("item["FIXNUM_PRINTF_PREFIX "i]"), ii);

      op = dump_next_object(buf);

      if (op == FASL_OP_EOF)
	dump_error("incomplete vector definition");
    }
}

static void dump_instance_map()
{
  fasl_opcode_t op = dump_next_object(_T("proto"));

  if ((op != FASL_OP_FALSE) && (op != FASL_OP_INSTANCE) && (op != FASL_OP_SYMBOL))
    dump_error("malformed proto in instance map");

  op = dump_next_object(_T("slot-names"));

  if ((op != FASL_OP_NIL) && (op != FASL_OP_LIST))
    dump_error("malformed slot-names in instance map");
}

static void dump_instance()
{
  fasl_opcode_t op = dump_next_object(_T("map"));

  if (op != FASL_OP_INSTANCE_MAP)
    dump_error("missing instance map in instance.");

  op = dump_next_object(_T("slots"));

  if ((op != FASL_OP_NIL) && (op != FASL_OP_LIST))
    dump_error("Expected list for slot values");
}

static void dump_hash()
{
  fasl_opcode_t op = dump_next_object(_T("shallow?"));

  if ((op != FASL_OP_TRUE) && (op != FASL_OP_FALSE))
    dump_error("expected boolean for hash table shallow");

  op = dump_next_object(_T("key/values"));

  if ((op != FASL_OP_NIL) && (op != FASL_OP_LIST) && (op != FASL_OP_LISTD))
    dump_error("malformed key/value list for hash table");
}

static void dump_closure()
{
  fasl_opcode_t op = dump_next_object(_T("env"));

  if ((op != FASL_OP_NIL) && (op != FASL_OP_LIST) && (op != FASL_OP_LISTD))
    dump_error("malformed closure, bad environment");

  op = dump_next_object(_T("code"));

  if ((op != FASL_OP_NIL) && (op != FASL_OP_LIST) && (op != FASL_OP_LISTD))
    dump_error("malformed closure, bad code");

  op = dump_next_object(_T("plist"));

  if ((op != FASL_OP_NIL) && (op != FASL_OP_LIST) && (op != FASL_OP_LISTD))
    dump_error("malformed closure, bad property list");
}

void dump_to_newline()
{
  _TCHAR ch = _T('\0');

  while ((ch != _T('\n')) && (ch != _T('\r'))) {
    if (read_binary(&ch, sizeof(_TCHAR), 1) == 0)
      break;

    putchar(ch);
  }
}

void dump_macro()
{
  fasl_opcode_t op = dump_next_object(_T("transformer"));

  if (op != FASL_OP_CLOSURE)
    dump_error("malformed macro, bad transformer");
}

static void dump_structure_layout()
{
  fasl_opcode_t op = dump_next_object(_T("layout-data"));

  if (op != FASL_OP_LIST)
    dump_error("Expected list for structure layout");
}

static void dump_fast_op(int arity)
{
  fixnum_t opcode;

  fasl_opcode_t op = dump_next_object(_T("opcode"), &opcode); /*  TODO: parse opcode to print name */

  _TCHAR buf[STRBUF_SIZE];

  for(fixnum_t ii = 0; ii < arity; ii++)
    {
      _sntprintf(buf, STRBUF_SIZE, _T("operand["FIXNUM_PRINTF_PREFIX"i]"), ii);

      op = dump_next_object(buf);
    }
}

static void dump_structure()
{
  fixnum_t length;
  _TCHAR buf[STRBUF_SIZE];

  fasl_opcode_t op = dump_next_object(_T("layout"));

  if (op != FASL_OP_STRUCTURE_LAYOUT)
    dump_error("Expected structure layout");

  op = dump_next_object(_T("length"), &length);

  if (!FIXNUM_OP_P(op))
    dump_error("Expected fixnum for structure length");

  for(fixnum_t ii = 0; ii < length; ii++)
    {
      _sntprintf(buf, STRBUF_SIZE, _T("slot["FIXNUM_PRINTF_PREFIX "i]"), ii);

      op = dump_next_object(buf);

      if (op == FASL_OP_EOF)
	dump_error("incomplete structure definition");
    }
}

fixnum_t dump_table_index()
{
  fixnum_t index;

  fasl_opcode_t op = dump_next_object(_T("index"), &index);

  if (!FIXNUM_OP_P(op))
    dump_error("Expected fixnum for FASL table index");

  return index;
}

void dump_loader_definition()
{
  fasl_opcode_t op = dump_next_object(_T("symbol"));

  if (op != FASL_OP_SYMBOL)
    dump_error("Expected symbol for definition");

  dump_next_object(_T("definition"));
}

void  dump_loader_apply0()
{
  fasl_opcode_t op = dump_next_object(_T("fn"));

  if ((op != FASL_OP_CLOSURE) && (op != FASL_OP_SUBR))
    dump_error("Expected function for apply");
}

void dump_load_unit_boundary(fasl_opcode_t op)
{
  newline();

  if (op == FASL_OP_BEGIN_LOAD_UNIT)
    printf(" ---------------- BEGIN LOAD UNIT ----------------");

  dump_next_object(_T("module-name"));

  if (op == FASL_OP_END_LOAD_UNIT)
    printf("\n ----------------- END LOAD UNIT -----------------");
}


static fasl_opcode_t dump_next_object(const _TCHAR *desc /* = NULL*/,
                                   fixnum_t *fixnum_value /* = NULL*/)
{
  fasl_opcode_t opcode = FASL_OP_NIL;
  size_t offset;
  fixnum_t index;
  fixnum_t reader_defn_fixnum;

  g_nesting_level++;

  /* The core of this function is wrapped in a giant while loop to remove
   * tail recursive calls.*/
  
  bool current_read_complete = false;
  while (!current_read_complete) {
       /*  Assume we're going to complete the read unless we find out otherwise.. */
    current_read_complete = true;

    opcode = fast_read_opcode(&offset);

    show_opcode(offset, opcode, desc);


    switch(opcode) {
    case FASL_OP_NIL:			                        break;
    case FASL_OP_TRUE:			                        break;
    case FASL_OP_FALSE:			                        break;

    case FASL_OP_CHARACTER:		dump_character();	break;

    case FASL_OP_LIST:			dump_list(false);	break;
    case FASL_OP_LISTD:			dump_list(true);	break;

    case FASL_OP_FIX8:			dump_fixnum(1, fixnum_value);  	break;
    case FASL_OP_FIX16:			dump_fixnum(2, fixnum_value);	break;
    case FASL_OP_FIX32:			dump_fixnum(4, fixnum_value);	break;
    case FASL_OP_FIX64:			dump_fixnum(8, fixnum_value);   break;

    case FASL_OP_FLOAT:			dump_flonum(false);	break;
    case FASL_OP_COMPLEX:		dump_flonum(true);   	break;

    case FASL_OP_STRING:		dump_string();		break;
    case FASL_OP_PACKAGE:		dump_package();		break;
    case FASL_OP_VECTOR:		dump_vector();		break;

    case FASL_OP_INSTANCE:		dump_instance();	break;
    case FASL_OP_HASH:			dump_hash();	        break;

    case FASL_OP_CLOSURE:		dump_closure();		break;
    case FASL_OP_MACRO:			dump_macro();		break;
    case FASL_OP_SYMBOL:		dump_symbol();		break;
    case FASL_OP_SUBR:                  dump_subr();            break;

    case FASL_OP_STRUCTURE:             dump_structure();       break;
    case FASL_OP_STRUCTURE_LAYOUT:      dump_structure_layout();break;

    case FASL_OP_FAST_OP_0:             dump_fast_op(0);        break;
    case FASL_OP_FAST_OP_1:             dump_fast_op(1);        break;
    case FASL_OP_FAST_OP_2:             dump_fast_op(2);        break;
    case FASL_OP_FAST_OP_3:             dump_fast_op(3);        break;

    case FASL_OP_NOP_1:
    case FASL_OP_NOP_2:
    case FASL_OP_NOP_3:
      current_read_complete = false;
      break;

    case FASL_OP_INSTANCE_MAP:          dump_instance_map();    break;

    case FASL_OP_COMMENT_1:
    case FASL_OP_COMMENT_2:
      dump_to_newline();
      current_read_complete = false;
      break;

    case FASL_OP_RESET_READER_DEFS:
      memset(g_reader_definition_ops, 0, sizeof(g_reader_definition_ops));
      memset(g_reader_definition_fixnums, 0, sizeof(g_reader_definition_fixnums));
      break;

    case FASL_OP_READER_DEFINITION:
      index = dump_table_index();

      opcode = dump_next_object(_T("definition"), &reader_defn_fixnum);
      g_reader_definition_ops[index] = opcode;

      if (FIXNUM_OP_P(opcode)) {
	g_reader_definition_fixnums[index] = reader_defn_fixnum;

	if (fixnum_value != NULL)
	  *fixnum_value = g_reader_definition_fixnums[index];
      }
      break;

    case FASL_OP_READER_REFERENCE:
      index = dump_table_index();
      opcode = g_reader_definition_ops[index];

      if (FIXNUM_OP_P(opcode) && (fixnum_value != NULL))
	  *fixnum_value = g_reader_definition_fixnums[index];
      break;

    case FASL_OP_EOF: break;

    case FASL_OP_LOADER_DEFINEQ:
    case FASL_OP_LOADER_DEFINEA0:
      dump_loader_definition();
      current_read_complete = false;
      break;

    case FASL_OP_LOADER_APPLY0:
      dump_loader_apply0();
      break;

    case FASL_OP_BEGIN_LOAD_UNIT:
    case FASL_OP_END_LOAD_UNIT:
      dump_load_unit_boundary(opcode);
      break;


    default:
      dump_error("invalid opcode");
    }
  }

  g_nesting_level--;

  return opcode;
}


int dump_file(char *filename)
{
  _TCHAR buf[STRBUF_SIZE];

  g_file = fopen(filename, "rb");

  if (g_file == NULL) {
    fprintf(stderr, "Error opening file %s\n", filename);
    return 1;
  }

  fixnum_t object_number = 0;

  for(;;) {
    _sntprintf(buf, STRBUF_SIZE, _T("top["FIXNUM_PRINTF_PREFIX "i]"), object_number);

    if (dump_next_object(buf) == FASL_OP_EOF)
      break;

    object_number++;
  }

  fclose(g_file);
  g_file = NULL;

  newline();

  return 0;
}

END_NAMESPACE

int main(int argc, char *argv[])
{
     memset(scan::g_reader_definition_ops, 0, sizeof(scan::g_reader_definition_ops));
     memset(scan::g_reader_definition_fixnums, 0, sizeof(scan::g_reader_definition_fixnums));

  if (argc < 2) {
    fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
    return 1;
  }

  for(int arg = 1; arg < argc; arg++)
       if (scan::dump_file(argv[arg]))
            break;

  return 0;
}


