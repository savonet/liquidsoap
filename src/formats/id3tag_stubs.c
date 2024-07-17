/*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************/

/*
 * Read id3 tags using the libid3tag library.
 *
 * @author Samuel Mimram
 */

/* $Id: id3tag_stubs.c 2645 2006-07-17 08:28:51Z dbaelde $ */

#include <id3tag.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

/* Shamelessly inspired of gtkpod. */
static char* id3_get_string(struct id3_tag *tag, char *frame_name)
{
  const id3_ucs4_t *string = NULL;
  const id3_byte_t *binary = NULL;
  id3_length_t len = 0;
  struct id3_frame *frame;
  union id3_field *field;
  char *utf8 = NULL;
  enum id3_field_textencoding encoding = ID3_FIELD_TEXTENCODING_ISO_8859_1;

  /* Find the frame. */
  frame = id3_tag_findframe(tag, frame_name, 0);
#ifdef DEBUG
  printf("[DD] Frame %s: %p\n", frame_name, frame);
#endif
  if (!frame) return NULL;

  /* Find the encoding used for the field */
  field = id3_frame_field (frame, 0);
#ifdef DEBUG
  printf("[DD] Field: %p\n", field);
#endif

  if (field && (id3_field_type(field) == ID3_FIELD_TYPE_TEXTENCODING))
  {
    encoding = field->number.value;
#ifdef DEBUG
    printf("[DD] Encoding: %d\n", encoding);
#endif
  }

  /* The last field contains the data */
  field = id3_frame_field(frame, frame->nfields-1);
#ifdef DEBUG
  printf("[DD] Last field: %p\n", field);
#endif
  if (!field) return NULL;

  switch (field->type)
  {
    case ID3_FIELD_TYPE_STRINGLIST:
      string = id3_field_getstrings(field, 0);
      break;

    case ID3_FIELD_TYPE_STRINGFULL:
      string = id3_field_getfullstring(field);
      break;

    case ID3_FIELD_TYPE_BINARYDATA:
      binary = id3_field_getbinarydata(field, &len);
      /* TODO: malloc + copy + return */
      /*
      if (len > 0)
        return charset_to_utf8(binary+1);
      */
      return NULL;
      break;

    default:
      break;
  }
#ifdef DEBUG
  printf("[DD] String: %p\n", string);
#endif

  if (!string) return NULL;

  if (strcmp(frame_name, ID3_FRAME_GENRE) == 0)
    string = id3_genre_name(string);

  if (encoding == ID3_FIELD_TEXTENCODING_ISO_8859_1)
  {
    /* ISO_8859_1 is just a "marker" -- most people just drop
       whatever coding system they are using into it, so we use
       charset_to_utf8() to convert to utf8 */
    id3_latin1_t *raw = id3_ucs4_latin1duplicate(string);
    /* TODO: convert to utf8 */
    /*
    utf8 = charset_to_utf8(raw);
    g_free(raw);
    */
    utf8 = (char*)raw;
  }
  else if (encoding == ID3_FIELD_TEXTENCODING_UTF_16 || encoding == ID3_FIELD_TEXTENCODING_UTF_16BE)
  {
    utf8 = (char*)id3_ucs4_utf16duplicate(string);
  }
  else
  {
    /* Standard unicode is being used -- we won't have to worry
       about charsets then. */
    utf8 = (char*)id3_ucs4_utf8duplicate(string);
  }
  return utf8;
}

#define Id3_file_val(v) ((struct id3_file*)v)

CAMLprim value caml_id3tag_open_file(value fname, value mode)
{
  CAMLparam2(fname, mode);
  struct id3_file *file;

  /* TODO: use mode */
  file = id3_file_open(String_val(fname), ID3_FILE_MODE_READONLY);
#ifdef DEBUG
  printf("[DD] Opened file: %p\n", file);
#endif
  if (!file) caml_failwith("Invalid file");

  CAMLreturn((value)file);
}

CAMLprim value caml_id3tag_close_file(value f)
{
  CAMLparam1(f);

  id3_file_close(Id3_file_val(f));

  CAMLreturn(Val_unit);
}

/* TODO: use tags instead of file? */
CAMLprim value caml_id3tag_get_tag(value file, value tag_name)
{
  CAMLparam2(file, tag_name);
  CAMLlocal1(ans);
  struct id3_tag *tags;
  char *ret;

  tags = id3_file_tag(Id3_file_val(file));
  if (!tags) caml_failwith("No tags");
#ifdef DEBUG
  /* printf("[DD] Tag %p for file %p\n", tags, Id3_file_val(file)); */
#endif
  ret = id3_get_string(tags, String_val(tag_name));
  if (!ret) caml_failwith("No such tag");

  ans = caml_copy_string(ret);
  free(ret);

  CAMLreturn(ans);
}
