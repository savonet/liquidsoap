/*
 * OCaml bindings for libmad
 *
 * Copyright 2001 Bertrand Petit
 * Copyright 2003-2006 Savonet team
 *
 * This file is part of Ocaml-mad.
 * The libmad's stubs are based on madlld (see README for details).
 *
 * Ocaml-mad is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-mad is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-mad; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * @author Samuel Mimram, David Baelde
 */

#define CAML_INTERNALS
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#ifdef WIN32
#include <caml/osdeps.h>
#endif
#include <caml/signals.h>
#include <caml/threads.h>

#ifndef Bytes_val
#define Bytes_val String_val
#endif

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#ifdef __MINGW32__
#include <winsock.h>
#else
#include <netinet/in.h>
#endif
#include <mad.h>
#include <stdint.h>

// The theoretical maximum frame size is 2881 bytes,
// MPEG 2.5 Layer II, 8000 Hz @ 160 kbps, with a padding slot plus 8 byte
// MAD_BUFFER_GUARD.
#define READ_SIZE 4096
#define BUFFER_SIZE READ_SIZE + MAD_BUFFER_GUARD
#define FRAME_SIZE 2881

struct madfile__t {
  struct mad_stream stream;
  struct mad_frame frame;
  struct mad_synth synth;
  mad_timer_t timer;
  FILE *fd;
  value read_func;
  unsigned char buf[BUFFER_SIZE];
  int eof;
  value data;
};

typedef struct madfile__t madfile_t;

/* Utility to scale and round samples to 16 bits
 * It converts a sample from mad's fixed point number format to an unsigned
 * short (16 bits).
 *
 * Shamelessly taken from the MPlayer source code.
 */
static short short_of_madfixed(mad_fixed_t sample) {
  /* A fixed point number is formed of the following bit pattern:
   *
   * SWWWFFFFFFFFFFFFFFFFFFFFFFFFFFFF
   * MSB                          LSB
   * S ==> Sign (0 is positive, 1 is negative)
   * W ==> Whole part bits
   * F ==> Fractional part bits
   *
   * This pattern contains MAD_F_FRACBITS fractional bits, one
   * should always use this macro when working on the bits of a fixed
   * point number. It is not guaranteed to be constant over the
   * different platforms supported by libmad.
   *
   * The unsigned short value is formed by the least significant
   * whole part bit, followed by the 15 most significant fractional
   * part bits.
   */

  /* round */
  sample += (1L << (MAD_F_FRACBITS - 16));

  /* clip */
  if (sample >= MAD_F_ONE)
    sample = MAD_F_ONE - 1;
  else if (sample < -MAD_F_ONE)
    sample = -MAD_F_ONE;

  /* quantize */
  return sample >> (MAD_F_FRACBITS + 1 - 16);

  return (short)sample;
}

#define Madfile_val(v) (*((madfile_t **)Data_custom_val(v)))

static void finalize_madfile(value madf) {
  madfile_t *mf = Madfile_val(madf);
  if (mf->read_func)
    caml_remove_generational_global_root(&mf->read_func);
  if (mf->data)
    caml_remove_generational_global_root(&mf->data);
  mad_synth_finish(&mf->synth);
  mad_frame_finish(&mf->frame);
  mad_stream_finish(&mf->stream);
  free(mf);
}

static struct custom_operations madfile_ops = {
    "ocaml_mad_madfile", finalize_madfile,         custom_compare_default,
    custom_hash_default, custom_serialize_default, custom_deserialize_default};

static inline madfile_t *create_mf() {
  madfile_t *mf;

  mf = malloc(sizeof(madfile_t));
  mad_stream_init(&mf->stream);
  mad_frame_init(&mf->frame);
  mad_synth_init(&mf->synth);
  mad_timer_reset(&mf->timer);
  mf->read_func = (value)NULL;
  mf->eof = 0;
  mf->data = (value)NULL;

  return (mf);
}

/* Returns a "normal" 4-bytes integer from
 * a synchsafe integer */
int unsynchsafe(unsigned int in) {
  int out = 0, i;
  unsigned int mask = 0x7F000000;

  for (i = 0; i < 4; ++i) {
    out >>= 1;
    out |= in & mask;
    mask >>= 8;
  }

  return out;
}

// TODO: unify file/callback API?
CAMLprim value ocaml_mad_openfile(value file) {
  CAMLparam1(file);
  CAMLlocal1(block);
  madfile_t *mf;

#ifdef WIN32
  FILE *fd;
  wchar_t *fname = caml_stat_strdup_to_os(String_val(file));

  errno_t err = _wfopen_s(&fd, fname, L"rb");
  free(fname);

  if (err != 0)
    caml_raise_with_arg(*caml_named_value("mad_exn_openfile_error"),
                        caml_copy_string(strerror(err)));
#else
  FILE *fd = fopen(String_val(file), "rb");
#endif

  if (!fd)
    caml_raise_with_arg(*caml_named_value("mad_exn_openfile_error"),
                        caml_copy_string(strerror(errno)));

  /* Remove ID3 tag
   * Ref: http://www.id3.org/id3v2.4.0-structure */
  char id3_header[3];
  uint32_t id3_len;
  int footer_len = 0;
  fread(&id3_header, sizeof(char), 3, fd);
  /* Check for ID3 tag magic */
  if ((id3_header[0] == 0x49) & (id3_header[1] == 0x44) &
      (id3_header[2] == 0x33)) { /* Read version and flags */
    fread(&id3_header, sizeof(char), 3, fd);
    /* Check for footer flag */
    if (id3_header[2] & 0x10)
      // 0b00010000 doesn't seem to work will all compilers..
      footer_len = 10;
    /* Get synchsafe len of ID3 tag */
    fread(&id3_len, sizeof(char), 4, fd);
    fseek(fd, unsynchsafe(ntohl(id3_len)) + footer_len, SEEK_CUR);
  } else
    rewind(fd);

  mf = create_mf();
  mf->fd = fd;
  if (!mf->fd)
    caml_raise_with_arg(*caml_named_value("mad_exn_openfile_error"),
                        caml_copy_string(strerror(errno)));
  memset(mf->buf, 0, BUFFER_SIZE);

  // The amount of data "secretly" owned by the madfile_t may be underestimated
  // here since I only take into account mf->buf, while it may be possible
  // that the mad_*_init functions also allocate stuff.
  block = caml_alloc_custom(&madfile_ops, sizeof(madfile_t *), BUFFER_SIZE,
                            1000000);
  Madfile_val(block) = mf;

  CAMLreturn(block);
}

CAMLprim value ocaml_mad_openstream(value read_func) {
  CAMLparam1(read_func);
  CAMLlocal1(block);
  madfile_t *mf;

  mf = create_mf();
  mf->read_func = read_func;
  caml_register_generational_global_root(&mf->read_func);
  mf->data = caml_alloc_string(BUFFER_SIZE);
  caml_register_generational_global_root(&mf->data);
  mf->fd = 0;
  memset(mf->buf, 0, BUFFER_SIZE);

  block = caml_alloc_custom(&madfile_ops, sizeof(madfile_t *), 0, 1);
  Madfile_val(block) = mf;

  CAMLreturn(block);
}

CAMLprim value ocaml_mad_close(value madf) {
  CAMLparam1(madf);
  madfile_t *mf = Madfile_val(madf);
  if (!mf->read_func && fclose(mf->fd) != 0)
    caml_raise_with_arg(*caml_named_value("mad_exn_closefile_error"),
                        caml_copy_string(strerror(errno)));
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_mad_skip_id3tag(value read, value seek, value tell) {
  CAMLparam3(read, seek, tell);
  CAMLlocal3(ret, buf, s);
  /* These values are used to
   * store integers. Therefore, they do
   * not need to be protected. */
  value initial_position;
  int readlen;

  /* Get initial position. */
  initial_position = Int_val(caml_callback(tell, Val_unit));

  /* Remove ID3 tag
   * Ref: http://www.id3.org/id3v2.4.0-structure */
  uint32_t *id3_len;
  int footer_len = 0;
  /* Here we assume that such small read length
   * will always have either 0 (end of stream)
   * or the right amount of data... */
  buf = caml_alloc_string(4);

  ret = caml_callback3(read, buf, Val_int(0), Val_int(3));

  readlen = Int_val(ret);

  if (readlen == 0) {
    caml_raise_constant(*caml_named_value("mad_exn_end_of_stream"));
  }

  const char *id3_header = (const char *)String_val(buf);
  /* Check for ID3 tag magic */
  if ((id3_header[0] == 0x49) & (id3_header[1] == 0x44) &
      (id3_header[2] == 0x33)) { /* Read version and flags */
    ret = caml_callback3(read, buf, Val_int(0), Val_int(3));

    readlen = Int_val(ret);

    if (readlen == 0) {
      caml_raise_constant(*caml_named_value("mad_exn_end_of_stream"));
    }

    id3_header = (const char *)String_val(buf);
    /* Check for footer flag */
    if (id3_header[2] & 0x10)
      // 0b00010000 doesn't seem to work will all compilers..
      footer_len = 10;
    /* Get synchsafe len of ID3 tag */
    ret = caml_callback3(read, buf, Val_int(0), Val_int(3));

    readlen = Int_val(ret);

    if (readlen == 0) {
      caml_raise_constant(*caml_named_value("mad_exn_end_of_stream"));
    }

    id3_len = (uint32_t *)String_val(buf);
    int position = Int_val(caml_callback(tell, Val_unit));
    caml_callback(
        seek, Val_int(position + unsynchsafe(ntohl(*id3_len)) + footer_len));
  } else
    caml_callback(seek, initial_position);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_mad_get_current_position(value madf) {
  CAMLparam1(madf);
  madfile_t *mf = Madfile_val(madf);
  if (mf->read_func)
    caml_raise_with_arg(*caml_named_value("mad_exn_closefile_error"),
                        caml_copy_string("Not created with openfile."));
  int pos = (int)ftell(mf->fd);
  CAMLreturn(Val_int(pos));
}

/*
 * Shamelessly inspired of the Madlld source code.
 */
static void mf_fill_buffer(madfile_t *mf) {
  CAMLparam0();

  if (mf->eof)
    return;

  /* The input bucket must be filled if it becomes empty
   */
  if (mf->stream.buffer == NULL || mf->stream.error == MAD_ERROR_BUFLEN) {
    size_t read_size, remaining;
    unsigned char *read_start;

    /* {1} libmad may not consume all bytes of the input
     * buffer. If the last frame in the buffer is not wholly
     * contained by it, then that frame's start is pointed by
     * the next_frame member of the Stream structure. This
     * common situation occurs when mad_frame_decode() fails,
     * sets the stream error code to MAD_ERROR_BUFLEN, and
     * sets the next_frame pointer to a non NULL value. (See
     * also the comment marked {2} below.)
     *
     * When this occurs, the remaining unused bytes must be
     * put back at the beginning of the buffer and taken in
     * account before refilling the buffer. This means that
     * the input buffer must be large enough to hold a whole
     * frame at the highest observable bit-rate (currently 448
     * kb/s). */
    if (mf->stream.error != MAD_ERROR_BUFLEN) {
      remaining = 0;
    } else if (mf->stream.next_frame) {
      remaining = mf->stream.bufend - mf->stream.next_frame;
    } else if ((mf->stream.bufend - mf->stream.buffer) < BUFFER_SIZE) {
      remaining = mf->stream.bufend - mf->stream.buffer;
    } else {
      remaining = BUFFER_SIZE - FRAME_SIZE;
    }

    if (remaining)
      memmove(mf->buf, mf->stream.bufend - remaining, remaining);

    read_start = mf->buf + remaining;
    read_size = READ_SIZE - remaining;

    /* Fill-in the buffer. If an error occurs print a message
     * and leave the decoding loop. If the end of stream is
     * reached we also leave the loop but the return status is
     * left untouched.
     */
    if (mf->read_func) {
      read_size = Int_val(caml_callback3(mf->read_func, mf->data, Val_int(0),
                                         Val_int(read_size)));
      if (read_size > 0)
        memcpy(read_start, String_val(mf->data), read_size);
      else if (read_size == 0)
        mf->eof = 1;
      else
        caml_raise_with_arg(*caml_named_value("mad_exn_read_error"),
                            caml_copy_string("reading callback error"));
    } else {
      read_size = fread(read_start, 1, read_size, mf->fd);
      if (read_size <= 0) {
        if (ferror(mf->fd)) {
          /* Read error on bitstream. */
          caml_raise_with_arg(*caml_named_value("mad_exn_read_error"),
                              caml_copy_string((char *)strerror(errno)));
        }
        if (feof(mf->fd))
          mf->eof = 1;
      }
    }

    /* {3} When decoding the last frame of a file, it must be
     * followed by MAD_BUFFER_GUARD zero bytes if one wants to
     * decode that last frame. When the end of file is
     * detected we append that quantity of bytes at the end of
     * the available data. Note that the buffer can't overflow
     * as the guard size was allocated but not used the the
     * buffer management code. (See also the comment marked
     * {1}.)
     *
     * In a message to the mad-dev mailing list on May 29th,
     * 2001, Rob leslie explains the guard zone as follows:
     *
     *    "The reason for MAD_BUFFER_GUARD has to do with the
     *    way decoding is performed. In Layer III, Huffman
     *    decoding may inadvertently read a few bytes beyond
     *    the end of the buffer in the case of certain invalid
     *    input. This is not detected until after the fact. To
     *    prevent this from causing problems, and also to
     *    ensure the next frame's main_data_begin pointer is
     *    always accessible, MAD requires MAD_BUFFER_GUARD
     *    (currently 8) bytes to be present in the buffer past
     *    the end of the current frame in order to decode the
     *    frame."
     */
    if (mf->eof) {
      read_size += MAD_BUFFER_GUARD;
      memset(mf->buf + read_size + remaining, 0, MAD_BUFFER_GUARD);
    }

    /* Pipe the new buffer content to libmad's stream decoder
     * facility.
     */
    mad_stream_buffer(&mf->stream, mf->buf, read_size + remaining);
    mf->stream.error = 0;
  }

  CAMLreturn0;
}

/* Returns 1 if a recoverable error occurred, 0 else. */
static int mf_decode(madfile_t *mf, int synth) {
  int dec;

  caml_enter_blocking_section();

  /* Decode the next mpeg frame. The streams is read from the
   * buffer, its constituents are break down and stored the the
   * Frame structure, ready for examination/alteration or PCM
   * synthesis. Decoding options are carried in the Frame
   * structure from the Stream structure.
   *
   * Error handling: mad_frame_decode() returns a non zero value
   * when an error occurs. The error condition can be checked in
   * the error member of the Stream structure. A mad error is
   * recoverable or fatal, the error status is checked with the
   * MAD_RECOVERABLE macro.
   *
   * {2} When a fatal error is encountered all decoding
   * activities shall be stopped, except when a MAD_ERROR_BUFLEN
   * is signaled. This condition means that the
   * mad_frame_decode() function needs more input to achieve
   * it's work. One shall refill the buffer and repeat the
   * mad_frame_decode() call. Some bytes may be left unused at
   * the end of the buffer if those bytes forms an incomplete
   * frame. Before refilling, the remainign bytes must be moved
   * to the beginning of the buffer and used for input for the
   * next mad_frame_decode() invocation. (See the comments marked
   * {1} earlier for more details.)
   *
   * Recoverable errors are caused by malformed bit-streams, in
   * this case one can call again mad_frame_decode() in order to
   * skip the faulty part and re-sync to the next frame.
   */
  dec = mad_frame_decode(&mf->frame, &mf->stream);

  if (dec) {
    caml_leave_blocking_section();

    if (MAD_RECOVERABLE(mf->stream.error) ||
        mf->stream.error == MAD_ERROR_BUFLEN) {

      if (mf->eof)
        caml_raise_constant(*caml_named_value("mad_exn_end_of_stream"));

      return 1;
    } else {
      caml_raise_with_arg(*caml_named_value("mad_exn_mad_error"),
                          caml_copy_string(mad_stream_errorstr(&mf->stream)));
    }
  }

  /* Accounting. The computed frame duration is in the frame
   * header structure. It is expressed as a fixed point number
   * whole data type is mad_timer_t. It is different from the
   * samples fixed point format and unlike it, it can't directly
   * be added or subtracted. The timer module provides several
   * functions to operate on such numbers. Be careful there, as
   * some functions of mad's timer module receive some of their
   * mad_timer_t arguments by value!
   */
  mad_timer_add(&mf->timer, mf->frame.header.duration);

  /* Once decoded the frame is synthesized to PCM samples. No errors
   * are reported by mad_synth_frame();
   */
  if (synth == 1)
    mad_synth_frame(&mf->synth, &mf->frame);

  caml_leave_blocking_section();
  return 0;
}

CAMLprim value ocaml_mad_decode_frame(value madf) {
  CAMLparam1(madf);
  CAMLlocal1(ret);
  madfile_t *mf = Madfile_val(madf);
  char *output_buf = NULL;
  int output_pos = 0;
  int i;
  int chans = MAD_NCHANNELS(&mf->frame.header);

  do {
    mf_fill_buffer(mf);
  } while (mf_decode(mf, 1) == 1);

  /* Synthesized samples must be converted from mad's fixed
   * point number to the consumer format. Here we use unsigned
   * 16 bit little endian integers on two channels. Integer samples
   * are temporarily stored in a buffer that is flushed when
   * full.
   */
  ret = caml_alloc_string(mf->synth.pcm.length * chans * 2);
  output_buf = (char *)Bytes_val(ret);

  for (i = 0; i < mf->synth.pcm.length; i++) {
    unsigned short sample;

    /* Left channel */
    sample = short_of_madfixed(mf->synth.pcm.samples[0][i]);
    *(output_buf + output_pos + 0) = sample & 0xff;
    *(output_buf + output_pos + 1) = sample >> 8;
    output_pos += 2;

    /* Right channel. If the decoded stream is monophonic then
     * the right output channel is the same as the left one
     * and we omit it here.
     */
    if (chans > 1) {
      sample = short_of_madfixed(mf->synth.pcm.samples[1][i]);
      *(output_buf + output_pos + 0) = sample & 0xff;
      *(output_buf + output_pos + 1) = sample >> 8;
      output_pos += 2;
    }
  }

  CAMLreturn(ret);
}

CAMLprim value ocaml_mad_decode_frame_float(value madf) {
  CAMLparam1(madf);
  CAMLlocal1(ret);
  madfile_t *mf = Madfile_val(madf);
  int chans;
  int i, c;

  do {
    mf_fill_buffer(mf);
  } while (mf_decode(mf, 1) == 1);

  chans = MAD_NCHANNELS(&mf->frame.header);
  ret = caml_alloc_tuple(chans);

  for (c = 0; c < chans; c++)
    Store_field(
        ret, c,
        caml_alloc(mf->synth.pcm.length * Double_wosize, Double_array_tag));

  for (c = 0; c < chans; c++)
    for (i = 0; i < mf->synth.pcm.length; i++)
      Store_double_field(Field(ret, c), i,
                         mad_f_todouble(mf->synth.pcm.samples[c][i]));

  CAMLreturn(ret);
}

CAMLprim value ocaml_mad_decode_frame_floatarray(value madf) {
  CAMLparam1(madf);
  CAMLlocal1(ret);
  madfile_t *mf = Madfile_val(madf);
  int chans;
  int i, c;

  do {
    mf_fill_buffer(mf);
  } while (mf_decode(mf, 1) == 1);

  chans = MAD_NCHANNELS(&mf->frame.header);
  ret = caml_alloc_tuple(chans);

  for (c = 0; c < chans; c++)
    Store_field(
        ret, c,
        caml_alloc(mf->synth.pcm.length * Double_wosize, Double_array_tag));

  for (c = 0; c < chans; c++)
    for (i = 0; i < mf->synth.pcm.length; i++)
      Store_double_flat_field(Field(ret, c), i,
                              mad_f_todouble(mf->synth.pcm.samples[c][i]));

  CAMLreturn(ret);
}

CAMLprim value ocaml_mad_decode_frame_float_ba(value madf) {
  CAMLparam1(madf);
  CAMLlocal2(ret, tmp);
  madfile_t *mf = Madfile_val(madf);
  float *data;
  int chans;
  int i, c;

  do {
    mf_fill_buffer(mf);
  } while (mf_decode(mf, 1) == 1);

  chans = MAD_NCHANNELS(&mf->frame.header);
  ret = caml_alloc_tuple(chans);

  for (c = 0; c < chans; c++) {
    tmp = caml_ba_alloc_dims(CAML_BA_C_LAYOUT | CAML_BA_MANAGED, 1, NULL,
                             mf->synth.pcm.length);
    data = Caml_ba_data_val(tmp);
    caml_release_runtime_system();
    for (i = 0; i < mf->synth.pcm.length; i++)
      data[i] = mad_f_todouble(mf->synth.pcm.samples[c][i]);
    caml_acquire_runtime_system();
    Store_field(ret, c, tmp);
  }

  CAMLreturn(ret);
}

CAMLprim value ocaml_mad_skip_frame(value madf) {
  CAMLparam1(madf);
  madfile_t *mf = Madfile_val(madf);
  int err;

  if (mf->eof)
    caml_raise_constant(*caml_named_value("mad_exn_end_of_stream"));

  do {
    mf_fill_buffer(mf);

    caml_enter_blocking_section();
    err = mad_header_decode(&mf->frame.header, &mf->stream);
    caml_leave_blocking_section();

    if (err == 0) {
      mad_timer_add(&mf->timer, mf->frame.header.duration);
    } else {
      if (!MAD_RECOVERABLE(mf->stream.error) &&
          mf->stream.error != MAD_ERROR_BUFLEN) {
        caml_raise_with_arg(*caml_named_value("mad_exn_mad_error"),
                            caml_copy_string(mad_stream_errorstr(&mf->stream)));
      }
    }
  } while (!mf->eof);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_mad_time(value madf, value u) {
  CAMLparam1(madf);
  madfile_t *mf = Madfile_val(madf);

  CAMLreturn(Val_long(mad_timer_count(mf->timer, Int_val(u))));
}

CAMLprim value ocaml_mad_get_frame_format(value madf) {
  CAMLparam1(madf);
  CAMLlocal1(ans);
  madfile_t *mf = Madfile_val(madf);
  int private = 0;
  if ((mf->frame.header.private_bits & MAD_PRIVATE_HEADER) > 0)
  private = 1;
  int copyright = 0;
  if ((mf->frame.header.flags & MAD_FLAG_COPYRIGHT) > 0)
    copyright = 1;
  int original = 0;
  if ((mf->frame.header.flags & MAD_FLAG_ORIGINAL) > 0)
    original = 1;

  ans = caml_alloc_tuple(10);
  Store_field(ans, 0,
              Val_int(mf->frame.header.layer - 1)); // Layers start at 1..
  Store_field(ans, 1, Val_int(mf->frame.header.mode));
  Store_field(ans, 2, Val_int(mf->frame.header.emphasis));
  Store_field(ans, 3, Val_long(mf->frame.header.bitrate));
  Store_field(ans, 4, Val_int(mf->synth.pcm.samplerate));
  Store_field(ans, 5, Val_int(mf->synth.pcm.channels));
  Store_field(ans, 6, Val_int(mf->synth.pcm.length));
  Store_field(ans, 7, Val_bool(original));
  Store_field(ans, 8, Val_bool(copyright));
  Store_field(ans, 9, Val_bool(private));

  CAMLreturn(ans);
}
