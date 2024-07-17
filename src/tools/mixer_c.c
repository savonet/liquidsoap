/**
 * Some C functions for the Mixer module.
 *
 * @author Samuel Mimram
 */

/* $Id: mixer_c.c,v 1.38 2005/03/10 18:12:03 dbaelde Exp $ */

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <limits.h>

#define PI 3.1415926535897932384

#define BUF_FREQ 44100
#define BUF_SAMPLESIZE 16
#define BUF_CHANS 2

static inline int max(int x, int y)
{
  if (x >= y)
    return x;
  else
    return y;
}

CAMLprim value ocaml_mixer_convert_format(value fmt, value buff)
{
  CAMLparam2(fmt, buff);

  int channels = Int_val(Field(fmt, 0));
  int sample_freq = Int_val(Field(fmt, 1));
  int sample_size = Int_val(Field(fmt, 2));
  int big_endian = Bool_val(Field(fmt, 3));
  int sign = Bool_val(Field(fmt, 4));

  int i;
  unsigned long long in_buf_len = caml_string_length(buff);
  unsigned long long out_buf_len = (in_buf_len * BUF_CHANS * BUF_FREQ * BUF_SAMPLESIZE)
    / (channels * sample_freq * sample_size) ;
  unsigned int buf_len = in_buf_len;

  CAMLlocal1(ret);
  ret = alloc_string(out_buf_len);
  char *buf;
  int malloced_buf = 0;

  if (channels > BUF_CHANS || sample_freq > BUF_FREQ || sample_size > BUF_SAMPLESIZE)
    {
      buf = malloc((in_buf_len * max(channels, BUF_CHANS) * max(sample_freq, BUF_FREQ) * max(sample_size, BUF_SAMPLESIZE))/(channels * sample_freq * sample_size));
      malloced_buf = 1;
    }
  else
    buf = String_val(ret);

  memcpy(buf, String_val(buff), in_buf_len);

  short *sbuf = (short*)buf;

  /* TODO: handle unsigned formats */
  switch (sample_size)
    {
    case 16:
      if(big_endian)
	{
	  char tmp;

	  for (i = 0; i < buf_len / 2; i++)
	    {
	      tmp = buf[2 * i];
	      buf[2 * i] = buf[2 * i + 1];
	      buf[2 * i + 1] = tmp;
	    }
	}
      break;

    case 8:
      buf_len *= 2;
      for (i = buf_len / 2 - 1; i >= 0; i--)
	if (sign)
	  sbuf[i] = ((short)buf[i]) << 8;
	else
	  sbuf[i] = (short)((unsigned char)buf[i] - 127) << 8;
      break;

    case 32:
      {
	buf_len /= 2;
	for (i = 0; i < buf_len / 2; i++)
	  sbuf[i] = (short)(((long*)buf)[i] >> 16);
      }
      break;

    default:
      raise_constant(*caml_named_value("mixer_exn_invalid_format"));
    }

  switch (channels)
    {
    case 2:
      break;

    case 1:
      buf_len *= 2;
      for (i = buf_len / 4 - 1; i >= 0; i--)
	{
	  sbuf[2 * i] = sbuf[i];
	  sbuf[2 * i + 1] = sbuf[i];
	}
      break;

    default:
      raise_constant(*caml_named_value("mixer_exn_invalid_format"));
    }

  /* TODO: 11025 */
  switch (sample_freq)
    {
    case 44100:
      break;

    case 22050:
      buf_len *= 2;
      for (i = buf_len / 8 - 1; i >= 0; i--)
	{
	  sbuf[4 * i] = sbuf[2 * i];
	  sbuf[4 * i + 1] = sbuf[2 * i + 1];
	  sbuf[4 * i + 2] = (sbuf[2 * i] / 2) + (sbuf[2 * i + 2] / 2);
	  sbuf[4 * i + 3] = (sbuf[2 * i + 1] / 2) + (sbuf[2 * i + 3] / 2);
	}
      break;

      /* TODO: this case stinks of the head */
    case 16000:
      {
	float f = 44.100 / 16.000;
	buf_len = (int)(f * buf_len);
	for (i = buf_len / 4 - 1; i >= 0; i--)
	  {
	    float fj = (2 * i) / f;
	    int j = (int)fj;
	    float c1 = (fj - j);
	    float c2 = (j + 1 - fj);

	    sbuf[2 * i] = c1 * sbuf[j] / 2 + c2 * sbuf[j + 1] / 2;
	    sbuf[2 * i + 1] = c1 * sbuf[j + 1] / 2 + c2 * sbuf[j + 3] / 2;
	  }
      }
      break;

    default:
      raise_constant(*caml_named_value("mixer_exn_invalid_format"));
    }

  if (malloced_buf)
    {
      memcpy(String_val(ret), buf, buf_len);
      free(buf);
    }

  CAMLreturn(ret);
}

CAMLprim value ocaml_mixer_change_volume(value buff,
					 value off, value len,
					 value coeff)
{
  CAMLparam4(buff, off, len, coeff);

  short *buf = (short*)String_val(buff);
  int offset = Int_val(off) / 2;        /* From char to short */
  int length = Int_val(len) / 2;        /* we need to /2      */
  double c = Double_val(coeff);
  int i;

  if (offset < 0 || length < 0
      || (offset + length) * 2 > caml_string_length(buff))
    raise_constant(*caml_named_value("mixer_exn_invalid_argument"));

  for (i = offset; i < offset + length; i++)
    buf[i] = (short)(c * buf[i]);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_mixer_add_buffer(value buff1, value off1,
				      value buff2, value off2,
				      value len)
{
  CAMLparam5(buff1, off1, buff2, off2, len);

  int length = Int_val(len) / 2;
  int offset1 = Int_val(off1) / 2;
  int offset2 = Int_val(off2) / 2;
  short *buf1 = (short*)String_val(buff1);
  short *buf2 = (short*)String_val(buff2);
  int i;
  long l;

  if (offset1 < 0 || offset2 < 0 || length < 0
      || (offset1 + length) * 2 > caml_string_length(buff1)
      || (offset2 + length) * 2 > caml_string_length(buff2))
    raise_constant(*caml_named_value("mixer_exn_invalid_argument"));

  for (i = 0; i < length; i++)
    {
      /* buf1[offset1 + i] += buf2[offset2 + i]; */
      l = (long)(buf1[offset1 + i]) + (long)(buf2[offset2 + i]);
      if (l > SHRT_MAX)
	l = SHRT_MAX;
      else if (l < SHRT_MIN)
	l = SHRT_MIN;
      buf1[offset1 + i] = (short)l;
    }

  CAMLreturn(Val_unit);
}

/*
 * @param cutoff: cutoff frequency in Hz
 * @param q resonance/bandwidth [0 < q <= 1]  most res: q=1, less: q=0
 * @parm filter_type: low (0), high (1), band (2), notch (3)
 */
CAMLprim value ocaml_mixer_simple_filter(value buff, value off, value len, value cutoff_freq, value q_coeff, value filter_type)
{
  CAMLparam5(buff, off, len, cutoff_freq, q_coeff);
  CAMLxparam1(filter_type);

  short *buf = (short*)String_val(buff);
  int offset = Int_val(off);
  int length = Int_val(len);
  float q = Double_val(q_coeff);
  int cutf = Int_val(cutoff_freq);
  float f = 2 * sin(PI * cutf / BUF_FREQ);
  int type = Int_val(filter_type);
  int i;
  short lowl = 0, highl = 0, bandl = 0, notchl = 0;
  short lowr = 0, highr = 0, bandr = 0, notchr = 0;

  if (offset + length > caml_string_length(buff))
    raise_constant(*caml_named_value("mixer_exn_invalid_argument"));

  for (i = offset / 4; i < (offset + length) / 4; i++)
    {
      lowl = lowl + f * bandl;
      lowr = lowr + f * bandr;
      highl = q * buf[2 * i] - lowl - q * bandl;
      highr = q * buf[2 * i + 1] - lowr - q * bandr;
      bandl = f * highl + bandl;
      bandr = f * highr + bandr;
      notchl = highl + lowl;
      notchr = highr + lowr;
      switch (type)
	{
	case 0:
	  buf[2 * i] = lowl;
	  buf[2 * i + 1] = lowr;
	  break;

	case 1:
	  buf[2 * i] = highl;
	  buf[2 * i + 1] = highr;
	  break;

	case 2:
	  buf[2 * i] = bandl;
	  buf[2 * i + 1] = bandr;
	  break;

	case 3:
	  buf[2 * i] = notchl;
	  buf[2 * i + 1] = notchr;
	  break;

	default: /* This should never be reached. */
	  raise_constant(*caml_named_value("mixer_exn_invalid_argument"));
	}
    }

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_mixer_simple_filter_bytecode(value *argv, int argc)
{
  return ocaml_mixer_simple_filter(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

CAMLprim value ocaml_mixer_sine(value buffer, value offset, value length, value frequency, value phi_)
{
  CAMLparam5(buffer, offset, length, frequency, phi_);

  int len = Int_val(length) / sizeof(short);
  int off = Int_val(offset) / sizeof(short);
  int freq = Int_val(frequency);
  float phi = Double_val(phi_);
  short *buf = (short*)String_val(buffer);
  int i;
  float omega = freq / 44100.;

  if (freq < 0 || off < 0 || len < 0
      || (off + len) * sizeof(short) > caml_string_length(buffer))
    raise_constant(*caml_named_value("mixer_exn_invalid_argument"));

  for(i = off / 2; i < (off + len) / 2; i++)
    {
      phi += omega * 4;
      buf[2 * i] = SHRT_MAX * sin(phi);
      buf[2 * i + 1] = buf[2 * i];
    }

  while(phi > 2 * PI)
    phi -= 2 * PI;

  CAMLreturn(copy_double(phi));
}
