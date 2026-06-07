#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

#include <pulse/pulseaudio.h>
#include <pulse/simple.h>

CAMLprim value ocaml_pa_strerror(value e) {
  return caml_copy_string(pa_strerror(Int_val(e)));
}

/* char* for Some string or NULL for None */
static const char *string_opt_val(value v) {
  if (Is_long(v))
    return NULL;
  else
    return String_val(Field(v, 0));
}

static pa_sample_spec *sample_spec_val(value spec) {
  pa_sample_spec *ans = malloc(sizeof(pa_sample_spec));

  /* TODO */
  ans->format = PA_SAMPLE_FLOAT32LE;
  ans->rate = Int_val(Field(spec, 1));
  ans->channels = Int_val(Field(spec, 2));

  return ans;
}

#define PaSimple_val(v) (*(pa_simple **)Data_abstract_val(v))
#define Simple_val(v) (PaSimple_val(Field(v, 0)))
#define Simple_chans_val(v) (Int_val(Field(v, 1)))

static pa_stream_direction_t dir_val(value dir) {
  switch (Int_val(dir)) {
  case 0:
    return PA_STREAM_NODIRECTION;

  case 1:
    return PA_STREAM_PLAYBACK;

  case 2:
    return PA_STREAM_RECORD;

  case 3:
    return PA_STREAM_UPLOAD;

  default:
    assert(0);
  }
}

CAMLprim value ocaml_pa_simple_new(value server, value name, value dir,
                                   value device, value description,
                                   value sample, value map, value attr) {
  CAMLparam5(server, name, dir, device, description);
  CAMLxparam3(sample, map, attr);
  CAMLlocal2(ans, tmp);
  pa_simple *simple;
  pa_sample_spec *ss;
  int err;

  pa_buffer_attr *ba = NULL;
  if (Is_block(attr)) {
    ba = malloc(sizeof(pa_buffer_attr));
    attr = Field(attr, 0);
    ba->maxlength = Int_val(Field(attr, 0));
    ba->tlength = Int_val(Field(attr, 1));
    ba->prebuf = Int_val(Field(attr, 2));
    ba->minreq = Int_val(Field(attr, 3));
    ba->fragsize = Int_val(Field(attr, 4));
  }

  ss = sample_spec_val(sample);
  simple = pa_simple_new(string_opt_val(server), String_val(name), dir_val(dir),
                         string_opt_val(device), String_val(description), ss,
                         NULL, ba, &err);
  if (ba)
    free(ba);
  if (!simple) {
    free(ss);
    caml_raise_with_arg(*caml_named_value("pa_exn_error"), Val_int(err));
  }

  tmp = caml_alloc(1, Abstract_tag);
  PaSimple_val(tmp) = simple;

  ans = caml_alloc_tuple(2);
  Store_field(ans, 0, tmp);
  Store_field(ans, 1, Val_int(ss->channels));
  free(ss);

  CAMLreturn(ans);
}

CAMLprim value ocaml_pa_simple_new_byte(value *argv, int argc) {
  return ocaml_pa_simple_new(argv[0], argv[1], argv[2], argv[3], argv[4],
                             argv[5], argv[6], argv[7]);
}

static void check_err(int ret, int err) {
  if (ret < 0)
    caml_raise_with_arg(*caml_named_value("pa_exn_error"), Val_int(err));
}

CAMLprim value ocaml_pa_simple_free(value simple) {
  pa_simple_free(Simple_val(simple));
  return Val_unit;
}

CAMLprim value ocaml_pa_simple_write_float(value _simple, value _buf,
                                           value _ofs, value _len) {
  CAMLparam2(_simple, _buf);
  CAMLlocal1(bufc);
  pa_simple *simple = Simple_val(_simple);
  int ofs = Int_val(_ofs);
  int len = Int_val(_len);
  float *buf;
  int err;
  int ret;
  int chans = Wosize_val(_buf);
  int c, i;

  buf = malloc(chans * len * sizeof(float));
  for (c = 0; c < chans; c++) {
    bufc = Field(_buf, c);
    for (i = 0; i < len; i++)
      buf[chans * i + c] = Double_field(bufc, ofs + i);
  }

  caml_enter_blocking_section();
  ret = pa_simple_write(simple, buf, len * chans * sizeof(float), &err);
  caml_leave_blocking_section();
  free(buf);
  check_err(ret, err);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_pa_simple_write_floatarray(value _simple, value _buf,
                                                value _ofs, value _len) {
  CAMLparam2(_simple, _buf);
  CAMLlocal1(bufc);
  pa_simple *simple = Simple_val(_simple);
  int ofs = Int_val(_ofs);
  int len = Int_val(_len);
  float *buf;
  int err;
  int ret;
  int chans = Wosize_val(_buf);
  int c, i;

  buf = malloc(chans * len * sizeof(float));
  for (c = 0; c < chans; c++) {
    bufc = Field(_buf, c);
    for (i = 0; i < len; i++)
      buf[chans * i + c] = Double_flat_field(bufc, ofs + i);
  }

  caml_enter_blocking_section();
  ret = pa_simple_write(simple, buf, len * chans * sizeof(float), &err);
  caml_leave_blocking_section();
  free(buf);
  check_err(ret, err);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_pa_simple_write_float_ba(value _simple, value _buf) {
  CAMLparam2(_simple, _buf);
  CAMLlocal1(bufc);
  struct caml_ba_array *ba = Caml_ba_array_val(_buf);
  int len = ba->dim[0];
  float *buf = ba->data;
  int err;
  int ret;
  pa_simple *simple = Simple_val(_simple);

  caml_enter_blocking_section();
  ret = pa_simple_write(simple, buf, len * sizeof(float), &err);
  caml_leave_blocking_section();
  check_err(ret, err);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_pa_simple_drain(value _simple) {
  CAMLparam1(_simple);
  pa_simple *simple = Simple_val(_simple);
  int ret, err;

  caml_enter_blocking_section();
  ret = pa_simple_drain(simple, &err);
  caml_leave_blocking_section();
  check_err(ret, err);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_pa_simple_flush(value _simple) {
  CAMLparam1(_simple);
  pa_simple *simple = Simple_val(_simple);
  int ret, err;

  caml_enter_blocking_section();
  ret = pa_simple_flush(simple, &err);
  caml_leave_blocking_section();
  check_err(ret, err);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_pa_simple_get_latency(value _simple) {
  CAMLparam1(_simple);
  pa_simple *simple = Simple_val(_simple);
  int ret, err;

  caml_enter_blocking_section();
  ret = pa_simple_flush(simple, &err);
  caml_leave_blocking_section();
  check_err(ret, err);

  CAMLreturn(Int_val(ret));
}

CAMLprim value ocaml_pa_read_float(value _simple, value _buf, value _ofs,
                                   value _len) {
  CAMLparam2(_simple, _buf);
  CAMLlocal1(bufc);
  pa_simple *simple = Simple_val(_simple);
  int ofs = Int_val(_ofs);
  int len = Int_val(_len);
  float *buf;
  int err, ret;
  int chans = Simple_chans_val(_simple);
  int c, i;

  buf = malloc(chans * len * sizeof(float));

  caml_enter_blocking_section();
  ret = pa_simple_read(simple, buf, chans * len * sizeof(float), &err);
  caml_leave_blocking_section();

  if (ret < 0) {
    free(buf);
    caml_raise_with_arg(*caml_named_value("pa_exn_error"), Val_int(err));
  }

  for (c = 0; c < chans; c++) {
    bufc = Field(_buf, c);
    for (i = 0; i < len; i++)
      Store_double_field(bufc, ofs + i, buf[chans * i + c]);
  }
  free(buf);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_pa_read_floatarray(value _simple, value _buf, value _ofs,
                                        value _len) {
  CAMLparam2(_simple, _buf);
  CAMLlocal1(bufc);
  pa_simple *simple = Simple_val(_simple);
  int ofs = Int_val(_ofs);
  int len = Int_val(_len);
  float *buf;
  int err, ret;
  int chans = Simple_chans_val(_simple);
  int c, i;

  buf = malloc(chans * len * sizeof(float));

  caml_enter_blocking_section();
  ret = pa_simple_read(simple, buf, chans * len * sizeof(float), &err);
  caml_leave_blocking_section();

  if (ret < 0) {
    free(buf);
    caml_raise_with_arg(*caml_named_value("pa_exn_error"), Val_int(err));
  }

  for (c = 0; c < chans; c++) {
    bufc = Field(_buf, c);
    for (i = 0; i < len; i++)
      Store_double_flat_field(bufc, ofs + i, buf[chans * i + c]);
  }
  free(buf);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_pa_read_float_ba(value _simple, value _buf) {
  CAMLparam2(_simple, _buf);
  pa_simple *simple = Simple_val(_simple);
  struct caml_ba_array *ba = Caml_ba_array_val(_buf);
  int buflen = caml_ba_byte_size(ba);
  float *buf = ba->data;
  int err, ret;

  caml_enter_blocking_section();
  ret = pa_simple_read(simple, buf, buflen, &err);
  caml_leave_blocking_section();

  if (ret < 0)
    caml_raise_with_arg(*caml_named_value("pa_exn_error"), Val_int(err));

  CAMLreturn(Val_unit);
}
