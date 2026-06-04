#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/threads.h>

#include <samplerate.h>

#include <assert.h>
#include <stdio.h>

#define Int_conv(c) Int_val(c)

static inline float clip(float s) {
  if (s != s)
    return 0;

  if (s < -1) {
    return -1;
  } else if (s > 1) {
    return 1;
  } else
    return s;
}

CAMLprim value ocaml_samplerate_get_conv_name(value conv) {
  return caml_copy_string(src_get_name(Int_conv(conv)));
}

CAMLprim value ocaml_samplerate_get_conv_descr(value conv) {
  return caml_copy_string(src_get_description(Int_conv(conv)));
}

CAMLprim value ocaml_samplerate_convert(value vconv, value vchans, value vratio,
                                        value vinbuf, value inofs, value len) {
  CAMLparam2(vratio, vinbuf);
  int chans = Int_val(vchans);
  float ratio = Double_val(vratio);
  int inbuflen = Int_val(len);
  float *inbuf = (float *)malloc(sizeof(float) * inbuflen * chans);
  int outbuflen = (int)(inbuflen * ratio) + 64;
  float *outbuf = (float *)malloc(sizeof(float) * outbuflen * chans);
  SRC_DATA src_data;
  int i, ret;
  value ans;
  int anslen;

  for (i = 0; i < inbuflen * chans; i++)
    inbuf[i] = Double_field(vinbuf, i + Int_val(inofs));

  src_data.data_in = inbuf;
  src_data.input_frames = inbuflen;
  src_data.data_out = outbuf;
  src_data.output_frames = outbuflen;
  src_data.src_ratio = ratio;

  caml_enter_blocking_section();
  ret = src_simple(&src_data, Int_val(vconv), chans);
  caml_leave_blocking_section();

  free(inbuf);
  if (ret != 0) {
    fprintf(stderr, "ocaml-samplerate (%d): %s\n", ret, src_strerror(ret));
    assert(ret == 0);
  }
  assert(src_data.input_frames_used == src_data.input_frames);
  anslen = src_data.output_frames_gen * chans;
  ans = caml_alloc(anslen * Double_wosize, Double_array_tag);

  for (i = 0; i < anslen; i++)
    Store_double_field(ans, i, clip(outbuf[i]));
  free(outbuf);

  CAMLreturn(ans);
}

CAMLprim value ocaml_samplerate_convert_byte(value *argv, int argc) {
  return ocaml_samplerate_convert(argv[0], argv[1], argv[2], argv[3], argv[4],
                                  argv[5]);
}

#define State_val(v) (*((SRC_STATE **)Data_custom_val(v)))

static void finalize_src(value s) { src_delete(State_val(s)); }

static struct custom_operations state_ops = {
    "ocaml_samplerate_state", finalize_src,
    custom_compare_default,   custom_hash_default,
    custom_serialize_default, custom_deserialize_default};

CAMLprim value ocaml_samplerate_new(value converter, value channels) {
  CAMLparam2(converter, channels);
  int err;
  SRC_STATE *state = src_new(Int_val(converter), Int_val(channels), &err);
  value ans;

  ans = caml_alloc_custom(&state_ops, sizeof(SRC_STATE *), 1, 0);
  assert(state); /* TODO: raise depending on err */
  State_val(ans) = state;

  CAMLreturn(ans);
}

CAMLprim value ocaml_samplerate_process(value src, value _ratio, value _inbuf,
                                        value _inbufofs, value _inbuflen,
                                        value _outbuf, value _outbufofs,
                                        value _outbuflen) {
  CAMLparam4(src, _ratio, _inbuf, _outbuf);
  CAMLlocal1(ans);
  SRC_DATA data;
  SRC_STATE *state = State_val(src);
  int channels = src_get_channels(state);
  int inbufofs = Int_val(_inbufofs) * channels;
  int inbuflen = Int_val(_inbuflen);
  int outbufofs = Int_val(_outbufofs) * channels;
  int outbuflen = Int_val(_outbuflen);
  float *inbuf, *outbuf;
  int i;

  inbuf = malloc(inbuflen * channels * sizeof(float));
  if (inbuf == NULL)
    caml_raise_out_of_memory();
  for (i = 0; i < inbuflen * channels; i++)
    inbuf[i] = Double_field(_inbuf, inbufofs + i);

  data.src_ratio = Double_val(_ratio);
  caml_release_runtime_system();

  outbuf = malloc(outbuflen * channels * sizeof(float));
  if (outbuf == NULL) {
    free(inbuf);
    caml_acquire_runtime_system();
    caml_raise_out_of_memory();
  }

  data.data_in = inbuf;
  data.input_frames = inbuflen;
  data.data_out = outbuf;
  data.output_frames = outbuflen;
  if (inbuflen == 0)
    data.end_of_input = 1;
  else
    data.end_of_input = 0;

  int ret = src_process(state, &data);
  free(inbuf);

  caml_acquire_runtime_system();

  if (ret != 0) {
    free(outbuf);
    char msg[256];
    snprintf(msg, sizeof(msg), "Samplerate.process: %s", src_strerror(ret));
    caml_failwith(msg);
  }

  for (i = 0; i < data.output_frames_gen * channels; i++)
    Store_double_field(_outbuf, outbufofs + i, clip(outbuf[i]));
  free(outbuf);

  ans = caml_alloc_tuple(2);
  Store_field(ans, 0, Val_int(data.input_frames_used));
  Store_field(ans, 1, Val_int(data.output_frames_gen));

  CAMLreturn(ans);
}

CAMLprim value ocaml_samplerate_process_byte(value *argv, int argc) {
  return ocaml_samplerate_process(argv[0], argv[1], argv[2], argv[3], argv[4],
                                  argv[5], argv[6], argv[7]);
}

CAMLprim value ocaml_samplerate_process_ba(value src, value _ratio,
                                           value _inbuf, value _outbuf) {
  CAMLparam4(src, _ratio, _inbuf, _outbuf);
  CAMLlocal1(ans);

  SRC_DATA data;
  SRC_STATE *state = State_val(src);
  int channels = src_get_channels(state);
  float ratio = Double_val(_ratio);
  struct caml_ba_array *inba = Caml_ba_array_val(_inbuf);
  struct caml_ba_array *outba = Caml_ba_array_val(_outbuf);

  caml_release_runtime_system();

  data.data_in = inba->data;
  data.input_frames = inba->dim[0] / channels;
  data.data_out = outba->data;
  data.output_frames = outba->dim[0] / channels;
  data.src_ratio = ratio;
  if (data.input_frames == 0)
    data.end_of_input = 1;
  else
    data.end_of_input = 0;

  int ret = src_process(state, &data);

  caml_acquire_runtime_system();

  if (ret != 0)
    caml_failwith(src_strerror(ret));

  ans = caml_alloc_tuple(2);
  Store_field(ans, 0, Val_int(data.input_frames_used));
  Store_field(ans, 1, Val_int(data.output_frames_gen));

  CAMLreturn(ans);
}

CAMLprim value ocaml_samplerate_process_alloc(value src, value _ratio,
                                              value _inbuf, value _inbufofs,
                                              value _inbuflen) {
  CAMLparam3(src, _ratio, _inbuf);
  CAMLlocal1(ans);
  SRC_DATA data;
  SRC_STATE *state = State_val(src);
  int channels = src_get_channels(state);
  int inbufofs = Int_val(_inbufofs) * channels;
  int inbuflen = Int_val(_inbuflen);
  float ratio = Double_val(_ratio);
  int outbuflen = (int)(inbuflen * ratio) + 64;
  int anslen;
  float *inbuf, *outbuf;
  int i;

  inbuf = malloc(inbuflen * channels * sizeof(float));
  if (inbuf == NULL)
    caml_raise_out_of_memory();
  for (i = 0; i < inbuflen * channels; i++)
    inbuf[i] = Double_field(_inbuf, inbufofs + i);
  outbuf = malloc(outbuflen * channels * sizeof(float));
  if (outbuf == NULL) {
    free(inbuf);
    caml_raise_out_of_memory();
  }
  data.data_in = inbuf;
  data.input_frames = inbuflen;
  data.data_out = outbuf;
  data.output_frames = outbuflen;
  data.src_ratio = ratio;
  if (inbuflen == 0)
    data.end_of_input = 1;
  else
    data.end_of_input = 0;

  caml_release_runtime_system();
  int ret = src_process(state, &data);
  caml_acquire_runtime_system();
  free(inbuf);
  if (ret != 0) {
    free(outbuf);
    caml_failwith(src_strerror(ret));
  }

  anslen = data.output_frames_gen * channels;
  ans = caml_alloc(anslen * Double_wosize, Double_array_tag);

  for (i = 0; i < anslen; i++)
    Store_double_field(ans, i, clip(outbuf[i]));
  free(outbuf);

  CAMLreturn(ans);
}

CAMLprim value ocaml_samplerate_reset(value src) {
  src_reset(State_val(src));

  return Val_unit;
}
