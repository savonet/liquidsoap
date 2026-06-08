/*
 * Copyright 2007-2010 Savonet team
 *
 * This file is part of ocaml-ladspa.
 *
 * ocaml-ladspa is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-ladspa is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-ladspa; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * As a special exception to the GNU Library General Public License, you may
 * link, statically or dynamically, a "work that uses the Library" with a
 * publicly distributed version of the Library to produce an executable file
 * containing portions of the Library, and distribute that executable file under
 * terms of your choice, without any of the additional requirements listed in
 * clause 6 of the GNU Library General Public License. By "a publicly
 * distributed version of the Library", we mean either the unmodified Library as
 * distributed by The Savonet Team, or a modified version of the Library that is
 * distributed under the conditions defined in clause 3 of the GNU Library
 * General Public License. This exception does not however invalidate any other
 * reasons why the executable file might be covered by the GNU Library General
 * Public License.
 *
 */

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include <assert.h>
#include <dlfcn.h>
#include <math.h>
#include <stdio.h>

#include "ocaml_ladspa.h"

#include "ocaml_ladspa_config.h"

#ifdef HAS_LADSPA
#include <ladspa.h>
#else
#include "ladspa.h"
#endif

#define Descr_val(v) (*(void **)Data_abstract_val(v))

static inline value value_of_descr(value ret, void *d) {
  ret = caml_alloc(1, Abstract_tag);
  Descr_val(ret) = d;
  return ret;
}

CAMLprim value ocaml_ladspa_version(value unit) {
  return caml_copy_string(LADSPA_VERSION);
}

CAMLprim value ocaml_ladspa_version_major(value unit) {
  return Val_int(LADSPA_VERSION_MAJOR);
}

CAMLprim value ocaml_ladspa_version_minor(value unit) {
  return Val_int(LADSPA_VERSION_MINOR);
}

CAMLprim value ocaml_ladspa_open(value fname) {
  CAMLparam0();
  CAMLlocal1(tmp);
  void *handle = dlopen(String_val(fname), RTLD_LAZY);
  LADSPA_Descriptor_Function ladspa_descriptor;

  if (!handle)
    caml_raise_constant(*caml_named_value("ocaml_ladspa_exn_not_a_plugin"));

  ladspa_descriptor =
      (LADSPA_Descriptor_Function)dlsym((void *)handle, "ladspa_descriptor");

  if (dlerror() != NULL || !ladspa_descriptor) {
    dlclose(handle);
    caml_raise_constant(*caml_named_value("ocaml_ladspa_exn_not_a_plugin"));
  }

  CAMLreturn(value_of_descr(tmp, handle));
}

CAMLprim value ocaml_ladspa_close(value handle) {
  CAMLparam1(handle);
  dlclose(Descr_val(handle));
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_ladspa_descriptor(value handle, value n) {
  CAMLparam1(handle);
  CAMLlocal1(ret);
  LADSPA_Descriptor_Function ladspa_descriptor =
      (LADSPA_Descriptor_Function)dlsym(Descr_val(handle), "ladspa_descriptor");
  const LADSPA_Descriptor *d = ladspa_descriptor(Int_val(n));

  if (!d)
    caml_raise_constant(*caml_named_value("ocaml_ladspa_exn_not_found"));

  CAMLreturn(value_of_ladspa_descr(ret, d));
}

CAMLprim value ocaml_ladspa_unique_id(value d) {
  CAMLparam1(d);
  CAMLreturn(Val_int(LADSPA_descr_val(d)->UniqueID));
}

CAMLprim value ocaml_ladspa_label(value d) {
  CAMLparam1(d);
  CAMLreturn(caml_copy_string(LADSPA_descr_val(d)->Label));
}

CAMLprim value ocaml_ladspa_name(value d) {
  CAMLparam1(d);
  CAMLreturn(caml_copy_string(LADSPA_descr_val(d)->Name));
}

CAMLprim value ocaml_ladspa_maker(value d) {
  CAMLparam1(d);
  CAMLreturn(caml_copy_string(LADSPA_descr_val(d)->Maker));
}

CAMLprim value ocaml_ladspa_copyright(value d) {
  CAMLparam1(d);
  CAMLreturn(caml_copy_string(LADSPA_descr_val(d)->Copyright));
}

CAMLprim value ocaml_ladspa_port_count(value d) {
  CAMLparam1(d);
  CAMLreturn(Val_int(LADSPA_descr_val(d)->PortCount));
}

CAMLprim value ocaml_ladspa_port_names(value d) {
  CAMLparam1(d);
  CAMLlocal1(ans);
  const char *const *pn = LADSPA_descr_val(d)->PortNames;
  int i;
  int n = LADSPA_descr_val(d)->PortCount;

  ans = caml_alloc_tuple(n);
  for (i = 0; i < n; i++)
    Store_field(ans, i, caml_copy_string(pn[i]));

  CAMLreturn(ans);
}

CAMLprim value ocaml_ladspa_port_is_input(value d, value n) {
  CAMLparam1(d);
  CAMLreturn(Val_bool(
      LADSPA_IS_PORT_INPUT(LADSPA_descr_val(d)->PortDescriptors[Int_val(n)])));
}

CAMLprim value ocaml_ladspa_port_is_output(value d, value n) {
  CAMLparam1(d);
  CAMLreturn(Val_bool(
      LADSPA_IS_PORT_OUTPUT(LADSPA_descr_val(d)->PortDescriptors[Int_val(n)])));
}

CAMLprim value ocaml_ladspa_port_is_control(value d, value n) {
  CAMLparam1(d);
  CAMLreturn(Val_bool(LADSPA_IS_PORT_CONTROL(
      LADSPA_descr_val(d)->PortDescriptors[Int_val(n)])));
}

CAMLprim value ocaml_ladspa_port_is_audio(value d, value n) {
  CAMLparam1(d);
  CAMLreturn(Val_bool(
      LADSPA_IS_PORT_AUDIO(LADSPA_descr_val(d)->PortDescriptors[Int_val(n)])));
}

CAMLprim value ocaml_ladspa_port_is_integer(value d, value n) {
  CAMLparam1(d);
  CAMLreturn(Val_bool(LADSPA_IS_HINT_INTEGER(
      LADSPA_descr_val(d)->PortRangeHints[Int_val(n)].HintDescriptor)));
}

CAMLprim value ocaml_ladspa_port_is_boolean(value d, value n) {
  CAMLparam1(d);
  CAMLreturn(Val_bool(LADSPA_IS_HINT_TOGGLED(
      LADSPA_descr_val(d)->PortRangeHints[Int_val(n)].HintDescriptor)));
}

CAMLprim value ocaml_ladspa_port_is_logarithmic(value d, value n) {
  CAMLparam1(d);
  CAMLreturn(Val_bool(LADSPA_IS_HINT_LOGARITHMIC(
      LADSPA_descr_val(d)->PortRangeHints[Int_val(n)].HintDescriptor)));
}

CAMLprim value ocaml_ladspa_port_get_default(value d, value samplerate,
                                             value n) {
  CAMLparam1(d);
  CAMLlocal1(ans);

  assert(
      LADSPA_IS_PORT_CONTROL(LADSPA_descr_val(d)->PortDescriptors[Int_val(n)]));

  const LADSPA_PortRangeHint ri =
      LADSPA_descr_val(d)->PortRangeHints[Int_val(n)];
  LADSPA_PortRangeHintDescriptor h = ri.HintDescriptor;
  float lower = ri.LowerBound;
  float upper = ri.UpperBound;
  float def = 0.;

  if (LADSPA_IS_HINT_SAMPLE_RATE(h)) {
    lower *= Int_val(samplerate);
    upper *= Int_val(samplerate);
  }

  if LADSPA_IS_HINT_HAS_DEFAULT (h) {
    if LADSPA_IS_HINT_DEFAULT_MINIMUM (h)
      def = lower;
    else if LADSPA_IS_HINT_DEFAULT_LOW (h)
      if LADSPA_IS_HINT_LOGARITHMIC (h)
        def = exp(log(lower) * 0.75 + log(upper) * 0.25);
      else
        def = lower * 0.75 + upper * 0.25;
    else if LADSPA_IS_HINT_DEFAULT_MIDDLE (h)
      if LADSPA_IS_HINT_LOGARITHMIC (h)
        def = exp(log(lower) * 0.5 + log(upper) * 0.5);
      else
        def = lower * 0.5 + upper * 0.5;
    else if LADSPA_IS_HINT_DEFAULT_HIGH (h)
      if LADSPA_IS_HINT_LOGARITHMIC (h)
        def = exp(log(lower) * 0.25 + log(upper) * 0.75);
      else
        def = lower * 0.25 + upper * 0.75;
    else if LADSPA_IS_HINT_DEFAULT_MAXIMUM (h)
      def = upper;
    else if LADSPA_IS_HINT_DEFAULT_0 (h)
      def = 0;
    else if LADSPA_IS_HINT_DEFAULT_1 (h)
      def = 1;
    else if LADSPA_IS_HINT_DEFAULT_100 (h)
      def = 100;
    else if LADSPA_IS_HINT_DEFAULT_440 (h)
      def = 440;

    ans = caml_alloc(1, 0);
    Store_field(ans, 0, caml_copy_double(def));
    CAMLreturn(ans);
  } else
    CAMLreturn(Val_int(0));
}

CAMLprim value ocaml_ladspa_port_get_min(value d, value samplerate, value n) {
  CAMLparam1(d);
  CAMLlocal1(ans);
  const LADSPA_PortRangeHint ri =
      LADSPA_descr_val(d)->PortRangeHints[Int_val(n)];
  LADSPA_Data bound;

  assert(
      LADSPA_IS_PORT_CONTROL(LADSPA_descr_val(d)->PortDescriptors[Int_val(n)]));

  if (!LADSPA_IS_HINT_BOUNDED_BELOW(ri.HintDescriptor))
    CAMLreturn(Val_int(0));

  bound = ri.LowerBound;
  if (LADSPA_IS_HINT_SAMPLE_RATE(ri.HintDescriptor))
    bound *= Int_val(samplerate);
  ans = caml_alloc(1, 0);
  Store_field(ans, 0, caml_copy_double(bound));
  CAMLreturn(ans);
}

CAMLprim value ocaml_ladspa_port_get_max(value d, value samplerate, value n) {
  CAMLparam1(d);
  CAMLlocal1(ans);
  const LADSPA_PortRangeHint ri =
      LADSPA_descr_val(d)->PortRangeHints[Int_val(n)];
  LADSPA_Data bound;

  assert(
      LADSPA_IS_PORT_CONTROL(LADSPA_descr_val(d)->PortDescriptors[Int_val(n)]));

  if (!LADSPA_IS_HINT_BOUNDED_ABOVE(ri.HintDescriptor))
    CAMLreturn(Val_int(0));

  bound = ri.UpperBound;
  if (LADSPA_IS_HINT_SAMPLE_RATE(ri.HintDescriptor))
    bound *= Int_val(samplerate);
  ans = caml_alloc(1, 0);
  Store_field(ans, 0, caml_copy_double(bound));
  CAMLreturn(ans);
}

/***** Instances ****/

static void finalize_instance(value inst) {
  ladspa_instance *instance = Instance_val(inst);
  int i;

  instance->descr->cleanup(instance->handle);
  for (i = 0; i < instance->descr->PortCount; i++) {
    if (instance->vbuf[i])
      caml_remove_generational_global_root(&instance->vbuf[i]);
  }
  free(instance->vbuf);
  free(instance);
}

static struct custom_operations instance_ops = {
    "ocaml_ladspa_instance",  finalize_instance,
    custom_compare_default,   custom_hash_default,
    custom_serialize_default, custom_deserialize_default};

CAMLprim value ocaml_ladspa_instantiate(value d, value rate) {
  CAMLparam2(d, rate);
  CAMLlocal1(ans);
  ladspa_instance *instance = malloc(sizeof(ladspa_instance));
  int ports;
  int i;

  instance->descr = LADSPA_descr_val(d);
  ports = instance->descr->PortCount;
  instance->handle =
      instance->descr->instantiate(instance->descr, Int_val(rate));
  instance->vbuf = malloc(ports * sizeof(value));
  for (i = 0; i < ports; i++)
    instance->vbuf[i] = 0;

  ans = caml_alloc_custom(&instance_ops, sizeof(ladspa_instance *), 1, 0);
  Instance_val(ans) = instance;

  CAMLreturn(ans);
}

CAMLprim value ocaml_ladspa_get_descriptor(value i) {
  CAMLparam1(i);
  CAMLlocal1(ret);
  ladspa_instance *instance = Instance_val(i);

  CAMLreturn(value_of_ladspa_descr(ret, instance->descr));
}

CAMLprim value ocaml_ladspa_connect_port(value i, value _n, value buf) {
  CAMLparam3(i, _n, buf);
  ladspa_instance *instance = Instance_val(i);
  int n = Int_val(_n);

  if (!instance->vbuf[n]) {
    instance->vbuf[n] = buf;
    caml_register_generational_global_root(&instance->vbuf[n]);
  } else
    caml_modify_generational_global_root(&instance->vbuf[n], buf);
  instance->descr->connect_port(instance->handle, n, Caml_ba_data_val(buf));

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_ladspa_activate(value i) {
  CAMLparam1(i);
  ladspa_instance *instance = Instance_val(i);

  if (instance->descr->activate)
    instance->descr->activate(instance->handle);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_ladspa_deactivate(value i) {
  CAMLparam1(i);
  ladspa_instance *instance = Instance_val(i);

  if (instance->descr->deactivate)
    instance->descr->deactivate(instance->handle);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_ladspa_run(value inst, value _samples) {
  CAMLparam2(inst, _samples);
  ladspa_instance *instance = Instance_val(inst);
  int samples = Int_val(_samples);

  caml_release_runtime_system();
  instance->descr->run(instance->handle, samples);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}
