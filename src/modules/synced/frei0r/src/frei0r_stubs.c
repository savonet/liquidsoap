#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/threads.h>

#include <dlfcn.h>

#include <frei0r.h>

#ifndef Bytes_val
#define Bytes_val String_val
#endif

CAMLprim value ocaml_f0r_version(value unit) {
  CAMLparam0();
  CAMLlocal1(ans);
  ans = caml_alloc_tuple(2);
  Store_field(ans, 0, Val_int(FREI0R_MAJOR_VERSION));
  Store_field(ans, 1, Val_int(FREI0R_MINOR_VERSION));
  CAMLreturn(ans);
}

/***** Plugins *****/

typedef struct {
  void *handle;
  int (*init)();
  void (*deinit)();
  void (*get_plugin_info)(f0r_plugin_info_t *);
  void (*get_param_info)(f0r_param_info_t *, int);
  f0r_instance_t (*construct)(unsigned int, unsigned int);
  void (*destruct)(f0r_instance_t);
  void (*get_param_value)(f0r_instance_t, f0r_param_t, int);
  void (*set_param_value)(f0r_instance_t, f0r_param_t, int);
  void (*update)(f0r_instance_t, double, const uint32_t *, uint32_t *);
  void (*update2)(f0r_instance_t, double, const uint32_t *, const uint32_t *,
                  const uint32_t *, uint32_t *);
} plugin_t;

#define Plugin_val(v) (*(plugin_t **)Data_custom_val(v))

static void finalize_plugin(value v) {
  plugin_t *p = Plugin_val(v);
  p->deinit();
  dlclose(p->handle);
  free(p);
}

static struct custom_operations plugin_ops = {
    "ocaml_f0r_plugin",  finalize_plugin,          custom_compare_default,
    custom_hash_default, custom_serialize_default, custom_deserialize_default};

CAMLprim value ocaml_f0r_dlopen(value fname) {
  CAMLparam1(fname);
  CAMLlocal1(ans);
  plugin_t *p = malloc(sizeof(plugin_t));

  p->handle = dlopen(String_val(fname), RTLD_LAZY);
  if (!p->handle) {
    free(p);
    caml_raise_constant(*caml_named_value("f0r_exn_not_a_plugin"));
  }
  p->init = dlsym(p->handle, "f0r_init");
  if (!p->init) {
    dlclose(p->handle);
    free(p);
    caml_raise_constant(*caml_named_value("f0r_exn_not_a_plugin"));
  }
  p->deinit = dlsym(p->handle, "f0r_deinit");
  if (!p->deinit) {
    dlclose(p->handle);
    free(p);
    caml_raise_constant(*caml_named_value("f0r_exn_not_a_plugin"));
  }
  p->get_plugin_info = dlsym(p->handle, "f0r_get_plugin_info");
  if (!p->get_plugin_info) {
    dlclose(p->handle);
    free(p);
    caml_raise_constant(*caml_named_value("f0r_exn_not_a_plugin"));
  }
  p->get_param_info = dlsym(p->handle, "f0r_get_param_info");
  if (!p->get_param_info) {
    dlclose(p->handle);
    free(p);
    caml_raise_constant(*caml_named_value("f0r_exn_not_a_plugin"));
  }
  p->construct = dlsym(p->handle, "f0r_construct");
  if (!p->construct) {
    dlclose(p->handle);
    free(p);
    caml_raise_constant(*caml_named_value("f0r_exn_not_a_plugin"));
  }
  p->destruct = dlsym(p->handle, "f0r_destruct");
  if (!p->destruct) {
    dlclose(p->handle);
    free(p);
    caml_raise_constant(*caml_named_value("f0r_exn_not_a_plugin"));
  }
  p->get_param_value = dlsym(p->handle, "f0r_get_param_value");
  if (!p->get_param_value) {
    dlclose(p->handle);
    free(p);
    caml_raise_constant(*caml_named_value("f0r_exn_not_a_plugin"));
  }
  p->set_param_value = dlsym(p->handle, "f0r_set_param_value");
  if (!p->set_param_value) {
    dlclose(p->handle);
    free(p);
    caml_raise_constant(*caml_named_value("f0r_exn_not_a_plugin"));
  }
  p->update = dlsym(p->handle, "f0r_update");
  p->update2 = dlsym(p->handle, "f0r_update2");
  if (!p->update && !p->update2) {
    dlclose(p->handle);
    free(p);
    caml_raise_constant(*caml_named_value("f0r_exn_not_a_plugin"));
  }

  p->init();
  ans = caml_alloc_custom(&plugin_ops, sizeof(plugin_t *), 0, 1);
  Plugin_val(ans) = p;

  CAMLreturn(ans);
}

CAMLprim value ocaml_f0r_plugin_info(value plugin) {
  CAMLparam1(plugin);
  CAMLlocal1(ans);
  plugin_t *p = Plugin_val(plugin);
  f0r_plugin_info_t info = {0};

  caml_release_runtime_system();
  p->get_plugin_info(&info);
  caml_acquire_runtime_system();

  ans = caml_alloc_tuple(9);
  Store_field(ans, 0, caml_copy_string(info.name ? info.name : ""));
  Store_field(ans, 1, caml_copy_string(info.author ? info.author : ""));
  Store_field(ans, 2, Val_int(info.plugin_type));
  Store_field(ans, 3, Val_int(info.color_model));
  Store_field(ans, 4, Val_int(info.frei0r_version));
  Store_field(ans, 5, Val_int(info.major_version));
  Store_field(ans, 6, Val_int(info.minor_version));
  Store_field(ans, 7, Val_int(info.num_params));
  Store_field(ans, 8,
              caml_copy_string(info.explanation ? info.explanation : ""));

  CAMLreturn(ans);
}

CAMLprim value ocaml_f0r_param_info(value plugin, value parameter) {
  CAMLparam1(plugin);
  CAMLlocal1(ans);
  plugin_t *p = Plugin_val(plugin);
  int param = Int_val(parameter);
  f0r_param_info_t info = {0};

  caml_release_runtime_system();
  p->get_param_info(&info, param);
  caml_acquire_runtime_system();

  ans = caml_alloc_tuple(3);
  Store_field(ans, 0, caml_copy_string(info.name ? info.name : ""));
  Store_field(ans, 1, Val_int(info.type));
  Store_field(ans, 2,
              caml_copy_string(info.explanation ? info.explanation : ""));

  CAMLreturn(ans);
}

/***** Instances *****/

#define Instance_val(v) (*(f0r_instance_t **)Data_custom_val(v))

static struct custom_operations instance_ops = {
    "ocaml_f0r_instance", custom_finalize_default,  custom_compare_default,
    custom_hash_default,  custom_serialize_default, custom_deserialize_default};

CAMLprim value ocaml_f0r_plugin_destruct(value plugin, value instance) {
  CAMLparam2(plugin, instance);
  plugin_t *p = Plugin_val(plugin);
  f0r_instance_t *i = Instance_val(instance);
  p->destruct(i);
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_f0r_construct(value plugin, value width, value height) {
  CAMLparam1(plugin);
  CAMLlocal1(ans);
  plugin_t *p = Plugin_val(plugin);
  int w = Int_val(width);
  int h = Int_val(height);
  f0r_instance_t *instance;

  caml_release_runtime_system();
  instance = p->construct(w, h);
  caml_acquire_runtime_system();

  if (!instance)
    caml_raise_constant(*caml_named_value("f0r_exn_failure"));

  ans = caml_alloc_custom(&instance_ops, sizeof(f0r_instance_t *), 0, 1);
  Instance_val(ans) = instance;
  CAMLreturn(ans);
}

CAMLprim value ocaml_f0r_get_param_bool(value plugin, value instance,
                                        value parameter) {
  CAMLparam3(plugin, instance, parameter);
  f0r_instance_t *i = Instance_val(instance);
  plugin_t *p = Plugin_val(plugin);
  int param = Int_val(parameter);
  f0r_param_bool x;

  caml_release_runtime_system();
  p->get_param_value(i, &x, param);
  caml_acquire_runtime_system();

  CAMLreturn(Val_bool(x >= 0.5));
}

CAMLprim value ocaml_f0r_get_param_double(value plugin, value instance,
                                          value parameter) {
  CAMLparam3(plugin, instance, parameter);
  f0r_instance_t *i = Instance_val(instance);
  plugin_t *p = Plugin_val(plugin);
  int param = Int_val(parameter);
  f0r_param_double x;

  caml_release_runtime_system();
  p->get_param_value(i, &x, param);
  caml_acquire_runtime_system();

  CAMLreturn(caml_copy_double(x));
}

CAMLprim value ocaml_f0r_get_param_color(value plugin, value instance,
                                         value parameter) {
  CAMLparam3(plugin, instance, parameter);
  CAMLlocal1(ans);
  f0r_instance_t *i = Instance_val(instance);
  plugin_t *p = Plugin_val(plugin);
  int param = Int_val(parameter);
  f0r_param_color_t x;

  caml_release_runtime_system();
  p->get_param_value(i, &x, param);
  caml_acquire_runtime_system();

  ans = caml_alloc_tuple(3);
  Store_field(ans, 0, caml_copy_double(x.r));
  Store_field(ans, 1, caml_copy_double(x.g));
  Store_field(ans, 2, caml_copy_double(x.b));

  CAMLreturn(ans);
}

CAMLprim value ocaml_f0r_get_param_position(value plugin, value instance,
                                            value parameter) {
  CAMLparam3(plugin, instance, parameter);
  CAMLlocal1(ans);
  f0r_instance_t *i = Instance_val(instance);
  plugin_t *p = Plugin_val(plugin);
  int param = Int_val(parameter);
  f0r_param_position_t x;

  caml_release_runtime_system();
  p->get_param_value(i, &x, param);
  caml_acquire_runtime_system();

  ans = caml_alloc_tuple(2);
  Store_field(ans, 0, caml_copy_double(x.x));
  Store_field(ans, 1, caml_copy_double(x.y));

  CAMLreturn(ans);
}

CAMLprim value ocaml_f0r_get_param_string(value plugin, value instance,
                                          value parameter) {
  CAMLparam3(plugin, instance, parameter);
  f0r_instance_t *i = Instance_val(instance);
  plugin_t *p = Plugin_val(plugin);
  int param = Int_val(parameter);
  /* TODO: can we avoid buffer overflows?... */
  char val[1024];

  caml_release_runtime_system();
  p->get_param_value(i, val, param);
  caml_acquire_runtime_system();

  CAMLreturn(caml_copy_string(val));
}

CAMLprim value ocaml_f0r_set_param_bool(value plugin, value instance,
                                        value parameter, value val) {
  CAMLparam4(plugin, instance, parameter, val);
  f0r_instance_t *i = Instance_val(instance);
  plugin_t *p = Plugin_val(plugin);
  int param = Int_val(parameter);
  f0r_param_bool x = Bool_val(val) ? 1. : 0.;

  caml_release_runtime_system();
  p->set_param_value(i, &x, param);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_f0r_set_param_double(value plugin, value instance,
                                          value parameter, value val) {
  CAMLparam4(plugin, instance, parameter, val);
  f0r_instance_t *i = Instance_val(instance);
  plugin_t *p = Plugin_val(plugin);
  int param = Int_val(parameter);
  f0r_param_double x = Double_val(val);

  caml_release_runtime_system();
  p->set_param_value(i, &x, param);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_f0r_set_param_color(value plugin, value instance,
                                         value parameter, value val) {
  CAMLparam4(plugin, instance, parameter, val);
  f0r_instance_t *i = Instance_val(instance);
  plugin_t *p = Plugin_val(plugin);
  int param = Int_val(parameter);
  f0r_param_color_t c;

  c.r = Double_val(Field(val, 0));
  c.g = Double_val(Field(val, 1));
  c.b = Double_val(Field(val, 2));

  caml_release_runtime_system();
  p->set_param_value(i, &c, param);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_f0r_set_param_position(value plugin, value instance,
                                            value parameter, value val) {
  CAMLparam4(plugin, instance, parameter, val);
  f0r_instance_t *i = Instance_val(instance);
  plugin_t *p = Plugin_val(plugin);
  int param = Int_val(parameter);
  f0r_param_position_t pos;

  pos.x = Double_val(Field(val, 0));
  pos.y = Double_val(Field(val, 1));

  caml_release_runtime_system();
  p->set_param_value(i, &pos, param);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_f0r_set_param_string(value plugin, value instance,
                                          value parameter, value val) {
  CAMLparam4(plugin, instance, parameter, val);
  f0r_instance_t *i = Instance_val(instance);
  plugin_t *p = Plugin_val(plugin);
  int param = Int_val(parameter);

  p->set_param_value(i, Bytes_val(val), param);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_f0r_update(value plugin, value instance, value time,
                                value inframe, value outframe) {
  CAMLparam5(plugin, instance, time, inframe, outframe);
  f0r_instance_t *i = Instance_val(instance);
  plugin_t *p = Plugin_val(plugin);
  double t = Double_val(time);
  const uint32_t *in;
  uint32_t *out = Caml_ba_data_val(outframe);

  if (Is_block(inframe))
    in = Caml_ba_data_val(Field(inframe, 0));
  else
    in = NULL;

  caml_release_runtime_system();
  if (p->update)
    p->update(i, t, in, out);
  else
    p->update2(i, t, in, NULL, NULL, out);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_f0r_update2(value plugin, value instance, value time,
                                 value inframe1, value inframe2, value inframe3,
                                 value outframe) {
  CAMLparam5(plugin, instance, time, inframe1, inframe2);
  CAMLxparam2(inframe3, outframe);
  f0r_instance_t *i = Instance_val(instance);
  plugin_t *p = Plugin_val(plugin);
  double t = Double_val(time);
  const uint32_t *in1, *in2, *in3;
  uint32_t *out = Caml_ba_data_val(outframe);

  in1 = Is_block(inframe1) ? Caml_ba_data_val(Field(inframe1, 0)) : NULL;
  in2 = Is_block(inframe2) ? Caml_ba_data_val(Field(inframe2, 0)) : NULL;
  in3 = Is_block(inframe3) ? Caml_ba_data_val(Field(inframe3, 0)) : NULL;

  caml_release_runtime_system();
  p->update2(i, t, in1, in2, in3, out);
  caml_acquire_runtime_system();

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_f0r_update2_byte(value *argv, int argn) {
  return ocaml_f0r_update2(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5],
                           argv[6]);
}
