#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

#include <lo/lo.h>

#include <assert.h>
#include <stdio.h>

static void check(int n) {
  if (n)
    caml_raise_constant(*caml_named_value("lo_exn_error"));
}

#define Address_val(v) *((lo_address *)Data_custom_val(v))

static void address_finalize(value a) { lo_address_free(Address_val(a)); }

static struct custom_operations address_ops = {
    "caml_lo_address",   address_finalize,         custom_compare_default,
    custom_hash_default, custom_serialize_default, custom_deserialize_default};

CAMLprim value caml_lo_address_new(value host, value port) {
  CAMLparam2(host, port);
  CAMLlocal1(ans);
  lo_address a;

  a = lo_address_new(String_val(host), String_val(port));
  ans = caml_alloc_custom(&address_ops, sizeof(lo_address), 0, 1);
  Address_val(ans) = a;

  CAMLreturn(ans);
}

CAMLprim value caml_lo_address_default(value unit) {
  CAMLparam0();
  CAMLlocal1(ans);

  lo_address a;

  a = lo_address_new(NULL, NULL);
  ans = caml_alloc_custom(&address_ops, sizeof(lo_address), 0, 1);
  assert(ans);
  Address_val(ans) = a;

  CAMLreturn(ans);
}

#define Message_val(v) *((lo_message *)Data_custom_val(v))

static void message_finalize(value a) { lo_message_free(Message_val(a)); }

static struct custom_operations message_ops = {
    "caml_lo_message",   message_finalize,         custom_compare_default,
    custom_hash_default, custom_serialize_default, custom_deserialize_default};

CAMLprim value caml_lo_message_new(value unit) {
  CAMLparam0();
  CAMLlocal1(ans);
  lo_message m;

  m = lo_message_new();
  ans = caml_alloc_custom(&message_ops, sizeof(lo_message), 0, 1);
  Message_val(ans) = m;

  CAMLreturn(ans);
}

CAMLprim value caml_lo_message_add(value message, value data) {
  CAMLparam2(message, data);
  lo_message m = Message_val(message);
  if (Is_block(data)) {
    value v = Field(data, 0);
    value d = Field(data, 1);

    if (v == caml_hash_variant("Int32"))
      check(lo_message_add_int32(m, Int_val(d)));
    else if (v == caml_hash_variant("Float"))
      check(lo_message_add_float(m, Double_val(d)));
    else if (v == caml_hash_variant("Double"))
      check(lo_message_add_double(m, Double_val(d)));
    else if (v == caml_hash_variant("Char"))
      check(lo_message_add_char(m, Int_val(d)));
    else if (v == caml_hash_variant("String"))
      check(lo_message_add_string(m, String_val(d)));
    else
      /* TODO */
      caml_raise_constant(*caml_named_value("lo_exn_unhandled"));
  } else {
    if (data == caml_hash_variant("True"))
      check(lo_message_add_true(m));
    else if (data == caml_hash_variant("False"))
      check(lo_message_add_false(m));
    else if (data == caml_hash_variant("Nil"))
      check(lo_message_add_nil(m));
    else if (data == caml_hash_variant("Infinitum"))
      check(lo_message_add_infinitum(m));
    else
      caml_raise_constant(*caml_named_value("lo_exn_unhandled"));
  }

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_lo_send_message(value address, value path, value message) {
  CAMLparam3(address, path, message);
  int ret;

  /* TODO: blocking section */
  /* The return value is the one of send, i.e. the length of the sent data or -1
     on error. */
  ret = lo_send_message(Address_val(address), String_val(path),
                        Message_val(message));

  if (ret == -1)
    caml_raise_constant(*caml_named_value("lo_exn_error"));

  CAMLreturn(Val_unit);
}

typedef struct {
  lo_server server;
  value handler;
} server_t;

#define Server_t_val(v) *((server_t **)Data_custom_val(v))
#define Server_val(v) ((Server_t_val(v))->server)

static void server_finalize(value srv) {
  server_t *s = Server_t_val(srv);
  if (s->server)
    lo_server_free(s->server);
  if (s->handler)
    caml_remove_global_root(&s->handler);
  free(s);
}

static struct custom_operations server_ops = {
    "caml_lo_server",    server_finalize,          custom_compare_default,
    custom_hash_default, custom_serialize_default, custom_deserialize_default};

static void error_msg(int num, const char *msg, const char *path) {
  fprintf(stderr, "liblo server error %d in path %s: %s\n", num, path, msg);
}

static int generic_handler(const char *path, const char *types, lo_arg **argv,
                           int argc, lo_message msg, void *user_data) {
  server_t *s = (server_t *)user_data;
  int i;
  value arg = 0;
  value v = 0;

  caml_leave_blocking_section();

  caml_register_global_root(&arg);
  caml_register_global_root(&v);

  arg = caml_alloc_tuple(argc);

  for (i = 0; i < argc; i++) {
    switch (types[i]) {
    case 'i':
      v = caml_alloc_tuple(2);
      Store_field(v, 0, caml_hash_variant("Int32"));
      Store_field(v, 1, Val_int(argv[i]->i));
      break;

    case 'f':
      v = caml_alloc_tuple(2);
      Store_field(v, 0, caml_hash_variant("Float"));
      Store_field(v, 1, caml_copy_double(argv[i]->f));
      break;

    case 'd':
      v = caml_alloc_tuple(2);
      Store_field(v, 0, caml_hash_variant("Double"));
      Store_field(v, 1, caml_copy_double(argv[i]->d));
      break;

    case 'c':
      v = caml_alloc_tuple(2);
      Store_field(v, 0, caml_hash_variant("Char"));
      Store_field(v, 1, Val_int(argv[i]->c));
      break;

    case 's':
      v = caml_alloc_tuple(2);
      Store_field(v, 0, caml_hash_variant("String"));
      Store_field(v, 1, caml_copy_string(&argv[i]->s));
      break;

    case 'T':
      v = caml_hash_variant("True");
      break;

    case 'F':
      v = caml_hash_variant("False");
      break;

    case 'N':
      v = caml_hash_variant("Nil");
      break;

    case 'I':
      v = caml_hash_variant("Infinitum");
      break;

    default:
      printf("Handler not implemented: '%c'\n", types[i]);
      caml_raise_constant(*caml_named_value("lo_exn_unhandled"));
    }

    // printf("message on %s: %c\n", path, types[i]);
    Store_field(arg, i, v);
  }

  caml_callback2(s->handler, caml_copy_string(path), arg);

  caml_remove_global_root(&v);
  caml_remove_global_root(&arg);

  caml_enter_blocking_section();

  return 0;
}

CAMLprim value caml_lo_server_new(value port, value handler) {
  CAMLparam2(port, handler);
  CAMLlocal1(ans);
  server_t *s = malloc(sizeof(server_t));

  s->handler = handler;
  caml_register_global_root(&s->handler);

  /* TODO: custom error message handling */
  s->server = lo_server_new(String_val(port), error_msg);
  if (!s->server)
    caml_raise_constant(*caml_named_value("lo_exn_error"));
  ans = caml_alloc_custom(&server_ops, sizeof(server_t *), 0, 1);
  Server_t_val(ans) = s;
  lo_server_add_method(s->server, NULL, NULL, generic_handler, s);

  CAMLreturn(ans);
}

static void check_server(lo_server s) {
  if (!s)
    caml_raise_constant(*caml_named_value("lo_exn_stopped"));
}

CAMLprim value caml_lo_server_recv(value server) {
  CAMLparam1(server);
  lo_server s = Server_val(server);
  check_server(s);

  caml_enter_blocking_section();
  lo_server_recv(s);
  caml_leave_blocking_section();

  CAMLreturn(Val_unit);
}

CAMLprim value caml_lo_server_free(value server) {
  CAMLparam1(server);
  lo_server s = Server_val(server);

  caml_enter_blocking_section();
  lo_server_free(s);
  caml_leave_blocking_section();

  Server_val(server) = NULL;

  CAMLreturn(Val_unit);
}
