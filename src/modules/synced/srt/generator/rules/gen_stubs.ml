let c_headers =
  {|
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/threads.h>
#include <pthread.h>
#include <srt/srt.h>
#include <stdio.h>

#define MAX_LOG_STRING 1024

typedef struct {
  int level;
  char file[MAX_LOG_STRING];
  int line;
  char area[MAX_LOG_STRING];
  char message[MAX_LOG_STRING];
  void *next;
} log_msg_t;

static pthread_cond_t log_condition = PTHREAD_COND_INITIALIZER;
static pthread_mutex_t log_mutex = PTHREAD_MUTEX_INITIALIZER;
static log_msg_t top_level_log_msg = {0, "", 0, "", "", NULL};

CAMLprim value ocaml_srt_process_log(value cb) {
  CAMLparam1(cb);
  CAMLlocal1(payload);
  log_msg_t *log_msg, *next_log_msg;

  while (1) {
    caml_release_runtime_system();
    pthread_mutex_lock(&log_mutex);

    while (top_level_log_msg.next == NULL)
      pthread_cond_wait(&log_condition, &log_mutex);

    log_msg = top_level_log_msg.next;
    top_level_log_msg.next = NULL;
    pthread_mutex_unlock(&log_mutex);

    caml_acquire_runtime_system();

    while (log_msg != NULL) {
      payload = caml_alloc_tuple(5);
      Store_field(payload, 0, Val_int(log_msg->level));
      Store_field(payload, 1, caml_alloc_initialized_string(strnlen(log_msg->file, MAX_LOG_STRING), log_msg->file));
      Store_field(payload, 2, Val_int(log_msg->line));
      Store_field(payload, 3, caml_alloc_initialized_string(strnlen(log_msg->area, MAX_LOG_STRING), log_msg->area));
      Store_field(payload, 4, caml_alloc_initialized_string(strnlen(log_msg->message, MAX_LOG_STRING), log_msg->message));

      caml_callback(cb, payload);

      next_log_msg = log_msg->next;
      free(log_msg);
      log_msg = next_log_msg;
    }
  }

  CAMLreturn(Val_unit);
}

void ocaml_srt_log_handler(void *opaque, int level, const char *file, int line,
                           const char *area, const char *message) {
  pthread_mutex_lock(&log_mutex);

  log_msg_t *log_msg = &top_level_log_msg;

  while (log_msg->next != NULL) {
    log_msg = log_msg->next;
  }

  // TODO: check for NULL here
  log_msg->next = malloc(sizeof(log_msg_t));

  log_msg = (log_msg_t *)log_msg->next;
  log_msg->next = NULL;

  log_msg->level = level;
  memcpy(log_msg->file, file, strnlen(file, MAX_LOG_STRING));
  log_msg->line -= line;
  memcpy(log_msg->area, message, strnlen(area, MAX_LOG_STRING));
  memcpy(log_msg->message, message, strnlen(message, MAX_LOG_STRING));

  pthread_cond_signal(&log_condition);
  pthread_mutex_unlock(&log_mutex);
}

CAMLprim value ocaml_srt_setup_log_callback(value unit) {
  srt_setloghandler(NULL, &ocaml_srt_log_handler);
  return Val_unit;
}

CAMLprim value ocaml_srt_clear_log_callback() {
  srt_setloghandler(NULL, NULL);
  return Val_unit;
}
|}

let locked_c_headers = {|
#include <string.h>
|}

let () =
  let mode = Sys.argv.(1) in
  let fname = Sys.argv.(2) in
  let locked = Array.length Sys.argv > 3 in
  let oc = open_out_bin fname in
  let format = Format.formatter_of_out_channel oc in
  let fn =
    match mode with
      | "ml" -> Cstubs.write_ml
      | "c" ->
          if locked then Format.fprintf format "%s@\n" locked_c_headers
          else Format.fprintf format "%s@\n" c_headers;
          Cstubs.write_c
      | _ -> assert false
  in
  if locked then fn format ~prefix:"ocaml_srt" (module Srt_stubs_locked.Def)
  else
    fn ~concurrency:Cstubs.unlocked format ~prefix:"ocaml_srt"
      (module Srt_stubs.Def);
  Format.pp_print_flush format ();
  close_out oc
