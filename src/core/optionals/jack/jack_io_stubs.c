#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include "jack_stubs.h"

#include <math.h>
#include <stdatomic.h>
#include <stdlib.h>

#define JACK_MAX_PORTS 12

/* Per-server timing state: one instance shared by all sources on the same server */
typedef struct {
  jack_sem_t           semaphore;
  _Atomic jack_time_t  current_usecs;  /* raw microseconds from jack_get_cycle_times */
  _Atomic double       sleep_target;   /* seconds, set from OCaml */
  _Atomic int          stopped;
} jack_server_state_t;

typedef struct {
  _Atomic(jack_port_t *)       port;
  _Atomic(jack_ringbuffer_t *) ringbuffer;
  _Atomic size_t               dropped_frames;
  int                          is_input;
} jack_port_state_t;

typedef struct {
  jack_client_t       *jack_client;   /* set before process callback fires */
  jack_server_state_t *server_state;  /* shared with other sources on the same server */
  jack_port_state_t    ports[JACK_MAX_PORTS];
} jack_source_t;

#define ServerState_val(v) (*((jack_server_state_t **)Data_custom_val(v)))
#define Source_val(v)      (*((jack_source_t **)Data_custom_val(v)))

static void server_state_finalize(value _server_state_block)
{
  jack_server_state_t *state = ServerState_val(_server_state_block);
  jack_sem_destroy(&state->semaphore);
  free(state);
}

static struct custom_operations server_state_ops = {
  "liquidsoap_jack_server_state",
  server_state_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static void source_finalize(value _source_block)
{
  free(Source_val(_source_block));
}

static struct custom_operations source_ops = {
  "liquidsoap_jack_source",
  source_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static void jack_source_process_ports(jack_source_t *source, jack_nframes_t nframes)
{
  for (int i = 0; i < JACK_MAX_PORTS; i++) {
    jack_port_t *port = atomic_load_explicit(&source->ports[i].port, memory_order_acquire);
    if (!port) continue;
    float *jack_buf = jack_port_get_buffer(port, nframes);
    jack_ringbuffer_t *rb = atomic_load_explicit(&source->ports[i].ringbuffer, memory_order_relaxed);
    if (source->ports[i].is_input) {
      if (!rb) continue;
      size_t written = jack_ringbuffer_write(rb, (const char *)jack_buf, nframes * sizeof(float));
      size_t dropped = nframes - written / sizeof(float);
      if (dropped)
        atomic_fetch_add_explicit(&source->ports[i].dropped_frames, dropped, memory_order_relaxed);
    } else {
      size_t got_frames = 0;
      if (rb) {
        size_t got = jack_ringbuffer_read(rb, (char *)jack_buf, nframes * sizeof(float));
        got_frames = got / sizeof(float);
      }
      size_t dropped = (size_t)nframes - got_frames;
      if (dropped) {
        atomic_fetch_add_explicit(&source->ports[i].dropped_frames, dropped, memory_order_relaxed);
        for (size_t j = got_frames; j < (size_t)nframes; j++) jack_buf[j] = 0.f;
      }
    }
  }
}

static int jack_source_process_callback(jack_nframes_t nframes, void *arg)
{
  jack_source_t *source = (jack_source_t *)arg;
  jack_server_state_t *state = source->server_state;

  jack_nframes_t current_frames;
  jack_time_t    current_usecs, next_usecs;
  float          period_usecs;
  jack_get_cycle_times(source->jack_client,
                       &current_frames, &current_usecs, &next_usecs, &period_usecs);

  jack_source_process_ports(source, nframes);

  /* Only the first source to advance current_usecs for this cycle posts the
     semaphore. Others see the same integer value and the CAS fails.
     We compare next_usecs (predicted start of next cycle) against the target so
     the OCaml thread wakes a full period early and has time to prepare data. */
  jack_time_t old_usecs = atomic_load_explicit(&state->current_usecs, memory_order_relaxed);
  if (old_usecs != current_usecs &&
      atomic_compare_exchange_strong_explicit(&state->current_usecs, &old_usecs, current_usecs,
                                             memory_order_release, memory_order_relaxed)) {
    double next_sec = (double)next_usecs * 1e-6;
    double target   = atomic_load_explicit(&state->sleep_target, memory_order_acquire);
    if (atomic_load_explicit(&state->stopped, memory_order_relaxed) || next_sec >= target) {
      atomic_store_explicit(&state->sleep_target, INFINITY, memory_order_relaxed);
      jack_sem_post(&state->semaphore);
    }
  }

  return 0;
}

CAMLprim value caml_jack_server_state_create(value _unit)
{
  CAMLparam1(_unit);
  CAMLlocal1(_server_state_block);
  jack_server_state_t *state = calloc(1, sizeof(jack_server_state_t));
  if (!state) caml_failwith("jack_server_state_create: out of memory");
  jack_sem_init(&state->semaphore);
  atomic_store(&state->current_usecs, (jack_time_t)0);
  atomic_store(&state->sleep_target, INFINITY);
  atomic_store(&state->stopped, 0);
  _server_state_block = caml_alloc_custom(&server_state_ops,
                                          sizeof(jack_server_state_t *), 0, 1);
  ServerState_val(_server_state_block) = state;
  CAMLreturn(_server_state_block);
}

CAMLprim value caml_jack_server_state_set_stopped(value _server_state_block, value _stopped)
{
  atomic_store_explicit(&ServerState_val(_server_state_block)->stopped,
                        Bool_val(_stopped), memory_order_relaxed);
  return Val_unit;
}

CAMLprim value caml_jack_server_state_get_stopped(value _server_state_block)
{
  int stopped = atomic_load_explicit(&ServerState_val(_server_state_block)->stopped,
                                     memory_order_relaxed);
  return Val_bool(stopped);
}

CAMLprim value caml_jack_server_state_get_elapsed(value _server_state_block)
{
  CAMLparam1(_server_state_block);
  jack_time_t usecs = atomic_load_explicit(&ServerState_val(_server_state_block)->current_usecs,
                                           memory_order_acquire);
  CAMLreturn(caml_copy_double((double)usecs * 1e-6));
}

CAMLprim value caml_jack_server_state_set_sleep_target(value _server_state_block, value _target)
{
  atomic_store_explicit(&ServerState_val(_server_state_block)->sleep_target,
                        Double_val(_target), memory_order_release);
  return Val_unit;
}

CAMLprim value caml_jack_server_state_wait(value _server_state_block)
{
  CAMLparam1(_server_state_block);
  jack_server_state_t *state = ServerState_val(_server_state_block);
  caml_release_runtime_system();
  jack_sem_wait(&state->semaphore);
  caml_acquire_runtime_system();
  CAMLreturn(Val_unit);
}

CAMLprim value caml_jack_source_create(value _unit)
{
  CAMLparam1(_unit);
  CAMLlocal1(_source_block);
  jack_source_t *source = calloc(1, sizeof(jack_source_t));
  if (!source) caml_failwith("jack_source_create: out of memory");
  _source_block = caml_alloc_custom(&source_ops, sizeof(jack_source_t *), 0, 1);
  Source_val(_source_block) = source;
  CAMLreturn(_source_block);
}

CAMLprim value caml_jack_source_set_client(value _source_block, value _client_block)
{
  Source_val(_source_block)->jack_client = Client_val(_client_block)->client;
  return Val_unit;
}

CAMLprim value caml_jack_source_set_server_state(value _source_block,
                                                  value _server_state_block)
{
  Source_val(_source_block)->server_state = ServerState_val(_server_state_block);
  return Val_unit;
}

CAMLprim value caml_jack_source_register_callback(value _client_block, value _source_block)
{
  CAMLparam2(_client_block, _source_block);
  jack_client_t *client = Client_val(_client_block)->client;
  jack_source_t *source = Source_val(_source_block);
  int result = jack_set_process_callback(client, jack_source_process_callback, source);
  if (result != 0) caml_failwith("jack_source_register_callback: failed");
  CAMLreturn(Val_unit);
}

CAMLprim value caml_jack_source_add_port(value _source_block, value _port_block, value _is_input)
{
  jack_source_t *source = Source_val(_source_block);
  jack_port_t *port = Port_val(_port_block);
  int is_input = Bool_val(_is_input);
  for (int i = 0; i < JACK_MAX_PORTS; i++) {
    if (atomic_load_explicit(&source->ports[i].port, memory_order_relaxed) == NULL) {
      source->ports[i].is_input = is_input;
      atomic_store_explicit(&source->ports[i].dropped_frames, 0, memory_order_relaxed);
      atomic_store_explicit(&source->ports[i].ringbuffer, (jack_ringbuffer_t *)NULL, memory_order_relaxed);
      atomic_store_explicit(&source->ports[i].port, port, memory_order_release);
      return Val_int(i);
    }
  }
  caml_failwith("jack_source_add_port: no free port slot");
}

CAMLprim value caml_jack_source_enable_port(value _source_block, value _index, value _rb_block)
{
  jack_source_t *source = Source_val(_source_block);
  int index = Int_val(_index);
  atomic_store_explicit(&source->ports[index].ringbuffer, Rb_val(_rb_block), memory_order_release);
  return Val_unit;
}

CAMLprim value caml_jack_source_disable_port(value _source_block, value _index)
{
  jack_source_t *source = Source_val(_source_block);
  int index = Int_val(_index);
  atomic_store_explicit(&source->ports[index].ringbuffer, (jack_ringbuffer_t *)NULL, memory_order_relaxed);
  return Val_unit;
}

CAMLprim value caml_jack_source_remove_port(value _source_block, value _index)
{
  jack_source_t *source = Source_val(_source_block);
  int index = Int_val(_index);
  atomic_store_explicit(&source->ports[index].port, (jack_port_t *)NULL, memory_order_release);
  atomic_store_explicit(&source->ports[index].ringbuffer, (jack_ringbuffer_t *)NULL, memory_order_relaxed);
  return Val_unit;
}

CAMLprim value caml_jack_source_get_and_reset_dropped(value _source_block, value _index)
{
  jack_source_t *source = Source_val(_source_block);
  int index = Int_val(_index);
  size_t dropped = atomic_exchange_explicit(&source->ports[index].dropped_frames, 0,
                                            memory_order_relaxed);
  return Val_int((int)dropped);
}
