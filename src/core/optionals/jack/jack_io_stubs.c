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
#include <string.h>

#define JACK_MAX_PORTS 12
#define JACK_MAX_WAITERS 12

/* One slot per concurrent OCaml clock thread waiting on this server. */
typedef struct {
  _Atomic int in_use;
  _Atomic double sleep_target; /* seconds; INFINITY when not waiting */
  jack_sem_t semaphore;
} jack_waiter_t;

/* Per-server timing state: one instance shared by all sources on the same
 * server */
typedef struct {
  jack_waiter_t waiters[JACK_MAX_WAITERS];
  _Atomic jack_time_t
      current_usecs; /* raw microseconds from jack_get_cycle_times */
  _Atomic int stopped;
  _Atomic int active_client_count; /* number of registered clients */
  _Atomic int pending_callbacks;   /* countdown per cycle: last to zero posts */
} jack_server_state_t;

typedef struct {
  _Atomic(jack_port_t *) port;
  _Atomic(jack_ringbuffer_t *) ringbuffer;
  _Atomic size_t dropped_frames;
  int is_input;
} jack_port_state_t;

typedef struct {
  jack_client_t *jack_client; /* set before process callback fires */
  jack_server_state_t
      *server_state; /* shared with other sources on the same server */
  jack_port_state_t ports[JACK_MAX_PORTS];
} jack_source_t;

#define ServerState_val(v) (*((jack_server_state_t **)Data_custom_val(v)))
#define Source_val(v) (*((jack_source_t **)Data_custom_val(v)))

static void server_state_finalize(value _server_state_block) {
  jack_server_state_t *state = ServerState_val(_server_state_block);
  for (int i = 0; i < JACK_MAX_WAITERS; i++)
    jack_sem_destroy(&state->waiters[i].semaphore);
  free(state);
}

static struct custom_operations server_state_ops = {
    "liquidsoap_jack_server_state", server_state_finalize,
    custom_compare_default,         custom_hash_default,
    custom_serialize_default,       custom_deserialize_default,
    custom_compare_ext_default,     custom_fixed_length_default};

static void source_finalize(value _source_block) {
  free(Source_val(_source_block));
}

static struct custom_operations source_ops = {
    "liquidsoap_jack_source",   source_finalize,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

static void jack_source_process_ports(jack_source_t *source,
                                      jack_nframes_t nframes) {
  for (int i = 0; i < JACK_MAX_PORTS; i++) {
    jack_port_t *port =
        atomic_load_explicit(&source->ports[i].port, memory_order_acquire);
    if (!port)
      continue;
    float *jack_buf = jack_port_get_buffer(port, nframes);
    jack_ringbuffer_t *rb = atomic_load_explicit(&source->ports[i].ringbuffer,
                                                 memory_order_relaxed);
    if (source->ports[i].is_input) {
      if (!rb)
        continue;
      size_t written = jack_ringbuffer_write(rb, (const char *)jack_buf,
                                             nframes * sizeof(float));
      size_t dropped = nframes - written / sizeof(float);
      if (dropped)
        atomic_fetch_add_explicit(&source->ports[i].dropped_frames, dropped,
                                  memory_order_relaxed);
    } else {
      if (!rb) {
        memset(jack_buf, 0, nframes * sizeof(float));
        continue;
      }
      size_t got =
          jack_ringbuffer_read(rb, (char *)jack_buf, nframes * sizeof(float));
      size_t got_frames = got / sizeof(float);
      size_t dropped = (size_t)nframes - got_frames;
      if (dropped) {
        atomic_fetch_add_explicit(&source->ports[i].dropped_frames, dropped,
                                  memory_order_relaxed);
        for (size_t j = got_frames; j < (size_t)nframes; j++)
          jack_buf[j] = 0.f;
      }
    }
  }
}

static int jack_source_process_callback(jack_nframes_t nframes, void *arg) {
  jack_source_t *source = (jack_source_t *)arg;
  jack_server_state_t *state = source->server_state;

  jack_nframes_t current_frames;
  jack_time_t current_usecs, next_usecs;
  float period_usecs;
  jack_get_cycle_times(source->jack_client, &current_frames, &current_usecs,
                       &next_usecs, &period_usecs);

  jack_source_process_ports(source, nframes);

  /* On a new cycle (current_usecs changed) the first callback to win the CAS
     resets the per-cycle countdown to the number of active clients.  Every
     callback then decrements the countdown; the one that reaches zero is the
     last for this cycle and is the one that checks timing and posts the
     semaphore.  This guarantees all clients have processed their ports before
     the streaming thread is woken. */
  jack_time_t old_usecs =
      atomic_load_explicit(&state->current_usecs, memory_order_relaxed);
  if (old_usecs != current_usecs &&
      atomic_compare_exchange_strong_explicit(
          &state->current_usecs, &old_usecs, current_usecs,
          memory_order_release, memory_order_relaxed)) {
    int client_count =
        atomic_load_explicit(&state->active_client_count, memory_order_relaxed);
    atomic_store_explicit(&state->pending_callbacks, client_count,
                          memory_order_relaxed);
  }

  if (atomic_fetch_sub_explicit(&state->pending_callbacks, 1,
                                memory_order_acq_rel) == 1) {
    double next_sec = (double)next_usecs * 1e-6;
    int stopped = atomic_load_explicit(&state->stopped, memory_order_relaxed);
    for (int i = 0; i < JACK_MAX_WAITERS; i++) {
      if (!atomic_load_explicit(&state->waiters[i].in_use,
                                memory_order_acquire))
        continue;
      double waiter_target = atomic_load_explicit(
          &state->waiters[i].sleep_target, memory_order_acquire);
      if (stopped || next_sec >= waiter_target) {
        atomic_store_explicit(&state->waiters[i].sleep_target, INFINITY,
                              memory_order_relaxed);
        jack_sem_post(&state->waiters[i].semaphore);
      }
    }
  }

  return 0;
}

CAMLprim value caml_jack_server_state_create(value _unit) {
  CAMLparam1(_unit);
  CAMLlocal1(_server_state_block);
  jack_server_state_t *state = calloc(1, sizeof(jack_server_state_t));
  if (!state)
    caml_failwith("jack_server_state_create: out of memory");
  for (int i = 0; i < JACK_MAX_WAITERS; i++) {
    jack_sem_init(&state->waiters[i].semaphore);
    atomic_store(&state->waiters[i].in_use, 0);
    atomic_store(&state->waiters[i].sleep_target, INFINITY);
  }
  atomic_store(&state->current_usecs, (jack_time_t)0);
  atomic_store(&state->stopped, 0);
  atomic_store(&state->active_client_count, 0);
  atomic_store(&state->pending_callbacks, 0);
  _server_state_block =
      caml_alloc_custom(&server_state_ops, sizeof(jack_server_state_t *), 0, 1);
  ServerState_val(_server_state_block) = state;
  CAMLreturn(_server_state_block);
}

CAMLprim value caml_jack_server_state_set_stopped(value _server_state_block,
                                                  value _stopped) {
  jack_server_state_t *state = ServerState_val(_server_state_block);
  int stopped_val = Bool_val(_stopped);
  atomic_store_explicit(&state->stopped, stopped_val, memory_order_release);
  if (stopped_val) {
    for (int i = 0; i < JACK_MAX_WAITERS; i++) {
      if (atomic_load_explicit(&state->waiters[i].in_use, memory_order_acquire))
        jack_sem_post(&state->waiters[i].semaphore);
    }
  }
  return Val_unit;
}

CAMLprim value caml_jack_server_state_get_stopped(value _server_state_block) {
  int stopped = atomic_load_explicit(
      &ServerState_val(_server_state_block)->stopped, memory_order_relaxed);
  return Val_bool(stopped);
}

CAMLprim value caml_jack_server_state_get_elapsed(value _server_state_block) {
  CAMLparam1(_server_state_block);
  jack_time_t usecs =
      atomic_load_explicit(&ServerState_val(_server_state_block)->current_usecs,
                           memory_order_acquire);
  CAMLreturn(caml_copy_double((double)usecs * 1e-6));
}

static inline int claim_free_spot(jack_server_state_t *state) {
  int slot = -1;
  for (int i = 0; i < JACK_MAX_WAITERS; i++) {
    int expected = 0;
    if (atomic_compare_exchange_strong_explicit(
            &state->waiters[i].in_use, &expected, 1, memory_order_acq_rel,
            memory_order_relaxed)) {
      slot = i;
      break;
    }
  }
  if (slot < 0)
    caml_failwith("jack_server_state_wait_until: too many concurrent waiters");

  return slot;
}

static inline void wait_on_slot(jack_server_state_t *state, int slot) {
  caml_release_runtime_system();
  jack_sem_wait(&state->waiters[slot].semaphore);
  caml_acquire_runtime_system();
}

CAMLprim value caml_jack_server_wait(value _server_state_block) {
  CAMLparam1(_server_state_block);
  jack_server_state_t *state = ServerState_val(_server_state_block);
  wait_on_slot(state, claim_free_spot(state));
  CAMLreturn(Val_unit);
}

CAMLprim value caml_jack_server_state_wait_until(value _server_state_block,
                                                 value _target) {
  CAMLparam2(_server_state_block, _target);
  jack_server_state_t *state = ServerState_val(_server_state_block);
  double target = Double_val(_target);
  int slot = claim_free_spot(state);

  /* Store the sleep target before sampling current_usecs so that any RT
     callback firing between the two sees the target and posts if the cycle
     qualifies. */
  atomic_store_explicit(&state->waiters[slot].sleep_target, target,
                        memory_order_release);

  jack_time_t usecs =
      atomic_load_explicit(&state->current_usecs, memory_order_acquire);
  if ((double)usecs * 1e-6 >= target ||
      atomic_load_explicit(&state->stopped, memory_order_relaxed)) {
    atomic_store_explicit(&state->waiters[slot].sleep_target, INFINITY,
                          memory_order_relaxed);
    atomic_store_explicit(&state->waiters[slot].in_use, 0,
                          memory_order_release);
    CAMLreturn(Val_unit);
  }

  /* Wait for the RT callback to post this slot's semaphore.  Loop to handle
     spurious wakeups (e.g., a leftover post from a previous waiter on this
     slot). */
  do {
    wait_on_slot(state, slot);
    usecs = atomic_load_explicit(&state->current_usecs, memory_order_acquire);
  } while ((double)usecs * 1e-6 < target &&
           !atomic_load_explicit(&state->stopped, memory_order_relaxed));

  atomic_store_explicit(&state->waiters[slot].sleep_target, INFINITY,
                        memory_order_relaxed);
  atomic_store_explicit(&state->waiters[slot].in_use, 0, memory_order_release);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_jack_source_create(value _unit) {
  CAMLparam1(_unit);
  CAMLlocal1(_source_block);
  jack_source_t *source = calloc(1, sizeof(jack_source_t));
  if (!source)
    caml_failwith("jack_source_create: out of memory");
  _source_block = caml_alloc_custom(&source_ops, sizeof(jack_source_t *), 0, 1);
  Source_val(_source_block) = source;
  CAMLreturn(_source_block);
}

CAMLprim value caml_jack_source_set_client(value _source_block,
                                           value _client_block) {
  Source_val(_source_block)->jack_client = Client_val(_client_block);
  return Val_unit;
}

CAMLprim value caml_jack_source_set_server_state(value _source_block,
                                                 value _server_state_block) {
  Source_val(_source_block)->server_state =
      ServerState_val(_server_state_block);
  return Val_unit;
}

static void jack_server_shutdown_callback(void *arg) {
  jack_source_t *source = (jack_source_t *)arg;
  jack_server_state_t *state = source->server_state;
  atomic_store_explicit(&state->stopped, 1, memory_order_release);
  for (int i = 0; i < JACK_MAX_WAITERS; i++) {
    if (atomic_load_explicit(&state->waiters[i].in_use, memory_order_acquire))
      jack_sem_post(&state->waiters[i].semaphore);
  }
}

CAMLprim value caml_jack_source_register_callback(value _client_block,
                                                  value _source_block) {
  CAMLparam2(_client_block, _source_block);
  jack_client_t *client = Client_val(_client_block);
  jack_source_t *source = Source_val(_source_block);
  int result =
      jack_set_process_callback(client, jack_source_process_callback, source);
  if (result != 0)
    caml_failwith("jack_source_register_callback: failed");
  jack_on_shutdown(client, jack_server_shutdown_callback, source);
  atomic_fetch_add_explicit(&source->server_state->active_client_count, 1,
                            memory_order_relaxed);
  /* Seed current_usecs with the real JACK time so that the first call to
     time() returns a sensible value rather than 0, avoiding a spurious
     "Too much latency" warning on the first clock tick. */
  jack_time_t now = jack_get_time();
  jack_time_t zero = 0;
  atomic_compare_exchange_strong_explicit(&source->server_state->current_usecs,
                                          &zero, now, memory_order_release,
                                          memory_order_relaxed);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_jack_source_unregister_callback(value _source_block) {
  jack_source_t *source = Source_val(_source_block);
  atomic_fetch_sub_explicit(&source->server_state->active_client_count, 1,
                            memory_order_relaxed);
  return Val_unit;
}

CAMLprim value caml_jack_source_add_port(value _source_block, value _port_block,
                                         value _is_input) {
  jack_source_t *source = Source_val(_source_block);
  jack_port_t *port = Port_val(_port_block);
  int is_input = Bool_val(_is_input);
  for (int i = 0; i < JACK_MAX_PORTS; i++) {
    if (atomic_load_explicit(&source->ports[i].port, memory_order_relaxed) ==
        NULL) {
      source->ports[i].is_input = is_input;
      atomic_store_explicit(&source->ports[i].dropped_frames, 0,
                            memory_order_relaxed);
      atomic_store_explicit(&source->ports[i].ringbuffer,
                            (jack_ringbuffer_t *)NULL, memory_order_relaxed);
      atomic_store_explicit(&source->ports[i].port, port, memory_order_release);
      return Val_int(i);
    }
  }
  caml_failwith("jack_source_add_port: no free port slot");
}

CAMLprim value caml_jack_source_enable_port(value _source_block, value _index,
                                            value _rb_block) {
  jack_source_t *source = Source_val(_source_block);
  int index = Int_val(_index);
  atomic_store_explicit(&source->ports[index].ringbuffer, Rb_val(_rb_block),
                        memory_order_release);
  return Val_unit;
}

CAMLprim value caml_jack_source_disable_port(value _source_block,
                                             value _index) {
  jack_source_t *source = Source_val(_source_block);
  int index = Int_val(_index);
  atomic_store_explicit(&source->ports[index].ringbuffer,
                        (jack_ringbuffer_t *)NULL, memory_order_relaxed);
  return Val_unit;
}

CAMLprim value caml_jack_source_remove_port(value _source_block, value _index) {
  jack_source_t *source = Source_val(_source_block);
  int index = Int_val(_index);
  atomic_store_explicit(&source->ports[index].port, (jack_port_t *)NULL,
                        memory_order_release);
  atomic_store_explicit(&source->ports[index].ringbuffer,
                        (jack_ringbuffer_t *)NULL, memory_order_relaxed);
  return Val_unit;
}

CAMLprim value caml_jack_source_get_and_reset_dropped(value _source_block,
                                                      value _index) {
  jack_source_t *source = Source_val(_source_block);
  int index = Int_val(_index);
  size_t dropped = atomic_exchange_explicit(
      &source->ports[index].dropped_frames, 0, memory_order_relaxed);
  return Val_int((int)dropped);
}
