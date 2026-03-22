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

#define JACK_MAX_PORTS 64

typedef struct {
  _Atomic(jack_port_t *)       port;
  _Atomic(jack_ringbuffer_t *) ringbuffer;
  _Atomic size_t               dropped_frames;
  int                          is_input;
} jack_port_state_t;

typedef struct {
  _Atomic double     elapsed_time;
  _Atomic double     sleep_target;
  _Atomic int        stopped;
  double             sample_rate;
  jack_port_state_t  ports[JACK_MAX_PORTS];
  jack_sem_t         semaphore;
} jack_state_t;

#define State_val(v) (*((jack_state_t **)Data_custom_val(v)))

static void state_finalize(value _state_block)
{
  jack_state_t *state = State_val(_state_block);
  jack_sem_destroy(&state->semaphore);
  free(state);
}

static struct custom_operations state_ops = {
  "liquidsoap_jack_state",
  state_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static int jack_process_callback(jack_nframes_t nframes, void *arg)
{
  jack_state_t *state = (jack_state_t *)arg;

  double cur = atomic_load_explicit(&state->elapsed_time, memory_order_relaxed);
  atomic_store_explicit(&state->elapsed_time,
                        cur + (double)nframes / state->sample_rate,
                        memory_order_release);

  for (int i = 0; i < JACK_MAX_PORTS; i++) {
    jack_port_t *port = atomic_load_explicit(&state->ports[i].port, memory_order_acquire);
    if (!port) continue;
    float *jack_buf = jack_port_get_buffer(port, nframes);
    jack_ringbuffer_t *rb = atomic_load_explicit(&state->ports[i].ringbuffer, memory_order_relaxed);
    if (state->ports[i].is_input) {
      if (!rb) continue;
      size_t written = jack_ringbuffer_write(rb, (const char *)jack_buf, nframes * sizeof(float));
      size_t dropped = nframes - written / sizeof(float);
      if (dropped)
        atomic_fetch_add_explicit(&state->ports[i].dropped_frames, dropped, memory_order_relaxed);
    } else {
      size_t got_frames = 0;
      if (rb) {
        size_t got = jack_ringbuffer_read(rb, (char *)jack_buf, nframes * sizeof(float));
        got_frames = got / sizeof(float);
      }
      size_t dropped = (size_t)nframes - got_frames;
      if (dropped) {
        atomic_fetch_add_explicit(&state->ports[i].dropped_frames, dropped, memory_order_relaxed);
        for (size_t j = got_frames; j < (size_t)nframes; j++) jack_buf[j] = 0.f;
      }
    }
  }

  double elapsed = atomic_load_explicit(&state->elapsed_time, memory_order_relaxed);
  double target  = atomic_load_explicit(&state->sleep_target, memory_order_acquire);
  if (atomic_load_explicit(&state->stopped, memory_order_relaxed) || elapsed >= target) {
    atomic_store_explicit(&state->sleep_target, INFINITY, memory_order_relaxed);
    jack_sem_post(&state->semaphore);
  }

  return 0;
}

CAMLprim value caml_jack_state_create(value _unit)
{
  CAMLparam1(_unit);
  CAMLlocal1(_state_block);
  jack_state_t *state = calloc(1, sizeof(jack_state_t));
  if (!state) caml_failwith("jack_state_create: out of memory");
  atomic_store(&state->elapsed_time, 0.0);
  atomic_store(&state->sleep_target, INFINITY);
  atomic_store(&state->stopped, 0);
  state->sample_rate = 0.0;
  /* calloc zeroes all port/ringbuffer/dropped_frames fields */
  jack_sem_init(&state->semaphore);
  _state_block = caml_alloc_custom(&state_ops, sizeof(jack_state_t *), 0, 1);
  State_val(_state_block) = state;
  CAMLreturn(_state_block);
}

CAMLprim value caml_jack_state_set_sample_rate(value _state_block, value _rate)
{
  State_val(_state_block)->sample_rate = Double_val(_rate);
  return Val_unit;
}

CAMLprim value caml_jack_state_set_stopped(value _state_block, value _stopped)
{
  atomic_store_explicit(&State_val(_state_block)->stopped,
                        Bool_val(_stopped), memory_order_relaxed);
  return Val_unit;
}

CAMLprim value caml_jack_state_get_stopped(value _state_block)
{
  int stopped = atomic_load_explicit(&State_val(_state_block)->stopped,
                                     memory_order_relaxed);
  return Val_bool(stopped);
}

CAMLprim value caml_jack_state_get_elapsed(value _state_block)
{
  CAMLparam1(_state_block);
  double elapsed = atomic_load_explicit(&State_val(_state_block)->elapsed_time,
                                        memory_order_acquire);
  CAMLreturn(caml_copy_double(elapsed));
}

CAMLprim value caml_jack_state_set_sleep_target(value _state_block, value _target)
{
  atomic_store_explicit(&State_val(_state_block)->sleep_target,
                        Double_val(_target), memory_order_release);
  return Val_unit;
}

CAMLprim value caml_jack_state_wait(value _state_block)
{
  CAMLparam1(_state_block);
  jack_state_t *state = State_val(_state_block);
  caml_release_runtime_system();
  jack_sem_wait(&state->semaphore);
  caml_acquire_runtime_system();
  CAMLreturn(Val_unit);
}

/* Find a free slot, register the port (without ringbuffer), return the index. */
CAMLprim value caml_jack_state_add_port(value _state_block, value _port_block, value _is_input)
{
  jack_state_t *state = State_val(_state_block);
  jack_port_t *port = Port_val(_port_block);
  int is_input = Bool_val(_is_input);
  for (int i = 0; i < JACK_MAX_PORTS; i++) {
    if (atomic_load_explicit(&state->ports[i].port, memory_order_relaxed) == NULL) {
      state->ports[i].is_input = is_input;
      atomic_store_explicit(&state->ports[i].dropped_frames, 0, memory_order_relaxed);
      atomic_store_explicit(&state->ports[i].ringbuffer, (jack_ringbuffer_t *)NULL, memory_order_relaxed);
      atomic_store_explicit(&state->ports[i].port, port, memory_order_release);
      return Val_int(i);
    }
  }
  caml_failwith("jack_state_add_port: no free port slot");
}

/* Set the ringbuffer for a port slot, enabling data flow. */
CAMLprim value caml_jack_state_enable_port(value _state_block, value _index, value _rb_block)
{
  jack_state_t *state = State_val(_state_block);
  int index = Int_val(_index);
  atomic_store_explicit(&state->ports[index].ringbuffer, Rb_val(_rb_block), memory_order_release);
  return Val_unit;
}

/* Clear the ringbuffer only, suspending data flow without removing the port. */
CAMLprim value caml_jack_state_disable_port(value _state_block, value _index)
{
  jack_state_t *state = State_val(_state_block);
  int index = Int_val(_index);
  atomic_store_explicit(&state->ports[index].ringbuffer, (jack_ringbuffer_t *)NULL, memory_order_relaxed);
  return Val_unit;
}

/* Clear the port slot entirely. */
CAMLprim value caml_jack_state_remove_port(value _state_block, value _index)
{
  jack_state_t *state = State_val(_state_block);
  int index = Int_val(_index);
  atomic_store_explicit(&state->ports[index].port, (jack_port_t *)NULL, memory_order_release);
  atomic_store_explicit(&state->ports[index].ringbuffer, (jack_ringbuffer_t *)NULL, memory_order_relaxed);
  return Val_unit;
}

CAMLprim value caml_jack_state_get_and_reset_dropped(value _state_block, value _index)
{
  jack_state_t *state = State_val(_state_block);
  int index = Int_val(_index);
  size_t dropped = atomic_exchange_explicit(&state->ports[index].dropped_frames, 0,
                                            memory_order_relaxed);
  return Val_int((int)dropped);
}

CAMLprim value caml_jack_state_register_callback(value _client_block, value _state_block)
{
  CAMLparam2(_client_block, _state_block);
  jack_client_t *client = Client_val(_client_block)->client;
  jack_state_t *state = State_val(_state_block);
  int result = jack_set_process_callback(client, jack_process_callback, state);
  if (result != 0) caml_failwith("jack_state_register_callback: failed");
  CAMLreturn(Val_unit);
}
