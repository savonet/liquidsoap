#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#include "jack_stubs.h"
#include <pthread.h>
#include <stdlib.h>

/* --- OCaml thread registration for JACK callbacks --- */

static pthread_key_t ocaml_jack_thread_key;
static pthread_once_t ocaml_jack_thread_key_once = PTHREAD_ONCE_INIT;

static void ocaml_jack_on_thread_exit(void *key)
{
  caml_c_thread_unregister();
}

static void ocaml_jack_make_key()
{
  pthread_key_create(&ocaml_jack_thread_key, ocaml_jack_on_thread_exit);
}

static void ocaml_jack_register_thread()
{
  static int registered_sentinel = 1;
  pthread_once(&ocaml_jack_thread_key_once, ocaml_jack_make_key);
  if (caml_c_thread_register() && !pthread_getspecific(ocaml_jack_thread_key))
    pthread_setspecific(ocaml_jack_thread_key, (void *)&registered_sentinel);
}

/* --- jack_client_t custom block --- */

static void client_finalize(value _client_block)
{
  client_wrapper_t *wrapper = Client_val(_client_block);
  if (wrapper->client)
    jack_client_close(wrapper->client);
  caml_remove_generational_global_root(&wrapper->_process_callback);
  free(wrapper);
}

static struct custom_operations client_ops = {
  "liquidsoap_jack_client",
  client_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

/* --- jack_port_t custom block (no finalization, owned by client) --- */

static struct custom_operations port_ops = {
  "liquidsoap_jack_port",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

/* --- process callback glue --- */

static int process_callback(jack_nframes_t frame_count, void *arg)
{
  client_wrapper_t *wrapper = (client_wrapper_t *)arg;
  if (wrapper->_process_callback == Val_unit) return 0;
  ocaml_jack_register_thread();
  caml_acquire_runtime_system();
  caml_callback_exn(wrapper->_process_callback, Val_int(frame_count));
  caml_release_runtime_system();
  return 0;
}

CAMLprim value caml_jack_client_open(value _name, value _flags, value _server)
{
  CAMLparam3(_name, _flags, _server);
  CAMLlocal1(_client_block);
  jack_status_t status;
  jack_options_t options = (jack_options_t)Int_val(_flags);
  client_wrapper_t *wrapper = malloc(sizeof(client_wrapper_t));
  if (!wrapper)
    caml_failwith("jack_client_open: out of memory");
  if (_server == Val_int(0))
    wrapper->client = jack_client_open(String_val(_name), options, &status);
  else
    wrapper->client = jack_client_open(String_val(_name), options, &status,
                                       String_val(Field(_server, 0)));
  if (!wrapper->client) {
    free(wrapper);
    caml_failwith("jack_client_open: could not connect to JACK server");
  }
  wrapper->_process_callback = Val_unit;
  caml_register_generational_global_root(&wrapper->_process_callback);
  jack_set_process_callback(wrapper->client, process_callback, wrapper);
  _client_block = caml_alloc_custom(&client_ops, sizeof(client_wrapper_t *), 0, 1);
  Client_val(_client_block) = wrapper;
  CAMLreturn(_client_block);
}

CAMLprim value caml_jack_client_close(value _client)
{
  CAMLparam1(_client);
  client_wrapper_t *wrapper = Client_val(_client);
  caml_modify_generational_global_root(&wrapper->_process_callback, Val_unit);
  jack_client_close(wrapper->client);
  wrapper->client = NULL;
  CAMLreturn(Val_unit);
}

CAMLprim value caml_jack_get_sample_rate(value _client)
{
  return Val_int(jack_get_sample_rate(Client_val(_client)->client));
}

CAMLprim value caml_jack_get_buffer_size(value _client)
{
  return Val_int(jack_get_buffer_size(Client_val(_client)->client));
}

CAMLprim value caml_jack_port_register(value _client, value _name, value _port_type,
                                       value _flags, value _buffer_size)
{
  CAMLparam5(_client, _name, _port_type, _flags, _buffer_size);
  CAMLlocal1(_port_block);
  jack_port_t *port = jack_port_register(Client_val(_client)->client,
                                         String_val(_name),
                                         String_val(_port_type),
                                         (unsigned long)Int_val(_flags),
                                         (unsigned long)Int_val(_buffer_size));
  if (!port)
    caml_failwith("jack_port_register: failed to register port");
  _port_block = caml_alloc_custom(&port_ops, sizeof(jack_port_t *), 0, 1);
  Port_val(_port_block) = port;
  CAMLreturn(_port_block);
}

CAMLprim value caml_jack_set_process_callback(value _client, value _callback)
{
  CAMLparam2(_client, _callback);
  client_wrapper_t *wrapper = Client_val(_client);
  caml_modify_generational_global_root(&wrapper->_process_callback, _callback);
  CAMLreturn(Val_unit);
}



CAMLprim value caml_jack_activate(value _client)
{
  CAMLparam1(_client);
  int result = jack_activate(Client_val(_client)->client);
  if (result != 0)
    caml_failwith("jack_activate: failed");
  CAMLreturn(Val_unit);
}

CAMLprim value caml_jack_port_get_buffer(value _port, value _frame_count)
{
  CAMLparam2(_port, _frame_count);
  CAMLlocal1(_audio_bigarray);
  jack_nframes_t frame_count = (jack_nframes_t)Int_val(_frame_count);
  float *audio_buffer = (float *)jack_port_get_buffer(Port_val(_port), frame_count);
  intnat dimensions[1];
  dimensions[0] = (intnat)frame_count;
  _audio_bigarray = caml_ba_alloc(CAML_BA_FLOAT32 | CAML_BA_C_LAYOUT | CAML_BA_EXTERNAL,
                                  1, audio_buffer, dimensions);
  CAMLreturn(_audio_bigarray);
}

CAMLprim value caml_jack_port_unregister(value _client, value _port)
{
  jack_port_unregister(Client_val(_client)->client, Port_val(_port));
  return Val_unit;
}

/* --- jack_ringbuffer_t custom block --- */

static void rb_finalize(value _ringbuffer_block)
{
  jack_ringbuffer_t *ringbuffer = Rb_val(_ringbuffer_block);
  if (ringbuffer) jack_ringbuffer_free(ringbuffer);
}

static struct custom_operations rb_ops = {
  "liquidsoap_jack_ringbuffer",
  rb_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value caml_jack_ringbuffer_create(value _size)
{
  CAMLparam1(_size);
  CAMLlocal1(_ringbuffer_block);
  jack_ringbuffer_t *ringbuffer = jack_ringbuffer_create((size_t)Int_val(_size));
  if (!ringbuffer) caml_failwith("jack_ringbuffer_create: failed");
  _ringbuffer_block = caml_alloc_custom(&rb_ops, sizeof(jack_ringbuffer_t *), 0, 1);
  Rb_val(_ringbuffer_block) = ringbuffer;
  CAMLreturn(_ringbuffer_block);
}

CAMLprim value caml_jack_ringbuffer_mlock(value _ringbuffer)
{
  CAMLparam1(_ringbuffer);
  int result = jack_ringbuffer_mlock(Rb_val(_ringbuffer));
  if (result != 0) caml_failwith("jack_ringbuffer_mlock: failed");
  CAMLreturn(Val_unit);
}

CAMLprim value caml_jack_ringbuffer_read(value _ringbuffer, value _buffer, value _offset,
                                         value _count)
{
  size_t bytes_read = jack_ringbuffer_read(Rb_val(_ringbuffer),
                                           (char *)Bytes_val(_buffer) + Int_val(_offset),
                                           (size_t)Int_val(_count));
  return Val_int(bytes_read);
}

CAMLprim value caml_jack_ringbuffer_read_space(value _ringbuffer)
{
  return Val_int(jack_ringbuffer_read_space(Rb_val(_ringbuffer)));
}

CAMLprim value caml_jack_ringbuffer_write(value _ringbuffer, value _buffer, value _offset,
                                          value _count)
{
  size_t bytes_written = jack_ringbuffer_write(Rb_val(_ringbuffer),
                                               (const char *)Bytes_val(_buffer) + Int_val(_offset),
                                               (size_t)Int_val(_count));
  return Val_int(bytes_written);
}

CAMLprim value caml_jack_ringbuffer_write_space(value _ringbuffer)
{
  return Val_int(jack_ringbuffer_write_space(Rb_val(_ringbuffer)));
}

/* Read from ringbuffer into a float32 Bigarray. Offsets and count in samples. */
CAMLprim value caml_jack_ringbuffer_read_ba(value _ringbuffer, value _bigarray, value _offset,
                                            value _count)
{
  float *destination = (float *)Caml_ba_data_val(_bigarray) + Int_val(_offset);
  size_t byte_count = (size_t)Int_val(_count) * sizeof(float);
  size_t bytes_read = jack_ringbuffer_read(Rb_val(_ringbuffer), (char *)destination, byte_count);
  return Val_int((int)(bytes_read / sizeof(float)));
}

/* Write from a float32 Bigarray into ringbuffer. Offsets and count in samples. */
CAMLprim value caml_jack_ringbuffer_write_ba(value _ringbuffer, value _bigarray, value _offset,
                                             value _count)
{
  const float *source = (const float *)Caml_ba_data_val(_bigarray) + Int_val(_offset);
  size_t byte_count = (size_t)Int_val(_count) * sizeof(float);
  size_t bytes_written = jack_ringbuffer_write(Rb_val(_ringbuffer), (const char *)source, byte_count);
  return Val_int((int)(bytes_written / sizeof(float)));
}

/* Read all available data from the ringbuffer into a freshly allocated float64
   OCaml array, converting from float32. Returns the allocated array. */
CAMLprim value caml_jack_ringbuffer_read_alloc(value _ringbuffer)
{
  CAMLparam1(_ringbuffer);
  CAMLlocal1(_array);
  jack_ringbuffer_t *ringbuffer = Rb_val(_ringbuffer);

  jack_ringbuffer_data_t segments[2];
  jack_ringbuffer_get_read_vector(ringbuffer, segments);

  int first_available  = (int)(segments[0].len / sizeof(float));
  int second_available = (int)(segments[1].len / sizeof(float));
  int total = first_available + second_available;

  _array = caml_alloc_float_array(total);

  float *first_src  = (float *)segments[0].buf;
  float *second_src = (float *)segments[1].buf;

  for (int i = 0; i < first_available; i++)
    Store_double_field(_array, i, (double)first_src[i]);
  for (int i = 0; i < second_available; i++)
    Store_double_field(_array, first_available + i, (double)second_src[i]);

  jack_ringbuffer_read_advance(ringbuffer, (size_t)total * sizeof(float));
  CAMLreturn(_array);
}

/* Read from the ringbuffer into a float64 OCaml array, converting from float32.
   Offset and count are in samples. Returns the number of samples read. */
CAMLprim value caml_jack_ringbuffer_read_array(value _ringbuffer, value _array,
                                               value _offset, value _count)
{
  jack_ringbuffer_t *ringbuffer = Rb_val(_ringbuffer);
  int offset = Int_val(_offset);
  int count = Int_val(_count);

  jack_ringbuffer_data_t segments[2];
  jack_ringbuffer_get_read_vector(ringbuffer, segments);

  int first_segment_available = (int)(segments[0].len / sizeof(float));
  int second_segment_available = (int)(segments[1].len / sizeof(float));
  int to_read = count < first_segment_available + second_segment_available
                  ? count : first_segment_available + second_segment_available;

  float *first_segment_source = (float *)segments[0].buf;
  float *second_segment_source = (float *)segments[1].buf;

  int read_from_first = to_read < first_segment_available ? to_read : first_segment_available;
  for (int i = 0; i < read_from_first; i++)
    Store_double_field(_array, offset + i, (double)first_segment_source[i]);
  for (int i = read_from_first; i < to_read; i++)
    Store_double_field(_array, offset + i, (double)second_segment_source[i - first_segment_available]);

  jack_ringbuffer_read_advance(ringbuffer, (size_t)to_read * sizeof(float));
  return Val_int(to_read);
}

/* Write from a float64 OCaml array into the ringbuffer, converting to float32.
   Offset and count are in samples. Returns the number of samples written. */
CAMLprim value caml_jack_ringbuffer_write_array(value _ringbuffer, value _array,
                                                value _offset, value _count)
{
  jack_ringbuffer_t *ringbuffer = Rb_val(_ringbuffer);
  int offset = Int_val(_offset);
  int count = Int_val(_count);

  jack_ringbuffer_data_t segments[2];
  jack_ringbuffer_get_write_vector(ringbuffer, segments);

  int first_segment_space = (int)(segments[0].len / sizeof(float));
  int second_segment_space = (int)(segments[1].len / sizeof(float));
  int to_write = count < first_segment_space + second_segment_space
                   ? count : first_segment_space + second_segment_space;

  float *first_segment_dest = (float *)segments[0].buf;
  float *second_segment_dest = (float *)segments[1].buf;

  int write_to_first = to_write < first_segment_space ? to_write : first_segment_space;
  for (int i = 0; i < write_to_first; i++)
    first_segment_dest[i] = (float)Double_field(_array, offset + i);
  for (int i = write_to_first; i < to_write; i++)
    second_segment_dest[i - first_segment_space] = (float)Double_field(_array, offset + i);

  jack_ringbuffer_write_advance(ringbuffer, (size_t)to_write * sizeof(float));
  return Val_int(to_write);
}

CAMLprim value caml_jack_ringbuffer_read_advance(value _ringbuffer, value _count)
{
  jack_ringbuffer_read_advance(Rb_val(_ringbuffer), (size_t)Int_val(_count));
  return Val_unit;
}

CAMLprim value caml_jack_port_connect(value _client, value _source_port, value _destination_port)
{
  jack_connect(Client_val(_client)->client, String_val(_source_port), String_val(_destination_port));
  return Val_unit;
}

/* --- Semaphore custom block --- */

#define Sem_val(v) (*((jack_sem_t **)Data_custom_val(v)))

static void sem_block_finalize(value _sem_block)
{
  jack_sem_t *semaphore = Sem_val(_sem_block);
  jack_sem_destroy(semaphore);
  free(semaphore);
}

static struct custom_operations sem_ops = {
  "liquidsoap_jack_sem",
  sem_block_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value caml_jack_sem_create(value _unit)
{
  CAMLparam1(_unit);
  CAMLlocal1(_sem_block);
  jack_sem_t *semaphore = malloc(sizeof(jack_sem_t));
  if (!semaphore) caml_failwith("jack_sem_create: out of memory");
  jack_sem_init(semaphore);
  _sem_block = caml_alloc_custom(&sem_ops, sizeof(jack_sem_t *), 0, 1);
  Sem_val(_sem_block) = semaphore;
  CAMLreturn(_sem_block);
}

CAMLprim value caml_jack_sem_post(value _sem_block)
{
  jack_sem_post(Sem_val(_sem_block));
  return Val_unit;
}

CAMLprim value caml_jack_sem_wait(value _sem_block)
{
  CAMLparam1(_sem_block);
  caml_release_runtime_system();
  jack_sem_wait(Sem_val(_sem_block));
  caml_acquire_runtime_system();
  CAMLreturn(Val_unit);
}
