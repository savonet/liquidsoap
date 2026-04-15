#pragma once

#include <caml/mlvalues.h>
#include <jack/jack.h>
#include <jack/ringbuffer.h>

/* Client wrapper */
typedef struct {
  jack_client_t *client;
  value _process_callback;
} client_wrapper_t;

#define Client_val(v) (*((client_wrapper_t **)Data_custom_val(v)))

/* Custom block accessors */
#define Port_val(v)  (*((jack_port_t **)Data_custom_val(v)))
#define Rb_val(v)    (*((jack_ringbuffer_t **)Data_custom_val(v)))

/* Platform-specific semaphore */
#ifdef _WIN32
#include <windows.h>
typedef HANDLE jack_sem_t;
static inline void jack_sem_init(jack_sem_t *s)    { *s = CreateSemaphore(NULL, 0, LONG_MAX, NULL); }
static inline void jack_sem_post(jack_sem_t *s)    { ReleaseSemaphore(*s, 1, NULL); }
static inline void jack_sem_wait(jack_sem_t *s)    { WaitForSingleObject(*s, INFINITE); }
static inline void jack_sem_destroy(jack_sem_t *s) { CloseHandle(*s); }
#elif defined(__APPLE__)
#include <dispatch/dispatch.h>
typedef dispatch_semaphore_t jack_sem_t;
static inline void jack_sem_init(jack_sem_t *s)    { *s = dispatch_semaphore_create(0); }
static inline void jack_sem_post(jack_sem_t *s)    { dispatch_semaphore_signal(*s); }
static inline void jack_sem_wait(jack_sem_t *s)    { dispatch_semaphore_wait(*s, DISPATCH_TIME_FOREVER); }
static inline void jack_sem_destroy(jack_sem_t *s) { dispatch_release(*s); }
#else
#include <semaphore.h>
typedef sem_t jack_sem_t;
static inline void jack_sem_init(jack_sem_t *s)    { sem_init(s, 0, 0); }
static inline void jack_sem_post(jack_sem_t *s)    { sem_post(s); }
static inline void jack_sem_wait(jack_sem_t *s)    { sem_wait(s); }
static inline void jack_sem_destroy(jack_sem_t *s) { sem_destroy(s); }
#endif
