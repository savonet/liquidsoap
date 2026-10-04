/* Exercises duppy_core against SPEC.md as a plain C program: building it
   without the OCaml headers is what shows the core does not need them. */

#include "duppy_core.h"

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <time.h>
#include <unistd.h>

#define EXPECTED_CHECKS 110
#define CAPACITY 256

static int checks = 0;
static int failures = 0;
static const char *backend = "";

static void check(int condition, const char *what) {
  checks++;
  if (!condition) {
    failures++;
    fprintf(stderr, "FAILED (%s): %s\n", backend, what);
  }
}

typedef struct {
  duppy_core *core;
  int wake[2];
} fixture;

static fixture open_core(int force_fallback) {
  fixture opened;
  if (socketpair(AF_UNIX, SOCK_STREAM, 0, opened.wake) != 0)
    abort();
  fcntl(opened.wake[1], F_SETFL, O_NONBLOCK);
  opened.core =
      duppy_core_create(opened.wake[0], opened.wake[1], force_fallback);
  if (opened.core == NULL)
    abort();
  backend = duppy_core_backend(opened.core);
  return opened;
}

static void close_core(fixture *opened) {
  duppy_core_stop(opened->core);
  duppy_core_free(opened->core);
  close(opened->wake[0]);
  close(opened->wake[1]);
}

static int submit(duppy_core *core, intptr_t handle, duppy_class task_class,
                  int rank, double delay, int fd, int interest) {
  duppy_task task = {handle,
                     rank,
                     task_class,
                     DUPPY_ANY_WORKER,
                     DUPPY_EVERY_WORKER,
                     delay,
                     fd >= 0 ? 1 : 0,
                     &fd,
                     &interest};
  return duppy_core_submit(core, &task);
}

static duppy_work take(duppy_core *core, int worker, intptr_t *out,
                       size_t *written) {
  return duppy_core_take(core, worker, out, CAPACITY, written);
}

static duppy_work next(duppy_core *core, int worker, intptr_t *out,
                       size_t *written) {
  for (;;) {
    duppy_work work = take(core, worker, out, written);
    if (work != DUPPY_NONE)
      return work;
    duppy_core_wait(core, worker);
  }
}

static void test_deadlines(int force_fallback) {
  fixture opened = open_core(force_fallback);
  duppy_core *core = opened.core;
  intptr_t out[CAPACITY];
  size_t written;
  double delays[] = {0.03, 0.01, 0.02};
  double start = duppy_core_now();
  for (int i = 0; i < 3; i++)
    submit(core, i, DUPPY_DIRECT, 0, delays[i], -1, 0);
  duppy_core_start(core, 1, 4);
  intptr_t order[] = {1, 2, 0};
  for (int i = 0; i < 3; i++) {
    duppy_work work = next(core, 0, out, &written);
    check(work == DUPPY_ONE_DIRECT && out[0] == order[i],
          "expired tasks become ready in deadline order");
    check(out[1] == 1, "an expired delay is reported");
    check(duppy_core_now() - start >= delays[order[i]],
          "a delay is not reported before its deadline");
  }
  close_core(&opened);
}

static void test_elapsed_delay(int force_fallback) {
  fixture opened = open_core(force_fallback);
  duppy_core *core = opened.core;
  intptr_t out[CAPACITY];
  size_t written;
  duppy_core_start(core, 1, 4);
  submit(core, 7, DUPPY_DIRECT, 0, 0., -1, 0);
  check(take(core, 0, out, &written) == DUPPY_ONE_DIRECT && out[0] == 7 &&
            out[1] == 1,
        "a zero delay is ready at submission");
  close_core(&opened);
}

static void test_descriptors(int force_fallback) {
  fixture opened = open_core(force_fallback);
  duppy_core *core = opened.core;
  intptr_t out[CAPACITY];
  size_t written;
  int ends[2];
  if (pipe(ends) != 0)
    abort();
  duppy_core_start(core, 1, 4);

  submit(core, 1, DUPPY_IMMEDIATE, 0, -1., ends[0], DUPPY_READ);
  check(take(core, 0, out, &written) == DUPPY_NONE,
        "a quiet descriptor leaves its task waiting");
  check(write(ends[1], "x", 1) == 1, "pipe write");
  duppy_core_wait(core, 0);
  check(take(core, 0, out, &written) == DUPPY_BATCH && written == 3 &&
            out[0] == 1 && out[1] == 0 && out[2] == DUPPY_READ,
        "a readable descriptor is reported, and its delay is not");

  submit(core, 2, DUPPY_IMMEDIATE, 0, -1., ends[0], DUPPY_READ);
  check(next(core, 0, out, &written) == DUPPY_BATCH && out[2] == DUPPY_READ,
        "readiness is level-triggered");

  char sink;
  check(read(ends[0], &sink, 1) == 1, "pipe read");
  submit(core, 3, DUPPY_IMMEDIATE, 0, 0.02, ends[0], DUPPY_READ);
  check(next(core, 0, out, &written) == DUPPY_BATCH && out[0] == 3 &&
            out[1] == 1 && out[2] == 0,
        "a quiet descriptor leaves its task to its deadline");

  submit(core, 4, DUPPY_IMMEDIATE, 0, -1., ends[0], DUPPY_READ);
  close(ends[1]);
  check(next(core, 0, out, &written) == DUPPY_BATCH && out[0] == 4 &&
            out[2] == DUPPY_READ,
        "a hung up descriptor satisfies a read");

  close(ends[0]);
  close_core(&opened);
}

static void test_unwatchable(void) {
#ifdef __linux__
  fixture opened = open_core(0);
  duppy_core *core = opened.core;
  intptr_t out[CAPACITY];
  size_t written;
  int ends[2];
  if (pipe(ends) != 0)
    abort();
  int file = open("/proc/self/exe", O_RDONLY);
  int fds[] = {ends[0], file};
  int interests[] = {DUPPY_READ, DUPPY_READ};
  duppy_task task = {9,   0, DUPPY_DIRECT, DUPPY_ANY_WORKER, DUPPY_EVERY_WORKER,
                     -1., 2, fds,          interests};
  duppy_core_start(core, 1, 4);
  check(duppy_core_submit(core, &task) == 0, "an unwatchable task is accepted");
  check(take(core, 0, out, &written) == DUPPY_ONE_DIRECT && written == 4 &&
            out[2] == 0 && out[3] == DUPPY_READ,
        "only the unwatchable descriptor is reported, at once");
  close(file);
  close(ends[0]);
  close(ends[1]);
  close_core(&opened);
#endif
}

static void test_order(int force_fallback) {
  fixture opened = open_core(force_fallback);
  duppy_core *core = opened.core;
  intptr_t out[CAPACITY];
  size_t written;
  submit(core, 0, DUPPY_DIRECT, 2, 0., -1, 0);
  submit(core, 1, DUPPY_DIRECT, 1, 0., -1, 0);
  submit(core, 2, DUPPY_DIRECT, 1, 0., -1, 0);
  submit(core, 3, DUPPY_THREADED, 0, 0., -1, 0);
  submit(core, 4, DUPPY_THREADED, 1, 0., -1, 0);
  duppy_core_start(core, 1, 4);
  intptr_t order[] = {3, 1, 2, 4, 0};
  duppy_work kinds[] = {DUPPY_ONE_THREADED, DUPPY_ONE_DIRECT, DUPPY_ONE_DIRECT,
                        DUPPY_ONE_THREADED, DUPPY_ONE_DIRECT};
  for (int i = 0; i < 5; i++)
    check(take(core, 0, out, &written) == kinds[i] && out[0] == order[i],
          "lowest rank first, direct before threaded, then first ready");
  check(take(core, 0, out, &written) == DUPPY_NONE, "nothing is left");
  close_core(&opened);
}

static void test_alternation(int force_fallback) {
  fixture opened = open_core(force_fallback);
  duppy_core *core = opened.core;
  intptr_t out[CAPACITY];
  size_t written;
  submit(core, 0, DUPPY_IMMEDIATE, 0, 0., -1, 0);
  submit(core, 1, DUPPY_IMMEDIATE, 5, 0., -1, 0);
  submit(core, 2, DUPPY_DIRECT, 0, 0., -1, 0);
  duppy_core_start(core, 1, 4);
  check(take(core, 0, out, &written) == DUPPY_BATCH && written == 4 &&
            out[0] == 0 && out[2] == 1,
        "immediate tasks are taken as one batch, in order");
  submit(core, 3, DUPPY_IMMEDIATE, 0, 0., -1, 0);
  check(take(core, 0, out, &written) == DUPPY_ONE_DIRECT && out[0] == 2,
        "a single task is taken between two batches");
  check(take(core, 0, out, &written) == DUPPY_BATCH && out[0] == 3,
        "the next batch follows");
  close_core(&opened);
}

static void test_batch_bound(int force_fallback) {
  fixture opened = open_core(force_fallback);
  duppy_core *core = opened.core;
  intptr_t out[CAPACITY];
  size_t written;
  for (int i = 0; i < CAPACITY; i++)
    submit(core, i, DUPPY_IMMEDIATE, 0, 0., -1, 0);
  duppy_core_start(core, 1, 4);
  check(take(core, 0, out, &written) == DUPPY_BATCH && written == CAPACITY &&
            out[CAPACITY - 2] == CAPACITY / 2 - 1,
        "a batch stops at the capacity it was given");
  check(take(core, 0, out, &written) == DUPPY_BATCH && written == CAPACITY &&
            out[0] == CAPACITY / 2,
        "what a bounded batch left is taken next, in order");
  close_core(&opened);
}

static void test_slots(int force_fallback) {
  fixture opened = open_core(force_fallback);
  duppy_core *core = opened.core;
  intptr_t out[CAPACITY];
  size_t written;
  submit(core, 0, DUPPY_THREADED, 0, 0., -1, 0);
  submit(core, 1, DUPPY_THREADED, 0, 0., -1, 0);
  submit(core, 2, DUPPY_DIRECT, 3, 0., -1, 0);
  duppy_core_start(core, 1, 1);
  check(take(core, 0, out, &written) == DUPPY_ONE_THREADED && out[0] == 0,
        "a threaded task takes a slot");
  check(take(core, 0, out, &written) == DUPPY_ONE_DIRECT && out[0] == 2,
        "a worker out of slots still takes direct tasks");
  check(take(core, 0, out, &written) == DUPPY_NONE,
        "a worker out of slots leaves threaded tasks ready");
  duppy_core_blocking_done(core, 0);
  duppy_core_wait(core, 0);
  check(take(core, 0, out, &written) == DUPPY_ONE_THREADED && out[0] == 1,
        "a freed slot wakes the worker for the task left ready");
  check(take(core, 0, out, &written) == DUPPY_NONE, "out of slots again");
  check(duppy_core_reserve(core, 1) == 2, "a reservation adds a slot");
  duppy_core_wait(core, 0);
  submit(core, 3, DUPPY_THREADED, 0, 0., -1, 0);
  check(take(core, 0, out, &written) == DUPPY_ONE_THREADED && out[0] == 3,
        "a raised budget is usable");
  close_core(&opened);
}

static void test_eligibility(int force_fallback) {
  fixture opened = open_core(force_fallback);
  duppy_core *core = opened.core;
  intptr_t out[CAPACITY];
  size_t written;
  duppy_task task = {0,  0, DUPPY_DIRECT, 1,   DUPPY_EVERY_WORKER,
                     0., 0, NULL,         NULL};
  check(duppy_core_submit(core, &task) == -1 && errno == EINVAL,
        "a pin fails before the pool exists");
  duppy_core_start(core, 2, 4);
  check(duppy_core_submit(core, &task) == 0, "a pin to a worker is accepted");
  task.pin = 2;
  check(duppy_core_submit(core, &task) == -1 && errno == EINVAL,
        "a pin to an unknown worker fails");
  check(take(core, 0, out, &written) == DUPPY_NONE,
        "a pinned task is not taken by another worker");
  check(take(core, 1, out, &written) == DUPPY_ONE_DIRECT,
        "a pinned task is taken by its worker");

  task.pin = DUPPY_ANY_WORKER;
  task.accepted_by = 2;
  task.handle = 5;
  check(duppy_core_submit(core, &task) == 0, "an acceptance mask is accepted");
  check(take(core, 0, out, &written) == DUPPY_NONE,
        "a worker leaves the tasks it does not accept");
  check(take(core, 1, out, &written) == DUPPY_ONE_DIRECT && out[0] == 5,
        "a worker takes the tasks it accepts");

  task.pin = 0;
  check(duppy_core_submit(core, &task) == -1 && errno == EINVAL,
        "a pin to a worker that does not accept the task fails");
  task.pin = DUPPY_ANY_WORKER;
  task.accepted_by = DUPPY_EVERY_WORKER;
  task.rank = DUPPY_RANKS;
  check(duppy_core_submit(core, &task) == -1 && errno == EINVAL,
        "a rank out of range fails");
  task.rank = 0;
  task.fd_count = DUPPY_MAX_FDS + 1;
  check(duppy_core_submit(core, &task) == -1 && errno == EINVAL,
        "too many descriptors fail");
  close_core(&opened);
}

/* Worker 0 is woken for a task either worker may take, then takes one that
   only it accepts: the first must reach worker 1, whose wait would otherwise
   never return. */
static void test_leftover_is_offered(int force_fallback) {
  fixture opened = open_core(force_fallback);
  duppy_core *core = opened.core;
  intptr_t out[CAPACITY];
  size_t written;
  duppy_task task = {
      1,    0,   DUPPY_THREADED, DUPPY_ANY_WORKER, DUPPY_EVERY_WORKER, 0., 0,
      NULL, NULL};
  duppy_core_start(core, 2, 4);
  check(take(core, 0, out, &written) == DUPPY_NONE &&
            take(core, 1, out, &written) == DUPPY_NONE,
        "both workers are idle");
  duppy_core_submit(core, &task);
  task.handle = 2;
  task.task_class = DUPPY_DIRECT;
  task.accepted_by = 1;
  duppy_core_submit(core, &task);
  check(take(core, 0, out, &written) == DUPPY_ONE_DIRECT && out[0] == 2,
        "the woken worker takes the task only it accepts");
  duppy_core_wait(core, 1);
  check(take(core, 1, out, &written) == DUPPY_ONE_THREADED && out[0] == 1,
        "the task it left is offered to the idle worker");
  close_core(&opened);
}

static double cpu_seconds(void) {
  struct timespec used;
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &used);
  return (double)used.tv_sec + (double)used.tv_nsec * 1e-9;
}

/* Waiting out a deadline by polling would burn about a millisecond of CPU for
   each of these. */
static void test_deadline_wait_is_idle(int force_fallback) {
  fixture opened = open_core(force_fallback);
  duppy_core *core = opened.core;
  intptr_t out[CAPACITY];
  size_t written;
  duppy_core_start(core, 1, 4);
  double before = cpu_seconds();
  double earliest = 1;
  for (int i = 0; i < 50; i++) {
    double deadline = duppy_core_now() + 0.0045;
    submit(core, i, DUPPY_DIRECT, 0, 0.0045, -1, 0);
    next(core, 0, out, &written);
    double late = duppy_core_now() - deadline;
    earliest = late < earliest ? late : earliest;
  }
  check(cpu_seconds() - before < 0.015, "waiting for a deadline uses no CPU");
  if (!force_fallback)
    check(earliest < 0.0003,
          "a deadline is met without rounding to the millisecond");
  close_core(&opened);
}

static void test_stop(int force_fallback) {
  fixture opened = open_core(force_fallback);
  duppy_core *core = opened.core;
  intptr_t out[CAPACITY];
  size_t written;
  submit(core, 0, DUPPY_DIRECT, 0, 10., -1, 0);
  submit(core, 1, DUPPY_DIRECT, 0, 0., -1, 0);
  duppy_core_start(core, 1, 4);
  duppy_core_stop(core);
  check(take(core, 0, out, &written) == DUPPY_STOPPED,
        "a stopped core hands out nothing");
  check(submit(core, 2, DUPPY_DIRECT, 0, 0., -1, 0) == 0,
        "a task submitted after stop is dropped quietly");
  check(take(core, 0, out, &written) == DUPPY_STOPPED, "and stays dropped");
  close_core(&opened);
}

int main(void) {
  alarm(60);
  for (int force_fallback = 0; force_fallback <= 1; force_fallback++) {
    int before = checks;
    test_deadlines(force_fallback);
    test_elapsed_delay(force_fallback);
    test_descriptors(force_fallback);
    if (!force_fallback)
      test_unwatchable();
    test_order(force_fallback);
    test_alternation(force_fallback);
    test_batch_bound(force_fallback);
    test_slots(force_fallback);
    test_eligibility(force_fallback);
    test_leftover_is_offered(force_fallback);
    test_deadline_wait_is_idle(force_fallback);
    test_stop(force_fallback);
    printf("core (%s): %d checks\n", backend, checks - before);
  }
#ifdef __linux__
  check(checks == EXPECTED_CHECKS - 1, "every check ran");
#endif
  if (failures) {
    fprintf(stderr, "%d of %d core checks failed\n", failures, checks);
    return 1;
  }
  printf("all %d core checks passed\n", checks);
  return 0;
}
