#ifdef _WIN32
#include <processthreadsapi.h>
#include <windows.h>
#else
#define _GNU_SOURCE

#include <pthread.h>
#include <unistd.h>

#if defined(__FreeBSD__) || defined(__OpenBSD__)
#include <pthread_np.h>
#endif
#endif

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/osdeps.h>
#include <caml/unixsupport.h>
#include <errno.h>
#include <locale.h>
#include <stddef.h>
#include <stdio.h>
#include <time.h>

/* Some libraries mess with locale. In OCaml, locale should always
 * be "C", otherwise float_of_string and other functions do not behave
 * as expected. This issues arises in particular when using telnet
 * commands that need floats and when loading modules in bytecode mode.. */
CAMLprim value liquidsoap_set_locale(value _locale) {
  CAMLparam1(_locale);
  const char *locale = String_val(_locale);

#ifdef _WIN32
  char var[LOCALE_NAME_MAX_LENGTH];
  snprintf(var, LOCALE_NAME_MAX_LENGTH, "LANG=%s", locale);
  putenv(var);
  snprintf(var, LOCALE_NAME_MAX_LENGTH, "LC_ALL=%s", locale);
  putenv(var);
#else
  setenv("LANG", locale, 1);
  setenv("LC_ALL", locale, 1);
#endif
  /* This set the locale. */
  setlocale(LC_ALL, locale);
  CAMLreturn(Val_unit);
}

CAMLprim value liquidsoap_get_timezone() {
  tzset();
  return Val_long(timezone);
}

CAMLprim value liquidsoap_get_timezone_by_name() {
  CAMLparam0();
  CAMLlocal1(ret);
  tzset();
  ret = caml_alloc_tuple(2);
  Store_field(ret, 0, caml_copy_string(tzname[0]));
  Store_field(ret, 1, caml_copy_string(tzname[1]));
  CAMLreturn(ret);
}

CAMLprim value liquidsoap_mktime(value _tm) {
  CAMLparam1(_tm);
  struct tm tm;
  time_t time;

  tm.tm_sec = Int_val(Field(_tm, 0));
  tm.tm_min = Int_val(Field(_tm, 1));
  tm.tm_hour = Int_val(Field(_tm, 2));
  tm.tm_mday = Int_val(Field(_tm, 3));
  tm.tm_mon = Int_val(Field(_tm, 4));
  tm.tm_year = Int_val(Field(_tm, 5));
  tm.tm_wday = 0;
  tm.tm_yday = 0;
  tm.tm_isdst =
      Field(_tm, 6) == Val_int(0) ? -1 : Bool_val(Field(Field(_tm, 6), 0));
  time = mktime(&tm);
  if (time == -1)
    unix_error(ERANGE, "mktime", Nothing);

  CAMLreturn(caml_copy_double((double)time));
}

CAMLprim value liquidsoap_get_pagesize() {
#ifdef _WIN32
  SYSTEM_INFO systemInfo;
  GetSystemInfo(&systemInfo);
  return Val_int(systemInfo.dwPageSize);
#else
  return Val_int(getpagesize());
#endif
}

CAMLprim value liquidsoap_set_thread_name(value _name) {
#if defined(_WIN32)
  char_os *thread_name = caml_stat_strdup_to_os(String_val(_name));
  SetThreadDescription(GetCurrentThread(), thread_name);
  caml_stat_free(thread_name);
#elif defined(__APPLE__)
  pthread_setname_np(String_val(_name));
#elif defined(__NetBSD__)
  pthread_setname_np(pthread_self(), "%s", String_val(_name));
#else
  pthread_setname_np(pthread_self(), String_val(_name));
#endif
  return Val_unit;
}
