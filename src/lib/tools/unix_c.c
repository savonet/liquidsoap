#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <locale.h>
#include <stdio.h>
#include <stddef.h>
#include <time.h>

#ifdef WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

/* Some libraries mess with locale. In OCaml, locale should always
 * be "C", otherwise float_of_string and other functions do not behave
 * as expected. This issues arises in particular when using telnet 
 * commands that need floats and when loading modules in bytecode mode.. */
CAMLprim value liquidsoap_set_locale(value unit)
{
  /* This will prevent further call to setlocale to override
   * "C" */
#ifdef WIN32
  putenv("LANG=C");
  putenv("LC_ALL=C");
#else
  setenv("LANG","C",1);
  setenv("LC_ALL","C",1);
#endif
  /* This set the locale to "C". */
  setlocale (LC_ALL, "C");
  return Val_unit;
}

CAMLprim value liquidsoap_get_timezone() {
  tzset();
  return Val_long(timezone);
}

CAMLprim value liquidsoao_get_pagesize() {
#ifdef WIN32
  SYSTEM_INFO systemInfo;
  GetSystemInfo(&systemInfo);
  return Val_int(systemInfo.dwPageSize);
#else
  return Val_int(getpagesize());
#endif
}
