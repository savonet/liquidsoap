AC_DEFUN([AC_CHECK_OCAML_COMPILERS],[
OCAMLFIND_LDCONF=""
AC_ARG_ENABLE([ldconf], AC_HELP_STRING([--disable-ldconf],[don't modify the dynamic loader configuration file (default is enable)]),[ac_enable_ldconf=$enableval],[ac_enable_ldconf=$enableval],[ac_enable_ldconf=yes])
if test "$ac_enable_ldconf" = no ; then
	AC_MSG_RESULT([disabling modification of ld.conf])
	OCAMLFIND_LDCONF=dummy
fi
AC_SUBST(OCAMLFIND_LDCONF)

# Check for Ocaml compilers

AC_PROG_OCAML()

AC_CHECK_TOOL(CAMLIDL,camlidl,no)
AC_SUBST(CAMLIDL)

AC_PROG_OCAMLLEX()

AC_PROG_OCAMLYACC()

AC_PROG_CAMLP4()

AC_PROG_FINDLIB()

# Check if caml/threads.h is present 
AC_CHECK_HEADER([caml/threads.h],[CAML_THREADS=yes],[],[#include <caml/misc.h>])

AC_ARG_ENABLE([debugging],
   AC_HELP_STRING(
      [--disable-debugging],
      [disable debugging information (backtrace printing in particular)]))

if test "x$enable_debugging" = "xyes" ; then
  OCAMLFLAGS="$OCAMLFLAGS -g"
fi

AC_ARG_ENABLE([profiling],
   AC_HELP_STRING(
      [--enable-profiling],
      [compile to generate profiling infomation]))
if test "x$enable_profiling" = "xyes" ; then
  OCAMLNCFLAGS="$OCAMLNCFLAGS -p"
fi
AC_SUBST(OCAMLNCFLAGS)

AC_ARG_ENABLE([nativecode],
   AC_HELP_STRING(
      [--disable-nativecode],
      [compile in bytecode]))

AC_ARG_ENABLE([custom],
   AC_HELP_STRING(
      [--disable-custom],
      [disable custom mode for bytecode compilation (use if you know what you are doing)]))

CAMLLIBPATH=$OCAMLLIB
AC_SUBST(CAMLLIBPATH)

AC_SUBST(CAMLIDL)
AC_SUBST(OCAMLFLAGS)
])
