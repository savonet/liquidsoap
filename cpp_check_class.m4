dnl $1: linker lib name
dnl $2: included file
dnl $3: tested class
dnl $4: emitted variable
AC_DEFUN([AC_CPP_CHECK_CLASS],
[AC_LANG_PUSH([C++])
 SAVED_LDFLAGS=$LDFLAGS
 LDFLAGS="$LDFLAGS -l$1"
 AC_MSG_CHECKING([for class $3 in $1 library])
 AC_LINK_IFELSE(
   [AC_LANG_PROGRAM([#include <$2>],
     [$3 *x = NULL])],
   [TEST_LIBS="$TEST_LIBS -l$1"] [RESULT=1],
   [RESULT=])
 if test -z $RESULT; then
   AC_MSG_RESULT([not found])
 else
   AC_DEFINE([$4], [1], [Defined if class $3 has been found in $1 library])
   AC_MSG_RESULT([ok])
 fi
 $4=$RESULT
 AC_SUBST([$4])
 LDFLAGS=$SAVED_LDFLAGS
 AC_LANG_POP([C++])])
