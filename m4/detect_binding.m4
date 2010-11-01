dnl ===========================================================================
dnl  Helper macro to detect an optional binding

m4_defun([AC_OCAML_COMPARE_VERSION],
  [if test -z "$2" ; then
    VERSION_OK=yes
  else
    AS_VERSION_COMPARE([$1],[$2],
        [VERSION_OK=],
        [VERSION_OK=yes],
        [VERSION_OK=yes])
fi])

m4_defun([AC_OCAML_CHECK_DEPS],
  [dnl
m4_define([deps],[m4_translit([$1],['a-z'],['A-Z'])])
DEPS_CHECK=yes
for i in deps(); do
  eval "dep_check=\$W_$i"
  if test -z "${dep_check}"; then
    DEPS_CHECK=
    break
  fi
done])

m4_defun([AC_MSG_RESULT_NOT],
  [dnl
if test -z "$1"; then
  AC_MSG_RESULT($2)
else
  AC_MSG_ERROR($2)
fi])

AC_DEFUN([AC_CHECK_OCAML_BINDING],[dnl

m4_define([BINDING],[m4_translit([$1],['a-z.'],['A-Z_'])])
m4_define([binding],[m4_translit([$1],['A-Z.'],['a-z_'])])

AC_ARG_WITH([binding()-dir],
   AC_HELP_STRING(
      [--with-binding()-dir=path],
      [look for ocaml-binding() library in "path" (autodetected by default)]))

# Version stuff
m4_define([VERSION_CHECK],[ifelse([$2],[],[],[ >= $2])])

AC_MSG_CHECKING([for ocaml $1 module[]VERSION_CHECK()])

AC_OCAML_CHECK_DEPS([$3])
if test -z $DEPS_CHECK; then
  AC_MSG_RESULT([[]binding() needs $3])
else
  if test -z "${with_[]binding()_dir}" ; then
     if ! ${OCAMLFIND} query $1 > /dev/null 2>&1 ; then
         AC_MSG_RESULT_NOT([$4],[Not found.])
     else
         version="`ocamlfind query -format "%v" $1`"
         AC_OCAML_COMPARE_VERSION([${version}],[$2])
         if test -z "${VERSION_OK}"; then
           AC_MSG_RESULT_NOT([$4],[requires version >= $2 found ${version}.])
         else
           PACKAGES="-package $1"
           for i in $5; do
             PACKAGES="${PACKAGES} -package $i"
           done
           ocamlcflags="${ocamlcflags} $PACKAGES"
           requires="${requires} $1 $5"
           W_[]BINDING()=yes
           LIBS_VERSIONS="${LIBS_VERSIONS} $1=$version"
           AC_MSG_RESULT(ok)
         fi
     fi
  else
    if ! test -r ${with_[]binding()_dir}/META >/dev/null 2>&1; then
      AC_MSG_RESULT_NOT([$4],[Cannot find META file for $1 in ${with_[]binding()_dir}])
    else
      # Grab version
      version=`cat "${with_[]binding()_dir}/META" | grep version | cut -d'=' -f 2 | tr -d ' ' | tr -d '"'`
      AC_OCAML_COMPARE_VERSION([${version}],[$2])
      if test -z "${VERSION_OK}"; then
        AC_MSG_RESULT_NOT([$4],[requires version >= $2 found ${version}.])
      else
        echo ${with_[]binding()_dir} | grep ^/ > /dev/null 2>&1 \
        || with_[]binding()_dir=${PWD}/${with_[]binding()_dir}
        for i in $5; do
          PACKAGES="${PACKAGES} -package $i"
        done
        ocamlcflags="${ocamlcflags} -I ${with_[]binding()_dir} ${PACKAGES}"
        if ! test -z "$6"; then
          for i in $6; do
            CMA_OBJS="${CMA_OBJS} $i.${cma}"
          done
        else
          CMA_OBJS=$1.${cma}
        fi  
        ocamllflags="${ocamllflags} ${CMA_OBJS}"
        requires="${requires} $1"
        W_[]BINDING()=yes
        LIBS_VERSIONS="${LIBS_VERSIONS} $1=$version"
        AC_MSG_RESULT(ok)
      fi
    fi
  fi
fi

AC_SUBST(W_[]BINDING())
if test -z "${W_[]BINDING()}" ; then
    w_[]BINDING()="no (requires $1)"
else
    w_[]BINDING()=yes
fi])


