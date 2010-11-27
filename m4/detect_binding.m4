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
  [ifelse([$1],[],[AC_MSG_RESULT($2)],[AC_MSG_ERROR($2)])])

AC_DEFUN([AC_CHECK_OCAML_BINDING],[dnl

m4_define([BINDING],[m4_translit([$1],['a-z.-'],['A-Z__'])])
m4_define([binding],[m4_translit([$1],['A-Z.-'],['a-z__'])])

AC_ARG_WITH([binding()-dir],
   AC_HELP_STRING(
      [--with-binding()-dir=path],
      [look for ocaml-binding() library in "path" (autodetected by default)]))

# Version stuff
m4_define([VERSION_CHECK],[ifelse([$2],[],[],[ >= $2])])

AC_MSG_CHECKING([for ocaml $1 module[]VERSION_CHECK()])

OCAML_CHECK="${OCAMLFIND} query $1"
if ! test -z "$7"; then
  OCAML_CHECK="$7"
fi

AC_OCAML_CHECK_DEPS([$3])
if test -z $DEPS_CHECK; then
  AC_MSG_RESULT([[]binding() needs $3])
else
  if test -z "${with_[]binding()_dir}" ; then
     if ! ${OCAML_CHECK} > /dev/null 2>&1 ; then
         AC_MSG_RESULT_NOT([$4],[Not found.])
     else
         BINDING()_version="`${OCAMLFIND} query -format "%v" $1 2>/dev/null`"
         AC_OCAML_COMPARE_VERSION([${[]BINDING()_version}],[$2])
         if test -z "${VERSION_OK}"; then
           AC_MSG_RESULT_NOT([$4],[requires version >= $2 found ${[]BINDING()_version}.])
         else
           BINDING()_PACKAGES="`${OCAMLFIND} query -r -separator " " -format "-package %p" $1 $5 2>/dev/null`"
           ocamlcflags="${ocamlcflags} ${[]BINDING()_PACKAGES}"
           W_[]BINDING()=yes
           LIBS_VERSIONS="${LIBS_VERSIONS} $1=$[]BINDING()_version"
           AC_MSG_RESULT(ok)
         fi
     fi
  else
    BINDING()_STOP_CHECK=
    BINDING()_version=changequote({,})"[unknown version]"changequote([,])
    BINDING()_requires=
    if test -r ${with_[]binding()_dir}/META >/dev/null 2>&1; then
      # Grab version
      BINDING()_version=`cat "${with_[]binding()_dir}/META" | grep version | cut -d'=' -f 2 | tr -d ' ' | tr -d '"'`
      AC_OCAML_COMPARE_VERSION([${[]BINDING()_version}],[$2])
      if test -z "${VERSION_OK}"; then
        AC_MSG_RESULT_NOT([$4],[requires version >= $2 found ${[]BINDING()version}.])
        BINDING()_STOP_CHECK=yes
      fi
      BINDING()_requires=`cat "${with_[]binding()_dir}/META" | grep 'requires' | cut -d '=' -f 2 | tr -d '"'`
    else
      if ! test -z "$2"; then
        AC_MSG_RESULT_NOT([$4],[cannot find version from META file.])
        BINDING()_STOP_CHECK=yes
      fi
    fi
    if test -z "${STOP_CHECK}"; then
      if ! test -z "$6"; then
        for i in $6; do
          CMA_OBJS="${CMA_OBJS} $i.${cma}"
        done
      else
        CMA_OBJS=$1.${cma}
      fi 
      BINDING()_PACKAGES="`${OCAMLFIND} query -r -separator " " -format "-package %p" $5 2>/dev/null`"
      echo ${with_[]binding()_dir} | grep ^/ > /dev/null 2>&1 \
          || with_[]binding()_dir=${PWD}/${with_[]binding()_dir}
      ocamlcflags="${ocamlcflags} -I ${with_[]binding()_dir} ${[]BINDING()_PACKAGES}"
      # We need to recurse here because
      # some package may not be registered using ocamlfind
      for i in ${[]BINDING()_requires}; do
        BINDING()_PACKAGES="${[]BINDING()_PACKAGES} `${OCAMLFIND} query -r -separator " " -format "-package %p" $i 2>/dev/null`"
      done
      ocamllflags="${ocamllflags} ${[]BINDING()_PACKAGES} ${CMA_OBJS}"
      W_[]BINDING()=yes
      LIBS_VERSIONS="${LIBS_VERSIONS} $1=$[]BINDING()_version"
      AC_MSG_RESULT(ok)
    fi
  fi
fi

AC_SUBST(W_[]BINDING())
if test -z "${W_[]BINDING()}" ; then
    w_[]BINDING()="no (requires $1)"
else
    w_[]BINDING()=yes
fi])


