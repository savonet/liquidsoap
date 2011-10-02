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

m4_defun([AC_COND_PLUGIN],
  [ifelse([$1],[],
[AC_ARG_ENABLE([$2-dynamic-plugin],
   AC_HELP_STRING(
      [--enable-$2-dynamic-plugin],
      [Compile $2 as an optional dynamic plugin.]))])])

AC_DEFUN([AC_CHECK_OCAML_BINDING],[dnl

m4_define([BINDING],[m4_translit([$1],['a-z.-'],['A-Z__'])])
m4_define([binding],[m4_translit([$1],['A-Z.-'],['a-z__'])])

dnl $1 = PKG_NAME
dnl $2 = PKG_VERSION
dnl $3 = PKG_DEPS
dnl $4 = PKG_MANDATORY
dnl $5 = PKG_USED (for instance sdl.mixer au lieu of sdl. Should only be used for bindings not provided by us..)
dnl $6 = DYNAMIC_PLUGIN
dnl $7 = PKG_CMA (used for duppy.syntax and flac.ogg when locally compiled..)

if test -n "$5"; then
  BINDING_PKGS="$5"
else
  BINDING_PKGS="$1"
fi

AC_ARG_WITH([binding()-dir],
   AC_HELP_STRING(
      [--with-binding()-dir=path],
      [look for ocaml-binding() library in "path" (autodetected by default)]))

AC_COND_PLUGIN([$6],[$1])

dnl Version stuff
m4_define([VERSION_CHECK],[ifelse([$2],[],[],[ >= $2])])

AC_MSG_CHECKING([for ocaml $1 module[]VERSION_CHECK()])

OCAML_CHECK="${OCAMLFIND} query $1"

dnl This (horrible) macro does the following:
dnl Detect optional binding
dnl If builtin and provided by ocamlfind,
dnl fills liquidsoap_ocamlcflags with "-package deps" for
dnl each dependency
dnl If builtin and provided by us, fills
dnl liquidsoap_ocamlcflags with "-I /path/to/ocaml-foo/src"
dnl and liquidsoap_ocamllfflags with "foo.cmxa"
dnl If plugin and provided by ocamlfind,
dnl fills plugin_packages with "deps"
dnl If plugin and provided by us, fills
dnl plugin_ocamlcflags with "-I /path/to/ocaml-foo/src"
dnl and plugin_ocamllflags with "foo.cmxa"
dnl Ultimately, plugin_ocamlcflags and plugin_ocamllflags
dnl are added to the global $PLUGINS_DATA variable and 
dnl $PLUGINS is updated with the name of this plugin.
dnl W_plugin is set to "yes" for builtin compilation
dnl and "binding" for plugin compilation.

if test "x$enable_[]binding()_dynamic_plugin" = "xyes" ; then
  BINDING()_SHARED="yes"
  PLUGINS="$PLUGINS binding()"
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
           if test -z "${[]BINDING()_SHARED}"; then
             BINDING()_PACKAGES="`${OCAMLFIND} query -r -separator " " -format "-package %p" $BINDING_PKGS 2>/dev/null`"
             liquidsoap_ocamlcflags="${liquidsoap_ocamlcflags} ${[]BINDING()_PACKAGES}"
             W_[]BINDING()=yes
           else
             []binding()_packages="`${OCAMLFIND} query -r -separator " " -format "%p" $BINDING_PKGS 2>/dev/null`"
             W_[]BINDING()=[]binding()
           fi
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
      BINDING()_version=`cat "${with_[]binding()_dir}/META" | grep version | cut -d'=' -f 2 | tr -d ' ' | tr -d '"' | head -n 1`
      AC_OCAML_COMPARE_VERSION([${[]BINDING()_version}],[$2])
      if test -z "${VERSION_OK}"; then
        AC_MSG_RESULT_NOT([$4],[requires version >= $2 found ${[]BINDING()_version}.])
        BINDING()_STOP_CHECK=yes
      fi
      BINDING()_requires=`cat "${with_[]binding()_dir}/META" | grep 'requires' | cut -d '=' -f 2 | tr -d '"'`
      BINDING()_path="${with_[]binding()_dir}"
    else
      BINDING()_path=`${OCAMLFIND} -query $1 2>/dev/null`
      if ! test -z "$2"; then
        AC_MSG_RESULT_NOT([$4],[cannot find version from META file.])
        BINDING()_STOP_CHECK=yes
      fi
    fi
    if test -z "${BINDING()_STOP_CHECK}"; then
      BINDING()_PACKAGES="`${OCAMLFIND} query -r -separator " " -format "-package %p" $BINGING_PKGS 2>/dev/null`"
      echo ${with_[]binding()_dir} | grep ^/ > /dev/null 2>&1 \
          || with_[]binding()_dir=${PWD}/${with_[]binding()_dir}
      if test -z "${[]BINDING()_SHARED}"; then
        liquidsoap_ocamlcflags="${liquidsoap_ocamlcflags} -I ${with_[]binding()_dir} ${[]BINDING()_PACKAGES}"
      else
        []binding()_ocamlcflags="-I ${with_[]binding()_dir} ${[]BINDING()_PACKAGES}"
      fi
      # We need to recurse here because
      # some package may not be registered using ocamlfind
      if test -n "$7"; then
        BINDING()_CMA=$7.${cma}
      else
        BINDING()_CMA=$1.${cma}
      fi
      for i in ${[]BINDING()_requires}; do
        BINDING()_PACKAGES="${[]BINDING()_PACKAGES} `${OCAMLFIND} query -r -separator " " -format "-package %p" $i 2>/dev/null`"
      done
      if test -z "${[]BINDING()_SHARED}"; then
        liquidsoap_ocamllflags="${liquidsoap_ocamllflags} ${[]BINDING()_PACKAGES} ${[]BINDING()_CMA}"
        W_[]BINDING()=yes
      else
        []binding()_ocamllflags="${[]BINDING()_PACKAGES} ${[]BINDING()_CMA}"
        W_[]BINDING()=[]binding()
      fi
      LIBS_VERSIONS="${LIBS_VERSIONS} $1=$[]BINDING()_version"
      AC_MSG_RESULT(ok)
    fi
  fi
fi

AC_SUBST(W_[]BINDING())
if test -z "${W_[]BINDING()}" ; then
    w_[]BINDING()="no (requires $1)"
else
    if test -z "${[]BINDING()_SHARED}"; then
      w_[]BINDING()=yes
    else
      PLUGINS_DATA="$PLUGINS_DATA
[]binding()_ocamlcflags=${[]binding()_ocamlcflags}
[]binding()_ocamllflags=${[]binding()_ocamllflags}
[]binding()_packages=${[]binding()_packages}"
      w_[]BINDING()=plugin
    fi
fi])


