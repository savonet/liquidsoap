dnl autoconf macros for OCaml
dnl
dnl Copyright © 2009      Richard W.M. Jones
dnl Copyright © 2009      Stefano Zacchiroli
dnl Copyright © 2000-2005 Olivier Andrieu
dnl Copyright © 2000-2005 Jean-Christophe Filliâtre
dnl Copyright © 2000-2005 Georges Mariano
dnl
dnl For documentation, please read the ocaml.m4 man page.

AC_DEFUN([AC_PROG_OCAML],
[dnl
  # checking for ocamlc
  AC_CHECK_TOOL([OCAMLC],[ocamlc],[no])

  if test "$OCAMLC" = "no"; then
    AC_MSG_ERROR(Cannot find ocamlc.)
  fi

  AC_SUBST([OCAMLC])

  OCAMLVERSION=`$OCAMLC -v | sed -n -e 's|.*version* *\(.*\)$|\1|p'`
  AC_MSG_RESULT([OCaml version is $OCAMLVERSION])
  # Check if version is >= 3.12.0
  AC_MSG_CHECKING([if ocaml compiler supports first-class modules])
  AS_VERSION_COMPARE([$OCAMLVERSION],[3.12.0],[],[OCAML_HAS_FIRST_CLASS_MODULES="yes"],[OCAML_HAS_FIRST_CLASS_MODULES="yes"])
  if test -n "${OCAML_HAS_FIRST_CLASS_MODULES}"; then
    AC_MSG_RESULT([yes])
  else
    AC_MSG_RESULT([no])
  fi
  AC_SUBST(OCAML_HAS_FIRST_CLASS_MODULES)

  # If OCAMLLIB is set, use it
  if test "$OCAMLLIB" = ""; then
     OCAMLLIB=`$OCAMLC -where 2>/dev/null || $OCAMLC -v|tail -1|cut -d ' ' -f 4`
  else
     AC_MSG_RESULT([OCAMLLIB previously set; preserving it.])
  fi
  AC_MSG_RESULT([OCaml library path is $OCAMLLIB])

  AC_SUBST([OCAMLVERSION])
  AC_SUBST([OCAMLLIB])

  # checking for ocamlopt
  AC_CHECK_TOOL_STRICT([OCAMLOPT],[ocamlopt],[no])
  OCAMLBEST=byte
  OCAML_DYNLINK=byte-dyn
  if test "$OCAMLOPT" = "no"; then
    AC_MSG_WARN([Cannot find ocamlopt; bytecode compilation only.])
  else
    TMPVERSION=`$OCAMLOPT -v | sed -n -e 's|.*version* *\(.*\)$|\1|p' `
    if test "$TMPVERSION" != "$OCAMLVERSION" ; then
      AC_MSG_RESULT([versions differs from ocamlc; ocamlopt discarded.])
        OCAMLOPT=no
    else
        OCAMLBEST="byte opt"
        OCAML_DYNLINK="byte-dyn opt-dyn"
    fi
  fi

  AC_SUBST([OCAMLBEST])
  AC_SUBST([OCAML_DYNLINK])

  # checking for ocamlc.opt
  AC_CHECK_TOOL_STRICT([OCAMLCDOTOPT],[ocamlc.opt],[no])
  if test "$OCAMLCDOTOPT" != "no"; then
    TMPVERSION=`$OCAMLCDOTOPT -v | sed -n -e 's|.*version* *\(.*\)$|\1|p' `
    if test "$TMPVERSION" != "$OCAMLVERSION" ; then
      AC_MSG_RESULT([versions differs from ocamlc; ocamlc.opt discarded.])
    else
      OCAMLC=$OCAMLCDOTOPT
    fi
  fi

  # checking for ocamlopt.opt
  if test "$OCAMLOPT" != "no" ; then
     AC_CHECK_TOOL_STRICT([OCAMLOPTDOTOPT],[ocamlopt.opt],[no])
     if test "$OCAMLOPTDOTOPT" != "no"; then
       TMPVERSION=`$OCAMLOPTDOTOPT -v | sed -n -e 's|.*version* *\(.*\)$|\1|p' `
       if test "$TMPVERSION" != "$OCAMLVERSION" ; then
         AC_MSG_RESULT([version differs from ocamlc; ocamlopt.opt discarded.])
	else
	 OCAMLOPT=$OCAMLOPTDOTOPT
       fi
    fi
  fi

  AC_SUBST([OCAMLOPT])

  # checking for ocaml toplevel
  AC_CHECK_TOOL_STRICT([OCAML],[ocaml],[no])

  AC_SUBST([OCAML])

  # checking for ocamldep
  AC_CHECK_TOOL_STRICT([OCAMLDEP],[ocamldep],[no])
  if test "$OCAMLDEP" = "no"; then
    AC_MSG_ERROR(Cannot find ocamlmklib.)
  else
    AC_CHECK_TOOL_STRICT([OCAMLDEPOPT],[ocamldep.opt],[no])
    if test "$OCAMLDEPOPT" != "no"; then
      OCAMLDEP=$OCAMLDEPOPT
    fi
  fi

  AC_SUBST([OCAMLDEP])

  # checking for ocamlmktop
  AC_CHECK_TOOL_STRICT([OCAMLMKTOP],[ocamlmktop],[no])

  AC_SUBST([OCAMLMKTOP])

  # checking for ocamlmklib
  AC_CHECK_TOOL_STRICT([OCAMLMKLIB],[ocamlmklib],[no])
  if test "$OCAMLMKLIB" = "no"; then
    AC_MSG_ERROR(Cannot find ocamlmklib.)
  fi

  AC_SUBST([OCAMLMKLIB])

  # checking for ocamldoc
  AC_CHECK_TOOL([OCAMLDOC],[ocamldoc],[no])
  if test "$OCAMLDOC" != "no"; then
    AC_CHECK_TOOL([OCAMLDOCOPT],[ocamldoc.opt],[no])
    if test "$OCAMLDOCOPT" != "no"; then
      OCAMLDOC=$OCAMLDOCOPT
    fi
  fi

  AC_SUBST([OCAMLDOC])

  # checking for ocamlbuild
  AC_CHECK_TOOL([OCAMLBUILD],[ocamlbuild],[no])

  AC_SUBST([OCAMLBUILD])
])


AC_DEFUN([AC_PROG_OCAMLLEX],
[dnl
  # checking for ocamllex
  AC_CHECK_TOOL([OCAMLLEX],[ocamllex],[no])
  if test "$OCAMLLEX" != "no"; then
    AC_CHECK_TOOL([OCAMLLEXDOTOPT],[ocamllex.opt],[no])
    if test "$OCAMLLEXDOTOPT" != "no"; then
	OCAMLLEX=$OCAMLLEXDOTOPT
    fi
  fi
  AC_SUBST([OCAMLLEX])
])

AC_DEFUN([AC_PROG_OCAMLYACC],
[dnl
  AC_CHECK_TOOL([OCAMLYACC],[ocamlyacc],[no])
  AC_SUBST([OCAMLYACC])
])


AC_DEFUN([AC_PROG_CAMLP4],
[dnl
  AC_REQUIRE([AC_PROG_OCAML])dnl


  AC_ARG_ENABLE([camlp4],
    AC_HELP_STRING([--disable-camlp4],
                   [disable camlp4 auto-detection.]))

  # checking for camlp4
  if test "x$enable_camlp4" != "xno"; then
    AC_CHECK_TOOL_STRICT([CAMLP4],[camlp4],[no])
    if test "$CAMLP4" != "no"; then
       TMPVERSION=`$CAMLP4 -v 2>&1| sed -n -e 's|.*version *\(.*\)$|\1|p' | tr -d '\r'`
       if test "$TMPVERSION" != "$OCAMLVERSION" ; then
  	  AC_MSG_RESULT([versions differs from ocamlc])
          CAMLP4=no
       fi
    fi
    AC_SUBST([CAMLP4])

    # checking for companion tools
    AC_CHECK_TOOL_STRICT([CAMLP4BOOT],[camlp4boot],[no])
    AC_CHECK_TOOL_STRICT([CAMLP4O],[camlp4o],[no])
    AC_CHECK_TOOL_STRICT([CAMLP4OF],[camlp4of],[no])
    AC_CHECK_TOOL_STRICT([CAMLP4OOF],[camlp4oof],[no])
    AC_CHECK_TOOL_STRICT([CAMLP4ORF],[camlp4orf],[no])
    AC_CHECK_TOOL_STRICT([CAMLP4PROF],[camlp4prof],[no])
    AC_CHECK_TOOL_STRICT([CAMLP4R],[camlp4r],[no])
    AC_CHECK_TOOL_STRICT([CAMLP4RF],[camlp4rf],[no])
  else
    CAMLP4=no
    CAMLP4BOOT=no
    CAMLP4O=no
    CAMLP4OF=no
    CAMLP4OOF=no
    CAMLP4ORF=no
    CAMLP4PROF=no
    CAMLP4R=no
    CAMLP4RF=no
  fi

  AC_SUBST([CAMLP4BOOT])
  AC_SUBST([CAMLP4O])
  AC_SUBST([CAMLP4OF])
  AC_SUBST([CAMLP4OOF])
  AC_SUBST([CAMLP4ORF])
  AC_SUBST([CAMLP4PROF])
  AC_SUBST([CAMLP4R])
  AC_SUBST([CAMLP4RF])
])

AC_DEFUN([AC_PROG_CAMLIDL],
[dnl
  AC_CHECK_TOOL(CAMLIDL,camlidl,no)
  AC_SUBST(CAMLIDL)
])

AC_DEFUN([AC_PROG_FINDLIB],
[dnl
  AC_REQUIRE([AC_PROG_OCAML])dnl

  # checking for ocamlfind
  AC_CHECK_TOOL([OCAMLFIND],[ocamlfind],[no])
  AC_SUBST([OCAMLFIND])
])

AC_DEFUN([AC_CHECK_OCAML_STDLIB],
[dnl
  AC_REQUIRE([AC_PROG_FINDLIB])dnl

  AC_MSG_CHECKING([for ocaml standard library path])
  OCAML_STDLIB=`$OCAMLFIND printconf stdlib`
  AC_SUBST(OCAML_STDLIB)
  AC_MSG_RESULT([$OCAML_STDLIB])
])

dnl Thanks to Jim Meyering for working this next bit out for us.
dnl XXX We should define AS_TR_SH if it's not defined already
dnl (eg. for old autoconf).
AC_DEFUN([AC_CHECK_OCAML_PKG],
[dnl
  AC_REQUIRE([AC_PROG_FINDLIB])dnl

  AC_ARG_WITH([$1-dir],AC_HELP_STRING([--with-$1-dir=path],
              [use "path" as the location of ocaml-$1 (autodetected by default)]))
  AC_MSG_CHECKING([for OCaml library $1])

  unset found
  unset pkg
  found=no
  if test -z "$with_$1_dir"; then
    for pkg in $1 $2 ; do
      if $OCAMLFIND query $pkg >/dev/null 2>/dev/null; then
        AC_MSG_RESULT([found])
        AS_TR_SH([OCAML_PKG_$1])=$pkg
        AS_TR_SH([OCAML_DIR_$1])=`$OCAMLFIND query $1`
        found=yes
        break
      fi
    done
  else
    echo $with_$1_dir | grep ^/ > /dev/null 2>&1 || with_$1_dir=$PWD/$with_$1_dir
    AS_TR_SH([OCAML_PKG_$1])=no
    OCAML_DIR_$1="$with_$1_dir"
    found=yes
  fi
  if test "$found" = "no" ; then
    AC_MSG_RESULT([not found])
    AS_TR_SH([OCAML_HAS_$1])=no
    AS_TR_SH([OCAML_PKG_$1])=no
  else
    AS_TR_SH([OCAML_HAS_$1])=yes
  fi

  AC_SUBST(AS_TR_SH([OCAML_PKG_$1]))
])


AC_DEFUN([AC_CHECK_OCAML_MODULE],
[dnl
  AC_MSG_CHECKING([for OCaml module $2])

  cat > conftest.ml <<EOF
open $3
EOF
  unset found
  for $1 in $$1 $4 ; do
    if $OCAMLC -c -I "$$1" conftest.ml >&5 2>&5 ; then
      found=yes
      break
    fi
  done

  if test "$found" ; then
    AC_MSG_RESULT([$$1])
  else
    AC_MSG_RESULT([not found])
    $1=no
  fi
  AC_SUBST([$1])
])


dnl XXX Cross-compiling
AC_DEFUN([AC_CHECK_OCAML_WORD_SIZE],
[dnl
  AC_REQUIRE([AC_PROG_OCAML])dnl
  AC_MSG_CHECKING([for OCaml compiler word size])
  cat > conftest.ml <<EOF
  print_endline (string_of_int Sys.word_size)
  EOF
  OCAML_WORD_SIZE=`$OCAML conftest.ml`
  AC_MSG_RESULT([$OCAML_WORD_SIZE])
  AC_SUBST([OCAML_WORD_SIZE])
])

AC_DEFUN([AC_CHECK_OCAML_OS_TYPE],
[dnl
  AC_REQUIRE([AC_PROG_OCAML])dnl
  AC_MSG_CHECKING([OCaml Sys.os_type])

  cat > conftest.ml <<EOF
  print_string(Sys.os_type);;
EOF

  OCAML_OS_TYPE=`$OCAML conftest.ml`
  AC_MSG_RESULT([$OCAML_OS_TYPE])
  AC_SUBST([OCAML_OS_TYPE])
])
