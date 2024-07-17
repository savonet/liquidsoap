
AC_INIT([liquidsoap],[0.2.0])
AC_PREREQ(2.57)
AC_CONFIG_SRCDIR([src/main.ml])
AC_PROG_INSTALL

echo > src/configure.ml

# oRTP
AC_PROG_CC

if test -z `which pkg-config 2>/dev/null` ; then
  AC_MSG_ERROR(Cannot find pkg-config)
fi

AC_MSG_CHECKING(for glib-2.0)
if pkg-config --exists glib-2.0 ; then
  AC_MSG_RESULT(ok)
else
  AC_MSG_RESULT(no)
  AC_MSG_ERROR(Cannot find glib-2.0)
fi

AC_ARG_WITH([ortp-include],
   AC_HELP_STRING(
      [--with-ortp-include=path],
      [look for ortp headers in path (defaults to /usr/include/ortp)]))

# oRTP

AC_MSG_CHECKING(for libortp)

if test -z "$with_ortp_include" ; then
  ORTP_INCLUDE=/usr/include/ortp
else
  ORTP_INCLUDE=$with_ortp_include
fi

if test \! -f ${ORTP_INCLUDE}/ortp.h ; then
  AC_MSG_RESULT(Header not found: ${ORTP_INCLUDE}/ortp.h)
  W_RTP=""
  w_RTP=no
else
  W_RTP=yes
  w_RTP=yes
  AC_MSG_RESULT(ok)
fi
AC_SUBST(W_RTP)


AC_SUBST(ORTP_INCLUDE)

#
# OCaml stuff
#
###############################################################################

AC_PATH_PROG(OCAMLDEP,ocamldep,no)
if test "$OCAMLDEP" = no ; then
	AC_MSG_ERROR(Cannot find ocamldep.)
fi

AC_PATH_PROG(OCAMLFIND,ocamlfind,no)
if test "$OCAMLFIND" = no ; then
	AC_MSG_ERROR(Cannot find ocamlfind.)
fi

AC_PATH_PROG(OCAMLLEX,ocamllex,no)
if test "$OCAMLLEX" = no ; then
	AC_MSG_ERROR(Cannot find ocamllex.)
fi

AC_PATH_PROG(OCAMLYACC,ocamlyacc,no)
if test "$OCAMLYACC" = no ; then
	AC_MSG_ERROR(Cannot find ocamlyacc.)
fi

AC_ARG_ENABLE([nativecode],
   AC_HELP_STRING(
      [--enable-nativecode],
      [compile liquidsoap in nativecode, i.e. with ocamlopt (disabled)]))

if test "$enable_nativecode" = "yes" ; then
    cma=cmxa
    cmo=cmx
    cmi=cmi
    ocamlcflags=""
    ocamllflags="-linkpkg"
    OCAMLC="$OCAMLFIND ocamlopt"
    _DEP_OPTS="-native"
else
    cma=cma
    cmo=cmo
    cmi=cmi
    ocamlcflags="-g -dtypes"
    ocamllflags="-linkpkg"
    OCAMLC="$OCAMLFIND ocamlc"
    _DEP_OPTS=""
fi

#
# Libs
#
###############################################################################

#
# Dtools
#

AC_ARG_WITH([dtools-dir],
   AC_HELP_STRING(
      [--with-dtools-dir=path],
      [look for dtools library in "path" (autodetected by default)]))

AC_MSG_CHECKING(for dtools)
if test -z "$with_dtools_dir" ; then
   if ! $OCAMLFIND query dtools > /dev/null 2>&1; then
       AC_MSG_ERROR(Not found.)
   fi
else
   echo $with_dtools_dir | grep ^/ > /dev/null 2>&1 \
   || with_dtools_dir=$PWD/$with_dtools_dir
   ocamlcflags="$ocamlcflags -I $with_dtools_dir"
   ocamllflags="$ocamllflags dtools.$cma"
fi
AC_MSG_RESULT(ok)

#
# Shout
#

AC_ARG_WITH([shout-dir],
   AC_HELP_STRING(
      [--with-shout-dir=path],
      [look for ocaml-shout library in "path" (autodetected by default)]))

AC_MSG_CHECKING(for ocaml-shout)
if test -z "$with_shout_dir" ; then
   if ! $OCAMLFIND query shout > /dev/null 2>&1 ; then
       AC_MSG_ERROR(Not found.)
   fi
else
   echo $with_shout_dir | grep ^/ > /dev/null 2>&1 \
   || with_shout_dir=$PWD/$with_shout_dir
   ocamlcflags="$ocamlcflags -I $with_shout_dir"
   ocamllflags="$ocamllflags shout.$cma"
fi
AC_MSG_RESULT(ok)

#
# Vorbis
#

AC_ARG_WITH([vorbis-dir],
   AC_HELP_STRING(
      [--with-vorbis-dir=path],
      [look for ocaml-vorbis library in "path" (autodetected by default)]))

AC_MSG_CHECKING(for ocaml-vorbis)
if test -z "$with_vorbis_dir" ; then
   if ! $OCAMLFIND query vorbis > /dev/null 2>&1 ; then
       AC_MSG_ERROR(Not found.)
   fi
else
   echo $with_vorbis_dir | grep ^/ > /dev/null 2>&1 \
   || with_vorbis_dir=$PWD/$with_vorbis_dir
   ocamlcflags="$ocamlcflags -I $with_vorbis_dir"
   ocamllflags="$ocamllflags vorbis.$cma"
fi
AC_MSG_RESULT(ok)

#
# Mad
#

AC_ARG_WITH([mad-dir],
   AC_HELP_STRING(
      [--with-mad-dir=path],
      [look for ocaml-mad library in "path" (autodetected by default)]))

AC_MSG_CHECKING(for ocaml-mad)
if test -z "$with_mad_dir" ; then
    if ! $OCAMLFIND query mad > /dev/null 2>&1 ; then
	AC_MSG_RESULT(not found)
    else
	W_MP3=yes
	AC_MSG_RESULT(ok)
    fi
else
    echo $with_mad_dir | grep ^/ > /dev/null 2>&1 \
	|| with_mad_dir=$PWD/$with_mad_dir
    ocamlcflags="$ocamlcflags -I $with_mad_dir"
    ocamllflags="$ocamllflags mad.$cma"
    W_MP3=yes
    AC_MSG_RESULT(ok)
fi

#
# Mysql
#

AC_ARG_WITH([mysql-dir],
   AC_HELP_STRING(
      [--with-mysql-dir=path],
      [look for ocaml-mysql library in "path" (autodetected by default)]))
AC_ARG_WITH([mysql],
   AC_HELP_STRING(
      [--with-mysql],
      [enables the use of mysql for strider database access (experimental)]))

if test \! -z "$with_mysql" ; then
AC_MSG_CHECKING(for ocaml-mysql)
if test "$with_mysql" = "yes" ; then
    if ! $OCAMLFIND query mysql > /dev/null 2>&1 ; then
	AC_MSG_RESULT(not found)
    else
	W_MYSQL=yes
	AC_MSG_RESULT(ok)
    fi
else
    echo $with_mysql | grep ^/ > /dev/null 2>&1 \
	|| with_mysql_dir=$PWD/$with_mysql_dir
    ocamlcflags="$ocamlcflags -I $with_mysql_dir"
    ocamllflags="$ocamllflags mysql.$cma"
    W_MYSQL=yes
    AC_MSG_RESULT(ok)
fi
fi

AC_SUBST(W_MYSQL)
if test "a$W_MYSQL" = "ayes" ; then
        w_MYSQL=yes
else
        w_MYSQL=no
fi

#
# Fetch
#

AC_ARG_WITH([fetch-dir],
   AC_HELP_STRING(
      [--with-fetch-dir=path],
      [look for ocaml-fetch library in "path" (autodetected by default)]))

AC_ARG_WITH([ftp-dir],
   AC_HELP_STRING(
      [--with-ftp-dir=path],
      [look for ocaml-ftp library in "path" (autodetected by default)]))

AC_ARG_WITH([smbclient-dir],
   AC_HELP_STRING(
      [--with-smbclient-dir=path],
      [look for ocaml-smbclient library in "path" (autodetected by default)]))

AC_ARG_WITH([fetch],
   AC_HELP_STRING(
      [--without-fetch],
      [disables the use of any module that depends on fetch]))

if test \! "$with_fetch" = "no" ; then

if test -n "$with_ftp_dir" ; then
   echo $with_ftp_dir | grep ^/ > /dev/null 2>&1 \
   || with_ftp_dir=$PWD/$with_ftp_dir
   ocamlcflags="$ocamlcflags -I $with_ftp_dir"
   ocamllflags="$ocamllflags ftp.$cma"
fi

if test -n "$with_smbclient_dir" ; then
   echo $with_smbclient_dir | grep ^/ > /dev/null 2>&1 \
   || with_smbclient_dir=$PWD/$with_smbclient_dir
   ocamlcflags="$ocamlcflags -I $with_smbclient_dir"
   ocamllflags="$ocamllflags smbclient.$cma"
fi

AC_MSG_CHECKING(for ocaml-fetch)
if test -z "$with_fetch_dir" ; then
   if ! $OCAMLFIND query fetch > /dev/null 2>&1 ; then
    AC_MSG_RESULT(no)
   else
    W_FETCH=yes
    AC_MSG_RESULT(ok)
   fi
else
   echo $with_fetch_dir | grep ^/ > /dev/null 2>&1 \
   || with_fetch_dir=$PWD/$with_fetch_dir
   ocamlcflags="$ocamlcflags -I $with_fetch_dir"
   ocamllflags="$ocamllflags fetch.$cma"
   W_FETCH=yes
   AC_MSG_RESULT(ok)
fi

fi

AC_SUBST(W_FETCH)

if test -z "$W_FETCH" ; then
    w_FETCH=no
else
    w_FETCH=yes
fi

#
# Mp3Id3
#

AC_ARG_WITH([mp3id3-dir],
   AC_HELP_STRING(
      [--with-mp3id3-dir=path],
      [look for ocaml-mp3id3 library in "path" (autodetected by default)]))

if test "z$W_MP3" = "zyes" ; then
    AC_MSG_CHECKING(for ocaml-mp3id3)
    if test -z "$with_mp3id3_dir" ; then
	if ! $OCAMLFIND query mp3id3 > /dev/null 2>&1 ; then
	    W_MP3=
	    AC_MSG_RESULT(not found)
	else
	    AC_MSG_RESULT(ok)
	fi
    else
	echo $with_mp3id3_dir | grep ^/ > /dev/null 2>&1 \
	    || with_mp3id3_dir=$PWD/$with_mp3id3_dir
	ocamlcflags="$ocamlcflags -I $with_mp3id3_dir"
	ocamllflags="$ocamllflags mp3id3.$cma"
	AC_MSG_RESULT(ok)
    fi
fi

AC_SUBST(W_MP3)

if test -z "$W_MP3" ; then
    w_MP3=no
else
    w_MP3=yes
fi

#
# Savonet and Strider
#

AC_ARG_WITH([savonet-dir],
   AC_HELP_STRING(
      [--with-savonet-dir=path],
      [look for libsavonet library in "path" (autodetected by default)]))
AC_ARG_WITH([savonet],
   AC_HELP_STRING(
      [--with-savonet],
      [enables the use of modules that depend on savonet]))

AC_ARG_WITH([strider-dir],
   AC_HELP_STRING(
      [--with-strider-dir=path],
      [look for libstrider library in "path" (autodetected by default)]))
AC_ARG_WITH([strider],
   AC_HELP_STRING(
      [--without-strider],
      [disables the use of any module that depends on strider]))

if test "$with_savonet" = "yes" ; then

 AC_MSG_CHECKING(for libsavonet)
 if test -z "$with_savonet_dir" ; then
   if ! $OCAMLFIND query savonet > /dev/null 2>&1 ; then
    AC_MSG_RESULT(no)
   else
    AC_MSG_RESULT(ok)
    W_SAVONET=yes
   fi
 else
   AC_MSG_RESULT(ok)
   echo $with_savonet_dir | grep ^/ > /dev/null 2>&1 \
   || with_savonet_dir=$PWD/$with_savonet_dir
   ocamlcflags="$ocamlcflags -I $with_savonet_dir"
   ocamllflags="$ocamllflags savonet.$cma"
 fi

 if test \! "$with_strider" = "no" ; then

  AC_MSG_CHECKING(for libstrider)
  if test -z "$with_strider_dir" ; then
   if ! $OCAMLFIND query strider > /dev/null 2>&1 ; then
    AC_MSG_RESULT(no)
   else
    AC_MSG_RESULT(ok)
    W_STRIDER=yes
   fi
  else
   AC_MSG_RESULT(ok)
   echo $with_strider_dir | grep ^/ > /dev/null 2>&1 \
   || with_strider_dir=$PWD/$with_strider_dir
   ocamlcflags="$ocamlcflags -I $with_strider_dir"
   ocamllflags="$ocamllflags strider.$cma"
  fi

 fi

fi

AC_SUBST(W_STRIDER)
AC_SUBST(W_SAVONET)

if test -z "$W_STRIDER" ; then
    w_STRIDER=no
else
    w_STRIDER=yes
fi

if test -z "$W_SAVONET" ; then
    w_SAVONET=no
else
    w_SAVONET=yes
fi

#
# Tuning (See INSTALL to understand these features)
#
###############################################################################

#
# Requests
#

requests_table_size=50
requests_max_id=10000

cat >> src/configure.ml <<EOCONF
let requests_max_id = $requests_max_id
let requests_table_size = $requests_table_size
EOCONF

#
# Speech
#

AC_PATH_PROG(CP,cp,false)
AC_PATH_PROG(RM,rm,false)

if test "$RM" = false ; then
  AC_MSG_ERROR(Cannot find rm)
fi
if test "$CP" = false ; then
  AC_MSG_ERROR(Cannot find cp)
fi

AC_PATH_PROG(TEXT2WAVE,text2wave,false)
AC_PATH_PROG(SOX,sox,false)
AC_PATH_PROG(NORMALIZE,normalize,false)

if test "$TEXT2WAVE" = false ; then
   w_TTS="liquidtts will use \"false\" as a synthetizer,
                  but you can edit it later."
else
   w_TTS="yes, using $TEXT2WAVE"
fi

if test "$SOX" = false ; then
   SOX=${CP}' $2.tmp.wav $2'
else
   SOX=${SOX}' $2.tmp.wav -w -c 2 -r 44100 $2'
fi

#
# All done.
#
###############################################################################

AC_SUBST(OCAMLC)
AC_SUBST(OCAMLDEP)
AC_SUBST(OCAMLLEX)
AC_SUBST(OCAMLYACC)
AC_SUBST(OCAMLFIND)
AC_SUBST(_DEP_OPTS)
AC_SUBST(ocamlcflags)
AC_SUBST(ocamllflags)
AC_SUBST(cma)
AC_SUBST(cmo)
AC_SUBST(cmi)

echo

AC_CONFIG_FILES([Makefile.defs],[chmod a-w Makefile.defs])
AC_CONFIG_FILES([src/liquidtts],[chmod 555 src/liquidtts])
AC_CONFIG_FILES([examples/service-liquidsoap],[chmod a-w examples/service-liquidsoap])
AC_CONFIG_FILES([examples/service-soaptube],[chmod a-w examples/service-soaptube])
AC_CONFIG_FILES([examples/gentoo-liquidsoap],[chmod a-w examples/gentoo-liquidsoap])
AC_CONFIG_FILES([examples/gentoo-soaptube],[chmod a-w examples/gentoo-soaptube])
AC_OUTPUT
make finish-configure

cat <<EOMSG

 ----------------------------------------------------------------- Compilation

 Compiler       : $OCAMLC
 OCAML_CFLAGS   : $ocamlcflags
 OCAML_LFLAGS   : $ocamllflags

 ---------------------------------------------------------------- Installation

 prefix         : $prefix
 exec_prefix    : $exec_prefix
 bindir         : $bindir
 configuration  : $sysconfdir/liquidsoap
 logging        : $localstatedir/log
 PIDs           : $localstatedir/run
 man page       : $mandir/man1
 doc            : $datadir/doc

 -------------------------------------------------------------------- Features

 Text to speech : $w_TTS
 MP3            : $w_MP3
 libSavonet     : $w_SAVONET (experimental)
 Strider        : $w_MYSQL (experimental)
 DirectStrider  : $w_STRIDER (experimental)
 OCaml-fetch    : $w_FETCH
 RTP output     : $w_RTP

 -----------------------------------------------------------------------------

 The Savonet Team thanks you for using liquidsoap ...
 Have fun !

EOMSG
