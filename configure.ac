
AC_INIT([liquidsoap],[0.3.1])
AC_PREREQ(2.57)
AC_CONFIG_SRCDIR([src/main.ml])
AC_PROG_INSTALL

echo > src/configure.ml

# This is ugly, I'd like to find a better solution.
if test "x$prefix" = "xNONE" ; then
  prefix=/usr/local
fi

# User/group for installed files and directories

AC_ARG_WITH([user],
            AC_HELP_STRING([--with-user],[sets the owner of installed files]))
AC_ARG_WITH([group],
            AC_HELP_STRING([--with-group],[sets the group of installed files]))
if test -z "$with_user" ; then
  install_user=liquidsoap
else
  install_user=$with_user
fi
if test -z "$with_group" ; then
  install_group=$install_user
else
  install_group=$with_group
fi
AC_SUBST(install_user)
AC_SUBST(install_group)

# oRTP

AC_ARG_WITH([ortp],
   AC_HELP_STRING(
      [--without-ortp],
      [forbids ortp use]))
AC_ARG_WITH([ortp-include],
   AC_HELP_STRING(
      [--with-ortp-include=path],
      [look for ortp headers in path (defaults to /usr/include/ortp)]))

AC_PROG_CC

if test -z `which pkg-config 2>/dev/null` ; then
  AC_MSG_ERROR(Cannot find pkg-config)
fi

AC_MSG_CHECKING(for glib-2.0)
if pkg-config --exists glib-2.0 ; then
  AC_MSG_RESULT(ok)

  AC_MSG_CHECKING(for libortp)

  if test -z "$with_ortp_include" ; then
    ORTP_INCLUDE=/usr/include/ortp
  else
    ORTP_INCLUDE=$with_ortp_include
  fi

  if test "$with_ortp" = "no" -o \! -f ${ORTP_INCLUDE}/ortp.h ; then
    AC_MSG_RESULT(Header not found: ${ORTP_INCLUDE}/ortp.h)
    W_RTP=""
    w_RTP=no
  else
    W_RTP=yes
    w_RTP=yes
    AC_MSG_RESULT(ok)
  fi
else
  W_RTP=""
  w_RTP=no
  AC_MSG_RESULT(no)
fi

# oRTP

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
    BYTE=
    cma=cmxa
    cmo=cmx
    cmi=cmi
    ocamlcflags=""
    ocamllflags="-linkpkg"
    OCAMLC="$OCAMLFIND ocamlopt"
    _DEP_OPTS="-native"
    cat >> src/configure.ml <<__BLAH__
let dynliq_option = [[]]
__BLAH__
else
    BYTE=yes
    cma=cma
    cmo=cmo
    cmi=cmi
    ocamlcflags=""
    ocamllflags="-linkpkg"
    OCAMLC="$OCAMLFIND ocamlc"
    _DEP_OPTS=""
    cat >> src/configure.ml <<__BLAH__
let dynliq_option = [[["-m";"--load-module"],
                     Arg.String Dynliq.load,
                     "Load a bytecode plugin."]]
__BLAH__
fi

AC_SUBST(BYTE)

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

AC_MSG_CHECKING(for ocaml-dtools)
if test -z "$with_dtools_dir" ; then
   if ! $OCAMLFIND query dtools > /dev/null 2>&1; then
       AC_MSG_ERROR(Not found.)
   fi
   ocamlcflags="$ocamlcflags -package dtools"
else
   echo $with_dtools_dir | grep ^/ > /dev/null 2>&1 \
   || with_dtools_dir=$PWD/$with_dtools_dir
   ocamlcflags="$ocamlcflags -I $with_dtools_dir"
   ocamllflags="$ocamllflags unix.$cma threads.$cma dtools.$cma"
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
   ocamlcflags="$ocamlcflags -package shout"
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
   ocamlcflags="$ocamlcflags -package vorbis"
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
        ocamlcflags="$ocamlcflags -package mad"
    fi
else
    echo $with_mad_dir | grep ^/ > /dev/null 2>&1 \
	|| with_mad_dir=$PWD/$with_mad_dir
    ocamlcflags="$ocamlcflags -I $with_mad_dir"
    ocamllflags="$ocamllflags mad.$cma"
    W_MP3=yes
    AC_MSG_RESULT(ok)
fi

AC_SUBST(W_MP3)

if test -z "$W_MP3" ; then
    w_MP3=no
else
    w_MP3=yes
fi

#
# Lame
#

AC_ARG_WITH([lame-dir],
   AC_HELP_STRING(
      [--with-lame-dir=path],
      [look for ocaml-lame library in "path" (autodetected by default)]))

AC_MSG_CHECKING(for ocaml-lame)
if test -z "$with_lame_dir" ; then
    if ! $OCAMLFIND query lame > /dev/null 2>&1 ; then
	AC_MSG_RESULT(not found)
    else
	W_LAME=yes
	AC_MSG_RESULT(ok)
        ocamlcflags="$ocamlcflags -package lame"
    fi
else
    echo $with_lame_dir | grep ^/ > /dev/null 2>&1 \
	|| with_lame_dir=$PWD/$with_lame_dir
    ocamlcflags="$ocamlcflags -I $with_lame_dir"
    ocamllflags="$ocamllflags lame.$cma"
    W_LAME=yes
    AC_MSG_RESULT(ok)
fi

AC_SUBST(W_LAME)
if test "a$W_LAME" = "ayes" ; then
        w_LAME=yes
else
        w_LAME=no
fi

#
# Alsa
#

AC_ARG_WITH([alsa-dir],
   AC_HELP_STRING(
      [--with-alsa-dir=path],
      [look for ocaml-alsa library in "path" (autodetected by default)]))

AC_MSG_CHECKING(for ocaml-alsa)
if test -z "$with_alsa_dir" ; then
    if ! $OCAMLFIND query alsa > /dev/null 2>&1 ; then
	AC_MSG_RESULT(not found)
    else
	W_ALSA=yes
	AC_MSG_RESULT(ok)
	ocamlcflags="$ocamlcflags -package alsa"
    fi
else
    echo $with_alsa_dir | grep ^/ > /dev/null 2>&1 \
	|| with_alsa_dir=$PWD/$with_alsa_dir
    ocamlcflags="$ocamlcflags -I $with_alsa_dir"
    ocamllflags="$ocamllflags alsa.$cma"
    W_ALSA=yes
    AC_MSG_RESULT(ok)
fi

AC_SUBST(W_ALSA)
if test "a$W_ALSA" = "ayes" ; then
        w_ALSA=yes
else
        w_ALSA=no
fi

#
# Mp3Id3
#

AC_ARG_WITH([mp3id3-dir],
   AC_HELP_STRING(
      [--with-mp3id3-dir=path],
      [look for ocaml-mp3id3 library in "path" (autodetected by default)]))

# Check for ocaml-mp3id3 only if ocaml-mad is available for decoding MP3
if test "z$W_MP3" = "zyes" ; then
    AC_MSG_CHECKING(for ocaml-mp3id3)
    if test -z "$with_mp3id3_dir" ; then
	if ! $OCAMLFIND query mp3id3 > /dev/null 2>&1 ; then
	    AC_MSG_RESULT(not found)
	else
	    AC_MSG_RESULT(ok)
	    ocamlcflags="$ocamlcflags -package mp3id3"
            w_ID3READER="ocaml-mp3id3 "
            W_MP3ID3=yes
	fi
    else
	echo $with_mp3id3_dir | grep ^/ > /dev/null 2>&1 \
	    || with_mp3id3_dir=$PWD/$with_mp3id3_dir
	ocamlcflags="$ocamlcflags -I $with_mp3id3_dir"
	ocamllflags="$ocamllflags mp3id3.$cma"
        w_ID3READER="ocaml-mp3id3 "
	AC_MSG_RESULT(ok)
        W_MP3ID3=yes
    fi
fi

AC_SUBST(W_MP3ID3)

#
# ID3Tag
#

# Check for id3tag only if ocaml-mad is available for decoding MP3
if test "z$W_MP3" = "zyes" ; then
  LIBS="-lid3tag $LIBS"
  AC_CHECK_HEADERS([id3tag.h],
                   [AC_CHECK_FUNC([id3_tag_new], [W_ID3TAG="yes"])])
  if test "$W_ID3TAG" = "yes" ; then
    w_ID3READER="${w_ID3READER}id3tag"
  fi
fi

AC_SUBST(W_ID3TAG)

#
# Camomile
#

AC_ARG_WITH([camomile-dir],
   AC_HELP_STRING(
      [--with-camomile-dir=path],
      [look for ocaml-camomile library in "path" (autodetected by default)]))

AC_MSG_CHECKING(for camomile)
if test -z "$with_camomile_dir" ; then
    if ! $OCAMLFIND query camomile > /dev/null 2>&1 ; then
        W_CAMOMILE=
        AC_MSG_RESULT(not found)
    else
        W_CAMOMILE=yes
        AC_MSG_RESULT(ok)
        ocamlcflags="$ocamlcflags -package camomile"
        if $OCAMLFIND query camomile -l | grep '^version:[[ \\t]]*0\.7\.0' ; then
          AC_MSG_ERROR(libcamomile >=0.7.1 is required)
        fi
        if $OCAMLFIND query camomile -l | grep '^version:[[ \\t]]*0\.[[0-6]]\\.' ; then
          AC_MSG_ERROR(libcamomile >=0.7.1 is required)
        fi
    fi
else
    echo $with_camomile_dir | grep ^/ > /dev/null 2>&1 \
        || with_camomile_dir=$PWD/$with_camomile_dir
    ocamlcflags="$ocamlcflags -I $with_camomile_dir"
    ocamllflags="$ocamllflags camomile.$cma"
    W_CAMOMILE=yes
    AC_MSG_RESULT(ok)
fi

AC_SUBST(W_CAMOMILE)

if test -z "$W_CAMOMILE" ; then
    w_CAMOMILE="no (requires camomile)"
    echo let recode_tag s = s >> src/configure.ml
else
    w_CAMOMILE=yes
    cat >> src/configure.ml <<EOCONF
module C = CamomileLibrary.Default.Camomile.CharEncoding
let enc_of_name s =
  try C.of_name s with
    | Not_found -> failwith (Printf.sprintf "Unknown encoding %s !" s)
let recode_tag s =
  let encs =
    (* List of encodings to try for automatic encoding detection. *)
    Dtools.Conf.get_list ~default:[["ISO-8859-1";"UTF-8"]] "tag.encodings"
  in
  let in_enc =
    C.automatic "LIQ-TAGS" (List.map enc_of_name encs) C.utf8
  in
    C.recode_string ~in_enc ~out_enc:C.utf8 s
EOCONF
fi


#
# Tuning (See INSTALL to understand these features)
#
###############################################################################

#
# Requests
#

requests_table_size=50
requests_max_id=50

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
# GUI
#

AC_PATH_PROG(PYTHON,python,no)
if test \! "$PYTHON" = no ; then
  PYVERSION=`$PYTHON -c "import sys ; import re ; import pygtk ; print re.search('^\d+\.\d+',sys.version).group(0)"`
  if test $? = 0 ; then
    PYMDIR="\${prefix}/lib/python$PYVERSION/site-packages/liquidsoap"
    W_PYTHON=yes
    w_PYTHON="yes (requires >=pyGTK-2.0)"
    cat > gui/liguidsoap <<__EOP__
#!/usr/bin/env python
import sys
sys.path.insert(0,"${prefix}/lib/python$PYVERSION/site-packages/liquidsoap")
import liguidsoap
liguidsoap.liguidsoap()
__EOP__
    chmod +x gui/liguidsoap
  else
    w_PYTHON=no
  fi
else
  w_PYTHON=no
fi
AC_SUBST(W_PYTHON)
AC_SUBST(PYMDIR)

#
# Wget and ufetch
#

AC_PATH_PROG(WGET,wget,no)
AC_PATH_PROG(UFETCH,ufetch,no)

echo let extproto = [[ >> src/configure.ml

if test "$WGET" != no ; then
  cat >> src/configure.ml <<EOCONF
  "$WGET",
  [ "http";"https";"ftp" ],
  (fun src dst ->
     [|"$WGET";"-nv";src;"-O";dst|]) ;
EOCONF
fi

if test "$UFETCH" != no ; then
  cat >> src/configure.ml <<EOCONF
  "$UFETCH",
  [ `$UFETCH --supported-protocols` ],
  (fun src dst ->
     [|"$UFETCH";"--quiet";src;"file://"^dst|]) ;
EOCONF
fi

echo ]] >> src/configure.ml

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
AC_CONFIG_FILES([scripts/liquidsoap.initd],[chmod a-w scripts/liquidsoap.initd])
AC_CONFIG_FILES([scripts/liquidsoap.logrotate],
                [chmod a-w scripts/liquidsoap.logrotate])
AC_CONFIG_FILES([scripts/liquidsoap.gentoo.initd],
                [chmod a-w scripts/liquidsoap.gentoo.initd])
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
 binary         : $bindir
 configuration  : $sysconfdir/liquidsoap
 logging        : $localstatedir/log/liquidsoap
 PIDs           : $localstatedir/run/liquidsoap
 logrotate conf : $sysconfdir/logrotate.d
 man page       : $mandir/man1
 doc            : $datadir/doc

 -------------------------------------------------------------------- Features

 Mandatory support for shout and vorbis.
 Optional features:
 MP3            : $w_MP3
 ID3 tag access : $w_ID3READER
 MP3 encoding   : $w_LAME
 Text to speech : $w_TTS
 Tag recoding   : $w_CAMOMILE
 RTP support    : $w_RTP
 ALSA I/O       : $w_ALSA
 Python GUI     : $w_PYTHON

 -----------------------------------------------------------------------------

 The Savonet Team thanks you for using liquidsoap ...
 Have fun !

EOMSG
