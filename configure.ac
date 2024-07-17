
dnl BEFORE RELEASING
dnl Make sure that you edit the version number on the next line
dnl as well as the "version" variable below AND the "libs_dir_version",
dnl which is used as the final component of the standard .liq libdir,
dnl $libdir/liquidsoap/$libs_dir_version

AC_INIT([liquidsoap],[0.3.5])
AC_PREREQ(2.57)
AC_CONFIG_SRCDIR([src/main.ml])
AC_PROG_INSTALL

version="0.3.5"
libs_dir_version="0.3.5"
AC_SUBST(version)
AC_SUBST(libs_dir_version)
cat > src/configure.ml << _BLAH_
let version = "$version"
let conf = Dtools.Conf.void "Liquidsoap configuration"
let var_script = ref "default"
let substs = ref
    [[
      "<script>", (fun () -> !var_script);
      "<pid>", (fun () -> string_of_int (Unix.getpid ()));
      "<home>", (fun () -> Sys.getenv "HOME");
    ]]
let add_subst r s = substs := (r, fun () -> s) :: !substs
let subst_vars s =
  List.fold_left
    (fun v (r, s) -> Str.global_replace (Str.regexp r) (s ()) v)
     s !substs
let env_has key = try ignore (Sys.getenv key) ; true with Not_found -> false
_BLAH_

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

AC_PROG_CC

if test -z `which pkg-config 2>/dev/null` ; then
  AC_MSG_ERROR(Cannot find pkg-config)
fi

# oRTP

AC_ARG_WITH([ortp-include],
   AC_HELP_STRING(
      [--with-ortp-include=path],
      [look for ortp headers in path (defaults to /usr/include/ortp)]))
AC_ARG_ENABLE([rtp],AC_HELP_STRING([--disable-rtp],[don't use RTP]))

W_RTP=""
w_RTP=no
if test "x$enable_rtp" != "xno" ; then
AC_MSG_CHECKING(for glib-2.0 required by libortp)
if pkg-config --exists glib-2.0 ; then
  AC_MSG_RESULT(ok)

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
else
  AC_MSG_RESULT(no)
fi
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

AC_ARG_ENABLE([profiling],
   AC_HELP_STRING(
      [--enable-profiling],
      [compile to generate profiling infomation]))

AC_ARG_ENABLE([debugging],
   AC_HELP_STRING(
      [--enable-debugging],
      [compile with debugging information (backtrace printing in particular)]))

AC_ARG_ENABLE([nativecode],
   AC_HELP_STRING(
      [--disable-nativecode],
      [compile in bytecode]))

AC_PATH_PROG(OCAMLOPT,ocamlopt,no)
if test "$OCAMLOPT" = no ; then
    enable_nativecode=no
fi

# Add -fPIC to CFLAGS for compatibility with x86_64
CFLAGS="$CFLAGS -fPIC"
ocamlcflags="-ccopt \"\$(CFLAGS)\""
ocamllflags="-linkpkg -package unix -package threads -package str"
requires="unix threads str"
if test "$enable_nativecode" \!= "no" ; then
    BYTE=
    cma=cmxa
    cmo=cmx
    cmi=cmi
    ocamlcflags="$ocamlcflags"
    if test "x$enable_profiling" = "xyes" ; then
        ocamlcflags="-p $ocamlcflags"
    fi
    if test "x$enable_debugging" = "xyes" ; then
        ocamlcflags="-g $ocamlcflags"
    fi
    AC_MSG_CHECKING(whether ocamlopt accepts -ffast-math)
    if [ `$OCAMLFIND ocamlopt -ffast-math 2>/dev/null` ]; then
        AC_MSG_RESULT(yes)
        ocamlcflags="-ffast-math $ocamlcflags"
    else
        AC_MSG_RESULT(no)
    fi
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
    if test "x$enable_debugging" = "xyes" ; then
        CFLAGS="$CFLAGS -g"
        ocamlcflags="$ocamlcflags -g"
    fi
    if test "x$enable_profiling" = "xyes" ; then
        OCAMLC="$OCAMLFIND ocamlcp"
    else
        OCAMLC="$OCAMLFIND ocamlc"
    fi
    _DEP_OPTS=""
    cat >> src/configure.ml <<__BLAH__
let dynliq_option = [[["-m";"--load-module"],
                     Arg.String Dynliq.load,
                     "Load a bytecode plugin."]]
__BLAH__
fi

AC_SUBST(BYTE)

AC_ARG_ENABLE([graphics],
   AC_HELP_STRING(
      [--enable-graphics],
      [enable plugins which need the Graphics library]))

if test "x$enable_graphics" = "xyes"; then
    AC_MSG_CHECKING(for Graphics)
    cat >> configure_graphics.ml <<__BLAH__
open Graphics
__BLAH__
    if ocamlfind ocamlc -package graphics configure_graphics.ml -o configure_graphics > /dev/null 2>&1 ; then
        W_GRAPHICS=yes
        AC_MSG_RESULT(ok)
        ocamlcflags="$ocamlcflags -package graphics"
        requires="$requires graphics"
    else
        W_GRAPHICS=
        AC_MSG_RESULT(not found)
    fi
    rm -f configure_graphics
    rm -f configure_graphics.ml
    rm -f configure_graphics.cmi
    rm -f configure_graphics.cmo
fi

if test "x$W_GRAPHICS" = "xyes"; then
    w_GRAPHICS=yes
else
    w_GRAPHICS=no
fi

AC_SUBST(W_GRAPHICS)

#
# Libs
#
###############################################################################

#
# PCRE
#

AC_MSG_CHECKING(for pcre-ocaml)
if ! $OCAMLFIND query pcre > /dev/null 2>&1; then
  AC_MSG_ERROR(Not found.)
fi
ocamlcflags="$ocamlcflags -package pcre"
requires="$requires pcre"
AC_MSG_RESULT(ok)

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
   requires="$requires dtools"
else
   echo $with_dtools_dir | grep ^/ > /dev/null 2>&1 \
   || with_dtools_dir=$PWD/$with_dtools_dir
   ocamlcflags="$ocamlcflags -I $with_dtools_dir"
   ocamllflags="$ocamllflags dtools.$cma"
   requires="$requires dtools"
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
       AC_MSG_RESULT(Not found.)
   else
       ocamlcflags="$ocamlcflags -package shout"
       requires="$requires shout"
       W_SHOUT=yes
       AC_MSG_RESULT(ok)
   fi
else
   echo $with_shout_dir | grep ^/ > /dev/null 2>&1 \
   || with_shout_dir=$PWD/$with_shout_dir
   ocamlcflags="$ocamlcflags -I $with_shout_dir"
   ocamllflags="$ocamllflags shout.$cma"
   requires="$requires shout"
   W_SHOUT=yes
   AC_MSG_RESULT(ok)
fi

AC_SUBST(W_SHOUT)
if test -z "$W_SHOUT" ; then
    w_SHOUT=no
else
    w_SHOUT=yes
fi

#
# xml-light
#

AC_MSG_CHECKING(for ocaml-xml-light)
   if ! $OCAMLFIND query xml-light > /dev/null 2>&1 ; then
       AC_MSG_RESULT(Not found.)
   else
   ocamlcflags="$ocamlcflags -package xml-light"
   requires="$requires xml-light"
   W_XML=yes
   AC_MSG_RESULT(ok)
fi
AC_SUBST(W_XML)
if test -z "$W_XML" ; then
    w_XML=no
else
    w_XML=yes
fi


#
# Ogg
#

AC_ARG_WITH([ogg-dir],
   AC_HELP_STRING(
      [--with-ogg-dir=path],
      [look for ocaml-ogg library in "path" (autodetected by default)]))

AC_MSG_CHECKING(for ocaml-ogg)
if test -z "$with_ogg_dir" ; then
   if ! $OCAMLFIND query ogg > /dev/null 2>&1 ; then
       AC_MSG_RESULT(Not found.)
   else
       ocamlcflags="$ocamlcflags -package ogg"
       requires="$requires ogg"
       W_OGG=yes
       AC_MSG_RESULT(ok)
   fi
else
   echo $with_ogg_dir | grep ^/ > /dev/null 2>&1 \
   || with_ogg_dir=$PWD/$with_ogg_dir
   ocamlcflags="$ocamlcflags -I $with_ogg_dir"
   ocamllflags="$ocamllflags ogg.$cma"
   requires="$requires ogg"
   W_OGG=yes
   AC_MSG_RESULT(ok)
fi

AC_SUBST(W_OGG)
if test -z "$W_OGG" ; then
    w_OGG=no
else
    w_OGG=yes
fi

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
       AC_MSG_RESULT(Not found.)
   else
       ocamlcflags="$ocamlcflags -package vorbis"
       requires="$requires vorbis"
       if test "$w_OGG" = no ; then
           AC_MSG_ERROR(Ocaml-vorbis needs ocaml-ogg.)
       fi
       W_VORBIS=yes
       AC_MSG_RESULT(ok)
   fi
else
   echo $with_vorbis_dir | grep ^/ > /dev/null 2>&1 \
   || with_vorbis_dir=$PWD/$with_vorbis_dir
   ocamlcflags="$ocamlcflags -I $with_vorbis_dir"
   ocamllflags="$ocamllflags vorbis.$cma"
   requires="$requires vorbis"
   if test "$w_OGG" = no ; then
       AC_MSG_ERROR(ocaml-vorbis needs ocaml-ogg.)
   fi
   W_VORBIS=yes
   AC_MSG_RESULT(ok)
fi

AC_SUBST(W_VORBIS)
if test -z "$W_VORBIS" ; then
    w_VORBIS=no
else
    w_VORBIS=yes
fi


#
# Mad
#

AC_ARG_WITH([mad-dir],
   AC_HELP_STRING(
      [--with-mad-dir=path],
      [look for ocaml-mad library in "path" (autodetected by default)]))
AC_ARG_ENABLE([mad],AC_HELP_STRING([--disable-mad],[don't use ocaml-mad]))

if test "x$enable_mad" != "xno" ; then
AC_MSG_CHECKING(for ocaml-mad)
if test -z "$with_mad_dir" ; then
    if ! $OCAMLFIND query mad > /dev/null 2>&1 ; then
	AC_MSG_RESULT(not found)
    else
	W_MP3=yes
	AC_MSG_RESULT(ok)
        ocamlcflags="$ocamlcflags -package mad"
        requires="$requires mad"
    fi
else
    echo $with_mad_dir | grep ^/ > /dev/null 2>&1 \
	|| with_mad_dir=$PWD/$with_mad_dir
    ocamlcflags="$ocamlcflags -I $with_mad_dir"
    ocamllflags="$ocamllflags mad.$cma"
    requires="$requires mad"
    W_MP3=yes
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
# Lame
#

AC_ARG_WITH([lame-dir],
   AC_HELP_STRING(
      [--with-lame-dir=path],
      [look for ocaml-lame library in "path" (autodetected by default)]))
AC_ARG_ENABLE([lame],AC_HELP_STRING([--disable-lame],[don't use ocaml-lame]))

if test "x$enable_lame" != "xno" ; then
AC_MSG_CHECKING(for ocaml-lame)
if test -z "$with_lame_dir" ; then
    if ! $OCAMLFIND query lame > /dev/null 2>&1 ; then
	AC_MSG_RESULT(not found)
    else
	W_LAME=yes
	AC_MSG_RESULT(ok)
        ocamlcflags="$ocamlcflags -package lame"
        requires="$requires lame"
    fi
else
    echo $with_lame_dir | grep ^/ > /dev/null 2>&1 \
	|| with_lame_dir=$PWD/$with_lame_dir
    ocamlcflags="$ocamlcflags -I $with_lame_dir"
    ocamllflags="$ocamllflags lame.$cma"
    requires="$requires lame"
    W_LAME=yes
    AC_MSG_RESULT(ok)
fi
fi

AC_SUBST(W_LAME)
if test "a$W_LAME" = "ayes" ; then
        w_LAME=yes
else
        w_LAME=no
fi

#
# Jack
#

AC_ARG_WITH([jack-dir],
   AC_HELP_STRING(
      [--with-jack-dir=path],
      [look for ocaml-jack library in "path" (autodetected by default)]))
AC_ARG_ENABLE([jack],AC_HELP_STRING([--disable-jack],[don't use ocaml-jack]))

if test "x$enable_jack" != "xno" ; then
AC_MSG_CHECKING(for ocaml-jack)
if test -z "$with_jack_dir" ; then
    if ! $OCAMLFIND query jack > /dev/null 2>&1 ; then
	AC_MSG_RESULT(not found)
    else
	W_JACK=yes
	AC_MSG_RESULT(ok)
	ocamlcflags="$ocamlcflags -package jack"
        requires="$requires jack"
    fi
else
    echo $with_jack_dir | grep ^/ > /dev/null 2>&1 \
	|| with_jack_dir=$PWD/$with_jack_dir
    ocamlcflags="$ocamlcflags -I $with_jack_dir"
    ocamllflags="$ocamllflags jack.$cma"
    requires="$requires jack"
    W_JACK=yes
    AC_MSG_RESULT(ok)
fi
fi

AC_SUBST(W_JACK)
if test "a$W_JACK" = "ayes" ; then
        w_JACK=yes
else
        w_JACK=no
fi

#
# Alsa
#

AC_ARG_WITH([alsa-dir],
   AC_HELP_STRING(
      [--with-alsa-dir=path],
      [look for ocaml-alsa library in "path" (autodetected by default)]))
AC_ARG_ENABLE([alsa],AC_HELP_STRING([--disable-alsa],[don't use ocaml-alsa]))

if test "x$enable_alsa" != "xno" ; then
AC_MSG_CHECKING(for ocaml-alsa)
if test -z "$with_alsa_dir" ; then
    if ! $OCAMLFIND query alsa > /dev/null 2>&1 ; then
	AC_MSG_RESULT(not found)
    else
	W_ALSA=yes
	AC_MSG_RESULT(ok)
	ocamlcflags="$ocamlcflags -package alsa"
        requires="$requires alsa"
    fi
else
    echo $with_alsa_dir | grep ^/ > /dev/null 2>&1 \
	|| with_alsa_dir=$PWD/$with_alsa_dir
    ocamlcflags="$ocamlcflags -I $with_alsa_dir"
    ocamllflags="$ocamllflags alsa.$cma"
    requires="$requires alsa"
    W_ALSA=yes
    AC_MSG_RESULT(ok)
fi
fi

AC_SUBST(W_ALSA)
if test "a$W_ALSA" = "ayes" ; then
        w_ALSA=yes
else
        w_ALSA=no
fi

#
# LibAO
#

AC_ARG_WITH([ao-dir],
   AC_HELP_STRING(
      [--with-ao-dir=path],
      [look for ocaml-ao library in "path" (autodetected by default)]))
AC_ARG_ENABLE([ao],AC_HELP_STRING([--disable-ao],[don't use ocaml-ao]))

if test "x$enable_ao" != "xno" ; then
AC_MSG_CHECKING(for ocaml-ao)
if test -z "$with_ao_dir" ; then
    if ! $OCAMLFIND query ao > /dev/null 2>&1 ; then
	AC_MSG_RESULT(not found)
    else
	W_AO=yes
	AC_MSG_RESULT(ok)
	ocamlcflags="$ocamlcflags -package ao"
        requires="$requires ao"
    fi
else
    echo $with_ao_dir | grep ^/ > /dev/null 2>&1 \
	|| with_ao_dir=$PWD/$with_ao_dir
    ocamlcflags="$ocamlcflags -I $with_ao_dir"
    ocamllflags="$ocamllflags ao.$cma"
    requires="$requires ao"
    W_AO=yes
    AC_MSG_RESULT(ok)
fi
fi

AC_SUBST(W_AO)
if test "a$W_AO" = "ayes" ; then
        w_AO=yes
else
        w_AO=no
fi

#
# Samplerate
#

AC_ARG_WITH([samplerate-dir],
   AC_HELP_STRING(
      [--with-samplerate-dir=path],
      [look for ocaml-samplerate library in "path" (autodetected by default)]))
AC_ARG_ENABLE([samplerate],AC_HELP_STRING([--disable-samplerate],[don't use ocaml-samplerate]))

if test "x$enable_samplerate" != "xno" ; then
AC_MSG_CHECKING(for ocaml-samplerate)
if test -z "$with_samplerate_dir" ; then
    if ! $OCAMLFIND query samplerate > /dev/null 2>&1 ; then
	AC_MSG_RESULT(not found)
    else
	W_SAMPLERATE=yes
	AC_MSG_RESULT(ok)
	ocamlcflags="$ocamlcflags -package samplerate"
        requires="$requires samplerate"
    fi
else
    echo $with_samplerate_dir | grep ^/ > /dev/null 2>&1 \
	|| with_samplerate_dir=$PWD/$with_samplerate_dir
    ocamlcflags="$ocamlcflags -I $with_samplerate_dir"
    ocamllflags="$ocamllflags samplerate.$cma"
    requires="$requires samplerate"
    W_SAMPLERATE=yes
    AC_MSG_RESULT(ok)
fi
fi

AC_SUBST(W_SAMPLERATE)
if test "a$W_SAMPLERATE" = "ayes" ; then
  cat >> src/configure.ml << EOCONF
let resample =
  if env_has "LIQ_DISABLE_SAMPLERATE" then
    None
  else
    Some (fun ratio buf offs len ->
            Samplerate.convert Samplerate.Conv_fastest 1 ratio buf offs len)
EOCONF
  w_SAMPLERATE=yes
else
  echo "let resample = None" >> src/configure.ml
  w_SAMPLERATE=no
fi

#
# Mp3Id3
#

AC_ARG_WITH([mp3id3-dir],
   AC_HELP_STRING(
      [--with-mp3id3-dir=path],
      [look for ocaml-mp3id3 library in "path" (autodetected by default)]))
AC_ARG_ENABLE([mp3id3],
              AC_HELP_STRING([--disable-mp3id3],[don't use ocaml-mp3id3]))

# Check for ocaml-mp3id3 only if ocaml-mad is available for decoding MP3
if test "x$enable_mp3id3" != "xno" ; then
if test "z$W_MP3" = "zyes" ; then
    AC_MSG_CHECKING(for ocaml-mp3id3)
    if test -z "$with_mp3id3_dir" ; then
	if ! $OCAMLFIND query mp3id3 > /dev/null 2>&1 ; then
	    AC_MSG_RESULT(not found)
	else
	    AC_MSG_RESULT(ok)
	    ocamlcflags="$ocamlcflags -package mp3id3"
            requires="$requires mp3id3"
            w_ID3READER="ocaml-mp3id3 "
            W_MP3ID3=yes
	fi
    else
	echo $with_mp3id3_dir | grep ^/ > /dev/null 2>&1 \
	    || with_mp3id3_dir=$PWD/$with_mp3id3_dir
	ocamlcflags="$ocamlcflags -I $with_mp3id3_dir"
	ocamllflags="$ocamllflags mp3id3.$cma"
        requires="$requires mp3id3"
        w_ID3READER="ocaml-mp3id3 "
	AC_MSG_RESULT(ok)
        W_MP3ID3=yes
    fi
fi
fi

AC_SUBST(W_MP3ID3)

#
# ID3Tag
#

AC_ARG_ENABLE([id3tag],
              AC_HELP_STRING([--disable-id3tag],[don't use libid3tag]))

if test "x$enable_id3tag" != "xno" ; then
# Check for id3tag only if ocaml-mad is available for decoding MP3
if test "z$W_MP3" = "zyes" ; then
  LIBS="-lid3tag $LIBS"
  AC_CHECK_HEADERS([id3tag.h],
                   [AC_CHECK_FUNC([id3_tag_new], [W_ID3TAG="yes"])])
  if test "$W_ID3TAG" = "yes" ; then
    w_ID3READER="${w_ID3READER}id3tag"
  fi
fi
fi

AC_SUBST(W_ID3TAG)

if test -z "$w_ID3READER" ; then
  w_ID3READER=none
fi

#
# OSS
#

AC_ARG_ENABLE([oss],AC_HELP_STRING([--disable-oss],[don't use OSS]))

if test "x$enable_oss" != "xno" ; then
  AC_CHECK_HEADERS([linux/sound.h],[W_OSS="yes"])
fi

AC_SUBST(W_OSS)
if test "a$W_OSS" = "ayes" ; then
        w_OSS=yes
else
        w_OSS=no
fi

#
# Camomile
#

AC_ARG_WITH([camomile-dir],
   AC_HELP_STRING(
      [--with-camomile-dir=path],
      [look for ocaml-camomile library in "path" (autodetected by default)]))
AC_ARG_ENABLE([camomile],
              AC_HELP_STRING([--disable-camomile],[don't use ocaml-camomile]))

if test "x$enable_camomile" != "xno" ; then
AC_MSG_CHECKING(for camomile)
if test -z "$with_camomile_dir" ; then
    if ! $OCAMLFIND query camomile > /dev/null 2>&1 ; then
        W_CAMOMILE=
        AC_MSG_RESULT(not found)
    else
        W_CAMOMILE=yes
        AC_MSG_RESULT(ok)
        ocamlcflags="$ocamlcflags -package camomile"
        requires="$requires camomile"
    fi
else
    echo $with_camomile_dir | grep ^/ > /dev/null 2>&1 \
        || with_camomile_dir=$PWD/$with_camomile_dir
    ocamlcflags="$ocamlcflags -I $with_camomile_dir"
    ocamllflags="$ocamllflags camomile.$cma"
    dnl TODO The META needs info here too...
    W_CAMOMILE=yes
    AC_MSG_RESULT(ok)
fi
fi

AC_SUBST(W_CAMOMILE)

if test -z "$W_CAMOMILE" ; then
    w_CAMOMILE="no (requires camomile)"
    echo let recode_tag ?encoding s = s >> src/configure.ml
else
    w_CAMOMILE=yes
    camdir=`$OCAMLFIND query camomile -l | \
      grep location | sed -e 's/location: \+//'`
    camencoding=Camomile.CharEncoding
    if test -f $camdir/camomileLibrary.cmi ; then
      camencoding=CamomileLibrary.Default.$camencoding
    fi
    cat >> src/configure.ml <<EOCONF
module C = $camencoding
let enc_of_name s =
  try C.of_name s with
    | Not_found -> failwith (Printf.sprintf "Unknown encoding %s !" s)

let conf_tag =
  Dtools.Conf.void ~p:(conf#plug "tag") "Settings related to metadata tags"
let conf_encoding =
  Dtools.Conf.list ~p:(conf_tag#plug "encodings") ~d:["UTF-8";"ISO-8859-1"]
    "List of encodings to try for automatic encoding detection"

let recode_tag ?encoding s =
  let encs = conf_encoding#get in
  let in_enc =
    match encoding with
      | Some e -> enc_of_name e
      | None -> C.automatic "LIQ-TAGS" (List.map enc_of_name encs) C.utf8
  in
    C.recode_string ~in_enc ~out_enc:C.utf8 s

let recode_tag =
  if env_has "LIQ_DISABLE_CAMOMILE" then
    fun ?encoding s -> s
  else
    recode_tag
EOCONF
fi

#
# LablGL
#

AC_ARG_WITH([gl-dir],
   AC_HELP_STRING(
      [--with-gl-dir=path],
      [look for lablGL library in "path" (autodetected by default)]))
AC_ARG_ENABLE([gl],AC_HELP_STRING([--disable-gl],[don't use lablGL]))

if test "x$enable_gl" != "xno" ; then
AC_MSG_CHECKING(for lablGL)
if test -z "$with_gl_dir" ; then
    if ! $OCAMLFIND query lablGL > /dev/null 2>&1 ; then
        AC_MSG_RESULT(not found)
    else
        W_GL=yes
        AC_MSG_RESULT(ok)
        ocamlcflags="$ocamlcflags -package lablGL"
        ocamllflags="$ocamllflags lablgl.$cma lablglut.$cma"
        requires="$requires lablGL"
    fi
else
    echo $with_gl_dir | grep ^/ > /dev/null 2>&1 \
        || with_gl_dir=$PWD/$with_gl_dir
    ocamlcflags="$ocamlcflags -I $with_gl_dir"
    ocamllflags="$ocamllflags lablgl.$cma lablglut.$cma"
    dnl TOTO See Camomile, something needs to be done for META here
    W_GL=yes
    AC_MSG_RESULT(ok)
fi
fi

AC_SUBST(W_GL)
if test "a$W_GL" = "ayes" ; then
        w_GL=yes
else
        w_GL=no
fi

#
# Extlib
#

AC_ARG_WITH([extlib-dir],
   AC_HELP_STRING(
      [--with-extlib-dir=path],
      [look for extlib library in "path" (autodetected by default)]))
AC_ARG_ENABLE([extlib],AC_HELP_STRING([--disable-exlib],[don't use extlib]))

if test "x$enable_extlib" != "xno" ; then
AC_MSG_CHECKING(for extlib)
if test -z "$with_extlib_dir" ; then
    if ! $OCAMLFIND query extlib > /dev/null 2>&1 ; then
        AC_MSG_RESULT(not found)
    else
        W_EXTLIB=yes
        AC_MSG_RESULT(ok)
        ocamlcflags="$ocamlcflags -package extlib"
        requires="$requires extlib"
    fi
else
    echo $with_extlib_dir | grep ^/ > /dev/null 2>&1 \
        || with_extlib_dir=$PWD/$with_extlib_dir
    ocamlcflags="$ocamlcflags -I $with_extlib_dir"
    ocamllflags="$ocamllflags extLib.$cma"
    dnl TODO same as camomile and lablGL, plus extlib is not used ;)
    W_EXTLIB=yes
    AC_MSG_RESULT(ok)
fi
fi

AC_SUBST(W_EXTLIB)
if test "a$W_EXTLIB" = "ayes" ; then
        w_EXTLIB=yes
else
        w_EXTLIB=no
fi

#
# Faad
#

AC_ARG_WITH([faad-dir],
   AC_HELP_STRING(
      [--with-faad-dir=path],
      [look for ocaml-faad library in "path" (autodetected by default)]))
AC_ARG_ENABLE([faad],AC_HELP_STRING([--disable-faad],[don't use ocaml-faad]))

if test "x$enable_faad" != "xno" ; then
AC_MSG_CHECKING(for ocaml-faad)
if test -z "$with_faad_dir" ; then
    if ! $OCAMLFIND query faad > /dev/null 2>&1 ; then
	AC_MSG_RESULT(not found)
    else
	W_FAAD=yes
	AC_MSG_RESULT(ok)
        ocamlcflags="$ocamlcflags -package faad"
        requires="$requires faad"
    fi
else
    echo $with_faad_dir | grep ^/ > /dev/null 2>&1 \
	|| with_faad_dir=$PWD/$with_faad_dir
    ocamlcflags="$ocamlcflags -I $with_faad_dir"
    ocamllflags="$ocamllflags faad.$cma"
    requires="$requires faad"
    W_FAAD=yes
    AC_MSG_RESULT(ok)
fi
fi

AC_SUBST(W_FAAD)

if test -z "$W_FAAD" ; then
    w_FAAD=no
else
    w_FAAD=yes
fi

#
# Faac
#

AC_ARG_WITH([faac-dir],
   AC_HELP_STRING(
      [--with-faac-dir=path],
      [look for ocaml-faac library in "path" (autodetected by default)]))
AC_ARG_ENABLE([faac],AC_HELP_STRING([--disable-faac],[don't use ocaml-faac]))

if test "x$enable_faac" != "xno" ; then
AC_MSG_CHECKING(for ocaml-faac)
if test -z "$with_faac_dir" ; then
    if ! $OCAMLFIND query faac > /dev/null 2>&1 ; then
	AC_MSG_RESULT(not found)
    else
	W_FAAC=yes
	AC_MSG_RESULT(ok)
        ocamlcflags="$ocamlcflags -package faac"
        requires="$requires faac"
    fi
else
    echo $with_faac_dir | grep ^/ > /dev/null 2>&1 \
	|| with_faac_dir=$PWD/$with_faac_dir
    ocamlcflags="$ocamlcflags -I $with_faac_dir"
    ocamllflags="$ocamllflags faac.$cma"
    requires="$requires faac"
    W_FAAC=yes
    AC_MSG_RESULT(ok)
fi
fi

AC_SUBST(W_FAAC)

if test -z "$W_FAAC" ; then
    w_FAAC=no
else
    w_FAAC=yes
fi

#
# SoundTouch
#

AC_ARG_WITH([soundtouch-dir],
   AC_HELP_STRING(
      [--with-soundtouch-dir=path],
      [look for ocaml-soundtouch library in "path" (autodetected by default)]))
AC_ARG_ENABLE([soundtouch],AC_HELP_STRING([--disable-soundtouch],[don't use ocaml-soundtouch]))

if test "x$enable_soundtouch" != "xno" ; then
AC_MSG_CHECKING(for ocaml-soundtouch)
if test -z "$with_soundtouch_dir" ; then
    if ! $OCAMLFIND query soundtouch > /dev/null 2>&1 ; then
        AC_MSG_RESULT(not found)
    else
        W_SOUNDTOUCH=yes
        AC_MSG_RESULT(ok)
        ocamlcflags="$ocamlcflags -package soundtouch"
        requires="$requires soundtouch"
    fi
else
    echo $with_soundtouch_dir | grep ^/ > /dev/null 2>&1 \
        || with_soundtouch_dir=$PWD/$with_soundtouch_dir
    ocamlcflags="$ocamlcflags -I $with_soundtouch_dir"
    ocamllflags="$ocamllflags soundtouch.$cma"
    requires="$requires soundtouch"
    W_SOUNDTOUCH=yes
    AC_MSG_RESULT(ok)
fi
fi

AC_SUBST(W_SOUNDTOUCH)

if test -z "$W_SOUNDTOUCH" ; then
    w_SOUNDTOUCH=no
else
    w_SOUNDTOUCH=yes
fi

#
# Portaudio
#

AC_ARG_WITH([portaudio-dir],
   AC_HELP_STRING(
      [--with-portaudio-dir=path],
      [look for ocaml-portaudio library in "path" (autodetected by default)]))
AC_ARG_ENABLE([portaudio],AC_HELP_STRING([--disable-portaudio],[don't use ocaml-portaudio]))

if test "x$enable_portaudio" != "xno" ; then
AC_MSG_CHECKING(for ocaml-portaudio)
if test -z "$with_portaudio_dir" ; then
    if ! $OCAMLFIND query portaudio > /dev/null 2>&1 ; then
        AC_MSG_RESULT(not found)
    else
        W_PORTAUDIO=yes
        AC_MSG_RESULT(ok)
        ocamlcflags="$ocamlcflags -package portaudio"
        requires="$requires portaudio"
    fi
else
    echo $with_portaudio_dir | grep ^/ > /dev/null 2>&1 \
        || with_portaudio_dir=$PWD/$with_portaudio_dir
    ocamlcflags="$ocamlcflags -I $with_portaudio_dir"
    ocamllflags="$ocamllflags portaudio.$cma"
    requires="$requires portaudio"
    W_PORTAUDIO=yes
    AC_MSG_RESULT(ok)
fi
fi

AC_SUBST(W_PORTAUDIO)

if test -z "$W_PORTAUDIO" ; then
    w_PORTAUDIO=no
else
    w_PORTAUDIO=yes
fi

#
# LADSPA
#

AC_ARG_WITH([ladspa-dir],
   AC_HELP_STRING(
      [--with-ladspa-dir=path],
      [look for ocaml-ladspa library in "path" (autodetected by default)]))
AC_ARG_ENABLE([ladspa],AC_HELP_STRING([--disable-ladspa],[don't use ocaml-ladspa]))

if test "x$enable_ladspa" != "xno" ; then
AC_MSG_CHECKING(for ocaml-ladspa)
if test -z "$with_ladspa_dir" ; then
    if ! $OCAMLFIND query ladspa > /dev/null 2>&1 ; then
        AC_MSG_RESULT(not found)
    else
        W_LADSPA=yes
        AC_MSG_RESULT(ok)
        ocamlcflags="$ocamlcflags -package ladspa"
        requires="$requires ladspa"
    fi
else
    echo $with_ladspa_dir | grep ^/ > /dev/null 2>&1 \
        || with_ladspa_dir=$PWD/$with_ladspa_dir
    ocamlcflags="$ocamlcflags -I $with_ladspa_dir"
    ocamllflags="$ocamllflags ladspa.$cma"
    requires="$requires ladspa"
    W_LADSPA=yes
    AC_MSG_RESULT(ok)
fi
fi

AC_SUBST(W_LADSPA)

if test -z "$W_LADSPA" ; then
    w_LADSPA=no
else
    w_LADSPA=yes
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

AC_ARG_ENABLE([gui],
       AC_HELP_STRING([--disable-gui],[don't use the controller GUI]))
w_PYTHON=no
if test "x$enable_gui" != "xno" ; then
AC_PATH_PROG(PYTHON,python,no)
if test \! "$PYTHON" = no ; then
  AC_MSG_CHECKING(for pygtk)
  dnl Check for pygtk and find python version
  PYVERSION=`$PYTHON -c "import sys ; import re ; import pygtk ; print re.search('^\d+\.\d+',sys.version).group(0)" 2> /dev/null`
  if test $? = 0 ; then
    AC_MSG_RESULT(yes)
    PYMDIR="\${libdir}/python$PYVERSION/site-packages/liquidsoap"
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
    AC_MSG_RESULT(no)
  fi
fi
fi
AC_SUBST(W_PYTHON)
AC_SUBST(PYMDIR)

if test "x$W_AO" = "xyes" ; then
  prefered_output=output.ao
else
  if test "x$W_ALSA" = "xyes" ; then
    prefered_output=output.alsa
  else
    prefered_output=output.dummy
  fi
fi
AC_SUBST(prefered_output)

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
AC_SUBST(requires)
AC_SUBST(cma)
AC_SUBST(cmo)
AC_SUBST(cmi)

echo

AC_CONFIG_FILES([Makefile.defs],[chmod a-w Makefile.defs])
AC_CONFIG_FILES([scripts/liquidtts],[chmod 555 scripts/liquidtts])
AC_CONFIG_FILES([scripts/liquidsoap.initd],[chmod a-w scripts/liquidsoap.initd])
AC_CONFIG_FILES([scripts/liquidsoap.gentoo.initd],
                [chmod a-w scripts/liquidsoap.gentoo.initd])
AC_CONFIG_FILES([src/META],[chmod a-w src/META])
AC_OUTPUT
make finish-configure

cat <<EOMSG

 ----------------------------------------------------------------- Compilation

 Compiler       : $OCAMLC
 OCAML_CFLAGS   : $ocamlcflags
 OCAML_LFLAGS   : $ocamllflags
 CFLAGS         : $CFLAGS
 LDFLAGS        : $LDFLAGS

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

 * Supported input formats
   - Vorbis            : $w_VORBIS
   - MP3               : $w_MP3
   - AAC               : $w_FAAD
   - text-to-speech    : $w_TTS

 * Supported output formats
   - Vorbis            : $w_VORBIS
   - MP3               : $w_LAME
   - AAC               : $w_FAAC

 * Tags
   - ID3 tags          : $w_ID3READER
   - charset detection : $w_CAMOMILE

 * Input / output
   - shout             : $w_SHOUT
   - AO                : $w_AO
   - OSS               : $w_OSS
   - ALSA              : $w_ALSA
   - Portaudio         : $w_PORTAUDIO
   - JACK              : $w_JACK
   - RTP               : $w_RTP

 * Audio manipulation
   - Samplerate        : $w_SAMPLERATE
   - SoundTouch        : $w_SOUNDTOUCH
   - LADSPA            : $w_LADSPA

 * Visualization
   - Graphics          : $w_GRAPHICS
   - OpenGL            : $w_GL

 * Additional libraries
   - xml-light         : $w_XML
   - extlib            : $w_EXTLIB

 * Graphical interfaces
   - Python GUI        : $w_PYTHON

 -----------------------------------------------------------------------------

 The Savonet Team thanks you for using liquidsoap...
 Have fun!

EOMSG
