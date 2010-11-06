
dnl BEFORE RELEASING
dnl Make sure that you edit the version number on the next line
dnl as well as the "version" variable below AND the "libs_dir_version",
dnl which is used as the final component of the standard .liq libdir,
dnl $libdir/liquidsoap/$libs_dir_version

# Remove +svn and set svn_snapshot to false
# before releasing
AC_INIT([liquidsoap],[1.0.0-beta])
svn_snapshot=true
version=1.0.0-beta

if test $svn_snapshot != "false"; then
  version="$version+svn"
  libs_dir_version="svn"
else
  libs_dir_version="$version"
fi
AC_SUBST(version)
AC_SUBST(libs_dir_version)

AC_PREREQ(2.57)
AC_CONFIG_SRCDIR([src/main.ml])
AC_PROG_INSTALL

# Check which make is GNU's
AC_MSG_CHECKING(for GNU make)
make_command=""
for a in "$MAKE" make gmake gnumake 
  do test -z "$a" && continue
     if  ( sh -c "$a --version" 2>/dev/null | grep GNU >/dev/null ) 
       then make_command=$a ; break;
     fi
  done
if test $make_command != ""
 then
  MAKE=$make_command
  AC_SUBST(MAKE)
  AC_MSG_RESULT($MAKE)
 else 
  AC_MSG_ERROR(not found)
fi

# Include a config.h
AC_CONFIG_HEADERS([config.h:config.h.in])
# Include it
CFLAGS="$CFLAGS -I\$(top_srcdir)"

cat > src/configure.ml << _BLAH_
let version = "$version"
let svn_snapshot = $svn_snapshot
let conf = Dtools.Conf.void "Liquidsoap configuration"
let var_script = ref "default"
let substs = ref
    [[
      "<script>", (fun () -> !var_script);
      "<pid>", (fun () -> string_of_int (Unix.getpid ()));
      "<home>", (fun () -> try Sys.getenv "HOME" with Not_found -> "<home>");
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
  default_prefix=/usr
else
  # Add passed prefix to compilation variables.
  CFLAGS="$CFLAGS -I$prefix/include"
  LDFLAGS="$LDFLAGS -L$prefix/lib"
  CPPFLAGS="$CPPFLAGS -I$prefix/include"
  default_prefix=$prefix
fi

# User/group for installed files and directories
# user is assumed to have access to the group.

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

AC_MSG_CHECKING(whether user $install_user exists)
if ! id $install_user >/dev/null 2>&1 ; then
  AC_MSG_RESULT(not found: will not install daemon files)
  INSTALL_DAEMON=""
else
  AC_MSG_RESULT(ok)
  AC_MSG_CHECKING(if user $install_user belongs to group $install_group)
  if ! groups $install_user | grep $install_group >/dev/null 2>&1 ; then
    AC_MSG_RESULT(no: will not install daemon files)
    INSTALL_DAEMON=""
  else
    AC_MSG_RESULT(ok)
    INSTALL_DAEMON="yes"
  fi
fi
AC_SUBST(INSTALL_DAEMON)

AC_PROG_CC

if test -z `which pkg-config 2>/dev/null` ; then
  AC_MSG_ERROR(Cannot find pkg-config)
fi

# Check endianess
AC_C_BIGENDIAN
if test "x$ac_cv_c_bigendian" = "xyes"; then
cat >> src/configure.ml <<__BLAH__
let big_endian = true
__BLAH__
else
cat >> src/configure.ml <<__BLAH__
let big_endian = false
__BLAH__
fi

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

AC_ARG_ENABLE([custom],
   AC_HELP_STRING(
      [--disable-custom],
      [disable custom mode for bytecode compilation (use if you know what you are doing)]))

AC_PATH_PROG(OCAMLOPT,ocamlopt,no)
if test "$OCAMLOPT" = no ; then
    enable_nativecode=no
fi

# Check for OS
OS_TYPE=`$OCAMLFIND ocamlc -config | grep os_type | tr -d ' ' | cut -d':' -f 2`
AC_SUBST(OS_TYPE)

# Add -fPIC to CFLAGS for compatibility with x86_64
if test "$OS_TYPE" \!= "Win32" ; then
  CFLAGS="$CFLAGS -fPIC"
fi

# Add -I`ocamlc -where` to CFLAGS for 
# user with locally installed headers.
if test "$OS_TYPE" \!= "Win32" ; then
  CFLAGS="$CFLAGS -I`ocamlc -where`"
fi

# Optimize for local machine
AC_ARG_ENABLE([optimize],
   AC_HELP_STRING(
      [--enable-optimize],
      [optimize for local machine (adds "-03 -march=native -mtune=native" to CFLAGS)]))

if test "x$enable_optimize" = "xyes" ; then
  CFLAGS="$CFLAGS -O3 -march=native -mtune=native"
fi

ocamlcflags="-ccopt \"\$(CFLAGS)\""
ocamllflags="-linkpkg -package unix -package threads -package str -package bigarray"
requires="unix threads str"
if test "$enable_nativecode" \!= "no" ; then
    BYTE=
    OCAMLCOMP=OCAMLOPT
    cma=cmxa
    cmo=cmx
    cmi=cmi
    ocamlcflags="$ocamlcflags"
    if test "x$enable_profiling" = "xyes" ; then
        ocamlcflags="-p $ocamlcflags"
    fi
    if test "x$enable_debugging" = "xyes" ; then
        CFLAGS="$CFLAGS -g"
        ocamlcflags="-g $ocamlcflags"
        ocamllflags="-g $ocamllflags"
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
    OCAMLCOMP=OCAMLC
    cma=cma
    cmo=cmo
    cmi=cmi
    if test "$enable_custom" = "no" ; then
      NO_CUSTOM=yes ;
      AC_PATH_PROG(OCAMLMKLIB,ocamlmklib,no)
      if test "$OCAMLMKLIB" = "no"; then
        AC_MSG_ERROR(Not found.)
      fi
    fi
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

OCAML_VERSION="`$OCAMLC -version`"
OCAML_MAJOR="`echo $OCAML_VERSION | cut -d'.' -f 1`"
AC_SUBST(OCAML_MAJOR)
OCAML_MINOR="`echo $OCAML_VERSION | cut -d'.' -f 2`"
AC_SUBST(OCAML_MINOR)
OCAML_REVISION="`echo $OCAML_VERSION | cut -d'.' -f 3`"
AC_SUBST(OCAML_REVISION)
AC_MSG_CHECKING([for ocamlc version])
AC_MSG_RESULT([$OCAML_MAJOR.$OCAML_MINOR.$OCAML_REVISION])

# Variables for support of TCP_NODELAY:
if test $OCAML_MAJOR -ge 3 -a $OCAML_MINOR -ge 11 ; then
  NEED_TCP_NODELAY=
  TCP_NODELAY_VALUE='let set_tcp_nodelay fd v = Unix.setsockopt fd Unix.TCP_NODELAY v'
else
  NEED_TCP_NODELAY=yes
  TCP_NODELAY_VALUE='external set_tcp_nodelay : Unix.file_descr -> bool -> unit = "stub_set_tcp_nodelay"'
fi
AC_SUBST(NEED_TCP_NODELAY)
AC_SUBST(TCP_NODELAY_VALUE)

AC_SUBST(BYTE)

# opal

AC_ARG_WITH([opal-include],
   AC_HELP_STRING(
      [--with-opal-include=path],
      [look for opal headers in path (defaults to /usr/include/opal)]))
AC_ARG_ENABLE([opal],AC_HELP_STRING([--disable-opal],["don't use opal"]))

W_OPAL=""
w_OPAL=no
if test "x$enable_opal" != "xno" ; then
  AC_MSG_CHECKING(for libopal)

  if test -z "$with_opal_include" ; then
    OPAL_INCLUDE=$default_prefix/include
  else
    OPAL_INCLUDE=$with_opal_include
  fi

  if test \! -f ${OPAL_INCLUDE}/opal/opal.h ; then
    AC_MSG_RESULT(Header not found: ${OPAL_INCLUDE}/opal/opal.h)
    W_OPAL=""
    w_OPAL=no
  else
    W_OPAL=yes
    w_OPAL=yes
    CFLAGS="$CFLAGS -I$OPAL_INCLUDE"
    AC_MSG_RESULT(ok)
  fi
else
  AC_MSG_RESULT(no)
fi

AC_SUBST(W_OPAL)

# V4L

AC_ARG_WITH([v4l-include],
   AC_HELP_STRING(
      [--with-v4l-include=path],
      [look for v4l headers in path (defaults to /usr/include/linux)]))
AC_ARG_ENABLE([v4l],AC_HELP_STRING([--disable-v4l],["don't use v4l"]))

if test "x$enable_v4l" != "xno" ; then
  AC_MSG_CHECKING(for v4l)

  if test -z "$with_v4l_include" ; then
    V4L_INCLUDE=$default_prefix/include
  else
    V4L_INCLUDE=$with_v4l_include
  fi

  if test \! -f ${V4L_INCLUDE}/linux/videodev.h ; then
    AC_MSG_RESULT(Header not found: ${V4L_INCLUDE}/linux/videodev.h)
    W_V4L=""
    w_V4L=no
  else
    W_V4L=yes
    w_V4L=yes
    CFLAGS="$CFLAGS -I$V4L_INCLUDE"
    AC_MSG_RESULT(ok)
  fi
else
    W_V4L=""
    w_V4L=no
fi

AC_SUBST(W_V4L)

#
# Libs
#
###############################################################################

# 
# Graphics
#

# ocamlfind query graphics is not enough, but linking seems okay
AC_CHECK_OCAML_BINDING([graphics],[],[],[],[],[],[${OCAMLFIND} ocamlc -linkpkg -package graphics -o /dev/null])

#
# PCRE
#

AC_CHECK_OCAML_BINDING([pcre],[],[],[1])

#
# Dtools
#

AC_CHECK_OCAML_BINDING([dtools],[],[],[1])

#
# Duppy
#

AC_CHECK_OCAML_BINDING([duppy],[0.3.0],[],[1])

#
# Cry
#

AC_CHECK_OCAML_BINDING([cry],[0.1.2])

#
# ocaml-mm
#

AC_CHECK_OCAML_BINDING([mm],[],[],[1])

#
# xmlplaylist
#

AC_CHECK_OCAML_BINDING([xmlplaylist])

#
# Lastfm
#

AC_CHECK_OCAML_BINDING([lastfm],[0.3.0],[xmlplaylist])

#
# Ogg
#

AC_CHECK_OCAML_BINDING([ogg],[0.4.0])

#
# Vorbis
#

AC_CHECK_OCAML_BINDING([vorbis],[0.5.0],[ogg])

#
# Speex
#

AC_CHECK_OCAML_BINDING([speex],[],[ogg])

#
# Mad
#

AC_CHECK_OCAML_BINDING([mad],[0.4.0])

#
# Flac
#

AC_CHECK_OCAML_BINDING([flac])
# Small hack..
if test -z "${with_flac_ogg_dir}"; then
  with_flac_ogg_dir=${with_flac_dir}
fi
AC_CHECK_OCAML_BINDING([flac.ogg],[],[ogg flac],[],[],[flac])

#
# Lame
#

AC_CHECK_OCAML_BINDING([lame])

#
# Gstreamer
#

AC_CHECK_OCAML_BINDING([gstreamer])

#
# Aacplus
#

AC_CHECK_OCAML_BINDING([aacplus],[0.2.0])

#
# Theora
#

AC_CHECK_OCAML_BINDING([theora],[0.2.0],[ogg])

#
# Schroedinger
#

AC_CHECK_OCAML_BINDING([schroedinger],[],[ogg])

#
# Gavl
#

AC_CHECK_OCAML_BINDING([gavl])

#
# Jack
#

AC_CHECK_OCAML_BINDING([bjack])

#
# Old Jack
#

AC_CHECK_OCAML_BINDING([jack])

#
# Alsa
#

AC_CHECK_OCAML_BINDING([alsa],[0.2.0])

#
# LibAO
#

AC_CHECK_OCAML_BINDING([ao],[0.2.0])

#
# Samplerate
#

AC_CHECK_OCAML_BINDING([samplerate])

#
# Taglib
#

AC_CHECK_OCAML_BINDING([taglib])

#
# OSS
#

AC_ARG_ENABLE([oss],AC_HELP_STRING([--disable-oss],["don't use OSS"]))

if test "x$enable_oss" != "xno" ; then
  AC_CHECK_HEADERS([sys/soundcard.h],[W_OSS="yes"])
fi

AC_SUBST(W_OSS)
if test "a$W_OSS" = "ayes" ; then
        w_OSS=yes
else
        w_OSS=no
fi

#
# Magic
#

AC_CHECK_OCAML_BINDING([magic])

if test -z "$W_MAGIC" ; then
    cat >> src/configure.ml <<EOCONF
let file_mime = None
let data_mime = None
EOCONF
else
    cat >> src/configure.ml <<EOCONF
let magic_cookie = Magic.create ~flags:[[Magic.Mime; Magic.Symlink]] [[]]
let file_mime = Some (Magic.file magic_cookie)
let data_mime = Some (Magic.buffer magic_cookie)
EOCONF
fi

#
# Camomile
#

AC_CHECK_OCAML_BINDING([camomile])

if test -z "$W_CAMOMILE" ; then
    echo let recode_tag ?encoding s = s >> src/configure.ml
else
    camdir=`$OCAMLFIND query camomile -l | \
      grep location | tr -d ' ' | cut -d':' -f 2`
    camencoding=Camomile.CharEncoding
    # Use Camomile with environment variables if
    # available
    if test -f $camdir/camomileLibraryDyn.cmi ; then
      if test "$OS_TYPE" \!= "Win32" ; then
        camencoding=CamomileLibraryDyn.$camencoding
      else
        # For win32, we use the functorial version 
        # with our own config.
        camencoding="CamomileLibrary.CharEncoding.Configure(CamomileConfig)"
        cat >> src/configure.ml <<EOCONF
module CamomileConfig = 
struct
  let datadir = Filename.concat "camomile" "database"
  let localedir = Filename.concat "camomile" "locales"
  let charmapdir = Filename.concat "camomile" "charmaps"
  let unimapdir = Filename.concat "camomile" "mappings"
end
EOCONF
      fi
    else
      if test -f $camdir/camomileLibrary.cmi ; then
        camencoding=CamomileLibrary.Default.$camencoding
      fi
    fi
    cat >> src/configure.ml <<EOCONF
module C = $camencoding
let enc_of_name s =
  try C.of_name s with
    | Not_found -> failwith (Printf.sprintf "Unknown encoding %s !" s)

let conf_tag =
  Dtools.Conf.void ~p:(conf#plug "tag") "Settings related to metadata tags"
let conf_encoding =
  (* The [["foo";"bla"]] may be stripped by configure here.. We avoided it by chance because
   * the configure script takes conf_tag#plug as the begining of a comment.
   * Don't forget this when modifying this inclusion later... *)
  Dtools.Conf.list ~p:(conf_tag#plug "encodings") ~d:["UTF-8";"ISO-8859-1"]
    "List of encodings to try for automatic encoding detection"

let custom_encoding = ref None

let get_encoding () =
  match !custom_encoding with
    | Some e -> e
    | None ->
        let encs = conf_encoding#get in
        let e = C.automatic "LIQ-TAGS" (List.map enc_of_name encs) C.utf8 in
          custom_encoding := Some e ;
          e

let recode_tag ?encoding s =
  let in_enc =
    match encoding with
      | Some e -> enc_of_name e
      | None -> get_encoding ()
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
# Faad
#

AC_CHECK_OCAML_BINDING([faad])

#
# Faac
#

AC_CHECK_OCAML_BINDING([faac])

#
# SoundTouch
#

AC_CHECK_OCAML_BINDING([soundtouch])

#
# Portaudio
#

AC_CHECK_OCAML_BINDING([portaudio])

#
# Pulseaudio
#

AC_CHECK_OCAML_BINDING([pulseaudio])

#
# LADSPA
#

AC_CHECK_OCAML_BINDING([ladspa])

#
# DSSI
#

AC_CHECK_OCAML_BINDING([dssi])

#
# SDL
#

AC_CHECK_OCAML_BINDING([sdl],[],[],[],[sdl.sdlmixer sdl.sdlttf sdl.sdlimage],[sdlmixer sdlttf])

#
# camlimages
#

AC_CHECK_OCAML_BINDING([camlimages])

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

if test $OS_TYPE != "Win32"; then
  AC_PATH_PROG(TEXT2WAVE,text2wave,false)
  AC_PATH_PROG(SOX,sox,false)
  AC_PATH_PROG(NORMALIZE,normalize,false)
fi

if test "$TEXT2WAVE" = false ; then
   w_TTS="liquidtts will use \"false\" as a synthetizer,
                         but you can edit it later."
else
   w_TTS="yes, using $TEXT2WAVE"
fi

if test "$SOX" = false ; then
   SOX=${CP}' $2.tmp.wav $2'
else
   SOX=${SOX}' $2.tmp.wav -t wav -c 2 -r 44100 $2'
fi

#
# GUI
#

AC_ARG_ENABLE([gui],
       AC_HELP_STRING([--disable-gui],["don't use the controller GUI"]))
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

cat >> src/configure.ml <<__BLAH__
let libs_versions = "$LIBS_VERSIONS"
__BLAH__

AC_SUBST(OCAMLC)
AC_SUBST(OCAMLCOMP)
AC_SUBST(NO_CUSTOM)
AC_SUBST(OCAMLMKLIB)
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
AC_CONFIG_FILES([src/tools/liq_sockets.ml],[chmod a-w src/tools/liq_sockets.ml])
AC_OUTPUT
$MAKE finish-configure

if test "$BYTE" = "yes"; then
  if test "$NO_CUSTOM" = "yes" ; then
    BYTE_COMP="NO_CUSTOM      : yes
 OCAMLMKLIB     : $OCAMLMKLIB
"
  fi
fi

cat <<EOMSG

 ----------------------------------------------------------------- Compilation

 Compiler       : $OCAMLC
 Version        : $OCAML_MAJOR.$OCAML_MINOR.$OCAML_REVISION
 OS             : $OS_TYPE
 OCAML_CFLAGS   : $ocamlcflags
 OCAML_LFLAGS   : $ocamllflags
 CFLAGS         : $CFLAGS
 LDFLAGS        : $LDFLAGS
 $BYTE_COMP
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
   - Theora            : $w_THEORA
   - Speex             : $w_SPEEX
   - Dirac             : $w_SCHROEDINGER
   - Flac (native)     : $w_FLAC
   - Flac (ogg)        : $w_FLAC_OGG
   - MP3               : $w_MAD
   - AAC               : $w_FAAD
   - text-to-speech    : $w_TTS
   - XML playlists     : $w_XMLPLAYLIST
   - Lastfm            : $w_LASTFM

 * Supported output formats
   - Vorbis            : $w_VORBIS
   - MP3               : $w_LAME
   - AAC               : $w_FAAC
   - AAC+              : $w_AACPLUS
   - SPEEX             : $w_SPEEX
   - Theora            : $w_THEORA
   - Dirac             : $w_SCHROEDINGER

 * Tags
   - Taglib (ID3 tags) : $w_TAGLIB
   - Vorbis            : $w_VORBIS
   - charset detection : $w_CAMOMILE

 * Input / output
   - Icecast/Shoutcast : $w_CRY
   - AO                : $w_AO
   - OSS               : $w_OSS
   - ALSA              : $w_ALSA
   - Portaudio         : $w_PORTAUDIO
   - Pulseaudio        : $w_PULSEAUDIO
   - JACK              : $w_BJACK
   - Old JACK          : $w_JACK
   - V4L               : $w_V4L
   - opal              : $w_OPAL
   - GStreamer         : $w_GSTREAMER

 * Audio manipulation
   - Samplerate        : $w_SAMPLERATE
   - SoundTouch        : $w_SOUNDTOUCH
   - LADSPA            : $w_LADSPA

 * Video manipulation
   - Gavl              : $w_GAVL
   - camlimages        : $w_CAMLIMAGES

 * MIDI manipulation
   - DSSI              : $w_DSSI

 * Visualization
   - Graphics          : $w_GRAPHICS
   - SDL               : $w_SDL

 * Additional libraries
   - magic             : $w_MAGIC

 * Graphical interfaces
   - Python GUI        : $w_PYTHON

 -----------------------------------------------------------------------------

 The Savonet Team thanks you for using liquidsoap...

 Now, run
   $MAKE
 to compile, and as root
   $MAKE install
 to install, and...
   Have fun!

EOMSG
