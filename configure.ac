
dnl BEFORE RELEASING
dnl Make sure that you edit the version number on the next line
dnl as well as the "version" variable below AND the "libs_dir_version",
dnl which is used as the final component of the standard .liq libdir,
dnl $libdir/liquidsoap/$libs_dir_version

# Remove +svn and set svn_snapshot to false
# before releasing
AC_INIT([liquidsoap],[1.0.0-beta3])
svn_snapshot=true
version=1.0.0-beta3

if test $svn_snapshot != "false"; then
  version="$version+svn"
  libs_dir_version="svn"
else
  libs_dir_version="$version"
fi
AC_SUBST(version)
AC_SUBST(libs_dir_version)

AC_PREREQ(2.61)
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
            AS_HELP_STRING([--with-user],[sets the owner of installed files]))
AC_ARG_WITH([group],
            AS_HELP_STRING([--with-group],[sets the group of installed files]))
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

AC_CHECK_OCAML_COMPILERS()

# Check for OS
OS_TYPE=`$OCAMLFIND ocamlc -config | grep os_type | tr -d ' ' | cut -d':' -f 2`
AC_SUBST(OS_TYPE)

# Add -fPIC to CFLAGS for compatibility with x86_64
if test "$OS_TYPE" \!= "Win32" ; then
  CFLAGS="$CFLAGS -fPIC"
fi

OCAML_WHERE="`$OCAMLFIND ocamlc -where`"
# Beurk!
FINDLIB_PATH="`$OCAMLFIND printconf path | tr \\\\n \\; | sed -e 's#;$##' | sed -e 's#;#";"#g'`"
if test -n "${FINDLIB_PATH}"; then
  FINDLIB_PATH="\"${FINDLIB_PATH}\""
fi
# Add -I`ocamlc -where` to CFLAGS for 
# user with locally installed headers.
if test "$OS_TYPE" \!= "Win32" ; then
  CFLAGS="$CFLAGS -I${OCAML_WHERE}"
fi
cat >> src/configure.ml <<__BLAH__
let findlib_path = [[${FINDLIB_PATH}]]
__BLAH__

# Optimize for local machine
AC_ARG_ENABLE([optimize],
   AS_HELP_STRING([--enable-optimize],[optimize for local machine (adds "-03 -march=native -mtune=native" to CFLAGS)]))

if test "x$enable_optimize" = "xyes" ; then
  CFLAGS="$CFLAGS -O3 -march=native -mtune=native"
fi

liquidsoap_ocamlcflags="-ccopt \"\$(CFLAGS)\""
liquidsoap_ocamllflags="-linkpkg -package unix -package threads -package str -package bigarray"
requires="unix threads str"
# Disable nativecode if ocamlopt is not present
if test "$OCAMLOPT" = "no"; then 
  enable_nativecode="no"
fi
if test "$enable_nativecode" \!= "no" ; then
    BYTE=
    OCAMLCOMP=OCAMLOPT
    cma=cmxa
    cmo=cmx
    cmi=cmi
    cmxs=cmxs
    shared_flag=-shared
    liquidsoap_ocamlcflags="$liquidsoap_ocamlcflags"
    if test "x$enable_profiling" = "xyes" ; then
        liquidsoap_ocamlcflags="-p $liquidsoap_ocamlcflags"
    fi
    if test "x$enable_debugging" = "xyes" ; then
        CFLAGS="$CFLAGS -g"
        liquidsoap_ocamlcflags="-g -annot $liquidsoap_ocamlcflags"
        liquidsoap_ocamllflags="-g $liquidsoap_ocamllflags"
    fi
    AC_MSG_CHECKING(whether ocamlopt accepts -ffast-math)
    if [ `$OCAMLFIND ocamlopt -ffast-math 2>/dev/null` ]; then
        AC_MSG_RESULT(yes)
        liquidsoap_ocamlcflags="-ffast-math $liquidsoap_ocamlcflags"
    else
        AC_MSG_RESULT(no)
    fi
    OCAMLC="$OCAMLFIND ocamlopt"
    _DEP_OPTS="-native"
else
    BYTE=yes
    OCAMLCOMP=OCAMLC
    cma=cma
    cmo=cmo
    cmi=cmi
    cmxs=cma
    shared_flag=-a
    if test "$enable_custom" = "no" ; then
      NO_CUSTOM=yes ;
      AC_PATH_PROG(OCAMLMKLIB,ocamlmklib,no)
      if test "$OCAMLMKLIB" = "no"; then
        AC_MSG_ERROR(Not found.)
      fi
    fi
    if test "x$enable_debugging" = "xyes" ; then
        CFLAGS="$CFLAGS -g"
        liquidsoap_ocamlcflags="$liquidsoap_ocamlcflags -g"
    fi
    if test "x$enable_profiling" = "xyes" ; then
        OCAMLC="$OCAMLFIND ocamlcp"
    else
        OCAMLC="$OCAMLFIND ocamlc"
    fi
    _DEP_OPTS=""
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
   AS_HELP_STRING([--with-opal-include=path],[look for opal headers in path (defaults to /usr/include/opal)]))
AC_ARG_ENABLE([opal],AS_HELP_STRING([--disable-opal],[don't use opal]))

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

#
# Libs
#
###############################################################################

#
# Graphics
#

AC_ARG_ENABLE([graphics],AS_HELP_STRING([--disable-graphics],[don't use the graphics library]))
if test "x$enable_graphics" = "xno" ; then
   W_GRAPHICS=""
   w_GRAPHICS="no"
else
   AC_CHECK_OCAML_BINDING([graphics])
fi

#
# PCRE
#

AC_CHECK_OCAML_BINDING([pcre],[],[],[1],[],[1])

#
# Dtools
#

AC_CHECK_OCAML_BINDING([dtools],[],[],[1],[],[1])

#
# Duppy
#

AC_CHECK_OCAML_BINDING([duppy],[0.4.1],[],[1],[],[1])

# Small hack..
if test -z "${with_duppy_syntax_dir}"; then
  if test "${CAMLP4O}" != "no" ; then
    with_duppy_syntax_dir=${with_duppy_dir}
  fi
fi
AC_CHECK_OCAML_BINDING([duppy.syntax],[0.4.1],[],[],[],[1],[duppy])
if test -n "${w_DUPPY_SYNTAX}"; then
  if test "${CAMLP4O}" != "no"; then
    if test -z "${DUPPY_SYNTAX_path}"; then
      DUPPY_SYNTAX_path=`${OCAMLFIND} query duppy`
    fi
    CAMLP4O="${CAMLP4O} ${DUPPY_SYNTAX_path}/pa_duppy.cmo"
  fi
fi

#
# Cry
#

AC_CHECK_OCAML_BINDING([cry],[0.1.2])

# ocaml-mm
#

AC_CHECK_OCAML_BINDING([mm],[],[],[1],[],[1])

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

AC_CHECK_OCAML_BINDING([ogg],[0.4.2])

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

AC_CHECK_OCAML_BINDING([mad],[0.4.3])

#
# Flac
#

AC_CHECK_OCAML_BINDING([flac])
# Small hack..
if test -z "${with_flac_ogg_dir}"; then
  with_flac_ogg_dir=${with_flac_dir}
fi
AC_CHECK_OCAML_BINDING([flac.ogg],[],[ogg flac],[],[],[],[flac])

#
# Dynlink
#

AC_CHECK_OCAML_BINDING([dynlink],[],[],[],[],[1])
# Dynlink may be for bytecode only..
AC_MSG_CHECKING([whether ocaml compiler supports dynlink])
if test -n "${W_DYNLINK}"; then
  DYN_TEST_FILE=`mktemp XXXX.ml`
  trap "rm -f $DYN_TEST_FILE; exit" SIGHUP SIGINT SIGTERM
  echo "module Foo = Dynlink" > $DYN_TEST_FILE
  if ! $OCAMLC -linkpkg -package dynlink -o /dev/null $DYN_TEST_FILE >/dev/null 2>&1; then
    AC_MSG_RESULT([no])
    AC_MSG_WARN([Dynlink module is installed but not available])
    AC_MSG_WARN([Are you compiling to native code with ocaml < 3.11 ?])
    AC_MSG_WARN([If your version of ocaml is >= 3.11, you may want to])
    AC_MSG_WARN([report that issue to your distribution.])
    W_DYNLINK=""
    w_DYNLINK="no (not available)"
  else
    AC_MSG_RESULT([yes])
  fi
  rm -f $DYN_TEST_FILE
fi

if test -z "${W_DYNLINK}"; then
  AC_MSG_WARN([no dynlink module: liquidsoap will not be able to load dynamic plugins!])
  cat >> src/configure.ml <<EOCONF
let dynlink = false
let load_dynlinks _ = ()
let load_plugins_dir _ = ()
EOCONF
else
  cat >> src/configure.ml <<EOCONF
let dynlink = true
let load_dynlinks = Dyntools.load_dynlinks
let load_plugins_dir = Dyntools.load_plugins_dir
EOCONF
fi

#
# Lame
#

AC_CHECK_OCAML_BINDING([lame])
if test -z "${W_LAME}"; then
  if test -n "${W_DYNLINK}"; then
    if test -n "${OCAML_HAS_FIRST_CLASS_MODULES}"; then
       W_LAME_DYN=yes
       w_LAME="detected at runtime"
    fi
  fi
fi
AC_SUBST(W_LAME_DYN)

#
# Gstreamer
#

AC_CHECK_OCAML_BINDING([gstreamer])

#
# Aacplus
#

AC_CHECK_OCAML_BINDING([aacplus],[0.2.0])
if test -z "${W_AACPLUS}"; then
  if test -n "${W_DYNLINK}"; then
    if test -n "${OCAML_HAS_FIRST_CLASS_MODULES}"; then
       W_AACPLUS_DYN=yes
       w_AACPLUS="detected at runtime"
    fi
  fi
fi
AC_SUBST(W_AACPLUS_DYN)

#
# Voaacenc
#

AC_CHECK_OCAML_BINDING([voaacenc])

#
# Theora
#

AC_CHECK_OCAML_BINDING([theora],[0.3.0],[ogg])

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

AC_CHECK_OCAML_BINDING([taglib],[0.2.0])

#
# OSS
#

AC_ARG_ENABLE([oss],AS_HELP_STRING([--disable-oss],[don't use OSS]))

AC_ARG_ENABLE([oss-dynamic-plugin],
   AS_HELP_STRING([--enable-oss-dynamic-plugin],[Compile oss as an optional plugin.]))

if test "x$enable_oss" != "xno" ; then
  AC_CHECK_HEADERS([sys/soundcard.h],[W_OSS="yes"])
fi

if test "a$W_OSS" = "ayes" ; then
  if test "x$enable_oss_dynamic_plugin" = "xyes"; then
        W_OSS=oss
        w_OSS=plugin
        PLUGINS="$PLUGINS oss"
  else
        w_OSS=yes
  fi
else
        w_OSS=no
fi
AC_SUBST(W_OSS)

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

AC_ARG_ENABLE([camomile],
  AS_HELP_STRING([--disable-camomile],[don't use camomile (no charset detection and conversion)]))

AC_CHECK_OCAML_BINDING([camomile])

if test -z "$W_CAMOMILE" ; then
  if test "x$enable_camomile" = "xno" ; then
    echo let recode_tag ?in_enc ?out_enc s = s >> src/configure.ml
  else
    AC_MSG_ERROR([Camomile provides charset detection and conversions. It is strongly advised to enable those features. If you really don't want this, use --disable-camomile.])
  fi
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

exception Unknown_encoding of string

let enc_of_name s =
  try C.of_name s with
    | Not_found -> raise (Unknown_encoding s)

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

let camolog = Dtools.Log.make [["camomile"]]

exception Input_encoding of string
exception Output_encoding of string

let recode_tag ?in_enc ?out_enc s =
  try
    let in_enc =
      try
       match in_enc with
         | Some e -> enc_of_name e
         | None -> get_encoding ()
      with Unknown_encoding s -> 
             raise (Input_encoding s)
    in
    let out_enc =
      try
       match out_enc with
         | Some e -> enc_of_name e
         | None -> C.utf8
      with Unknown_encoding s ->
             raise (Output_encoding s)
    in
      try C.recode_string ~in_enc ~out_enc s with e ->
        let in_enc =
          if in_enc == get_encoding () then
            Printf.sprintf "auto(%s)" (String.concat "," conf_encoding#get)
          else
            C.name_of in_enc
        in
        camolog#f 3 "Failed to convert %S from %s to %s (%s)!"
          s in_enc (C.name_of out_enc) (Utils.error_message e) ;
        s
  with
    | Unknown_encoding e ->
          camolog#f 3 "Failed to convert %S: unknown encoding %s"
              s e ; s
    | Input_encoding e -> 
          camolog#f 3 "Failed to convert %S: unknown input encoding %s"
              s e ; s
    | Output_encoding e ->        
          camolog#f 3 "Failed to convert %S: unknown output encoding %s"
              s e ; s
    | e ->
          camolog#f 3 "Failed to convert %S: unknown error %s"
              s (Utils.error_message e) ; s

let recode_tag =
  if env_has "LIQ_DISABLE_CAMOMILE" then
    fun ?in_enc ?out_enc s -> s
  else
    recode_tag
EOCONF
fi

# OCaml bindings

AC_CHECK_OCAML_BINDING([faad])
AC_CHECK_OCAML_BINDING([soundtouch])
AC_CHECK_OCAML_BINDING([portaudio])
AC_CHECK_OCAML_BINDING([pulseaudio])
AC_CHECK_OCAML_BINDING([ladspa])
AC_CHECK_OCAML_BINDING([dssi])
AC_CHECK_OCAML_BINDING([sdl],[],[],[],[sdl.sdlmixer sdl.sdlttf sdl.sdlimage])
AC_CHECK_OCAML_BINDING([camlimages],[4.0.0],[],[],[camlimages.all_formats])
AC_CHECK_OCAML_BINDING([lo])
AC_CHECK_OCAML_BINDING([yojson],[],[],[],[],[1])
AC_CHECK_OCAML_BINDING([gd],[],[],[],[],[1])

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

if test $OS_TYPE != "Win32"; then
  AC_PATH_PROG(TEXT2WAVE,text2wave,false)
  AC_PATH_PROG(SOX,sox,false)
fi

if test "$TEXT2WAVE" = false -o "$SOX" = false ; then
   w_TTS="requires text2wave (festival) and sox at runtime"
else
   w_TTS="yes"
fi

#
# GUI
#

AC_ARG_ENABLE([gui],
       AS_HELP_STRING([--disable-gui],[don't use the controller GUI]))
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

AC_SUBST(OCAMLCOMP)
AC_SUBST(NO_CUSTOM)
AC_SUBST(_DEP_OPTS)
AC_SUBST(liquidsoap_ocamlcflags)
AC_SUBST(liquidsoap_ocamllflags)
AC_SUBST(requires)
AC_SUBST(cma)
AC_SUBST(cmo)
AC_SUBST(cmi)
AC_SUBST(cmxs)
AC_SUBST(shared_flag)
AC_SUBST(PLUGINS)
AC_SUBST(PLUGINS_DATA)

echo

AC_CONFIG_FILES([Makefile.defs],[chmod a-w Makefile.defs])
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

if test "$CAMLP4O" != "no"; then
    CAMLP4O_COMP="CAMLP4O        : $CAMLP4O
"
fi

cat <<EOMSG

 ----------------------------------------------------------------- Compilation

 Compiler       : $OCAMLC
 Version        : $OCAML_MAJOR.$OCAML_MINOR.$OCAML_REVISION
 OS             : $OS_TYPE
 Plugins        :$PLUGINS
 OCAML_CFLAGS   : $liquidsoap_ocamlcflags
 OCAML_LFLAGS   : $liquidsoap_ocamllflags
 CFLAGS         : $CFLAGS
 LDFLAGS        : $LDFLAGS
 ${BYTE_COMP}${CAMLP4O_COMP}
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
   - AAC               : $w_VOAACENC
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
   - GD                : $w_GD

 * Additional libraries
   - dynlink           : $w_DYNLINK
   - magic             : $w_MAGIC
   - lo                : $w_LO
   - yojson            : $w_YOJSON

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
