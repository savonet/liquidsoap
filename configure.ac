# Liquidsoap autoconf setup

AC_INIT([liquidsoap],
        m4_esyscmd([cat liquidsoap.opam | grep ^version | sed 's/version: "\([^"]*\)"/\1/' | tr -d '\r\n' | sed -e 's#~#-#g']),
        [savonet-users@lists.sourceforge.net],
        [liquidsoap],
        [https://github.com/savonet/liquidsoap])
git_snapshot=true
version="$PACKAGE_VERSION"
min_ocaml_version=m4_esyscmd([cat liquidsoap.opam | grep \"ocaml\" | sed 's/[ ]*"ocaml" {>= "\([^"]*\)"}/\1/' | tr -d '\r\n'])
git_commit=`git rev-parse --short HEAD || true`

if test $git_snapshot != "false"; then
  if test -n "$git_commit"; then
    version="$version+git@$git_commit"
    liq_libs_dir_version="git/$git_commit"
  else
    version="$version+dev"
    liq_libs_dir_version="dev"
  fi
else
  liq_libs_dir_version="$version"
fi

AC_ARG_WITH([libs-dir],
            AS_HELP_STRING([--with-libs-dir],[sets path to install the internal libraries to. Default: <datadir>/liquidsoap/<version>]))
if test -z "$with_libs_dir" ; then
  liq_libs_dir="${datadir}/liquidsoap/${liq_libs_dir_version}"
else
  liq_libs_dir="$with_libs_dir"
fi

AC_SUBST(version)
AC_SUBST(min_ocaml_version)
AC_SUBST(liq_libs_dir)

ocaml_version_descr=">= ${min_ocaml_version}"
AC_SUBST(ocaml_version_descr)

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

AC_CANONICAL_HOST

build_os=""

# Detect the target system
case "${host_os}" in
    linux*)
        build_os="linux"
        ;;
    cygwin*|mingw*)
        build_os="windows"
        ;;
    darwin*)
        build_os="osx"
        ;;
    *)
        AC_MSG_ERROR(["OS $host_os is not supported"])
        ;;
esac

cat > src/configure.ml << _BLAH_
let host = "$build_os"
let version = "$version"
let git_commit = "$git_commit"
let git_snapshot = $git_snapshot
let conf = Utils.conf
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
  AC_MSG_RESULT(no)
  INSTALL_DAEMON=""
else
  AC_MSG_RESULT(ok)
  AC_MSG_CHECKING(if user $install_user belongs to group $install_group)
  if ! groups $install_user | grep $install_group >/dev/null 2>&1 ; then
    AC_MSG_RESULT(no)
    INSTALL_DAEMON=""
  else
    AC_MSG_RESULT(ok)
    INSTALL_DAEMON="yes"
  fi
fi
if test "$INSTALL_DAEMON" != "yes" ; then
  AC_MSG_WARN([Won't be able to install log and PID directories!])
fi
AC_SUBST(INSTALL_DAEMON)

AC_PROG_CC

if test -z `which pkg-config 2>/dev/null` ; then
  AC_MSG_ERROR(Cannot find pkg-config)
fi

AC_ARG_WITH([bash-completion-dir],
   AS_HELP_STRING(
      [--with-bash-completion-dir=path],
      [path to directory used to install bash-completion files (autodetected by default)]))

if test -n "${with_bash_completion_dir}" ; then
  bashcompdir="${with_bash_completion_dir}"
else
  dnl The following does not correctly uses prefix
  dnl PKG_CHECK_VAR(bashcompdir, [bash-completion], [completionsdir], , bashcompdir="${sysconfdir}/bash_completion.d")
  bashcompdir=`pkg-config --define-variable=prefix='${prefix}' --variable=completionsdir bash-completion 2>/dev/null`
  AS_IF([test -z "$bashcompdir"], [bashcompdir='${sysconfdir}/bash_completion.d'])
fi
AC_SUBST(bashcompdir)

emacsdir='${prefix}'/share/emacs/site-lisp/liquidsoap-mode
AC_SUBST(emacsdir)

# Check endianness
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
OCAMLC_SYSTEM=`$OCAMLFIND ocamlc -config | grep system | cut -d' ' -f 2`
if test "$OCAMLC_SYSTEM" = "mingw" -o "$OCAMLC_SYSTEM" = "mingw64"; then
  OS_TYPE="Win32"
  WIN32_HOST="yes"
else
  OS_TYPE="Unix"
  UNIX_HOST="yes"
fi
AC_SUBST(OS_TYPE)
AC_SUBST(WIN32_HOST)
AC_SUBST(UNIX_HOST)

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
CFLAGS="$CFLAGS -I${OCAML_WHERE}"
cat >> src/configure.ml <<__BLAH__
let findlib_path = [[${FINDLIB_PATH}]]
__BLAH__

# Optimize for local machine
AC_ARG_ENABLE([optimize],
   AS_HELP_STRING([--enable-optimize],[optimize for local machine (adds "-03 -march=native -mtune=native" to CFLAGS)]))

if test "x$enable_optimize" = "xyes" ; then
  CFLAGS="$CFLAGS -O3 -march=native -mtune=native"
fi

liquidsoap_ocamlcflags="$OCAMLFLAGS -ccopt \"\$(CFLAGS)\""
liquidsoap_ocamllflags="-linkpkg -package unix -package threads -package str -package bigarray"

if test "$OS_TYPE" \!= "Win32" ; then
  liquidsoap_ocamllflags="$liquidsoap_ocamllflags -cc \"\$(CC)\""
fi

requires="unix threads str"
if test "$enable_debugging" \!= "no" ; then
  CFLAGS="$CFLAGS -g"
  liquidsoap_ocamlcflags="-g -annot -bin-annot $liquidsoap_ocamlcflags"
  liquidsoap_ocamllflags="-g $liquidsoap_ocamllflags"
fi
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

AC_OCAML_COMPARE_VERSION([$OCAML_MAJOR.$OCAML_MINOR.$OCAML_REVISION],[$min_ocaml_version])
if test -z "${VERSION_OK}"; then
  AC_MSG_ERROR([version $min_ocaml_version or greater of the OCaml compiler is required to build liquidsoap])
fi

AC_SUBST(BYTE)

AC_ARG_ENABLE([custom-path],
   AS_HELP_STRING([--enable-custom-path],[Enable custom path support for loading liquidsoap's dependencies (always enable for win32 builds).]))

if test "$OS_TYPE" = "Win32" -o "x$enable_custom_path" = "xyes"; then
  W_CUSTOM_PATH="yes"
  w_CUSTOM_PATH="ok"
cat >> src/configure.ml <<__BLAH__
let get_dir d = 
  let dir =
    Utils.resolve_path (Filename.dirname Sys.executable_name)
  in
  let cwd =
    match Utils.getenv_opt "LIQUIDSOAP_BASE_DIR" with
      | None      -> dir
      | Some path -> Utils.resolve_path ~cwd:dir path
  in 
  Utils.resolve_path ~cwd d
__BLAH__
else
  W_CUSTOM_PATH="no"
  w_CUSTOM_PATH=""
fi
AC_SUBST(W_CUSTOM_PATH)

#
# Libs
#
###############################################################################

#
# Graphics
#

AC_CHECK_OCAML_BINDING([graphics])

#
# Bytes
#

AC_CHECK_OCAML_BINDING([bytes],[],[1])

#
# Posix-time
#
AC_CHECK_OCAML_BINDING([posix-time2])

#
# PCRE
#

AC_CHECK_OCAML_BINDING([pcre],[],[1])

#
# SEDLEX
#

AC_CHECK_OCAML_BINDING([sedlex],[],[1],[sedlex sedlex.ppx],[1])

#
# MENHIR
#

AC_CHECK_OCAML_BINDING([menhirLib],[],[1],[],[1])

MENHIR=`which menhir 2>/dev/null`
if test -z "${MENHIR}" ; then
  AC_MSG_ERROR(Cannot find menhir binary)
fi
AC_SUBST(MENHIR)

#
# Curl
#

AC_CHECK_OCAML_BINDING([curl])

#
# Memtrace
#

AC_CHECK_OCAML_BINDING([memtrace])

#
# Mem_usage
#

AC_CHECK_OCAML_BINDING([mem_usage])

#
# Dtools
#

AC_CHECK_OCAML_BINDING([dtools],[],[1])

#
# Duppy
#

AC_CHECK_OCAML_BINDING([duppy],[],[1])

#
# Cry
#

AC_CHECK_OCAML_BINDING([cry])

# ocaml-mm
#

AC_CHECK_OCAML_BINDING([mm],[],[1])

#
# xmlplaylist
#

AC_CHECK_OCAML_BINDING([xmlplaylist])

#
# Lastfm
#

AC_CHECK_OCAML_BINDING([lastfm],[xmlplaylist curl])

#
# Ogg
#

AC_CHECK_OCAML_BINDING([ogg])
AC_CHECK_OCAML_BINDING([ogg.decoder],[ogg])

#
# Vorbis
#

AC_CHECK_OCAML_BINDING([vorbis],[ogg])
AC_CHECK_OCAML_BINDING([vorbis.decoder],[vorbis])

#
# Opus
#

AC_CHECK_OCAML_BINDING([opus],[ogg])
AC_CHECK_OCAML_BINDING([opus.decoder],[opus])

#
# Speex
#

AC_CHECK_OCAML_BINDING([speex],[ogg])
AC_CHECK_OCAML_BINDING([speex.decoder],[speex])

#
# Mad
#

AC_CHECK_OCAML_BINDING([mad])

#
# Flac
#

AC_CHECK_OCAML_BINDING([flac])
# Small hack..
if test -z "${with_flac_ogg_dir}"; then
  with_flac_ogg_dir=${with_flac_dir}
fi
AC_CHECK_OCAML_BINDING([flac.ogg],[ogg flac],[],[],[flac])
AC_CHECK_OCAML_BINDING([flac.decoder],[flac.ogg])

#
# Dynlink
#

AC_CHECK_OCAML_BINDING([dynlink])
# Dynlink may be for bytecode only..
AC_MSG_CHECKING([whether ocaml compiler supports dynlink])
if test -n "${W_DYNLINK}"; then
  DYN_TEST_FILE_ML=`mktemp XXXX.ml`
  trap "rm -f $DYN_TEST_FILE_ML; exit" SIGHUP SIGINT SIGTERM
  echo "module Foo = Dynlink" > $DYN_TEST_FILE_ML
  if ! $OCAMLC -linkpkg -package dynlink -o /dev/null $DYN_TEST_FILE_ML >/dev/null 2>&1; then
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
  DYN_TEST_FILE=`basename $DYN_TEST_FILE_ML .ml`
  rm -f $DYN_TEST_FILE_ML $DYN_TEST_FILE.cmx $DYN_TEST_FILE.cmo $DYN_TEST_FILE.cmi $DYN_TEST_FILE.o
fi

if test -z "${W_DYNLINK}"; then
  AC_MSG_WARN([no dynlink module: liquidsoap will not be able to load dynamic plugins!])
  cat >> src/configure.ml <<EOCONF
let dynlink = false
EOCONF
else
  cat >> src/configure.ml <<EOCONF
let dynlink = true
let () =
  Lifecycle.before_init (fun () ->
    Dyntools.load_dynlinks ())
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
# Shine
#

AC_CHECK_OCAML_BINDING([shine])

#
# Gstreamer
#

AC_CHECK_OCAML_BINDING([gstreamer])

#
# frei0r
#

AC_CHECK_OCAML_BINDING([frei0r])

#
# Fdkaac
#

AC_CHECK_OCAML_BINDING([fdkaac])
if test -z "${W_FDKAAC}"; then
  if test -n "${W_DYNLINK}"; then
    if test -n "${OCAML_HAS_FIRST_CLASS_MODULES}"; then
       W_FDKAAC_DYN=yes
       w_FDKAAC="detected at runtime"
    fi
  fi
fi
AC_SUBST(W_FDKAAC_DYN)

#
# Theora
#

AC_CHECK_OCAML_BINDING([theora],[ogg])
AC_CHECK_OCAML_BINDING([theora.decoder],[theora])

#
# FFMPEG
#

AC_CHECK_OCAML_BINDING([ffmpeg])

#
# Jack
#

AC_CHECK_OCAML_BINDING([bjack])

#
# Alsa
#

AC_CHECK_OCAML_BINDING([alsa])

#
# LibAO
#

AC_CHECK_OCAML_BINDING([ao])

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

AC_ARG_ENABLE([oss],AS_HELP_STRING([--disable-oss],[don't use OSS]))

if test "x$enable_oss" != "xno" ; then
  AC_CHECK_HEADERS([sys/soundcard.h],[W_OSS="yes"])
fi

if test "a$W_OSS" = "ayes" ; then
  w_OSS=yes
else
  w_OSS=no
fi
AC_SUBST(W_OSS)

#
# SSL
#

AC_CHECK_OCAML_BINDING([ssl])

if test "a$W_SSL" = "ayes"; then
    cat >> src/configure.ml <<EOCONF
let () =
  Ssl_threads.init ();
  Ssl.init ()
EOCONF
fi

#
# SecureTransport (OSX)
#

AC_CHECK_OCAML_BINDING([osx-secure-transport])

#
# Magic
#

AC_CHECK_OCAML_BINDING([magic])

if test -z "$W_MAGIC" ; then
    cat >> src/configure.ml <<EOCONF
let file_mime = None
EOCONF
else
  if test -z "$w_CUSTOM_PATH"; then
    cat >> src/configure.ml <<EOCONF
let magic_cookie = Magic.create ~flags:[[Magic.Mime; Magic.Symlink]] [[]]
let file_mime = Some (Magic.file magic_cookie)
EOCONF
  else
    cat >> src/configure.ml <<EOCONF
let magic_file  =
  let cwd = Filename.dirname Sys.executable_name in
  let magic = 
    Utils.getenv ~default:(get_dir "magic/magic.mgc") "MAGIC"
  in
  Utils.resolve_path ~cwd magic
let magic_cookie = Magic.create ~flags:[[Magic.Mime; Magic.Symlink]] [[magic_file]]
let file_mime = Some (Magic.file magic_cookie)
EOCONF
  fi
fi

#
# Camomile
#

AC_ARG_ENABLE([camomile],
  AS_HELP_STRING([--disable-camomile],[don't use camomile (no charset detection and conversion)]))

AC_CHECK_OCAML_BINDING([camomile])

AC_ARG_WITH([camomile-data-dir],
   AS_HELP_STRING(
      [--with-camomile-data-dir=path],
      [path to camomile data files (autodetected by default)]))

if test -z "$W_CAMOMILE" ; then
  if test "x$enable_camomile" = "xno" ; then
    echo let recode_tag ?in_enc:_ ?out_enc:_ s = s >> src/configure.ml
  else
    AC_MSG_ERROR([Camomile provides charset detection and conversions. It is strongly advised to enable those features. If you really don't want this, use --disable-camomile.])
  fi
else
    if test -n "${with_camomile_data_dir}" ; then
      camencoding="CamomileLibrary.CharEncoding.Configure(CamomileConfig)"
      cat >> src/configure.ml <<EOCONF
let camomile_dir = "${with_camomile_data_dir}"
EOCONF
    elif test -n "$w_CUSTOM_PATH" ; then
      cat >> src/configure.ml <<EOCONF
let camomile_dir = get_dir "camomile"
EOCONF
    else
      cat >> src/configure.ml <<EOCONF
let camomile_dir = CamomileDefaultConfig__.InstallConfig.share_dir
EOCONF
    fi
    cat >> src/configure.ml <<EOCONF
module C = CamomileLibrary.CharEncoding.Configure(struct
  let datadir = Filename.concat camomile_dir "database"
  let localedir = Filename.concat camomile_dir "locales"
  let charmapdir = Filename.concat camomile_dir "charmaps"
  let unimapdir = Filename.concat camomile_dir "mappings"
end)

exception Unknown_encoding of string

let enc_of_name s =
  try C.of_name s with
    | Not_found -> raise (Unknown_encoding s)

let conf_tag =
  Dtools.Conf.void ~p:(conf#plug "tag") "Settings related to metadata tags"
let conf_encoding =
  (* The [["foo";"bla"]] may be stripped by configure here.. We avoided it by chance because
   * the configure script takes conf_tag#plug as the beginning of a comment.
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

let camolog = Log.make [["camomile"]]

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
        camolog#important "Failed to convert %S from %s to %s (%s)!"
          s in_enc (C.name_of out_enc) (Printexc.to_string e) ;
        s
  with
    | Unknown_encoding e ->
          camolog#important "Failed to convert %S: unknown encoding %s"
              s e ; s
    | Input_encoding e -> 
          camolog#important "Failed to convert %S: unknown input encoding %s"
              s e ; s
    | Output_encoding e ->        
          camolog#important "Failed to convert %S: unknown output encoding %s"
              s e ; s
    | e ->
          camolog#important "Failed to convert %S: unknown error %s"
              s (Printexc.to_string e) ; s

let env_has key = try ignore (Sys.getenv key) ; true with Not_found -> false

let recode_tag =
  if env_has "LIQ_DISABLE_CAMOMILE" then
    fun ?in_enc:_ ?out_enc:_ s -> s
  else
    recode_tag
EOCONF
fi

# Inotify

AC_CHECK_OCAML_BINDING([inotify])

cat >> src/configure.ml <<EOCONF
let file_watcher = ref File_watcher_mtime.watch
EOCONF

# Yojson

AC_CHECK_OCAML_BINDING([yojson])

if test -z "${W_YOJSON}"; then
cat >> src/configure.ml <<__BLAH__
module JSON = JSON
__BLAH__
else
cat >> src/configure.ml <<__BLAH__
module JSON = struct include Yojson.Basic let from_string s = from_string s let to_string j = pretty_to_string j end
__BLAH__
fi

# OCaml bindings

AC_CHECK_OCAML_BINDING([faad])
AC_CHECK_OCAML_BINDING([soundtouch])
AC_CHECK_OCAML_BINDING([portaudio])
AC_CHECK_OCAML_BINDING([pulseaudio])
AC_CHECK_OCAML_BINDING([ladspa])
AC_CHECK_OCAML_BINDING([dssi])
AC_CHECK_OCAML_BINDING([lilv])
AC_CHECK_OCAML_BINDING([tsdl])
AC_CHECK_OCAML_BINDING([tsdl-ttf],[tsdl])
AC_CHECK_OCAML_BINDING([tsdl-image],[tsdl])
AC_CHECK_OCAML_BINDING([camlimages],[],[],[camlimages.all_formats])
AC_CHECK_OCAML_BINDING([cohttp-lwt-unix])
AC_CHECK_OCAML_BINDING([prometheus-app],[cohttp-lwt-unix])

# TODO: switch to dune all the way?
with_srt_constants_dir=${with_srt_dir}
liquidsoap_ocamlcflags="${liquidsoap_ocamlcflags} -I ${with_srt_dir}/constants"
AC_CHECK_OCAML_BINDING([srt.constants],[],[],[],[srt_constants])

with_srt_types_dir=${with_srt_dir}
liquidsoap_ocamlcflags="${liquidsoap_ocamlcflags} -I ${with_srt_dir}/types"
AC_CHECK_OCAML_BINDING([srt.types],[],[],[],[srt_types])

with_srt_stubs_dir=${with_srt_dir}
liquidsoap_ocamlcflags="${liquidsoap_ocamlcflags} -I ${with_srt_dir}/stubs"
AC_CHECK_OCAML_BINDING([srt.stubs],[srt.types],[],[],[srt_stubs])

with_srt_stubs_locked_dir=${with_srt_dir}
liquidsoap_ocamlcflags="${liquidsoap_ocamlcflags} -I ${with_srt_dir}/stubs/locked"
AC_CHECK_OCAML_BINDING([srt.stubs.locked],[],[],[],[srt_stubs_locked])

AC_CHECK_OCAML_BINDING([srt],[srt.constants srt.types srt.stubs srt.stubs.locked])

AC_CHECK_OCAML_BINDING([lo])
AC_CHECK_OCAML_BINDING([gd])

# Winsvc (to run as a service)

AC_CHECK_OCAML_BINDING([winsvc])

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
# Font for video.add_text*()
#

AC_ARG_WITH([default-font],
            AS_HELP_STRING([--with-default-font],
                           [sets the default TTF font for video.add_text()]))
if test -z "$with_default_font" ; then
  DEFAULT_FONT=/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf
else
  DEFAULT_FONT=$with_default_font
fi
cat >> src/configure.ml <<__BLAH__
let default_font = "$DEFAULT_FONT"
__BLAH__

if test "x$W_AO" = "xyes" ; then
  preferred_output=output.ao
else
  if test "x$W_ALSA" = "xyes" ; then
    preferred_output=output.alsa
  else
    preferred_output=output.dummy
  fi
fi
AC_SUBST(preferred_output)

#
# All done.
#
###############################################################################

cat >> src/configure.ml <<__BLAH__
let libs_versions = "$LIBS_VERSIONS"
__BLAH__

AC_SUBST(OCAMLPATH)
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

echo

AC_CONFIG_FILES([doc/content/install.md])
AC_CONFIG_FILES([doc/content/build.md])
AC_CONFIG_FILES([doc/liquidsoap.1.md])
AC_CONFIG_FILES([Makefile.defs],[chmod a-w Makefile.defs])
AC_CONFIG_FILES([src/META],[chmod a-w src/META])
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

 Liquidsoap version: $version

 ----------------------------------------------------------------- Compilation

 Compiler       : $OCAMLC
 Version        : $OCAML_MAJOR.$OCAML_MINOR.$OCAML_REVISION
 OS             : $OS_TYPE
 Plugins        :$PLUGINS
 Library path   : $liq_libs_dir
 Custom path    : $W_CUSTOM_PATH
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
 bash completion: $bashcompdir
 emacs mode     : $emacsdir

 -------------------------------------------------------------------- Features

 * Liquidsoap
   - version           : $version

 * Supported input formats
   - MP3               : $w_MAD
   - AAC               : $w_FAAD
   - FFmpeg            : $w_FFMPEG
   - Flac (native)     : $w_FLAC
   - Flac (ogg)        : $w_FLAC_OGG
   - Lastfm            : $w_LASTFM
   - Opus              : $w_OPUS
   - Speex             : $w_SPEEX
   - Theora            : $w_THEORA
   - Vorbis            : $w_VORBIS
   - XML playlists     : $w_XMLPLAYLIST

 * Supported output formats
   - FDK-AAC           : $w_FDKAAC
   - FFmpeg            : $w_FFMPEG
   - MP3               : $w_LAME
   - MP3 (fixed-point) : $w_SHINE
   - Opus              : $w_OPUS
   - SPEEX             : $w_SPEEX
   - Theora            : $w_THEORA
   - Vorbis            : $w_VORBIS

 * Tags
   - charset detection : $w_CAMOMILE
   - Taglib (ID3 tags) : $w_TAGLIB
   - Vorbis            : $w_VORBIS

 * Input / output
   - ALSA              : $w_ALSA
   - AO                : $w_AO
   - FFmpeg            : $w_FFMPEG
   - Icecast/Shoutcast : $w_CRY
   - GStreamer         : $w_GSTREAMER
   - JACK              : $w_BJACK
   - OSS               : $w_OSS
   - Portaudio         : $w_PORTAUDIO
   - Pulseaudio        : $w_PULSEAUDIO
   - SRT               : $w_SRT

 * Audio manipulation
   - FFmpeg            : $w_FFMPEG
   - LADSPA            : $w_LADSPA
   - Lilv              : $w_LILV
   - Samplerate        : $w_SAMPLERATE
   - SoundTouch        : $w_SOUNDTOUCH

 * Video manipulation
   - camlimages        : $w_CAMLIMAGES
   - FFmpeg            : $w_FFMPEG
   - frei0r            : $w_FREI0R
   - SDL-image         : $w_TSDL_IMAGE
   - SDL-ttf           : $w_TSDL_TTF

 * MIDI manipulation
   - DSSI              : $w_DSSI

 * Visualization
   - GD                : $w_GD
   - Graphics          : $w_GRAPHICS
   - SDL               : $w_TSDL

 * Additional libraries
   - http requests     : $w_CURL
   - FFmpeg filters    : $w_FFMPEG
   - FFmpeg devices    : $w_FFMPEG
   - dynlink           : $w_DYNLINK
   - inotify           : $w_INOTIFY
   - lastfm            : $w_LASTFM
   - lo                : $w_LO
   - magic             : $w_MAGIC
   - memtrace          : $w_MEMTRACE
   - mem_usage         : $w_MEM_USAGE
   - SecureTransport   : $w_OSX_SECURE_TRANSPORT
   - ssl               : $w_SSL
   - posix-time2       : $w_POSIX_TIME2
   - windows service   : $w_WINSVC
   - yojson            : $w_YOJSON

 * Monitoring
   - Prometheus        : $w_PROMETHEUS_APP

 -----------------------------------------------------------------------------

 You may now compile liquidsoap by running "$MAKE".
 In case of significant library update, "$MAKE clean" is needed first.

 If you want a local version of the documentation, run "$MAKE doc".

 Then, perform installation by running "$MAKE install" as root.

 And... have fun!

EOMSG

if test "$INSTALL_DAEMON" != "yes" ; then
  cat <<EOMSG
<WARNING>

 The specified user/group ($with_user/$with_group) for running liquidsoap as a daemon
 do not exist, so I won't install log and PID directories.

 For a fully functional liquidsoap installation, please configure again
 with existing user/group. You may use --with-user=<user> --with-group=<group> 
 to override the default.

</WARNING>

EOMSG
fi

if ! test -f $DEFAULT_FONT ; then
  cat <<EOMSG
<WARNING>

  The specified default font does not exist. If it is not installed before
  running liquidsoap, the video.add_text*() operators won't work with their 
  default setting.

  The current value is $DEFAULT_FONT.
  You may use --with-default-font=<font> to override it.

</WARNING>
EOMSG
fi

cat >| liquidsoap.config <<EOCFG
opam-version: "2.0"
variables {
  alsa-enabled: $b_ALSA
  ao-enabled: $b_AO
  bjack-enabled: $b_BJACK
  camlimages-enabled: $b_CAMLIMAGES
  camomile-enabled: $b_CAMOMILE
  curl-enabled: $b_CURL
  cry-enabled: $b_CRY
  dssi-enabled: $b_DSSI
  faad-enabled: $b_FAAD
  fdkaac-enabled: $b_FDKAAC
  ffmpeg-enabled: $b_FFMPEG
  flac-enabled: $b_FLAC
  frei0r-enabled: $b_FREI0R
  gd-enabled: $b_GD
  graphics-enabled: $b_GRAPHICS
  gstreamer-enabled: $b_GSTREAMER
  inotify-enabled: $b_INOTIFY
  ladspa-enabled: $b_LADSPA
  lame-enabled: $b_LAME
  lastfm-enabled: $b_LASTFM
  lo-enabled: $b_LO
  mad-enabled: $b_MAD
  magic-enabled: $b_MAGIC
  memtrace-enabled: $b_MEMTRACE
  mem_usage-enabled: $b_MEM_USAGE
  ogg-enabled: $b_OGG
  ogg_flac-enabled: $b_FLAC_OGG
  opus-enabled: $b_OPUS
  portaudio-enabled: $b_PORTAUDIO
  posix-time2-enabled: $b_POSIX_TIME2
  prometheus-enabled: $b_PROMETHEUS_APP
  pulseaudio-enabled: $b_PULSEAUDIO
  samplerate-enabled: $b_SAMPLERATE
  sdl-enabled: $b_TSDL
  sdl-image-enabled: $b_TSDL_IMAGE
  sdl-ttf-enabled: $b_TSDL_TTF
  secure_transport-enabled: $b_OSX_SECURE_TRANSPORT
  shine-enabled: $b_SHINE
  soundtouch-enabled: $b_SOUNDTOUCH
  speex-enabled: $b_SPEEX
  srt-enabled: $b_SRT
  ssl-enabled: $b_SSL
  taglib-enabled: $b_TAGLIB
  theora-enabled: $b_THEORA
  vorbis-enabled: $b_VORBIS
  winscv-enabled: $b_WINSVC
  xmlplaylist-enabled: $b_XMLPLAYLIST
  yojson-enabled: $b_YOJSON
}
EOCFG
