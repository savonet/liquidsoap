AC_DEFUN([AC_BASE_CHECKS],
[AC_REQUIRE([AC_PROG_CC])

dnl check for base compilers
AC_CANONICAL_HOST()

dnl Detect the target toolchain
AC_MSG_CHECKING([target toolchain])
case "${host_os}" in
  linux*)
    TARGET_TOOLCHAIN="linux"
    ;;
  mingw*)
    TARGET_TOOLCHAIN="mingw"
    ;;
  cygwin*)
    TARGET_TOOLCHAIN="cygwin"
    ;;
  darwin*)
    TARGET_TOOLCHAIN="darwin"
    ;;
  *)
    TARGET_TOOLCHAIN="other"
    ;;
esac
AC_MSG_RESULT([$TARGET_TOOLCHAIN])
AC_SUBST(TARGET_TOOLCHAIN)

# AC_CANONICAL_HOST needs those files
AUTOCONF_INSTALL_FILES="config.guess config.sub install-sh m4/*.m4"
AC_SUBST(AUTOCONF_INSTALL_FILES)

AC_PROG_CC()
AC_PROG_INSTALL()
AC_CHECK_TOOL([AR],[ar],no)
AC_SUBST(AR)
AC_CHECK_OCAML_COMPILERS()

dnl add some flags
AC_DETECT_PIC_FLAGS()

CXXFLAGS="$CXXFLAGS $PIC_FLAGS"
CPPFLAGS="$CPPFLAGS $PIC_FLAGS"

# Add prefix to compilation variables
# if passed
if test "x$prefix" != "xNONE"; then
  CFLAGS="$CFLAGS -I$prefix/include"
  LDFLAGS="$LDFLAGS -L$prefix/lib"
  CPPFLAGS="$CPPFLAGS -I$prefix/include"
  CXXFLAGS="$CXXFLAGS -I$prefix/include"
fi
])

dnl Check for basic stuff
dnl The following was stolen from mesa..
dnl A few convenience macros for Mesa, mostly to keep all the platform
dnl specifics out of configure.ac.

dnl AC_DETECT_PIC_FLAGS()
dnl
dnl Find out whether to build PIC code using the option --enable-pic and
dnl the configure enable_static/enable_shared settings. If PIC is needed,
dnl figure out the necessary flags for the platform and compiler.
dnl
dnl The platform checks have been shamelessly taken from libtool and
dnl stripped down to just what's needed for Mesa. See _LT_COMPILER_PIC in
dnl /usr/share/aclocal/libtool.m4 or
dnl http://git.savannah.gnu.org/gitweb/?p=libtool.git;a=blob;f=libltdl/m4/libtool.m4;hb=HEAD
dnl
AC_DEFUN([AC_DETECT_PIC_FLAGS],
[AC_ARG_VAR([PIC_FLAGS], [compiler flags for PIC code])
AC_ARG_ENABLE([pic],
    [AS_HELP_STRING([--disable-pic],
        [compile PIC objects @<:@default=enabled for shared builds
        on supported platforms@:>@])],
    [enable_pic="$enableval"
    test "x$enable_pic" = x && enable_pic=auto],
    [enable_pic=auto])
dnl disable PIC by default for static builds
if test "$enable_pic" = auto && test "$enable_static" = yes; then
    enable_pic=no
fi
dnl if PIC hasn't been explicitly disabled, try to figure out the flags
if test "$enable_pic" != no; then
    AC_MSG_CHECKING([for $CC option to produce PIC])
    dnl allow the user's flags to override
    if test "x$PIC_FLAGS" = "x"; then
        dnl see if we're using GCC
        if test "x$GCC" = "xyes"; then
            case "$host_os" in
            aix*|beos*|cygwin*|irix5*|irix6*|osf3*|osf4*|osf5*)
                dnl PIC is the default for these OSes.
                ;;
            mingw*|os2*|pw32*)
                dnl This hack is so that the source file can tell whether
                dnl it is being built for inclusion in a dll (and should
                dnl export symbols for example).
                PIC_FLAGS="-DDLL_EXPORT"
                ;;
            darwin*|rhapsody*)
                dnl PIC is the default on this platform
                dnl Common symbols not allowed in MH_DYLIB files
                PIC_FLAGS="-fno-common"
                ;;
            hpux*)
                dnl PIC is the default for IA64 HP-UX and 64-bit HP-UX,
                dnl but not for PA HP-UX.
                case $host_cpu in
                hppa*64*|ia64*)
                    ;;
                *)
                    PIC_FLAGS="-fPIC"
                    ;;
                esac
                ;;
            *)
                dnl Everyone else on GCC uses -fPIC
                PIC_FLAGS="-fPIC"
                ;;
            esac
        else dnl !GCC
            case "$host_os" in
            hpux9*|hpux10*|hpux11*)
                dnl PIC is the default for IA64 HP-UX and 64-bit HP-UX,
                dnl but not for PA HP-UX.
                case "$host_cpu" in
                hppa*64*|ia64*)
                    dnl +Z the default
                    ;;
                *)
                    PIC_FLAGS="+Z"
                    ;;
                esac
                ;;
            linux*|k*bsd*-gnu)
                case `basename "$CC"` in
                icc*|ecc*|ifort*)
                    PIC_FLAGS="-KPIC"
                    ;;
                pgcc*|pgf77*|pgf90*|pgf95*)
                    dnl Portland Group compilers (*not* the Pentium gcc
                    dnl compiler, which looks to be a dead project)
                    PIC_FLAGS="-fpic"
                    ;;
                ccc*)
                    dnl All Alpha code is PIC.
                    ;;
                xl*)
                    dnl IBM XL C 8.0/Fortran 10.1 on PPC
                    PIC_FLAGS="-qpic"
                    ;;
                *)
                    case `$CC -V 2>&1 | sed 5q` in
                    *Sun\ C*|*Sun\ F*)
                        dnl Sun C 5.9 or Sun Fortran
                        PIC_FLAGS="-KPIC"
                        ;;
                    esac
                esac
                ;;
            solaris*)
                PIC_FLAGS="-KPIC"
                ;;
            sunos4*)
                PIC_FLAGS="-PIC"
                ;;
            esac
        fi
    fi 
    AC_MSG_RESULT([$PIC_FLAGS])
fi
AC_SUBST([PIC_FLAGS])
])dnl PIC_FLAGS

