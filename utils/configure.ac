# Process this file with autoconf to produce a configure script.
AC_INIT
AC_CONFIG_SRCDIR([src/gmk_sym.c])
AC_PREREQ(2.54)

PACKAGE=geda-utils
DOTTED_VERSION=1.1
DATE_VERSION=20070526
echo Configuring $PACKAGE version $DOTTED_VERSION.$DATE_VERSION

# Initialize automake 
AM_INIT_AUTOMAKE($PACKAGE, $DOTTED_VERSION, no-define)
AM_CONFIG_HEADER([config.h])

# Initialize maintainer mode
AM_MAINTAINER_MODE


#########################################################################
# Command line flags start
# 
# Change default location for rc files
AC_ARG_WITH(rcdir, [  --with-rcdir=path       Change where the system-*rc files are installed], [opt_rcdir=$withval])

# --with-docdir : tells where to store documentation if not default
# defined below 
# 
# Command line flags end
#########################################################################

# Checks for programs.
AC_PROG_CC
AM_PROG_CC_C_O
AM_CONDITIONAL(CCISGCC, test "$GCC" = "yes")
AC_PROG_CPP
AC_PROG_MAKE_SET
AM_PROG_LEX

AC_PATH_PROG(PERL, perl, [notfound])
AM_CONDITIONAL(MISSING_PERL, test x$PERL = xnotfound)

##############################################################3
# Test for __func__
#

AC_MSG_CHECKING([For a working C99 __func__])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],
        [[const char *foo = __func__;]])],
   [ac_cv_cpp_func=yes],
   [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],
                [[const char *foo = __FUNCTION__;]])],
                [ac_cv_cpp_func=__FUNCTION__],
                [ac_cv_cpp_func="\"no\""])]
)

if test "X$ac_cv_cpp_func" = "X__FUNCTION__"; then
        AC_MSG_RESULT([__FUNCTION__])
elif test "x$ac_cv_cpp_func" = "xyes" ; then
        AC_MSG_RESULT([__func__])
else
        AC_MSG_RESULT([no])
fi
if test "X$ac_cv_cpp_func" != "Xyes" ; then
        AC_DEFINE_UNQUOTED([__func__], [$ac_cv_cpp_func], [Define to be a __func__ replacement])
fi

#
##############################################################3

#########################################################################
# 
# Misc win32 / mingw checks and variables start
AC_CANONICAL_HOST

# Figure out if we are building on win32 and what environment.
case $host_os in
  *mingw32* ) echo "Configuring for mingw"; MINGW=yes ;;
esac

if ! test "$MINGW" = "no" -o "$MINGW"x = x; then
   MINGW_CFLAGS="-mms-bitfields -mwindows"
   MINGW_LIBS="-lregex -liberty"
   MINGW="yes"
   PATHSEP=\\\\
   OTHERPATHSEP=/
else
   # Unix host
   MINGW_CFLAGS=
   MINGW_LIBS=
   MINGW="no"
   PATHSEP=/
   OTHERPATHSEP=\\\\
fi

# 
# Misc win32 / mingw checks and variables end
#########################################################################

############################################################################
# Check for mics things start
# 
# Checking for rint in math library
AC_CHECK_LIB(m, rint, AC_DEFINE(HAS_RINT, 1, [If your math library has rint in it, define this]), no_RINT="yes")

# Checking for dynamic lib
AC_CHECK_LIB(dl, dlopen, DL_LIB="-ldl", DL_LIB="")
#
# Check for mics things start
############################################################################

############################################################################
# Check for libgeda start
# 
PKG_CHECK_MODULES(LIBGEDA, libgeda >= $DATE_VERSION, LIBGEDA="yes", 
                  no_LIBGEDA="yes")

if test "$LIBGEDA" = "yes" 
then
   LIBGEDA_VERSION=`$PKG_CONFIG libgeda --modversion`
else
   AC_MSG_ERROR([libgeda detection error: $LIBGEDA_PKG_ERRORS])
fi

# 
# Check for libgeda end
############################################################################

############################################################################
# Check for gtk+ 2.4 start
# 

# Check for pkg-config
AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
if test $PKG_CONFIG = no; then
   AC_MSG_ERROR([Cannot find pkg-config, make sure it is installed and in your PATH])
fi

PKG_CHECK_MODULES(GTK24, gtk+-2.0 >= 2.4.0, GTK24="yes", no_GTK24="yes")

# This next bit of code figures out what gtk we need to use.
if test "$GTK24" = "yes"
then

   AC_DEFINE(HAS_GTK24, 1, [If gtk+ 2.4.x has been installed, define this])
   GTK_CFLAGS=$GTK24_CFLAGS
   GTK_LIBS=$GTK24_LIBS
   GTK_VERSION=`$PKG_CONFIG gtk+-2.0 --modversion`

   # Search for glib
   PKG_CHECK_MODULES(GLIB24, glib-2.0 >= 2.4.0, GLIB24="yes", no_GLIB24="yes")
   if test "$GLIB24" != "yes"
   then
       AC_MSG_ERROR([Cannot find glib 2.4.x, install it and rerun ./configure.])
   fi
   GLIB_CFLAGS=$GLIB24_CFLAGS
   GLIB_LIBS=$GLIB24_LIBS
   GLIB_VERSION=`$PKG_CONFIG glib-2.0 --modversion`

else
   GTK_VERSION=""
fi

if test "$GTK_VERSION" = ""
then
   AC_MSG_ERROR([Cannot find gtk+ 2.4.x or later, please install gtk+.])
fi

# 
# Check for gtk+ 2.4 end
############################################################################

############################################################################
# Check for glib 2.4 start
# 

# Check for pkg-config
AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
if test $PKG_CONFIG = no; then
   AC_MSG_ERROR([Cannot find pkg-config, make sure it is installed and in your PATH])
fi

# Search for glib 2.4.x first (only if we are not forcing the glib to 1.2)
if test "$opt_gtkver" != "1.2"
then
   PKG_CHECK_MODULES(GLIB24, glib-2.0 >= 2.4.0, GLIB24="yes", no_GLIB24="yes")
fi

# This next bit of code figures out what glib we need to use.
if test "$GLIB24" = "yes" -a "$opt_gtkver" != "1.2"
then
   GLIB_CFLAGS=$GLIB24_CFLAGS
   GLIB_LIBS=$GLIB24_LIBS
   GLIB_VERSION=`$PKG_CONFIG glib-2.0 --modversion`
else
   GLIB_VERSION=""
fi

if test "$GLIB_VERSION" = ""
then
   AC_MSG_ERROR([Cannot find glib 2.4.x, install one of them.])
fi

# 
# Check for glib 2.4 end
############################################################################

#########################################################################
# Checks for header files start
# 
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_HEADER_DIRENT
AC_CHECK_HEADERS(unistd.h string.h stdlib.h \
                 stdarg.h assert.h fcntl.h errno.h \
                 getopt.h)

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# Checks for library functions.
AC_TYPE_SIGNAL
AC_CHECK_FUNCS(getcwd strstr vsnprintf snprintf getopt_long)
AC_CHECK_FUNCS(strcasecmp strncasecmp stricmp strnicmp)

AC_MSG_CHECKING([for optarg in unistd.h])
AC_TRY_COMPILE(
[#include <unistd.h>],
[ char *string = optarg; int i = optind; ],
optarg_found=yes,
optarg_found=no)
AC_MSG_RESULT($optarg_found)

if test $optarg_found = yes; then
    AC_DEFINE(OPTARG_IN_UNISTD, 1, [Define if you have optarg in unistd.h])
fi

# 
# Checks for header files start
#########################################################################

#########################################################################
#

# ------------- dmalloc -------------------
dnl dmalloc checks
with_dmalloc="no"
AC_MSG_CHECKING([if dmalloc debugging should be enabled])
AC_ARG_ENABLE([dmalloc],
[  --enable-dmalloc        Compile and link with dmalloc for malloc debugging [[default=no]]],
[
if test "X$enable_dmalloc" != "Xno" ; then
        AC_MSG_RESULT([yes])
        AC_CHECK_HEADER(dmalloc.h,,
                AC_ERROR([You have requested dmalloc debugging but dmalloc.h could not be found]))
        AC_CHECK_LIB(dmalloc,main,,
                AC_ERROR([You have requested dmalloc debugging but -ldmalloc could not be found]))
        DMALLOC_LIBS="-ldmalloc"
        with_dmalloc="yes"
else
        AC_MSG_RESULT([no])
        DMALLOC_LIBS=""
fi
],
[
        AC_MSG_RESULT([no])
        DMALLOC_LIBS=""
])

# ------------- ElectricFence -------------------
dnl ElectricFence checks
with_efence="no"
AC_MSG_CHECKING([if ElectricFence debugging should be enabled])
AC_ARG_ENABLE([efence],
[  --enable-efence         Link with ElectricFence for malloc debugging [[default=no]]],
[
if test "X$enable_efence" != "Xno" ; then
        AC_MSG_RESULT([yes])
	if test "X$with_dmalloc" = "Xyes" ; then
		AC_ERROR([You have requested both dmalloc and Electric Fence, however only 1 is allowed.])
	fi
        AC_CHECK_LIB(efence,main,,
                AC_ERROR([You have requested ElectricFence debugging but -lefence could not be found]))
        with_efence="yes"
else
        AC_MSG_RESULT([no])
fi
],
[
AC_MSG_RESULT([no])
])

#
#########################################################################

#########################################################################
#
# Change default location for pcb's newlib library directory
#
# Used by gsch2pcb

AC_MSG_CHECKING([For the default pcb installation data directory])
AC_ARG_WITH(pcb-datadir,
	[  --with-pcb-datadir=path
                          Change the default location for an installed pcb.  This is used
                          to set the default search path for pcb newlib libraries.
                          [[default=${datadir}]]],
	[PCBDATADIR=$withval],
	[PCBDATADIR="${datadir}"]
)
AC_MSG_RESULT([${PCBDATADIR}])
CPPFLAGS="$CPPFLAGS -DPCBDATADIR=\\\"${PCBDATADIR}\\\""

AC_MSG_CHECKING([For the default pcb newlib search path])
AC_ARG_WITH(pcb-newlib-path,
	[  --with-pcb-newlib-path=path
                          Change the default search path for PCB newlib libraries
                          [[default=${datadir}/pcb/pcblib-newlib:${datadir}/pcb/newlib]]],
	[PCBLIBPATH=$withval],
	[PCBLIBPATH="${PCBDATADIR}/pcb/pcblib-newlib:${PCBDATADIR}/pcb/newlib"]
)
AC_MSG_RESULT([${PCBLIBPATH}])
CPPFLAGS="$CPPFLAGS -DPCBLIBPATH=\\\"${PCBLIBPATH}\\\""

#
#########################################################################

#########################################################################
# Check for groff start
#

# search for groff
AC_PATH_PROG(GROFF, groff, no, ${PATH})
if test $GROFF = "no"; then
        echo "Cannot find groff, some documentation will not be created."
fi

# 
# Check for groff end
#########################################################################

#########################################################################
# gEDA/gaf specify setup start
#

DATADIR=gEDA
GEDADATADIR=$datadir/$DATADIR

if eval "test x$opt_rcdir = x"; then
	# path not was specified with --with-rcdir
        AC_DEFINE_UNQUOTED(GEDARCDIR, "none", [gEDA/gaf's rc directory])
	GEDARCDIR=$GEDADATADIR
else
	# path WAS specified with --with-rcdir
        AC_DEFINE_UNQUOTED(GEDARCDIR, "$opt_rcdir", [gEDA/gaf's rc directory])
	GEDARCDIR="$opt_rcdir"
fi

# Expand the prefix variable
# I don't like the way this is done, but it works (I hope).
if eval "test x$prefix = xNONE"; then
 dprefix=$ac_default_prefix
else
 dprefix=$prefix
fi

gedatopdir=$dprefix/share/$DATADIR
expandgedadatadir=`echo $gedatopdir`

gedadocdir=$dprefix/share/doc/geda-doc
# --with-docdir : tells where to store documentation if not default
AC_ARG_WITH(docdir,
    [  --with-docdir      Where to store documentation if not default.],
	gedadocdir=$withval)
expandgedadocdir=`echo $gedadocdir`
GEDADOCDIR=$expandgedadocdir

# this has to be expanded ( no ${prefix} ) --
AC_DEFINE_UNQUOTED(GEDADATADIR, "$gedatopdir", [gEDA/gaf's data directory])
AC_DEFINE_UNQUOTED(DATE_VERSION, "$DATE_VERSION", [Currently running date version of gEDA/gaf])
AC_DEFINE_UNQUOTED(DOTTED_VERSION, "$DOTTED_VERSION", [Currently running dotted version of gEDA/gaf])
AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE", [Name of this program's package])

#
# gEDA/gaf specify things which need to setup
#########################################################################

# Finally create the final CFLAGS and LDFLAGS for use in the makefiles
GSCHLAS_CFLAGS="$LIBGEDA_CFLAGS"
GSCHLAS_LDFLAGS="$LIBGEDA_LIBS $GTK_LIBS"  
GSCH2PCB_CFLAGS="$GLIB_CFLAGS $MINGW_CFLAGS"
GSCH2PCB_LDFLAGS="$GLIB_LIBS $MINGW_LIBS"  
GRENUM_CFLAGS="$LIBGEDA_CFLAGS"
GRENUM_LDFLAGS="$GLIB_LIBS"  

# Makefile.in variable substitution
AC_SUBST(DATE_VERSION)
AC_SUBST(DOTTED_VERSION)
AC_SUBST(GSCHLAS_CFLAGS)
AC_SUBST(GSCHLAS_LDFLAGS)
AC_SUBST(GSCH2PCB_CFLAGS)
AC_SUBST(GSCH2PCB_LDFLAGS)
AC_SUBST(GRENUM_CFLAGS)
AC_SUBST(GRENUM_LDFLAGS)
AC_SUBST(MINGW_CFLAGS)
AC_SUBST(MINGW_LIBS)
AC_SUBST(GEDARCDIR)
AC_SUBST(GEDADATADIR)
AC_SUBST(GEDADOCDIR)
AC_SUBST(PATHSEP)
AC_SUBST(OTHERPATHSEP)

# Create all the necessary derived files
AC_CONFIG_FILES([Makefile 
                 examples/Makefile 
                 examples/gmk_sym/Makefile 
                 examples/tragesym/Makefile 
                 examples/gsch2pcb/Makefile 
                 include/Makefile 
                 scripts/Makefile 
                 docs/Makefile 
                 man/Makefile 
                 lib/Makefile 
                 lib/system-gschlasrc 
                 gschlas/Makefile 
                 src/Makefile
                 tests/Makefile
                 tests/refdes_renum/Makefile
                 tests/refdes_renum/inputs/Makefile
                 tests/refdes_renum/outputs/Makefile
])

AC_OUTPUT

expandedGEDADATADIR=`eval "echo $GEDADATADIR"`
expandedGEDARCDIR=`eval "echo $GEDARCDIR"`
expandedGEDADOCDIR=`eval "echo $GEDADOCDIR"`
expandedPCBLIBPATH=`eval "echo $PCBLIBPATH"`

AC_MSG_RESULT([
** Configuration summary for $PACKAGE $DOTTED_VERSION.$DATE_VERSION:

   GTK+ library version:             $GTK_VERSION
   GLIB library version:             $GLIB_VERSION
   libgeda library version:          $LIBGEDA_VERSION
   mingw build:                      $MINGW
   data directory:                   $expandedGEDADATADIR
   rc directory:                     $expandedGEDARCDIR
   documentation directory:          $expandedGEDADOCDIR
   default pcb newlib path:          $expandedPCBLIBPATH
   dmalloc debugging:                $with_dmalloc
   Electric Fence debugging:         $with_efence
])

