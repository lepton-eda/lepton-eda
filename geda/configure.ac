# Process this file with autoconf to produce a configure script.
AC_INIT(configure.ac)

PACKAGE=geda
VERSION=20050313
echo Configuring $PACKAGE version $VERSION

# Initialize automake 
AM_INIT_AUTOMAKE($PACKAGE, $VERSION)
AM_CONFIG_HEADER([config.h])

#########################################################################
# Command line flags start
# 

# Force a specific version of gtk+          
AC_ARG_WITH(gtk12, [  --with-gtk12            Force the use of gtk+ 1.2.x], [opt_gtkver=1.2])

# 
# Command line flags end
#########################################################################

#########################################################################
# 
# Misc win32 / mingw checks and variables start

# Figure out if we are building on win32 and what environment.
case $host_os in
  *cygwin*  ) AC_MSG_ERROR([Building gEDA/gaf under cygwin is not supported]) ;;
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

# Checks for programs.
AC_PROG_CC
AM_CONDITIONAL(CCISGCC, test "$GCC" = "yes")    
AC_PROG_CPP
AC_PROG_MAKE_SET

AC_ISC_POSIX
AC_PROG_CC
AM_PROG_CC_STDC
AC_HEADER_STDC

# AM_PATH_GTK(1.2.0, ,
#             AC_MSG_ERROR(Cannot find GTK: Is gtk-config in path?))

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
# Check for X11 start
# 

if test "$MINGW" = "no"
then
   AC_PATH_X
   AC_PATH_XTRA 
    
   X_EXTRA_LIBS="$X_EXTRA_LIBS"
   X_LDFLAGS="$X_LDFLAGS $X_LIBS $X_EXTRA_LIBS $X_PRE_LIBS -lX11 -lm"
fi

# 
# Check for X11 end
############################################################################

############################################################################
# Check for gtk+ 1.2 and 2.2 start
# 

# Check for pkg-config
AC_PATH_PROG(PKG_CONFIG, pkg-config, no)
if test $PKG_CONFIG = no; then
   AC_MSG_ERROR([Cannot find pkg-config, make sure it is installed and in your PATH])
fi

# Search for gtk+ 2.2.x first (only if we are not forcing the gtk+ to 1.2)
if test "$opt_gtkver" != "1.2"
then
   PKG_CHECK_MODULES(GTK22, gtk+-2.0 >= 2.2.0, GTK22="yes", no_GTK22="yes")
fi

# This next bit of code figures out what gtk we need to use.
if test "$GTK22" = "yes" -a "$opt_gtkver" != "1.2"
then

   AC_DEFINE(HAS_GTK22, 1, [If gtk+ 2.2.x has been installed, define this])
   GTK_CFLAGS=$GTK22_CFLAGS
   GTK_LIBS=$GTK22_LIBS
   GTK_VERSION=`$PKG_CONFIG gtk+-2.0 --modversion`

   # Search for glib
   PKG_CHECK_MODULES(GLIB22, glib-2.0 >= 2.2.0, GLIB22="yes", no_GLIB22="yes")
   if test "$GLIB22" != "yes"
   then
       AC_MSG_ERROR([Cannot find glib 2.2.x, install it and rerun ./configure.])
   fi
   GLIB_CFLAGS=$GLIB22_CFLAGS
   GLIB_LIBS=$GLIB22_LIBS
   GLIB_VERSION=`$PKG_CONFIG glib-2.0 --modversion`

else

   # Search for gtk+ 1.2 now 
   PKG_CHECK_MODULES(GTK12, gtk+ >= 1.2.3, GTK12="yes", no_GTK12="yes")

   AC_DEFINE(HAS_GTK12, 1, [If gtk+ 1.2.x has been installed, define this])
   GTK_CFLAGS=$GTK12_CFLAGS
   GTK_LIBS=$GTK12_LIBS
   GTK_VERSION=`$PKG_CONFIG gtk+ --modversion`

   # Search for glib
   PKG_CHECK_MODULES(GLIB12, glib >= 1.2.3, GLIB12="yes", no_GLIB12="yes")
   if test "$GLIB12" != "yes"
   then
       AC_MSG_ERROR([Cannot find glib >= 1.2.3, install it and rerun ./configure.])
   fi
   GLIB_CFLAGS=$GLIB12_CFLAGS
   GLIB_LIBS=$GLIB12_LIBS
   GLIB_VERSION=`$PKG_CONFIG glib --modversion`
fi

if test "$GTK_VERSION" = ""
then
   AC_MSG_ERROR([Cannot find gtk+ 2.2.x or gtk+ 1.2.x, install one of them.])
fi

# 
# Check for gtk+ 1.2 and 2.2 end
############################################################################

#########################################################################
# Checks for header files start
# 

AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_HEADER_DIRENT
AC_CHECK_HEADERS(unistd.h string.h stdlib.h stdarg.h assert.h fcntl.h \
                 errno.h)

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

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# Checks for library functions.
AC_TYPE_SIGNAL
AC_CHECK_FUNCS(getcwd strstr vsnprintf snprintf)

# 
# Checks for header files end
#########################################################################

#########################################################################
# gEDA/gaf specify setup start
#

# Defs for libary symbol files
#
# Expand the prefix variable
if eval "test x$prefix = xNONE"; then
 dprefix=$ac_default_prefix
else
 dprefix=$prefix
fi

DATADIR=gEDA
GEDADATADIR=$datadir/$DATADIR

gedatopdir=$dprefix/share/$DATADIR
expandgedadatadir=`echo $gedatopdir`

# this has to be expanded ( no ${prefix} ) --
AC_DEFINE_UNQUOTED(GEDADATADIR, "$gedatopdir", [gEDA/gaf's data directory])
AC_DEFINE_UNQUOTED(VERSION, "$VERSION", [Currently running version of gEDA/gaf])

#
# gEDA/gaf specify things which need to setup
#########################################################################

# Finally create the final CFLAGS and LDFLAGS for use in the makefiles
GMANAGER_CFLAGS="$MINGW_CFLAGS $GTK_CFLAGS $X_CFLAGS"
GMANAGER_LDFLAGS="$MINGW_LIBS $GLIB_LIBS"
AC_SUBST(GMANAGER_CFLAGS)
AC_SUBST(GMANAGER_LDFLAGS)
AC_SUBST(GTK_LIBS)

dnl Checks for programs.
dnl Checks for libraries.
dnl Checks for header files.
AC_CHECK_HEADERS(time.h)

dnl Checks for typedefs, structures, and compiler characteristics.
dnl Checks for library functions.
AC_CHECK_FUNCS(nanosleep)

dnl Checks for Additional stuffs.

dnl Languages which your application supports

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

ALL_LINGUAS=""
AM_GNU_GETTEXT
AM_GNU_GETTEXT_VERSION(0.12.1)

dnl Set PACKAGE_LOCALE_DIR in config.h.
if test "x${prefix}" = "xNONE"; then
  AC_DEFINE_UNQUOTED(PACKAGE_LOCALE_DIR, "${ac_default_prefix}/${DATADIRNAME}/locale", [Package locale directory])
else
  AC_DEFINE_UNQUOTED(PACKAGE_LOCALE_DIR, "${prefix}/${DATADIRNAME}/locale", [Package locale directory])
fi


dnl Set PACKAGE SOURCE DIR in config.h.
packagesrcdir=`cd $srcdir && pwd`

dnl Set PACKAGE PREFIX
if test "x${prefix}" = "xNONE"; then
  packageprefix=${ac_default_prefix}
else
  packageprefix=${prefix}
fi

dnl Set PACKAGE DATA & DOC DIR
packagedatadir=share/${PACKAGE}
packagedocdir=doc/${PACKAGE}

dnl Set PACKAGE DIRS in config.h.
packagepixmapsdir=${packagedatadir}/pixmaps
packagehelpdir=${packagedatadir}/help
packagemenudir=${packagedatadir}

dnl Subst PACKAGE_DATA_DIR.
NO_PREFIX_PACKAGE_DATA_DIR="${packagedatadir}"
AC_SUBST(NO_PREFIX_PACKAGE_DATA_DIR)
PACKAGE_DATA_DIR="${packageprefix}/${packagedatadir}"
AC_SUBST(PACKAGE_DATA_DIR)

dnl Subst PACKAGE_DOC_DIR.
NO_PREFIX_PACKAGE_DOC_DIR="${packagedocdir}"
AC_SUBST(NO_PREFIX_PACKAGE_DOC_DIR)
PACKAGE_DOC_DIR="${packageprefix}/${packagedocdir}"
AC_SUBST(PACKAGE_DOC_DIR)

dnl Subst PACKAGE_PIXMAPS_DIR.
NO_PREFIX_PACKAGE_PIXMAPS_DIR="${packagepixmapsdir}"
AC_SUBST(NO_PREFIX_PACKAGE_PIXMAPS_DIR)
PACKAGE_PIXMAPS_DIR="${packageprefix}/${packagepixmapsdir}"
AC_SUBST(PACKAGE_PIXMAPS_DIR)

dnl Subst PACKAGE_HELP_DIR.
NO_PREFIX_PACKAGE_HELP_DIR="${packagehelpdir}"
AC_SUBST(NO_PREFIX_PACKAGE_HELP_DIR)
PACKAGE_HELP_DIR="${packageprefix}/${packagehelpdir}"
AC_SUBST(PACKAGE_HELP_DIR)

dnl Subst PACKAGE_MENU_DIR.
NO_PREFIX_PACKAGE_MENU_DIR="${packagemenudir}"
AC_SUBST(NO_PREFIX_PACKAGE_MENU_DIR)
PACKAGE_MENU_DIR="${packageprefix}/${packagemenudir}"
AC_SUBST(PACKAGE_MENU_DIR)

AC_DEFINE_UNQUOTED(PACKAGE_DATA_DIR, "${packageprefix}/${packagedatadir}", [Package data directory])
AC_DEFINE_UNQUOTED(PACKAGE_DOC_DIR, "${packageprefix}/${packagedocdir}", [Package doc directory])
AC_DEFINE_UNQUOTED(PACKAGE_PIXMAPS_DIR, "${packageprefix}/${packagepixmapsdir}", [Package pixmap directory])
AC_DEFINE_UNQUOTED(PACKAGE_HELP_DIR, "${packageprefix}/${packagehelpdir}", [Package help directory])
AC_DEFINE_UNQUOTED(PACKAGE_MENU_DIR, "${packageprefix}/${packagemenudir}", [Package menu directroy])
AC_DEFINE_UNQUOTED(PACKAGE_SOURCE_DIR, "${packagesrcdir}", [Package source directory])

AC_OUTPUT([ Makefile 
	 m4/Makefile 
	 intl/Makefile 
	 po/Makefile.in 
         include/Makefile
	 src/Makefile 
	 pixmaps/Makefile 
	 graphman/Makefile ])

AC_MSG_RESULT([
** Configuration summary for $PACKAGE $VERSION:

   GLIB library version:             $GLIB_VERSION
   GTK+ library version:             $GTK_VERSION
   mingw build:                      $MINGW
   dmalloc debugging:                $with_dmalloc
   ElectricFence debugging:          $with_efence
])
