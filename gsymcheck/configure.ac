# Process this file with autoconf to produce a configure script.
AC_INIT
AC_CONFIG_SRCDIR([src/gsymcheck.c])
AC_PREREQ(2.54)

PACKAGE=geda-gsymcheck
DOTTED_VERSION=1.1.1
DATE_VERSION=20070708
echo Configuring $PACKAGE version $DOTTED_VERSION.$DATE_VERSION

# Initialize automake 
AM_INIT_AUTOMAKE($PACKAGE, $DOTTED_VERSION, no-define)
AM_CONFIG_HEADER([config.h])

#########################################################################
# Command line flags start
# 
# Change default location for rc files
AC_ARG_WITH(rcdir, [  --with-rcdir=path       Change where the system-*rc files are installed], [opt_rcdir=$withval])

# 
# Command line flags end
#########################################################################

# Checks for programs.
AC_PROG_CC
AM_CONDITIONAL(CCISGCC, test "$GCC" = "yes")    
AC_PROG_CPP
AC_PROG_MAKE_SET

#########################################################################
# 
# Misc win32 / mingw checks and variables start
AC_CANONICAL_HOST

# Figure out if we are building on win32 and what environment.
case $host_os in
  *mingw32* ) echo "Configuring for mingw"; MINGW=yes ;;
esac

if ! test "$MINGW" = "no" -o "$MINGW"x = x; then
   MINGW="yes"
   PATHSEP=\\\\
   OTHERPATHSEP=/
else
   # Unix host
   MINGW="no"
   PATHSEP=/
   OTHERPATHSEP=\\\\
fi

# 
# Misc win32 / mingw checks and variables end
#########################################################################

############################################################################
# Check for guile start
GUILE_FLAGS

# Check Guile version
guile_need_major=1
guile_need_minor=6
guile_need_version="$guile_need_major[].$guile_need_minor.0"

AC_MSG_CHECKING([Guile version >= $guile_need_version])
GUILE_VERSION=`$GUILE_CONFIG info guileversion`

guile_major=`echo "$GUILE_VERSION" | sed 's/\([[^.]][[^.]]*\).*/\1/'`
guile_minor=`echo "$GUILE_VERSION" | sed 's/[[^.]][[^.]]*.\([[^.]][[^.]]*\).*/\1/'`
AC_MSG_RESULT($GUILE_VERSION)

if test $guile_need_major -gt $guile_major \
   || test $guile_need_minor -gt $guile_minor; then
  AC_MSG_ERROR([Guile version >= $guile_need_version is required.])
fi

# Guile 1.6 compatability

AC_CHECK_DECLS([scm_is_string, scm_is_integer, scm_to_int,
scm_from_int,  scm_is_true,    scm_is_false,
scm_from_locale_string, scm_to_locale_string],,,
[#include <libguile.h>])

AH_VERBATIM(SCM_IS_STRING, [#if !HAVE_DECL_SCM_IS_STRING
#  define scm_is_string(x) SCM_STRINGP(x)
#endif])
AH_VERBATIM(SCM_IS_INTEGER, [#if !HAVE_DECL_SCM_IS_INTEGER
#  define scm_is_integer(x) SCM_INUMP(x)
#endif])
AH_VERBATIM(SCM_TO_INT, [#if !HAVE_DECL_SCM_TO_INT
#  define scm_to_int(x)     SCM_INUM(x)
#endif])
AH_VERBATIM(SCM_FROM_INT, [#if !HAVE_DECL_SCM_FROM_INT
#  define scm_from_int(x)   SCM_MAKINUM(x)
#endif])
AH_VERBATIM(SCM_IS_TRUE, [#if !HAVE_DECL_SCM_IS_TRUE
#  define scm_is_true(x)    SCM_NFALSEP(x)
#endif])
AH_VERBATIM(SCM_IS_FALSE, [#if !HAVE_DECL_SCM_IS_FALSE
#  define scm_is_false(x)   SCM_FALSEP(x)
#endif])
AH_VERBATIM(SCM_FROM_LOCALE_STRING, [#if !HAVE_DECL_SCM_FROM_LOCALE_STRING
#  define scm_from_locale_string(x) scm_makfrom0str(x)
#endif])
AH_VERBATIM(SCM_TO_LOCALE_STRING, [#if !HAVE_DECL_SCM_TO_LOCALE_STRING
#  define scm_to_locale_string(x)   strdup(SCM_STRING_CHARS(x))
#endif])

#
# Check for guile end
############################################################################

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

#########################################################################
# Checks for header files start
# 
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_HEADER_DIRENT
AC_CHECK_HEADERS(unistd.h string.h stdlib.h \
                 stdarg.h assert.h fcntl.h errno.h)

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# Checks for library functions.
AC_TYPE_SIGNAL
AC_CHECK_FUNCS(getcwd strstr vsnprintf snprintf)

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
AC_SUBST(GEDADOCDIR)

# this has to be expanded ( no ${prefix} ) --
AC_DEFINE_UNQUOTED(GEDADATADIR, "$gedatopdir", [gEDA/gaf's data directory])
AC_DEFINE_UNQUOTED(DATE_VERSION, "$DATE_VERSION", [Currently running date version of gEDA/gaf])
AC_DEFINE_UNQUOTED(DOTTED_VERSION, "$DOTTED_VERSION", [Currently running dotted version of gEDA/gaf])
AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE", [Name of this program's package])

#
# gEDA/gaf specify things which need to setup
#########################################################################

# Finally create the final CFLAGS and LDFLAGS for use in the makefiles
GSYMCHECK_CFLAGS="$LIBGEDA_CFLAGS"
GSYMCHECK_LDFLAGS="$LIBGEDA_LIBS"  

# Makefile.in variable substitution
AC_SUBST(DATE_VERSION)
AC_SUBST(DOTTED_VERSION)
AC_SUBST(GSYMCHECK_CFLAGS)
AC_SUBST(GSYMCHECK_LDFLAGS)
AC_SUBST(GEDARCDIR)
AC_SUBST(GEDADATADIR)
AC_SUBST(PATHSEP)
AC_SUBST(OTHERPATHSEP)

# Create all the necessary derived files
AC_CONFIG_FILES([Makefile 
		 src/Makefile 
		 include/Makefile 
		 scheme/Makefile 
		 examples/Makefile 
		 lib/Makefile 
		 utils/Makefile
		 docs/Makefile
		 tests/Makefile
		 lib/system-gsymcheckrc ])

AC_OUTPUT

expandedGEDADATADIR=`eval "echo $GEDADATADIR"`
expandedGEDARCDIR=`eval "echo $GEDARCDIR"`
expandedGEDADOCDIR=`eval "echo $GEDADOCDIR"`

AC_MSG_RESULT([
** Configuration summary for $PACKAGE $DOTTED_VERSION.$DATE_VERSION:

   GTK+ library version:             $GTK_VERSION
   libgeda library version:          $LIBGEDA_VERSION
   mingw build:                      $MINGW
   data directory:                   $expandedGEDADATADIR
   rc directory:                     $expandedGEDARCDIR
   documentation directory:          $expandedGEDADOCDIR
])

