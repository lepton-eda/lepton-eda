# Process this file with autoconf to produce a configure script.
AC_INIT
AC_CONFIG_SRCDIR([74/7400-1.sym])
AC_PREREQ(2.54)

PACKAGE=geda-symbols
DOTTED_VERSION=1.5.2
DATE_VERSION=20090328
echo Configuring $PACKAGE version $DOTTED_VERSION.$DATE_VERSION

# Initialize automake stuff
AM_INIT_AUTOMAKE($PACKAGE, $DOTTED_VERSION, no-define)

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
else
   # Unix host
   MINGW="no"
fi

# 
# Misc win32 / mingw checks and variables end
#########################################################################

#########################################################################
# Check for fig2dev, groff, ps2pdf start
# 
# search for fig2dev
AC_PATH_PROG(FIG2DEV, fig2dev, no, ${PATH})
if test $FIG2DEV = "no"; then
	echo "Cannot find fig2dev (transfig), some documentation will not be created."
fi

# search for groff
AC_PATH_PROG(GROFF, groff, no, ${PATH})
if test $GROFF = "no"; then
	echo "Cannot find groff, some documentation will not be created."
fi

# search for ps2pdf
AC_PATH_PROG(PS2PDF, ps2pdf, no, ${PATH})
if test $PS2PDF = "no"; then
	echo "Cannot find ps2pdf, some documentation will not be created."
fi

# 
# Check for fig2dev, groff, ps2pdf end
#########################################################################

#########################################################################
# gEDA/gaf specify setup start
#
DATADIR=gEDA
GEDADATADIR=$datadir/$DATADIR

if eval "test x$opt_rcdir = x"; then
	# path not was specified with --with-rcdir
	GEDARCDIR=$GEDADATADIR
else
	# path WAS specified with --with-rcdir
	GEDARCDIR="$opt_rcdir"
fi

# Expand the prefix variable
# I don't like the way this is done, but it works (I hope).
if eval "test x$prefix = xNONE"; then
 dprefix=$ac_default_prefix
else
 dprefix=$prefix
fi

gedadocdir=$dprefix/share/doc/geda-doc
AC_ARG_WITH(docdir, [  --with-docdir          Where to store documentation if not default.], gedadocdir=$withval)
expandgedadocdir=`echo $gedadocdir`
GEDADOCDIR=$expandgedadocdir
#
# gEDA/gaf specify setup end
#########################################################################

AC_SUBST(DATE_VERSION)
AC_SUBST(DOTTED_VERSION)
AC_SUBST(GEDADATADIR)
AC_SUBST(GEDADOCDIR)
AC_SUBST(GEDARCDIR)

AC_CONFIG_FILES([Makefile
                 documentation/Makefile])

AC_OUTPUT 

expandedGEDADATADIR=`eval "echo $GEDADATADIR"`
expandedGEDARCDIR=`eval "echo $GEDARCDIR"`
expandedGEDADOCDIR=`eval "echo $GEDADOCDIR"`

AC_MSG_RESULT([
** Configuration summary for $PACKAGE $DOTTED_VERSION.$DATE_VERSION: 

   data directory:                   $expandedGEDADATADIR
   rc directory:                     $expandedGEDARCDIR
   documentation directory:          $expandedGEDADOCDIR
   mingw build:                      $MINGW
])

