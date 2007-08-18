# Process this file with autoconf to produce a configure script.
AC_INIT
AC_CONFIG_SRCDIR([74/7400-1.sym])
AC_PREREQ(2.54)

PACKAGE=geda-symbols
DOTTED_VERSION=1.1.2
DATE_VERSION=20070818
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
# Do individual library files start
# 

# Some of our own macros for setting up library files
AC_DEFUN([GEDA_DECLARE_SYMLIB],
 [
	data$1=`ls -1 $srcdir/$1 | grep -v Makefile | grep -v ChangeLog \
				 | grep -v CVS | grep -v "\\.cvsignore" \
				 | grep -v README \
				 | grep -v "\\~" | grep -v "\\.orig"`
	data$1=`echo $data$1`
        AC_SUBST(data$1)
 ])

GEDA_DECLARE_SYMLIB(4000)
GEDA_DECLARE_SYMLIB(national)
GEDA_DECLARE_SYMLIB(local)
GEDA_DECLARE_SYMLIB(io)
GEDA_DECLARE_SYMLIB(amphenol)
GEDA_DECLARE_SYMLIB(analog)
GEDA_DECLARE_SYMLIB(maxim)
GEDA_DECLARE_SYMLIB(verilog)
GEDA_DECLARE_SYMLIB(vhdl)
GEDA_DECLARE_SYMLIB(spice)
GEDA_DECLARE_SYMLIB(connector)
GEDA_DECLARE_SYMLIB(altera)
GEDA_DECLARE_SYMLIB(linear)
GEDA_DECLARE_SYMLIB(switch)
GEDA_DECLARE_SYMLIB(switcap)
GEDA_DECLARE_SYMLIB(titleblock)
GEDA_DECLARE_SYMLIB(tube)
GEDA_DECLARE_SYMLIB(radio)
GEDA_DECLARE_SYMLIB(micro)
GEDA_DECLARE_SYMLIB(memory)
GEDA_DECLARE_SYMLIB(74)
GEDA_DECLARE_SYMLIB(relay)
GEDA_DECLARE_SYMLIB(diode)
GEDA_DECLARE_SYMLIB(opto)
GEDA_DECLARE_SYMLIB(bus)
GEDA_DECLARE_SYMLIB(supervisor)
GEDA_DECLARE_SYMLIB(lattice)
GEDA_DECLARE_SYMLIB(misc)
GEDA_DECLARE_SYMLIB(transistor)
GEDA_DECLARE_SYMLIB(power)
GEDA_DECLARE_SYMLIB(philips)
GEDA_DECLARE_SYMLIB(xilinx)
GEDA_DECLARE_SYMLIB(dec)
GEDA_DECLARE_SYMLIB(rf)
GEDA_DECLARE_SYMLIB(pla)
GEDA_DECLARE_SYMLIB(ecl)
GEDA_DECLARE_SYMLIB(IEC417)
GEDA_DECLARE_SYMLIB(font)
GEDA_DECLARE_SYMLIB(idt)
GEDA_DECLARE_SYMLIB(diode)
GEDA_DECLARE_SYMLIB(opto)
GEDA_DECLARE_SYMLIB(cascade)
GEDA_DECLARE_SYMLIB(asic)
GEDA_DECLARE_SYMLIB(asicpads)
GEDA_DECLARE_SYMLIB(gnetman)
GEDA_DECLARE_SYMLIB(minicircuits)
GEDA_DECLARE_SYMLIB(st)
GEDA_DECLARE_SYMLIB(apex)
GEDA_DECLARE_SYMLIB(allegro)
GEDA_DECLARE_SYMLIB(irf)
# 
# Do individual library files start
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
AC_SUBST(PATHSEP)
AC_SUBST(OTHERPATHSEP)

AC_CONFIG_FILES([ 
        Makefile 
	4000/Makefile
	74/Makefile 
	relay/Makefile 
	bus/Makefile 
	supervisor/Makefile 
	lattice/Makefile 
	xilinx/Makefile 
	dec/Makefile 
	pla/Makefile 
	ecl/Makefile 
	rf/Makefile 
	IEC417/Makefile 
	altera/Makefile 
	amphenol/Makefile 
	analog/Makefile 
	connector/Makefile 
	font/Makefile 
	idt/Makefile 
	io/Makefile 
	linear/Makefile 
	local/Makefile
        maxim/Makefile 
	memory/Makefile 
	micro/Makefile 
	misc/Makefile 
	transistor/Makefile 
	power/Makefile 
	philips/Makefile 
	radio/Makefile 
	switch/Makefile 
	switcap/Makefile 
	titleblock/Makefile 
	tube/Makefile 
	verilog/Makefile
	vhdl/Makefile 
	spice/Makefile
	diode/Makefile 
	opto/Makefile 
	national/Makefile
	cascade/Makefile 
	asic/Makefile 
	asicpads/Makefile 
	gnetman/Makefile 
	minicircuits/Makefile
	st/Makefile 
	apex/Makefile 
	allegro/Makefile 
	irf/Makefile 
	documentation/Makefile ])

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

