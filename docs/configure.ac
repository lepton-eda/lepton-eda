# Process this file with autoconf to produce a configure script.
AC_INIT

VERSION=20060123
PACKAGE=geda-docs
echo Configuring $PACKAGE version $VERSION

# Init automake
AM_INIT_AUTOMAKE($PACKAGE, $VERSION)

############################################################################
# Check for various doc processing programs start
# 

# latex
AC_PATH_PROG(LATEX, latex, no, ${PATH})
if test $LATEX = "no"; then
   echo "Cannot find latex -- documentation cannot be created"
   echo "Install latex or install the prepared geda-docs package"
fi

AC_PATH_PROG(DVIPS, dvips, no, ${PATH})
AC_PATH_PROG(DVI2PS, dvi2ps, no, ${PATH})

if test $DVIPS = "no" -a $DVI2PS = "no"; then
   echo "Cannot find dvips or dvi2ps -- documentation cannot be created" 
   echo "Install dvips or dvi2ps or install the prepared geda-docs package"
fi

AC_PATH_PROG(LATEX2HTML, latex2html, no, ${PATH})
if test $LATEX2HTML = "no"; then 
   echo "Cannot find latex2html -- documentation cannot be created" 
   echo "Install latex2html or install the prepared geda-docs package"
fi

AC_PATH_PROG(PS2PDF, ps2pdf, no, ${PATH})
if test $PS2PDF = "no"; then 
   echo "Cannot find pdf2ps -- documentation cannot be created" 
   echo "Install pdf2ps or install the prepared geda-docs package"
fi

# 
# Check for various doc processing programs end
############################################################################


# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.


# Other variables
#

# Expand the prefix variable
# I don't like the way this is done, but it works (I hope).
if eval "test x$prefix = xNONE"; then
 dprefix=$ac_default_prefix
else
 dprefix=$prefix
fi

gedadocdir=$dprefix/share/doc/geda-doc
# --with-docdir : tells where to store documentation if not default
AC_ARG_WITH(docdir,
    [  --with-docdir      Where to store documentation if not default.],
	gedadocdir=$withval)
expandgedadocdir=`echo $gedadocdir`
GEDADOCDIR=$expandgedadocdir
AC_SUBST(GEDADOCDIR)

AC_CONFIG_FILES([Makefile 
	         attributes/Makefile 
		 symbols/Makefile 
	         fileformats/Makefile 
                 hooks/Makefile 
                 keymapping/Makefile 
                 netattrib/Makefile 
                 pads_backannotate/Makefile 
                 cascade/Makefile 
		 gschem/Makefile 
		 gnetlist/Makefile 
		 switcap/Makefile 
		 spice-sdb/Makefile 
		 tragesym_tutorial/Makefile 
		 gsch2pcb_tutorial/Makefile 
		 gsch2pcb_tutorial/images/Makefile 
		 toplevel/Makefile 
		 toplevel/gedadocs.html 
                 templates/Makefile 
                 scripts/Makefile ])
AC_OUTPUT

expandedGEDADOCDIR=`eval "echo $GEDADOCDIR"`

AC_MSG_RESULT([
** Configuration summary for $PACKAGE $VERSION:

   documentation directory:          $expandedGEDADOCDIR
])

