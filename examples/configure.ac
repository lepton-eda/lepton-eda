# Process this file with autoconf to produce a configure script.
AC_INIT

PACKAGE=geda-examples
VERSION=20070626
echo Configuring $PACKAGE version $VERSION

# Initialize automake 
AM_INIT_AUTOMAKE($PACKAGE, $VERSION, no-define)


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


AC_CONFIG_FILES([ 
        Makefile 
        gTAG/Makefile 
        lightning_detector/Makefile 
        lightning_detector/sym/Makefile 
        RF_Amp/Makefile 
        RF_Amp/model/Makefile 
        RF_Amp/sym/Makefile 
        TwoStageAmp/Makefile 
        TwoStageAmp/models/Makefile 
        TwoStageAmp/sym/Makefile ])

AC_OUTPUT 

AC_MSG_RESULT([
** Configuration complete for $PACKAGE $VERSION
])

