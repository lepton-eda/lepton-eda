# Process this file with autoconf to produce a configure script.
AC_INIT
AC_CONFIG_SRCDIR([gTAG/gTAG.sch])

PACKAGE=geda-examples
VERSION=20041228
echo Configuring $PACKAGE version $VERSION

# Initialize automake 
AM_INIT_AUTOMAKE($PACKAGE, $VERSION, no-define)

# Initialize maintainer mode
AM_MAINTAINER_MODE

HOME=`echo $HOME`
DATADIR=gEDA
GEDADATADIR=$datadir/$DATADIR

AC_SUBST(VERSION)
AC_SUBST(GEDADATADIR)

AC_CONFIG_FILES([ 
        Makefile 
        gTAG/Makefile 
        lightning_detector/Makefile 
        lightning_detector/sym/Makefile 
        RF_Amp/Makefile 
        RF_Amp/model/Makefile 
        RF_Amp/sym/Makefile 
        TwoStageAmp/Makefile 
        TwoStageAmp/sym/Makefile ])

AC_OUTPUT 

AC_MSG_RESULT([
** Configuration complete for $PACKAGE $VERSION
])

