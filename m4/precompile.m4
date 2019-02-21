# precompile.m4
# serial 2.0

dnl precompilation settings

AC_DEFUN([AX_PRECOMPILE],
[
  AC_PREREQ([2.60])dnl
  AC_REQUIRE([AX_DATA_DIRS])dnl


  # data directory:
  #
  DDIR=`eval "echo $GEDADATADIR"`


  # create #define LEPTON_SCM_PRECOMPILE_DIR in config.h:
  #
  AC_DEFINE_UNQUOTED([LEPTON_SCM_PRECOMPILE_DIR],
                     ["$DDIR/ccache"],
                     [precompiled scm files dir])


  # create #define LEPTON_SCM_PRECOMPILE_CFG in config.h:
  #
  AC_DEFINE_UNQUOTED([LEPTON_SCM_PRECOMPILE_CFG],
                     ["$DDIR/scheme/schematic/precompile-config.scm"],
                     [config file for precompile script])


  AC_SUBST([LEPTON_SCM_PRECOMPILE_DIR], ["$DDIR/ccache"])
  AC_SUBST([LEPTON_SCM_PRECOMPILE_CFG], ["$DDIR/scheme/schematic/precompile-config.scm"])

])dnl AX_PRECOMPILE
