dnl                                                      -*-Autoconf-*-
dnl Process this file with autoconf to generate configure.
dnl
dnl The AX_* macros are defined in files in the top-level m4
dnl directory.

#####################################################################
# Set up configuration system
#####################################################################

AC_INIT([Lepton EDA],
        [1.9.11],
        [https://github.com/lepton-eda/lepton-eda/issues],
        [lepton-eda],
        [https://github.com/lepton-eda/lepton-eda])
AC_PREREQ([2.60])

AC_CONFIG_SRCDIR([liblepton/src/liblepton.c])
AC_CONFIG_AUX_DIR([build-tools])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_HEADER([config.h])

AM_INIT_AUTOMAKE([parallel-tests color-tests foreign])
AM_SILENT_RULES([yes])

AC_GNU_SOURCE # FIXME for some reason this is needed?

#####################################################################
# Do version number magic
#####################################################################

AX_GIT_VERSION([20200604])

# This is used for keeping the ChangeLog files up-to-date
AC_SUBST([CHANGELOG_BASE], [1.0-20070526])

#####################################################################
# Windows/MinGW/Cygwin support
#####################################################################

AX_WINDOWS_FLAGS

#####################################################################
# Check for compiling & linking tools
#####################################################################

AC_PROG_CC
AC_PROG_CXX
AC_PROG_CPP
m4_ifdef([AM_PROG_AR], [AM_PROG_AR]) dnl Workaround for Automake 1.11

AC_LIBTOOL_WIN32_DLL
AC_DISABLE_STATIC
AC_PROG_LIBTOOL

AM_PROG_CC_C_O
AX_GCC_FLAGS([-Wall -Werror=uninitialized -std=c99])

AC_PROG_MKDIR_P
AM_PROG_LEX
AX_PROG_AWK
AX_PROG_GROFF
AC_PATH_PROGS([M4], [gm4 m4], [m4])

#####################################################################
# Internationalisation
#####################################################################

AM_NLS
AM_GNU_GETTEXT([external])
AM_GNU_GETTEXT_VERSION([0.18])
AX_DESKTOP_I18N

#####################################################################
# Check for libraries that use pkg-config
#####################################################################

PKG_PROG_PKG_CONFIG

AX_CHECK_GUILE([2.0.0])

PKG_CHECK_MODULES(GLIB, [glib-2.0 >= 2.38.0], ,
  AC_MSG_ERROR([GLib 2.38.0 or later is required.]))
AC_DEFINE([G_DISABLE_DEPRECATED], [1], [Disable deprecated GLib features])

PKG_CHECK_MODULES(GIO, [gio-2.0 >= 2.38.0], ,
  AC_MSG_ERROR([GIO 2.38.0 or later is required.]))

dnl GTK2 is used by default, though GTK3 can be choosen instead
dnl with the --with-gtk3 configure option.
AC_ARG_WITH([gtk3],
  AS_HELP_STRING([--with-gtk3], [Build with GTK3.]))

AS_CASE(["$with_gtk3"],
dnl Use GTK3.
  [yes],
    [PKG_CHECK_MODULES(GTK, [gtk+-3.0 >= 3.0.0],
      [WITH_GTK3=1],
      AC_MSG_ERROR([GTK+ 3.0.0 or later is required.]))],
dnl Default case: use GTK2.
  [PKG_CHECK_MODULES(GTK, [gtk+-2.0 >= 2.24.0],
    [WITH_GTK3=0],
    AC_MSG_ERROR([GTK+ 2.24.0 or later is required.]))])

AM_CONDITIONAL([ENABLE_GTK3], [test "x$WITH_GTK3" = x1])

PKG_CHECK_MODULES(GDK, [gdk-2.0 >= 2.24.0], ,
  AC_MSG_ERROR([GDK 2.24.0 or later is required.]))

PKG_CHECK_MODULES(GDK_PIXBUF, [gdk-pixbuf-2.0 >= 2.21.0], ,
  AC_MSG_ERROR([GDK_PIXBUF 2.21.0 or later is required.]))

AX_CHECK_CAIRO

PKG_CHECK_MODULES(PANGOCAIRO, [pangocairo >= 0.26], ,
  AC_MSG_ERROR([PANGOCAIRO 0.26 or later is required.]))

PKG_CHECK_MODULES(PANGO, [pango >= 1.23.0], ,
  AC_MSG_ERROR([PANGO 1.23.0 or later is required.]))

#####################################################################
# Header files & particular functions
#####################################################################

AC_HEADER_SYS_WAIT

# FIXME these checks are obsolete, because every plausible platform
# provides what they check for.
AC_HEADER_STDC

# FIXME these checks may be pointless for one of three reasons: every
# even vaguely sane libc implementation provides them; we don't use
# any of their functions in favour of GLib alternatives; or we don't
# provide an alternative, so the build will fail anyway.
AC_CHECK_HEADERS([math.h stdlib.h string.h unistd.h])

# FIXME since we don't have a sensible alternative, if these headers
# are missing we should fail. Also need to remove HAVE_ERRNO_H tests
# in the C source code, since if we *don't* have it the build will
# fail anyway.
AC_CHECK_HEADERS([errno.h fcntl.h])

# FIXME On a system without locale.h, the user may have just disabled NLS
# to be able to build.  But are there known systems with working NLS but
# without a locale.h?  We do need to include locale.h on some systems
# to be able to build libleptongui/src/lepton-schematic.c
AC_CHECK_HEADERS([locale.h])

# Check for lrint in math library.
AC_CHECK_LIB([m], [lrint],
             AC_DEFINE([HAVE_LRINT], 1,
                       [If your math library has lrint in it, define this]))

AC_CHECK_LIB([m], [atan2])

AC_CHECK_FUNCS([chown getlogin])

# Check if the getopt header is present
AC_CHECK_HEADERS([getopt.h])
# Check for getopt_long
# On FreeBSD the 'gnugetopt' library is needed.
AC_SEARCH_LIBS([getopt_long], [gnugetopt],
               AC_DEFINE([HAVE_GETOPT_LONG], 1,
                         [Define to 1 if you have the `getopt_long' function.]))

# Check for misc features of awk
AX_AWK_FEATURES

#####################################################################
# Optional things
#####################################################################

# Assertions
AX_OPTION_ASSERT
# Viewing documentation
AX_OPTION_URI_VIEWER
# API documentation
AX_OPTION_DOXYGEN
# XDG desktop files
AX_OPTION_XDG_DATA
# XDG database updates
AX_OPTION_XDG_DB
# libstroke support
AX_OPTION_STROKE
# attrib
AX_OPTION_ATTRIB
# contributed software
AX_OPTION_CONTRIB

#####################################################################
# Tool-specific setup
#####################################################################

# Where should data files be installed/searched for?
AX_DATA_DIRS
# Where should PCB footprints be searched for?
AX_PCB_DIRS
# Set up liblepton with the correct ld version number
AX_LIBLEPTON([6:0:0])
# Set up libleptonrenderer with the correct ld version number
AX_LIBLEPTONRENDERER([3:0:0])
# Set up libleptonattrib with the correct ld version number
AX_LIBLEPTONATTRIB([1:0:0])
# Set up libleptongui with the correct ld version number.
AX_LIBLEPTONGUI([1:0:0])

#####################################################################
# Generate output
#####################################################################

AC_CONFIG_FILES([Makefile

                 liblepton/Makefile
                 liblepton/liblepton.pc
                 liblepton/po/Makefile.in
                 liblepton/data/Makefile
                 liblepton/docs/Makefile
                 liblepton/docs/images/Makefile
                 liblepton/include/Makefile
                 liblepton/lib/Makefile
                 liblepton/scheme/Makefile
                 liblepton/src/Makefile
                 liblepton/tests/Makefile

                 libleptonrenderer/Makefile
                 libleptonrenderer/libleptonrenderer.pc

                 cli/Makefile
                 cli/po/Makefile.in

                 libleptongui/Makefile
                 libleptongui/libleptongui.pc
                 libleptongui/po/Makefile.in
                 libleptongui/docs/Makefile
                 libleptongui/include/Makefile
                 libleptongui/src/Makefile
                 libleptongui/examples/Makefile
                 libleptongui/lib/Makefile
                 libleptongui/data/Makefile
                 libleptongui/bitmap/Makefile
                 libleptongui/tests/Makefile
                 libleptongui/scheme/Makefile

                 schematic/Makefile

                 libleptonattrib/libleptonattrib.pc
                 libleptonattrib/Makefile
                 libleptonattrib/po/Makefile.in
                 libleptonattrib/include/Makefile
                 libleptonattrib/src/Makefile
                 libleptonattrib/design/Makefile
                 libleptonattrib/data/Makefile
                 libleptonattrib/docs/Makefile

                 symcheck/tests/Makefile
                 symcheck/Makefile

                 netlist/Makefile
                 netlist/scheme/Makefile
                 netlist/examples/Makefile
                 netlist/tests/Makefile
                 netlist/docs/Makefile
                 netlist/docs/vams/Makefile
                 netlist/examples/spice-noqsi/Makefile
                 netlist/examples/vams/Makefile
                 netlist/examples/vams/vhdl/Makefile
                 netlist/examples/vams/vhdl/basic-vhdl/Makefile
                 netlist/examples/vams/vhdl/new-vhdl/Makefile
                 netlist/examples/switcap/Makefile
                 netlist/examples/verilog/Makefile
                 netlist/examples/verilog/T_FF_example/Makefile
                 netlist/examples/verilog/T_FF_example/verilog_modules/Makefile
                 netlist/examples/analog/Makefile
                 netlist/examples/analog/varactor_osc/Makefile
                 netlist/examples/analog/bandpass/Makefile
                 netlist/examples/analog/voltage_doubler/Makefile

                 utils/Makefile
                 utils/docs/Makefile
                 utils/examples/Makefile
                 utils/examples/gmk_sym/Makefile
                 utils/examples/gsch2pcb/Makefile
                 utils/examples/tragesym/Makefile
                 utils/scripts/Makefile
                 utils/src/Makefile
                 utils/tests/Makefile
                 utils/tests/refdes_renum/Makefile
                 utils/tests/refdes_renum/inputs/Makefile
                 utils/tests/refdes_renum/outputs/Makefile

                 symbols/Makefile
                 symbols/documentation/Makefile
                 symbols/gnetman/Makefile
                 symbols/verilog/Makefile
                 symbols/vhdl/Makefile

                 docs/Makefile
                 docs/changelogs/Makefile
                 docs/manual/Makefile
                 docs/scheme-api/Makefile
                 docs/toplevel/Makefile
                 docs/toplevel/gedadocs.html
                 docs/wiki/Makefile

                 examples/Makefile
                 examples/RF_Amp/Makefile
                 examples/TwoStageAmp/Makefile
                 examples/gTAG/Makefile
                 examples/lightning_detector/Makefile

                 contrib/Makefile
                 contrib/smash_megafile/Makefile
                 contrib/olib/Makefile
                 contrib/gmk_sym/Makefile
                 contrib/sarlacc_schem/Makefile
                 contrib/scripts/Makefile
                 contrib/scripts/sw2asc
])
AC_OUTPUT
