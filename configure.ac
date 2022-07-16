dnl                                                      -*-Autoconf-*-
dnl Process this file with autoconf to generate configure.
dnl
dnl The AX_* macros are defined in files in the top-level m4
dnl directory.

#####################################################################
# Set up configuration system
#####################################################################

AC_INIT([Lepton EDA],
        [1.9.18],
        [https://github.com/lepton-eda/lepton-eda/issues],
        [lepton-eda],
        [https://github.com/lepton-eda/lepton-eda])
AC_PREREQ([2.60])

AC_CONFIG_SRCDIR([liblepton/src/liblepton.c])
AC_CONFIG_AUX_DIR([build-tools])
AC_REQUIRE_AUX_FILE([test-driver.scm])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_HEADER([config.h])

AM_INIT_AUTOMAKE([parallel-tests color-tests foreign])
AM_SILENT_RULES([yes])

# Without this macro, some variables like M_PI won't be defined.
AC_GNU_SOURCE

#####################################################################
# Do version number magic
#####################################################################

AX_GIT_VERSION([20220529])

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

AX_CHECK_GUILE([2.2.0])

PKG_CHECK_MODULES(GLIB, [glib-2.0 >= 2.38.0], ,
  AC_MSG_ERROR([GLib 2.38.0 or later is required.]))
AC_DEFINE([G_DISABLE_DEPRECATED], [1], [Disable deprecated GLib features])

PKG_CHECK_MODULES(GIO, [gio-2.0 >= 2.38.0], ,
  AC_MSG_ERROR([GIO 2.38.0 or later is required.]))

dnl GTK2 is used by default, though GTK3 can be choosen instead
dnl with the --with-gtk3 configure option.
AC_MSG_CHECKING([whether to use GTK3])
AC_ARG_WITH([gtk3],
  AS_HELP_STRING([--with-gtk3], [Build with GTK3.]))

if test "X$with_gtk3" = "Xyes"; then
  AC_MSG_RESULT([yes])
  AC_DEFINE([ENABLE_GTK3], [1],
    [Define to 1 if GTK3 will be used for build.])
else
  AC_MSG_RESULT([no])
fi

AS_CASE(["$with_gtk3"],
dnl Use GTK3.
  [yes],
    [PKG_CHECK_MODULES(GTK, [gtk+-3.0 >= 3.22.0],
      [WITH_GTK3=1],
      AC_MSG_ERROR([GTK+ 3.22.0 or later is required.]))],
dnl Default case: use GTK2.
  [PKG_CHECK_MODULES(GTK, [gtk+-2.0 >= 2.24.0],
    [WITH_GTK3=0],
    AC_MSG_ERROR([GTK+ 2.24.0 or later is required.]))])

AM_CONDITIONAL([ENABLE_GTK3], [test "x$WITH_GTK3" = x1])

AS_CASE(["$with_gtk3"],
dnl Use GDK3.
  [yes],
    [PKG_CHECK_MODULES(GDK, [gdk-3.0 >= 3.22.0], ,
      AC_MSG_ERROR([GDK 3.22.0 or later is required.]))],
dnl Default case: use GDK2.
  [PKG_CHECK_MODULES(GDK, [gdk-2.0 >= 2.24.0], ,
    AC_MSG_ERROR([GDK+ 2.24.0 or later is required.]))])

PKG_CHECK_MODULES(GDK_PIXBUF, [gdk-pixbuf-2.0 >= 2.21.0], ,
  AC_MSG_ERROR([GDK_PIXBUF 2.21.0 or later is required.]))


dnl Enable compilation of Scheme files with guild.
AC_MSG_CHECKING([whether to use GUILD])

AC_ARG_ENABLE([guild],
  AS_HELP_STRING([--enable-guild], [Enable guild compilation of Scheme files.]))

if test "X$enable_guild" = "Xyes"; then
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([no])
fi

AM_CONDITIONAL([ENABLE_GUILD], [test "X$enable_guild" = "Xyes"])


AX_CHECK_CAIRO

PKG_CHECK_MODULES(PANGOCAIRO, [pangocairo >= 0.26], ,
  AC_MSG_ERROR([PANGOCAIRO 0.26 or later is required.]))

PKG_CHECK_MODULES(PANGO, [pango >= 1.23.0], ,
  AC_MSG_ERROR([PANGO 1.23.0 or later is required.]))

dnl Use ldconfig to get shared library names in a case -dev files
dnl are not installed on the system.
AC_ARG_VAR(USE_LDCONFIG, [Use ldconfig to get shared library names])

dnl ImageMagick tools for tests.
AC_PATH_PROG(IM_COMPARE, compare)
AC_PATH_PROG(IM_CONVERT, convert)
AC_PATH_PROG(IM_IDENTIFY, identify)

dnl 'file' tool for tests.
AC_PATH_PROG(FILE, file)

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
AX_LIBLEPTON([11:0:0])
# Set up libleptonattrib with the correct ld version number
AX_LIBLEPTONATTRIB([5:0:0])
# Set up libleptongui with the correct ld version number.
AX_LIBLEPTONGUI([5:0:0])

#####################################################################
# Generate output
#####################################################################

AC_CONFIG_FILES([schematic/lepton-schematic:script.in:schematic/lepton-schematic.scm],
                [chmod +x schematic/lepton-schematic])

AC_CONFIG_FILES([tools/cli/scheme/lepton-cli:script.in:tools/cli/scheme/lepton-cli.scm],
                [chmod +x tools/cli/scheme/lepton-cli])

AC_CONFIG_FILES([tools/cli/scheme/lepton-config:script.in:tools/cli/scheme/lepton-config.scm],
                [chmod +x tools/cli/scheme/lepton-config])

AC_CONFIG_FILES([tools/cli/scheme/lepton-export:script.in:tools/cli/scheme/lepton-export.scm],
                [chmod +x tools/cli/scheme/lepton-export])

AC_CONFIG_FILES([tools/cli/scheme/lepton-shell:script.in:tools/cli/scheme/lepton-shell.scm],
                [chmod +x tools/cli/scheme/lepton-shell])

AC_CONFIG_FILES([tools/archive/lepton-archive:script.in:tools/archive/lepton-archive.scm],
                [chmod +x tools/archive/lepton-archive])

AC_CONFIG_FILES([tools/attrib/lepton-attrib:script.in:tools/attrib/lepton-attrib.scm],
                [chmod +x tools/attrib/lepton-attrib])

AC_CONFIG_FILES([tools/embed/lepton-embed:script.in:tools/embed/lepton-embed.scm],
                [chmod +x tools/embed/lepton-embed])

AC_CONFIG_FILES([tools/netlist/lepton-netlist:script.in:tools/netlist/lepton-netlist.scm],
                [chmod +x tools/netlist/lepton-netlist])

AC_CONFIG_FILES([tools/symcheck/lepton-symcheck:script.in:tools/symcheck/lepton-symcheck.scm],
                [chmod +x tools/symcheck/lepton-symcheck])

AC_CONFIG_FILES([tools/tragesym/lepton-tragesym:script.in:tools/tragesym/lepton-tragesym.scm],
                [chmod +x tools/tragesym/lepton-tragesym])

AC_CONFIG_FILES([tools/upcfg/lepton-upcfg:script.in:tools/upcfg/lepton-upcfg.scm],
                [chmod +x tools/upcfg/lepton-upcfg])

AC_CONFIG_FILES([tools/update/lepton-update:script.in:tools/update/lepton-update.scm],
                [chmod +x tools/update/lepton-update])

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

                 tools/cli/Makefile
                 tools/cli/scheme/Makefile
                 tools/cli/docs/Makefile
                 tools/cli/po/Makefile.in
                 tools/cli/tests/Makefile

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
                 libleptonattrib/data/Makefile
                 libleptonattrib/docs/Makefile

                 tools/attrib/Makefile

                 tools/symcheck/tests/Makefile
                 tools/symcheck/docs/Makefile
                 tools/symcheck/Makefile

                 tools/netlist/Makefile
                 tools/netlist/scheme/Makefile
                 tools/netlist/examples/Makefile
                 tools/netlist/tests/Makefile
                 tools/netlist/tests/symcache/Makefile
                 tools/netlist/docs/Makefile
                 tools/netlist/docs/vams/Makefile
                 tools/netlist/examples/spice-noqsi/Makefile
                 tools/netlist/examples/vams/Makefile
                 tools/netlist/examples/vams/vhdl/Makefile
                 tools/netlist/examples/vams/vhdl/basic-vhdl/Makefile
                 tools/netlist/examples/vams/vhdl/new-vhdl/Makefile
                 tools/netlist/examples/switcap/Makefile
                 tools/netlist/examples/switcap/cache/Makefile
                 tools/netlist/examples/verilog/Makefile
                 tools/netlist/examples/verilog/T_FF_example/Makefile
                 tools/netlist/examples/verilog/T_FF_example/verilog_modules/Makefile
                 tools/netlist/examples/analog/Makefile
                 tools/netlist/examples/analog/varactor_osc/Makefile
                 tools/netlist/examples/analog/bandpass/Makefile
                 tools/netlist/examples/analog/voltage_doubler/Makefile

                 tools/Makefile

                 tools/symfix/Makefile
                 tools/symfix/docs/Makefile

                 tools/archive/docs/Makefile
                 tools/archive/Makefile

                 tools/embed/Makefile
                 tools/embed/docs/Makefile

                 tools/pcb_backannotate/Makefile
                 tools/pcb_backannotate/docs/Makefile

                 tools/refdes_renum/Makefile
                 tools/refdes_renum/docs/Makefile
                 tools/refdes_renum/tests/Makefile
                 tools/refdes_renum/tests/inputs/Makefile
                 tools/refdes_renum/tests/outputs/Makefile

                 tools/schdiff/Makefile
                 tools/schdiff/docs/Makefile

                 tools/upcfg/Makefile
                 tools/upcfg/docs/Makefile

                 tools/update/Makefile
                 tools/update/docs/Makefile
                 tools/update/tests/Makefile

                 tools/sch2pcb/Makefile
                 tools/sch2pcb/src/Makefile
                 tools/sch2pcb/examples/Makefile
                 tools/sch2pcb/docs/Makefile

                 tools/tragesym/Makefile
                 tools/tragesym/examples/Makefile
                 tools/tragesym/docs/Makefile

                 symbols/Makefile
                 symbols/docs/Makefile
                 symbols/scheme/Makefile
                 symbols/sym/Makefile
                 symbols/sym-gnetman/Makefile
                 symbols/sym-verilog/Makefile
                 symbols/sym-vhdl/Makefile

                 docs/Makefile
                 docs/changelogs/Makefile
                 docs/manual/Makefile
                 docs/scheme-api/Makefile
                 docs/wiki/Makefile
                 docs/news/Makefile
                 docs/design/Makefile

                 examples/Makefile
                 examples/RF_Amp/Makefile
                 examples/TwoStageAmp/Makefile
                 examples/gTAG/Makefile
                 examples/lightning_detector/Makefile

                 contrib/Makefile
                 contrib/smash_megafile/Makefile
                 contrib/olib/Makefile
                 contrib/gmk_sym/Makefile
                 contrib/gmk_sym/examples/Makefile
                 contrib/sarlacc_schem/Makefile
                 contrib/scripts/Makefile
                 contrib/scripts/sw2asc
                 contrib/renum/Makefile
                 contrib/renum/src/Makefile
                 contrib/renum/docs/Makefile
])
AC_OUTPUT
