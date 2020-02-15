# geda-data-dirs.m4                                     -*-Autoconf-*-
# serial 2.0

dnl gEDA data and configuration directories
dnl Copyright (C) 2009, 2016  Peter Brett <peter@peter-b.co.uk>
dnl Copyright (C) 2018-2019 Lepton EDA Contributors
dnl
dnl This program is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 2 of the License, or
dnl (at your option) any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software
dnl Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

# Check where gEDA data and configuration should be stored.
AC_DEFUN([AX_DATA_DIRS],
[
  AC_PREREQ([2.60])dnl
  AC_REQUIRE([AX_OPTION_RELOCATABLE])dnl

  # Check where to install ordinary data files (e.g. symbols and
  # gnetlist backends)
  # FIXME at some point this should become "$datarootdir/geda-gaf" to
  # match the tarball name.
  GEDADATADIR="$datarootdir/lepton-eda"

  # Check where to install rc files.
  # FIXME at some point the rc directory needs to start *defaulting*
  # to "$sysconfdir/geda-gaf" in order to comply with the GNU & Linux
  # FHS guidelines.
  AC_ARG_WITH([rcdir],
    AS_HELP_STRING([--with-rcdir[[[=DIR]]]],
      [install system config in specific DIR]),
    [ if test "X$with_rcdir" != "Xno"; then
        if test "X$with_rcdir" = "Xyes"; then
          GEDARCDIR="$sysconfdir/lepton-eda"
        else
          GEDARCDIR="$with_rcdir"
        fi
      else
        GEDARCDIR="$GEDADATADIR"
      fi ],
      GEDARCDIR="$GEDADATADIR")

  # Now define some preprocessor symbols with the *expanded* values,
  # but only if not doing a relocatable build.
  if test "x$enable_relocatable" != "xyes"; then
    GEDADATADIR_expand=`eval "echo $GEDADATADIR" | sed -e"s:^NONE:$ac_default_prefix:"`
    AC_DEFINE_UNQUOTED([GEDADATADIR], ["$GEDADATADIR_expand"],
      [Define to gEDA/gaf shared data directory.
Only libgeda should use this - apps should use eda_get_system_data_dirs()])

    if test "x$GEDARCDIR" != "x"; then
      GEDARCDIR_expand=`eval "echo $GEDARCDIR" | sed -e"s:^NONE:$ac_default_prefix:"`
      AC_DEFINE_UNQUOTED([GEDARCDIR], ["$GEDARCDIR_expand"],
        [Define to gEDA/gaf rc directory if different from GEDADATADIR.
Only libgeda should use this - apps should use eda_get_system_data_dirs()])
    fi
  fi

  if test "x$GEDARCDIR" = "x"; then
    GEDARCDIR=$GEDADATADIR
  fi

  AC_SUBST([GEDADATADIR])
  AC_SUBST([GEDARCDIR])

  AC_MSG_CHECKING([where to install Lepton shared data (GEDADATADIR)])
  AC_MSG_RESULT([$GEDADATADIR])

  AC_MSG_CHECKING([where to install Lepton rc files (GEDARCDIR)])
  AC_MSG_RESULT([$GEDARCDIR])

  # create #define LEPTON_SCM_PRECOMPILE_DIR in config.h:
  #
  AC_DEFINE_UNQUOTED([LEPTON_SCM_PRECOMPILE_DIR],
                     ["$GEDADATADIR_expand/ccache"],
                     [precompiled scm files dir])

  AC_SUBST([LEPTON_SCM_PRECOMPILE_DIR], ["$GEDADATADIR_expand/ccache"])

  AC_DEFINE_UNQUOTED([BITMAP_DIRECTORY],
                     ["$GEDADATADIR_expand/bitmap"],
                     [directory with bitmaps])

  AC_SUBST([BITMAP_DIRECTORY], ["$GEDADATADIR_expand/bitmap"])

])dnl AX_DATA_DIRS
