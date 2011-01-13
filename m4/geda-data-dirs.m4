# geda-data-dirs.m4                                     -*-Autoconf-*-
# serial 1.0

dnl gEDA data and configuration directories
dnl Copyright (C) 2009  Peter Brett <peter@peter-b.co.uk>
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

  # Check where to install ordinary data files (e.g. symbols and
  # gnetlist backends)
  # FIXME at some point this should become "$datarootdir/geda-gaf" to
  # match the tarball name.
  AC_MSG_CHECKING([where to install gEDA shared data])
  GEDADATADIR="$datarootdir/gEDA"
  AC_MSG_RESULT([$GEDADATADIR])

  # Check where to install rc files.
  # FIXME at some point the rc directory needs to start *defaulting*
  # to "$sysconfdir/geda-gaf" in order to comply with the GNU & Linux
  # FHS guidelines.
  AC_MSG_CHECKING([where to install gEDA rc files])
  AC_ARG_WITH([rcdir],
    AS_HELP_STRING([--with-rcdir[[[=DIR]]]],
      [install system config in specific DIR]),
    [ if test "X$with_rcdir" != "Xno"; then
        if test "X$with_rcdir" = "Xyes"; then
          GEDARCDIR="$sysconfdir/gEDA"
        else
          GEDARCDIR="$with_rcdir"
        fi
        AC_MSG_RESULT([$GEDARCDIR])
      else
        AC_MSG_RESULT([$GEDADATADIR])
      fi ],
    [ AC_MSG_RESULT([$GEDADATADIR])
  ])

  # Now define some preprocessor symbols with the *expanded* values
  GEDADATADIR_expand=`eval "echo $GEDADATADIR" | sed -e"s:^NONE:$ac_default_prefix:"`
  AC_DEFINE_UNQUOTED([GEDADATADIR], ["$GEDADATADIR_expand"],
    [Define to gEDA/gaf shared data directory.
Only libgeda should use this - apps should use s_path_sys_data()])

  if test "x$GEDARCDIR" != "x"; then
    GEDARCDIR_expand=`eval "echo $GEDARCDIR" | sed -e"s:^NONE:$ac_default_prefix:"`
    AC_DEFINE_UNQUOTED([GEDARCDIR], ["$GEDARCDIR_expand"],
      [Define to gEDA/gaf rc directory if different from GEDADATADIR.
Only libgeda should use this - apps should use s_path_sys_config()])

  else
    GEDARCDIR=$GEDADATADIR
  fi

  AC_SUBST([GEDADATADIR])
  AC_SUBST([GEDARCDIR])

])dnl AX_DATA_DIRS
