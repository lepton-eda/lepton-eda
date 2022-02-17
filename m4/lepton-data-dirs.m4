# lepton-data-dirs.m4                                   -*-Autoconf-*-
# serial 1.1

dnl Lepton EDA data and configuration directories
dnl Copyright (C) 2009, 2016  Peter Brett <peter@peter-b.co.uk>
dnl Copyright (C) 2018-2022 Lepton EDA Contributors
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

# Check where Lepton data and configuration should be stored.
AC_DEFUN([AX_DATA_DIRS],
[
  AC_PREREQ([2.60])dnl

  # Check where to install ordinary data files (e.g. symbols and
  # gnetlist backends)
  LEPTONDATADIR="$datarootdir/lepton-eda"


  # Define some preprocessor symbols with the *expanded* values:
  #
    LEPTONDATADIR_expand=`eval "echo $LEPTONDATADIR" | sed -e"s:^NONE:$ac_default_prefix:"`
    AC_DEFINE_UNQUOTED([LEPTONDATADIR], ["$LEPTONDATADIR_expand"],
      [Define to Lepton EDA shared data directory.
Only liblepton should use this - apps should use eda_get_system_data_dirs()])


  AC_SUBST([LEPTONDATADIR])

  AC_MSG_CHECKING([where to install Lepton shared data (LEPTONDATADIR)])
  AC_MSG_RESULT([$LEPTONDATADIR])


  # create #define LEPTON_CCACHE_DIR in config.h:
  #
  AC_DEFINE_UNQUOTED([LEPTON_CCACHE_DIR],
                     ["$LEPTONDATADIR_expand/ccache"],
                     [precompiled scm files dir])

  AC_SUBST([LEPTON_CCACHE_DIR], ["$LEPTONDATADIR_expand/ccache"])

  AC_DEFINE_UNQUOTED([BITMAP_DIRECTORY],
                     ["$LEPTONDATADIR_expand/bitmap"],
                     [directory with bitmaps])

  AC_SUBST([BITMAP_DIRECTORY], ["$LEPTONDATADIR_expand/bitmap"])

  AC_DEFINE_UNQUOTED([LEPTON_SCHEME_DIR],
                     ["$LEPTONDATADIR_expand/scheme"],
                     [directory with scheme modules])

  AC_SUBST([LEPTON_SCHEME_DIR], ["$LEPTONDATADIR_expand/scheme"])
])dnl AX_DATA_DIRS
