# geda-desktop.m4                                       -*-Autoconf-*-
# serial 1.0

dnl MIME & desktop icon directories, and MIME database update options
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
dnl Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

# Check where KDE data should be installed (needed for KDE 3)
AC_DEFUN([AX_OPTION_KDE3_DATA],
[
  AC_PREREQ([2.60])dnl
  AC_MSG_CHECKING([whether to install KDE 3 desktop files])

  AC_ARG_WITH([kdedatadir],
    AS_HELP_STRING([--with-kdedatadir[[[=DIR]]]],
      [install KDE 3 desktop files in DIR [[DIR=DATAROOTDIR]]]),
    # If --with-kdedatadir was specified, need to handle three cases
    # for its argument: "yes", "no" and an explicit path.
    [ if test "X$with_kdedatadir" != "Xno"; then
        AC_MSG_RESULT([yes])
        AC_MSG_CHECKING([where to install KDE 3 desktop files])
        # If a path was given to --with-kdedatadir, use that path;
        # otherwise, use $datadir.
        if test "X$with_kdedatadir" = "Xyes"; then
          KDEDATADIR="$datarootdir"
        else
          KDEDATADIR="$with_kdedatadir"
        fi
        AC_MSG_RESULT([$KDEDATADIR])
      else
        AC_MSG_RESULT([no])
      fi ],
    [ AC_MSG_RESULT([no]) ])

  AM_CONDITIONAL([ENABLE_KDE_DESKTOP_DATA],
                 test "X$KDEDATADIR" != "X")
  AC_SUBST([KDEDATADIR])
])dnl AX_OPTION_KDE3_DATA



# Check where XDG data should be installed
AC_DEFUN([AX_OPTION_XDG_DATA],
[
  AC_PREREQ([2.60])dnl

  dnl Complain if icon-theme-installer is missing
  AC_REQUIRE_AUX_FILE([icon-theme-installer])
  ICON_THEME_INSTALLER="\${SHELL} $am_aux_dir/icon-theme-installer"
  AC_SUBST([ICON_THEME_INSTALLER])

  AC_MSG_CHECKING([where to install XDG desktop files])
  AC_ARG_WITH([xdgdatadir],
    AS_HELP_STRING([--with-xdgdatadir[[[=DIR]]]],
      [install XDG desktop files in DIR [[DIR=DATAROOTDIR]]]))

  if (test "X$with_xdgdatadir" = "X$yes") || (test "X$with_xdgdatadir" = "X"); then
    with_xdgdatadir="$datarootdir"
  fi
  AC_MSG_RESULT([$with_xdgdatadir])
  AC_SUBST([XDGDATADIR], [$with_xdgdatadir])
])dnl AX_OPTION_XDG_DATA



# Check if the MIME database should be updated, and if so, find the
# update-mime-database program.
AC_DEFUN([AX_OPTION_XDG_DB],
[
  AC_PREREQ([2.60])dnl
  AC_ARG_VAR([UPDATE_MIME_DATABASE], [Path to update-mime-database executable])
  AC_ARG_VAR([UPDATE_DESKTOP_DATABASE], [Path to update-desktop-database executable])

  # Check if the user enabled updating of the MIME database
  AC_MSG_CHECKING([whether 'make install' should update XDG databases])
  AC_ARG_ENABLE([update-xdg-database],
    [AS_HELP_STRING([--disable-update-xdg-database],
      [do not update XDG database after installation])],
    [], [enable_update_xdg_database=yes])

  # If user didn't disable XDG database update, look for tools
  if test "X$enable_update_xdg_database" = "Xyes"; then
    AC_MSG_RESULT([yes])

    # Check for update-mime-database
    AC_CHECK_PROG([UPDATE_MIME_DATABASE], [update-mime-database],
                  [update-mime-database], [no])
    if test "X$UPDATE_MIME_DATABASE" = "Xno"; then
      AC_MSG_ERROR([The update-mime-database tool could not be found. Ensure it is
installed and in your path, or configure with
--disable-update-xdg-database.])
    fi

    # Check for update-desktop-database
    AC_CHECK_PROG([UPDATE_DESKTOP_DATABASE], [update-desktop-database],
                  [update-desktop-database], [no])
    if test "X$UPDATE_DESKTOP_DATABASE" = "Xno"; then
      AC_MSG_ERROR([The update-desktop-database tool could not be found. Ensure it is
installed and in your path, or configure with
--disable-update-xdg-database.])
    fi
  else
    AC_MSG_RESULT([no])
  fi

  AM_CONDITIONAL([ENABLE_UPDATE_XDG_DATABASE],
                 test "X$enable_update_xdg_database" = "Xyes")
  AC_SUBST([UPDATE_MIME_DATABASE])
  AC_SUBST([UPDATE_DESKTOP_DATABASE])
])dnl AX_OPTION_XDG_DB
