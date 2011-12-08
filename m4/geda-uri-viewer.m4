# geda-uri-viewer.m4                                    -*-Autoconf-*-
# serial 1

dnl Checks for default URI launcher method
dnl Copyright (C) 2011  Peter TB Brett <peter@peter-b.co.uk>
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

AC_DEFUN([AX_OPTION_URI_VIEWER],
[
  AC_PREREQ([2.60])dnl
  AC_REQUIRE([AX_HOST])dnl

  AC_ARG_VAR([URI_VIEWER], [Path to URI launcher executable])

  AC_MSG_CHECKING([whether to use GIO to launch URIs])
  AC_ARG_ENABLE([gio],
    [AS_HELP_STRING([--enable-gio],
      [use GIO to launch URIs [default=auto]])],
    [], [enable_gio=auto])

  if test "X$enable_gio" = "Xauto" && test "X$OS_LINUX" = "Xyes"; then
    enable_gio=yes
  else
    enable_gio=no
  fi
  if test "X$enable_gio" = "Xyes"; then
    AC_DEFINE([SHOW_URI_GIO], [1],
     [Define to 1 if GIO should be used to launch a default application for
      a URI.])
  fi
  AC_MSG_RESULT([$enable_gio])


  AC_MSG_CHECKING([platform URI viewer])

  if test "X$enable_gio" = "Xyes" || test "X$OS_WIN32_NATIVE" = "Xyes"; then
    # We use an API function, so we don't need a URI viewer application
    AC_MSG_RESULT([none required])

  else
    # If the user specified a viewer command, just use that.
    if test "X$URI_VIEWER" = "X"; then

      # On Cygwin, we use cygstart, because it takes care of any required
      # translation between cygwin filenames and native filenames.
      if test "X$OS_CYGWIN" = "Xyes"; then
        URI_VIEWER=cygstart

      # On Mac OS X, we can use open(1) to launch URIs.
      elif test "X$OS_CARBON" = "Xyes"; then
        URI_VIEWER=open

      # Default to xdg-open(1) on other platforms.
      else
        URI_VIEWER=xdg-open
      fi
    fi
    AC_DEFINE_UNQUOTED([SHOW_URI_COMMAND], ["$URI_VIEWER"],
                       [Command to launch default application for a URI.])
    AC_MSG_RESULT([$URI_VIEWER])
  fi
])
