# geda-host.m4                                          -*-Autoconf-*-
# serial 1

dnl Checks for host platform and features
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

AC_DEFUN([AX_HOST],
[
  AC_PREREQ([2.60])dnl
  AC_REQUIRE([AC_CANONICAL_HOST])dnl

  # Need to distinguish between native Windows (via MinGW) and Unix
  # compat. layer on Windows (Cygwin).
  OS_WIN32_NATIVE=no
  OS_CYGWIN=no
  OS_LINUX=no
  case "$host" in
    *-*-linux*)
      OS_LINUX=yes
      AC_DEFINE([OS_LINUX], [1],
                [Define to 1 if on Linux.])
      ;;
    *-*-mingw*)
      OS_WIN32_NATIVE=yes
      AC_DEFINE([OS_WIN32_NATIVE], [1],
                [Define to 1 if on native Windows.])
      ;;
    *-*-cygwin*)
      OS_CYGWIN=yes
      AC_DEFINE([OS_CYGWIN], [1],
                [Define to 1 if on Cygwin.])
      ;;
    *)
      ;;
  esac

  AC_MSG_CHECKING([for Linux host])
  AC_MSG_RESULT([$OS_LINUX])

  AC_MSG_CHECKING([for Windows host])
  OS_WIN32=no
  if test "$OS_WIN32_NATIVE" = "yes" ||
     test "$OS_CYGWIN" = "yes" ; then
    OS_WIN32=yes
    AC_DEFINE([OS_WIN32], [1],
              [Define to 1 if on Windows.])
  fi
  AC_MSG_RESULT([$OS_WIN32])

  AC_MSG_CHECKING([for Cygwin host])
  AC_MSG_RESULT([$OS_CYGWIN])

  # Carbon is the best way to check for OSX.  There are some non-OSX
  # Darwin platforms out there!
  AC_MSG_CHECKING([for Mac OS X Carbon host])
  OS_CARBON=no
  AC_TRY_CPP([
  #include <Carbon/Carbon.h>
  #include <CoreServices/CoreServices.h>
  ],[
    OS_CARBON=yes
    AC_DEFINE([OS_CARBON], [1],
              [Define to 1 if on Mac OS X Carbon.])])
  AC_MSG_RESULT([$OS_CARBON])
])
