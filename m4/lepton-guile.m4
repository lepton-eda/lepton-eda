# lepton-guile.m4                                           -*-Autoconf-*-
# serial 3

dnl Check for guile
dnl Copyright (C) 2009  Dan McMahill <dan@mcmahill.net>
dnl Copyright (C) 2010-2011  Peter Brett <peter@peter-b.co.uk>
dnl Copyright (C) 2009-2016 gEDA Contributors
dnl Copyright (C) 2017-2020 Lepton EDA Contributors
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

AC_DEFUN([AX_CHECK_GUILE],
[
  AC_PREREQ([2.60])dnl

  # First check for the libguile library
  # ------------------------------------

  # Argument is the minimum guile version.  For example
  # AX_CHECK_GUILE([1.8.0]) makes sure we have at least version 1.8.0
  GUILE_MIN_VER=[$1]
  GUILE_MIN_MAJOR=`echo ${GUILE_MIN_VER} | sed 's;\..*;;g'`
  # the double brackets are to get past m4's expansion
  GUILE_MIN_MINOR=`echo ${GUILE_MIN_VER} | sed -e 's;[[^\.]]*\.;;' -e 's;\..*;;g'`
  GUILE_MIN_TEENY=`echo ${GUILE_MIN_VER} | sed -e 's;.*\.;;'`

  _found_pkg_config_guile=yes

  PKG_CHECK_MODULES(GUILE, [guile-3.0 >= $GUILE_MIN_VER],
                           [_found_pkg_config_guile=yes
                            GUILE_PKG_NAME=guile-3.0],
                           [_found_pkg_config_guile=no])

  if test "${_found_pkg_config_guile}" = "no" ; then
    PKG_CHECK_MODULES(GUILE, [guile-2.2 >= $GUILE_MIN_VER],
                             [_found_pkg_config_guile=yes
                              GUILE_PKG_NAME=guile-2.2],
                             [_found_pkg_config_guile=no])
  fi

  if test "${_found_pkg_config_guile}" = "no" ; then
    AC_MSG_ERROR([you need at least version ${GUILE_MIN_VER} of guile])
  fi

  AC_SUBST([GUILE_PKG_NAME])


  GUILE_FLAGS
  GUILE_PROGS
  GUILE_PKG([3.0 2.2])


  # Check for the `guile-snarf' build tool
  # --------------------------------------
  AC_ARG_VAR([GUILE_SNARF], [path to guile-snarf utility])

  AC_CHECK_PROG([GUILE_SNARF], [guile-snarf], [guile-snarf], [no])
  if test "x$GUILE_SNARF" = xno ; then
    AC_MSG_ERROR([The `guile-snarf' tool could not be found. Please ensure that the
Guile development headers and tools are correctly installed, and rerun
configure.])
  fi

  # Check for behaviour of `scm_display_error'
  # ------------------------------------------
  if test "X$GUILE" != "Xno"; then

    AC_MSG_CHECKING([whether scm_display_error accepts a stack])
    if $GUILE -c \
"(exit
   (false-if-exception
     (begin
       (display-error (make-stack #t) (current-output-port) \"a\" \"b\" '() '())
       #t)))" > /dev/null 2>&1; then
      AC_MSG_RESULT([yes])
      AC_DEFINE([HAVE_SCM_DISPLAY_ERROR_STACK], 1,
                [Define to 1 if scm_display_error accepts a stack as first argument.])
    else
      AC_MSG_RESULT([no])
    fi

    AC_MSG_CHECKING([whether scm_display_error accepts a frame])
    if $GUILE -c \
"(exit
   (false-if-exception
     (begin
       (display-error (stack-ref (make-stack #t) 0)
                      (current-output-port) \"a\" \"b\" '() '())
       #t)))" > /dev/null 2>&1; then
      AC_MSG_RESULT([yes])
      AC_DEFINE([HAVE_SCM_DISPLAY_ERROR_FRAME], 1,
                [Define to 1 if scm_display_error expects a frame as first argument.])
    else
      AC_MSG_RESULT([no])
    fi
  fi

])dnl AX_CHECK_GUILE
