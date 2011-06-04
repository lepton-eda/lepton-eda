# geda-guile-display-error.m4                             -*-Autoconf-*-
# serial 1

dnl Check for behaviour of scm_display_error().
dnl Copyright (C) 2011  Peter Brett <peter@peter-b.co.uk>
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

AC_DEFUN([AX_GUILE_DISPLAY_ERROR],
[
  AC_PREREQ([2.60])dnl
  AC_REQUIRE([AX_CHECK_GUILE])dnl

  AC_ARG_VAR([GUILE], [Path to guile executable])
  AC_CHECK_PROG([GUILE], [guile], [guile], [no])

  if test "X$GUILE" != "Xno"; then

    AC_MSG_CHECKING([whether scm_display_error expects a stack argument])
    if $GUILE -c \
"(exit
   (false-if-exception
     (begin
       (display-error (make-stack #t) (current-output-port) \"a\" \"b\" '() '())
       #t)))" > /dev/null; then
      AC_MSG_RESULT([yes])
      AC_DEFINE([HAVE_SCM_DISPLAY_ERROR_STACK], 1,
                [Define to 1 if scm_display_error expects a stack as first argument.])
    else
      AC_MSG_RESULT([no])
    fi

    AC_MSG_CHECKING([whether scm_display_error expects a frame argument])
    if $GUILE -c \
"(exit
   (false-if-exception
     (begin
       (display-error (stack-ref (make-stack #t) 0)
                      (current-output-port) \"a\" \"b\" '() '())
       #t)))" > /dev/null; then
      AC_MSG_RESULT([yes])
      AC_DEFINE([HAVE_SCM_DISPLAY_ERROR_FRAME], 1,
                [Define to 1 if scm_display_error expects a frame as first argument.])
    else
      AC_MSG_RESULT([no])
    fi
  fi
])dnl AX_GUILE_DISPLAY_ERROR
