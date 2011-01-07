# geda-awk.m4                                           -*-Autoconf-*-
# serial 1

dnl Improved checks for awk executable path and features
dnl Copyright (C) 2008  Dan McMahill <dan@mcmahill.net>
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

AC_DEFUN([AX_PROG_AWK],
[
  AC_PREREQ([2.60])dnl
  AC_PROVIDE([AC_PROG_AWK])dnl
  AC_ARG_VAR([AWK], [Path to awk executable])
  AC_PATH_PROGS([AWK], [mawk gawk nawk awk], [no])
  if test "X$AWK" = "Xno"; then
    AC_MSG_ERROR([The awk tool could not be found. Ensure it is installed and in your
path.])
  fi
])dnl AX_PROG_AWK

dnl AX_AWK_IFELSE (PROGRAM, INPUT, [ACTION-IF-TRUE], [ACTION-IF-FALSE])
AC_DEFUN([AX_AWK_IFELSE],
[
  AC_PREREQ([2.60])dnl
  AC_REQUIRE([AC_PROG_AWK])dnl
  cat > conftest.awk <<EOF
[#]line __oline__ "configure"
[$1]
EOF
  cat > conftest.txt <<EOF
[$2]
EOF
  if ($AWK -f conftest.awk conftest.txt >/dev/null; exit) 2>&AC_FD_CC; then
    ifelse([$3], , :,
           [rm -fr conftest*;
$3])
  else
    echo "configure:__oline__: $AWK -f conftest.awk conftest.txt" >&AC_FD_CC
    echo "configure:__oline__: failed program was:" >&AC_FD_CC
    cat conftest.awk >&AC_FD_CC
    echo "configure:__oline__: failed input file was:" >&AC_FD_CC
    cat conftest.txt >&AC_FD_CC
    ifelse([$4], , ,
           [rm -fr conftest*;
$4])
  fi
])dnl AX_AWK_IFELSE

dnl Check for some miscellaneous Awk features used by gEDA.
AC_DEFUN([AX_AWK_FEATURES],
[
  AC_PREREQ([2.60])dnl
  AC_REQUIRE([AC_PROG_AWK])dnl

  AC_MSG_CHECKING([whether $AWK has gsub])
  AX_AWK_IFELSE([{gsub(/foo/,"bar");}], [foo bar],
    [AWK_GSUB="yes"],[AWK_GSUB="no"])
  AC_MSG_RESULT([$AWK_GSUB])

  AC_MSG_CHECKING([whether $AWK has toupper])
  AX_AWK_IFELSE([{print toupper("test")}], [foo bar],
    [AWK_TOUPPER=yes],[AWK_TOUPPER=no])
  AC_MSG_RESULT([$AWK_TOUPPER])

  if (test "X$AWK_GSUB" = "Xno" || test "X$AWK_TOUPPER" = "Xno"); then
    AC_MSG_ERROR([Your awk ($AWK) is missing the `gsub' and/or `toupper' functions.
Consider installing gawk.])
  fi
])dnl AX_AWK_FEATURES
