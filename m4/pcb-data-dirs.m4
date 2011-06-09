# pcb-data-dirs.m4                                      -*-Autoconf-*-
# serial 2

dnl PCB prefix and m4 library directory (needed by gnetlist)
dnl Copyright (C) 2009  Peter Brett <peter@peter-b.co.uk>
dnl Copyright (C) 2011  Peter Clifton <pcjc2@cam.ac.uk>
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

# Check where to look for PCB footprints.
# FIXME All of this should be done at runtime.
AC_DEFUN([AX_PCB_DIRS],
[
  AC_PREREQ([2.60])dnl

  # Check what prefix to use for PCB footprint libraries
  AC_MSG_CHECKING([where to look for PCB footprints])
  PCBDATADIR="${datarootdir}/pcb"
  AC_ARG_WITH([pcb-datadir],
    AS_HELP_STRING([--with-pcb-datadir=DIR],
      [search for PCB libraries in DIR [[DATAROOTDIR/pcb]]]),
    [ if (test "X$with_pcb_datadir" != "Xno" &&
          test "X$with_pcb_datadir" != "Xyes"); then
        PCBDATADIR="$with_pcb_datadir"
      fi ], [])
  AC_MSG_RESULT([$PCBDATADIR])
  AC_SUBST([PCBDATADIR])

  # m4 libraries
  AC_MSG_CHECKING([where to look for PCB m4 footprints])
  PCBM4DIR="${PCBDATADIR}/m4"
  AC_ARG_WITH([pcb-m4dir],
    AS_HELP_STRING([--with-pcb-m4dir=DIR],
      [search for PCB m4 libraries in DIR [[PCBDATADIR/m4]]]),
    [ if (test "X$with_pcb_m4dir" != "Xno" &&
          test "X$with_pcb_m4dir" != "Xyes"); then
        PCBM4DIR="$with_pcb_m4dir"
      fi ], [])
  AC_MSG_RESULT([$PCBM4DIR])
  AC_SUBST([PCBM4DIR])

  # newlib search path
  AC_MSG_CHECKING([search path for PCB newlib footprints])
  PCBLIBPATH="${PCBDATADIR}/pcblib-newlib:$PCBDATADIR/newlib"
  AC_ARG_WITH([pcb-lib-path],
    AS_HELP_STRING([--with-pcb-lib-path=PATH],
      [search path for PCB newlib footprint libraries]),
    [ if (test "X$with_pcb_lib_path" != "Xno" &&
          test "X$with_pcb_lib_path" != "Xyes"); then
        PCBLIBPATH="$with_pcb_lib_path"
      fi ], [])
  AC_MSG_RESULT([$PCBLIBPATH])
  AC_SUBST([PCBLIBPATH])
])dnl AX_PCB_DIRS
