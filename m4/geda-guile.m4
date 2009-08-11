# geda-guile.m4                                           -*-Autoconf-*-
# serial 1

dnl Check for guile
dnl Copyright (C) 2009  Dan McMahill <dan@mcmahill.net>
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

AC_DEFUN([AX_CHECK_GUILE],
[
  AC_PREREQ([2.60])dnl

  # Argument is the minimum guile version.  For example
  # AX_CHECK_GUILE([1.8.0]) makes sure we have at least version 1.8.0
  GUILE_MIN_VER=[$1]
  GUILE_MIN_MAJOR=`echo ${GUILE_MIN_VER} | sed 's;\..*;;g'`
  # the double brackets are to get past m4's expansion
  GUILE_MIN_MINOR=`echo ${GUILE_MIN_VER} | sed -e 's;[[^\.]]*\.;;' -e 's;\..*;;g'`
  GUILE_MIN_TEENY=`echo ${GUILE_MIN_VER} | sed -e 's;.*\.;;'`

  _found_pkg_config_guile=yes
  PKG_CHECK_MODULES(GUILE, [guile-1.8] , ,
    [_found_pkg_config_guile=no])

  if test "${_found_pkg_config_guile}" = "no" ; then
    _found_pkg_config_guile=yes
    PKG_CHECK_MODULES(GUILE, [guile-2.0] , ,
      [_found_pkg_config_guile=no])
  fi

  if test "${_found_pkg_config_guile}" = "no" ; then
    AC_PATH_PROG([GUILE_CONFIG], [guile-config], [notfound])

    if test "${GUILE_CONFIG}" = "notfound" ; then
      AC_ERROR([${PKG_CONFIG} could not locate guile and guile-config was not found])
    fi

    AC_MSG_CHECKING([if guile is version ${GUILE_MIN_VER} or later])
    GUILE_VER=`${GUILE_CONFIG} --version 2>&1 | awk '{print $NF}'`

    GUILE_MAJOR=`echo ${GUILE_VER} | sed 's;\..*;;g'`
    # the double brackets are to get past m4's expansion
    GUILE_MINOR=`echo ${GUILE_VER} | sed -e 's;[[^\.]]*\.;;' -e 's;\..*;;g'`
    GUILE_TEENY=`echo ${GUILE_VER} | sed -e 's;.*\.;;'`

    _guile_ok=no
    if test ${GUILE_MAJOR} -lt ${GUILE_MIN_MAJOR} ; then
      # Our installed major version is less than the min major version

      _guile_ok=no

    elif test ${GUILE_MAJOR} -eq ${GUILE_MIN_MAJOR} ; then
      # Our installed major version is equal the min major version
      # so we need to check the minor version

      if test ${GUILE_MINOR} -lt ${GUILE_MIN_MINOR} ; then
	# majors match, minor is too small
	_guile_ok=no
      elif test  ${GUILE_MINOR} -eq ${GUILE_MIN_MINOR} ; then
	# majors match, minors match, check teeny
	if test ${GUILE_TEENY} -lt ${GUILE_MIN_TEENY} ; then
	  _guile_ok=no
	else
	  _guile_ok=yes
	fi
      else
	# majors match, minor is larger than min
	_guile_ok=yes
      fi

    else
      # Our installed major version is greater than the min major version

      _guile_ok=yes
    fi

    if test "${_guile_ok}" = "yes" ; then
        AC_MSG_RESULT([yes (${GUILE_VER})])
        AC_MSG_CHECKING([for guile CFLAGS])
        GUILE_CFLAGS=`${GUILE_CONFIG} compile`
        AC_MSG_RESULT([${GUILE_CFLAGS}])

        AC_MSG_CHECKING([for guile libs])
        GUILE_LIBS=`${GUILE_CONFIG} link`
        AC_MSG_RESULT([$GUILE_LIBS])
    else
        AC_MSG_ERROR([you need at least version ${GUILE_MIN_VER} of guile])
    fi
    AC_SUBST(GUILE_VER)
    AC_SUBST(GUILE_CFLAGS)
    AC_SUBST(GUILE_LIBS)
  fi


]
)
