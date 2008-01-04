
dnl This macro checks that a m4 macro is define.  The primary use of
dnl this macro is to inform users that they haven't installed required
dnl -devel packages at autogen.sh time.  This macro is based on a 
dnl prototype macro created by Peter Brett.

AC_DEFUN([AC_GEDA_MACRO_CHECK], 
[
  ifdef(  [$1], 
          true,   dnl NOP
          [
            AC_FATAL(
                      [m4 macro `$1' is not defined.  Ensure that `$2' is installed in your aclocal search path.  Maybe you are missing a -dev package?], 
                    )
          ]
       )
])

