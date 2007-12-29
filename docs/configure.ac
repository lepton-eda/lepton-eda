# Process this file with autoconf to produce a configure script.
AC_INIT
AC_CONFIG_SRCDIR([wiki/index.html])
AC_PREREQ(2.54)

DOTTED_VERSION=1.3.0
DATE_VERSION=20071229
PACKAGE=geda-docs
echo Configuring $PACKAGE version $DOTTED_VERSION.$DATE_VERSION

# Init automake
AM_INIT_AUTOMAKE($PACKAGE, $DOTTED_VERSION, no-define)

# Check for programs.

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.


# Other variables
#

# Expand the prefix variable
# I don't like the way this is done, but it works (I hope).
if eval "test x$prefix = xNONE"; then
 dprefix=$ac_default_prefix
else
 dprefix=$prefix
fi

gedadocdir=$dprefix/share/doc/geda-doc
# --with-docdir : tells where to store documentation if not default
AC_ARG_WITH(docdir,
    [  --with-docdir      Where to store documentation if not default.],
	gedadocdir=$withval)
expandgedadocdir=`echo $gedadocdir`
GEDADOCDIR=$expandgedadocdir
AC_SUBST(GEDADOCDIR)

AC_SUBST(DATE_VERSION)
AC_SUBST(DOTTED_VERSION)

AC_DEFUN([GEDA_DECLARE_DOCS],
 [
        data$1=`find $srcdir/$1 -name \* -print \
				 | sed "s%\.\./wiki/%%" \
				 | sed "s%\.\./wiki%%" \
				 | sed "s%\./wiki/%%" \
				 | sed "s%\./wiki%%" \
				 | grep -v Makefile | grep -v ChangeLog \
                                 | grep -v CVS | grep -v "\\.cvsignore" \
                                 | grep -v README \
                                 | grep -v "\\~" | grep -v "\\.orig"`
        data$1=`echo $data$1`
        AC_SUBST(data$1)
 ])

GEDA_DECLARE_DOCS(wiki)

AC_CONFIG_FILES([Makefile 
		 toplevel/Makefile 
		 toplevel/gedadocs.html 
                 scripts/Makefile 
                 wiki/Makefile 
		 wiki/_detail/Makefile
 		 wiki/_media/Makefile
		 wiki/lib/exe/Makefile
		 wiki/lib/images/interwiki/Makefile
		 wiki/lib/images/fileicons/Makefile
		 wiki/lib/images/smileys/Makefile
		 wiki/lib/images/Makefile
		 wiki/lib/tpl/default/images/Makefile
		 wiki/lib/tpl/default/Makefile
		 wiki/lib/tpl/Makefile
		 wiki/lib/plugins/note/images/Makefile
		 wiki/lib/plugins/note/Makefile
		 wiki/lib/plugins/definitionlist/images/Makefile
		 wiki/lib/plugins/definitionlist/Makefile
	 	 wiki/lib/plugins/Makefile
		 wiki/lib/Makefile ])
AC_OUTPUT

expandedGEDADOCDIR=`eval "echo $GEDADOCDIR"`

AC_MSG_RESULT([
** Configuration summary for $PACKAGE $DOTTED_VERSION.$DATE_VERSION:

   documentation directory:          $expandedGEDADOCDIR
])

