## Process this file with automake to produce Makefile.in

libgedaincludedir = $(includedir)/libgeda
libgedainclude_HEADERS = \
	colors.h defines.h funcs.h globals.h o_types.h \
	prototype.h struct.h libgeda.h i_vars.h papersizes.h \
	geda_list.h

noinst_HEADERS = \
	libgeda_priv.h \
	prototype_priv.h \
	gettext_priv.h \
	guile_compat_1.6.h

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in configure

