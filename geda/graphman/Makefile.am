INCLUDES = \
	$(GTK_CFLAGS)\
	 -I./ -I$(top_srcdir)/intl

localedir = @datadir@/locale
DEFS = -DPACKAGE_LOCALE_DIR=\"$(localedir)\" @DEFS@

AM_CFLAGS =\
	 -Wall -Wimplicit -Wreturn-type -Wunused -Wswitch -Wcomment -Wuninitialized -Wparentheses -Wpointer-arith -Wmissing-prototypes\
	 -O1\
	 -g

bin_PROGRAMS = raw2gw

graphman_SOURCES = \
	raw2gw.c
	
graphman_LDFLAGS =

graphman_LDADD =  \
	$(GTK_LIBS)

MOSTLYCLEANFILES = *.log *~ graphman
CLEANFILES = *.log *~ graphman
DISTCLEANFILES = *.log core FILE *~ prototype.bak graphman
MAINTAINERCLEANFILES = *.log *~ Makefile.in configure graphman
