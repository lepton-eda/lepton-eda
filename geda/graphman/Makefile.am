# GraphMan Makefile.am

INCLUDES = @GMANAGER_CFLAGS@ -I./ -I$(top_srcdir)/intl

localedir = @datadir@/locale
DEFS = -DPACKAGE_LOCALE_DIR=\"$(localedir)\" @DEFS@

AM_CFLAGS =\
	 -Wall -Wimplicit -Wreturn-type -Wunused -Wswitch -Wcomment -Wuninitialized -Wparentheses -Wpointer-arith -Wmissing-prototypes\
	 -O1\
	 -g

bin_PROGRAMS = graphman raw2gw

raw2gw_SOURCES = \
	raw2gw.c

graphman_SOURCES = \
	callbacks.c \
	callbacks.h \
	configfile.c \
	configfile.h \
	export2gwave.c \
	export2gwave.h \
	filetool.c \
	filetool.h \
	global.h \
	graph.c \
	graph.h \
	interface.c \
	interface.h \
	main.c \
	msg.h \
	msgbox.c \
	msgbox.h \
	libstring.c \
	libstring.h \
	support.c \
	support.h \
	value.c \
	value.h

	
graphman_LDFLAGS = @GMANAGER_LDFLAGS@

graphman_LDADD = @GTK_LIBS@ @LIBINTL@

MOSTLYCLEANFILES = *.log *~ graphman
CLEANFILES = *.log *~ graphman
DISTCLEANFILES = *.log core FILE *~ prototype.bak graphman
MAINTAINERCLEANFILES = *.log *~ Makefile.in configure graphman
