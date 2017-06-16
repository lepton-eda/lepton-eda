## Process this file with automake to produce Makefile.in

bin_PROGRAMS = gnetlist

# don't forget all *.h files */
gnetlist_SOURCES = \
	globals.c \
	gnetlist.c \
	i_vars.c \
	parsecmd.c

gnetlist_CPPFLAGS = -I$(top_srcdir)/liblepton/include -I$(srcdir)/../include \
	-I$(top_srcdir) -I$(includedir) -DBINDIR=\"$(bindir)\"
gnetlist_CFLAGS = $(GCC_CFLAGS) $(MINGW_CFLAGS) $(GLIB_CFLAGS) \
	$(GUILE_CFLAGS) $(GDK_PIXBUF_CFLAGS)
gnetlist_LDFLAGS = $(GLIB_LIBS) $(GUILE_LIBS) $(GDK_PIXBUF_LIBS)
gnetlist_LDADD = $(top_builddir)/liblepton/src/liblepton.la

localedir = @datadir@/locale
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@


MOSTLYCLEANFILES = *.log *.ps core FILE *~
CLEANFILES = *.log *.ps core FILE *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ Makefile.in
