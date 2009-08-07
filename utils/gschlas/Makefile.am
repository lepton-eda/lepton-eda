## Process this file with automake to produce Makefile.in

bin_PROGRAMS = gschlas

# don't forget all *.h files */
gschlas_SOURCES = \
	g_rc.c \
	g_register.c \
	globals.c \
	gschlas.c \
	i_vars.c \
	parsecmd.c \
	s_util.c

gschlas_CPPFLAGS = -I$(top_srcdir)/libgeda/include -I$(srcdir)/../include \
	-I$(top_srcdir) -I$(includedir)
gschlas_CFLAGS = $(GCC_CFLAGS) $(MINGW_CFLIGS) $(GLIB_CFLAGS) \
	$(GUILE_CFLAGS) $(GDK_PIXBUF_CFLAGS)
gschlas_LDFLAGS = $(GLIB_LIBS) $(GUILE_LIBS) $(GDK_PIXBUF_LIBS)
gschlas_LDADD = $(top_builddir)/libgeda/src/libgeda.la

MOSTLYCLEANFILES = *.log *.ps core FILE *~
CLEANFILES = *.log *.ps core FILE *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ Makefile.in

