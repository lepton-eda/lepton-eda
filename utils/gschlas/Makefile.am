## Process this file with automake to produce Makefile.in

bin_PROGRAMS = lepton-schlas

# don't forget all *.h files */
lepton_schlas_SOURCES = \
	g_rc.c \
	g_register.c \
	globals.c \
	lepton-schlas.c \
	i_vars.c \
	parsecmd.c \
	s_util.c

lepton_schlas_CPPFLAGS = -I$(top_srcdir)/liblepton/include -I$(srcdir)/../include \
	-I$(top_srcdir) -I$(includedir)
lepton_schlas_CFLAGS = $(GCC_CFLAGS) $(MINGW_CFLAGS) $(GLIB_CFLAGS) \
	$(GUILE_CFLAGS) $(GDK_PIXBUF_CFLAGS)
lepton_schlas_LDFLAGS = $(GLIB_LIBS) $(GUILE_LIBS) $(GDK_PIXBUF_LIBS)
lepton_schlas_LDADD = $(top_builddir)/liblepton/src/liblepton.la

MOSTLYCLEANFILES = *.log *.ps core FILE *~
CLEANFILES = *.log *.ps core FILE *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ Makefile.in
