## Process this file with automake to produce Makefile.in

bin_PROGRAMS = lepton-symcheck

# don't forget all *.h files */
lepton_symcheck_SOURCES = \
	g_rc.c \
	g_register.c \
	globals.c \
	lepton-symcheck.c \
	i_vars.c \
	parsecmd.c \
	s_check.c \
	s_log.c \
	s_symstruct.c

lepton_symcheck_CPPFLAGS = -I$(top_srcdir)/liblepton/include -I$(srcdir)/../include \
	-I$(top_srcdir) -I$(includedir)
lepton_symcheck_CFLAGS = $(GCC_CFLAGS) $(MINGW_CFLAGS) $(GLIB_CFLAGS) \
	$(GUILE_CFLAGS) $(GDK_PIXBUF_CFLAGS)
lepton_symcheck_LDFLAGS = $(GLIB_LIBS) $(GUILE_LIBS) $(GDK_PIXBUF_LIBS)
lepton_symcheck_LDADD = $(top_builddir)/liblepton/src/liblepton.la

MOSTLYCLEANFILES = *.log *.ps core FILE *~
CLEANFILES = *.log *.ps core FILE *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ Makefile.in

