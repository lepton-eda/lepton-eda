## Process this file with automake to produce Makefile.in

bin_PROGRAMS = gsymcheck 

# don't forget all *.h files */
gsymcheck_SOURCES = \
	g_rc.c \
	g_register.c \
	globals.c \
	gsymcheck.c \
	i_vars.c \
	parsecmd.c \
	s_check.c \
	s_log.c \
	s_symstruct.c

gsymcheck_CPPFLAGS = -I$(top_srcdir)/libgeda/include -I$(srcdir)/../include \
	-I$(top_srcdir) -I$(includedir)
gsymcheck_CFLAGS = $(GCC_CFLAGS) $(MINGW_CFLIGS) $(GLIB_CFLAGS) \
	$(GUILE_CFLAGS) $(GDK_PIXBUF_CFLAGS)
gsymcheck_LDFLAGS = $(GLIB_LIBS) $(GUILE_LIBS) $(GDK_PIXBUF_LIBS)
gsymcheck_LDADD = $(top_builddir)/libgeda/src/libgeda.la

MOSTLYCLEANFILES = *.log *.ps core FILE *~
CLEANFILES = *.log *.ps core FILE *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ Makefile.in

