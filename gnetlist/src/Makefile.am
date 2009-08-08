## Process this file with automake to produce Makefile.in

bin_PROGRAMS = gnetlist 

# don't forget all *.h files */
gnetlist_SOURCES = \
	g_netlist.c \
	g_rc.c \
	g_register.c \
	globals.c \
	gnetlist.c \
	i_vars.c \
	parsecmd.c \
	s_cpinlist.c \
	s_hierarchy.c \
	s_misc.c \
	s_net.c \
	s_netattrib.c \
	s_netlist.c \
	s_rename.c \
	s_traverse.c \
	vams_misc.c

gnetlist_CPPFLAGS = -I$(top_srcdir)/libgeda/include -I$(srcdir)/../include \
	-I$(top_srcdir) -I$(includedir)
gnetlist_CFLAGS = $(GCC_CFLAGS) $(MINGW_CFLIGS) $(GLIB_CFLAGS) \
	$(GUILE_CFLAGS) $(GDK_PIXBUF_CFLAGS)
gnetlist_LDFLAGS = $(GLIB_LIBS) $(GUILE_LIBS) $(GDK_PIXBUF_LIBS)
gnetlist_LDADD = $(top_builddir)/libgeda/src/libgeda.la

MOSTLYCLEANFILES = *.log *.ps core FILE *~
CLEANFILES = *.log *.ps core FILE *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ Makefile.in configure

