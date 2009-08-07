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

EXTRA_DIST = create_proto 

CPROTO =        cproto
CPROTOCFLAGS =  $(INCLUDES) -I. -I.. -I../include
ALES_HACK =     -D__GNUC__

proto: prototype.h

prototype.h: $(gschem_SOURCES)
	$(CPROTO) $(ALES_HACK) $(CPROTOCFLAGS) *.c > new_proto
	mv new_proto ../include/prototype.h

MOSTLYCLEANFILES = *.log *.ps core FILE *~ prototype.bak
CLEANFILES = *.log *.ps core FILE *~ prototype.bak
DISTCLEANFILES = *.log core FILE *~ prototype.bak
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ prototype.bak prototype.h Makefile.in configure

