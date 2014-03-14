## Process this file with automake to produce Makefile.in

noinst_PROGRAMS = sarlacc_schem

sarlacc_schem_SOURCES = sarlacc_schem.c
sarlacc_schem_CFLAGS = $(GLIB_CFLAGS) $(GUILE_CFLAGS) $(GDK_PIXBUF_CFLAGS)
sarlacc_schem_LDFLAGS = $(GLIB_LIBS) $(GUILE_LIBS) $(GDK_PIXBUF_LIBS)
sarlacc_schem_LDADD = $(top_builddir)/libgeda/src/libgeda.la

AM_CFLAGS = $(GCC_CFLAGS) $(MINGW_CFLAGS)
AM_CPPFLAGS = -I$(top_srcdir)/libgeda/include

dist_noinst_MANS = sarlacc_schem.1

EXTRA_DIST = sarlacc_schem.1.in sarlacc_schem.1

.1.in.1:
	d=`LANG=C date -d $(DATE_VERSION) "+%B %-d, %Y"` ; \
	v="$(DOTTED_VERSION).$(DATE_VERSION)" ; \
	sed -e"s:[@]DATE@:$$d:" -e"s:[@]VERSION@:$$v:" < $< > $@

SUFFIXES = .1.in .1

MOSTLYCLEANFILES = *.log *.ps core FILE *~
CLEANFILES = *.log *.ps core FILE *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ Makefile.in
