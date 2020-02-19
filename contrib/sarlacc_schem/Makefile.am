noinst_PROGRAMS = sarlacc_schem

sarlacc_schem_SOURCES = sarlacc_schem.c
sarlacc_schem_CFLAGS = $(GLIB_CFLAGS) $(GUILE_CFLAGS) $(GDK_PIXBUF_CFLAGS)
sarlacc_schem_LDFLAGS = $(GLIB_LIBS) $(GUILE_LIBS) $(GDK_PIXBUF_LIBS)
sarlacc_schem_LDADD = $(top_builddir)/liblepton/src/liblepton.la

AM_CFLAGS = $(GCC_CFLAGS) $(MINGW_CFLAGS)
AM_CPPFLAGS = -I$(top_srcdir)/liblepton/include

dist_noinst_MANS = sarlacc_schem.1

EXTRA_DIST = sarlacc_schem.1.in sarlacc_schem.1

.1.in.1:
	d=`$(GUILE) -c '(setlocale LC_ALL "C") \
	  (display (strftime "%B %-d, %Y" \
	  (car (strptime "%Y%m%d" (cadr (program-arguments))))))' $(DATE_VERSION)` ; \
	v="$(DOTTED_VERSION).$(DATE_VERSION)" ; \
	y=`echo $(DATE_VERSION)| sed -e"s/\(....\).*/\1/"`; \
	sed -e"s:[@]DATE@:$$d:" -e"s:[@]VERSION@:$$v:" -e"s:[@]YEAR@:$$y:" < $< > $@

SUFFIXES = .1.in .1
