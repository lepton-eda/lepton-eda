## Process this file with automake to produce Makefile.in

noinst_PROGRAMS = gmk_sym convert_sym

dist_noinst_MANS = convert_sym.1 gmk_sym.1

EXTRA_DIST = convert_sym.1.in convert_sym.1 gmk_sym.1.in gmk_sym.1

gmk_sym_SOURCES = gmk_sym.c char_width.c
convert_sym_SOURCES = convert_sym.c char_width.c
convert_sym_LDFLAGS = -lm

.1.in.1:
	d=`$(GUILE) -c '(setlocale LC_ALL "C") \
	  (display (strftime "%B %-d, %Y" \
	  (car (strptime "%Y%m%d" (cadr (program-arguments))))))' $(DATE_VERSION)` ; \
	v="$(DOTTED_VERSION).$(DATE_VERSION)" ; \
	sed -e"s:[@]DATE@:$$d:" -e"s:[@]VERSION@:$$v:" < $< > $@

SUFFIXES = .1.in .1

AM_CFLAGS = $(GCC_CFLAGS) $(MINGW_CFLAGS)
AM_CPPFLAGS = -I$(top_srcdir)/libgeda/include -I$(top_srcdir)

MOSTLYCLEANFILES = *.log *.ps core FILE *~
CLEANFILES = *.log *.ps core FILE *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ Makefile.in
