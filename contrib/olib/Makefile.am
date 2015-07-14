## Process this file with automake to produce Makefile.in

noinst_PROGRAMS = olib

olib_SOURCES = olib.l geda_sym_format.h
olib_LDFLAGS = -lm

dist_noinst_MANS = olib.1

EXTRA_DIST = olib.1 olib.1.in README.olib

.1.in.1:
	d=`$(GUILE) -c '(setlocale LC_ALL "C") \
	  (display (strftime "%B %-d, %Y" \
	  (car (strptime "%Y%m%d" (cadr (program-arguments))))))' $(DATE_VERSION)` ; \
	v="$(DOTTED_VERSION).$(DATE_VERSION)" ; \
	sed -e"s:[@]DATE@:$$d:" -e"s:[@]VERSION@:$$v:" < $< > $@

SUFFIXES = .1.in .1

MOSTLYCLEANFILES = *.log *.ps core FILE *~ olib.c
CLEANFILES = *.log *.ps core FILE *~  olib.c
DISTCLEANFILES = *.log core FILE *~ olib.c
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ Makefile.in olib.c olib.1

