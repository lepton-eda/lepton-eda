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
	y=`echo $(DATE_VERSION)| sed -e"s/\(....\).*/\1/"`; \
	sed -e"s:[@]DATE@:$$d:" -e"s:[@]VERSION@:$$v:" -e"s:[@]YEAR@:$$y:" < $< > $@

SUFFIXES = .1.in .1

CLEANFILES = olib.c
MAINTAINERCLEANFILES = olib.1 Makefile.in
