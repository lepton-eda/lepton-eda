noinst_PROGRAMS = smash_megafile

smash_megafile_SOURCES = smash_megafile.c

dist_noinst_MANS = smash_megafile.1

EXTRA_DIST = smash_megafile.1.in smash_megafile.1

.1.in.1:
	d=`$(GUILE) -c '(setlocale LC_ALL "C") \
	  (display (strftime "%B %-d, %Y" \
	  (car (strptime "%Y%m%d" (cadr (program-arguments))))))' $(DATE_VERSION)` ; \
	v="$(DOTTED_VERSION).$(DATE_VERSION)" ; \
	y=`echo $(DATE_VERSION)| sed -e"s/\(....\).*/\1/"`; \
	sed -e"s:[@]DATE@:$$d:" -e"s:[@]VERSION@:$$v:" -e"s:[@]YEAR@:$$y:" < $< > $@

SUFFIXES = .1.in .1

MAINTAINERCLEANFILES = Makefile.in smash_megafile.1
