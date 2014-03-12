noinst_PROGRAMS = smash_megafile

smash_megafile_SOURCES = smash_megafile.c

dist_noinst_MANS = smash_megafile.1

EXTRA_DIST = smash_megafile.1.in smash_megafile.1

.1.in.1:
	d=`LANG=C date -d $(DATE_VERSION) "+%B %-d, %Y"` ; \
	v="$(DOTTED_VERSION).$(DATE_VERSION)" ; \
	sed -e"s:[@]DATE@:$$d:" -e"s:[@]VERSION@:$$v:" < $< > $@

SUFFIXES = .1.in .1

MOSTLYCLEANFILES = *.log *.ps core FILE *~
CLEANFILES = *.log *.ps core FILE *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ Makefile.in smash_megafile.1
