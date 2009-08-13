
EXTRA_DIST = $(html_man_files) gsymcheck.dox logo.png \
	     gsymcheck-main.txt what-is-geda.html what-is-gsymcheck.html

dist_man_MANS = gsymcheck.1


# Rules for converting man->html
html_man_files = gsymcheck.html
SUFFIXES = .1 .html

if ENABLE_GROFF_HTML
docsmandir=$(docdir)/man
docsman_DATA=$(html_man_files)
.1.html:
	rm -rf $@.tmp && \
	LC_NUMERIC=C $(GROFF) -man -T html  $< >$@.tmp && \
	mv -f $@.tmp $@
else
.1.html:
	@echo "No working groff available: cannot create $@"
	@! :
endif


MOSTLYCLEANFILES = *.log *~
CLEANFILES = *.log *~ gsymcheck.html
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *~ Makefile.in $(html_man_files)

