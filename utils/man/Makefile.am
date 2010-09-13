## Process this file with automake to produce Makefile.in

dist_man_MANS = grenum.1 gxyrs.1


# Rules for converting man->html
html_man_files = grenum.html
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


EXTRA_DIST = $(html_man_files)

MAINTAINERCLEANFILES = Makefile.in $(html_man_files)
