
SUBDIRS = vams

EXTRA_DIST = $(html_man_files) logo.png \
	     gnetlist-main.txt what-is-geda.html what-is-gnetlist.html \
	     gnetlist.dox

docsreadmedir = $(docdir)/readmes
dist_docsreadme_DATA = \
	README.bom README.pcb README.switcap README.verilog \
	README.vhdl README.sysc README.eagle

dist_man_MANS = gnetlist.1


# Rules for converting man->html
html_man_files = gnetlist.html
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

doxygen: gnetlist.dox
	@echo 'Creating doxygen documentation for gnetlist...'
if ENABLE_DOXYGEN
	$(DOXYGEN) gnetlist.dox
else
	@echo 'Doxygen is not installed on your system.'
	@echo 'The documentation can not be built from the sources.'
endif

MOSTLYCLEANFILES = *.log *~
CLEANFILES = *.log *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *~ Makefile.in $(html_man_files)

distclean-local:
	-rm -rf html latex

maintainer-clean-local:
	-rm -rf html latex
