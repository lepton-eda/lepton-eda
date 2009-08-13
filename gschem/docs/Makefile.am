EXTRA_DIST = $(html_man_files) gschem-main.txt what-is-geda.html \
	what-is-gschem.html logo.png gschem.dox

all:
	@echo 'Type: make doxygen to create doxygen documentation for gschem'

doxygen: gschem.dox
	@echo 'Creating doxygen documentation for gschem...'
if ENABLE_DOXYGEN
	$(DOXYGEN) gschem.dox
	-cd latex && $(MAKE)
	-cd ..
else
	@echo 'Doxygen is not installed on your system.' >> $@
	@echo 'The documentation can not be built from the sources.' >> $@
endif

dist_man_MANS = gschem.1


# Rules for converting man->html
html_man_files = gschem.html
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


MOSTLYCLEANFILES = 	*.log *~
CLEANFILES = 		*.log *~
DISTCLEANFILES = 	*.log core FILE *~ \
			*.dvi *.info *.info-* *.aux *.fn \
			*.fns *.cp *.ky *.pg *.toc *.tp *.vrs *.vr 
MAINTAINERCLEANFILES = *.log *~ Makefile.in $(html_man_files)

distclean-local:
	-rm -rf html latex

maintainer-clean-local:
	-rm -rf html latex
