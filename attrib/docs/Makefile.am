EXTRA_DIST = lepton-attrib.dox what-is-lepton-attrib.html what-is-geda.html \
	     attrib-main.txt logo.png lepton-attrib.1.in

all:
	@echo 'Type: make doxygen  to create doxygen documentation for lepton-attrib'

doxygen: lepton-attrib.dox
	@echo 'Creating doxygen documentation for lepton-attrib...'
if ENABLE_DOXYGEN
	$(DOXYGEN) lepton-attrib.dox
else
	@echo 'configure was not run with the --enable-doxygen switch.'
	@echo 'The documentation cannot be built from the sources.'
endif

dist_man_MANS = lepton-attrib.1

.1.in.1:
	d=`$(GUILE) -c '(setlocale LC_ALL "C") \
	  (display (strftime "%B %-d, %Y" \
	  (car (strptime "%Y%m%d" (cadr (program-arguments))))))' $(DATE_VERSION)` ; \
	v="$(DOTTED_VERSION).$(DATE_VERSION)" ; \
	y=`echo $(DATE_VERSION)| sed -e"s/\(....\).*/\1/"`; \
	sed -e"s:[@]DATE@:$$d:" -e"s:[@]VERSION@:$$v:" -e"s:[@]YEAR@:$$y:" < $< > $@

# Rules for converting man->html
html_man_files = lepton-attrib.html
SUFFIXES = .1.in .1 .html

docsmandir=$(docdir)/man
dist_docsman_DATA=$(html_man_files)
if ENABLE_GROFF_HTML
.1.html:
	rm -rf $@.tmp && \
	LC_NUMERIC=C $(GROFF) -man -T html  $< >$@.tmp && \
	mv -f $@.tmp $@
else
.1.html:
	@echo "No working groff available: cannot create $@"
	@! :
endif

CLEANFILES = $(dist_man_MANS) $(html_man_files)
MAINTAINERCLEANFILES = Makefile.in

distclean-local:
	-rm -rf html latex

maintainer-clean-local:
	-rm -rf html latex
