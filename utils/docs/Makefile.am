dist_man_MANS = gsch2pcb.1 \
	pcb_backannotate.1 tragesym.1 \
	gsymfix.1

EXTRA_DIST = README.gsch2pcb README.tragesym \
	logo.png utils-main.txt \
	what-is-geda.html what-is-utils.html utils.dox \
	gsch2pcb.1.in \
	pcb_backannotate.1.in \
	tragesym.1.in \
	gsymfix.1.in

docsreadmedir = $(docdir)/readmes
docsreadme_DATA = README.gsch2pcb README.tragesym

doxygen: utils.dox
	@echo 'Creating doxygen documentation for utils...'
if ENABLE_DOXYGEN
	$(DOXYGEN) utils.dox
else
	@echo 'configure was not run with the --enable-doxygen switch.'
	@echo 'The documentation cannot be built from the sources.'
endif

.1.in.1:
	d=`$(GUILE) -c '(setlocale LC_ALL "C") \
	  (display (strftime "%B %-d, %Y" \
	  (car (strptime "%Y%m%d" (cadr (program-arguments))))))' $(DATE_VERSION)` ; \
	v="$(DOTTED_VERSION).$(DATE_VERSION)" ; \
	y=`echo $(DATE_VERSION)| sed -e"s/\(....\).*/\1/"`; \
	sed -e"s:[@]DATE@:$$d:" -e"s:[@]VERSION@:$$v:" -e"s:[@]YEAR@:$$y:" < $< > $@

# Rules for converting man->html
html_man_files = $(dist_man_MANS:%.1=%.html)
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

MOSTLYCLEANFILES = *.log *~
CLEANFILES = *.log *~ 
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *~ Makefile.in $(html_man_files)

distclean-local:
	-rm -rf html latex

maintainer-clean-local:
	-rm -rf html latex 
