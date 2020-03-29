dist_man_MANS = \
	lepton-archive.1 \
	lepton-embed.1 \
	lepton-pcb_backannotate.1 \
	lepton-refdes_renum.1 \
	lepton-renum.1 \
	lepton-sch2pcb.1 \
	lepton-schdiff.1 \
	lepton-symfix.1 \
	lepton-tragesym.1 \
	lepton-upcfg.1

EXTRA_DIST = \
	lepton-archive.1.in \
	lepton-embed.1.in \
	lepton-pcb_backannotate.1.in \
	lepton-refdes_renum.1.in \
	lepton-renum.1.in \
	lepton-sch2pcb.1.in \
	lepton-schdiff.1.in \
	lepton-symfix.1.in \
	lepton-tragesym.1.in \
	lepton-upcfg.1.in \
	README.lepton-renum \
	README.lepton-sch2pcb \
	README.lepton-tragesym \
	TODO.lepton-renum \
	why.lepton-renum \
	logo.png \
	utils-main.txt \
	utils.dox \
	what-is-geda.html \
	what-is-utils.html

docsreadmedir = $(docdir)/readmes
docsreadme_DATA = \
	README.lepton-renum \
	README.lepton-sch2pcb \
	README.lepton-tragesym

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

CLEANFILES = $(dist_man_MANS) $(html_man_files)

distclean-local:
	-rm -rf html latex

maintainer-clean-local:
	-rm -rf html latex
