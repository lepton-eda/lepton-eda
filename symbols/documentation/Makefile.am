doc_DATA = nc.pdf

EXTRA_DIST = nc.doc nc.ps nc.pdf

BUILT_SOURCES = nc.pdf nc.ps

SUFFIXES = .ps .doc .pdf

#
#  some standard rules for groff based documentation
#
.doc.ps:
	@echo "Creating ps (using groff)"
	@rm -rf $@.tmp
	@if test "x$(GROFF)" != xno; then                        \
		LC_NUMERIC=C $(GROFF) -t -sp -ms $< > $@.tmp    ;\
		mv -f $@.tmp $@                                 ;\
	else						         \
		echo "** Did not find groff **"                 ;\
		echo "Some Documentation might not be correct!" ;\
		touch $@                                     ;\
	fi

.ps.pdf:
	@echo "Creating pdf (using ps2pdf)"
	@rm -rf $@
	@if which ps2pdf >/dev/null 2>/dev/null; then             \
		ps2pdf $<					;\
	else                                                     \
		echo "** Did not find ps2pdf **"                ;\
		echo "Some Documentation might not be correct!" ;\
		touch $@                                        ;\
	fi

#
#  dependencies
#
nc.pdf: nc.ps

MOSTLYCLEANFILES = *.log *~
CLEANFILES = *.log *~ *.ps *.pdf
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *~ Makefile.in *.pdf *.ps
