datadocdir = @GEDADOCDIR@

datadoc_DATA = nc.pdf

EXTRA_DIST = nc.doc geda.fig geda.pic nc.ps nc.pdf

BUILT_SOURCES = geda.pic nc.pdf nc.ps

#
#  some standard rules for groff based documentation
#
%.pic: %.fig
	@echo "Creating pic (using fig2dev)"
	@if which fig2dev >/dev/null 2>/dev/null; then           \
		fig2dev -L pic $*.fig >$*.pic		       ;\
	else                                                    \
		echo "** Did not find fig2dev **"              ;\
		echo "Some documentation might not be correct!";\
		touch $*.pic				       ;\
	fi

%.ps: %.doc
	@echo "Creating ps (using groff)"
	@rm -rf $*.tmp
	@if which groff >/dev/null 2>/dev/null; then              \
		LC_NUMERIC=C groff -t -sp -ms $*.doc >$*.tmp ;\
	else						         \
		echo "** Did not find groff **"                 ;\
		echo "Some Documentation might not be correct!" ;\
		touch $*.ps                                     ;\
	fi
	@mv -f $*.tmp $*.ps

%.pdf: %.ps
	@echo "Creating pdf (using ps2pdf)"
	@rm -rf $*.pdf
	@if which ps2pdf >/dev/null 2>/dev/null; then             \
		ps2pdf $*.ps					;\
	else                                                     \
		echo "** Did not find ps2pdf **"                ;\
		echo "Some Documentation might not be correct!" ;\
		touch $*.pdf                                    ;\
	fi

#
#  dependencies
#
nc.ps: geda.pic
nc.pdf: nc.ps

MOSTLYCLEANFILES = *.log *~
CLEANFILES = *.log *~ *.ps *.pdf 
DISTCLEANFILES = *.log core FILE *~ prototype.bak 
MAINTAINERCLEANFILES = *.log *~ Makefile.in configure geda.pic *.pdf *.ps
