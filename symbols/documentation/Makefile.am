datadocdir = @GEDADATADIR@/documentation

datadoc_DATA = nc.pdf

EXTRA_DIST = nc.doc

MOSTLYCLEANFILES = *.log *~
CLEANFILES = *.log *~ *.ps *.pdf geda.pic
DISTCLEANFILES = *.log core FILE *~ prototype.bak geda.pic
MAINTAINERCLEANFILES = *.log *~ Makefile.in configure geda.pic

#
#  some standard rules for groff based documentation
#
%.pic: %.fig
	fig2dev -L pic $*.fig >$*.pic

%.ps: %.doc
	rm -rf $*.tmp
	groff -t -sp -ms $*.doc >$*.tmp
	mv -f $*.tmp $*.ps

%.pdf: %.ps
	rm -rf $*.pdf
	ps2pdf $*.ps

#
#  dependencies
#
nc.ps: geda.pic
