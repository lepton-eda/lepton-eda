
EXTRA_DIST = gsymcheck.1 gsymcheck.html

BUILT_SOURCES = gsymcheck.html

man_MANS = gsymcheck.1

docsmandir = @GEDADOCDIR@/man
docsman_DATA = gsymcheck.html


%.html: %.1
	@echo "Creating html manpage (using groff)"
	@rm -rf $*.tmp
	@if which groff >/dev/null 2>/dev/null; then             \
		LC_NUMERIC=C groff -man -T html  $*.1 >$*.tmp   ;\
	else                                                     \
		echo "** Did not find groff **"                 ;\
		echo "Some Documentation might not be correct!" ;\
                touch $*.html                                   ;\
	fi
	@mv -f $*.tmp $*.html

gsymcheck.html: gsymcheck.1

MOSTLYCLEANFILES = *.log *~
CLEANFILES = *.log *~ gsymcheck.html
DISTCLEANFILES = *.log core FILE *~ prototype.bak
MAINTAINERCLEANFILES = *.log *~ Makefile.in configure gsymcheck.html

