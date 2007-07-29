
EXTRA_DIST = gsymcheck.1 gsymcheck.html

BUILT_SOURCES = gsymcheck.html

man_MANS = gsymcheck.1

docsmandir = @GEDADOCDIR@/man
docsman_DATA = gsymcheck.html


%.html: %.1
	@echo "Creating html manpage (using groff)"
	@rm -rf $*.tmp
	@if test "x$(GROFF)" != xno; then                        \
		LC_NUMERIC=C $(GROFF) -man -T html  $*.1 >$*.tmp;\
		mv -f $*.tmp $*.html                            ;\
	else                                                     \
		echo "** Did not find groff **"                 ;\
		echo "Some Documentation might not be correct!" ;\
                touch $*.html                                   ;\
	fi

gsymcheck.html: gsymcheck.1

MOSTLYCLEANFILES = *.log *~
CLEANFILES = *.log *~ gsymcheck.html
DISTCLEANFILES = *.log core FILE *~ prototype.bak
MAINTAINERCLEANFILES = *.log *~ Makefile.in configure gsymcheck.html

