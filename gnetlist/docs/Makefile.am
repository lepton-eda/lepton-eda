
SUBDIRS = vams

EXTRA_DIST = README.bom README.pcb README.verilog README.vhdl README.switcap \
	     gnetlist.1 gnetlist.html

BUILT_SOURCES = gnetlist.html

man_MANS = gnetlist.1

docsreadmedir = @GEDADOCDIR@/readme
docsreadme_DATA = README.bom README.pcb README.switcap README.verilog \
		  README.vhdl

docsmandir = @GEDADOCDIR@/man
docsman_DATA = gnetlist.html


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

gnetlist.html: gnetlist.1

MOSTLYCLEANFILES = *.log *~
CLEANFILES = *.log *~ gnetlist.html
DISTCLEANFILES = *.log core FILE *~ prototype.bak gnetlist.html
MAINTAINERCLEANFILES = *.log *~ Makefile.in configure gnetlist.html
