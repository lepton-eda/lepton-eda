## Process this file with automake to produce Makefile.in

bin_SCRIPTS = sarlacc_sym gschupdate gsymupdate gschemdoc refdes_renum \
	      tragesym pads_backannotate

EXTRA_DIST = convert_sym.awk gpstoimage mk_char_tab.pl \
	     sarlacc_sym gschupdate gsymupdate refdes_renum \
	     config.sh.in gschemdoc.sh pads_backannotate

MOSTLYCLEANFILES = *.log *.ps core FILE tmp *~
CLEANFILES = *.log *.ps core FILE tmp  *~
DISTCLEANFILES = *.log core FILE tmp *~ prototype.bak gschemdoc
MAINTAINERCLEANFILES = *.log *.ps core FILE tmp *~ Makefile.in \
	               configure gschemdoc

gschemdoc: config.sh gschemdoc.sh
	cat config.sh gschemdoc.sh >tmp
	chmod a+x tmp
	mv -f tmp gschemdoc
