## Process this file with automake to produce Makefile.in

bin_SCRIPTS = sarlacc_sym gschupdate gsymupdate gschemdoc refdes_renum \
	      tragesym pads_backannotate garchive gsymfix.pl

EXTRA_DIST = convert_sym.awk gpstoimage mk_char_tab.pl \
	     sarlacc_sym gschupdate gsymupdate refdes_renum gsymfix.pl \
	     config.sh.in gschemdoc.sh tragesym pads_backannotate garchive.py

MOSTLYCLEANFILES = *.log *.ps core FILE tmp *~
CLEANFILES = *.log *.ps core FILE tmp  *~
DISTCLEANFILES = *.log core FILE tmp *~ prototype.bak gschemdoc garchive
MAINTAINERCLEANFILES = *.log *.ps core FILE tmp *~ Makefile.in \
	               configure gschemdoc garchive

gschemdoc: config.sh gschemdoc.sh
	cat config.sh gschemdoc.sh >tmp
	chmod a+x tmp
	mv -f tmp gschemdoc

##  SDB's stuff to enable garchive.  This hardcodes the path to the
##  system libraries into garchive prior to installation.
garchive: garchive.py
	sed -e "s#GEDADATADIR#$(GEDADATADIR)#" garchive.py > garchive
	chmod a+x garchive
