## $Id$
##
## Process this file with automake to produce Makefile.in

bin_SCRIPTS = sarlacc_sym gschupdate gsymupdate refdes_renum \
	      tragesym pads_backannotate pcb_backannotate garchive gsymfix.pl \
	      gnet_hier_verilog.sh

EXTRA_DIST = convert_sym.awk gpstoimage mk_char_tab.pl \
	     sarlacc_sym gschupdate gsymupdate refdes_renum gsymfix.pl \
	     tragesym pads_backannotate pcb_backannotate garchive.py \
	     gnet_hier_verilog.sh

##  SDB's stuff to enable garchive.  This hardcodes the path to the
##  system libraries into garchive prior to installation.
garchive: $(srcdir)/garchive.py
	sed -e "s#GEDADATADIR#$(GEDADATADIR)#" $(srcdir)/garchive.py > tmp$@
	chmod a+x tmp$@
	mv -f tmp$@ $@

MOSTLYCLEANFILES = *.log *.ps core FILE tmp *~
CLEANFILES = *.log *.ps core FILE tmp  *~
DISTCLEANFILES = *.log core FILE tmp *~ garchive
MAINTAINERCLEANFILES = *.log *.ps core FILE tmp *~ Makefile.in garchive
