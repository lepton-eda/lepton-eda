
SUBDIRS = \
	4000 74 IEC417 altera analog connector font idt io linear \
	local maxim memory micro misc radio switch titleblock tube spice \
	verilog rf pla ecl dec xilinx lattice national supervisor bus \
	opto diode relay vhdl power transistor documentation

bin_SCRIPTS = gesym-config

rcdatadir = @GEDARCDIR@
rcdata_DATA = system-commonrc 


gesym-config-install: gesym-config
	chmod +x gesym-config
	$(mkinstalldirs) $(DESTDIR)$(bindir)
	$(binSCRIPT_INSTALL) gesym-config $(DESTDIR)$(bindir)

dist-clean-local:
	-rm -rf autom4te.cache

maintainer-clean-local:
	-rm -rf autom4te.cache

MOSTLYCLEANFILES = *.log *~
CLEANFILES = *.log *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak 
MAINTAINERCLEANFILES = *.log *~ Makefile.in configure config.status
