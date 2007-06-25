
SUBDIRS = \
	4000 74 IEC417 altera amphenol analog connector font idt io linear \
	local maxim memory micro misc radio switch titleblock tube spice \
	verilog rf pla ecl dec xilinx lattice national supervisor bus \
	opto diode relay vhdl power philips transistor switcap cascade \
	asic asicpads gnetman minicircuits st apex allegro irf documentation

scmdatadir = @GEDADATADIR@@PATHSEP@scheme
dist_scmdata_DATA = geda-clib.scm geda-font.scm

EXTRA_DIST = autogen.sh

distclean-local:
	-rm -rf autom4te.cache

maintainer-clean-local:
	-rm -rf autom4te.cache

MOSTLYCLEANFILES = *.log *~
CLEANFILES = *.log *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak 
MAINTAINERCLEANFILES = *.log *~ Makefile.in configure config.status \
		       aclocal.m4 config.guess config.sub install-sh \
		       missing mkinstalldirs config.h.in


