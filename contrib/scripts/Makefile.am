## Process this file with automake to produce Makefile.in

noinst_SCRIPTS = \
	gnet_hier_verilog.sh \
	sarlacc_sym \
	pads_backannotate \
	bompp.sh.in \
	bom_xref.sh.in \
	sch2eaglepos.sh \
	sw2asc \
	bom

dist_noinst_MANS = gnet_hier_verilog.1 pads_backannotate.1 \
	sarlacc_sym.1 sw2asc.1

EXTRA_DIST = \
	gnet_hier_verilog.sh \
	sarlacc_sym \
	pads_backannotate \
	sw2asc.in \
	mk_char_tab.pl \
	gnet_hier_verilog.1.in \
	pads_backannotate.1.in \
	sarlacc_sym.1.in \
	sw2asc.1.in \
	sch2eaglepos.sh \
	bom \
	bom.readme

.1.in.1:
	d=`$(GUILE) -c '(setlocale LC_ALL "C") \
	  (display (strftime "%B %-d, %Y" \
	  (car (strptime "%Y%m%d" (cadr (program-arguments))))))' $(DATE_VERSION)` ; \
	v="$(DOTTED_VERSION).$(DATE_VERSION)" ; \
	y=`echo $(DATE_VERSION)| sed -e"s/\(....\).*/\1/"`; \
	sed -e"s:[@]DATE@:$$d:" -e"s:[@]VERSION@:$$v:" -e"s:[@]YEAR@:$$y:" < $< > $@

SUFFIXES = .1.in .1

MOSTLYCLEANFILES = *.log *.ps core FILE tmp *~
CLEANFILES = *.log *.ps core FILE tmp  *~
DISTCLEANFILES = *.log core FILE tmp *~ bompp.sh bom_xref.sh sw2asc
MAINTAINERCLEANFILES = *.log *.ps core FILE tmp *~ Makefile.in *.1 bompp.sh bom_xref.sh sw2asc
