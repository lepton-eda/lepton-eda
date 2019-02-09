## -*- Makefile -*-

scmdatadir = $(GEDADATADIR)/scheme
nobase_dist_scmdata_DATA = \
	auto-refdes.scm \
	auto-uref.scm \
	gschem.scm \
	list-keys.scm \
	print-NB-attribs.scm \
	auto-place-attribs.scm \
	auto-place-netname.scm \
	pcb.scm \
	default-attrib-positions.scm \
	gschem/core/gettext.scm \
	gschem/gschemdoc.scm \
	gschem/window.scm \
	gschem/selection.scm \
	gschem/deprecated.scm \
	gschem/hook.scm \
	gschem/attrib.scm \
	gschem/keymap.scm \
	gschem/repl.scm \
	gschem/util.scm \
	gschem/action.scm \
	gschem/builtins.scm \
	gschem/symbol/check.scm \
	schematic/netlist.scm \
	schematic/undo.scm \
	conf/schematic/attribs.scm \
	conf/schematic/deprecated.scm \
	conf/schematic/keys.scm \
	conf/schematic/menu.scm \
	conf/schematic/options.scm \
	conf/schematic/stroke.scm

gschem/gschemdoc.scm: gschem/gschemdoc.scm.in
	sed -e 's,[@]docdir[@],$(docdir),g' < $(srcdir)/$@.in > $@

EXTRA_DIST = gschem/gschemdoc.scm.in

MOSTLYCLEANFILES = *.log *~
CLEANFILES = *.log *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *~ Makefile.in
