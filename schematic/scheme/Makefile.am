## -*- Makefile -*-

scmdatadir = $(GEDADATADIR)/scheme
nobase_dist_scmdata_DATA = \
	auto-refdes.scm \
	auto-uref.scm \
	auto-place-attribs.scm \
	auto-place-netname.scm \
	pcb.scm \
	default-attrib-positions.scm \
	gschem/core/gettext.scm \
	gschem/window.scm \
	gschem/selection.scm \
	gschem/deprecated.scm \
	gschem/hook.scm \
	gschem/attrib.scm \
	gschem/keymap.scm \
	gschem/util.scm \
	gschem/action.scm \
	gschem/builtins.scm \
	gschem/symbol/check.scm \
	schematic/gui/keymap.scm \
	schematic/gui/stroke.scm \
	schematic/netlist.scm \
	schematic/precompile.scm \
	schematic/repl.scm \
	schematic/undo.scm \
	conf/schematic/attribs.scm \
	conf/schematic/deprecated.scm \
	conf/schematic/keys.scm \
	conf/schematic/menu.scm \
	conf/schematic/stroke.scm

nobase_scmdata_DATA = gschem/gschemdoc.scm

gschem/gschemdoc.scm: gschem/gschemdoc.scm.in
	$(MKDIR_P) gschem/; \
	sed -e 's,[@]docdir[@],$(docdir),g' < $(srcdir)/$@.in > $@

EXTRA_DIST = gschem/gschemdoc.scm.in

MOSTLYCLEANFILES = *.log *~
CLEANFILES = *.log *~ gschem/gschemdoc.scm schematic/precompile-config.scm
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *~ Makefile.in
