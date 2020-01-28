## -*- Makefile -*-

scmdatadir = $(GEDADATADIR)/scheme
nobase_dist_scmdata_DATA = \
	auto-refdes.scm \
	auto-uref.scm \
	auto-place-attribs.scm \
	auto-place-netname.scm \
	pcb.scm \
	default-attrib-positions.scm \
	gschem/action.scm \
	gschem/attrib.scm \
	gschem/builtins.scm \
	gschem/gschemdoc.scm \
	gschem/hook.scm \
	gschem/keymap.scm \
	gschem/window.scm \
	gschem/selection.scm \
	gschem/symbol/check.scm \
	gschem/util.scm \
	gschem/deprecated.scm \
	schematic/action.scm \
	schematic/attrib.scm \
	schematic/builtins.scm \
	schematic/core/gettext.scm \
	schematic/gui/keymap.scm \
	schematic/gui/stroke.scm \
	schematic/hook.scm \
	schematic/keymap.scm \
	schematic/netlist.scm \
	schematic/precompile.scm \
	schematic/repl.scm \
	schematic/selection.scm \
	schematic/symbol/check.scm \
	schematic/util.scm \
	schematic/undo.scm \
	conf/schematic/attribs.scm \
	conf/schematic/deprecated.scm \
	conf/schematic/keys.scm \
	conf/schematic/menu.scm \
	conf/schematic/stroke.scm

nobase_scmdata_DATA = schematic/gschemdoc.scm

schematic/gschemdoc.scm: schematic/gschemdoc.scm.in
	$(MKDIR_P) schematic/; \
	sed -e 's,[@]docdir[@],$(docdir),g' < $(srcdir)/$@.in > $@

EXTRA_DIST = schematic/gschemdoc.scm.in

CLEANFILES = schematic/gschemdoc.scm schematic/precompile-config.scm
MAINTAINERCLEANFILES = Makefile.in
