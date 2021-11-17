## -*- Makefile -*-

scmdatadir = $(LEPTONDATADIR)/scheme
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
	gschem/selection.scm \
	gschem/symbol/check.scm \
	gschem/util.scm \
	gschem/window.scm \
	gschem/deprecated.scm \
	schematic/action.scm \
	schematic/attrib.scm \
	schematic/builtins.scm \
	schematic/core/gettext.scm \
	schematic/dialog.scm \
	schematic/gui/keymap.scm \
	schematic/gui/stroke.scm \
	schematic/hook.scm \
	schematic/keymap.scm \
	schematic/menu.scm \
	schematic/netlist.scm \
	schematic/precompile.scm \
	schematic/repl.scm \
	schematic/selection.scm \
	schematic/symbol/check.scm \
	schematic/util.scm \
	schematic/undo.scm \
	schematic/window.scm \
	conf/schematic/attribs.scm \
	conf/schematic/deprecated.scm \
	conf/schematic/keys.scm \
	conf/schematic/menu.scm \
	conf/schematic/stroke.scm

nobase_scmdata_DATA = \
	schematic/gschemdoc.scm \
	schematic/ffi/gobject.scm \
	schematic/ffi/gtk.scm \
	schematic/ffi.scm

if CYGWIN
LIBLEPTONGUI=cygleptongui-$(LIBLEPTONGUI_MAJOR)
LIBGOBJECT=cyggobject-2.0-0
else
LIBLEPTONGUI=libleptongui
LIBGOBJECT=libgobject-2.0
endif

schematic/gschemdoc.scm: schematic/gschemdoc.scm.in
	$(MKDIR_P) schematic/; \
	sed -e 's,[@]docdir[@],$(docdir),g' < $(srcdir)/$@.in > $@

schematic/ffi/gtk.scm: schematic/ffi/gtk.scm.in
	$(MKDIR_P) schematic/ffi/; \
	sed -e 's,[@]LIBGTK[@],$(LIBGTK),g' < $(srcdir)/$@.in > $@

schematic/ffi/gobject.scm: schematic/ffi/gobject.scm.in
	$(MKDIR_P) schematic/ffi/; \
	sed -e 's,[@]LIBGOBJECT[@],$(LIBGOBJECT),g' < $(srcdir)/$@.in > $@

schematic/ffi.scm: schematic/ffi.scm.in
	$(MKDIR_P) schematic/; \
	sed -e 's,[@]LIBLEPTONGUI[@],$(LIBLEPTONGUI),g' < $(srcdir)/$@.in > $@


EXTRA_DIST = \
	schematic/gschemdoc.scm.in \
	schematic/ffi/gobject.scm.in \
	schematic/ffi/gtk.scm.in \
	schematic/ffi.scm.in

CLEANFILES = \
	schematic/gschemdoc.scm \
	schematic/precompile-config.scm \
	schematic/ffi/gobject.scm \
	schematic/ffi/gtk.scm \
	schematic/ffi.scm
