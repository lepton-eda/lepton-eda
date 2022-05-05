scmdatadir = $(LEPTONDATADIR)/scheme
godir = $(LEPTONDATADIR)/ccache

nobase_dist_scmdata_DATA = \
	auto-refdes.scm \
	auto-uref.scm \
	pcb.scm \
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
	schematic/autoplace.scm \
	schematic/builtins.scm \
	schematic/core/gettext.scm \
	schematic/dialog.scm \
	schematic/ffi.scm \
	schematic/ffi/gobject.scm \
	schematic/ffi/gtk.scm \
	schematic/doc.scm \
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

# To check it all thoroughly, you could add:
# -Wunused-format -- to test issues with the built-in format()
#  function.
# -Wunused-toplevel -- to test for toplevel exported vars.
# -Wunused-variable -- to test for other unused vars (there are
#  some in (match ...) and other code).

GUILE_WARNINGS = \
	-Warity-mismatch \
	-Wbad-case-datum \
	-Wduplicate-case-datum \
	-Wmacro-use-before-definition \
	-Wshadowed-toplevel \
	-Wunbound-variable \
	-Wunsupported-warning

SOURCES = $(nobase_dist_scmdata_DATA)


# This part depends on the configure switch --enable-guild.
if ENABLE_GUILD

GOBJECTS = $(SOURCES:%.scm=%.go)

SUFFIXES = .scm .go
.scm.go:
	$(AM_V_GEN) \
	LIBLEPTON="${abs_top_builddir}/liblepton/src/liblepton" \
	LIBLEPTONGUI="${abs_top_builddir}/libleptongui/src/libleptongui" \
	$(GUILD) compile \
	-L . \
	-L $(abs_top_srcdir)/liblepton/scheme \
	-L $(abs_top_builddir)/liblepton/scheme \
	-L $(abs_top_srcdir)/libleptongui/scheme \
	-L $(abs_top_builddir)/libleptongui/scheme \
	$(GUILE_WARNINGS) -o "$@" "$<"

nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_dist_scmdataDATA

CLEANFILES = $(GOBJECTS)

endif
