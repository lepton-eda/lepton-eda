scmdatadir = $(LEPTONDATADIR)/scheme
godir = $(LEPTONDATADIR)/ccache

nobase_dist_scmdata_DATA = \
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
	schematic/action-mode.scm \
	schematic/action/copy.scm \
	schematic/action/delete.scm \
	schematic/action/edit.scm \
	schematic/attrib.scm \
	schematic/buffer.scm \
	schematic/builtins.scm \
	schematic/callback.scm \
	schematic/canvas.scm \
	schematic/canvas/foreign.scm \
	schematic/dialog.scm \
	schematic/dialog/file-select.scm \
	schematic/dialog/multiattrib.scm \
	schematic/dialog/new-text.scm \
	schematic/dialog/slot-edit.scm \
	schematic/doc.scm \
	schematic/event.scm \
	schematic/ffi.scm \
	schematic/ffi/gtk.scm \
	schematic/gettext.scm \
	schematic/gtk/helper.scm \
	schematic/gui/keymap.scm \
	schematic/gui/stroke.scm \
	schematic/hook.scm \
	schematic/keymap.scm \
	schematic/menu.scm \
	schematic/mouse-pointer.scm \
	schematic/netlist.scm \
	schematic/pcb.scm \
	schematic/precompile.scm \
	schematic/preview-widget.scm \
	schematic/refdes.scm \
	schematic/repl.scm \
	schematic/selection.scm \
	schematic/sellock.scm \
	schematic/symbol/check.scm \
	schematic/toolbar.scm \
	schematic/util.scm \
	schematic/undo.scm \
	schematic/viewport/foreign.scm \
	schematic/window.scm \
	schematic/window/foreign.scm \
	schematic/window/global.scm \
	schematic/window/list.scm \
	conf/schematic/attribs.scm \
	conf/schematic/deprecated.scm \
	conf/schematic/keys.scm \
	conf/schematic/menu.scm \
	conf/schematic/stroke.scm

# To check it all thoroughly, you could add:
# -Wformat -- to test issues with the built-in format() function.
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
