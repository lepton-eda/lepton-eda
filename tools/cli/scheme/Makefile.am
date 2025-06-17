bin_SCRIPTS = \
	lepton-cli \
	lepton-config \
	lepton-export \
	lepton-shell

godir = $(LEPTONDATADIR)/ccache

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

GOBJECTS = $(bin_SCRIPTS:%=%.go)

# This part depends on the configure switch --enable-guild.
if ENABLE_GUILD

lepton-config.go: lepton-config
	$(AM_V_GEN) \
	LIBLEPTON="${abs_top_builddir}/liblepton/src/liblepton" \
	$(GUILD) compile \
	-L . \
	-L $(abs_top_srcdir)/lepton-eda/scheme \
	-L $(abs_top_builddir)/lepton-eda/scheme \
	$(GUILE_WARNINGS) -o "$@" "$<"

lepton-export.go: lepton-export
	$(AM_V_GEN) \
	LIBLEPTON="${abs_top_builddir}/liblepton/src/liblepton" \
	$(GUILD) compile \
	-L . \
	-L $(abs_top_srcdir)/lepton-eda/scheme \
	-L $(abs_top_builddir)/lepton-eda/scheme \
	$(GUILE_WARNINGS) -o "$@" "$<"

lepton-shell.go: lepton-shell
	$(AM_V_GEN) \
	LIBLEPTON="${abs_top_builddir}/liblepton/src/liblepton" \
	$(GUILD) compile \
	-L . \
	-L $(abs_top_srcdir)/lepton-eda/scheme \
	-L $(abs_top_builddir)/lepton-eda/scheme \
	$(GUILE_WARNINGS) -o "$@" "$<"

lepton-cli.go: lepton-cli
	$(AM_V_GEN) \
	LIBLEPTON="${abs_top_builddir}/liblepton/src/liblepton" \
	$(GUILD) compile \
	-L . \
	-L $(abs_top_srcdir)/lepton-eda/scheme \
	-L $(abs_top_builddir)/lepton-eda/scheme \
	$(GUILE_WARNINGS) -o "$@" "$<"

nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-binSCRIPTS

CLEANFILES = $(GOBJECTS)

endif
