if ENABLE_DOC
DOC_DIR=docs
endif

SUBDIRS = \
	${DOC_DIR} \
	tests

bin_SCRIPTS = lepton-archive

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


# This part depends on the configure switch --enable-guild.
if ENABLE_GUILD

lepton-archive.go: lepton-archive
	$(AM_V_GEN) \
	LIBLEPTON="${abs_top_builddir}/liblepton/src/liblepton" \
	$(GUILD) compile \
	-L . \
	-L $(abs_top_srcdir)/liblepton/scheme \
	-L $(abs_top_builddir)/liblepton/scheme \
	$(GUILE_WARNINGS) -o "$@" "$<"

nobase_go_DATA = lepton-archive.go

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-binSCRIPTS

CLEANFILES = $(nobase_go_DATA)

endif
