if ENABLE_ATTRIB
ATTRIB_DIR=libleptonattrib
endif

if ENABLE_CONTRIB
CONTRIB_DIR=contrib
endif

SUBDIRS = \
	liblepton \
	${ATTRIB_DIR} \
	libleptongui \
	schematic \
	utils \
	symbols \
	docs \
	examples \
	${CONTRIB_DIR}

ACLOCAL_AMFLAGS = -I m4

# We need to ensure that `make distcheck' *doesn't* find a git
# repository!
#
# We also should be able to successfully build & install without
# groff.
false=/bin/sh -c '! :'
DISTCHECK_CONFIGURE_FLAGS = \
	GIT="$(false)" \
	GROFF="$(false)" \
	--disable-update-xdg-database

BUILT_SOURCES = version.h

TEST_LOG_DRIVER = $(GUILE) $(top_srcdir)/build-tools/test-driver.scm
AM_TESTS_ENVIRONMENT = env \
	LANG=C \
	abs_top_builddir='$(abs_top_builddir)' \
	abs_top_srcdir='$(abs_top_srcdir)' \
	GUILE_AUTO_COMPILE='0' \
	GUILE_LOAD_PATH='$(abs_top_srcdir)/tests' \
	GUILE_LOAD_COMPILED_PATH='' \
	LEPTON_INHIBIT_RC_FILES='yes' \
	COMPARE='$(IM_COMPARE)' \
	CONVERT='$(IM_CONVERT)' \
	IDENTIFY='$(IM_IDENTIFY)' \
	FILE='$(FILE)'

TESTS = \
	tests/archive.test \
	tests/cli.test \
	tests/config.test \
	tests/embed.test \
	tests/export.test \
	tests/netlist.test \
	tests/shell.test \
	tests/symcheck.test \
	tests/update.test

EXTRA_DIST = \
	autogen.sh \
	version.h \
	version.h.in \
	CODE_OF_CONDUCT.md \
	CONTRIBUTING.md \
	COPYING \
	COPYING.LGPL \
	NEWS.md \
	README.md \
	ChangeLog \
	tests/env.scm \
	$(TESTS)

# Generate version.h. This works by looking at any git repository that
# is present in the source directory (detected at configure time). If
# there is a git repository, the HEAD commitid is checked every time
# the package is built to see if it has changed. If so, version.h is
# regenerated. If there isn't a git repository, version.h is just
# assumed to be present (since the generated version.h is distributed
# in the source tarball).
if HAVE_GIT_REPO
version.h: stamp-git $(top_builddir)/configure $(srcdir)/version.h.in
	@if test ! -f version.h; then \
	  rm -f stamp-git; \
	  $(MAKE) $(AM_MAKEFLAGS) stamp-git; \
	else :; fi

	@date_ver=$(DATE_VERSION); dotted_ver=$(DOTTED_VERSION); \
	git_commit=`cd $(srcdir) && $(GIT) rev-parse HEAD`; \
	sed -e"s:^.*\(PACKAGE_DATE_VERSION\).*$$:#define \1 \"$$date_ver\":" \
	    -e"s:^.*\(PACKAGE_DOTTED_VERSION\).*$$:#define \1 \"$$dotted_ver\":" \
	    -e"s:^.*\(PACKAGE_GIT_COMMIT\).*$$:#define \1 \"$$git_commit\":" \
	      < $(srcdir)/version.h.in > version.h.new; \
	if diff version.h version.h.new > /dev/null 2>&1; then \
	  echo "version.h is unchanged"; \
	  rm -f version.h.new; \
	else \
	  echo "Recreating version.h"; \
	  mv version.h.new version.h; \
	fi
stamp-git:
	@last_version=`if test -f stamp-git; then cat stamp-git; else :; fi`; \
	now_version=`cd $(srcdir) && $(GIT) rev-parse HEAD`; \
	if test "X$$now_version" != "X$$last_version"; then \
	  echo $$now_version > stamp-git; \
	  echo "Now at git commit $$now_version..."; \
	fi
.PHONY: stamp-git
endif HAVE_GIT_REPO

doxygen:
if ENABLE_DOXYGEN
	@for subdir in libleptonattrib/docs libleptongui/docs \
	  liblepton/docs ; do \
	  (cd $$subdir; $(MAKE) doxygen) || exit 1; \
	done
else
	@echo 'configure was not run with the --enable-doxygen switch.'
	@echo 'The documentation cannot be built from the sources.'
endif


precompile:
	@LEPTON_SCM_PRECOMPILE=1 \
	LEPTON_SCM_PRECOMPILE_SCRIPT=$(datadir)/lepton-eda/scheme/schematic/precompile.scm \
	LEPTON_SCM_PRECOMPILE_SCM_DIR=$(datadir)/lepton-eda/scheme \
	LEPTON_SCM_PRECOMPILE_OUT_DIR=$(datadir)/lepton-eda/ccache \
	LEPTON_SCM_PRECOMPILE_RECURSIVE=1 \
	LEPTON_SCM_PRECOMPILE_FNAME_EXT=.scm \
	$(bindir)/lepton-schematic


install-data-local:
	$(MKDIR_P) $(DESTDIR)$(docdir)
	cp -v $(srcdir)/README.md $(DESTDIR)$(docdir)
	cp -v $(srcdir)/NEWS.md $(DESTDIR)$(docdir)

uninstall-local:
	rm -vfr $(DESTDIR)$(includedir)/liblepton
	rm -vfr $(DESTDIR)$(includedir)/libleptonattrib
	rm -vfr $(DESTDIR)$(pkgdatadir)
	rm -vfr $(DESTDIR)$(docdir)

CLEANFILES = stamp-git
