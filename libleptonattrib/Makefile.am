bin_SCRIPTS = lepton-attrib

if CYGWIN
LIBLEPTONATTRIB=cygleptonattrib-2
if ENABLE_GTK3
LIBGTK = cyggtk-3-0
else
LIBGTK = cyggtk-x11-2.0-0
endif
else
LIBLEPTONATTRIB=libleptonattrib
if ENABLE_GTK3
LIBGTK = libgtk-3
else
LIBGTK = libgtk-x11-2.0
endif
endif

SUBDIRS = po src include data docs

pkgconfigdir            = $(libdir)/pkgconfig
pkgconfig_DATA          = libleptonattrib.pc

libleptonattrib-pc-install: libleptonattrib.pc
	$(mkinstalldirs) $(DESTDIR)$(pkgconfigdir)
	$(INSTALL_DATA) libleptonattrib.pc $(DESTDIR)$(pkgconfigdir)

lepton-attrib: lepton-attrib.scm Makefile
	$(do_subst) < $(srcdir)/$@.scm > $@
	chmod +x $@

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	( \
	  cd $(srcdir) && \
	  $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- . || \
	  echo "WARNING: ChangeLog information not available from git" >&2 ; \
	) > $@
endif HAVE_GIT_REPO

do_subst = sed \
	-e 's,[@]libdir[@],$(libdir),g' \
	-e 's,[@]GUILE[@],$(GUILE),g' \
	-e 's,[@]LIBGTK[@],$(LIBGTK),g' \
	-e 's,[@]LIBLEPTONATTRIB[@],$(LIBLEPTONATTRIB),g' \
	-e 's,[@]localedir[@],$(localedir),g' \
	-e 's,[@]ccachedir[@],@LEPTON_SCM_PRECOMPILE_DIR@,g'

EXTRA_DIST = lepton-attrib.scm NOTES README ChangeLog ToDos

CLEANFILES = $(bin_SCRIPTS)
