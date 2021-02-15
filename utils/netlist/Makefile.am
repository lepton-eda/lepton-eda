SUBDIRS = scheme examples tests docs

bin_SCRIPTS = \
	lepton-netlist

EXTRA_DIST = \
	ChangeLog \
	lepton-netlist.in

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	( \
	  cd $(srcdir) && \
	  $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- . || \
	  echo "WARNING: ChangeLog information not available from git" >&2 ; \
	) > $@
endif HAVE_GIT_REPO

do_subst = sed -e 's,[@]libdir[@],$(libdir),g' \
	-e 's,[@]GUILE[@],$(GUILE),g' \
	-e 's,[@]localedir[@],$(localedir),g' \
	-e 's,[@]ccachedir[@],@LEPTON_SCM_PRECOMPILE_DIR@,g'

lepton-netlist: lepton-netlist.in Makefile
	$(do_subst) < $(srcdir)/$@.in > $@
	chmod +x $@

CLEANFILES = \
	lepton-netlist
