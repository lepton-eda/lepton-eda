SUBDIRS = scheme examples tests docs

bin_SCRIPTS = \
	lepton-netlist

EXTRA_DIST = \
	lepton-netlist.scm

do_subst = sed -e 's,[@]libdir[@],$(libdir),g' \
	-e 's,[@]GUILE[@],$(GUILE),g' \
	-e 's,[@]LEPTON_SCHEME_DIR[@],$(LEPTON_SCHEME_DIR),g' \
	-e 's,[@]localedir[@],$(localedir),g' \
	-e 's,[@]ccachedir[@],@LEPTON_CCACHE_DIR@,g'

lepton-netlist: lepton-netlist.scm
	$(do_subst) < $(srcdir)/$@.scm > $@
	chmod +x $@

CLEANFILES = \
	lepton-netlist
