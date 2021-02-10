bin_SCRIPTS = \
	lepton-netlist

EXTRA_DIST = \
	lepton-netlist.in

do_subst = sed -e 's,[@]libdir[@],$(libdir),g' \
	-e 's,[@]GUILE[@],$(GUILE),g' \
	-e 's,[@]localedir[@],$(localedir),g' \
	-e 's,[@]ccachedir[@],@LEPTON_SCM_PRECOMPILE_DIR@,g'

lepton-netlist: lepton-netlist.in Makefile
	$(do_subst) < $(srcdir)/$@.in > $@
	chmod +x $@

CLEANFILES = \
	lepton-netlist
