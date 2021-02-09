bin_SCRIPTS = \
	lepton-netlist \
	lepton-pcb_backannotate \
	lepton-refdes_renum \
	lepton-schdiff \
	lepton-symcheck \
	lepton-upcfg

EXTRA_DIST = \
	lepton-netlist.in \
	lepton-pcb_backannotate \
	lepton-refdes_renum \
	lepton-schdiff \
	lepton-symcheck.in \
	lepton-upcfg.in

do_subst = sed -e 's,[@]libdir[@],$(libdir),g' \
	-e 's,[@]GUILE[@],$(GUILE),g' \
	-e 's,[@]localedir[@],$(localedir),g' \
	-e 's,[@]ccachedir[@],@LEPTON_SCM_PRECOMPILE_DIR@,g'

lepton-upcfg: lepton-upcfg.in
	$(do_subst) < $(srcdir)/$@.in > $@
	chmod +x $@

lepton-symcheck: lepton-symcheck.in
	$(do_subst) < $(srcdir)/$@.in > $@
	chmod +x $@

lepton-netlist: lepton-netlist.in Makefile
	$(do_subst) < $(srcdir)/$@.in > $@
	chmod +x $@

CLEANFILES = \
	lepton-upcfg \
	lepton-symcheck \
	lepton-netlist
