bin_SCRIPTS = \
	lepton-archive \
	lepton-embed \
	lepton-netlist \
	lepton-pcb_backannotate \
	lepton-refdes_renum \
	lepton-schdiff \
	lepton-symcheck \
	lepton-symfix \
	lepton-upcfg

EXTRA_DIST = \
	lepton-archive.in \
	lepton-embed.in \
	lepton-netlist.in \
	lepton-pcb_backannotate \
	lepton-refdes_renum \
	lepton-schdiff \
	lepton-symfix \
	lepton-symcheck.in \
	lepton-upcfg.in

do_subst = sed -e 's,[@]libdir[@],$(libdir),g' \
	-e 's,[@]GUILE[@],$(GUILE),g' \
	-e 's,[@]localedir[@],$(localedir),g' \
	-e 's,[@]ccachedir[@],@LEPTON_SCM_PRECOMPILE_DIR@,g'

lepton-archive: lepton-archive.in
	$(do_subst) < $(srcdir)/$@.in > $@
	chmod +x $@

lepton-embed: lepton-embed.in
	$(do_subst) < $(srcdir)/$@.in > $@
	chmod +x $@

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
	lepton-archive \
	lepton-embed \
	lepton-upcfg \
	lepton-symcheck \
	lepton-netlist
