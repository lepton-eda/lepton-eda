bin_SCRIPTS = \
	lepton-archive \
	lepton-embed \
	lepton-pcb_backannotate \
	lepton-refdes_renum \
	lepton-schdiff \
	lepton-symfix \
	lepton-tragesym \
	lepton-upcfg

EXTRA_DIST = \
	lepton-archive.in \
	lepton-embed.in \
	lepton-pcb_backannotate \
	lepton-refdes_renum \
	lepton-schdiff \
	lepton-symfix \
	lepton-tragesym.in \
	lepton-upcfg.in

do_subst = sed -e 's,[@]libdir[@],$(libdir),g' \
	-e 's,[@]GUILE[@],$(GUILE),g' \
	-e 's,[@]ccachedir[@],@LEPTON_SCM_PRECOMPILE_DIR@,g'

lepton-archive: lepton-archive.in
	$(do_subst) < $(srcdir)/$@.in > $@
	chmod +x $@

lepton-tragesym: lepton-tragesym.in
	$(do_subst) < $(srcdir)/$@.in > $@
	chmod +x $@

lepton-embed: lepton-embed.in
	$(do_subst) < $(srcdir)/$@.in > $@
	chmod +x $@

lepton-upcfg: lepton-upcfg.in
	$(do_subst) < $(srcdir)/$@.in > $@
	chmod +x $@

CLEANFILES = lepton-archive lepton-embed lepton-tragesym lepton-upcfg
MAINTAINERCLEANFILES = Makefile.in
