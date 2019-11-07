bin_SCRIPTS = \
	lepton-archive \
	lepton-pcb_backannotate \
	lepton-refdes_renum \
	lepton-schdiff \
	lepton-symfix \
	lepton-tragesym

noinst_SCRIPTS = gschupdate gsymupdate

EXTRA_DIST = \
	gschupdate \
	gsymupdate \
	lepton-archive.in \
	lepton-pcb_backannotate \
	lepton-refdes_renum \
	lepton-schdiff \
	lepton-symfix \
	lepton-tragesym

do_subst = sed -e 's,[@]libdir[@],$(libdir),g' \
	-e 's,[@]GUILE[@],$(GUILE),g' \
	-e 's,[@]ccachedir[@],@LEPTON_SCM_PRECOMPILE_DIR@,g'

lepton-archive: lepton-archive.in
	$(do_subst) < $(srcdir)/$@.in > $@
	chmod +x $@

CLEANFILES = lepton-archive
MAINTAINERCLEANFILES = Makefile.in
