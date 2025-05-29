if ENABLE_DOC
DOC_DIR=docs
endif

SUBDIRS = \
	examples \
	${DOC_DIR} \
	tests

bin_SCRIPTS = lepton-sch2pcb

lepton-sch2pcb: lepton-sch2pcb.in
	sed -e 's|@leptonbindir[@]|$(bindir)|g' \
	-e 's|@leptondatadir[@]|$(LEPTONDATADIR)|g' $< > $@ \
	&& chmod +x $@

CLEANFILES = lepton-sch2pcb
