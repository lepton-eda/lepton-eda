scmdatadir = @LEPTONDATADIR@/scheme
nobase_scmdata_DATA = $(DIST_SCM_BACKENDS)

config-netlist.scm: config-netlist.scm.in
	sed -e 's,[@]backenddir[@],$(scmdatadir)/backend,g' < $(srcdir)/$@.in > $@

gafrcddir = @LEPTONDATADIR@/scheme/autoload
gafrcd_DATA = config-netlist.scm


DIST_SCM_BACKENDS = \
	backend/gnet-geda.scm \
	backend/gnet-gsch2pcb.scm

EXTRA_DIST = $(DIST_SCM_BACKENDS) config-netlist.scm.in

CLEANFILES = config-netlist.scm
