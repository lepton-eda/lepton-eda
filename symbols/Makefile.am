symbol_dirs = \
	4000 \
	74 \
	IEC417 \
	allegro \
	altera \
	amphenol \
	analog \
	apex \
	asic \
	asicpads \
	bus \
	cascade \
	connector \
	dec \
	diode \
	ecl \
	gnetman \
	idt \
	io \
	irf \
	lattice \
	linear \
	local \
	maxim \
	memory \
	micro \
	minicircuits \
	misc \
	national \
	opto \
	philips \
	pla \
	power \
	radio \
	relay \
	rf \
	spice \
	st \
	supervisor \
	switcap \
	switch \
	titleblock \
	transistor \
	tube \
	verilog \
	vhdl \
	xilinx \
	font

SUBDIRS = documentation

scmdatadir = $(GEDADATADIR)/scheme
dist_scmdata_DATA = geda-font.scm

gafrcddir = $(GEDADATADIR)/gafrc.d
dist_gafrcd_DATA = geda-clib.scm

datasymdir = $(GEDADATADIR)/sym
nobase_dist_datasym_DATA = \
	vhdl/verilog2vhdl.sh vhdl/script.sed

EXTRA_DIST = ChangeLog-1.0 \
	font/README font/ChangeLog font/ChangeLog-1.0 \
	radio/README

# Copies symbols into distribution
dist-hook:
	@for lib in $(symbol_dirs); do \
	  $(MKDIR_P) $(distdir)/$$lib && \
	  list=`(ls $(srcdir)/$$lib) | grep '.[Ss][Yy][Mm]$$'`; \
	  for file in $$list; do \
	    cp -p $(srcdir)/$$lib/$$file $(distdir)/$$lib || exit $$?; \
	  done; \
	done;

# Installs symbols
install-data-local:
	@$(NORMAL_INSTALL)
	@for lib in $(symbol_dirs); do \
	  d="$(DESTDIR)$(datasymdir)/$$lib"; \
	  $(MKDIR_P) "$$d"; \
	  list=`(ls $(srcdir)/$$lib) | grep '.[Ss][Yy][Mm]$$'`; \
	  if test -n "$$list"; then \
	    echo " ( cd $(srcdir)/$$lib && $(INSTALL_DATA)" $$list "'$$d' )"; \
	    (cd $(srcdir)/$$lib && $(INSTALL_DATA) $$list "$$d") || exit $$?; \
	  fi; \
	done

# Uninstalls symbols
uninstall-local:
	@$(NORMAL_UNINSTALL)
	@for lib in $(symbol_dirs); do \
	  d="$(DESTDIR)$(datasymdir)/$$lib"; \
	  list=`(ls $(srcdir)/$$lib) | grep '\.[Ss][Yy][Mm]$$'`; \
	  if test -n "$$list"; then \
	    echo " ( cd '$$d' && rm -f" $$list ")"; \
	    (cd "$$d" && rm -f $$list) || exit $$?; \
	  fi; \
	done

MOSTLYCLEANFILES = *.log *~
CLEANFILES = *.log *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *~ Makefile.in
