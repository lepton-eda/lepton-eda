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
	xilinx

SUBDIRS = \
	documentation \
	gnetman \
	verilog \
	vhdl

gafrcddir = $(LEPTONDATADIR)/scheme/autoload
dist_gafrcd_DATA = config-symbol-libraries.scm

datasymdir = $(LEPTONDATADIR)/sym

EXTRA_DIST = \
	ChangeLog \
	ChangeLog-1.0 \
	radio/README \
	AUTHORS

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	( \
	  cd $(srcdir) && \
	  $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- . || \
	  echo "WARNING: ChangeLog information not available from git" >&2 ; \
	) > $@
endif HAVE_GIT_REPO

# Copies symbols into distribution
dist-hook:
	@for lib in $(symbol_dirs); do \
	  $(MKDIR_P) $(distdir)/$$lib && \
	  list=`(cd $(srcdir)/$$lib && find . -name '*.[sS][yY][mM]' | sed -e 's:^\./::')`; \
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
	  list=`(cd $(srcdir)/$$lib && find . -name '*.[sS][yY][mM]' | sed -e 's:^\./::')`; \
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
	  list=`(cd $(srcdir)/$$lib && find . -name '*.[sS][yY][mM]' | sed -e 's:^\./::')`; \
	  if test -n "$$list"; then \
	    echo " ( cd '$$d' && rm -f" $$list ")"; \
	    (cd "$$d" && rm -f $$list) || exit $$?; \
	  fi; \
	done

MAINTAINERCLEANFILES = Makefile.in
