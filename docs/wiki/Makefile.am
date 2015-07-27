wikidir = $(docdir)/wiki

# Copy wiki into distribution
dist-hook:
	@for dir in $$(cd $(srcdir) && find . -type d -print | sed -e's:^\./::' ); do \
	  $(MKDIR_P) $(distdir)/$$dir; \
	  list=`(cd $(srcdir)/$$dir && find . -maxdepth 1 -type f \! -name 'Makefile*' \! -name '.gitignore' -print | sed -e 's:^\./::')`; \
	  for file in $$list; do \
	    cp -p $(srcdir)/$$dir/$$file $(distdir)/$$dir || exit $$?; \
	  done; \
	done

# Install wiki
install-data-local:
	@for dir in $$(cd $(srcdir) && find . -type d -print | sed -e's:^\./::' ); do \
	  d="$(DESTDIR)$(wikidir)/$$dir"; \
	  $(MKDIR_P) $$d; \
	  list=`(cd $(srcdir)/$$dir && find . -maxdepth 1 -type f \! -name 'Makefile*' \! -name '.gitignore' -print | sed -e 's:^\./::')`; \
	  if test -n "$$list"; then \
	    echo " ( cd $(srcdir)/$$dir && $(INSTALL_DATA)" $$list "'$$d' )"; \
	    (cd $(srcdir)/$$dir && $(INSTALL_DATA) $$list "$$d") || exit $$?; \
	  fi; \
	done

# Uninstall wiki
uninstall-local:
	@for dir in $$(cd $(srcdir) && find . -type d -print | sed -e's:^\./::' ); do \
	  d="$(DESTDIR)$(wikidir)/$$dir"; \
	  list=`(cd $(srcdir)/$$dir && find . -maxdepth 1 -type f \! -name 'Makefile*' \! -name '.gitignore' -print | sed -e 's:^\./::')`; \
	  if test -n "$$list"; then \
	    echo " ( cd '$$d' && rm -f" $$list ")"; \
	    (cd "$$d" && rm -f $$list) || exit $$?; \
	  fi; \
	done

MAINTAINERCLEANFILES = Makefile.in
