instdirname = sym-gnetman
datasymdir = $(LEPTONDATADIR)/$(instdirname)

dist-hook:
	$(MKDIR_P) $(distdir)
	cp -p $(srcdir)/*.[sS][yY][mM] $(distdir)

install-data-local:
	$(MKDIR_P) $(DESTDIR)$(datasymdir)
	$(INSTALL_DATA) $(srcdir)/*.[sS][yY][mM] $(DESTDIR)$(datasymdir)

uninstall-local:
	rm -fr $(DESTDIR)$(datasymdir)
