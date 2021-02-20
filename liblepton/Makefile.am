SUBDIRS = po data docs include lib src scheme tests

pkgconfigdir            = $(libdir)/pkgconfig
pkgconfig_DATA          = liblepton.pc

liblepton-pc-install: liblepton.pc
	$(mkinstalldirs) $(DESTDIR)$(pkgconfigdir)
	$(INSTALL_DATA) liblepton.pc $(DESTDIR)$(pkgconfigdir)
