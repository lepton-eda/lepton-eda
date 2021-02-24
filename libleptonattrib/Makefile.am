SUBDIRS = po src include data docs

pkgconfigdir            = $(libdir)/pkgconfig
pkgconfig_DATA          = libleptonattrib.pc

libleptonattrib-pc-install: libleptonattrib.pc
	$(mkinstalldirs) $(DESTDIR)$(pkgconfigdir)
	$(INSTALL_DATA) libleptonattrib.pc $(DESTDIR)$(pkgconfigdir)
