if ENABLE_DOC
DOC_DIR=docs
endif

SUBDIRS = \
	po \
	src \
	include \
	${DOC_DIR} \
	data

pkgconfigdir            = $(libdir)/pkgconfig
pkgconfig_DATA          = libleptonattrib.pc

libleptonattrib-pc-install: libleptonattrib.pc
	$(mkinstalldirs) $(DESTDIR)$(pkgconfigdir)
	$(INSTALL_DATA) libleptonattrib.pc $(DESTDIR)$(pkgconfigdir)
