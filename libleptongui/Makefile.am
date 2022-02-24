SUBDIRS = \
	bitmap \
	data \
	docs \
	examples \
	include \
	lib \
	po \
	src \
	scheme \
	tests

pkgconfigdir   = $(libdir)/pkgconfig
pkgconfig_DATA = libleptongui.pc

libleptongui-pc-install: libleptongui.pc
	$(mkinstalldirs) $(DESTDIR)$(pkgconfigdir)
	$(INSTALL_DATA) libleptongui.pc $(DESTDIR)$(pkgconfigdir)

maintainer-clean-local:
	-rm -rf po/*.sed po/*.header po/*.sin po/*.template po/Rules-quot
