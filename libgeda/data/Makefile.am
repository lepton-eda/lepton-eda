theme=hicolor
xdgdatadir=@XDGDATADIR@
themedir=$(xdgdatadir)/icons/$(theme)

mime_icons = \
	application-x-geda-gsch2pcb-project \
	application-x-geda-schematic        \
	application-x-geda-symbol

mime_icon_files = \
	$(mime_icons:%=%-16.png) \
	$(mime_icons:%=%-22.png) \
	$(mime_icons:%=%-24.png) \
	$(mime_icons:%=%-32.png) \
	$(mime_icons:%=%-48.png) \
	$(mime_icons:%=%.svg)

mime_icon_sources = \
	$(mime_icons:%=%-16.svg) \
	$(mime_icons:%=%-22.svg) \
	$(mime_icons:%=%-32.svg) \
	$(mime_icons:%=%-48.svg)

theme_icons = \
	$(mime_icon_files:%=mimetypes,%)

mimedir = $(xdgdatadir)/mime
xmldir = $(mimedir)/packages

xml_in_files = libgeda.xml.in
xml_DATA = $(xml_in_files:.xml.in=.xml)

@INTLTOOL_XML_RULE@

kdedatadir=@KDEDATADIR@
mimelnkdir = $(kdedatadir)/mimelnk
applicationdir = $(mimelnkdir)/application

application_in_files = \
	x-geda-gsch2pcb-project.desktop.in \
	x-geda-schematic.desktop.in \
	x-geda-symbol.desktop.in
application_DATA = $(application_in_files:.desktop.in=.desktop)

@INTLTOOL_DESKTOP_RULE@

install_icon_exec = \
	$(SHELL) $(top_srcdir)/icon-theme-installer \
	-t $(theme) \
	-s $(srcdir) \
	-d "x$(DESTDIR)" \
	-b $(themedir) \
	-m "$(mkinstalldirs)" \
	-x "$(INSTALL_DATA)"

install-data-local:
	$(install_icon_exec) -i $(theme_icons)

install-data-hook:
if ENABLE_UPDATE_MIME_DATABASE
	$(UPDATE_MIME_DATABASE) "$(DESTDIR)$(mimedir)"
endif

uninstall-hook:
if ENABLE_UPDATE_MIME_DATABASE
	$(UPDATE_MIME_DATABASE) "$(DESTDIR)$(mimedir)"
endif
	$(install_icon_exec) -u $(theme_icons)

MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in
DISTCLEANFILES = $(xml_DATA) $(application_DATA)
EXTRA_DIST = $(mime_icon_files) $(mime_icon_sources) \
             $(xml_in_files) $(xml_DATA) \
             $(application_in_files)

