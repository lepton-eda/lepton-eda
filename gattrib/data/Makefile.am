theme=hicolor
themedir=$(datadir)/icons/$(theme)

app_icon = gattrib

app_icon_files = \
	$(app_icon:%=%-48.png) \
	$(app_icon:%=%.svg)
#	$(app_icon:%=%-16.png)
#	$(app_icon:%=%-22.png)
#	$(app_icon:%=%-24.png)
#	$(app_icon:%=%-32.png)
#
theme_icons = \
	$(app_icon_files:%=apps,%)


desktopdir=$(datadir)/applications
desktop_in_files = geda-gattrib.desktop.in
desktop_DATA = $(desktop_in_files:.desktop.in=.desktop)

@INTLTOOL_DESKTOP_RULE@

install_icon_exec = \
	$(top_srcdir)/icon-theme-installer \
	-t $(theme) \
	-s $(srcdir) \
	-d "x$(DESTDIR)" \
	-b $(themedir) \
	-m "$(mkinstalldirs)" \
	-x "$(INSTALL_DATA)"

install-data-local:
	$(install_icon_exec) -i $(theme_icons)


install-data-hook:
if ENABLE_UPDATE_DESKTOP_DATABASE
	$(UPDATE_DESKTOP_DATABASE) "$(DESTDIR)$(desktopdir)"
endif

uninstall-hook:
if ENABLE_UPDATE_DESKTOP_DATABASE
	$(UPDATE_DESKTOP_DATABASE) "$(DESTDIR)$(desktopdir)"
endif
	$(install_icon_exec) -u $(theme_icons)

MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in
DISTCLEANFILES = $(desktop_DATA)
EXTRA_DIST = $(app_icon_files) \
             $(desktop_in_files) $(desktop_DATA)
