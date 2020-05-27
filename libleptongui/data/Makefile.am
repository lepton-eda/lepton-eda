theme=hicolor

# --------------------------------------------------------------------
# These icons get installed to a gEDA-specific icons
# directory. They're used within gschem.
icondir = $(LEPTONDATADIR)/icons/$(theme)
iconsvg = gschem-icons.svg

action_icons = \
	insert-box \
	insert-line \
	insert-circle \
	insert-arc \
	insert-path \
	insert-symbol \
	insert-net \
	insert-bus \
	insert-pin \
	insert-attribute \
	insert-text \
	select \
	symbol-datasheet \
	attribute-attach \
	attribute-detach \
	attribute-show-value \
	attribute-show-name \
	attribute-show-both \
	clone \
	multi-clone \
	deselect

action_icon_files = \
	$(action_icons:%=%-16.png) \
	insert-symbol-22.png \
	insert-net-22.png \
	insert-bus-22.png \
	insert-text-22.png \
	select-22.png

regen-svg-icons: $(iconsvg)
	if test -x "`which inkscape`"; then \
	  for f in $(action_icon_files); do \
	    id=`echo $$f | sed -e's:.png::'`; \
	    inkscape --export-use-hints \
		     --export-id=$$id \
		     --export-png=$(srcdir)/$$f \
		     $(srcdir)/$(iconsvg); \
	  done; \
	fi

icons = \
	$(action_icon_files:%=actions,%)

# --------------------------------------------------------------------
# The application icon gets installed to the system icons
# directory. It's used by e.g. the desktop environment to make buttons
# that launch gEDA look nice.
themedir=$(xdgdatadir)/icons/$(theme)

app_icon = lepton-schematic

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

# --------------------------------------------------------------------
# We also install a .desktop file.
xdgdatadir=@XDGDATADIR@

desktopdir=$(xdgdatadir)/applications
desktop_in_files = lepton-schematic.desktop.in
desktop_DATA = $(desktop_in_files:.desktop.in=.desktop)

DOMAIN=libleptongui

@DESKTOP_I18N_LANGS_RULE@
@DESKTOP_I18N_DESKTOP_RULE@

# --------------------------------------------------------------------
# Special rules for installing icon files
install_icons_exec = \
	$(ICON_THEME_INSTALLER) \
	-t $(theme) \
	-s $(srcdir) \
	-d "x$(DESTDIR)" \
	-b $(icondir) \
	-m "$(mkinstalldirs)" \
	-x "$(INSTALL_DATA)"
install_theme_icons_exec = \
	$(ICON_THEME_INSTALLER) \
	-t $(theme) \
	-s $(srcdir) \
	-d "x$(DESTDIR)" \
	-b $(themedir) \
	-m "$(mkinstalldirs)" \
	-x "$(INSTALL_DATA)"

install-data-local:
	$(install_icons_exec) -i $(icons)
	$(install_theme_icons_exec) -i $(theme_icons)


install-data-hook:
if ENABLE_UPDATE_XDG_DATABASE
	$(UPDATE_DESKTOP_DATABASE) "$(DESTDIR)$(desktopdir)"
endif

uninstall-hook:
if ENABLE_UPDATE_XDG_DATABASE
	$(UPDATE_DESKTOP_DATABASE) "$(DESTDIR)$(desktopdir)"
endif
	$(install_icons_exec) -u $(icons)
	$(install_theme_icons_exec) -u $(theme_icons)

CLEANFILES = $(desktop_DATA)
EXTRA_DIST = $(iconsvg) $(action_icon_files) \
	     $(app_icon_files) \
	     $(desktop_in_files) $(desktop_DATA)
