## This Makefile.am created for gattrib by SDB 12.13.2003

bin_PROGRAMS = lepton-attrib

## don't forget all *.h files */
lepton_attrib_SOURCES = \
		gtksheet_2_2.c \
		gtkextra-marshal.c \
		globals.c \
		listsort.c \
		parsecmd.c \
		f_export.c \
		i_vars.c \
		i_basic.c \
		x_window.c \
		x_gtksheet.c \
		x_dialog.c \
		x_fileselect.c \
		s_misc.c \
		s_string_list.c \
		s_sheet_data.c \
		s_table.c \
		s_object.c \
		s_attrib.c \
		s_toplevel.c \
		s_visibility.c \
		lepton-attrib.c


lepton_attrib_CPPFLAGS = -I$(top_srcdir)/liblepton/include \
	-I$(srcdir)/../include -I$(top_srcdir) -I$(includedir)
lepton_attrib_CFLAGS = $(GCC_CFLAGS) $(GLIB_CFLAGS) \
	$(MINGW_CFLAGS) \
	$(GTK_CFLAGS) $(GTHREAD_CFLAGS) $(GUILE_CFLAGS)
lepton_attrib_LDFLAGS = $(GLIB_LIBS) $(GTK_LIBS) $(GTHREAD_LIBS) \
	$(GUILE_LIBS) $(MINGW_GUI_LDFLAGS)
lepton_attrib_LDADD = $(top_builddir)/liblepton/src/liblepton.la


if INSTALL_COMPAT_SYMLINKS
NAME = lepton-attrib$(EXEEXT)
COMPAT_NAME = gattrib$(EXEEXT)
install-exec-hook:
	cd $(DESTDIR)$(bindir) && \
	if [ -h $(COMPAT_NAME) ]; then rm -f $(COMPAT_NAME); fi && \
	ln -s $(NAME) $(COMPAT_NAME)
uninstall-hook:
	rm -f $(DESTDIR)$(bindir)/$(COMPAT_NAME)
endif INSTALL_COMPAT_SYMLINKS


localedir = @datadir@/locale
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@

MAINTAINERCLEANFILES = Makefile.in
