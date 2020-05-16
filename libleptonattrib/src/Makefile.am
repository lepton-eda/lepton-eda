lib_LTLIBRARIES = libleptonattrib.la

libleptonattrib_la_SOURCES = \
	attrib.c \
	f_export.c \
	globals.c \
	gtkextra-marshal.c \
	gtksheet_2_2.c \
	listsort.c \
	s_attrib.c \
	s_misc.c \
	s_object.c \
	s_sheet_data.c \
	s_string_list.c \
	s_table.c \
	s_toplevel.c \
	s_visibility.c \
	x_dialog.c \
	x_fileselect.c \
	x_gtksheet.c \
	x_window.c

libleptonattrib_la_CPPFLAGS = -I$(top_srcdir)/liblepton/include \
	-I$(srcdir)/../include -I$(top_srcdir) -I$(includedir)
libleptonattrib_la_CFLAGS = $(GCC_CFLAGS) $(GLIB_CFLAGS) \
	$(MINGW_CFLAGS) \
	$(GTK_CFLAGS) $(GTHREAD_CFLAGS) $(GUILE_CFLAGS)
libleptonattrib_la_LDFLAGS = $(GLIB_LIBS) $(GTK_LIBS) $(GTHREAD_LIBS) \
	$(GUILE_LIBS) $(MINGW_GUI_LDFLAGS)
libleptonattrib_la_LIBADD = $(top_builddir)/liblepton/src/liblepton.la


localedir = @datadir@/locale
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@
