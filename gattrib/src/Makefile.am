## Process this file with automake to produce Makefile.in
## This Makefile.am created for gattrib by SDB 12.13.2003

bin_PROGRAMS = gattrib

## don't forget all *.h files */
gattrib_SOURCES = \
		gtksheet_2_2.c \
		gtkextra-marshal.c \
		globals.c \
		listsort.c \
		parsecmd.c \
		f_export.c \
		g_register.c \
		g_rc.c \
		i_vars.c \
		i_basic.c \
		x_window.c \
		x_gtksheet.c \
		x_dialog.c \
		x_fileselect.c \
		s_rename.c \
		s_misc.c \
		s_string_list.c \
		s_sheet_data.c \
		s_table.c \
		s_object.c \
		s_attrib.c \
		s_toplevel.c \
		s_visibility.c \
		gattrib.c 


gattrib_CPPFLAGS = -I$(top_srcdir)/libgeda/include \
	-I$(srcdir)/../include -I$(top_srcdir) -I$(includedir)
gattrib_CFLAGS = $(GCC_CFLAGS) $(MINGW_CFLAGS) $(GLIB_CFLAGS) \
	$(GTK_CFLAGS) $(GTHREAD_CFLAGS) $(GUILE_CFLAGS)
gattrib_LDFLAGS = $(GLIB_LIBS) $(GTK_LIBS) $(GTHREAD_LIBS) \
	$(GUILE_LIBS)
gattrib_LDADD = $(top_builddir)/libgeda/src/libgeda.la

MOSTLYCLEANFILES = *.log *.ps core FILE *~ #*# 
CLEANFILES = *.log *.ps core FILE *~ #*#
DISTCLEANFILES = *.log core FILE *~ #*#
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ Makefile.in #*#
