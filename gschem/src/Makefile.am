bin_PROGRAMS = gschem

gschem_SOURCES = \
	a_pan.c \
	a_zoom.c \
	g_funcs.c \
	g_hook.c \
	g_keys.c \
	g_rc.c \
	g_register.c \
	globals.c \
	gschem.c \
	gschem_accel_label.c \
	gschem_action.c \
	gschem_cairo.c \
	gschem_dialog.c \
	gschem_pango.c \
	gschem_toplevel.c \
	i_basic.c \
	i_callbacks.c \
	i_vars.c \
	m_basic.c \
	o_arc.c \
	o_attrib.c \
	o_basic.c \
	o_box.c \
	o_buffer.c \
	o_bus.c \
	o_circle.c \
	o_complex.c \
	o_copy.c \
	o_cue.c \
	o_delete.c \
	o_find.c \
	o_grips.c \
	o_line.c \
	o_misc.c \
	o_move.c \
	o_net.c \
	o_path.c \
	o_picture.c \
	o_pin.c \
	o_place.c \
	o_select.c \
	o_slot.c \
	o_text.c \
	o_undo.c \
	parsecmd.c \
	x_attribedit.c \
	x_autonumber.c \
	x_basic.c \
	x_clipboard.c \
	x_color.c \
	x_compselect.c \
	x_dialog.c \
	x_event.c \
	x_fileselect.c \
	x_grid.c \
	x_image.c \
	x_log.c \
	x_menus.c \
	x_multiattrib.c \
	x_pagesel.c \
	x_preview.c \
	x_print.c \
	x_script.c \
	x_stroke.c \
	x_window.c

gschem_CPPFLAGS = -I$(top_srcdir)/libgeda/include  -I$(srcdir)/../include \
	-I$(top_srcdir) -I$(includedir) -I$(top_srcdir)/intl
gschem_CFLAGS = $(GCC_CFLAGS) $(MINGW_CFLAGS) $(LIBSTROKE_CFLAGS) \
	$(GLIB_CFLAGS) $(GTK_CFLAGS) $(GTHREAD_CFLAGS) $(GUILE_CFLAGS)
gschem_LDFLAGS = $(LIBSTROKE_LDFLAGS) $(GLIB_LIBS) $(GTK_LIBS) \
	$(GTHREAD_LIBS) $(GUILE_LIBS)
gschem_LDADD = $(top_builddir)/libgeda/src/libgeda.la @LIBINTL@

localedir = @datadir@/locale
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@

EXTRA_DIST = rcstrings.c

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in
