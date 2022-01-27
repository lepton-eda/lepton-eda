lib_LTLIBRARIES = libleptongui.la

BUILT_SOURCES = \
	g_attrib.x \
	g_keys.x \
	g_window.x \
	g_select.x \
	scheme_undo.x

libleptongui_la_SOURCES = \
	a_zoom.c \
	g_action.c \
	g_attrib.c \
	g_hook.c \
	g_keys.c \
	g_select.c \
	g_window.c \
	globals.c \
	lepton-schematic.c \
	gschemhotkeystore.c \
	gschem_about_dialog.c \
	gschem_accel_label.c \
	gschem_action.c \
	gschem_alignment_combo.c \
	gschem_arc_dialog.c \
	gschem_bin.c \
	gschem_binding.c \
	gschem_binding_integer.c \
	gschem_bottom_widget.c \
	gschem_close_confirmation_dialog.c \
	gschem_coord_dialog.c \
	gschem_dialog.c \
	gschem_dialog_misc.c \
	gschem_fill_swatch_cell_renderer.c \
	gschem_find_text_state.c \
	gschem_find_text_widget.c \
	gschem_hotkey_dialog.c \
	gschem_integer_combo_box.c \
	gschem_log_widget.c \
	gschem_macro_widget.c \
	gschem_main_window.c \
	gschem_object_properties_widget.c \
	gschem_options.c \
	gschem_options_widget.c \
	gschem_page_geometry.c \
	gschem_page_view.c \
	gschem_pin_type_combo.c \
	gschem_preview.c \
	gschem_rotation_combo.c \
	gschem_selection_adapter.c \
	gschem_show_hide_text_widget.c \
	gschem_slot_edit_dialog.c \
	gschem_swatch_column_renderer.c \
	gschem_text_properties_widget.c \
	gschem_translate_widget.c \
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
	o_component.c \
	o_copy.c \
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
	s_stretch.c \
	schematic_hierarchy.c \
	x_attribedit.c \
	x_autonumber.c \
	x_basic.c \
	x_clipboard.c \
	x_color.c \
	x_colorcb.c \
	x_compselect.c \
	x_dialog.c \
	x_event.c \
	x_fileselect.c \
	x_fstylecb.c \
	x_grid.c \
	x_image.c \
	x_integerls.c \
	x_linecapcb.c \
	x_linetypecb.c \
	x_menus.c \
	x_misc.c \
	x_multiattrib.c \
	x_newtext.c \
	page_select_widget.c \
	x_print.c \
	x_rc.c \
	x_stroke.c \
	x_window.c \
	x_widgets.c \
	scheme_undo.c \
	x_tabs.c \
	color_edit_widget.c \
	font_select_widget.c

libleptongui_la_CPPFLAGS = -I$(top_srcdir)/liblepton/include  -I$(srcdir)/../include \
	-I$(top_srcdir) -I$(includedir)
libleptongui_la_CFLAGS = $(GCC_CFLAGS) $(LIBSTROKE_CFLAGS) \
	$(MINGW_CFLAGS) \
	$(GLIB_CFLAGS) $(GTK_CFLAGS) $(GTHREAD_CFLAGS) $(GUILE_CFLAGS) $(CFLAGS)
libleptongui_la_LDFLAGS = -version-info $(LIBLEPTONGUI_SHLIB_VERSION) \
	$(LIBSTROKE_LDFLAGS) $(GLIB_LIBS) $(GTK_LIBS) \
	$(GTHREAD_LIBS) $(GUILE_LIBS) $(MINGW_GUI_LDFLAGS)

libleptongui_la_LIBADD = \
	$(top_builddir)/liblepton/src/liblepton.la


# This is used to generate boilerplate for defining Scheme functions
# in C.
SUFFIXES = .x
snarf_cpp_opts = $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) \
	$(libleptongui_la_CPPFLAGS) $(AM_CFLAGS) $(libleptongui_la_CFLAGS)
.c.x:
	$(AM_V_GEN) CPP="$(CPP)" $(GUILE_SNARF) -o $@ $< $(snarf_cpp_opts)

localedir = @datadir@/locale
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@

CLEANFILES = $(BUILT_SOURCES)
