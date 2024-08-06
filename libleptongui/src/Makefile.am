lib_LTLIBRARIES = libleptongui.la

libleptongui_la_SOURCES = \
	a_zoom.c \
	action.c \
	action_mode.c \
	canvas.c \
	execute_script.c \
	g_action.c \
	g_hook.c \
	g_window.c \
	globals.c \
	grid_mode.c \
	gtk_helper.c \
	lepton-schematic.c \
	hotkey_store.c \
	about_dialog.c \
	accel_label.c \
	alignment_combo.c \
	arc_dialog.c \
	bin.c \
	binding.c \
	binding_integer.c \
	bottom_widget.c \
	close_confirmation_dialog.c \
	coord_dialog.c \
	dialog.c \
	dialog_misc.c \
	fill_swatch_cell_renderer.c \
	find_text_state.c \
	gschem_find_text_widget.c \
	hotkey_dialog.c \
	gschem_integer_combo_box.c \
	gschem_log_widget.c \
	gschem_macro_widget.c \
	gschem_main_window.c \
	gschem_object_properties_widget.c \
	options.c \
	options_widget.c \
	viewport.c \
	gschem_pin_type_combo.c \
	gschem_rotation_combo.c \
	gschem_selection_adapter.c \
	gschem_show_hide_text_widget.c \
	gschem_swatch_column_renderer.c \
	gschem_text_properties_widget.c \
	gschem_translate_widget.c \
	i_basic.c \
	i_callbacks.c \
	i_vars.c \
	keys.c \
	m_basic.c \
	o_arc.c \
	o_attrib.c \
	o_basic.c \
	o_box.c \
	o_bus.c \
	o_circle.c \
	o_component.c \
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
	page_revert_dialog.c \
	preview_widget.c \
	signals.c \
	s_stretch.c \
	schematic_hierarchy.c \
	slot_edit_dialog.c \
	snap_mode.c \
	window.c \
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
	toolbar.c \
	x_print.c \
	x_rc.c \
	x_stroke.c \
	x_window.c \
	x_widgets.c \
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

localedir = @datadir@/locale
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@
