#ifndef LEPTON_PROTOTYPE_H_
#define LEPTON_PROTOTYPE_H_

G_BEGIN_DECLS

/* a_zoom.c */
void a_zoom(GschemToplevel *w_current, GschemPageView *page_view, int dir, int selected_from);
void a_zoom_box_start(GschemToplevel *w_current, int x, int y);
void a_zoom_box_end(GschemToplevel *w_current, int x, int y);
void a_zoom_box_motion(GschemToplevel *w_current, int x, int y);
void a_zoom_box_invalidate_rubber(GschemToplevel *w_current);
void a_zoom_box_draw_rubber(GschemToplevel *w_current, EdaRenderer *renderer);
/* g_action.c */
gboolean g_action_eval_by_name (GschemToplevel *w_current, const gchar *action_name);
gboolean
g_action_get_position (GschemToplevel *w_current,
                       gboolean snap,
                       int *x,
                       int *y);

/* g_hook.c */
void g_run_hook_object (GschemToplevel *w_current, const char *name, LeptonObject *obj);
void g_run_hook_object_list (GschemToplevel *w_current, const char *name, GList *obj_lst);
void
g_run_hook_page (GschemToplevel *w_current,
                 const char *name,
                 LeptonPage *page);
void g_run_hook_action_mode (GschemToplevel *w_current, const char *name, const gchar *action_mode);

/* slot_edit_dialog.c */
GtkWidget*
slot_edit_dialog (GschemToplevel *w_current,
                  const char *count,
                  const char *string);
gboolean
slot_edit_dialog_response (gint response);

const char*
slot_edit_dialog_get_text (GtkWidget *widget);

void
slot_edit_dialog_quit (GschemToplevel *w_current);

/* keys.c */
guint
schematic_keys_get_event_keyval (GdkEventKey *event);

guint
schematic_keys_get_event_modifiers (GdkEventKey *event);

guint
schematic_keys_verify_keyval (guint keyval);

void
schematic_keys_reset (GschemToplevel *w_current);

/* g_window.c */
void g_dynwind_window (GschemToplevel *w_current);
void g_init_window (SCM fluid);

/* lepton-schematic.c */
void gschem_quit(void);
void
set_verbose_mode ();
void
set_quiet_mode ();

int
lepton_schematic_run (gpointer activate);

gpointer
lepton_schematic_app ();

/* signals.c */
void
schematic_signal_connect (gpointer instance,
                          const char *detailed_signal,
                          GCallback c_handler,
                          gpointer data);

/* i_basic.c */
void i_action_start(GschemToplevel *w_current);
void i_action_stop(GschemToplevel *w_current);
void i_action_update_status(GschemToplevel *w_current,gboolean inside_action);
void i_show_state(GschemToplevel *w_current, const char *message);

void
i_set_state (GschemToplevel *w_current,
             SchematicActionMode newstate);
void
i_set_state_msg (GschemToplevel *w_current,
                 SchematicActionMode newstate,
                 const char *message);

void i_update_menus(GschemToplevel *w_current);
void i_set_filename(GschemToplevel *w_current, const gchar *filename, gboolean changed);
void i_update_grid_info(GschemToplevel *w_current);
void i_update_grid_info_callback (GschemPageView *view, GschemToplevel *w_current);
void i_update_net_options_status (GschemToplevel* w_current);
/* i_callbacks.c */
void i_callback_file_script (GtkWidget *widget, gpointer data);
void i_callback_file_save (GtkWidget *widget, gpointer data);
void i_callback_edit_copy (GtkWidget *widget, gpointer data);
void i_callback_edit_mcopy (GtkWidget *widget, gpointer data);
void i_callback_edit_move (GtkWidget *widget, gpointer data);
void i_callback_edit_delete (GtkWidget *widget, gpointer data);
void i_callback_edit_edit (GtkWidget *widget, gpointer data);
void i_callback_edit_rotate_90 (GtkWidget *widget, gpointer data);
void i_callback_edit_mirror (GtkWidget *widget, gpointer data);
void i_callback_edit_lock (GtkWidget *widget, gpointer data);
void i_callback_edit_unlock (GtkWidget *widget, gpointer data);
void i_callback_edit_translate (GtkWidget *widget, gpointer data);
void i_callback_edit_invoke_macro (GtkWidget *widget, gpointer data);
void i_callback_edit_embed (GtkWidget *widget, gpointer data);
void i_callback_edit_unembed (GtkWidget *widget, gpointer data);
void i_callback_edit_update (GtkWidget *widget, gpointer data);
void i_callback_edit_show_hidden (GtkWidget *widget, gpointer data);
void i_callback_edit_find (GtkWidget *widget, gpointer data);
void i_callback_edit_hide_text (GtkWidget *widget, gpointer data);
void i_callback_edit_show_text (GtkWidget *widget, gpointer data);
void i_callback_edit_autonumber_text (GtkWidget *widget, gpointer data);
void i_callback_view_sidebar (GtkWidget *widget, gpointer data);
void i_callback_view_status (GtkWidget *widget, gpointer data);
void i_callback_view_zoom_full (GtkWidget *widget, gpointer data);
void i_callback_view_zoom_extents (GtkWidget *widget, gpointer data);
void i_callback_view_zoom_box (GtkWidget *widget, gpointer data);
void i_callback_view_zoom_in (GtkWidget *widget, gpointer data);
void i_callback_view_zoom_out (GtkWidget *widget, gpointer data);
void i_callback_view_pan (GtkWidget *widget, gpointer data);
void i_callback_view_pan_left (GtkWidget *widget, gpointer data);
void i_callback_view_pan_right (GtkWidget *widget, gpointer data);
void i_callback_view_pan_up (GtkWidget *widget, gpointer data);
void i_callback_view_pan_down (GtkWidget *widget, gpointer data);
void i_callback_view_color_edit (GtkWidget *widget, gpointer data);
void i_callback_page_next (GtkWidget *widget, gpointer data);
void i_callback_page_prev (GtkWidget *widget, gpointer data);
void i_callback_page_revert (GtkWidget *widget, gpointer data);
void i_callback_page_print (GtkWidget *widget, gpointer data);
void i_callback_clipboard_copy (GtkWidget *widget, gpointer data);
void i_callback_clipboard_cut (GtkWidget *widget, gpointer data);
void i_callback_clipboard_paste (GtkWidget *widget, gpointer data);
void i_callback_buffer_copy1 (GtkWidget *widget, gpointer data);
void i_callback_buffer_copy2 (GtkWidget *widget, gpointer data);
void i_callback_buffer_copy3 (GtkWidget *widget, gpointer data);
void i_callback_buffer_copy4 (GtkWidget *widget, gpointer data);
void i_callback_buffer_copy5 (GtkWidget *widget, gpointer data);
void i_callback_buffer_cut1 (GtkWidget *widget, gpointer data);
void i_callback_buffer_cut2 (GtkWidget *widget, gpointer data);
void i_callback_buffer_cut3 (GtkWidget *widget, gpointer data);
void i_callback_buffer_cut4 (GtkWidget *widget, gpointer data);
void i_callback_buffer_cut5 (GtkWidget *widget, gpointer data);
void i_callback_buffer_paste1 (GtkWidget *widget, gpointer data);
void i_callback_buffer_paste2 (GtkWidget *widget, gpointer data);
void i_callback_buffer_paste3 (GtkWidget *widget, gpointer data);
void i_callback_buffer_paste4 (GtkWidget *widget, gpointer data);
void i_callback_buffer_paste5 (GtkWidget *widget, gpointer data);
void i_callback_add_component (GtkWidget *widget, gpointer data);
void i_callback_add_attribute (GtkWidget *widget, gpointer data);
void i_callback_add_net (GtkWidget *widget, gpointer data);
void i_callback_toolbar_add_net(GtkWidget *widget, gpointer data);
void i_callback_add_bus (GtkWidget *widget, gpointer data);
void i_callback_toolbar_add_bus(GtkWidget *widget, gpointer data);
void i_callback_add_text (GtkWidget *widget, gpointer data);
void i_callback_add_line (GtkWidget *widget, gpointer data);
void i_callback_add_path  (GtkWidget *widget, gpointer data);
void i_callback_add_box (GtkWidget *widget, gpointer data);
void i_callback_add_picture (GtkWidget *widget, gpointer data);
void i_callback_add_circle (GtkWidget *widget, gpointer data);
void i_callback_add_arc (GtkWidget *widget, gpointer data);
void i_callback_add_pin (GtkWidget *widget, gpointer data);
void i_callback_hierarchy_down_schematic (GtkWidget *widget, gpointer data);
void i_callback_hierarchy_down_symbol (GtkWidget *widget, gpointer data);
void i_callback_hierarchy_up (GtkWidget *widget, gpointer data);
void i_callback_attributes_show_name (GtkWidget *widget, gpointer data);
void i_callback_attributes_show_value (GtkWidget *widget, gpointer data);
void i_callback_attributes_show_both (GtkWidget *widget, gpointer data);
void i_callback_attributes_visibility_toggle (GtkWidget *widget, gpointer data);
void i_callback_cancel (GtkWidget *widget, gpointer data);
gboolean i_callback_close_wm(GtkWidget *widget, GdkEvent *event, gpointer data);
/* i_vars.c */
void i_vars_set(GschemToplevel *w_current);
void i_vars_atexit_save_cache_config (gpointer user_data);
 /* m_basic.c */
int snap_grid(GschemToplevel *w_current, int input);
int clip_nochange(GschemPageGeometry *geometry, int x1, int y1, int x2, int y2);
int visible(GschemToplevel *w_current, int wleft, int wtop, int wright, int wbottom);
double round_5_2_1(double unrounded);
/* o_arc.c */
void o_arc_invalidate_rubber(GschemToplevel *w_current);
void o_arc_start(GschemToplevel *w_current, int x, int y);
void o_arc_end1(GschemToplevel *w_current, int x, int y);
void o_arc_end4(GschemToplevel *w_current, int radius, int start_angle, int sweep_angle);
void o_arc_motion(GschemToplevel *w_current, int x, int y, int whichone);
void o_arc_draw_rubber(GschemToplevel *w_current, EdaRenderer *renderer);
/* o_attrib.c */
void
o_attrib_add_selected (GschemToplevel *w_current,
                       LeptonSelection *selection,
                       LeptonObject *selected);
void
o_attrib_deselect_invisible (GschemToplevel *w_current,
                             LeptonSelection *selection,
                             LeptonObject *selected);
void
o_attrib_select_invisible (GschemToplevel *w_current,
                           LeptonSelection *selection,
                           LeptonObject *selected);
void o_attrib_toggle_visibility(GschemToplevel *w_current, LeptonObject *object);
void o_attrib_toggle_show_name_value(GschemToplevel *w_current, LeptonObject *object, int new_show_name_value);

LeptonObject*
o_attrib_add_attrib (GschemToplevel *w_current,
                     const char *text_string,
                     int visibility,
                     int show_name_value,
                     LeptonObject *object,
                     gboolean proposed_coord,
                     int x,
                     int y);

/* o_basic.c */
#ifdef ENABLE_GTK3
void
o_redraw_rect (GschemToplevel *w_current,
               GtkWidget *widget,
               LeptonPage *page,
               GschemPageGeometry *geometry,
               cairo_t *cr);
#else
void
o_redraw_rect (GschemToplevel *w_current,
               GdkDrawable *drawable,
               LeptonPage *page,
               GschemPageGeometry *geometry,
               GdkRectangle *rectangle);
#endif
int o_invalidate_rubber(GschemToplevel *w_current);
int o_redraw_cleanstates(GschemToplevel *w_current);
void o_invalidate_rect(GschemToplevel *w_current, int x1, int y1, int x2, int y2);
void o_invalidate(GschemToplevel *w_current, LeptonObject *object);
void o_invalidate_glist(GschemToplevel *w_current, GList *list);
/* o_box.c */
void o_box_draw(GschemToplevel *w_current, LeptonObject *o_current);
void o_box_invalidate_rubber(GschemToplevel *w_current);
void o_box_start(GschemToplevel *w_current, int x, int y);
void o_box_end(GschemToplevel *w_current, int x, int y);
void o_box_motion(GschemToplevel *w_current, int x, int y);
void o_box_draw_rubber (GschemToplevel *w_current, EdaRenderer *renderer);
/* o_buffer.c */
void o_buffer_copy(GschemToplevel *w_current, int buf_num);
void o_buffer_cut(GschemToplevel *w_current, int buf_num);
int o_buffer_paste_start(GschemToplevel *w_current, int x, int y, int buf_num);
void o_buffer_init(void);
void o_buffer_free(GschemToplevel *w_current);
/* o_bus.c */
void o_bus_start(GschemToplevel *w_current, int x, int y);
void o_bus_end(GschemToplevel *w_current, int x, int y);
void o_bus_reset(GschemToplevel *w_current);
void o_bus_motion(GschemToplevel *w_current, int x, int y);
void o_bus_draw_rubber (GschemToplevel *w_current, EdaRenderer *renderer);
void o_bus_invalidate_rubber(GschemToplevel *w_current);
/* o_circle.c */
void o_circle_draw(GschemToplevel *w_current, LeptonObject *o_current);
void o_circle_invalidate_rubber(GschemToplevel *w_current);
void o_circle_start(GschemToplevel *w_current, int x, int y);
void o_circle_end(GschemToplevel *w_current, int x, int y);
void o_circle_motion(GschemToplevel *w_current, int x, int y);
void o_circle_draw_rubber (GschemToplevel *w_current, EdaRenderer *renderer);
/* o_component.c */
void o_component_prepare_place (GschemToplevel *w_current, const CLibSymbol *sym);
void o_component_place_changed_run_hook (GschemToplevel *w_current);
void o_component_translate_all (GschemToplevel *w_current, int offset);
/* o_copy.c */
void o_copy_start(GschemToplevel *w_current, int x, int y);
void o_copy_end(GschemToplevel *w_current);
/* o_delete.c */
void o_delete(GschemToplevel *w_current, LeptonObject *object);
void o_delete_selected(GschemToplevel *w_current);
/* o_find.c */
gboolean o_find_object(GschemToplevel *w_current, int x, int y,
                       gboolean deselect_afterwards);
gboolean o_find_selected_object(GschemToplevel *w_current, int x, int y);
/* o_grips.c */
LeptonObject *o_grips_search_world(GschemToplevel *w_current, int x, int y, int *whichone);
LeptonObject *o_grips_search_arc_world(GschemToplevel *w_current, LeptonObject *o_current, int x, int y, int size, int *whichone);
LeptonObject *o_grips_search_box_world(GschemToplevel *w_current, LeptonObject *o_current, int x, int y, int size, int *whichone);
LeptonObject *o_grips_search_path_world(GschemToplevel *w_current, LeptonObject *o_current, int x, int y, int size, int *whichone);
LeptonObject *o_grips_search_picture_world(GschemToplevel *w_current, LeptonObject *o_current, int x, int y, int size, int *whichone);
LeptonObject *o_grips_search_circle_world(GschemToplevel *w_current, LeptonObject *o_current, int x, int y, int size, int *whichone);
LeptonObject *o_grips_search_line_world(GschemToplevel *w_current, LeptonObject *o_current, int x, int y, int size, int *whichone);
void o_grips_start(GschemToplevel *w_current, int x, int y);
void o_grips_motion(GschemToplevel *w_current, int x, int y);
void o_grips_end(GschemToplevel *w_current);
void o_grips_cancel(GschemToplevel *w_current);
void o_grips_draw_rubber(GschemToplevel *w_current, EdaRenderer *renderer);
/* o_line.c */
void o_line_invalidate_rubber(GschemToplevel *w_current);
void o_line_start(GschemToplevel *w_current, int x, int y);
void o_line_end(GschemToplevel *w_current, int x, int y);
void o_line_motion(GschemToplevel *w_current, int x, int y);
void o_line_draw_rubber(GschemToplevel *w_current, EdaRenderer *renderer);
int
o_line_visible (GschemToplevel *w_current,
                LeptonLine *line,
                int *x1,
                int *y1,
                int *x2,
                int *y2);
/* o_misc.c */
void o_edit(GschemToplevel *w_current, GList *list);
void o_lock(GschemToplevel *w_current);
void o_unlock(GschemToplevel *w_current);
void o_rotate_world_update(GschemToplevel *w_current, int centerx, int centery, int angle, GList *list);
void o_mirror_world_update(GschemToplevel *w_current, int centerx, int centery, GList *list);
void o_edit_show_hidden_lowlevel(GschemToplevel *w_current, const GList *o_list);
void o_edit_show_hidden(GschemToplevel *w_current, const GList *o_list);
void o_edit_hide_specific_text(GschemToplevel *w_current, const GList *o_list, const char *stext);
void o_edit_show_specific_text(GschemToplevel *w_current, const GList *o_list, const char *stext);
LeptonObject *o_update_component(GschemToplevel *w_current, LeptonObject *o_current);
void o_autosave_backups(GschemToplevel *w_current);
/* o_move.c */
void o_move_start(GschemToplevel *w_current, int x, int y);
void o_move_end_lowlevel(GschemToplevel *w_current, LeptonObject *object, int diff_x, int diff_y);
void o_move_end(GschemToplevel *w_current);
void o_move_cancel(GschemToplevel *w_current);
void o_move_motion(GschemToplevel *w_current, int x, int y);
void o_move_invalidate_rubber(GschemToplevel *w_current, int drawing);
void o_move_draw_rubber(GschemToplevel *w_current, EdaRenderer *renderer);
int o_move_return_whichone(LeptonObject *object, int x, int y);
void o_move_check_endpoint(GschemToplevel *w_current, LeptonObject *object);
void o_move_prep_rubberband(GschemToplevel *w_current);
int o_move_zero_length(LeptonObject *object);
void o_move_end_rubberband(GschemToplevel *w_current, int world_diff_x, int world_diff_y, GList **objects);
/* o_net.c */
void o_net_reset(GschemToplevel *w_current);
void o_net_guess_direction(GschemToplevel *w_current, int x, int y);
void o_net_find_magnetic(GschemToplevel *w_current, int event_x, int event_y);
void o_net_finishmagnetic(GschemToplevel *w_current);
void o_net_start_magnetic(GschemToplevel *w_current, int x, int y);
void o_net_start(GschemToplevel *w_current, int x, int y);
void o_net_end(GschemToplevel *w_current, int x, int y);
void o_net_motion(GschemToplevel *w_current, int x, int y);
void o_net_draw_rubber(GschemToplevel *w_current, EdaRenderer *renderer);
void o_net_invalidate_rubber(GschemToplevel *w_current);
int o_net_add_busrippers(GschemToplevel *w_current, LeptonObject *net_obj, GList *other_objects);
/* o_picture.c */
void o_picture_start(GschemToplevel *w_current, int x, int y);
void o_picture_end(GschemToplevel *w_current, int x, int y);
void o_picture_motion(GschemToplevel *w_current, int x, int y);
void picture_selection_dialog (GschemToplevel *w_current);
void o_picture_invalidate_rubber(GschemToplevel *w_current);
void o_picture_draw_rubber(GschemToplevel *w_current, EdaRenderer *renderer);
gboolean o_picture_exchange(GschemToplevel *w_current, const gchar *filename, GError **error);
void picture_change_filename_dialog (GschemToplevel *w_current);
void o_picture_set_pixbuf(GschemToplevel *w_current, GdkPixbuf *pixbuf, char *filename);

/* o_path.c */
void o_path_start(GschemToplevel *w_current, int x, int y);
void o_path_continue (GschemToplevel *w_current, int w_x, int w_y);
void o_path_motion (GschemToplevel *w_current, int w_x, int w_y);
void o_path_end(GschemToplevel *w_current, int x, int y);
void o_path_invalidate_rubber (GschemToplevel *w_current);
void o_path_draw_rubber (GschemToplevel *w_current, EdaRenderer *renderer);
void o_path_invalidate_rubber_grips (GschemToplevel *w_current);
void o_path_motion_grips (GschemToplevel *w_current, int x, int y);
void o_path_draw_rubber_grips (GschemToplevel *w_current, EdaRenderer *renderer);

/* o_pin.c */
void o_pin_start(GschemToplevel *w_current, int x, int y);
void o_pin_end(GschemToplevel *w_current, int x, int y);
void o_pin_motion(GschemToplevel *w_current, int x, int y);
void o_pin_draw_rubber(GschemToplevel *w_current, EdaRenderer *renderer);
void o_pin_invalidate_rubber(GschemToplevel *w_current);
/* o_place.c */
void o_place_start(GschemToplevel *w_current, int x, int y);
void o_place_end(GschemToplevel *w_current, int x, int y, int continue_placing, const char *hook_name);
void o_place_motion(GschemToplevel *w_current, int x, int y);
void o_place_invalidate_rubber(GschemToplevel *w_current, int drawing);
void o_place_draw_rubber(GschemToplevel *w_current, EdaRenderer *renderer);
void o_place_rotate(GschemToplevel *w_current);
void o_place_mirror(GschemToplevel *w_current);
/* o_select.c */
void o_select_start(GschemToplevel *w_current, int x, int y);
void o_select_end(GschemToplevel *w_current, int x, int y);
void o_select_motion(GschemToplevel *w_current, int x, int y);
void o_select_run_hooks(GschemToplevel *w_current, LeptonObject *o_current, int flag);
void o_select_object(GschemToplevel *w_current, LeptonObject *o_current, int type, int count);
void o_select_box_start(GschemToplevel *w_current, int x, int y);
void o_select_box_end(GschemToplevel *w_current, int x, int y);
void o_select_box_motion(GschemToplevel *w_current, int x, int y);
void o_select_box_invalidate_rubber(GschemToplevel *w_current);
void o_select_box_draw_rubber(GschemToplevel *w_current, EdaRenderer *renderer);
void o_select_box_search(GschemToplevel *w_current);
void o_select_connected_nets(GschemToplevel *w_current, LeptonObject* o_current);
LeptonObject *o_select_return_first_object(GschemToplevel *w_current);
int o_select_selected(GschemToplevel *w_current);
void o_select_unselect_all(GschemToplevel *w_current);
void o_select_visible_unlocked(GschemToplevel *w_current);
void o_select_move_to_place_list(GschemToplevel *w_current);
/* o_slot.c */
void o_slot_end(GschemToplevel *w_current, LeptonObject *object, const char *string);
/* o_text.c */
void o_text_prepare_place(GschemToplevel *w_current, char *text, int color, int align, int rotate, int size);
void o_text_change(GschemToplevel *w_current, LeptonObject *object, char *string, int visibility, int show);
/* o_undo.c */
void o_undo_init(void);
void
o_undo_savestate (GschemToplevel *w_current,
                  LeptonPage *page,
                  int flag);
void o_undo_savestate_old(GschemToplevel *w_current, int flag);

char*
o_undo_find_prev_filename (LeptonUndo *start);

GList*
o_undo_find_prev_object_head (LeptonUndo *start);

void
o_undo_callback (GschemToplevel *w_current,
                 LeptonPage *page,
                 gboolean redo);
void o_undo_cleanup(void);
/* s_stretch.c */
GList *s_stretch_add(GList *list, LeptonObject *object, int whichone);
GList *s_stretch_remove(GList *list, LeptonObject *object);
void s_stretch_destroy_all(GList *list);
/* gschem_alignment_combo.c */
GtkWidget* gschem_alignment_combo_new ();
int gschem_alignment_combo_get_align (GtkWidget *widget);
void gschem_alignment_combo_set_align (GtkWidget *widget, int align);
/* x_attribedit.c */
void attrib_edit_dialog_ok(GtkWidget *w, GschemToplevel *w_current);
void attrib_edit_dialog(GschemToplevel *w_current, LeptonObject *attr_obj, int flag);
/* x_autonumber.c */
void autonumber_text_dialog(GschemToplevel *w_current);
/* x_basic.c */
void x_basic_warp_cursor(GtkWidget *widget, gint x, gint y);
/* x_clipboard.c */
void x_clipboard_init (GschemToplevel *w_current);
void x_clipboard_finish (GschemToplevel *w_current);
void x_clipboard_query_usable (GschemToplevel *w_current, void (*callback) (int, void *), void *userdata);
gboolean x_clipboard_set (GschemToplevel *w_current, const GList *object_list);
GList *x_clipboard_get (GschemToplevel *w_current);
/* x_color.c */
void x_color_init();
#ifdef ENABLE_GTK3
GdkRGBA *x_color_lookup_gdk_rgba (size_t color_id);
#else
GdkColor *x_color_lookup_gdk(size_t color_id);
#endif
LeptonColor *x_color_lookup(size_t color_id);
gboolean x_color_display_enabled (size_t color_id);
#ifdef ENABLE_GTK3
void x_color_set_display_color (size_t color_id,
                                GdkRGBA* color);
void x_color_set_outline_color (size_t color_id,
                                GdkRGBA* color);
#else
void x_color_set_display (size_t color_id, GdkColor* color, guint16 alpha);
void x_color_set_outline (size_t color_id, GdkColor* color, guint16 alpha);
#endif
GString* x_color_map2str_display();
GString* x_color_map2str_outline();

/* x_colorcb.c */
GtkWidget* x_colorcb_new ();
int x_colorcb_get_index (GtkWidget *widget);
void x_colorcb_set_index (GtkWidget *widget, int color_index);
void x_colorcb_update_colors();
#ifdef ENABLE_GTK3
void x_colorcb_set_rgba_color (GtkTreeIter* iter, GdkRGBA* color);
#else
void x_colorcb_set_color (GtkTreeIter* iter, GdkColor* color);
#endif

/* x_dialog.c */
int text_view_calculate_real_tab_width(GtkTextView *textview, int tab_size);
void select_all_text_in_textview(GtkTextView *textview);
void text_input_dialog(GschemToplevel *w_current);
void text_edit_dialog(GschemToplevel *w_current);
void arc_angle_dialog(GschemToplevel *w_current, LeptonObject *arc_object);
void about_dialog(GschemToplevel *w_current);
void coord_display_update(GschemToplevel *w_current, int x, int y);
void coord_dialog(GschemToplevel *w_current, int x, int y);
char *index2functionstring(int index);
void x_dialog_hotkeys(GschemToplevel *w_current);

void generic_msg_dialog(const char *);
int generic_confirm_dialog(const char *);
char * generic_filesel_dialog(const char *, const char *, gint);

void find_text_dialog(GschemToplevel *w_current);
void hide_text_dialog(GschemToplevel *w_current);
void show_text_dialog(GschemToplevel *w_current);
void major_changed_dialog(GschemToplevel* w_current);
gboolean
x_dialog_close_changed_page (GschemToplevel *w_current,
                             LeptonPage *page);
gboolean x_dialog_close_window (GschemToplevel *w_current);
int x_dialog_validate_attribute(GtkWindow* parent, char *attribute);
/* x_event.c */
#ifdef ENABLE_GTK3
gint
x_event_draw (GschemPageView *widget,
                cairo_t *cr,
                GschemToplevel *w_current);
#else
gint x_event_expose(GschemPageView *widget, GdkEventExpose *event, GschemToplevel *w_current);
#endif
gint x_event_button_pressed(GschemPageView *page_view, GdkEventButton *event, GschemToplevel *w_current);
gint x_event_button_released(GschemPageView *page_view, GdkEventButton *event, GschemToplevel *w_current);
gint x_event_motion(GschemPageView *page_view, GdkEventMotion *event, GschemToplevel *w_current);
gboolean x_event_faked_motion (GschemPageView *view, GdkEventKey *event);
gboolean x_event_configure (GschemPageView *page_view, GdkEventConfigure *event, gpointer user_data);
gint x_event_enter(GtkWidget *widget, GdkEventCrossing *event, GschemToplevel *w_current);

GdkEventKey*
x_event_key (GschemPageView *page_view,
             GdkEventKey *event,
             GschemToplevel *w_current);

gint x_event_scroll(GtkWidget *widget, GdkEventScroll *event, GschemToplevel *w_current);
gboolean x_event_get_pointer_position (GschemToplevel *w_current, gboolean snapped, gint *wx, gint *wy);
/* x_compselect.c */
void x_compselect_open (GschemToplevel *w_current);
void x_compselect_deselect (GschemToplevel *w_current);
/* x_fileselect.c */
void x_fileselect_open(GschemToplevel *w_current);
gboolean
x_fileselect_save (GschemToplevel *w_current,
                   LeptonPage* page,
                   gboolean* result);
gboolean
schematic_file_open (GschemToplevel *w_current,
                     LeptonPage *page,
                     const gchar *filename,
                     GError **err);

/* x_fstylecb.c */
GtkWidget* x_fstylecb_new ();
int x_fstylecb_get_index (GtkWidget *widget);
void x_fstylecb_set_index (GtkWidget *widget, int style);
/* x_grid.c */
void x_grid_draw_region(GschemToplevel *w_current, cairo_t *cr, int x, int y, int width, int height);
int x_grid_query_drawn_spacing(GschemToplevel *w_current);
/* x_image.c */
void x_image_lowlevel(GschemToplevel *w_current, const char* filename,
                      int desired_width, int desired_height, const char *filetype, gboolean is_color);
void x_image_setup(GschemToplevel *w_current);
GdkPixbuf *x_image_get_pixbuf (GschemToplevel *w_current, int width, int height, gboolean is_color);
/* x_integerls.c */
GtkListStore* x_integerls_new ();
GtkListStore* x_integerls_new_with_values (const char *value[], int count);
void x_integerls_add_value (GtkListStore *store, const char *value);
int x_integerls_get_value_column ();
/* gschem_log_widget.c */
void x_log_message (const gchar *log_domain,
                    GLogLevelFlags log_level,
                    const gchar *message);
/* x_linecapcb.c */
GtkWidget* x_linecapcb_new ();
int x_linecapcb_get_index (GtkWidget *widget);
void x_linecapcb_set_index (GtkWidget *widget, int index);
/* x_linetypecb.c */
GtkWidget* x_linetypecb_new ();
int x_linetypecb_get_index (GtkWidget *widget);
void x_linetypecb_set_index (GtkWidget *widget, int index);
/* x_misc.c */
gboolean x_show_uri (GschemToplevel *w_current, const gchar *buf, GError **err);

/* x_menus.c */
GtkWidget*
schematic_window_create_main_popup_menu (GschemToplevel *w_current);

gint do_popup(GschemToplevel *w_current, GdkEventButton *event);
void x_menus_sensitivity (GtkWidget* menu, const gchar* action_name, gboolean sensitive);
GtkWidget*
make_separator_menu_item ();
#ifdef ENABLE_GTK3
GSimpleAction*
#else
GschemAction*
#endif
make_menu_action (const char *action_name,
                  const char *menu_item_name,
                  const char *menu_item_keys,
                  const char *menu_item_stock,
                  GschemToplevel *w_current);
#ifdef ENABLE_GTK3
GtkWidget*
lepton_action_create_menu_item (GSimpleAction* action,
                                const gchar *label,
                                const gchar *shortcut);
#else
GtkWidget*
lepton_action_create_menu_item (GtkAction *action,
                                gpointer data1,
                                gpointer data2);
#endif
void
lepton_menu_set_action_data (GtkWidget *menu,
                             const char *action_name,
                             GtkWidget *menu_item,
                             gpointer action);
void
x_menu_attach_recent_files_submenu (GschemToplevel* w_current,
                                    GtkWidget*      menuitem);
/* x_multiattrib.c */
void x_multiattrib_open (GschemToplevel *w_current);
void x_multiattrib_close (GschemToplevel *w_current);
void x_multiattrib_update (GschemToplevel *w_current);
/* x_print.c */
gboolean x_print_export_pdf_page (GschemToplevel *w_current, const gchar *filename);
gboolean x_print_export_pdf (GschemToplevel *w_current, const gchar *filename, gboolean is_color);
void x_print (GschemToplevel *w_current);
/* x_rc.c */
void
x_rc_parse_gschem (LeptonToplevel *toplevel);

/* x_rotatecb.c */
GtkWidget* gschem_rotation_combo_new ();
int gschem_rotation_combo_get_angle (GtkWidget *widget);
void gschem_rotation_combo_set_angle (GtkWidget *widget, int angle);
/* x_stroke.c */
void x_stroke_init (void);
void x_stroke_free (void);
void x_stroke_record (GschemToplevel *w_current, gint x, gint y);
gint x_stroke_translate_and_execute (GschemToplevel *w_current);

/* x_window.c */
GschemToplevel* x_window_setup (GschemToplevel *w_current);
void x_window_create_drawing(GtkWidget *drawbox, GschemToplevel *w_current);
void x_window_setup_draw_events_main_wnd (GschemToplevel* w_current,
                                          GtkWidget*      main_window);
void x_window_setup_draw_events_drawing_area (GschemToplevel* w_current,
                                              GschemPageView* drawing_area);
GtkWidget*
schematic_window_create_app_window (gpointer app);

GtkWidget*
schematic_window_create_main_box (GtkWidget *main_window);

GtkWidget*
schematic_window_create_work_box ();

GschemPageView*
schematic_window_create_page_view (GschemToplevel *w_current,
                                   GtkWidget *work_box);
void
schematic_window_create_menubar (GschemToplevel *w_current,
                                 GtkWidget *main_box,
                                 GtkWidget *menubar);
void
schematic_window_set_key_event_callback (gpointer key_event_callback);

void
schematic_window_create_find_text_widget (GschemToplevel *w_current,
                                          GtkWidget *work_box);
void
schematic_window_create_hide_text_widget (GschemToplevel *w_current,
                                          GtkWidget *work_box);
void
schematic_window_create_show_text_widget (GschemToplevel *w_current,
                                          GtkWidget *work_box);
void
schematic_window_create_macro_widget (GschemToplevel *w_current,
                                      GtkWidget *work_box);
void
schematic_window_create_translate_widget (GschemToplevel *w_current,
                                          GtkWidget *work_box);
void
schematic_window_create_notebooks (GschemToplevel *w_current,
                                   GtkWidget *main_box,
                                   GtkWidget *work_box);
void
schematic_window_create_statusbar (GschemToplevel *w_current,
                                   GtkWidget *main_box);
void
schematic_window_restore_geometry (GschemToplevel* w_current,
                                   GtkWidget* main_window);
void
schematic_window_show_all (GschemToplevel *w_current,
                           GtkWidget *main_window);
GschemToplevel*
schematic_window_set_main_window (GschemToplevel *w_current,
                                  GtkWidget *main_window);

void x_window_close(GschemToplevel *w_current);
void x_window_close_all(GschemToplevel *w_current);
LeptonPage*
x_window_open_page (GschemToplevel *w_current,
                    const gchar *filename);
LeptonPage*
x_window_open_page_impl (GschemToplevel *w_current,
                         const gchar *filename);
void
x_window_set_current_page (GschemToplevel *w_current,
                           LeptonPage *page);
void
x_window_set_current_page_impl (GschemToplevel *w_current,
                                LeptonPage *page);
gint
x_window_save_page (GschemToplevel *w_current,
                    LeptonPage *page,
                    const gchar *filename);
void
x_window_close_page (GschemToplevel *w_current,
                     LeptonPage *page);
LeptonPage*
x_window_close_page_impl (GschemToplevel *w_current,
                          LeptonPage *page);
GschemToplevel*
x_window_new (LeptonToplevel *toplevel);

void x_window_select_object (GschemFindTextState *state, LeptonObject *object, GschemToplevel *w_current);
void x_window_setup_scrolling (GschemToplevel *w_current, GtkWidget *scrolled);
gboolean
x_window_untitled_page (LeptonPage* page);

/* x_widgets.c */
gboolean x_widgets_use_docks();
void x_widgets_init();
void x_widgets_create (GschemToplevel* w_current);
void x_widgets_show_options (GschemToplevel* w_current);
void x_widgets_show_text_properties (GschemToplevel* w_current);
void x_widgets_show_object_properties (GschemToplevel* w_current);
void x_widgets_show_log (GschemToplevel* w_current);
void x_widgets_show_find_text_state (GschemToplevel* w_current);
void x_widgets_show_color_edit (GschemToplevel* w_current);
void x_widgets_show_font_select (GschemToplevel* w_current);
void x_widgets_show_page_select (GschemToplevel* w_current);
void x_widgets_destroy_dialogs (GschemToplevel* w_current);

/* x_tabs.c */
gboolean x_tabs_enabled();
void x_tabs_init();
void x_tabs_create (GschemToplevel* w_current, GtkWidget* work_box);
LeptonPage*
x_tabs_page_open (GschemToplevel* w_current,
                  const gchar* filename);
void
x_tabs_page_set_cur (GschemToplevel* w_current,
                     LeptonPage* page);
void
x_tabs_page_close (GschemToplevel* w_current,
                   LeptonPage* page);
void x_tabs_next (GschemToplevel* w_current);
void x_tabs_prev (GschemToplevel* w_current);
void
x_tabs_hdr_update (GschemToplevel* w_current,
                   LeptonPage* page);
#ifdef DEBUG
void x_tabs_dbg_infos_dump (GschemToplevel* w_current);
void x_tabs_dbg_pages_dump (GschemToplevel* w_current);
#endif

/* color_edit_widget.c */
void color_edit_widget_update (GschemToplevel* w_current);

/* schematic_hierarchy.c */
LeptonPage*
s_hierarchy_down_schematic_single (GschemToplevel *w_current,
                                   const gchar *filename,
                                   LeptonPage *parent,
                                   int page_control,
                                   int flag,
                                   GError **err);
void
s_hierarchy_down_symbol (GschemToplevel *w_current,
                         const CLibSymbol *symbol,
                         LeptonPage *parent);
LeptonPage*
s_hierarchy_find_up_page (LeptonPage *current_page);
LeptonPage*
s_hierarchy_load_subpage (GschemToplevel *w_current,
                          LeptonPage *page,
                          const char *filename,
                          GError **error);
GList*
s_hierarchy_traversepages (GschemToplevel *w_current,
                           LeptonPage *p_current,
                           gint flags);
gint
s_hierarchy_print_page (LeptonPage *p_current,
                        void * data);
LeptonPage*
s_hierarchy_find_prev_page (LeptonPageList *page_list,
                            LeptonPage *current_page);
LeptonPage*
s_hierarchy_find_next_page (LeptonPageList *page_list,
                            LeptonPage *current_page);

G_END_DECLS

#endif /* LEPTON_PROTOTYPE_H_ */
