#ifndef LEPTON_PROTOTYPE_H_
#define LEPTON_PROTOTYPE_H_

G_BEGIN_DECLS

/* a_zoom.c */
void
a_zoom (SchematicWindow *w_current,
        SchematicCanvas *page_view,
        int dir,
        int selected_from);
void
a_zoom_box_draw_rubber (SchematicWindow *w_current,
                        EdaRenderer *renderer);

/* execute_script.c */
char*
schematic_execute_script (SchematicWindow *w_current);

/* g_action.c */
gboolean
g_action_eval_by_name (SchematicWindow *w_current,
                       const gchar *action_name);

/* g_hook.c */
void
g_run_hook_object (SchematicWindow *w_current,
                   const char *name,
                   LeptonObject *obj);
void
g_run_hook_object_list (SchematicWindow *w_current,
                        const char *name,
                        GList *obj_lst);
void
g_run_hook_page (SchematicWindow *w_current,
                 const char *name,
                 LeptonPage *page);
void
g_run_hook_action_mode (SchematicWindow *w_current,
                        const char *name,
                        const gchar *action_mode);

/* page_revert_dialog.c */
gboolean
schematic_page_revert_dialog (SchematicWindow *w_current,
                              const char *filename);

/* slot_edit_dialog.c */
GtkWidget*
slot_edit_dialog (SchematicWindow *w_current,
                  const char *count,
                  const char *string);
gboolean
slot_edit_dialog_response (gint response);

const char*
slot_edit_dialog_get_text (GtkWidget *widget);

void
slot_edit_dialog_quit (SchematicWindow *w_current);

/* keys.c */
guint
schematic_keys_get_event_keyval (GdkEventKey *event);

guint
schematic_keys_get_event_modifiers (GdkEventKey *event);

guint
schematic_keys_verify_keyval (guint keyval);

void
schematic_keys_reset (SchematicWindow *w_current);

/* g_window.c */
void
g_dynwind_window (SchematicWindow *w_current);

void
g_init_window (SCM fluid);

/* lepton-schematic.c */
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
void
i_action_start (SchematicWindow *w_current);

void
i_action_stop (SchematicWindow *w_current);

void
i_action_update_status (SchematicWindow *w_current,
                        gboolean inside_action);

void
i_show_state (SchematicWindow *w_current,
              const char *message);

void
i_set_state (SchematicWindow *w_current,
             SchematicActionMode newstate);
void
i_set_state_msg (SchematicWindow *w_current,
                 SchematicActionMode newstate,
                 const char *message);

void
i_update_menus (SchematicWindow *w_current);

void
i_set_filename (SchematicWindow *w_current,
                const gchar *filename,
                gboolean changed);
void
i_update_grid_info (SchematicWindow *w_current);

void
i_update_grid_info_callback (SchematicCanvas *view,
                             SchematicWindow *w_current);
void
i_update_net_options_status (SchematicWindow* w_current);

/* i_callbacks.c */
void
i_callback_file_save (GtkWidget *widget,
                      gpointer data);
void
i_callback_cancel (GtkWidget *widget,
                   gpointer data);

/* i_vars.c */
void
i_vars_set (SchematicWindow *w_current);

/* m_basic.c */
int
snap_grid (SchematicWindow *w_current,
           int input);
int
clip_nochange (SchematicViewport *geometry,
               int x1,
               int y1,
               int x2,
               int y2);
int
visible (SchematicWindow *w_current,
         int wleft,
         int wtop,
         int wright,
         int wbottom);
double
round_5_2_1 (double unrounded);

/* o_arc.c */
void
o_arc_invalidate_rubber (SchematicWindow *w_current);

void
o_arc_start (SchematicWindow *w_current,
             int x,
             int y);
void
o_arc_end1 (SchematicWindow *w_current,
            int x,
            int y);
void
o_arc_end4 (SchematicWindow *w_current,
            int radius,
            int start_angle,
            int sweep_angle);
void
o_arc_motion (SchematicWindow *w_current,
              int x,
              int y,
              int whichone);
void
o_arc_draw_rubber (SchematicWindow *w_current,
                   EdaRenderer *renderer);
/* o_attrib.c */
void
o_attrib_add_selected (SchematicWindow *w_current,
                       LeptonSelection *selection,
                       LeptonObject *selected);
void
o_attrib_deselect_invisible (SchematicWindow *w_current,
                             LeptonSelection *selection,
                             LeptonObject *selected);
void
o_attrib_select_invisible (SchematicWindow *w_current,
                           LeptonSelection *selection,
                           LeptonObject *selected);
LeptonObject*
o_attrib_add_attrib (SchematicWindow *w_current,
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
o_redraw_rect (SchematicWindow *w_current,
               GtkWidget *widget,
               LeptonPage *page,
               SchematicViewport *geometry,
               cairo_t *cr);
#else
void
o_redraw_rect (SchematicWindow *w_current,
               GdkDrawable *drawable,
               LeptonPage *page,
               SchematicViewport *geometry,
               GdkRectangle *rectangle);
#endif
int
o_invalidate_rubber (SchematicWindow *w_current);

int
o_redraw_cleanstates (SchematicWindow *w_current);

void
o_invalidate_rect (SchematicWindow *w_current,
                   int x1,
                   int y1,
                   int x2,
                   int y2);
void
o_invalidate (SchematicWindow *w_current,
              LeptonObject *object);
void
o_invalidate_glist (SchematicWindow *w_current,
                    GList *list);

/* o_box.c */
void
o_box_draw (SchematicWindow *w_current,
            LeptonObject *o_current);
void
o_box_invalidate_rubber (SchematicWindow *w_current);

void
o_box_start (SchematicWindow *w_current,
             int x,
             int y);
void
o_box_end (SchematicWindow *w_current,
           int x,
           int y);
void
o_box_motion (SchematicWindow *w_current,
              int x,
              int y);
void
o_box_draw_rubber (SchematicWindow *w_current,
                   EdaRenderer *renderer);

/* o_bus.c */
void
o_bus_start (SchematicWindow *w_current,
             int x,
             int y);
void
o_bus_end (SchematicWindow *w_current,
           int x,
           int y);
void
o_bus_reset (SchematicWindow *w_current);

void
o_bus_motion (SchematicWindow *w_current,
              int x,
              int y);
void
o_bus_draw_rubber (SchematicWindow *w_current,
                   EdaRenderer *renderer);
void
o_bus_invalidate_rubber (SchematicWindow *w_current);

/* o_circle.c */
void
o_circle_draw (SchematicWindow *w_current,
               LeptonObject *o_current);
void
o_circle_invalidate_rubber (SchematicWindow *w_current);

void
o_circle_start (SchematicWindow *w_current,
                int x,
                int y);
void
o_circle_end (SchematicWindow *w_current,
              int x,
              int y);
void
o_circle_motion (SchematicWindow *w_current,
                 int x,
                 int y);
void
o_circle_draw_rubber (SchematicWindow *w_current,
                      EdaRenderer *renderer);

/* o_component.c */
void
o_component_prepare_place (SchematicWindow *w_current,
                           const CLibSymbol *sym);
void
o_component_place_changed_run_hook (SchematicWindow *w_current);

void
o_component_translate_all (SchematicWindow *w_current,
                           int offset);

/* delete_dialog.c */
void
schematic_delete (SchematicWindow *w_current,
                  LeptonObject *object);
gint
schematic_delete_dialog ();

/* o_find.c */
gboolean
o_find_object (SchematicWindow *w_current,
               int x,
               int y,
               gboolean deselect_afterwards);
gboolean
o_find_selected_object (SchematicWindow *w_current,
                        int x,
                        int y);
/* o_grips.c */
LeptonObject*
o_grips_search_world (SchematicWindow *w_current,
                      int x,
                      int y,
                      int *whichone);
LeptonObject*
o_grips_search_arc_world (SchematicWindow *w_current,
                          LeptonObject *o_current,
                          int x,
                          int y,
                          int size,
                          int *whichone);
LeptonObject*
o_grips_search_box_world (SchematicWindow *w_current,
                          LeptonObject *o_current,
                          int x,
                          int y,
                          int size,
                          int *whichone);
LeptonObject*
o_grips_search_path_world (SchematicWindow *w_current,
                           LeptonObject *o_current,
                           int x,
                           int y,
                           int size,
                           int *whichone);
LeptonObject*
o_grips_search_picture_world (SchematicWindow *w_current,
                              LeptonObject *o_current,
                              int x,
                              int y,
                              int size,
                              int *whichone);
LeptonObject*
o_grips_search_circle_world (SchematicWindow *w_current,
                             LeptonObject *o_current,
                             int x,
                             int y,
                             int size,
                             int *whichone);
LeptonObject*
o_grips_search_line_world (SchematicWindow *w_current,
                           LeptonObject *o_current,
                           int x,
                           int y,
                           int size,
                           int *whichone);
void
o_grips_start (SchematicWindow *w_current,
               int x,
               int y);
void
o_grips_motion (SchematicWindow *w_current,
                int x,
                int y);
void
o_grips_end (SchematicWindow *w_current);

void
o_grips_cancel (SchematicWindow *w_current);

void
o_grips_draw_rubber (SchematicWindow *w_current,
                     EdaRenderer *renderer);
void
o_line_invalidate_rubber (SchematicWindow *w_current);

void
o_line_start (SchematicWindow *w_current,
              int x,
              int y);
void
o_line_end (SchematicWindow *w_current,
            int x,
            int y);
void
o_line_motion (SchematicWindow *w_current,
               int x,
               int y);
void
o_line_draw_rubber (SchematicWindow *w_current,
                    EdaRenderer *renderer);
int
o_line_visible (SchematicWindow *w_current,
                LeptonLine *line,
                int *x1,
                int *y1,
                int *x2,
                int *y2);

/* o_misc.c */
void
o_rotate_world_update (SchematicWindow *w_current,
                       int centerx,
                       int centery,
                       int angle,
                       GList *list);
void
o_mirror_world_update (SchematicWindow *w_current,
                       int centerx,
                       int centery,
                       GList *list);
void
o_edit_show_hidden_lowlevel (SchematicWindow *w_current,
                             const GList *o_list);
void
o_edit_show_hidden (SchematicWindow *w_current,
                    const GList *o_list);
void
o_edit_hide_specific_text (SchematicWindow *w_current,
                           const GList *o_list,
                           const char *stext);
void
o_edit_show_specific_text (SchematicWindow *w_current,
                           const GList *o_list,
                           const char *stext);

/* o_move.c */
void
o_move_start (SchematicWindow *w_current,
              int x,
              int y);
void
o_move_end_lowlevel (SchematicWindow *w_current,
                     LeptonObject *object,
                     int diff_x,
                     int diff_y);
void
o_move_end (SchematicWindow *w_current);

void
o_move_cancel (SchematicWindow *w_current);

void
o_move_motion (SchematicWindow *w_current,
               int x,
               int y);
void
o_move_invalidate_rubber (SchematicWindow *w_current,
                          int drawing);
void
o_move_draw_rubber (SchematicWindow *w_current,
                    EdaRenderer *renderer);
int
o_move_return_whichone (LeptonObject *object,
                        int x,
                        int y);
void
o_move_check_endpoint (SchematicWindow *w_current,
                       LeptonObject *object);
void
o_move_prep_rubberband (SchematicWindow *w_current);

int
o_move_zero_length (LeptonObject *object);

void
o_move_end_rubberband (SchematicWindow *w_current,
                       int world_diff_x,
                       int world_diff_y,
                       GList **objects);

/* o_net.c */
void
o_net_reset (SchematicWindow *w_current);

void
o_net_guess_direction (SchematicWindow *w_current,
                       int x,
                       int y);
void
o_net_find_magnetic (SchematicWindow *w_current,
                     int event_x,
                     int event_y);
void
o_net_finishmagnetic (SchematicWindow *w_current);

void
o_net_start_magnetic (SchematicWindow *w_current,
                      int x,
                      int y);
void
o_net_start (SchematicWindow *w_current,
             int x,
             int y);
void
o_net_end (SchematicWindow *w_current,
           int x,
           int y);
void
o_net_motion (SchematicWindow *w_current,
              int x,
              int y);
void
o_net_draw_rubber (SchematicWindow *w_current,
                   EdaRenderer *renderer);
void
o_net_invalidate_rubber (SchematicWindow *w_current);

int
o_net_add_busrippers (SchematicWindow *w_current,
                      LeptonObject *net_obj,
                      GList *other_objects);

/* o_picture.c */
void
o_picture_start (SchematicWindow *w_current,
                 int x,
                 int y);
void
o_picture_end (SchematicWindow *w_current,
               int x,
               int y);
void
o_picture_motion (SchematicWindow *w_current,
                  int x,
                  int y);
void
picture_selection_dialog (SchematicWindow *w_current);

void
o_picture_invalidate_rubber(SchematicWindow *w_current);

void
o_picture_draw_rubber (SchematicWindow *w_current,
                       EdaRenderer *renderer);
gboolean
o_picture_exchange (SchematicWindow *w_current,
                    const gchar *filename,
                    GError **error);
void
picture_change_filename_dialog (SchematicWindow *w_current);

void
o_picture_set_pixbuf (SchematicWindow *w_current,
                      GdkPixbuf *pixbuf,
                      char *filename);

/* o_path.c */
void
o_path_start (SchematicWindow *w_current,
              int x,
              int y);
void
o_path_continue (SchematicWindow *w_current,
                 int w_x,
                 int w_y);
void
o_path_motion (SchematicWindow *w_current,
               int w_x,
               int w_y);
void
o_path_end (SchematicWindow *w_current,
            int x,
            int y);
void
o_path_invalidate_rubber (SchematicWindow *w_current);

void
o_path_draw_rubber (SchematicWindow *w_current,
                    EdaRenderer *renderer);
void
o_path_invalidate_rubber_grips (SchematicWindow *w_current);

void
o_path_motion_grips (SchematicWindow *w_current,
                     int x,
                     int y);
void
o_path_draw_rubber_grips (SchematicWindow *w_current,
                          EdaRenderer *renderer);

/* o_pin.c */
void
o_pin_start (SchematicWindow *w_current,
             int x,
             int y);
void
o_pin_end (SchematicWindow *w_current,
           int x,
           int y);
void
o_pin_motion (SchematicWindow *w_current,
              int x,
              int y);
void
o_pin_draw_rubber (SchematicWindow *w_current,
                   EdaRenderer *renderer);
void
o_pin_invalidate_rubber (SchematicWindow *w_current);

/* o_place.c */
void
o_place_start (SchematicWindow *w_current,
               int x,
               int y);
void
o_place_end (SchematicWindow *w_current,
             int x,
             int y,
             int continue_placing,
             const char *hook_name);
void
o_place_motion (SchematicWindow *w_current,
                int x,
                int y);
void
o_place_invalidate_rubber (SchematicWindow *w_current,
                           int drawing);
void
o_place_draw_rubber (SchematicWindow *w_current,
                     EdaRenderer *renderer);
void
o_place_rotate (SchematicWindow *w_current);

void
o_place_mirror (SchematicWindow *w_current);

/* o_select.c */
void
o_select_start (SchematicWindow *w_current,
                int x,
                int y);
void
o_select_end (SchematicWindow *w_current,
              int x,
              int y);
void
o_select_motion (SchematicWindow *w_current,
                 int x,
                 int y);
void
o_select_run_hooks (SchematicWindow *w_current,
                    LeptonObject *o_current,
                    int flag);
void
o_select_object (SchematicWindow *w_current,
                 LeptonObject *o_current,
                 int type,
                 int count);
void
o_select_box_start (SchematicWindow *w_current,
                    int x,
                    int y);
void
o_select_box_end (SchematicWindow *w_current,
                  int x,
                  int y);
void
o_select_box_motion (SchematicWindow *w_current,
                     int x,
                     int y);
void
o_select_box_invalidate_rubber (SchematicWindow *w_current);

void
o_select_box_draw_rubber (SchematicWindow *w_current,
                          EdaRenderer *renderer);
void
o_select_box_search (SchematicWindow *w_current);

void
o_select_connected_nets (SchematicWindow *w_current,
                         LeptonObject* o_current);
LeptonObject*
o_select_return_first_object (SchematicWindow *w_current);

int
o_select_selected (SchematicWindow *w_current);

void
o_select_unselect_all (SchematicWindow *w_current);

void
o_select_move_to_place_list (SchematicWindow *w_current);

/* o_slot.c */
void
o_slot_end (SchematicWindow *w_current,
            LeptonObject *object,
            const char *string);

/* o_text.c */
void
o_text_change (SchematicWindow *w_current,
               LeptonObject *object,
               char *string,
               int visibility,
               int show);

/* o_undo.c */
void
o_undo_savestate (SchematicWindow *w_current,
                  LeptonPage *page,
                  gboolean only_viewport);
void
o_undo_savestate_old (SchematicWindow *w_current);

void
o_undo_savestate_viewport (SchematicWindow *w_current);

int
schematic_undo_get_file_index ();

char*
schematic_undo_get_tmp_path ();

void
schematic_undo_set_tmp_path (char* path);

char*
schematic_undo_index_to_filename (int index);

/* s_stretch.c */
GList *s_stretch_add(GList *list, LeptonObject *object, int whichone);
GList *s_stretch_remove(GList *list, LeptonObject *object);
void s_stretch_destroy_all(GList *list);

/* alignment_combo.c */
GtkWidget*
schematic_alignment_combo_new ();

int
schematic_alignment_combo_get_align (GtkWidget *widget);

void
schematic_alignment_combo_set_align (GtkWidget *widget,
                                     int align);

/* x_attribedit.c */
void
attrib_edit_dialog_ok (GtkWidget *w,
                       SchematicWindow *w_current);
void
attrib_edit_dialog (SchematicWindow *w_current,
                    LeptonObject *attr_obj,
                    int flag);

/* x_basic.c */
void x_basic_warp_cursor(GtkWidget *widget, gint x, gint y);

/* x_clipboard.c */
void
x_clipboard_init (SchematicWindow *w_current);

void
x_clipboard_finish (SchematicWindow *w_current);

void
x_clipboard_query_usable (SchematicWindow *w_current,
                          void (*callback) (int, void *),
                          void *userdata);
gboolean
x_clipboard_set (SchematicWindow *w_current,
                 const GList *object_list);
GList*
x_clipboard_get (SchematicWindow *w_current);

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
void
generic_msg_dialog (const char*);

int
generic_confirm_dialog (const char*);

void
generic_error_dialog (const char *primary_message,
                      const char *secondary_message,
                      const char *title);
char*
generic_filesel_dialog (const char*,
                        const char*,
                        gint);
void
major_changed_dialog (SchematicWindow *w_current);

int
x_dialog_validate_attribute (GtkWindow *parent,
                             char *attribute);
void
text_edit_dialog (SchematicWindow *w_current);

void
arc_angle_dialog (SchematicWindow *w_current,
                  LeptonObject *arc_object);
void
about_dialog (SchematicWindow *w_current);

void
coord_display_update (SchematicWindow *w_current,
                      int x,
                      int y);
void
coord_dialog (SchematicWindow *w_current,
              int x,
              int y);
void
x_dialog_hotkeys (SchematicWindow *w_current);

void
find_text_dialog (SchematicWindow *w_current);

void
hide_text_dialog (SchematicWindow *w_current);

void
show_text_dialog (SchematicWindow *w_current);
gboolean
x_dialog_close_changed_page (SchematicWindow *w_current,
                             LeptonPage *page);
gboolean
x_dialog_close_window (SchematicWindow *w_current);

/* x_event.c */
gboolean
schematic_event_get_doing_stroke ();

void
schematic_event_set_doing_stroke (gboolean val);

gint
schematic_event_shift_mask ();

gint
schematic_event_control_mask ();

gint
schematic_event_alt_mask ();

guint
schematic_event_get_button (GdkEvent *event);

gboolean
schematic_event_is_double_button_press (GdkEvent *event);

gboolean
schematic_event_skip_motion_event (GdkEvent *event);

gboolean
x_event_faked_motion (SchematicCanvas *view,
                      GdkEventKey *event);
gboolean
x_event_configure (SchematicCanvas *page_view,
                   GdkEventConfigure *event,
                   gpointer user_data);
gint
x_event_enter(GtkWidget *widget,
              GdkEventCrossing *event,
              SchematicWindow *w_current);
GdkEventKey*
x_event_key (SchematicCanvas *page_view,
             GdkEventKey *event,
             SchematicWindow *w_current);

GdkScrollDirection
schematic_event_get_scroll_direction (GdkEventScroll *event);

gboolean
x_event_get_pointer_position (SchematicWindow *w_current,
                              gboolean snapped,
                              gint *wx,
                              gint *wy);

/* x_compselect.c */
void
x_compselect_open (GtkWidget *cswindow);

void
x_compselect_deselect (SchematicWindow *w_current);

void
x_compselect_callback_response (GtkDialog *dialog,
                                gint arg1,
                                gpointer user_data);
GtkWidget*
schematic_compselect_get_preview (GtkWidget *cs);

GtkWidget*
schematic_compselect_new (SchematicWindow *w_current);


/* x_fileselect.c */
GtkWidget*
schematic_file_select_dialog_new (SchematicWindow *w_current);

void
x_fileselect_add_preview (GtkWidget *dialog,
                          GtkWidget *preview);
GSList*
x_fileselect_open (SchematicWindow *w_current,
                   GtkWidget *dialog);

gboolean
x_fileselect_save (SchematicWindow *w_current,
                   LeptonPage* page,
                   gboolean* result);
gboolean
schematic_file_open (SchematicWindow *w_current,
                     LeptonPage *page,
                     const gchar *filename,
                     GError **err);
void
x_fileselect_callback_update_preview (GtkFileChooser *chooser,
                                      gpointer user_data);

/* x_fstylecb.c */
GtkWidget* x_fstylecb_new ();
int x_fstylecb_get_index (GtkWidget *widget);
void x_fstylecb_set_index (GtkWidget *widget, int style);

/* x_grid.c */
void
x_grid_draw_region (SchematicWindow *w_current,
                    cairo_t *cr,
                    int x,
                    int y,
                    int width,
                    int height);
int
x_grid_query_drawn_spacing (SchematicWindow *w_current);

/* x_image.c */
void
x_image_lowlevel (SchematicWindow *w_current,
                  const char* filename,
                  int desired_width,
                  int desired_height,
                  const char *filetype,
                  gboolean is_color);
void
x_image_setup (SchematicWindow *w_current);

GdkPixbuf*
x_image_get_pixbuf (SchematicWindow *w_current,
                    int width,
                    int height,
                    gboolean is_color);

/* x_integerls.c */
GtkListStore* x_integerls_new ();
GtkListStore* x_integerls_new_with_values (const char *value[], int count);
void x_integerls_add_value (GtkListStore *store, const char *value);
int x_integerls_get_value_column ();

/* log_widget.c */
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
gboolean
x_show_uri (SchematicWindow *w_current,
            const gchar *buf,
            GError **err);

/* x_menus.c */
GtkWidget*
schematic_window_create_main_popup_menu (SchematicWindow *w_current);

gint
do_popup (SchematicWindow *w_current,
          GdkEvent *event);

void x_menus_sensitivity (GtkWidget* menu, const gchar* action_name, gboolean sensitive);
GtkWidget*
make_separator_menu_item ();
#ifdef ENABLE_GTK3
GSimpleAction*
#else
SchematicAction*
#endif
make_menu_action (const char *action_name,
                  const char *menu_item_name,
                  const char *menu_item_keys,
                  const char *menu_item_stock,
                  SchematicWindow *w_current);
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
char*
schematic_menu_recent_chooser_get_filename (GtkRecentChooser *chooser,
                                            SchematicWindow *w_current);
void
x_menu_attach_recent_files_submenu (SchematicWindow* w_current,
                                    GtkWidget* menuitem,
                                    GCallback callback_item_activated,
                                    gint max_items);

/* x_print.c */
gboolean
x_print_export_pdf_page (SchematicWindow *w_current,
                         const gchar *filename);
gboolean
x_print_export_pdf (SchematicWindow *w_current,
                    const gchar *filename,
                    gboolean is_color);
void
x_print (SchematicWindow *w_current);
/* x_rc.c */
void
x_rc_parse_gschem (LeptonToplevel *toplevel);

/* x_rotatecb.c */
GtkWidget*
schematic_rotation_combo_new ();

int
schematic_rotation_combo_get_angle (GtkWidget *widget);

void
schematic_rotation_combo_set_angle (GtkWidget *widget,
                                    int angle);

/* x_stroke.c */
void
x_stroke_init (void);

void
x_stroke_free (void);

void
x_stroke_record (SchematicWindow *w_current,
                 gint x,
                 gint y);
char*
x_stroke_translate_and_execute (SchematicWindow *w_current);

/* x_window.c */
void
x_window_create_drawing (GtkWidget *drawbox,
                         SchematicWindow *w_current);
void
x_window_setup_draw_events_main_wnd (SchematicWindow* w_current,
                                     GtkWidget*      main_window);
void
x_window_setup_draw_events_drawing_area (SchematicWindow* w_current,
                                         SchematicCanvas* drawing_area);
GtkWidget*
schematic_window_create_app_window (gpointer app);

GtkWidget*
schematic_window_create_main_box (GtkWidget *main_window);

GtkWidget*
schematic_window_create_work_box ();

SchematicCanvas*
schematic_window_create_canvas (SchematicWindow *w_current,
                                GtkWidget *work_box);
void
schematic_window_create_menubar (SchematicWindow *w_current,
                                 GtkWidget *main_box,
                                 GtkWidget *menubar);
void
schematic_window_create_find_text_widget (SchematicWindow *w_current,
                                          GtkWidget *work_box);
void
schematic_window_create_hide_text_widget (SchematicWindow *w_current,
                                          GtkWidget *work_box);
void
schematic_window_create_show_text_widget (SchematicWindow *w_current,
                                          GtkWidget *work_box);
void
schematic_window_create_macro_widget (SchematicWindow *w_current,
                                      GtkWidget *work_box);
void
schematic_window_create_translate_widget (SchematicWindow *w_current,
                                          GtkWidget *work_box);
void
schematic_window_show_translate_widget (SchematicWindow *w_current);

void
schematic_window_create_notebooks (SchematicWindow *w_current,
                                   GtkWidget *main_box,
                                   GtkWidget *work_box);
void
schematic_window_create_statusbar (SchematicWindow *w_current,
                                   GtkWidget *main_box);
void
schematic_window_restore_geometry (SchematicWindow* w_current,
                                   GtkWidget* main_window);
void
schematic_window_save_geometry (SchematicWindow* w_current);

void
schematic_window_show_all (SchematicWindow *w_current,
                           GtkWidget *main_window);
LeptonPage*
x_window_open_page (SchematicWindow *w_current,
                    const gchar *filename);
void
x_window_set_current_page (SchematicWindow *w_current,
                           LeptonPage *page);
gint
x_window_save_page (SchematicWindow *w_current,
                    LeptonPage *page,
                    const gchar *filename);
LeptonPage*
x_window_close_page (SchematicWindow *w_current,
                     LeptonPage *page);
SchematicWindow*
x_window_new (LeptonToplevel *toplevel);

void
x_window_select_object (SchematicFindTextState *state,
                        LeptonObject *object,
                        SchematicWindow *w_current);
void
x_window_setup_scrolling (SchematicWindow *w_current,
                          GtkWidget *scrolled);
gboolean
x_window_untitled_page (LeptonPage* page);

/* x_widgets.c */
gboolean
x_widgets_use_docks();

void
x_widgets_init();

void
x_widgets_show_options (SchematicWindow* w_current);

void
x_widgets_show_text_properties (SchematicWindow* w_current);

void
x_widgets_show_object_properties (SchematicWindow* w_current);

void
x_widgets_show_log (SchematicWindow* w_current);

void
x_widgets_show_find_text_state (SchematicWindow* w_current);

void
x_widgets_show_color_edit (SchematicWindow* w_current);

void
x_widgets_show_font_select (SchematicWindow* w_current);

void
x_widgets_show_page_select (SchematicWindow* w_current);

void
x_widgets_destroy_dialogs (SchematicWindow* w_current);

void
x_widgets_toggle_widget_visibility (GtkWidget *widget);

/* x_tabs.c */
void
x_tabs_cancel_all (SchematicWindow* w_current);

gboolean x_tabs_enabled();

TabInfo*
x_tabs_info_add (SchematicWindow* w_current,
                 gint            ndx,
                 LeptonPage*     page,
                 SchematicCanvas* pview,
                 GtkWidget*      wtab);
TabInfo*
x_tabs_info_cur (SchematicWindow* w_current);

TabInfo*
x_tabs_info_find_by_page (GList* nfos,
                          LeptonPage* page);
void
x_tabs_info_rm (SchematicWindow* w_current,
                TabInfo* nfo);

void x_tabs_init();

GtkWidget*
x_tabs_nbook_create (SchematicWindow* w_current,
                     GtkWidget* work_box);
gint
x_tabs_nbook_page_add (SchematicWindow* w_current,
                       LeptonPage*     page,
                       SchematicCanvas* pview,
                       GtkWidget*      wtab);
void
x_tabs_nbook_page_close (SchematicWindow* w_current,
                         LeptonPage* page);
void
x_tabs_page_on_sel (GtkNotebook* nbook,
                    GtkWidget*   wtab,
                    guint        ndx,
                    gpointer     data);
void
x_tabs_page_on_reordered (GtkNotebook* nbook,
                          GtkWidget*   wtab,
                          guint        newindex,
                          gpointer     data);
void
schematic_tabs_add_canvas (SchematicCanvas *pview,
                           GtkWidget *wtab);
gboolean
x_tabs_tl_page_find (SchematicWindow* w_current,
                     LeptonPage* page);
SchematicCanvas*
x_tabs_tl_pview_cur (SchematicWindow* w_current);

void
x_tabs_tl_pview_cur_set (SchematicWindow* w_current,
                         SchematicCanvas* pview);
void
x_tabs_next (SchematicWindow* w_current);

void
x_tabs_prev (SchematicWindow* w_current);

void
x_tabs_hdr_set (GtkNotebook* nbook,
                TabInfo* nfo);
void
x_tabs_hdr_update (SchematicWindow* w_current,
                   LeptonPage* page);
void
schematic_tabs_set_callback (char *name,
                             GCallback callback);
SchematicWindow*
schematic_tab_info_get_window (TabInfo *tab_info);

LeptonPage*
schematic_tab_info_get_page (TabInfo *tab_info);

void
schematic_tab_info_set_page (TabInfo *tab_info,
                             LeptonPage* page);
SchematicCanvas*
schematic_tab_info_get_canvas (TabInfo *tab_info);

GtkWidget*
schematic_tab_info_get_tab_widget (TabInfo *tab_info);

/* color_edit_widget.c */
void
color_edit_widget_update (SchematicWindow* w_current);

/* schematic_hierarchy.c */
int
schematic_hierarchy_get_page_control_counter ();

void
schematic_hierarchy_increment_page_control_counter ();

LeptonPage*
s_hierarchy_down_schematic_single (SchematicWindow *w_current,
                                   const gchar *filename,
                                   LeptonPage *parent,
                                   int page_control,
                                   GError **err);
LeptonPage*
s_hierarchy_find_up_page (LeptonPage *current_page);

LeptonPage*
s_hierarchy_load_subpage (SchematicWindow *w_current,
                          LeptonPage *page,
                          const char *filename,
                          GError **error);
GList*
s_hierarchy_traversepages (SchematicWindow *w_current,
                           LeptonPage *p_current,
                           gint flags);
gint
s_hierarchy_print_page (LeptonPage *p_current,
                        void * data);
G_END_DECLS

#endif /* LEPTON_PROTOTYPE_H_ */
