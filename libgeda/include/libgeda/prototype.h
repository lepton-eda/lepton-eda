G_BEGIN_DECLS

/* a_basic.c */
const gchar *o_file_format_header();
gchar *o_save_buffer (TOPLEVEL *toplevel, const GList *object_list);
int o_save (TOPLEVEL *toplevel, const GList *object_list, const char *filename, GError **err);
GList *o_read_buffer(TOPLEVEL *toplevel, GList *object_list, char *buffer, const int size, const char *name, GError **err);
GList *o_read(TOPLEVEL *toplevel, GList *object_list, char *filename, GError **err);
void o_scale(TOPLEVEL *toplevel, GList *list, int x_scale, int y_scale);

/* f_basic.c */
gchar *f_get_autosave_filename (const gchar *filename);
gboolean f_has_active_autosave (const gchar *filename, GError **err);
int f_open(TOPLEVEL *toplevel, PAGE *page, const gchar *filename, GError **err);
int f_open_flags(TOPLEVEL *toplevel, PAGE *page, const gchar *filename,
                 const gint flags, GError **err);
void f_close(TOPLEVEL *toplevel);
int f_save(TOPLEVEL *toplevel, PAGE *page, const char *filename, GError **error);
gchar *f_normalize_filename (const gchar *filename, GError **error);
char *follow_symlinks (const gchar *filename, GError **error);

/* f_print.c */
int f_print_file (TOPLEVEL *toplevel, PAGE *page, const char *filename);
int f_print_command (TOPLEVEL *toplevel, PAGE *page, const char *command);
int f_print_stream(TOPLEVEL *toplevel, PAGE *page, FILE *fp);
void f_print_set_type(TOPLEVEL *toplevel, int type);

/* g_basic.c */
SCM g_scm_eval_protected (SCM exp, SCM module_or_state);
SCM g_scm_eval_string_protected (SCM str);
SCM g_scm_c_eval_string_protected (const gchar *str);
gboolean g_read_file(TOPLEVEL *toplevel, const gchar *filename, GError **err);

/* g_rc.c */
SCM g_rc_mode_general(SCM scmmode, const char *rc_name, int *mode_var, 
                      const vstbl_entry *table, int table_size);
gboolean g_rc_parse_system (TOPLEVEL *toplevel, const gchar *rcname, GError **err);
gboolean g_rc_parse_user (TOPLEVEL *toplevel, const gchar *rcname, GError **err);
gboolean g_rc_parse_local (TOPLEVEL *toplevel, const gchar *rcname, const gchar *path, GError **err);
gboolean g_rc_parse_file (TOPLEVEL *toplevel, const gchar *rcfile, GError **err);
void g_rc_parse(TOPLEVEL *toplevel, const gchar* pname, const gchar* rcname, const gchar* rcfile);
void g_rc_parse_handler (TOPLEVEL *toplevel, const gchar *rcname, const gchar *rcfile, ConfigParseErrorFunc handler, void *user_data);

/* i_vars.c */
void i_vars_libgeda_set(TOPLEVEL *toplevel);
void i_vars_libgeda_freenames();

/* libgeda.c */
void libgeda_init(void);

/* m_basic.c */
void set_window(TOPLEVEL *toplevel, PAGE *page, int xmin, int xmax, int ymin, int ymax);
void rotate_point(int x, int y, int angle, int *newx, int *newy);
void rotate_point_90(int x, int y, int angle, int *newx, int *newy);
void PAPERSIZEtoWORLD(int width, int height, int border, int *right, int *bottom);

/* m_hatch.c */
void m_hatch_box(BOX *box, gint angle, gint pitch, GArray *lines);
void m_hatch_circle(CIRCLE *circle, gint angle, gint pitch, GArray *lines);
void m_hatch_path(PATH *path, gint angle, gint pitch, GArray *lines);

/* m_polygon.c */
void m_polygon_append_bezier(GArray *points, BEZIER *bezier, int segments);
void m_polygon_append_point(GArray *points, int x, int y);


/* o_arc_basic.c */
OBJECT *o_arc_new(TOPLEVEL *toplevel, char type, int color, int x, int y, int radius, int start_angle, int end_angle);
OBJECT *o_arc_copy(TOPLEVEL *toplevel, OBJECT *o_current);
void o_arc_modify(TOPLEVEL *toplevel, OBJECT *object, int x, int y, int whichone);
void o_arc_translate_world(TOPLEVEL *toplevel, int dx, int dy, OBJECT *object);
void o_arc_rotate_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_arc_mirror_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);

/* o_attrib.c */
void o_attrib_add(TOPLEVEL *toplevel, OBJECT *object, OBJECT *item);
gboolean o_attrib_is_attached (TOPLEVEL *toplevel, OBJECT *attrib, OBJECT *object);
void o_attrib_attach(TOPLEVEL *toplevel, OBJECT *attrib, OBJECT *object, int set_color);
void o_attrib_attach_list(TOPLEVEL *toplevel, GList *attr_list, OBJECT *object, int set_color);
void o_attrib_detach_all(TOPLEVEL *toplevel, OBJECT *object);
void o_attrib_print(GList *attributes);
void o_attrib_remove(TOPLEVEL *toplevel, GList **list, OBJECT *remove);
gboolean o_attrib_string_get_name_value (const gchar *string, gchar **name_ptr, gchar **value_ptr);
gboolean o_attrib_get_name_value (OBJECT *attrib, gchar **name_ptr, gchar **value_ptr);
GList *o_attrib_find_floating_attribs (const GList *list);
char *o_attrib_search_floating_attribs_by_name (const GList *list, char *name, int counter);
char *o_attrib_search_attached_attribs_by_name (OBJECT *object, char *name, int counter);
char *o_attrib_search_inherited_attribs_by_name (OBJECT *object, char *name, int counter);
char *o_attrib_search_object_attribs_by_name (OBJECT *object, char *name, int counter);
GList *o_attrib_return_attribs(OBJECT *object);
int o_attrib_is_inherited(OBJECT *attrib);
void o_attrib_append_attribs_changed_hook(TOPLEVEL *toplevel, AttribsChangedFunc func, void *data);
void o_attrib_emit_attribs_changed(TOPLEVEL *toplevel, OBJECT *object);
void o_attrib_freeze_hooks(TOPLEVEL *toplevel, OBJECT *object);
void o_attrib_thaw_hooks(TOPLEVEL *toplevel, OBJECT *object);

/* o_basic.c */
int inside_region(int xmin, int ymin, int xmax, int ymax, int x, int y);
void o_recalc_single_object(TOPLEVEL *toplevel, OBJECT *o_current);
void o_recalc_object_glist(TOPLEVEL *toplevel, GList *object_glist);
void o_set_line_options(TOPLEVEL *toplevel, OBJECT *o_current, OBJECT_END end, OBJECT_TYPE type, int width, int length, int space);
gboolean o_get_line_options(OBJECT *object, OBJECT_END *end, OBJECT_TYPE *type, int *width, int *length, int *space);
void o_set_fill_options(TOPLEVEL *toplevel, OBJECT *o_current, OBJECT_FILLING type, int width, int pitch1, int angle1, int pitch2, int angle2);
gboolean o_get_fill_options(OBJECT *object, OBJECT_FILLING *type, int *width, int *pitch1, int *angle1, int *pitch2, int *angle2);
gboolean o_get_position(TOPLEVEL *toplevel, gint *x, gint *y, OBJECT *object);
void o_translate_world (TOPLEVEL *toplevel, gint dx, gint dy, OBJECT *object);
void o_rotate_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_mirror_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);
double o_shortest_distance(OBJECT *object, int x, int y);
void o_set_color(TOPLEVEL *toplevel, OBJECT *object, int color);
PAGE *o_get_page (TOPLEVEL *toplevel, OBJECT *object);
OBJECT *o_get_parent (TOPLEVEL *toplevel, OBJECT *object);
void o_add_change_notify(TOPLEVEL *toplevel, ChangeNotifyFunc pre_change_func, ChangeNotifyFunc change_func, void *user_data);
void o_remove_change_notify(TOPLEVEL *toplevel, ChangeNotifyFunc pre_change_func, ChangeNotifyFunc change_func, void *user_data);
gboolean o_is_visible (TOPLEVEL *toplevel, OBJECT *object);
void o_set_visibility (TOPLEVEL *toplevel, OBJECT *object, int visibility);

/* o_box_basic.c */
OBJECT *o_box_new(TOPLEVEL *toplevel, char type, int color, int x1, int y1, int x2, int y2);
OBJECT *o_box_copy(TOPLEVEL *toplevel, OBJECT *o_current);
void o_box_modify_all (TOPLEVEL *toplevel, OBJECT *object, int x1, int y1, int x2, int y2);
void o_box_modify(TOPLEVEL *toplevel, OBJECT *object, int x, int y, int whichone);
void o_box_translate_world(TOPLEVEL *toplevel, int dx, int dy, OBJECT *object);
void o_box_rotate_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_box_mirror_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);

/* o_bus_basic.c */
OBJECT *o_bus_new(TOPLEVEL *toplevel, char type, int color, int x1, int y1, int x2, int y2, int bus_ripper_direction);
void o_bus_translate_world(TOPLEVEL *toplevel, int dx, int dy, OBJECT *object);
OBJECT *o_bus_copy(TOPLEVEL *toplevel, OBJECT *o_current);
void o_bus_rotate_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_bus_mirror_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);
int o_bus_orientation(OBJECT *object);
void o_bus_consolidate(TOPLEVEL *toplevel);
void o_bus_modify(TOPLEVEL *toplevel, OBJECT *object, int x, int y, int whichone);

/* o_circle_basic.c */
int dist(int x1, int y1, int x2, int y2);
OBJECT *o_circle_new(TOPLEVEL *toplevel, char type, int color, int x, int y, int radius);
OBJECT *o_circle_copy(TOPLEVEL *toplevel, OBJECT *o_current);
void o_circle_modify(TOPLEVEL *toplevel, OBJECT *object, int x, int y, int whichone);
void o_circle_translate_world(TOPLEVEL *toplevel, int dx, int dy, OBJECT *object);
void o_circle_rotate_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_circle_mirror_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);

/* o_complex_basic.c */
int world_get_single_object_bounds(TOPLEVEL *toplevel, OBJECT *o_current,
			      int *rleft, int *rtop, 
			      int *rright, int *rbottom);
int world_get_object_glist_bounds(TOPLEVEL *toplevel, const GList *o_list,
			     int *left, int *top, 
			     int *right, int *bottom);
int o_complex_is_embedded(OBJECT *o_current);
GList *o_complex_promote_attribs (TOPLEVEL *toplevel, OBJECT *object);
OBJECT *o_complex_new(TOPLEVEL *toplevel, char type, int color, int x, int y, int angle, int mirror, const CLibSymbol *clib_sym, const gchar *basename, int selectable);
OBJECT *o_complex_new_embedded(TOPLEVEL *toplevel, char type, int color, int x, int y, int angle, int mirror, const gchar *basename, int selectable);
void o_complex_set_filename(TOPLEVEL *toplevel, const char *basename);
void o_complex_translate_world(TOPLEVEL *toplevel, int dx, int dy, OBJECT *object);
OBJECT *o_complex_copy(TOPLEVEL *toplevel, OBJECT *o_current);
void o_complex_rotate_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_complex_mirror_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);
OBJECT *o_complex_find_pin_by_attribute(OBJECT *object, char *name, char *wanted_value);
void o_complex_check_symversion(TOPLEVEL* toplevel, OBJECT* object);

/* o_embed.c */
void o_embed(TOPLEVEL *toplevel, OBJECT *o_current);
void o_unembed(TOPLEVEL *toplevel, OBJECT *o_current);

/* o_line_basic.c */
OBJECT *o_line_new(TOPLEVEL *toplevel, char type, int color, int x1, int y1, int x2, int y2);
OBJECT *o_line_copy(TOPLEVEL *toplevel, OBJECT *o_current);
void o_line_modify(TOPLEVEL *toplevel, OBJECT *object, int x, int y, int whichone);
void o_line_translate_world(TOPLEVEL *toplevel, int dx, int dy, OBJECT *object);
void o_line_rotate_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_line_mirror_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);
void o_line_scale_world(TOPLEVEL *toplevel, int x_scale, int y_scale, OBJECT *object);
double o_line_length(OBJECT *object);

/* o_list.c */
OBJECT *o_object_copy(TOPLEVEL *toplevel, OBJECT *selected);
GList *o_glist_copy_all(TOPLEVEL *toplevel, const GList *src_list, GList *dest_list);
void o_glist_translate_world(TOPLEVEL *toplevel, int dx, int dy, const GList *list);
void o_glist_rotate_world(TOPLEVEL *toplevel, int x, int y, int angle, const GList *list);
void o_glist_mirror_world(TOPLEVEL *toplevel, int x, int y, const GList *list);
void o_glist_set_color(TOPLEVEL *toplevel, const GList *list, int color);

/* o_net_basic.c */
OBJECT *o_net_new(TOPLEVEL *toplevel, char type, int color, int x1, int y1, int x2, int y2);
void o_net_translate_world(TOPLEVEL *toplevel, int dx, int dy, OBJECT *object);
OBJECT *o_net_copy(TOPLEVEL *toplevel, OBJECT *o_current);
void o_net_rotate_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_net_mirror_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);
int o_net_orientation(OBJECT *object);
void o_net_consolidate(TOPLEVEL *toplevel, PAGE *page);
void o_net_modify(TOPLEVEL *toplevel, OBJECT *object, int x, int y, int whichone);
void o_net_refresh_conn_cache(TOPLEVEL *toplevel, OBJECT *object);
gboolean o_net_is_fully_connected(TOPLEVEL *toplevel, OBJECT *object);

/* o_path_basic.c */
OBJECT *o_path_new(TOPLEVEL *toplevel, char type, int color, const char *path_string);
OBJECT *o_path_copy(TOPLEVEL *toplevel, OBJECT *o_current);
void o_path_modify(TOPLEVEL *toplevel, OBJECT *object, int x, int y, int whichone);
void o_path_translate_world(TOPLEVEL *toplevel, int x, int y, OBJECT *object);
void o_path_rotate_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_path_mirror_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);

/* o_picture.c */
OBJECT *o_picture_new(TOPLEVEL *toplevel,
                      const gchar *file_content, gsize file_length,
                      const gchar *filename, char type,
                      int x1, int y1, int x2, int y2, int angle, int mirrored,
                      int embedded) G_GNUC_WARN_UNUSED_RESULT;
double o_picture_get_ratio (TOPLEVEL *toplevel, OBJECT *object);
void o_picture_modify(TOPLEVEL *toplevel, OBJECT *object, int x, int y, int whichone);
void o_picture_modify_all (TOPLEVEL *toplevel, OBJECT *object, int x1, int y1, int x2, int y2);
void o_picture_rotate_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle,OBJECT *object);
void o_picture_mirror_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);
void o_picture_translate_world(TOPLEVEL *toplevel, int dx, int dy, OBJECT *object);
OBJECT *o_picture_copy(TOPLEVEL *toplevel, OBJECT *o_current) G_GNUC_WARN_UNUSED_RESULT;
gboolean o_picture_is_embedded (TOPLEVEL *toplevel, OBJECT *object);
GdkPixbuf *o_picture_get_pixbuf (TOPLEVEL *toplevel, OBJECT *object) G_GNUC_WARN_UNUSED_RESULT;
const char *o_picture_get_data (TOPLEVEL *toplevel, OBJECT *object,
                                size_t *len);
gboolean o_picture_set_from_buffer (TOPLEVEL *toplevel, OBJECT *object,
                                    const gchar *filename, const gchar *data,
                                    size_t len, GError **error);
gboolean o_picture_set_from_file (TOPLEVEL *toplevel, OBJECT *object,
                                  const gchar *filename, GError **error);
const gchar *o_picture_get_filename (TOPLEVEL *toplevel, OBJECT *object);
GdkPixbuf *o_picture_get_fallback_pixbuf (TOPLEVEL *toplevel) G_GNUC_WARN_UNUSED_RESULT;

/* o_pin_basic.c */
OBJECT *o_pin_new(TOPLEVEL *toplevel, char type, int color, int x1, int y1, int x2, int y2, int pin_type, int whichend);
void o_pin_translate_world(TOPLEVEL *toplevel, int dx, int dy, OBJECT *object);
OBJECT *o_pin_copy(TOPLEVEL *toplevel, OBJECT *o_current);
void o_pin_rotate_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_pin_mirror_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);
void o_pin_modify(TOPLEVEL *toplevel, OBJECT *object, int x, int y, int whichone);
void o_pin_update_whichend(TOPLEVEL *toplevel, GList *object_list, int num_pins);
void o_pin_set_type(TOPLEVEL *toplevel, OBJECT *o_current, int pin_type);
/* o_selection.c */
SELECTION *o_selection_new( void );
void o_selection_add(TOPLEVEL *toplevel, SELECTION *selection, OBJECT *o_selected);
void o_selection_print_all(const SELECTION *selection);
void o_selection_remove(TOPLEVEL *toplevel, SELECTION *selection, OBJECT *o_selected);
void o_selection_select(TOPLEVEL *toplevel, OBJECT *object) G_GNUC_DEPRECATED;
void o_selection_unselect(TOPLEVEL *toplevel, OBJECT *object) G_GNUC_DEPRECATED;

/* o_text_basic.c */
int o_text_num_lines(const char *string);
OBJECT *o_text_new(TOPLEVEL *toplevel, char type, int color, int x, int y, int alignment, int angle, const char *string, int size, int visibility, int show_name_value);
void o_text_recreate(TOPLEVEL *toplevel, OBJECT *o_current);
void o_text_translate_world(TOPLEVEL *toplevel, int dx, int dy, OBJECT *o_current);
OBJECT *o_text_copy(TOPLEVEL *toplevel, OBJECT *o_current);
void o_text_rotate_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_text_mirror_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);
void o_text_set_string(TOPLEVEL *toplevel, OBJECT *obj, const gchar *new_string);
const gchar *o_text_get_string(TOPLEVEL *toplevel, OBJECT *obj);
void o_text_set_rendered_bounds_func (TOPLEVEL *toplevel, RenderedBoundsFunc func, void *user_data);
double o_text_get_font_size_in_points(TOPLEVEL *toplevel, OBJECT *object);

/* s_attrib.c */
int s_attrib_add_entry(char *new_attrib);
void s_attrib_print(void);
int s_attrib_uniq(char *name);
void s_attrib_free(void);
void s_attrib_init(void);
char *s_attrib_get(int counter);

/* s_basic.c */
OBJECT *s_basic_init_object(OBJECT *new_node, int type, char const *name);
OBJECT *s_basic_new_object(int type, char const *prefix);
void print_struct_forw(GList *list);
void print_struct(OBJECT *ptr);
void s_delete_object(TOPLEVEL *toplevel, OBJECT *o_current);
void s_delete_object_glist(TOPLEVEL *toplevel, GList *list);
void s_object_weak_ref (OBJECT *object, void (*notify_func)(void *, void *), void *user_data);
void s_object_weak_unref (OBJECT *object, void (*notify_func)(void *, void *), void *user_data);
void s_object_add_weak_ptr (OBJECT *object, void *weak_pointer_loc);
void s_object_remove_weak_ptr (OBJECT *object, void *weak_pointer_loc);
char *remove_nl(char *string);
char *remove_last_nl(char *string);
gchar *s_expand_env_variables (const gchar *string);
const char *s_path_sys_data ();
const char *s_path_sys_config ();
const char *s_path_user_config ();

/* s_clib.c */
void s_clib_free (void);
GList *s_clib_get_sources (const gboolean sorted);
const CLibSource *s_clib_get_source_by_name (const gchar *name);
void s_clib_refresh ();
const CLibSource *s_clib_add_directory (const gchar *directory, 
					const gchar *name);
const CLibSource *s_clib_add_command (const gchar *list_cmd,
                                      const gchar *get_cmd,
				      const gchar *name);
const CLibSource *s_clib_add_scm (SCM listfunc, SCM getfunc, 
				  const gchar *name);
const gchar *s_clib_source_get_name (const CLibSource *source);
GList *s_clib_source_get_symbols (const CLibSource *source);
const gchar *s_clib_symbol_get_name (const CLibSymbol *symbol);
gchar *s_clib_symbol_get_filename (const CLibSymbol *symbol);
const CLibSource *s_clib_symbol_get_source (const CLibSymbol *symbol);
gchar *s_clib_symbol_get_data (const CLibSymbol *symbol);
GList *s_clib_search (const gchar *pattern, const CLibSearchMode mode);
void s_clib_flush_search_cache ();
void s_clib_flush_symbol_cache ();
void s_clib_symbol_invalidate_data (const CLibSymbol *symbol);
const CLibSymbol *s_clib_get_symbol_by_name (const gchar *name);
gchar *s_clib_symbol_get_data_by_name (const gchar *name);
GList *s_toplevel_get_symbols (const TOPLEVEL *toplevel);

/* s_color.c */
void s_color_map_defaults (COLOR *map);
gboolean s_color_rgba_decode (const gchar *rgba,
                              guchar *r, guchar *g, guchar *b, guchar *a);
gchar *s_color_rgba_encode (guint8 r, guint8 g, guint8 b, guint8 a);
SCM s_color_map_to_scm (const COLOR *map);
void s_color_map_from_scm (COLOR *map, SCM lst, const char *scheme_proc_name);

/* s_conn.c */
void s_conn_remove_object(TOPLEVEL *toplevel, OBJECT *to_remove);
void s_conn_update_object(TOPLEVEL *toplevel, OBJECT *object);
int s_conn_net_search(OBJECT* new_net, int whichone, GList * conn_list);
GList *s_conn_return_others(GList *input_list, OBJECT *object);
void s_conn_append_conns_changed_hook(TOPLEVEL *toplevel, ConnsChangedFunc func, void *data);
void s_conn_emit_conns_changed(TOPLEVEL *toplevel, OBJECT *object);
void s_conn_freeze_hooks(TOPLEVEL *toplevel, OBJECT *object);
void s_conn_thaw_hooks(TOPLEVEL *toplevel, OBJECT *object);

/* s_cue.c */
void s_cue_postscript_fillbox(TOPLEVEL *toplevel, FILE *fp, int x, int y);
void s_cue_postscript_junction (TOPLEVEL *toplevel, FILE *fp, int x, int y, int bus_involved);
void s_cue_output_all(TOPLEVEL *toplevel, const GList *obj_list, FILE *fp, int type);
void s_cue_output_lowlevel(TOPLEVEL *toplevel, OBJECT *object, int whichone, FILE *fp, int output_type);
void s_cue_output_lowlevel_midpoints(TOPLEVEL *toplevel, OBJECT *object, FILE *fp, int output_type);
void s_cue_output_single(TOPLEVEL *toplevel, OBJECT *object, FILE *fp, int type);

/* s_hierarchy.c */
PAGE *s_hierarchy_down_schematic_single(TOPLEVEL *toplevel, const gchar *filename, PAGE *parent, int page_control, int flag);
void s_hierarchy_down_symbol (TOPLEVEL *toplevel, const CLibSymbol *symbol, PAGE *parent);
PAGE *s_hierarchy_find_up_page(GedaPageList *page_list, PAGE *current_page);
GList* s_hierarchy_traversepages(TOPLEVEL *toplevel, PAGE *p_current, gint flags);
gint s_hierarchy_print_page(PAGE *p_current, void * data);
PAGE *s_hierarchy_find_prev_page(GedaPageList *page_list, PAGE *current_page);
PAGE *s_hierarchy_find_next_page(GedaPageList *page_list, PAGE *current_page);

/* s_log.c */
void s_log_init (const gchar *filename);
void s_log_close (void);
gchar *s_log_read (void);

/* s_menu.c */
int s_menu_return_num(void);
SCM s_menu_return_entry(int index, char **menu_name);
int s_menu_add_entry(char *new_menu, SCM menu_items);
void s_menu_print(void);
void s_menu_free(void);
void s_menu_init(void);

/* s_page.c */
PAGE *s_page_new (TOPLEVEL *toplevel, const gchar *filename);
void s_page_delete (TOPLEVEL *toplevel, PAGE *page);
void s_page_delete_list(TOPLEVEL *toplevel);
void s_page_weak_ref (PAGE *page, void (*notify_func)(void *, void *), void *user_data);
void s_page_weak_unref (PAGE *page, void (*notify_func)(void *, void *), void *user_data);
void s_page_add_weak_ptr (PAGE *page, void *weak_pointer_loc);
void s_page_remove_weak_ptr (PAGE *page, void *weak_pointer_loc);
void s_page_goto (TOPLEVEL *toplevel, PAGE *p_new);
PAGE *s_page_search (TOPLEVEL *toplevel, const gchar *filename);
PAGE *s_page_search_by_page_id (GedaPageList *list, int pid);
void s_page_print_all (TOPLEVEL *toplevel);
gint s_page_save_all (TOPLEVEL *toplevel);
gboolean s_page_check_changed (GedaPageList *list);
void s_page_clear_changed (GedaPageList *list);
void s_page_autosave_init(TOPLEVEL *toplevel);
gint s_page_autosave (TOPLEVEL *toplevel);
void s_page_append (TOPLEVEL *toplevel, PAGE *page, OBJECT *object);
void s_page_append_list (TOPLEVEL *toplevel, PAGE *page, GList *obj_list);
void s_page_remove (TOPLEVEL *toplevel, PAGE *page, OBJECT *object);
void s_page_replace (TOPLEVEL *toplevel, PAGE *page, OBJECT *object1, OBJECT *object2);
void s_page_delete_objects (TOPLEVEL *toplevel, PAGE *page);
const GList *s_page_objects (PAGE *page);
GList *s_page_objects_in_region (TOPLEVEL *toplevel, PAGE *page, int min_x, int min_y, int max_x, int max_y);
GList *s_page_objects_in_regions (TOPLEVEL *toplevel, PAGE *page, BOX *rects, int n_rects);

/* s_papersizes.c */
int s_papersizes_add_entry(char *new_papersize, int width, int height);
void s_papersizes_print(void);
int s_papersizes_uniq(char *name);
void s_papersizes_free(void);
void s_papersizes_init(void);
char *s_papersizes_get(int counter);
void s_papersizes_get_size(char *string, int *width, int *height);

/* s_path.c */
PATH *s_path_parse (const char *path_str);
char *s_path_string_from_path (const PATH *path);

/* s_toplevel.c */
void s_toplevel_append_new_hook (NewToplevelFunc func, void *user_data);
TOPLEVEL *s_toplevel_new (void);
void s_toplevel_delete (TOPLEVEL *toplevel);
void s_toplevel_weak_ref (TOPLEVEL *toplevel, void (*notify_func)(void *, void *), void *user_data);
void s_toplevel_weak_unref (TOPLEVEL *toplevel, void (*notify_func)(void *, void *), void *user_data);
void s_toplevel_add_weak_ptr (TOPLEVEL *toplevel, void *weak_pointer_loc);
void s_toplevel_remove_weak_ptr (TOPLEVEL *toplevel, void *weak_pointer_loc);

/* s_slib.c */
int s_slib_add_entry(char *new_path);
int s_slib_search_for_dirname(char *dir_name);
char *s_slib_search_dirs(const char *basename);
char *s_slib_search_lowlevel(const char *basename);
char *s_slib_getbasename(const char *rawname);
char *s_slib_search(const char *filename, int flag);
char *s_slib_search_single(const char *filename);
void s_slib_free(void);
void s_slib_init(void);
char *s_slib_getdir(int index);
char *s_slib_getfiles(char *directory, int flag);
void s_slib_print(void);
int s_slib_uniq(char *path);
void s_slib_print_dirs(void);

/* s_slot.c */
char *s_slot_search_slot(OBJECT *object, OBJECT **return_found);
void s_slot_update_object(TOPLEVEL *toplevel, OBJECT *object);

/* s_tile.c */
void s_tile_update_object(TOPLEVEL *toplevel, OBJECT *object);
GList *s_tile_get_objectlists(TOPLEVEL *toplevel, PAGE *p_current, int world_x1, int world_y1, int world_x2, int world_y2);

/* s_undo.c */
UNDO *s_undo_return_tail(UNDO *head);
UNDO *s_undo_return_head(UNDO *tail);
UNDO *s_undo_new_head(void);
void s_undo_destroy_head(UNDO *u_head);
UNDO *s_undo_add(UNDO *head, int type, char *filename, GList *object_list, int left, int top, int right, int bottom, int page_control, int up);
void s_undo_print_all(UNDO *head);
void s_undo_destroy_all(TOPLEVEL *toplevel, UNDO *head);
void s_undo_remove(TOPLEVEL *toplevel, UNDO *head, UNDO *u_tos);
void s_undo_remove_rest(TOPLEVEL *toplevel, UNDO *head);
int s_undo_levels(UNDO *head);
void s_undo_init(PAGE *p_current);
void s_undo_free_all(TOPLEVEL *toplevel, PAGE *p_current);

/* u_basic.c */
char *u_basic_breakup_string(char *string, char delimiter, int count);

G_END_DECLS
