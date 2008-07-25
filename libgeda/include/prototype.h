
/* a_basic.c */
const gchar *o_file_format_header();
gchar *o_save_buffer(TOPLEVEL *toplevel);
int o_save(TOPLEVEL *toplevel, const char *filename);
OBJECT *o_read_buffer(TOPLEVEL *toplevel, OBJECT *object_list, char *buffer, const int size, const char *name);
OBJECT *o_read(TOPLEVEL *toplevel, OBJECT *object_list, char *filename, GError **err);
void o_scale(TOPLEVEL *toplevel, OBJECT *list, int x_scale, int y_scale);

/* f_basic.c */
gchar *f_get_autosave_filename (const gchar *filename);
gboolean f_has_active_autosave (const gchar *filename, GError **err);
int f_open(TOPLEVEL *toplevel, const gchar *filename, GError **err);
int f_open_flags(TOPLEVEL *toplevel, const gchar *filename,
                 const gint flags, GError **err);
void f_close(TOPLEVEL *toplevel);
void f_save_close(TOPLEVEL *toplevel, char *filename);
int f_save(TOPLEVEL *toplevel, const char *filename);
gchar *f_normalize_filename (const gchar *filename, GError **error);
char *follow_symlinks (const gchar *filename, GError **error);

/* f_print.c */
int f_print_file (TOPLEVEL *toplevel, const char *filename);
int f_print_command (TOPLEVEL *toplevel, const char *command);
int f_print_stream(TOPLEVEL *toplevel, FILE *fp);
void f_print_set_type(TOPLEVEL *toplevel, int type);

/* g_basic.c */
SCM g_scm_eval_protected (SCM exp, SCM module_or_state);
SCM g_scm_eval_string_protected (SCM str);
SCM g_scm_c_eval_string_protected (const gchar *str);
int g_read_file(const gchar *filename);

/* g_rc.c */
SCM g_rc_mode_general(SCM scmmode, const char *rc_name, int *mode_var, 
                      const vstbl_entry *table, int table_size);
gint g_rc_parse_general(TOPLEVEL *toplevel,
                        const gchar *fname, 
                        const gchar *ok_msg, const gchar *err_msg);
const char *g_rc_parse_path(void);
gint g_rc_parse_system_rc(TOPLEVEL *toplevel, const gchar *rcname);
gint g_rc_parse_home_rc(TOPLEVEL *toplevel, const gchar *rcname);
gint g_rc_parse_local_rc(TOPLEVEL *toplevel, const gchar *rcname);
void g_rc_parse(TOPLEVEL *toplevel, const gchar* rcname,
                const gchar* specified_rc_filename);
gint g_rc_parse_specified_rc(TOPLEVEL *toplevel, const gchar *rcfilename);

/* g_smob.c */
SCM g_make_attrib_smob(TOPLEVEL *curr_w, ATTRIB *curr_attr);
SCM g_set_attrib_value_internal(SCM attrib_smob, SCM scm_value, TOPLEVEL **world, OBJECT **o_attrib, char *new_string[]);
gboolean g_get_data_from_object_smob(SCM object_smob, TOPLEVEL **toplevel, 
				     OBJECT **object);
SCM g_make_object_smob(TOPLEVEL *curr_w, OBJECT *object);
SCM g_get_object_attributes(SCM object_smob);
SCM g_make_page_smob(TOPLEVEL *curr_w, PAGE *page);
gboolean g_get_data_from_page_smob(SCM object_smob, TOPLEVEL **toplevel, 
				   PAGE **object);

/* i_vars.c */
void i_vars_libgeda_set(TOPLEVEL *toplevel);
void i_vars_freenames();

/* gdk-pixbuf-hacks.c */
GdkPixbuf *gdk_pixbuf_rotate (GdkPixbuf *pixbuf, guint angle);
void gdk_pixbuf_add (GdkPixbuf *pixbuf, int offset_x, int offset_y, GdkPixbuf *pixbuf_to_add);
GdkPixbuf *gdk_pixbuf_mirror_flip(GdkPixbuf *src, gint mirror, gint flip);

/* libgeda.c */
void libgeda_init(void);

/* m_basic.c */
int mil_x(TOPLEVEL *toplevel, int val);
int mil_y(TOPLEVEL *toplevel, int val);
int pix_x(TOPLEVEL *toplevel, int val);
int pix_y(TOPLEVEL *toplevel, int val);
void WORLDtoSCREEN(TOPLEVEL *toplevel, int x, int y, int *mil_x, int *mil_y);
void SCREENtoWORLD(TOPLEVEL *toplevel, int mx, int my, int *x, int *y);
int snap_grid(TOPLEVEL *toplevel, int input);
int SCREENabs(TOPLEVEL *toplevel, int val);
int WORLDabs(TOPLEVEL *toplevel, int val);
void set_window(TOPLEVEL *toplevel, PAGE *page, int xmin, int xmax, int ymin, int ymax);
int on_snap(int val);
int SCREENclip_change(TOPLEVEL *toplevel, int *x1, int *y1, int *x2, int *y2);
int clip_nochange(TOPLEVEL *toplevel, int x1, int y1, int x2, int y2);
int visible(TOPLEVEL *toplevel, int wleft, int wtop, int wright, int wbottom);
void rotate_point(int x, int y, int angle, int *newx, int *newy);
void rotate_point_90(int x, int y, int angle, int *newx, int *newy);
void PAPERSIZEtoWORLD(int width, int height, int border, int *right, int *bottom);
double round_5_2_1(double unrounded);

/* o_arc_basic.c */
OBJECT *o_arc_add(TOPLEVEL *toplevel, OBJECT *object_list, char type, int color, int x, int y, int radius, int start_angle, int end_angle);
OBJECT *o_arc_copy(TOPLEVEL *toplevel, OBJECT *list_tail, OBJECT *o_current);
void o_arc_modify(TOPLEVEL *toplevel, OBJECT *object, int x, int y, int whichone);
void o_arc_translate_world(TOPLEVEL *toplevel, int dx, int dy, OBJECT *object);
void o_arc_rotate_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_arc_mirror_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);
void o_arc_recalc(TOPLEVEL *toplevel, OBJECT *o_current);
void world_get_arc_bounds(TOPLEVEL *toplevel, OBJECT *object, int *left, int *top, int *right, int *bottom);

/* o_attrib.c */
ATTRIB *o_attrib_search(GList *list, OBJECT *item);
void o_attrib_add(TOPLEVEL *toplevel, OBJECT *object, OBJECT *item);
void o_attrib_free(TOPLEVEL *toplevel, ATTRIB *current);
void o_attrib_attach(TOPLEVEL *toplevel, OBJECT *parent_list, OBJECT *text_object, OBJECT *object);
void o_attrib_free_all(TOPLEVEL *toplevel, GList *list);
void o_attrib_print(GList *attributes);
void o_attrib_remove(GList **list, OBJECT *remove);
gboolean o_attrib_get_name_value (const gchar *string, gchar **name_ptr, gchar **value_ptr);
void o_attrib_free_current(TOPLEVEL *toplevel);
void o_attrib_set_string(TOPLEVEL *toplevel, char *string);
void o_attrib_set_color(TOPLEVEL *toplevel, GList *attributes);
char *o_attrib_search_name(OBJECT *list, char *name, int counter);
OBJECT *o_attrib_search_string_list(OBJECT *list, char *string);
char *o_attrib_search_string_partial(OBJECT *object, char *search_for, int counter);
OBJECT *o_attrib_search_string_single(OBJECT *object, char *search_for);
OBJECT *o_attrib_search_attrib_value(GList *list, char *value, char *name, int counter);
char *o_attrib_search_attrib_name(GList *list, char *name, int counter);
char *o_attrib_search_toplevel(OBJECT *list, char *name, int counter);
char *o_attrib_search_name_single(OBJECT *object, char *name, OBJECT **return_found);
char *o_attrib_search_name_single_count(OBJECT *object, char *name, int counter);
char *o_attrib_search_slot(OBJECT *object, OBJECT **return_found);
char *o_attrib_search_numslots(OBJECT *object, OBJECT **return_found);
char *o_attrib_search_default_slot(OBJECT *object);
OBJECT *o_attrib_search_pinseq(OBJECT *list, int pin_number);
char *o_attrib_search_slotdef(OBJECT *object, int slotnumber);
char *o_attrib_search_component(OBJECT *object, char *name);
void o_attrib_slot_update(TOPLEVEL *toplevel, OBJECT *object);
void o_attrib_slot_copy(TOPLEVEL *toplevel, OBJECT *original, OBJECT *target);
char *o_attrib_search_toplevel_all(GedaPageList *page_list, char *name);
OBJECT **o_attrib_return_attribs(OBJECT *object_list, OBJECT *sel_object);
void o_attrib_free_returned(OBJECT **found_objects);

/* o_basic.c */
int inside_region(int xmin, int ymin, int xmax, int ymax, int x, int y);
void o_recalc_single_object(TOPLEVEL *toplevel, OBJECT *o_current);
void o_recalc_object_list(TOPLEVEL *toplevel, OBJECT *object_list);
void o_recalc_object_glist(TOPLEVEL *toplevel, GList *object_glist);
void o_set_line_options(TOPLEVEL *toplevel, OBJECT *o_current, OBJECT_END end, OBJECT_TYPE type, int width, int length, int space);
void o_set_fill_options(TOPLEVEL *toplevel, OBJECT *o_current, OBJECT_FILLING type, int width, int pitch1, int angle1, int pitch2, int angle2);
void o_translate_world (TOPLEVEL *toplevel, gint dx, gint dy, OBJECT *object);
void o_rotate_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_mirror_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);
/* o_box_basic.c */
OBJECT *o_box_add(TOPLEVEL *toplevel, OBJECT *object_list, char type, int color, int x1, int y1, int x2, int y2);
OBJECT *o_box_copy(TOPLEVEL *toplevel, OBJECT *list_tail, OBJECT *o_current);
void o_box_modify(TOPLEVEL *toplevel, OBJECT *object, int x, int y, int whichone);
void o_box_translate_world(TOPLEVEL *toplevel, int x1, int y1, OBJECT *object);
void o_box_rotate_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_box_mirror_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);
void o_box_recalc(TOPLEVEL *toplevel, OBJECT *o_current);
void world_get_box_bounds(TOPLEVEL *toplevel, OBJECT *object, int *left, int *top, int *right, int *bottom);

/* o_bus_basic.c */
void world_get_bus_bounds(TOPLEVEL *toplevel, OBJECT *object, int *left, int *top, int *right, int *bottom);
OBJECT *o_bus_add(TOPLEVEL *toplevel, OBJECT *object_list, char type, int color, int x1, int y1, int x2, int y2, int bus_ripper_direction);
void o_bus_recalc(TOPLEVEL *toplevel, OBJECT *o_current);
void o_bus_translate_world(TOPLEVEL *toplevel, int x1, int y1, OBJECT *object);
OBJECT *o_bus_copy(TOPLEVEL *toplevel, OBJECT *list_tail, OBJECT *o_current);
void o_bus_rotate_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_bus_mirror_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);
int o_bus_orientation(OBJECT *object);
void o_bus_consolidate_lowlevel(OBJECT *object, OBJECT *del_object, int orient);
int o_bus_consolidate_segments(TOPLEVEL *toplevel, OBJECT *object);
void o_bus_consolidate(TOPLEVEL *toplevel);
void o_bus_modify(TOPLEVEL *toplevel, OBJECT *object, int x, int y, int whichone);

/* o_circle_basic.c */
int dist(int x1, int y1, int x2, int y2);
OBJECT *o_circle_add(TOPLEVEL *toplevel, OBJECT *object_list, char type, int color, int x, int y, int radius);
OBJECT *o_circle_copy(TOPLEVEL *toplevel, OBJECT *list_tail, OBJECT *o_current);
void o_circle_modify(TOPLEVEL *toplevel, OBJECT *object, int x, int y, int whichone);
void o_circle_translate_world(TOPLEVEL *toplevel, int x1, int y1, OBJECT *object);
void o_circle_rotate_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_circle_mirror_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);
void o_circle_recalc(TOPLEVEL *toplevel, OBJECT *o_current);
void world_get_circle_bounds(TOPLEVEL *toplevel, OBJECT *object, int *left, int *top, int *right, int *bottom);

/* o_complex_basic.c */
int world_get_single_object_bounds(TOPLEVEL *toplevel, OBJECT *o_current,
			      int *rleft, int *rtop, 
			      int *rright, int *rbottom);
int world_get_object_list_bounds(TOPLEVEL *toplevel, OBJECT *complex,
			    int *left, int *top, int *right, int *bottom);
int world_get_object_glist_bounds(TOPLEVEL *toplevel, GList *o_list,
			     int *left, int *top, 
			     int *right, int *bottom);
void world_get_complex_bounds(TOPLEVEL *toplevel, OBJECT *complex, int *left, int *top, int *right, int *bottom);
OBJECT *add_head(void);
int o_complex_is_eligible_attribute(TOPLEVEL *toplevel, OBJECT *object, int promote_invisible);
int o_complex_is_embedded(OBJECT *o_current);
OBJECT *o_complex_add(TOPLEVEL *toplevel, OBJECT *object_list,
		      GList **object_glist, char type, int color, 
		      int x, int y, int angle, int mirror, 
		      const CLibSymbol *clib_sym, const gchar *basename,
		      int selectable, int attribute_promotion);
OBJECT *o_complex_add_embedded(TOPLEVEL *toplevel, OBJECT *object_list, char type, int color, int x, int y, int angle, int mirror, const gchar *basename, int selectable);
void o_complex_recalc(TOPLEVEL *toplevel, OBJECT *o_current);
void o_complex_set_filename(TOPLEVEL *toplevel, const char *basename);
void o_complex_free_filename(TOPLEVEL *toplevel);
void o_complex_translate_world(TOPLEVEL *toplevel, int x1, int y1, OBJECT *object);
OBJECT *o_complex_copy(TOPLEVEL *toplevel, OBJECT *list_tail, OBJECT *o_current);
OBJECT *o_complex_copy_embedded(TOPLEVEL *toplevel, OBJECT *list_tail, OBJECT *o_current);
void o_complex_set_color(OBJECT *prim_objs, int color);
void o_complex_set_color_single(OBJECT *o_current, int color);
void o_complex_set_color_save(OBJECT *complex, int color);
void o_complex_unset_color(OBJECT *complex);
void o_complex_unset_color_single(OBJECT *o_current);
void o_complex_set_saved_color_only(OBJECT *complex, int color);
OBJECT *o_complex_return_nth_pin(OBJECT *o_list, int counter);
void o_complex_rotate_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_complex_mirror_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);
OBJECT *o_complex_return_pin_object(OBJECT *object, char *pin);
int  o_complex_count_pins(OBJECT *object);
void o_complex_check_symversion(TOPLEVEL* toplevel, OBJECT* object);

/* o_embed.c */
void o_embed(TOPLEVEL *toplevel, OBJECT *o_current);
void o_unembed(TOPLEVEL *toplevel, OBJECT *o_current);

/* o_line_basic.c */
OBJECT *o_line_add(TOPLEVEL *toplevel, OBJECT *object_list, char type, int color, int x1, int y1, int x2, int y2);
OBJECT *o_line_copy(TOPLEVEL *toplevel, OBJECT *list_tail, OBJECT *o_current);
void o_line_modify(TOPLEVEL *toplevel, OBJECT *object, int x, int y, int whichone);
void o_line_translate_world(TOPLEVEL *toplevel, int x1, int y1, OBJECT *object);
void o_line_rotate_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_line_mirror_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);
void o_line_recalc(TOPLEVEL *toplevel, OBJECT *o_current);
void world_get_line_bounds(TOPLEVEL *toplevel, OBJECT *object, int *left, int *top, int *right, int *bottom);
void o_line_scale_world(TOPLEVEL *toplevel, int x_scale, int y_scale, OBJECT *object);
int o_line_visible(TOPLEVEL *toplevel, LINE *line, int *x1, int *y1, int *x2, int *y2);
double o_line_length(OBJECT *object);

/* o_list.c */
OBJECT *o_list_copy_to(TOPLEVEL *toplevel, OBJECT *list_head, OBJECT *selected, int flag, OBJECT **return_end);
OBJECT *o_list_copy_all(TOPLEVEL *toplevel, OBJECT *src_list_head, OBJECT *dest_list_head, int flag);
GList *o_glist_copy_all_to_glist(TOPLEVEL *toplevel, GList *src_list, GList *dest_list, int flag);
OBJECT *o_list_search(OBJECT *list, OBJECT *current);
void o_list_delete(TOPLEVEL *toplevel, OBJECT *list, OBJECT *delete);
void o_list_delete_rest(TOPLEVEL *toplevel, OBJECT *list);
void o_list_translate_world(TOPLEVEL *toplevel, int x, int y, OBJECT *list);
void o_glist_translate_world(TOPLEVEL *toplevel, int x, int y, GList *list);
void o_list_rotate_world(TOPLEVEL *toplevel, int x, int y, int angle, OBJECT *list);
void o_glist_rotate_world(TOPLEVEL *toplevel, int x, int y, int angle, GList *list);
void o_list_mirror_world(TOPLEVEL *toplevel, int x, int y, OBJECT *list);
void o_glist_mirror_world(TOPLEVEL *toplevel, int x, int y, GList *list);

/* o_net_basic.c */
void world_get_net_bounds(TOPLEVEL *toplevel, OBJECT *object, int *left, int *top, int *right, int *bottom);
OBJECT *o_net_add(TOPLEVEL *toplevel, OBJECT *object_list, char type, int color, int x1, int y1, int x2, int y2);
void o_net_recalc(TOPLEVEL *toplevel, OBJECT *o_current);
void o_net_translate_world(TOPLEVEL *toplevel, int x1, int y1, OBJECT *object);
OBJECT *o_net_copy(TOPLEVEL *toplevel, OBJECT *list_tail, OBJECT *o_current);
void o_net_rotate_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_net_mirror_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);
int o_net_orientation(OBJECT *object);
void o_net_consolidate_lowlevel(OBJECT *object, OBJECT *del_object, int orient);
int o_net_consolidate_nomidpoint(OBJECT *object, int x, int y);
int o_net_consolidate_segments(TOPLEVEL *toplevel, OBJECT *object);
void o_net_consolidate(TOPLEVEL *toplevel);
void o_net_modify(TOPLEVEL *toplevel, OBJECT *object, int x, int y, int whichone);

/* o_picture.c */
OBJECT *o_picture_add(TOPLEVEL *toplevel, OBJECT *list_tail, GdkPixbuf *pixbuf,
                      gchar *file_content, gsize file_length, char *filename,
                      double ratio, char type,
                      int x1, int y1, int x2, int y2, int angle, char mirrored,
                      char embedded);
void o_picture_recalc(TOPLEVEL *toplevel, OBJECT *o_current);
void world_get_picture_bounds(TOPLEVEL *toplevel, OBJECT *object, int *left, int *top, int *right, int *bottom);
void o_picture_modify(TOPLEVEL *toplevel, OBJECT *object, int x, int y, int whichone);
void o_picture_rotate_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle,OBJECT *object);
void o_picture_mirror_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);
void o_picture_translate_world(TOPLEVEL *toplevel, int x1, int y1, OBJECT *object);
OBJECT *o_picture_copy(TOPLEVEL *toplevel, OBJECT *list_tail, OBJECT *o_current);
guint8 *o_picture_rgb_data(GdkPixbuf *image);
guint8 *o_picture_mask_data(GdkPixbuf *image);
void o_picture_embed(TOPLEVEL *toplevel, OBJECT *object);
void o_picture_unembed(TOPLEVEL *toplevel, OBJECT *object);
GdkPixbuf *o_picture_pixbuf_from_buffer (gchar *file_content, gsize file_length, GError **err);

/* o_pin_basic.c */
void world_get_pin_bounds(TOPLEVEL *toplevel, OBJECT *object, int *left, int *top, int *right, int *bottom);
OBJECT *o_pin_add(TOPLEVEL *toplevel, OBJECT *object_list, char type, int color, int x1, int y1, int x2, int y2, int pin_type, int whichend);
void o_pin_recalc(TOPLEVEL *toplevel, OBJECT *o_current);
void o_pin_translate_world(TOPLEVEL *toplevel, int x1, int y1, OBJECT *object);
OBJECT *o_pin_copy(TOPLEVEL *toplevel, OBJECT *list_tail, OBJECT *o_current);
void o_pin_rotate_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_pin_mirror_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);
void o_pin_modify(TOPLEVEL *toplevel, OBJECT *object, int x, int y, int whichone);
void o_pin_update_whichend(TOPLEVEL *toplevel, OBJECT *object_list, int num_pins);

/* o_selection.c */
SELECTION *o_selection_new( void );
void o_selection_add(SELECTION *selection, OBJECT *o_selected);
void o_selection_print_all(const SELECTION *selection);
void o_selection_remove(SELECTION *selection, OBJECT *o_selected);
void o_selection_select(OBJECT *object, int color); /* DEPRECATED */
void o_selection_unselect(OBJECT *object);          /* DEPRECATED */

/* o_text_basic.c */
int world_get_text_bounds(TOPLEVEL *toplevel, OBJECT *o_current, int *left, int *top, int *right, int *bottom);
OBJECT *o_text_add_head(void);
void o_text_init(void);
void o_text_print_set(void);
OBJECT *o_text_load_font(TOPLEVEL *toplevel, gunichar needed_char);
int o_text_num_lines(char *string);
int o_text_height(char *string, int size);
int o_text_width(TOPLEVEL *toplevel, char *string, int size);
OBJECT *o_text_create_string(TOPLEVEL *toplevel, OBJECT *object_list, char *string, int size, int color, int x, int y, int alignment, int angle);
OBJECT *o_text_add(TOPLEVEL *toplevel, OBJECT *object_list, char type, int color, int x, int y, int alignment, int angle, char *string, int size, int visibility, int show_name_value);
void o_text_recalc(TOPLEVEL *toplevel, OBJECT *o_current);
void o_text_set_info_font(char buf[]);
void o_text_recreate(TOPLEVEL *toplevel, OBJECT *o_current);
void o_text_translate_world(TOPLEVEL *toplevel, int x1, int y1, OBJECT *o_current);
OBJECT *o_text_copy(TOPLEVEL *toplevel, OBJECT *list_tail, OBJECT *o_current);
void o_text_freeallfonts(TOPLEVEL *toplevel);
void o_text_rotate_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_text_mirror_world(TOPLEVEL *toplevel, int world_centerx, int world_centery, OBJECT *object);

/* s_attrib.c */
int s_attrib_add_entry(char *new_attrib);
void s_attrib_print(void);
int s_attrib_uniq(char *name);
void s_attrib_free(void);
void s_attrib_init(void);
char *s_attrib_get(int counter);

/* s_basic.c */
void error_if_called(void);
void exit_if_null(void *ptr);
OBJECT *return_tail(OBJECT *head);
OBJECT *return_head(OBJECT *tail);
OBJECT *s_basic_init_object(char *name);
OBJECT *s_basic_link_object(OBJECT *new_node, OBJECT *ptr);
void print_struct_forw(OBJECT *ptr);
void print_struct_back(OBJECT *ptr);
void print_struct(OBJECT *ptr);
void s_delete_object(TOPLEVEL *toplevel, OBJECT *o_current);
void s_delete(TOPLEVEL *toplevel, OBJECT *o_current);
void s_delete_list_fromstart(TOPLEVEL *toplevel, OBJECT *start);
void s_delete_object_glist(TOPLEVEL *toplevel, GList *list);
OBJECT *s_remove(TOPLEVEL *toplevel, OBJECT *object);
char *remove_nl(char *string);
char *remove_last_nl(char *string);
gchar *s_expand_env_variables (const gchar *string);

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
const CLibSymbol *s_clib_get_symbol_by_name (const gchar *name);
gchar *s_clib_symbol_get_data_by_name (const gchar *name);
GList *s_toplevel_get_symbols (const TOPLEVEL *toplevel);

/* s_color.c */
int s_color_request(int color_index, char *color_name, char *outline_color_name, char *ps_color_string, int image_red, int image_green, int image_blue);
void s_color_destroy_all(void);
int s_color_image_int(int color);
int s_color_get_name(int index, char *string);
int s_color_get_index(char *string);

/* s_conn.c */
CONN *s_conn_return_new(OBJECT *other_object, int type, int x, int y, int whichone, int other_whichone);
int s_conn_uniq(GList *conn_list, CONN *input_conn);
int s_conn_remove_other(TOPLEVEL *toplevel, OBJECT *other_object, OBJECT *to_remove);
void s_conn_remove(TOPLEVEL *toplevel, OBJECT *to_remove);
void s_conn_remove_complex(TOPLEVEL *toplevel, OBJECT *to_remove);
OBJECT *s_conn_check_midpoint(OBJECT *o_current, int x, int y);
void s_conn_update_object(TOPLEVEL *toplevel, OBJECT *object);
void s_conn_update_complex(TOPLEVEL *toplevel, OBJECT *complex);
void s_conn_print(GList *conn_list);
int s_conn_net_search(OBJECT* new_net, int whichone, GList * conn_list);
GList *s_conn_return_others(GList *input_list, OBJECT *object);
GList *s_conn_return_complex_others(GList *input_list, OBJECT *object);

/* s_cue.c */
void s_cue_postscript_fillbox(TOPLEVEL *toplevel, FILE *fp, int x, int y);
void s_cue_postscript_fillcircle(TOPLEVEL *toplevel, FILE *fp, int x, int y, int size_flag);
void s_cue_output_all(TOPLEVEL *toplevel, OBJECT *head, FILE *fp, int type);
void s_cue_output_lowlevel(TOPLEVEL *toplevel, OBJECT *object, int whichone, FILE *fp, int output_type);
void s_cue_output_lowlevel_midpoints(TOPLEVEL *toplevel, OBJECT *object, FILE *fp, int output_type);
void s_cue_output_single(TOPLEVEL *toplevel, OBJECT *object, FILE *fp, int type);

/* s_hierarchy.c */
int s_hierarchy_down_schematic_single(TOPLEVEL *toplevel, const gchar *filename, PAGE *parent, int page_control, int flag);
void s_hierarchy_down_symbol (TOPLEVEL *toplevel, const CLibSymbol *symbol, PAGE *parent);
PAGE *s_hierarchy_find_up_page(GedaPageList *page_list, PAGE *current_page);
GList* s_hierarchy_traversepages(TOPLEVEL *toplevel, gint flags);
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
void s_page_goto (TOPLEVEL *toplevel, PAGE *p_new);
PAGE *s_page_search (TOPLEVEL *toplevel, const gchar *filename);
PAGE *s_page_search_by_page_id (GedaPageList *list, int pid);
void s_page_print_all (TOPLEVEL *toplevel);
gint s_page_save_all (TOPLEVEL *toplevel);
gboolean s_page_check_changed (GedaPageList *list);
void s_page_clear_changed (GedaPageList *list);
void s_page_autosave_init(TOPLEVEL *toplevel);
gint s_page_autosave (TOPLEVEL *toplevel);

/* s_papersizes.c */
int s_papersizes_add_entry(char *new_papersize, int width, int height);
void s_papersizes_print(void);
int s_papersizes_uniq(char *name);
void s_papersizes_free(void);
void s_papersizes_init(void);
char *s_papersizes_get(int counter);
void s_papersizes_get_size(char *string, int *width, int *height);

/* s_project.c */
TOPLEVEL *s_toplevel_new (void);
void s_toplevel_delete (TOPLEVEL *toplevel);

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

/* s_stretch.c */
STRETCH *s_stretch_return_tail(STRETCH *head);
STRETCH *s_stretch_return_head(STRETCH *tail);
STRETCH *s_stretch_new_head(void);
void s_stretch_destroy_head(STRETCH *s_head);
STRETCH *s_stretch_add(STRETCH *head, OBJECT *object, CONN *connection, int whichone);
void s_stretch_remove(STRETCH *head, OBJECT *object);
void s_stretch_remove_most(TOPLEVEL *toplevel, STRETCH *head);
void s_stretch_print_all(STRETCH *head);
void s_stretch_destroy_all(STRETCH *head);

/* s_tile.c */
void s_tile_init(TOPLEVEL *toplevel, PAGE *p_current);
TILE_LOC *s_tile_new_loc(int i, int j);
void s_tile_add_object(TOPLEVEL *toplevel, OBJECT *object, int world_x1, int world_y1, int world_x2, int world_y2);
void s_tile_remove_object_all(TOPLEVEL *toplevel, PAGE *p_current, OBJECT *object);
void s_tile_update_object(TOPLEVEL *toplevel, OBJECT *object);
GList *s_tile_get_objectlists(TOPLEVEL *toplevel, int world_x1, int world_y1, int world_x2, int world_y2);
void s_tile_print(TOPLEVEL *toplevel);
void s_tile_free_all(PAGE *p_current);

/* s_undo.c */
UNDO *s_undo_return_tail(UNDO *head);
UNDO *s_undo_return_head(UNDO *tail);
UNDO *s_undo_new_head(void);
void s_undo_destroy_head(UNDO *u_head);
UNDO *s_undo_add(UNDO *head, int type, char *filename, OBJECT *object_head, int left, int top, int right, int bottom, int page_control, int up);
void s_undo_print_all(UNDO *head);
void s_undo_destroy_all(TOPLEVEL *toplevel, UNDO *head);
void s_undo_remove(TOPLEVEL *toplevel, UNDO *head, UNDO *u_tos);
void s_undo_remove_rest(TOPLEVEL *toplevel, UNDO *head);
int s_undo_levels(UNDO *head);
void s_undo_init(PAGE *p_current);
void s_undo_free_all(TOPLEVEL *toplevel, PAGE *p_current);

/* u_basic.c */
char *u_basic_breakup_string(char *string, char delimiter, int count);
