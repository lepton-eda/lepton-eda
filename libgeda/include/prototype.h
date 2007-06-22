/* a_basic.c */
void o_save_embedded(TOPLEVEL *w_current, OBJECT *object_list, FILE *fp);
void o_save_write_header(FILE *fp);
int o_save(TOPLEVEL *w_current, const char *filename);
OBJECT *o_read_buffer(TOPLEVEL *w_current, OBJECT *object_list, char *buffer, const int size, const char *name);
OBJECT *o_read(TOPLEVEL *w_current, OBJECT *object_list, char *filename);
void o_scale(TOPLEVEL *w_current, OBJECT *list, int x_scale, int y_scale);

/* f_basic.c */
int f_open(TOPLEVEL *w_current, char *filename);
void f_close(TOPLEVEL *w_current);
void f_save_close(TOPLEVEL *w_current, char *filename);
int f_save(TOPLEVEL *w_current, const char *filename);
char* f_get_directory_from_path(char *path);
char* f_normalize_filename(const gchar *filename);
char *follow_symlinks (const gchar *filename, GError **error);

/* f_image.c */
void f_image_write_objects(TOPLEVEL *w_current, OBJECT *head, int start_x, int start_y, float scale, int color_mode);
void f_image_write(TOPLEVEL *w_current, const char *filename, int width, int height, int color_mode);
void f_image_set_type(TOPLEVEL *w_current, int type);

/* f_print.c */
void f_print_set_line_width(FILE *fp, int width);
void f_print_set_color(FILE *fp, int color);
int f_print_header(TOPLEVEL *w_current, FILE *fp, int paper_size_x, int paper_size_y, int eps);
void f_print_footer(FILE *fp);
void f_print_objects(TOPLEVEL *w_current, FILE *fp, OBJECT *head, int start_x, int start_y, float scale, int unicode_count, gunichar *unicode_table);
int f_print_file (TOPLEVEL *w_current, const char *filename);
int f_print_command (TOPLEVEL *w_current, const char *command);
int f_print_stream(TOPLEVEL *w_current, FILE *fp);
void f_print_set_type(TOPLEVEL *w_current, int type);
int f_print_initialize_glyph_table(void);

/* g_basic.c */
int g_read_file(const gchar *filename);

/* g_rc.c */
gint g_rc_parse_general(TOPLEVEL *w_current,
                        const gchar *fname, 
                        const gchar *ok_msg, const gchar *err_msg);
const char *g_rc_parse_path(void);
gint g_rc_parse_system_rc(TOPLEVEL *w_current, const gchar *rcname);
gint g_rc_parse_home_rc(TOPLEVEL *w_current, const gchar *rcname);
gint g_rc_parse_local_rc(TOPLEVEL *w_current, const gchar *rcname);
void g_rc_parse(TOPLEVEL *w_current, const gchar* rcname, 
                const gchar* specified_rc_filename);
gint g_rc_parse_specified_rc(TOPLEVEL *w_current, const gchar *rcfilename);
SCM g_rc_component_library(SCM path, SCM name);
SCM g_rc_component_library_command (SCM command, SCM name);
SCM g_rc_component_library_funcs (SCM listfunc, SCM getfunc, SCM name);
SCM g_rc_component_library_search(SCM path);
SCM g_rc_source_library(SCM path);
SCM g_rc_source_library_search(SCM path);
SCM g_rc_world_size(SCM width, SCM height, SCM border);
SCM g_rc_reset_component_library(void);
SCM g_rc_reset_source_library(void);
SCM g_rc_default_series_name(SCM name);
SCM g_rc_untitled_name(SCM name);
SCM g_rc_font_directory(SCM path);
SCM g_rc_bitmap_directory(SCM path);
SCM g_rc_scheme_directory(SCM path);
SCM g_rc_bus_ripper_symname(SCM scmsymname);
SCM g_rc_postscript_prolog(SCM scmsymname);
SCM g_rc_map_font_character_to_file(SCM character_param, SCM file_param);
SCM g_rc_always_promote_attributes(SCM scmsymname);

/* g_register.c */
void g_register_libgeda_funcs(void);
void g_register_libgeda_vars (void);

/* g_smob.c */
SCM g_make_attrib_smob(TOPLEVEL *curr_w, ATTRIB *curr_attr);
SCM g_get_attrib_name_value(SCM attrib_smob);
SCM g_set_attrib_value_internal(SCM attrib_smob, SCM scm_value, TOPLEVEL **world, OBJECT **o_attrib, char *new_string[]);
SCM g_calcule_new_attrib_bounds (SCM attrib_smob, SCM scm_alignment,
				 SCM scm_angle, SCM scm_x, SCM scm_y);
void g_init_attrib_smob(void);

SCM g_get_attrib_bounds(SCM attrib_smob);
SCM g_get_attrib_angle(SCM attrib_smob);
SCM g_make_object_smob(TOPLEVEL *curr_w, OBJECT *object);
SCM g_get_object_attributes(SCM object_smob);
SCM g_get_attrib_value_by_attrib_name(SCM object_smob, SCM scm_attrib_name);
SCM g_get_object_type(SCM object_smob);
void g_init_object_smob(void);
gboolean g_get_data_from_object_smob(SCM object_smob, TOPLEVEL **toplevel, 
				     OBJECT **object);
SCM g_make_page_smob(TOPLEVEL *curr_w, PAGE *page);
void g_init_page_smob(void);
gboolean g_get_data_from_page_smob(SCM object_smob, TOPLEVEL **toplevel, 
				   PAGE **object);
SCM g_get_page_filename(SCM page_smob);

/* i_vars.c */
void i_vars_libgeda_set(TOPLEVEL *w_current);
void i_vars_setnames(TOPLEVEL *w_current);
void i_vars_freenames();

/* gdk-pixbuf-hacks.c */
GdkPixbuf *gdk_pixbuf_rotate (GdkPixbuf *pixbuf, guint angle);
void gdk_pixbuf_add (GdkPixbuf *pixbuf, int offset_x, int offset_y, GdkPixbuf *pixbuf_to_add);
GdkPixbuf *gdk_pixbuf_mirror_flip(GdkPixbuf *src, gint mirror, gint flip);

/* libgeda.c */
void libgeda_init(void);

/* m_basic.c */
int mil_x(TOPLEVEL *w_current, int val);
int mil_y(TOPLEVEL *w_current, int val);
int pix_x(TOPLEVEL *w_current, int val);
int pix_y(TOPLEVEL *w_current, int val);
void WORLDtoSCREEN(TOPLEVEL *w_current, int x, int y, int *mil_x, int *mil_y);
void SCREENtoWORLD(TOPLEVEL *w_current, int mx, int my, int *x, int *y);
int snap_grid(TOPLEVEL *w_current, int input);
int SCREENabs(TOPLEVEL *w_current, int val);
int WORLDabs(TOPLEVEL *w_current, int val);
void set_window(TOPLEVEL *w_current, PAGE *page, int xmin, int xmax, int ymin, int ymax);
int fix_x(TOPLEVEL *w_current, int in);
int fix_y(TOPLEVEL *w_current, int in);
int on_snap(int val);
int SCREENclip_change(TOPLEVEL *w_current, int *x1, int *y1, int *x2, int *y2);
int clip_nochange(TOPLEVEL *w_current, int x1, int y1, int x2, int y2);
int visible(TOPLEVEL *w_current, int wleft, int wtop, int wright, int wbottom);
void rotate_point(int x, int y, int angle, int *newx, int *newy);
void rotate_point_90(int x, int y, int angle, int *newx, int *newy);
void PAPERSIZEtoWORLD(int width, int height, int border, int *right, int *bottom);
double round_5_2_1(double unrounded);

/* o_arc_basic.c */
OBJECT *o_arc_add(TOPLEVEL *w_current, OBJECT *object_list, char type, int color, int x, int y, int radius, int start_angle, int end_angle);
OBJECT *o_arc_copy(TOPLEVEL *w_current, OBJECT *list_tail, OBJECT *o_current);
void o_arc_modify(TOPLEVEL *w_current, OBJECT *object, int x, int y, int whichone);
OBJECT *o_arc_read(TOPLEVEL *w_current, OBJECT *object_list, char buf[], unsigned int release_ver, unsigned int fileformat_ver);
char *o_arc_save(OBJECT *object);
void o_arc_translate_world(TOPLEVEL *w_current, int dx, int dy, OBJECT *object);
void o_arc_rotate_world(TOPLEVEL *w_current, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_arc_mirror_world(TOPLEVEL *w_current, int world_centerx, int world_centery, OBJECT *object);
void o_arc_recalc(TOPLEVEL *w_current, OBJECT *o_current);
void world_get_arc_bounds(TOPLEVEL *w_current, OBJECT *object, int *left, int *top, int *right, int *bottom);
void o_arc_print(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current, int origin_x, int origin_y);
void o_arc_print_solid(TOPLEVEL *w_current, FILE *fp, int x, int y, int radius, int angle1, int angle2, int color, int arc_width, int length, int space, int origin_x, int origin_y);
void o_arc_print_dotted(TOPLEVEL *w_current, FILE *fp, int x, int y, int radius, int angle1, int angle2, int color, int arc_width, int length, int space, int origin_x, int origin_y);
void o_arc_print_dashed(TOPLEVEL *w_current, FILE *fp, int x, int y, int radius, int angle1, int angle2, int color, int arc_width, int length, int space, int origin_x, int origin_y);
void o_arc_print_center(TOPLEVEL *w_current, FILE *fp, int x, int y, int radius, int angle1, int angle2, int color, int arc_width, int length, int space, int origin_x, int origin_y);
void o_arc_print_phantom(TOPLEVEL *w_current, FILE *fp, int x, int y, int radius, int angle1, int angle2, int color, int arc_width, int length, int space, int origin_x, int origin_y);
void o_arc_image_write(TOPLEVEL *w_current, OBJECT *o_current, int origin_x, int origin_y, int color_mode);

/* o_attrib.c */
void o_attrib_update_urefBM(TOPLEVEL *w_current, OBJECT *o_current);
ATTRIB *o_attrib_search(ATTRIB *list, OBJECT *item);
ATTRIB *o_attrib_return_tail(ATTRIB *head);
ATTRIB *add_attrib_head(OBJECT *parent);
ATTRIB *o_attrib_add(TOPLEVEL *w_current, ATTRIB *list_head, OBJECT *item);
void o_attrib_free(TOPLEVEL *w_current, ATTRIB *current);
void o_attrib_attach(TOPLEVEL *w_current, OBJECT *parent_list, OBJECT *text_object, OBJECT *object);
void o_attrib_detach_test(TOPLEVEL *w_current, OBJECT *list, OBJECT *items);
void o_attrib_edit(OBJECT *list, OBJECT *item);
void o_attrib_select_draw(ATTRIB *list);
void o_attrib_unselect_draw(ATTRIB *list);
void o_attrib_free_all(TOPLEVEL *w_current, ATTRIB *list);
void o_attrib_print(ATTRIB *attributes);
void o_attrib_print_reverse(ATTRIB *attributes);
ATTRIB *o_attrib_copy(ATTRIB *list);
void o_attrib_delete(ATTRIB *a_current);
void o_attrib_remove(ATTRIB *list, ATTRIB *remove);
void o_attrib_detach_all(TOPLEVEL *w_current, OBJECT *object_list, OBJECT *main_head);
OBJECT *o_read_attribs(TOPLEVEL *w_current, 
		       OBJECT *object_to_get_attribs, 
		       TextBuffer *tb,
		       unsigned int release_ver, 
		       unsigned int fileformat_ver);
void o_save_attribs(FILE *fp, ATTRIB *attribs);
int o_attrib_get_name_value(char *string, char **name, char **value);
void o_attrib_free_current(TOPLEVEL *w_current);
void o_attrib_set_show(TOPLEVEL *w_current, int flag);
void o_attrib_set_visible(TOPLEVEL *w_current, int flag);
void o_attrib_set_string(TOPLEVEL *w_current, char *string);
OBJECT *o_attrib_return_parent(ATTRIB *attribute);
ATTRIB *o_attrib_copy_all(TOPLEVEL *w_current, OBJECT *attached_to, ATTRIB *attributes);
void o_attrib_reattach(ATTRIB *attributes);
void o_attrib_set_color(TOPLEVEL *w_current, ATTRIB *attributes);
char *o_attrib_search_name(OBJECT *list, char *name, int counter);
OBJECT *o_attrib_search_string_list(OBJECT *list, char *string);
char *o_attrib_search_string_partial(OBJECT *object, char *search_for, int counter);
OBJECT *o_attrib_search_string_single(OBJECT *object, char *search_for);
OBJECT *o_attrib_search_attrib_value(ATTRIB *list, char *value, char *name, int counter);
char *o_attrib_search_attrib_name(ATTRIB *list, char *name, int counter);
char *o_attrib_search_toplevel(OBJECT *list, char *name, int counter);
char *o_attrib_search_special(OBJECT *o_current);
char *o_attrib_search_name_single(OBJECT *object, char *name, OBJECT **return_found);
char *o_attrib_search_name_single_count(OBJECT *object, char *name, int counter);
char *o_attrib_search_slot(OBJECT *object, OBJECT **return_found);
char *o_attrib_search_numslots(OBJECT *object, OBJECT **return_found);
char *o_attrib_search_default_slot(OBJECT *object);
OBJECT *o_attrib_search_pinseq(OBJECT *list, int pin_number);
char *o_attrib_search_slotdef(OBJECT *object, int slotnumber);
char *o_attrib_search_component(OBJECT *object, char *name);
void o_attrib_slot_update(TOPLEVEL *w_current, OBJECT *object);
void o_attrib_slot_copy(TOPLEVEL *w_current, OBJECT *original, OBJECT *target);
int o_attrib_count_toplevel(TOPLEVEL *w_current, char *name);
char *o_attrib_search_toplevel_all(PAGE *page_head, char *name);
OBJECT **o_attrib_return_attribs(OBJECT *object_list, OBJECT *sel_object);
void o_attrib_free_returned(OBJECT **found_objects);

/* o_basic.c */
int inside_region(int xmin, int ymin, int xmax, int ymax, int x, int y);
void o_redraw_single(TOPLEVEL *w_current, OBJECT *o_current);
void o_recalc_single_object(TOPLEVEL *w_current, OBJECT *o_current);
void o_recalc_object_list(TOPLEVEL *w_current, OBJECT *object_list);
void o_recalc_object_glist(TOPLEVEL *w_current, GList *object_glist);
void o_set_line_options(TOPLEVEL *w_current, OBJECT *o_current, OBJECT_END end, OBJECT_TYPE type, int width, int length, int space);
void o_set_fill_options(TOPLEVEL *w_current, OBJECT *o_current, OBJECT_FILLING type, int width, int pitch1, int angle1, int pitch2, int angle2);

/* o_box_basic.c */
OBJECT *o_box_add(TOPLEVEL *w_current, OBJECT *object_list, char type, int color, int x1, int y1, int x2, int y2);
OBJECT *o_box_copy(TOPLEVEL *w_current, OBJECT *list_tail, OBJECT *o_current);
void o_box_modify(TOPLEVEL *w_current, OBJECT *object, int x, int y, int whichone);
OBJECT *o_box_read(TOPLEVEL *w_current, OBJECT *object_list, char buf[], unsigned int release_ver, unsigned int fileformat_ver);
char *o_box_save(OBJECT *object);
void o_box_translate_world(TOPLEVEL *w_current, int x1, int y1, OBJECT *object);
void o_box_rotate_world(TOPLEVEL *w_current, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_box_mirror_world(TOPLEVEL *w_current, int world_centerx, int world_centery, OBJECT *object);
void o_box_recalc(TOPLEVEL *w_current, OBJECT *o_current);
void world_get_box_bounds(TOPLEVEL *w_current, OBJECT *object, int *left, int *top, int *right, int *bottom);
void o_box_print(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current, int origin_x, int origin_y);
void o_box_print_solid(TOPLEVEL *w_current, FILE *fp, int x, int y, int width, int height, int color, int line_width, int length, int space, int origin_x, int origin_y);
void o_box_print_dotted(TOPLEVEL *w_current, FILE *fp, int x, int y, int width, int height, int color, int line_width, int length, int space, int origin_x, int origin_y);
void o_box_print_dashed(TOPLEVEL *w_current, FILE *fp, int x, int y, int width, int height, int color, int line_width, int length, int space, int origin_x, int origin_y);
void o_box_print_center(TOPLEVEL *w_current, FILE *fp, int x, int y, int width, int height, int color, int line_width, int length, int space, int origin_x, int origin_y);
void o_box_print_phantom(TOPLEVEL *w_current, FILE *fp, int x, int y, int width, int height, int color, int line_width, int length, int space, int origin_x, int origin_y);
void o_box_print_filled(TOPLEVEL *w_current, FILE *fp, int x, int y, int width, int height, int color, int fill_width, int angle1, int pitch1, int angle2, int pitch2, int origin_x, int origin_y);
void o_box_print_mesh(TOPLEVEL *w_current, FILE *fp, int x, int y, int width, int height, int color, int fill_width, int angle1, int pitch1, int angle2, int pitch2, int origin_x, int origin_y);
void o_box_print_hatch(TOPLEVEL *w_current, FILE *fp, int x, int y, int width, int height, int color, int fill_width, int angle1, int pitch1, int angle2, int pitch2, int origin_x, int origin_y);
void o_box_image_write(TOPLEVEL *w_current, OBJECT *o_current, int origin_x, int origin_y, int color_mode);

/* o_bus_basic.c */
void world_get_bus_bounds(TOPLEVEL *w_current, OBJECT *object, int *left, int *top, int *right, int *bottom);
OBJECT *o_bus_add(TOPLEVEL *w_current, OBJECT *object_list, char type, int color, int x1, int y1, int x2, int y2, int bus_ripper_direction);
void o_bus_recalc(TOPLEVEL *w_current, OBJECT *o_current);
OBJECT *o_bus_read(TOPLEVEL *w_current, OBJECT *object_list, char buf[], unsigned int release_ver, unsigned int fileformat_ver);
char *o_bus_save(OBJECT *object);
void o_bus_translate_world(TOPLEVEL *w_current, int x1, int y1, OBJECT *object);
OBJECT *o_bus_copy(TOPLEVEL *w_current, OBJECT *list_tail, OBJECT *o_current);
void o_bus_print(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current, int origin_x, int origin_y);
void o_bus_image_write(TOPLEVEL *w_current, OBJECT *o_current, int origin_x, int origin_y, int color_mode);
void o_bus_rotate_world(TOPLEVEL *w_current, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_bus_mirror_world(TOPLEVEL *w_current, int world_centerx, int world_centery, OBJECT *object);
int o_bus_orientation(OBJECT *object);
void o_bus_consolidate_lowlevel(OBJECT *object, OBJECT *del_object, int orient);
int o_bus_consolidate_segments(TOPLEVEL *w_current, OBJECT *object);
void o_bus_consolidate(TOPLEVEL *w_current);
void o_bus_modify(TOPLEVEL *w_current, OBJECT *object, int x, int y, int whichone);

/* o_circle_basic.c */
int dist(int x1, int y1, int x2, int y2);
OBJECT *o_circle_add(TOPLEVEL *w_current, OBJECT *object_list, char type, int color, int x, int y, int radius);
OBJECT *o_circle_copy(TOPLEVEL *w_current, OBJECT *list_tail, OBJECT *o_current);
void o_circle_modify(TOPLEVEL *w_current, OBJECT *object, int x, int y, int whichone);
OBJECT *o_circle_read(TOPLEVEL *w_current, OBJECT *object_list, char buf[], unsigned int release_ver, unsigned int fileformat_ver);
char *o_circle_save(OBJECT *object);
void o_circle_translate_world(TOPLEVEL *w_current, int x1, int y1, OBJECT *object);
void o_circle_rotate_world(TOPLEVEL *w_current, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_circle_mirror_world(TOPLEVEL *w_current, int world_centerx, int world_centery, OBJECT *object);
void o_circle_recalc(TOPLEVEL *w_current, OBJECT *o_current);
void world_get_circle_bounds(TOPLEVEL *w_current, OBJECT *object, int *left, int *top, int *right, int *bottom);
void o_circle_print(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current, int origin_x, int origin_y);
void o_circle_print_solid(TOPLEVEL *w_current, FILE *fp, int x, int y, int radius, int color, int circle_width, int length, int space, int origin_x, int origin_y);
void o_circle_print_dotted(TOPLEVEL *w_current, FILE *fp, int x, int y, int radius, int color, int circle_width, int length, int space, int origin_x, int origin_y);
void o_circle_print_dashed(TOPLEVEL *w_current, FILE *fp, int x, int y, int radius, int color, int circle_width, int length, int space, int origin_x, int origin_y);
void o_circle_print_center(TOPLEVEL *w_current, FILE *fp, int x, int y, int radius, int color, int circle_width, int length, int space, int origin_x, int origin_y);
void o_circle_print_phantom(TOPLEVEL *w_current, FILE *fp, int x, int y, int radius, int color, int circle_width, int length, int space, int origin_x, int origin_y);
void o_circle_print_filled(TOPLEVEL *w_current, FILE *fp, int x, int y, int radius, int color, int fill_width, int angle1, int pitch1, int angle2, int pitch2, int origin_x, int origin_y);
void o_circle_print_mesh(TOPLEVEL *w_current, FILE *fp, int x, int y, int radius, int color, int fill_width, int angle1, int pitch1, int angle2, int pitch2, int origin_x, int origin_y);
void o_circle_print_hatch(TOPLEVEL *w_current, FILE *fp, int x, int y, int radius, int color, int fill_width, int angle1, int pitch1, int angle2, int pitch2, int origin_x, int origin_y);
void o_circle_image_write(TOPLEVEL *w_current, OBJECT *o_current, int origin_x, int origin_y, int color_mode);

/* o_complex_basic.c */
int world_get_single_object_bounds(TOPLEVEL *w_current, OBJECT *o_current, 
			      int *rleft, int *rtop, 
			      int *rright, int *rbottom);
int world_get_object_list_bounds(TOPLEVEL *w_current, OBJECT *complex, 
			    int *left, int *top, int *right, int *bottom);
int world_get_object_glist_bounds(TOPLEVEL *w_current, GList *o_list, 
			     int *left, int *top, 
			     int *right, int *bottom);
void world_get_complex_bounds(TOPLEVEL *w_current, OBJECT *complex, int *left, int *top, int *right, int *bottom);
OBJECT *add_head(void);
int o_complex_is_eligible_attribute(TOPLEVEL *w_current, OBJECT *object, int promote_invisible);
int o_complex_is_embedded(OBJECT *o_current);
OBJECT *o_complex_add(TOPLEVEL *w_current, OBJECT *object_list, 
		      GList **object_glist, char type, int color, 
		      int x, int y, int angle, int mirror, 
		      const CLibSymbol *clib_sym, const gchar *basename,
		      int selectable, int attribute_promotion);
OBJECT *o_complex_add_embedded(TOPLEVEL *w_current, OBJECT *object_list, char type, int color, int x, int y, int angle, const gchar *basename, int selectable);
void o_complex_recalc(TOPLEVEL *w_current, OBJECT *o_current);
OBJECT *o_complex_read(TOPLEVEL *w_current, OBJECT *object_list, char buf[], unsigned int release_ver, unsigned int fileformat_ver);
char *o_complex_save(OBJECT *object);
void o_complex_set_filename(TOPLEVEL *w_current, const CLibSymbol *clib, char *basename);
void o_complex_free_filename(TOPLEVEL *w_current);
void o_complex_world_translate(TOPLEVEL *w_current, int x1, int y1, OBJECT *prim_objs);
void o_complex_world_translate_toplevel(TOPLEVEL *w_current, int x1, int y1, OBJECT *object);
OBJECT *o_complex_copy(TOPLEVEL *w_current, OBJECT *list_tail, OBJECT *o_current);
OBJECT *o_complex_copy_embedded(TOPLEVEL *w_current, OBJECT *list_tail, OBJECT *o_current);
void o_complex_delete(TOPLEVEL *w_current, OBJECT *delete);
void o_complex_set_color(OBJECT *prim_objs, int color);
void o_complex_set_color_single(OBJECT *o_current, int color);
void o_complex_set_color_save(OBJECT *complex, int color);
void o_complex_unset_color(OBJECT *complex);
void o_complex_unset_color_single(OBJECT *o_current);
void o_complex_set_saved_color_only(OBJECT *complex, int color);
OBJECT *o_complex_return_nth_pin(OBJECT *o_list, int counter);
void o_complex_rotate_lowlevel(TOPLEVEL *w_current, int world_centerx, int world_centery, int angle, int angle_change, OBJECT *object);
void o_complex_mirror_lowlevel(TOPLEVEL *w_current, int world_centerx, int world_centery, OBJECT *object);
OBJECT *o_complex_return_pin_object(OBJECT *object, char *pin);
int  o_complex_count_pins(OBJECT *object);
void o_complex_check_symversion(TOPLEVEL* w_current, OBJECT* object);

/* o_embed.c */
void o_embed(TOPLEVEL *w_current, OBJECT *o_current);
void o_unembed(TOPLEVEL *w_current, OBJECT *o_current);

/* o_image.c */
void o_image_init(void);
void o_image_create(int x, int y, int color_mode);
void o_image_close(void);
int o_image_write(const char *filename);
int o_image_geda2gd_color(int color);

/* o_line_basic.c */
OBJECT *o_line_add(TOPLEVEL *w_current, OBJECT *object_list, char type, int color, int x1, int y1, int x2, int y2);
OBJECT *o_line_copy(TOPLEVEL *w_current, OBJECT *list_tail, OBJECT *o_current);
void o_line_modify(TOPLEVEL *w_current, OBJECT *object, int x, int y, int whichone);
OBJECT *o_line_read(TOPLEVEL *w_current, OBJECT *object_list, char buf[], unsigned int release_ver, unsigned int fileformat_ver);
char *o_line_save(OBJECT *object);
void o_line_translate_world(TOPLEVEL *w_current, int x1, int y1, OBJECT *object);
void o_line_rotate_world(TOPLEVEL *w_current, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_line_mirror_world(TOPLEVEL *w_current, int world_centerx, int world_centery, OBJECT *object);
void o_line_recalc(TOPLEVEL *w_current, OBJECT *o_current);
void world_get_line_bounds(TOPLEVEL *w_current, OBJECT *object, int *left, int *top, int *right, int *bottom);
void o_line_print(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current, int origin_x, int origin_y);
void o_line_print_solid(TOPLEVEL *w_current, FILE *fp, int x1, int y1, int x2, int y2, int color, int line_width, int length, int space, int origin_x, int origin_y);
void o_line_print_dotted(TOPLEVEL *w_current, FILE *fp, int x1, int y1, int x2, int y2, int color, int line_width, int length, int space, int origin_x, int origin_y);
void o_line_print_dashed(TOPLEVEL *w_current, FILE *fp, int x1, int y1, int x2, int y2, int color, int line_width, int length, int space, int origin_x, int origin_y);
void o_line_print_center(TOPLEVEL *w_current, FILE *fp, int x1, int y1, int x2, int y2, int color, int line_width, int length, int space, int origin_x, int origin_y);
void o_line_print_phantom(TOPLEVEL *w_current, FILE *fp, int x1, int y1, int x2, int y2, int color, int line_width, int length, int space, int origin_x, int origin_y);
void o_line_image_write(TOPLEVEL *w_current, OBJECT *o_current, int origin_x, int origin_y, int color_mode);
void o_line_scale_world(TOPLEVEL *w_current, int x_scale, int y_scale, OBJECT *object);
int o_line_visible(TOPLEVEL *w_current, LINE *line, int *x1, int *y1, int *x2, int *y2);
double o_line_length(OBJECT *object);

/* o_list.c */
OBJECT *o_list_copy_to(TOPLEVEL *w_current, OBJECT *list_head, OBJECT *selected, int flag, OBJECT **return_end);
OBJECT *o_list_copy_all(TOPLEVEL *w_current, OBJECT *src_list_head, OBJECT *dest_list_head, int flag);
OBJECT *o_list_copy_all_selection2(TOPLEVEL *w_current, GList *src_list_head, OBJECT *dest_list_head, int flag);
OBJECT *o_list_search(OBJECT *list, OBJECT *current);
void o_list_delete(TOPLEVEL *w_current, OBJECT *list, OBJECT *delete);
void o_list_delete_rest(TOPLEVEL *w_current, OBJECT *list);

/* o_net_basic.c */
void world_get_net_bounds(TOPLEVEL *w_current, OBJECT *object, int *left, int *top, int *right, int *bottom);
OBJECT *o_net_add(TOPLEVEL *w_current, OBJECT *object_list, char type, int color, int x1, int y1, int x2, int y2);
void o_net_recalc(TOPLEVEL *w_current, OBJECT *o_current);
OBJECT *o_net_read(TOPLEVEL *w_current, OBJECT *object_list, char buf[], unsigned int release_ver, unsigned int fileformat_ver);
char *o_net_save(OBJECT *object);
void o_net_translate_world(TOPLEVEL *w_current, int x1, int y1, OBJECT *object);
OBJECT *o_net_copy(TOPLEVEL *w_current, OBJECT *list_tail, OBJECT *o_current);
void o_net_print(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current, int origin_x, int origin_y);
void o_net_image_write(TOPLEVEL *w_current, OBJECT *o_current, int origin_x, int origin_y, int color_mode);
void o_net_rotate_world(TOPLEVEL *w_current, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_net_mirror_world(TOPLEVEL *w_current, int world_centerx, int world_centery, OBJECT *object);
int o_net_orientation(OBJECT *object);
void o_net_consolidate_lowlevel(OBJECT *object, OBJECT *del_object, int orient);
int o_net_consolidate_nomidpoint(OBJECT *object, int x, int y);
int o_net_consolidate_segments(TOPLEVEL *w_current, OBJECT *object);
void o_net_consolidate(TOPLEVEL *w_current);
void o_net_modify(TOPLEVEL *w_current, OBJECT *object, int x, int y, int whichone);

/* o_picture.c */
OBJECT *o_picture_read(TOPLEVEL *w_current, OBJECT *object_list,
		       const char *first_line,
		       TextBuffer *tb,
		       unsigned int release_ver,
		       unsigned int fileformat_ver);
char *o_picture_save(OBJECT *object);
void o_picture_set_pixbuf(TOPLEVEL *w_current, GdkPixbuf *pixbuf, char *filename);
OBJECT *o_picture_add(TOPLEVEL *w_current, OBJECT *object_list,
		      GdkPixbuf * pixbuf, char *filename, double ratio,
		      char type, 
		      int x1, int y1, int x2, int y2, int angle, char mirrored,
		      char embedded);
void o_picture_recalc(TOPLEVEL *w_current, OBJECT *o_current);
void world_get_picture_bounds(TOPLEVEL *w_current, OBJECT *object, int *left, int *top, int *right, int *bottom);
void o_picture_modify(TOPLEVEL *w_current, OBJECT *object, int x, int y, int whichone);
void o_picture_rotate_world(TOPLEVEL *w_current, int world_centerx, int world_centery, int angle,OBJECT *object);
void o_picture_mirror_world(TOPLEVEL *w_current, int world_centerx, int world_centery, OBJECT *object);
void o_picture_translate_world(TOPLEVEL *w_current, int x1, int y1, OBJECT *object);
OBJECT *o_picture_copy(TOPLEVEL *w_current, OBJECT *list_tail, OBJECT *o_current);
guint8 *o_picture_rgb_data(GdkPixbuf *image);
guint8 *o_picture_mask_data(GdkPixbuf *image);
void o_picture_print(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current, 
		     int origin_x, int origin_y);

/* o_pin_basic.c */
void world_get_pin_bounds(TOPLEVEL *w_current, OBJECT *object, int *left, int *top, int *right, int *bottom);
OBJECT *o_pin_add(TOPLEVEL *w_current, OBJECT *object_list, char type, int color, int x1, int y1, int x2, int y2, int pin_type, int whichend);
void o_pin_recalc(TOPLEVEL *w_current, OBJECT *o_current);
OBJECT *o_pin_read(TOPLEVEL *w_current, OBJECT *object_list, char buf[], unsigned int release_ver, unsigned int fileformat_ver);
char *o_pin_save(OBJECT *object);
void o_pin_translate_world(TOPLEVEL *w_current, int x1, int y1, OBJECT *object);
OBJECT *o_pin_copy(TOPLEVEL *w_current, OBJECT *list_tail, OBJECT *o_current);
void o_pin_print(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current, int origin_x, int origin_y);
void o_pin_image_write(TOPLEVEL *w_current, OBJECT *o_current, int origin_x, int origin_y, int color_mode);
void o_pin_rotate_world(TOPLEVEL *w_current, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_pin_mirror_world(TOPLEVEL *w_current, int world_centerx, int world_centery, OBJECT *object);
void o_pin_modify(TOPLEVEL *w_current, OBJECT *object, int x, int y, int whichone);
void o_pin_update_whichend(TOPLEVEL *w_current, OBJECT *object_list, int num_pins);

/* o_selection.c */
void o_selection_add(GList **head, OBJECT *o_selected);
void o_selection_print_all(const GList **head);
void o_selection_select(OBJECT *object, int color); /* DEPRECATED */
void o_selection_unselect(OBJECT *object);          /* DEPRECATED */
void o_selection_remove(GList **head, OBJECT *o_selected);
void o_selection_unselect_list(TOPLEVEL *w_current, GList **head);

/* o_text_basic.c */
int world_get_text_bounds(TOPLEVEL *w_current, OBJECT *o_current, int *left, int *top, int *right, int *bottom);
OBJECT *o_text_add_head(void);
void o_text_init(void);
void o_text_print_set(void);
OBJECT *o_text_load_font(TOPLEVEL *w_current, gunichar needed_char);
int o_text_num_lines(char *string);
int o_text_height(char *string, int size);
int o_text_width(TOPLEVEL *w_current, char *string, int size);
OBJECT *o_text_create_string(TOPLEVEL *w_current, OBJECT *object_list, char *string, int size, int color, int x, int y, int alignment, int angle);
OBJECT *o_text_add(TOPLEVEL *w_current, OBJECT *object_list, char type, int color, int x, int y, int alignment, int angle, char *string, int size, int visibility, int show_name_value);
void o_text_recalc(TOPLEVEL *w_current, OBJECT *o_current);
OBJECT *o_text_read(TOPLEVEL *w_current, 
		    OBJECT *object_list, 
		    const char *first_line,
		    TextBuffer *tb,
		    unsigned int release_ver,
		    unsigned int fileformat_ver);
void o_text_set_info_font(char buf[]);
char *o_text_save(OBJECT *object);
void o_text_recreate(TOPLEVEL *w_current, OBJECT *o_current);
void o_text_translate_world(TOPLEVEL *w_current, int x1, int y1, OBJECT *o_current);
OBJECT *o_text_copy(TOPLEVEL *w_current, OBJECT *list_tail, OBJECT *o_current);
void o_text_freeallfonts(TOPLEVEL *w_current);
void o_text_print_text_width(FILE *fp, char *output_string);
void o_text_print_text_height(FILE *fp, int size);
void o_text_print_text_height_full(FILE *fp, char *string, int size);
void o_text_print_text_string(FILE *fp, char *string, int unicode_count, gunichar *unicode_table);
void o_text_print(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current, int origin_x, int origin_y, int unicode_count, gunichar *unicode_table);
void o_text_rotate_lowlevel(TOPLEVEL *w_current, int world_centerx, int world_centery, int angle, OBJECT *object);
void o_text_rotate_world(TOPLEVEL *w_current, int world_centerx, int world_centery, int angle, int angle_change, OBJECT *object);
void o_text_mirror_world(TOPLEVEL *w_current, int world_centerx, int world_centery, OBJECT *object);

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
void s_delete_object(TOPLEVEL *w_current, OBJECT *o_current);
void s_delete(TOPLEVEL *w_current, OBJECT *o_current);
void s_delete_list_fromstart(TOPLEVEL *w_current, OBJECT *start);
void s_delete_object_glist(TOPLEVEL *w_current, GList *list);
OBJECT *s_remove(TOPLEVEL *w_current, OBJECT *object);
void string_toupper(char *in, char *out);
void string_tolower(char *in, char *out);
int colornametovalue(char *string);
char *remove_nl(char *string);
char *remove_last_nl(char *string);
char *remove_string(char *string, int start, int end);
char *insert_string(char *string, int start, char *insert_string);
char *expand_env_variables(char *string);

/* s_clib.c */
void s_clib_init (void);
void s_clib_free (void);
GList *s_clib_get_sources (const gboolean sorted);
const CLibSource *s_clib_get_source_by_name (const gchar *name);
void s_clib_refresh ();
const CLibSource *s_clib_add_directory (const gchar *directory, 
					const gchar *name);
const CLibSource *s_clib_add_command (const gchar *command,
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
void s_clib_flush_cache ();
const CLibSymbol *s_clib_get_symbol_by_name (const gchar *name);
gchar *s_clib_symbol_get_data_by_name (const gchar *name);

/* s_color.c */
void s_color_init(void);
int s_color_request(int color_index, char *color_name, char *outline_color_name, char *ps_color_string, int image_red, int image_green, int image_blue);
void s_color_destroy_all(void);
char *s_color_ps_string(int color);
int s_color_image_int(int color);
void s_color_gdcolor_init(void);
int s_color_get_name(int index, char *string);
int s_color_get_index(char *string);

/* s_conn.c */
CONN *s_conn_return_new(OBJECT *other_object, int type, int x, int y, int whichone, int other_whichone);
int s_conn_uniq(GList *conn_list, CONN *input_conn);
int s_conn_remove_other(TOPLEVEL *w_current, OBJECT *other_object, OBJECT *to_remove);
void s_conn_remove(TOPLEVEL *w_current, OBJECT *to_remove);
void s_conn_remove_complex(TOPLEVEL *w_current, OBJECT *to_remove);
OBJECT *s_conn_check_midpoint(OBJECT *o_current, int x, int y);
void s_conn_update_object(TOPLEVEL *w_current, OBJECT *object);
void s_conn_update_complex(TOPLEVEL *w_current, OBJECT *complex);
void s_conn_print(GList *conn_list);
int s_conn_net_search(OBJECT* new_net, int whichone, GList * conn_list);
GList *s_conn_return_others(GList *input_list, OBJECT *object);
GList *s_conn_return_complex_others(GList *input_list, OBJECT *object);

/* s_cue.c */
void s_cue_postscript_fillbox(TOPLEVEL *w_current, FILE *fp, int x, int y);
void s_cue_postscript_fillcircle(TOPLEVEL *w_current, FILE *fp, int x, int y, int size_flag);
void s_cue_image_fillbox(TOPLEVEL *w_current, OBJECT *object, int world_x, int world_y);
void s_cue_image_fillcircle(TOPLEVEL *w_current, int world_x, int world_y, int size_flag);
void s_cue_output_all(TOPLEVEL *w_current, OBJECT *head, FILE *fp, int type);
void s_cue_output_lowlevel(TOPLEVEL *w_current, OBJECT *object, int whichone, FILE *fp, int output_type);
void s_cue_output_lowlevel_midpoints(TOPLEVEL *w_current, OBJECT *object, FILE *fp, int output_type);
void s_cue_output_single(TOPLEVEL *w_current, OBJECT *object, FILE *fp, int type);

/* s_encoding.c */
gchar* s_encoding_base64_encode (gchar* src, guint srclen, guint* dstlenp, gboolean strict);
gchar* s_encoding_base64_decode (gchar* src, guint srclen, guint* dstlenp);

/* s_hierarchy.c */
int s_hierarchy_down_schematic_single(TOPLEVEL *w_current, const gchar *filename, PAGE *parent, int page_control, int flag);
void s_hierarchy_down_schematic_multiple (TOPLEVEL *w_current, const gchar *filename, PAGE *parent);
void s_hierarchy_down_symbol (TOPLEVEL *w_current, const CLibSymbol *symbol, PAGE *parent);
void s_hierarchy_up(TOPLEVEL *w_current, int pid);
GList* s_hierarchy_traversepages(TOPLEVEL *w_current, gint flags);
gint s_hierarchy_print_page(PAGE *p_current, void * data);
PAGE *s_hierarchy_find_prev_page(PAGE *p_start, int page_control);
PAGE *s_hierarchy_find_next_page(PAGE *p_start, int page_control);
PAGE *s_hierarchy_find_page(PAGE *p_start, int pid);

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
void s_page_init_list (TOPLEVEL *toplevel);
void s_page_delete_list(TOPLEVEL *toplevel);
void s_page_goto (TOPLEVEL *toplevel, PAGE *p_new);
PAGE *s_page_search (TOPLEVEL *toplevel, const gchar *filename);
PAGE* s_page_search_pid(TOPLEVEL * toplevel, gint page_id);
void s_page_print_all (TOPLEVEL *toplevel);
gint s_page_save_all (TOPLEVEL *toplevel);
gboolean s_page_check_changed (PAGE *head);
void s_page_clear_changed (PAGE *head);
void s_page_autosave_init(TOPLEVEL *toplevel);
gint s_page_autosave (TOPLEVEL *w_current);

/* s_papersizes.c */
int s_papersizes_add_entry(char *new_papersize, int width, int height);
void s_papersizes_print(void);
int s_papersizes_uniq(char *name);
void s_papersizes_free(void);
void s_papersizes_init(void);
char *s_papersizes_get(int counter);
void s_papersizes_get_size(char *string, int *width, int *height);

/* s_project.c */
void s_toplevel_init (void);
TOPLEVEL *s_toplevel_new (void);
void s_toplevel_delete (TOPLEVEL *toplevel);

/* s_scratch.c */
void s_scratch_string_init(void);
void s_scratch_string_free(void);
int s_scratch_string_fill(char *string);
int s_scratch_non_unique_string_fill(char *string);

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
void s_stretch_remove_most(TOPLEVEL *w_current, STRETCH *head);
void s_stretch_print_all(STRETCH *head);
void s_stretch_destroy_all(STRETCH *head);

/* s_tile.c */
void s_tile_init(TOPLEVEL *w_current, PAGE *p_current);
TILE_LOC *s_tile_new_loc(int i, int j);
void s_tile_add_object(TOPLEVEL *w_current, OBJECT *object, int world_x1, int world_y1, int world_x2, int world_y2);
void s_tile_remove_object_all_crude(TOPLEVEL *w_current, OBJECT *object);
void s_tile_remove_object_all(TOPLEVEL *w_current, PAGE *p_current, OBJECT *object);
void s_tile_update_object(TOPLEVEL *w_current, OBJECT *object);
void s_tile_print(TOPLEVEL *w_current);
void s_tile_free_all(PAGE *p_current);

/* s_undo.c */
UNDO *s_undo_return_tail(UNDO *head);
UNDO *s_undo_return_head(UNDO *tail);
UNDO *s_undo_new_head(void);
void s_undo_destroy_head(UNDO *u_head);
UNDO *s_undo_add(UNDO *head, int type, char *filename, OBJECT *object_head, int left, int top, int right, int bottom, int page_control, int up);
void s_undo_print_all(UNDO *head);
void s_undo_destroy_all(TOPLEVEL *w_current, UNDO *head);
void s_undo_remove(TOPLEVEL *w_current, UNDO *head, UNDO *u_tos);
void s_undo_remove_rest(TOPLEVEL *w_current, UNDO *head);
int s_undo_levels(UNDO *head);
void s_undo_init(PAGE *p_current);
void s_undo_free_all(TOPLEVEL *w_current, PAGE *p_current);

/* u_basic.c */
char *u_basic_breakup_string(char *string, char delimiter, int count);
void u_basic_strip_trailing(char *string, char c);
int u_basic_has_trailing(char *string, char c);
int u_basic_count_char(const char *string, char character);

/* s_textbuffer.c */
TextBuffer *s_textbuffer_new (gchar *data, const gint size);
TextBuffer *s_textbuffer_free (TextBuffer *tb);
void s_textbuffer_seek (TextBuffer *tb, const gint offset);
gchar *s_textbuffer_next (TextBuffer *tb, const gsize count);
gchar *s_textbuffer_next_line (TextBuffer *tb);
