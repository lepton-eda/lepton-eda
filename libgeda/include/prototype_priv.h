/* a_basic.c */
gchar *o_save_objects(const GList *object_list, gboolean save_attribs);

/* f_print.c */
void f_print_set_line_width(FILE *fp, int width);
void f_print_set_color(TOPLEVEL *toplevel, FILE *fp, int color);
int f_print_header(TOPLEVEL *toplevel, PAGE *page, FILE *fp, int paper_size_x, int paper_size_y, int eps);
void f_print_footer(FILE *fp);
void f_print_objects(TOPLEVEL *toplevel, FILE *fp, const GList *obj_list, int start_x, int start_y, float scale, int unicode_count, gunichar *unicode_table);
int f_print_initialize_glyph_table(void);

/* g_rc.c */
int vstbl_lookup_str(const vstbl_entry *table, int size, const char *str);
int vstbl_get_val(const vstbl_entry *table, int index);
SCM g_rc_component_library(SCM path, SCM name);
SCM g_rc_component_library_command (SCM listcmd, SCM getcmd, SCM name);
SCM g_rc_component_library_funcs (SCM listfunc, SCM getfunc, SCM name);
SCM g_rc_component_library_search(SCM path);
SCM g_rc_source_library(SCM path);
SCM g_rc_source_library_search(SCM path);
SCM g_rc_world_size(SCM width, SCM height, SCM border);
SCM g_rc_reset_component_library(void);
SCM g_rc_reset_source_library(void);
SCM g_rc_untitled_name(SCM name);
SCM g_rc_bitmap_directory(SCM path);
SCM g_rc_scheme_directory(SCM path);
SCM g_rc_bus_ripper_symname(SCM scmsymname);
SCM g_rc_postscript_prolog(SCM scmsymname);
SCM g_rc_map_font_character_to_file(SCM character_param, SCM file_param);
SCM g_rc_attribute_promotion(SCM mode);
SCM g_rc_promote_invisible(SCM mode);
SCM g_rc_keep_invisible(SCM mode);
SCM g_rc_always_promote_attributes(SCM scmsymname);
SCM g_rc_print_color_map (SCM scm_map);

/* g_register.c */
void g_register_libgeda_funcs(void);
void g_register_libgeda_vars (void);

/* g_smob.c */
void g_init_attrib_smob(void);
SCM g_get_attrib_name_value(SCM attrib_smob);
SCM g_calcule_new_attrib_bounds (SCM attrib_smob, SCM scm_alignment,
				 SCM scm_angle, SCM scm_x, SCM scm_y);
SCM g_get_attrib_bounds(SCM attrib_smob);
SCM g_get_attrib_angle(SCM attrib_smob);
SCM g_get_attrib_value_by_attrib_name(SCM object_smob, SCM scm_attrib_name);
void g_init_object_smob(void);
SCM g_get_object_type(SCM object_smob);
SCM g_get_line_width(SCM object_smob);
void g_init_page_smob(void);
SCM g_get_page_filename(SCM page_smob);
SCM g_set_page_filename(SCM page_smob, SCM scm_filename);

/* m_bounds.c */
void m_bounds_init(BOUNDS *bounds);
void m_bounds_of_points(BOUNDS *bounds, sPOINT points[], gint count);

/* m_box.c */
double m_box_shortest_distance (BOX *box, int x, int y, int solid);

/* m_circle.c */
double m_circle_shortest_distance (CIRCLE *circle, int x, int y, int solid);

/* m_hatch.c */
void m_hatch_polygon(GArray *points, gint angle, gint pitch, GArray *lines);

/* m_line.c */
double m_line_shortest_distance (LINE *circle, int x, int y);

/* m_polygon.c */
gboolean m_polygon_interior_point(GArray *points, int x, int y);
double m_polygon_shortest_distance(GArray *points, int x, int y, gboolean closed);

/* m_transform.c */
void m_transform_combine(TRANSFORM *result, TRANSFORM *a, TRANSFORM *b );
void m_transform_init(TRANSFORM *transform);
void m_transform_invert(TRANSFORM *transform, TRANSFORM *inverse);
void m_transform_line(TRANSFORM *transform, LINE *line );
void m_transform_lines(TRANSFORM *transform, GArray *lines);
void m_transform_point(TRANSFORM *transform, gint *x, gint *y);
void m_transform_points(TRANSFORM *transform, GArray *points);
void m_transform_rotate(TRANSFORM *transform, gdouble angle);
void m_transform_scale(TRANSFORM *transform, gdouble factor);
void m_transform_translate(TRANSFORM *transform, gdouble dx, gdouble dy);

/* o_arc_basic.c */
OBJECT *o_arc_read(TOPLEVEL *toplevel, char buf[], unsigned int release_ver, unsigned int fileformat_ver);
char *o_arc_save(OBJECT *object);
void o_arc_print(TOPLEVEL *toplevel, FILE *fp, OBJECT *o_current, int origin_x, int origin_y);
void o_arc_print_solid(TOPLEVEL *toplevel, FILE *fp, int x, int y, int radius, int angle1, int angle2, int color, int arc_width, int length, int space, int origin_x, int origin_y);
void o_arc_print_dotted(TOPLEVEL *toplevel, FILE *fp, int x, int y, int radius, int angle1, int angle2, int color, int arc_width, int length, int space, int origin_x, int origin_y);
void o_arc_print_dashed(TOPLEVEL *toplevel, FILE *fp, int x, int y, int radius, int angle1, int angle2, int color, int arc_width, int length, int space, int origin_x, int origin_y);
void o_arc_print_center(TOPLEVEL *toplevel, FILE *fp, int x, int y, int radius, int angle1, int angle2, int color, int arc_width, int length, int space, int origin_x, int origin_y);
void o_arc_print_phantom(TOPLEVEL *toplevel, FILE *fp, int x, int y, int radius, int angle1, int angle2, int color, int arc_width, int length, int space, int origin_x, int origin_y);
double o_arc_shortest_distance(OBJECT *object, int x, int y, int force_soild);
gboolean o_arc_within_sweep(ARC *arc, gint x, gint y);
void world_get_arc_bounds(TOPLEVEL *toplevel, OBJECT *object, int *left, int *top, int *right, int *bottom);
gboolean o_arc_get_position(TOPLEVEL *toplevel, gint *x, gint *y, OBJECT *object);
void o_arc_recalc(TOPLEVEL *toplevel, OBJECT *o_current);

/* o_attrib.c */
GList *o_read_attribs(TOPLEVEL *toplevel,
                      GList *list,
                      OBJECT *object_to_get_attribs,
                      TextBuffer *tb,
                      unsigned int release_ver,
                      unsigned int fileformat_ver);
OBJECT *o_attrib_find_attrib_by_name(const GList *list, char *name, int count);

/* o_basic.c */
void o_bounds_invalidate(TOPLEVEL *toplevel, OBJECT *object);
double o_shortest_distance_full(OBJECT *object, int x, int y, int force_solid);
PAGE *o_get_page_compat (TOPLEVEL *toplevel, OBJECT *object);
void o_emit_pre_change_notify(TOPLEVEL *toplevel, OBJECT *object);
void o_emit_change_notify(TOPLEVEL *toplevel, OBJECT *object);

/* o_box_basic.c */
OBJECT *o_box_read(TOPLEVEL *toplevel, char buf[], unsigned int release_ver, unsigned int fileformat_ver);
char *o_box_save(OBJECT *object);
void o_box_print(TOPLEVEL *toplevel, FILE *fp, OBJECT *o_current, int origin_x, int origin_y);
void o_box_print_solid(TOPLEVEL *toplevel, FILE *fp, int x, int y, int width, int height, int color, int line_width, int length, int space, int origin_x, int origin_y);
void o_box_print_dotted(TOPLEVEL *toplevel, FILE *fp, int x, int y, int width, int height, int color, int line_width, int length, int space, int origin_x, int origin_y);
void o_box_print_dashed(TOPLEVEL *toplevel, FILE *fp, int x, int y, int width, int height, int color, int line_width, int length, int space, int origin_x, int origin_y);
void o_box_print_center(TOPLEVEL *toplevel, FILE *fp, int x, int y, int width, int height, int color, int line_width, int length, int space, int origin_x, int origin_y);
void o_box_print_phantom(TOPLEVEL *toplevel, FILE *fp, int x, int y, int width, int height, int color, int line_width, int length, int space, int origin_x, int origin_y);
void o_box_print_filled(TOPLEVEL *toplevel, FILE *fp, int x, int y, int width, int height, int color, int fill_width, int angle1, int pitch1, int angle2, int pitch2, int origin_x, int origin_y);
void o_box_print_mesh(TOPLEVEL *toplevel, FILE *fp, int x, int y, int width, int height, int color, int fill_width, int angle1, int pitch1, int angle2, int pitch2, int origin_x, int origin_y);
void o_box_print_hatch(TOPLEVEL *toplevel, FILE *fp, int x, int y, int width, int height, int color, int fill_width, int angle1, int pitch1, int angle2, int pitch2, int origin_x, int origin_y);
double o_box_shortest_distance(OBJECT *object, int x, int y, int force_soild);
void world_get_box_bounds(TOPLEVEL *toplevel, OBJECT *object, int *left, int *top, int *right, int *bottom);
gboolean o_box_get_position(TOPLEVEL *toplevel, gint *x, gint *y, OBJECT *object);
void o_box_recalc(TOPLEVEL *toplevel, OBJECT *o_current);

/* o_bus_basic.c */
OBJECT *o_bus_read(TOPLEVEL *toplevel, char buf[], unsigned int release_ver, unsigned int fileformat_ver);
char *o_bus_save(OBJECT *object);
void o_bus_print(TOPLEVEL *toplevel, FILE *fp, OBJECT *o_current, int origin_x, int origin_y);
void world_get_bus_bounds(TOPLEVEL *toplevel, OBJECT *object, int *left, int *top, int *right, int *bottom);
gboolean o_bus_get_position(TOPLEVEL *toplevel, gint *x, gint *y, OBJECT *object);
void o_bus_recalc(TOPLEVEL *toplevel, OBJECT *o_current);

/* o_circle_basic.c */
OBJECT *o_circle_read(TOPLEVEL *toplevel, char buf[], unsigned int release_ver, unsigned int fileformat_ver);
char *o_circle_save(OBJECT *object);
void o_circle_print(TOPLEVEL *toplevel, FILE *fp, OBJECT *o_current, int origin_x, int origin_y);
void o_circle_print_solid(TOPLEVEL *toplevel, FILE *fp, int x, int y, int radius, int color, int circle_width, int length, int space, int origin_x, int origin_y);
void o_circle_print_dotted(TOPLEVEL *toplevel, FILE *fp, int x, int y, int radius, int color, int circle_width, int length, int space, int origin_x, int origin_y);
void o_circle_print_dashed(TOPLEVEL *toplevel, FILE *fp, int x, int y, int radius, int color, int circle_width, int length, int space, int origin_x, int origin_y);
void o_circle_print_center(TOPLEVEL *toplevel, FILE *fp, int x, int y, int radius, int color, int circle_width, int length, int space, int origin_x, int origin_y);
void o_circle_print_phantom(TOPLEVEL *toplevel, FILE *fp, int x, int y, int radius, int color, int circle_width, int length, int space, int origin_x, int origin_y);
void o_circle_print_filled(TOPLEVEL *toplevel, FILE *fp, int x, int y, int radius, int color, int fill_width, int angle1, int pitch1, int angle2, int pitch2, int origin_x, int origin_y);
void o_circle_print_mesh(TOPLEVEL *toplevel, FILE *fp, int x, int y, int radius, int color, int fill_width, int angle1, int pitch1, int angle2, int pitch2, int origin_x, int origin_y);
void o_circle_print_hatch(TOPLEVEL *toplevel, FILE *fp, int x, int y, int radius, int color, int fill_width, int angle1, int pitch1, int angle2, int pitch2, int origin_x, int origin_y);
double o_circle_shortest_distance(OBJECT *object, int x, int y, int force_soild);
void world_get_circle_bounds(TOPLEVEL *toplevel, OBJECT *object, int *left, int *top, int *right, int *bottom);
gboolean o_circle_get_position(TOPLEVEL *toplevel, gint *x, gint *y, OBJECT *object);
void o_circle_recalc(TOPLEVEL *toplevel, OBJECT *o_current);

/* o_complex_basic.c */
OBJECT *o_complex_read(TOPLEVEL *toplevel, char buf[], unsigned int release_ver, unsigned int fileformat_ver);
char *o_complex_save(OBJECT *object);
double o_complex_shortest_distance(OBJECT *object, int x, int y, int force_soild);
void world_get_complex_bounds(TOPLEVEL *toplevel, OBJECT *complex, int *left, int *top, int *right, int *bottom);
gboolean o_complex_get_position(TOPLEVEL *toplevel, gint *x, gint *y, OBJECT *object);
void o_complex_recalc(TOPLEVEL *toplevel, OBJECT *o_current);
GList *o_complex_get_promotable (TOPLEVEL *toplevel, OBJECT *object, int detach);

/* o_line_basic.c */
OBJECT *o_line_read(TOPLEVEL *toplevel, char buf[], unsigned int release_ver, unsigned int fileformat_ver);
char *o_line_save(OBJECT *object);
void o_line_print(TOPLEVEL *toplevel, FILE *fp, OBJECT *o_current, int origin_x, int origin_y);
void o_line_print_solid(TOPLEVEL *toplevel, FILE *fp, int x1, int y1, int x2, int y2, int color, int line_width, int length, int space, int origin_x, int origin_y);
void o_line_print_dotted(TOPLEVEL *toplevel, FILE *fp, int x1, int y1, int x2, int y2, int color, int line_width, int length, int space, int origin_x, int origin_y);
void o_line_print_dashed(TOPLEVEL *toplevel, FILE *fp, int x1, int y1, int x2, int y2, int color, int line_width, int length, int space, int origin_x, int origin_y);
void o_line_print_center(TOPLEVEL *toplevel, FILE *fp, int x1, int y1, int x2, int y2, int color, int line_width, int length, int space, int origin_x, int origin_y);
void o_line_print_phantom(TOPLEVEL *toplevel, FILE *fp, int x1, int y1, int x2, int y2, int color, int line_width, int length, int space, int origin_x, int origin_y);
double o_line_shortest_distance(OBJECT *object, int x, int y, int force_soild);
void world_get_line_bounds(TOPLEVEL *toplevel, OBJECT *object, int *left, int *top, int *right, int *bottom);
gboolean o_line_get_position(TOPLEVEL *toplevel, gint *x, gint *y, OBJECT *object);
void o_line_recalc(TOPLEVEL *toplevel, OBJECT *o_current);

/* o_net_basic.c */
OBJECT *o_net_read(TOPLEVEL *toplevel, char buf[], unsigned int release_ver, unsigned int fileformat_ver);
char *o_net_save(OBJECT *object);
void o_net_print(TOPLEVEL *toplevel, FILE *fp, OBJECT *o_current, int origin_x, int origin_y);
void world_get_net_bounds(TOPLEVEL *toplevel, OBJECT *object, int *left, int *top, int *right, int *bottom);
gboolean o_net_get_position(TOPLEVEL *toplevel, gint *x, gint *y, OBJECT *object);
void o_net_recalc(TOPLEVEL *toplevel, OBJECT *o_current);

/* o_path_basic.c */
OBJECT *o_path_read(TOPLEVEL *toplevel, const char *first_line, TextBuffer *tb, unsigned int release_ver, unsigned int fileformat_ver);
char *o_path_save(OBJECT *object);
void o_path_print(TOPLEVEL *toplevel, FILE *fp, OBJECT *o_current, int origin_x, int origin_y);
double o_path_shortest_distance(OBJECT *object, int x, int y, int force_soild);
void world_get_path_bounds(TOPLEVEL *toplevel, OBJECT *object, int *left, int *top, int *right, int *bottom);
gboolean o_path_get_position(TOPLEVEL *toplevel, gint *x, gint *y, OBJECT *object);
void o_path_recalc(TOPLEVEL *toplevel, OBJECT *o_current);


/* o_picture.c */
OBJECT *o_picture_read(TOPLEVEL *toplevel, const char *first_line, TextBuffer *tb, unsigned int release_ver, unsigned int fileformat_ver);
char *o_picture_save(OBJECT *object);
void o_picture_print(TOPLEVEL *toplevel, FILE *fp, OBJECT *o_current,
		     int origin_x, int origin_y);
double o_picture_shortest_distance(OBJECT *object, int x, int y, int force_soild);
void world_get_picture_bounds(TOPLEVEL *toplevel, OBJECT *object, int *left, int *top, int *right, int *bottom);
gboolean o_picture_get_position(TOPLEVEL *toplevel, gint *x, gint *y, OBJECT *object);
void o_picture_recalc(TOPLEVEL *toplevel, OBJECT *o_current);

/* o_pin_basic.c */
OBJECT *o_pin_read(TOPLEVEL *toplevel, char buf[], unsigned int release_ver, unsigned int fileformat_ver);
char *o_pin_save(OBJECT *object);
void o_pin_print(TOPLEVEL *toplevel, FILE *fp, OBJECT *o_current, int origin_x, int origin_y);
void world_get_pin_bounds(TOPLEVEL *toplevel, OBJECT *object, int *left, int *top, int *right, int *bottom);
gboolean o_pin_get_position(TOPLEVEL *toplevel, gint *x, gint *y, OBJECT *object);
void o_pin_recalc(TOPLEVEL *toplevel, OBJECT *o_current);

/* o_text_basic.c */
OBJECT *o_text_read(TOPLEVEL *toplevel, const char *first_line, TextBuffer *tb, unsigned int release_ver, unsigned int fileformat_ver);
char *o_text_save(OBJECT *object);
void o_text_print_text_string(FILE *fp, char *string, int unicode_count, gunichar *unicode_table);
void o_text_print(TOPLEVEL *toplevel, FILE *fp, OBJECT *o_current, int origin_x, int origin_y, int unicode_count, gunichar *unicode_table);
double o_text_shortest_distance(OBJECT *object, int x, int y, int force_soild);
int world_get_text_bounds(TOPLEVEL *toplevel, OBJECT *o_current, int *left, int *top, int *right, int *bottom);
gboolean o_text_get_position(TOPLEVEL *toplevel, gint *x, gint *y, OBJECT *object);
void o_text_recalc(TOPLEVEL *toplevel, OBJECT *o_current);

/* s_clib.c */
void s_clib_init (void);

/* s_color.c */
void s_color_init(void);
gchar *s_color_ps_string(gint color);

/* s_conn.c */
CONN *s_conn_return_new(OBJECT *other_object, int type, int x, int y, int whichone, int other_whichone);
int s_conn_uniq(GList *conn_list, CONN *input_conn);
int s_conn_remove_other(TOPLEVEL *toplevel, OBJECT *other_object, OBJECT *to_remove);
OBJECT *s_conn_check_midpoint(OBJECT *o_current, int x, int y);
void s_conn_print(GList *conn_list);

/* s_encoding.c */
gchar* s_encoding_base64_encode (gchar* src, guint srclen, guint* dstlenp, gboolean strict);
gchar* s_encoding_base64_decode (gchar* src, guint srclen, guint* dstlenp);

/* s_path.c */
int s_path_to_polygon(PATH *path, GArray *points);
double s_path_shortest_distance (PATH *path, int x, int y, int solid);

/* s_textbuffer.c */
TextBuffer *s_textbuffer_new (gchar *data, const gint size);
TextBuffer *s_textbuffer_free (TextBuffer *tb);
void s_textbuffer_seek (TextBuffer *tb, const gint offset);
gchar *s_textbuffer_next (TextBuffer *tb, const gsize count);
gchar *s_textbuffer_next_line (TextBuffer *tb);

/* s_tile.c */
void s_tile_init(TOPLEVEL *toplevel, PAGE *p_current);
void s_tile_add_object(TOPLEVEL *toplevel, OBJECT *object);
void s_tile_remove_object(OBJECT *object);
void s_tile_print(TOPLEVEL *toplevel, PAGE *page);
void s_tile_free_all(PAGE *p_current);

/* s_weakref.c */
void s_weakref_notify (void *dead_ptr, GList *weak_refs);
GList *s_weakref_add (GList *weak_refs, void (*notify_func)(void *, void *), void *user_data);
GList *s_weakref_remove (GList *weak_refs, void (*notify_func)(void *, void *), void *user_data);
GList *s_weakref_add_ptr (GList *weak_refs, void **weak_pointer_loc);
GList *s_weakref_remove_ptr (GList *weak_refs, void **weak_pointer_loc);
