G_BEGIN_DECLS

/* a_basic.c */
int o_save (TOPLEVEL *toplevel, const GList *object_list, const char *filename, GError **err);
GList *o_read_buffer(TOPLEVEL *toplevel, GList *object_list, char *buffer, const int size, const char *name, GError **err);
GList *o_read(TOPLEVEL *toplevel, GList *object_list, char *filename, GError **err);

/* f_basic.c */
gchar *f_get_autosave_filename (const gchar *filename);
gboolean f_has_active_autosave (const gchar *filename, GError **err);
int f_open(TOPLEVEL *toplevel, PAGE *page, const gchar *filename, GError **err);
int f_open_flags(TOPLEVEL *toplevel, PAGE *page, const gchar *filename,
                 const gint flags, GError **err);
void f_close(TOPLEVEL *toplevel);
int f_save(TOPLEVEL *toplevel, PAGE *page, const char *filename, GError **error);
gchar *f_normalize_filename (const gchar *filename, GError **error) G_GNUC_WARN_UNUSED_RESULT;
char *follow_symlinks (const gchar *filename, GError **error);

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
void g_rc_parse(TOPLEVEL *toplevel, const gchar* pname, const gchar* rcname, const gchar* rcfile);
void g_rc_parse_handler (TOPLEVEL *toplevel, const gchar *rcname, const gchar *rcfile, ConfigParseErrorFunc handler, void *user_data);
SCM g_rc_rc_filename();
SCM g_rc_rc_config ();

/* i_vars.c */
void i_vars_libgeda_set(TOPLEVEL *toplevel);
void i_vars_libgeda_freenames();

/* libgeda.c */
void libgeda_init(void);

/* m_hatch.c */
void m_hatch_box(GedaBox *box, gint angle, gint pitch, GArray *lines);
void m_hatch_circle(GedaCircle *circle, gint angle, gint pitch, GArray *lines);
void m_hatch_path(GedaPath *path, gint angle, gint pitch, GArray *lines);

/* m_polygon.c */
void m_polygon_append_bezier(GArray *points, GedaBezier *bezier, int segments);
void m_polygon_append_point(GArray *points, int x, int y);

/* o_attrib.c */
void o_attrib_add(TOPLEVEL *toplevel, OBJECT *object, OBJECT *item);
gboolean o_attrib_is_attached (TOPLEVEL *toplevel, OBJECT *attrib, OBJECT *object);
void o_attrib_attach(TOPLEVEL *toplevel, OBJECT *attrib, OBJECT *object, int set_color);
void o_attrib_attach_list(TOPLEVEL *toplevel, GList *attr_list, OBJECT *object, int set_color);
void o_attrib_detach_all(TOPLEVEL *toplevel, OBJECT *object);
void o_attrib_print(GList *attributes);
void o_attrib_remove(TOPLEVEL *toplevel, GList **list, OBJECT *remove);
gboolean o_attrib_string_get_name_value (const gchar *string, gchar **name_ptr, gchar **value_ptr);
gboolean o_attrib_get_name_value (const OBJECT *attrib, gchar **name_ptr, gchar **value_ptr);
GList *o_attrib_find_floating_attribs (const GList *list);
char *o_attrib_search_floating_attribs_by_name (const GList *list, char *name, int counter);
char *o_attrib_search_attached_attribs_by_name (OBJECT *object, char *name, int counter);
char *o_attrib_search_inherited_attribs_by_name (OBJECT *object, char *name, int counter);
char *o_attrib_search_object_attribs_by_name (OBJECT *object, char *name, int counter);
GList *o_attrib_return_attribs(OBJECT *object);
int o_attrib_is_inherited(const OBJECT *attrib);

/* o_embed.c */
void o_embed(TOPLEVEL *toplevel, OBJECT *o_current);
void o_unembed(TOPLEVEL *toplevel, OBJECT *o_current);

/* o_selection.c */
SELECTION *o_selection_new( void );
void o_selection_add(TOPLEVEL *toplevel, SELECTION *selection, OBJECT *o_selected);
void o_selection_print_all(const SELECTION *selection);
void o_selection_remove(TOPLEVEL *toplevel, SELECTION *selection, OBJECT *o_selected);
void o_selection_select(TOPLEVEL *toplevel, OBJECT *object) G_GNUC_DEPRECATED;
void o_selection_unselect(TOPLEVEL *toplevel, OBJECT *object) G_GNUC_DEPRECATED;

/* s_attrib.c */
int s_attrib_add_entry(char *new_attrib);
void s_attrib_print(void);
int s_attrib_uniq(char *name);
void s_attrib_free(void);
void s_attrib_init(void);
char *s_attrib_get(int counter);

/* s_basic.c */
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
void s_clib_symbol_invalidate_data (const CLibSymbol *symbol);
const CLibSymbol *s_clib_get_symbol_by_name (const gchar *name);
gchar *s_clib_symbol_get_data_by_name (const gchar *name);
GList *s_toplevel_get_symbols (const TOPLEVEL *toplevel);

/* s_conn.c */
void s_conn_remove_object_connections (TOPLEVEL *toplevel, OBJECT *to_remove);
void s_conn_update_object (PAGE* page, OBJECT *object);
int s_conn_net_search(OBJECT* new_net, int whichone, GList * conn_list);
GList *s_conn_return_others(GList *input_list, OBJECT *object);

/* s_hierarchy.c */
PAGE *s_hierarchy_down_schematic_single(TOPLEVEL *toplevel, const gchar *filename, PAGE *parent, int page_control, int flag, GError **err);
void s_hierarchy_down_symbol (TOPLEVEL *toplevel, const CLibSymbol *symbol, PAGE *parent);
PAGE *s_hierarchy_find_up_page(GedaPageList *page_list, PAGE *current_page);
PAGE* s_hierarchy_load_subpage (PAGE *page, const char *filename, GError **error);
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

/* s_slot.c */
char *s_slot_search_slot(OBJECT *object, OBJECT **return_found);
void s_slot_update_object(TOPLEVEL *toplevel, OBJECT *object);

/* s_textbuffer.c */
TextBuffer *s_textbuffer_new (const gchar *data, const gint size);
TextBuffer *s_textbuffer_free (TextBuffer *tb);
const gchar *s_textbuffer_next (TextBuffer *tb, const gssize count);
const gchar *s_textbuffer_next_line (TextBuffer *tb);

G_END_DECLS
