G_BEGIN_DECLS

/* a_basic.c */
int
o_save (const GList *object_list,
        const char *filename,
        GError **err);
GList*
o_read_buffer (LeptonPage *page,
               GList *object_list,
               char *buffer,
               const int size,
               const char *name,
               GError **err);
LeptonPage*
o_read (LeptonPage *page,
        char *filename,
        GError **err);

/* f_basic.c */
gchar *f_get_autosave_filename (const gchar *filename);
gboolean f_has_active_autosave (const gchar *filename, GError **err);
int
f_open (LeptonToplevel *toplevel,
        LeptonPage *page,
        const gchar *filename,
        GError **err);
int
f_open_flags (LeptonToplevel *toplevel,
              LeptonPage *page,
              const gchar *filename,
              const gint flags,
              GError **err);
int
f_save (LeptonPage *page,
        const char *filename,
        GError **error);

gchar *f_normalize_filename (const gchar *filename, GError **error) G_GNUC_WARN_UNUSED_RESULT;
char *follow_symlinks (const gchar *filename, GError **error);

GString*
f_backup_message (gchar *backup_filename,
                  gboolean stat_error);

/* g_basic.c */
SCM g_scm_eval_protected (SCM exp, SCM module_or_state);
SCM g_scm_eval_string_protected (SCM str);
SCM g_scm_c_eval_string_protected (const gchar *str);

gboolean
g_read_file (LeptonToplevel *toplevel,
             const gchar *filename,
             GError **err);

/* g_rc.c */
gboolean
g_rc_parse_system (LeptonToplevel *toplevel,
                   const gchar *rcname,
                   GError **err);
gboolean
g_rc_parse_user (LeptonToplevel *toplevel,
                 const gchar *rcname,
                 GError **err);
gboolean
g_rc_parse_local (LeptonToplevel *toplevel,
                  const gchar *rcname,
                  const gchar *path,
                  GError **err);
gboolean
g_rc_load_cache_config (LeptonToplevel* toplevel,
                        GError** err);
void
g_rc_parse (const gchar* pname,
            const gchar* rcname,
            const gchar* rcfile);
void
g_rc_parse_handler (LeptonToplevel *toplevel,
                    const gchar *rcname,
                    const gchar *rcfile,
                    ConfigParseErrorFunc handler,
                    void *user_data);

/* liblepton.c */
void liblepton_init(void);

/* m_hatch.c */
void
m_hatch_box (LeptonBox *box,
             gint angle,
             gint pitch,
             GArray *lines);
void
m_hatch_circle (LeptonCircle *circle,
                gint angle,
                gint pitch,
                GArray *lines);
void
m_hatch_path (LeptonPath *path,
              gint angle,
              gint pitch,
              GArray *lines);
/* m_polygon.c */
void
m_polygon_append_bezier (GArray *points,
                         LeptonBezier *bezier,
                         int segments);
void m_polygon_append_point(GArray *points, int x, int y);

/* o_attrib.c */
void
o_attrib_add (LeptonObject *object,
              LeptonObject *item);

void
o_attrib_attach (LeptonObject *attrib,
                 LeptonObject *object,
                 int set_color);

void
o_attrib_attach_list (GList *attr_list,
                      LeptonObject *object,
                      int set_color);

void
o_attrib_detach_all (LeptonObject *object);

void o_attrib_print(GList *attributes);

gboolean o_attrib_string_get_name_value (const gchar *string, gchar **name_ptr, gchar **value_ptr);
GList *o_attrib_find_floating_attribs (const GList *list);
char *o_attrib_search_floating_attribs_by_name (const GList *list, const char *name, int counter);
char *o_attrib_search_attached_attribs_by_name (LeptonObject *object, const char *name, int counter);
char *o_attrib_search_inherited_attribs_by_name (LeptonObject *object, const char *name, int counter);
char *o_attrib_search_object_attribs_by_name (LeptonObject *object, const char *name, int counter);
GList *o_attrib_return_attribs(LeptonObject *object);
int o_attrib_is_inherited(const LeptonObject *attrib);

/* o_selection.c */
LeptonSelection*
o_selection_new ();

void
o_selection_add (LeptonSelection *selection,
                 LeptonObject *o_selected);
void
o_selection_print_all (LeptonSelection *selection);

void
o_selection_remove (LeptonSelection *selection,
                    LeptonObject *o_selected);

/* s_attrib.c */
int s_attrib_add_entry(char *new_attrib);
int s_attrib_uniq(char *name);
void s_attrib_free(void);
void s_attrib_init(void);
char *s_attrib_get(int counter);

/* s_clib.c */
void s_clib_init (void);
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
GList*
s_toplevel_get_symbols (const LeptonToplevel *toplevel);

/* s_conn.c */
void
s_conn_remove_object (LeptonPage *page,
                      LeptonObject *object);

void s_conn_remove_object_connections (LeptonObject *to_remove);
void
s_conn_update_object (LeptonPage* page,
                      LeptonObject *object);
int s_conn_net_search(LeptonObject* new_net, int whichone, GList * conn_list);
GList *s_conn_return_others(GList *input_list, LeptonObject *object);

/* s_log.c */
void s_log_init (const gchar *filename);
void s_log_close (void);
gchar *s_log_read (void);
GLogLevelFlags
lepton_log_flag_fatal ();
GLogLevelFlags
lepton_log_level_error ();
GLogLevelFlags
lepton_log_level_critical ();
GLogLevelFlags
lepton_log_level_warning ();
GLogLevelFlags
lepton_log_level_message ();
GLogLevelFlags
lepton_log_level_info ();
GLogLevelFlags
lepton_log_level_debug ();

/* s_slot.c */
char *s_slot_search_slot(LeptonObject *object, LeptonObject **return_found);

void
s_slot_update_object (LeptonObject *object);

/* s_textbuffer.c */
TextBuffer *s_textbuffer_new (const gchar *data, const gint size, const gchar* name);
TextBuffer *s_textbuffer_free (TextBuffer *tb);
const gchar *s_textbuffer_next (TextBuffer *tb, const gssize count);
const gchar *s_textbuffer_next_line (TextBuffer *tb);
gsize s_textbuffer_linenum (TextBuffer* tb);

/* i_vars.c */


/* \struct OptionStringInt
 * \brief  A mapping of a string option's value to an int value.
 */
struct OptionStringInt
{
  const gchar* str_; /* a string value of an option */
  gint         int_; /* an int value of an option */
};

gboolean
cfg_read_bool (const gchar* group,
               const gchar* key,
               gboolean defval,
               gboolean* result);

gboolean
cfg_read_int (const gchar* group,
              const gchar* key,
              gint defval,
              gint* result);

gboolean
cfg_check_int_not_0 (gint val);

gboolean
cfg_check_int_greater_0 (gint val);

gboolean
cfg_check_int_greater_eq_0 (gint val);

gboolean
cfg_check_int_text_size (gint val);

gboolean
cfg_read_int_with_check (const gchar* group,
                         const gchar* key,
                         gint         defval,
                         gint*        result,
                         gboolean     (*pfn_check)(int));

gboolean
cfg_read_string2int (const gchar* group,
                     const gchar* key,
                     gint         defval,
                     const struct OptionStringInt* vals,
                     size_t       nvals,
                     gint*        result);

/* version.c */
const char*
lepton_version_prepend ();

const char*
lepton_version_dotted ();

const char*
lepton_version_date ();

const char*
lepton_version_git_commit ();

const char*
lepton_version_bugreport ();

const char*
lepton_version_url ();

const char*
lepton_version_copyright ();

G_END_DECLS
