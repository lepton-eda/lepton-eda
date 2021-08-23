/*! \file
 * Function prototypes for the libleptonattrib library.
 */

G_BEGIN_DECLS

/* attrib.c */
gboolean attrib_really_quit(void);
gint attrib_quit(gint return_code);

/* -------------- listsort.c ----------------- */
int cmp(STRING_LIST *a, STRING_LIST *b);
STRING_LIST *listsort(STRING_LIST *list, int is_circular, int is_double);

/* ------------- f_export.c ------------- */
void f_export_components(gchar *filename);


/* ------------- s_attrib.c ------------- */
int s_attrib_name_in_list(STRING_LIST *name_value_list, char *name);
char *s_attrib_get_refdes(LeptonObject *object);

/* ------------- s_sheet_data.c ------------- */
int s_sheet_data_changed (const SHEET_DATA* data);
void s_sheet_data_set_changed (SHEET_DATA* data, int changed);
SHEET_DATA *s_sheet_data_new();

void s_sheet_data_add_master_comp_list_items(const GList *obj_list);
void s_sheet_data_add_master_comp_attrib_list_items(const GList *obj_list);
void s_sheet_data_add_master_net_list_items(const GList *obj_list);
void s_sheet_data_add_master_net_attrib_list_items(const GList *obj_list);
void s_sheet_data_add_master_pin_list_items(const GList *obj_list);
void s_sheet_data_add_master_pin_attrib_list_items(const GList *obj_list);

void s_sheet_data_gtksheet_to_sheetdata();


/* ------------- s_string_list.c ------------- */
STRING_LIST *s_string_list_new();
STRING_LIST *s_string_list_duplicate_string_list(STRING_LIST *old_string_list);
void s_string_list_add_item(STRING_LIST *list, int *count, char *item);
void s_string_list_delete_item(STRING_LIST **list, int *count, char *item);
int s_string_list_in_list(STRING_LIST *list, char *item);
gint s_string_list_find_in_list (STRING_LIST *list, char *item);
gchar *s_string_list_get_data_at_index(STRING_LIST *list, gint index);

void s_string_list_sort_master_comp_list();
void s_string_list_sort_master_comp_attrib_list();
void s_string_list_sort_master_net_list();
void s_string_list_sort_master_net_attrib_list();
void s_string_list_sort_master_pin_list();
void s_string_list_sort_master_pin_attrib_list();


/* ------------- s_table.c ------------- */
void s_table_print (TABLE** src, int rows, int cols);
TABLE **s_table_new(int rows, int cols);
TABLE **s_table_copy (TABLE** src, int col_skip, int rows, int cols);
TABLE **s_table_resize(TABLE **table,
                       int rows, int old_cols, int new_cols);
void s_table_destroy(TABLE **table, int row_count, int col_count);
int s_table_get_index(STRING_LIST *list, char *string);
STRING_LIST *s_table_create_attrib_pair(gchar *row_name,
                                        TABLE **table,
                                        STRING_LIST *row_list,
                                        int num_attribs);

void s_table_add_toplevel_comp_items_to_comp_table(const GList *obj_list);
void s_table_add_toplevel_net_items_to_net_table(const GList *obj_list);
void s_table_add_toplevel_pin_items_to_pin_table(const GList *obj_list);

void s_table_gtksheet_to_all_tables();
void s_table_gtksheet_to_table(GtkSheet *local_gtk_sheet,
                               STRING_LIST *master_row_list, STRING_LIST *master_col_list,
                               TABLE **local_table, int num_rows, int num_cols);

/* ------------- s_toplevel.c ------------- */
void
s_toplevel_verify_design (LeptonToplevel *toplevel);

void
s_toplevel_save_sheet ();
void s_toplevel_add_new_attrib(gchar *new_attrib_name);
void s_toplevel_delete_attrib_col();
void
s_toplevel_sheetdata_to_toplevel (LeptonToplevel *toplevel,
                                  LeptonPage *page);

STRING_LIST *s_toplevel_get_component_attribs_in_sheet(char *refdes);
void
s_toplevel_update_component_attribs_in_toplevel (LeptonToplevel *toplevel,
                                                 LeptonObject *o_current,
                                                 STRING_LIST *new_comp_attrib_list);
STRING_LIST *s_toplevel_get_net_attribs_in_sheet(char *netname);
void s_toplevel_update_net_attribs_in_toplevel(LeptonObject *o_current,
                                               STRING_LIST *new_net_attrib_list);
STRING_LIST *s_toplevel_get_pin_attribs_in_sheet(char *refdes, LeptonObject *pin);
void
s_toplevel_update_pin_attribs_in_toplevel (LeptonToplevel *toplevel,
                                           char *refdes,
                                           LeptonObject *pin,
                                           STRING_LIST *new_pin_attrib_list);
gint
s_page_save_all (LeptonToplevel *toplevel);


/* ------------- s_object.c ------------- */
void
s_object_add_comp_attrib_to_object (LeptonToplevel *toplevel,
                                    LeptonObject *o_current,
                                    char *new_attrib_name,
                                    char *new_attrib_value,
                                    gint visibility,
                                    gint show_name_value);
void
s_object_add_net_attrib_to_object (LeptonToplevel *toplevel,
                                   LeptonObject *o_current,
                                   char *new_attrib_name,
                                   char *new_attrib_value);
void
s_object_add_pin_attrib_to_object (LeptonToplevel *toplevel,
                                   LeptonObject *o_current,
                                   char *new_attrib_name,
                                   char *new_attrib_value);

void s_object_replace_attrib_in_object (LeptonObject *o_current,
                                        char *new_attrib_name,
                                        char *new_attrib_value,
                                        gint visibility,
                                        gint show_name_value);

void
s_object_remove_attrib_in_object (LeptonToplevel *toplevel,
                                  LeptonObject *o_current,
                                  char *new_attrib_name);
void
s_object_delete_text_object_in_object (LeptonToplevel *toplevel,
                                       LeptonObject *test_object);
int s_object_has_sym_file(LeptonObject *object);

/* ------------- s_misc.c ------------- */
void set_verbose_mode ();
void verbose_print (const char *string);
void verbose_done(void);
void verbose_reset_index(void);
char *s_misc_remaining_string(char *string, char delimiter, int count);

/* ------------- s_visibility.c ------------- */
void s_visibility_set_invisible();
void s_visibility_set_name_only();
void s_visibility_set_value_only();
void s_visibility_set_name_and_value();
void s_visibility_set_cell(gint cur_page, gint row, gint col,
                           gint visibility, gint show_name_value);

/* ------------- x_dialog.c ------------- */
void x_dialog_newattrib();
void x_dialog_delattrib();
void x_dialog_missing_sym();
void x_dialog_unsaved_data();
void x_dialog_unimplemented_feature();
void x_dialog_fatal_error(const gchar *string, gint return_code);
void x_dialog_about_dialog();
void x_dialog_export_file();

/* ------------- x_gtksheet.c ------------- */
void x_gtksheet_set_saved();
void x_gtksheet_init();
void x_gtksheet_add_row_labels(GtkSheet *sheet, int count, STRING_LIST *list_head);
void x_gtksheet_add_col_labels(GtkSheet *sheet, int count, STRING_LIST *list_head);
void x_gtksheet_add_cell_item(GtkSheet *sheet, gint i, gint j,
                              char *text, gint visibility, gint show_name_value);
void x_gtksheet_set_cell_text_color(GtkSheet *sheet, gint row, gint col,
                                    gint color_name);
int x_gtksheet_get_min_col(GtkSheet *sheet);
int x_gtksheet_get_max_col(GtkSheet *sheet);

/* ------------- x_fileselect.c ------------- */
GSList *x_fileselect_open (void);

/* ------------- x_window.c ------------- */
void x_window_add_items();
void
x_window_set_toplevel (LeptonToplevel *toplevel);
LeptonToplevel*
x_window_get_toplevel ();
void
lepton_attrib_window ();
void
x_window_set_title_changed (int changed);

G_END_DECLS
