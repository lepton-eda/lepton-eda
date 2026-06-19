/*! \file
 * Function prototypes for the libleptonattrib library.
 */

G_BEGIN_DECLS

/* attrib.c */
GtkWidget*
attrib_get_notebook ();

void
attrib_set_notebook (GtkWidget* widget);

SHEET_DATA*
attrib_get_sheet_data ();

void
attrib_set_sheet_data (SHEET_DATA *sheet_data);

GtkSheet*
attrib_get_sheet (int i);

void
attrib_set_sheet (int i,
                  GtkSheet* sheet);
int
attrib_get_sheets_number ();

LeptonToplevel*
attrib_get_toplevel ();

void
attrib_set_toplevel (LeptonToplevel *toplevel);

GtkWidget*
attrib_get_window ();

void
attrib_set_window (GtkWidget* window_widget);

/* -------------- listsort.c ----------------- */
int cmp(STRING_LIST *a, STRING_LIST *b);
STRING_LIST *listsort(STRING_LIST *list, int is_circular, int is_double);

/* ------------- s_attrib.c ------------- */
int s_attrib_name_in_list(STRING_LIST *name_value_list, char *name);
char *s_attrib_get_refdes(LeptonObject *object);

/* ------------- s_sheet_data.c ------------- */
SHEET_DATA*
attrib_sheet_data_new();

STRING_LIST*
attrib_sheet_data_get_component_list (SHEET_DATA *data);

void
attrib_sheet_data_set_component_list (SHEET_DATA *data,
                                      STRING_LIST *list);
STRING_LIST*
attrib_sheet_data_get_component_attrib_list (SHEET_DATA *data);

void
attrib_sheet_data_set_component_attrib_list (SHEET_DATA *data,
                                             STRING_LIST *list);
STRING_LIST**
attrib_sheet_data_get_component_attrib_list_address (SHEET_DATA *data);

STRING_LIST*
attrib_sheet_data_get_net_list (SHEET_DATA *data);

void
attrib_sheet_data_set_net_list (SHEET_DATA *data,
                                STRING_LIST *list);
STRING_LIST*
attrib_sheet_data_get_net_attrib_list (SHEET_DATA *data);

void
attrib_sheet_data_set_net_attrib_list (SHEET_DATA *data,
                                       STRING_LIST *list);
STRING_LIST*
attrib_sheet_data_get_pin_list (SHEET_DATA *data);

void
attrib_sheet_data_set_pin_list (SHEET_DATA *data,
                                STRING_LIST *list);
STRING_LIST*
attrib_sheet_data_get_pin_attrib_list (SHEET_DATA *data);

void
attrib_sheet_data_set_pin_attrib_list (SHEET_DATA *data,
                                       STRING_LIST *list);
TABLE**
attrib_sheet_data_get_component_table (SHEET_DATA *data);

void
attrib_sheet_data_set_component_table (SHEET_DATA *data,
                                       TABLE** table);
int
attrib_sheet_data_get_component_count (SHEET_DATA *data);

void
attrib_sheet_data_set_component_count (SHEET_DATA *data,
                                       int count);
int*
attrib_sheet_data_get_component_counter_address (SHEET_DATA *data);

int
attrib_sheet_data_get_component_attrib_count (SHEET_DATA *data);

void
attrib_sheet_data_set_component_attrib_count (SHEET_DATA *data,
                                              int count);
int*
attrib_sheet_data_get_component_attrib_counter_address (SHEET_DATA *data);

TABLE**
attrib_sheet_data_get_net_table (SHEET_DATA *data);

void
attrib_sheet_data_set_net_table (SHEET_DATA *data,
                                 TABLE** table);
int
attrib_sheet_data_get_net_count (SHEET_DATA *data);

void
attrib_sheet_data_set_net_count (SHEET_DATA *data,
                                 int count);
int
attrib_sheet_data_get_net_attrib_count (SHEET_DATA *data);

void
attrib_sheet_data_set_net_attrib_count (SHEET_DATA *data,
                                        int count);
TABLE**
attrib_sheet_data_get_pin_table (SHEET_DATA *data);

void
attrib_sheet_data_set_pin_table (SHEET_DATA *data,
                                 TABLE** table);
int
attrib_sheet_data_get_pin_count (SHEET_DATA *data);

void
attrib_sheet_data_set_pin_count (SHEET_DATA *data,
                                 int count);
int*
attrib_sheet_data_get_pin_counter_address (SHEET_DATA *data);

int
attrib_sheet_data_get_pin_attrib_count (SHEET_DATA *data);

void
attrib_sheet_data_set_pin_attrib_count (SHEET_DATA *data,
                                        int count);
int*
attrib_sheet_data_get_pin_attrib_counter_address (SHEET_DATA *data);

int
attrib_sheet_data_get_changed (const SHEET_DATA* data);

void
attrib_sheet_data_set_changed (SHEET_DATA* data,
                               int changed);

void s_sheet_data_set_changed (SHEET_DATA* data, int changed);


/* ------------- s_string_list.c ------------- */
char*
attrib_string_list_get_data (STRING_LIST *list);

int
attrib_string_list_get_pos (STRING_LIST *list);

STRING_LIST*
attrib_string_list_get_prev (STRING_LIST *list);

STRING_LIST*
attrib_string_list_get_next (STRING_LIST *list);

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
int
attrib_table_get_column (TABLE **table,
                         int i,
                         int j);
void
attrib_table_set_column (TABLE **table,
                         int i,
                         int j,
                         int val);
int
attrib_table_get_row (TABLE **table,
                      int i,
                      int j);
void
attrib_table_set_row (TABLE **table,
                      int i,
                      int j,
                      int val);
void
attrib_table_init_attrib_value (TABLE **table,
                                int i,
                                int j);
char*
attrib_table_get_attrib_value (TABLE **table,
                               int i,
                               int j);
void
attrib_table_set_attrib_value (TABLE **table,
                               int i,
                               int j,
                               char *val);
void
attrib_table_init_column_name (TABLE **table,
                               int i,
                               int j);
char*
attrib_table_get_column_name (TABLE **table,
                              int i,
                              int j);
void
attrib_table_set_column_name (TABLE **table,
                              int i,
                              int j,
                              char *val);
TABLE*
attrib_table_get_row_contents (TABLE **table,
                               int row);
void
attrib_table_set_row_contents (TABLE **table,
                               int row,
                               TABLE *contents);
void
attrib_table_init_row_name (TABLE **table,
                            int i,
                            int j);
char*
attrib_table_get_row_name (TABLE **table,
                           int i,
                           int j);
void
attrib_table_set_row_name (TABLE **table,
                           int i,
                           int j,
                           char *val);
int
attrib_table_get_visibility (TABLE **table,
                             int i,
                             int j);
void
attrib_table_set_visibility (TABLE **table,
                             int i,
                             int j,
                             int val);
int
attrib_table_get_show_name_value (TABLE **table,
                                  int i,
                                  int j);
void
attrib_table_set_show_name_value (TABLE **table,
                                  int i,
                                  int j,
                                  int val);
TABLE*
attrib_table_realloc_row (TABLE *table_row,
                          int columns);
TABLE**
attrib_table_new (int rows);

TABLE*
attrib_table_row_new (int columns);

/* ------------- s_misc.c ------------- */
void set_verbose_mode ();
void verbose_print (const char *string);
void verbose_done(void);
void verbose_reset_index(void);
char *s_misc_remaining_string(char *string, char delimiter, int count);

char*
u_basic_breakup_string (char *string,
                        char delimiter,
                        int count);

/* ------------- x_dialog.c ------------- */
GtkWidget*
x_dialog_unsaved_data ();

void x_dialog_unimplemented_feature();
void x_dialog_fatal_error(const gchar *string, gint return_code);

#ifdef ENABLE_GTK3
void
x_dialog_about_dialog (GSimpleAction *action,
                       GVariant *parameter,
                       gpointer user_data);
#else
void
x_dialog_about_dialog (gpointer action,
                       gpointer parameter,
                       gpointer user_data);
#endif

gboolean
x_dialog_confirm_overwrite (const gchar* fname);

/* ------------- x_gtksheet.c ------------- */
gboolean
attrib_gtksheet_activate (GtkSheet* sheet,
                          gint      row,
                          gint      column,
                          gpointer  data);
gboolean
attrib_gtksheet_deactivate (GtkSheet* sheet,
                            gint      row,
                            gint      column,
                            gpointer  data);
void
attrib_gtksheet_show_entry (GtkWidget *widget,
                            gpointer data);

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

/* ------------- x_window.c ------------- */
int
attrib_run (gpointer activate_callback,
            LeptonToplevel *toplevel);
GtkWidget*
attrib_window_menubar_new (GtkWidget *window);

GtkWidget*
attrib_window_new (gpointer app);

void
attrib_window_sheets_new ();

void
attrib_window_set_menu_callback (char *name,
                                 GCallback callback);
GtkWidget*
separator_new ();

void
x_window_set_title_changed (int changed);

G_END_DECLS
