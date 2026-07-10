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

const char*
attrib_gtksheet_state_to_string (int val);

int
attrib_gtksheet_state_from_string (char *s);

/* ------------- s_sheet_data.c ------------- */
SHEET_DATA*
attrib_sheet_data_new();

TABLE**
attrib_sheet_data_get_component_table (SHEET_DATA *data);

void
attrib_sheet_data_set_component_table (SHEET_DATA *data,
                                       TABLE** table);
TABLE**
attrib_sheet_data_get_net_table (SHEET_DATA *data);

void
attrib_sheet_data_set_net_table (SHEET_DATA *data,
                                 TABLE** table);
TABLE**
attrib_sheet_data_get_pin_table (SHEET_DATA *data);

void
attrib_sheet_data_set_pin_table (SHEET_DATA *data,
                                 TABLE** table);


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

/* ------------- x_dialog.c ------------- */

void x_dialog_unimplemented_feature();
void x_dialog_fatal_error(const gchar *string, gint return_code);

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

G_END_DECLS
