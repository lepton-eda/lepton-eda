/*! \file
 * This file holds all function prototypes for the entire gattrib
 * project.  It should be #include'ed after struct.h.
 */

/* ---------------- gattrib.c ---------------- */
gboolean gattrib_really_quit(void);
gint gattrib_quit(gint return_code);

/* -------------- parsecmd.c ----------------- */
void usage(char *cmd);   
     /* output usage string */
int parse_commandline(int argc, char *argv[]);
     /* run through cmd line options and set mode switches. */

/* -------------- listsort.c ----------------- */
int cmp(STRING_LIST *a, STRING_LIST *b);
STRING_LIST *listsort(STRING_LIST *list, int is_circular, int is_double);

/* ------------- f_export.c ------------- */
void f_export_components(gchar *filename);


/* ------------- g_register.c ------------- */
void g_register_funcs(void);
SCM g_quit(void);


/* ------------- g_rc.c ------------- */
SCM g_rc_gattrib_version(SCM version);

/* ------------- s_attrib.c ------------- */
int s_attrib_name_in_list(STRING_LIST *name_value_list, char *name);
char *s_attrib_get_refdes(OBJECT *object);

/* ------------- s_sheet_data.c ------------- */
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
gchar *s_string_list_get_data_at_index(STRING_LIST *list, gint index);

void s_string_list_sort_master_comp_list();
void s_string_list_sort_master_comp_attrib_list();
void s_string_list_sort_master_net_list();
void s_string_list_sort_master_net_attrib_list();
void s_string_list_sort_master_pin_list();
void s_string_list_sort_master_pin_attrib_list();


/* ------------- s_table.c ------------- */
TABLE **s_table_new(int rows, int cols);
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
int s_toplevel_read_page(TOPLEVEL *toplevel, char *filename);
void s_toplevel_verify_design(TOPLEVEL *toplevel);
void s_toplevel_gtksheet_to_toplevel(TOPLEVEL *toplevel);
void s_toplevel_add_new_attrib(gchar *new_attrib_name);
void s_toplevel_delete_attrib_col();
void s_toplevel_sheetdata_to_toplevel(TOPLEVEL *toplevel, PAGE *page);

STRING_LIST *s_toplevel_get_component_attribs_in_sheet(char *refdes);
void s_toplevel_update_component_attribs_in_toplevel(
					    TOPLEVEL *toplevel,
					    OBJECT *o_current,
					    STRING_LIST *new_comp_attrib_list);
STRING_LIST *s_toplevel_get_net_attribs_in_sheet(char *netname);
void s_toplevel_update_net_attribs_in_toplevel(OBJECT *o_current, 
					 STRING_LIST *new_net_attrib_list);
STRING_LIST *s_toplevel_get_pin_attribs_in_sheet(char *refdes, OBJECT *pin);
void s_toplevel_update_pin_attribs_in_toplevel(TOPLEVEL *toplevel,
					 char *refdes, OBJECT *pin,
					 STRING_LIST *new_pin_attrib_list);


/* ------------- s_object.c ------------- */
void s_object_add_comp_attrib_to_object (TOPLEVEL *toplevel,
                                         OBJECT *o_current,
                                         char *new_attrib_name,
                                         char *new_attrib_value,
                                         gint visibility,
                                         gint show_name_value);
void s_object_add_net_attrib_to_object (TOPLEVEL *toplevel,
                                        OBJECT *o_current,
                                        char *new_attrib_name,
                                        char *new_attrib_value);
void s_object_add_pin_attrib_to_object (TOPLEVEL *toplevel,
                                        OBJECT *o_current,
                                        char *new_attrib_name,
                                        char *new_attrib_value);

void s_object_replace_attrib_in_object (TOPLEVEL *toplevel,
                                        OBJECT *o_current,
                                        char *new_attrib_name,
                                        char *new_attrib_value,
                                        gint visibility,
                                        gint show_name_value);
void s_object_remove_attrib_in_object (TOPLEVEL *toplevel,
                                       OBJECT *o_current,
                                       char *new_attrib_name);

OBJECT *s_object_attrib_add_attrib_in_object (TOPLEVEL *toplevel,
                                              char *text_string,
                                              gint visibility,
                                              gint show_name_value,
                                              OBJECT * object);
void s_object_delete_text_object_in_object(TOPLEVEL *toplevel, OBJECT *test_object);
int s_object_has_sym_file(OBJECT *object);

/* ------------- s_rename.c ------------- */
void s_rename_init(void);
void s_rename_destroy_all(void);
void s_rename_next_set(void);
void s_rename_print(void);
int s_rename_search(char *src, char *dest, int quiet_flag);
void s_rename_add(char *src, char *dest);
void s_rename_all_lowlevel(NETLIST * netlist_head, char *src, char *dest);
void s_rename_all(TOPLEVEL *toplevel, NETLIST *netlist_head);

/* ------------- s_misc.c ------------- */
void verbose_print(char *string);
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

/* ------------- i_vars.c ------------- */
void i_vars_set(TOPLEVEL *toplevel);
void i_window_vars_set(TOPLEVEL *toplevel);

/* ------------- x_dialog.c ------------- */
void x_dialog_newattrib();
void x_dialog_delattrib();
void x_dialog_missing_sym();
void x_dialog_unsaved_data();
void x_dialog_unimplemented_feature();
void x_dialog_fatal_error(gchar *string, gint return_code);
void x_dialog_about_dialog();
void x_dialog_export_file();

/* ------------- x_gtksheet.c ------------- */
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
void x_fileselect_save (void);
gboolean x_fileselect_load_files (GSList *filenames);

/* ------------- x_window.c ------------- */
void x_window_init();
void x_window_add_items();



