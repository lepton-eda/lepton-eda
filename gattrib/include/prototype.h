/*
 * This file holds all function prototypes for the entire gattrib
 * project.  It should be #include'ed after struct.h.
 */

/* ---------------- gattrib.c ---------------- */
void gattrib_really_quit(void);
void gattrib_quit(void);
void gattrib_main(void *closure, int argc, char *argv[]);
int main(int argc, char *argv[]);


/* -------------- parsecmd.c ----------------- */
void usage(char *cmd);   
     /* output usage string */
int parse_commandline(int argc, char *argv[]);
     /* run through cmd line options and set mode switches. */

/* -------------- listsort.c ----------------- */
int cmp(STRING_LIST *a, STRING_LIST *b);
STRING_LIST *listsort(STRING_LIST *list, int is_circular, int is_double);

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

void s_sheet_data_add_master_comp_list_items(OBJECT *start_obj);
void s_sheet_data_add_master_comp_attrib_list_items(OBJECT *start_obj);
void s_sheet_data_add_master_net_list_items(OBJECT *start_obj);
void s_sheet_data_add_master_net_attrib_list_items(OBJECT *start_obj);
void s_sheet_data_add_master_pin_list_items(OBJECT *start_obj);
void s_sheet_data_add_master_pin_attrib_list_items(OBJECT *start_obj);

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
void s_table_destroy(TABLE **table, int row_count, int col_count);
int s_table_get_index(STRING_LIST *list, char *string);
STRING_LIST *s_table_create_attrib_pair(gchar *row_name, 
					TABLE **table, 
					STRING_LIST *row_list,
					int num_attribs);

void s_table_add_toplevel_comp_items_to_comp_table(OBJECT *start_obj);
void s_table_add_toplevel_net_items_to_net_table(OBJECT *start_obj);
void s_table_add_toplevel_pin_items_to_pin_table(OBJECT *start_obj);

void s_table_gtksheet_to_all_tables();
void s_table_gtksheet_to_table(GtkSheet *local_gtk_sheet, 
			      STRING_LIST *master_row_list, STRING_LIST *master_col_list, 
			      TABLE **local_table, int num_rows, int num_cols);

/* ------------- s_toplevel.c ------------- */
int s_toplevel_read_page(char *filename);
void s_toplevel_verify_design(TOPLEVEL *pr_current);
void s_toplevel_empty_project();
void s_toplevel_gtksheet_to_toplevel();
void s_toplevel_update_page(OBJECT *start_obj);
void s_toplevel_menubar_file_open(TOPLEVEL *pr_current);
void s_toplevel_menubar_file_save(TOPLEVEL *pr_current);
void s_toplevel_menubar_file_open(TOPLEVEL *pr_current);
void s_toplevel_menubar_edit_newattrib();
void s_toplevel_add_new_attrib(gchar *new_attrib_name);
void s_toplevel_menubar_edit_delattrib();
void s_toplevel_delete_attrib_col();
void s_toplevel_menubar_unimplemented_feature();
void s_toplevel_select_object();
void s_toplevel_sheetdata_to_toplevel(OBJECT *start_obj);

STRING_LIST *s_toplevel_get_component_attribs_in_sheet(char *refdes);
void s_toplevel_update_component_attribs_in_toplevel(OBJECT *o_current, 
					 STRING_LIST *new_comp_attrib_list);
STRING_LIST *s_toplevel_get_net_attribs_in_sheet(char *netname);
void s_toplevel_update_net_attribs_in_toplevel(OBJECT *o_current, 
					 STRING_LIST *new_net_attrib_list);
STRING_LIST *s_toplevel_get_pin_attribs_in_sheet(char *refdes, OBJECT *pin);
void s_toplevel_update_pin_attribs_in_toplevel(char *refdes, OBJECT *pin,
					 STRING_LIST *new_pin_attrib_list);


/* ------------- s_object.c ------------- */
void s_object_add_comp_attrib_to_object(OBJECT *o_current, char *new_attrib_name, 
				char *new_attrib_value);
void s_object_add_net_attrib_to_object(OBJECT *o_current, char *new_attrib_name, 
				char *new_attrib_value);
void s_object_add_pin_attrib_to_object(OBJECT *o_current, char *new_attrib_name, 
				char *new_attrib_value);

void s_object_replace_attrib_in_object(OBJECT *o_current, char *new_attrib_name, 
			       char *new_attrib_value);
void s_object_remove_attrib_in_object(OBJECT *o_current, char *new_attrib_name);

OBJECT *s_object_attrib_add_attrib_in_object(TOPLEVEL * w_current, char *text_string,
				     int visibility, int show_name_value,
				     OBJECT * object);
void s_object_delete_text_object_in_object(TOPLEVEL *w_current, OBJECT *test_object);
int s_object_has_sym_file(OBJECT *object);

/* ------------- s_rename.c ------------- */
void s_rename_init(void);
void s_rename_destroy_all(void);
void s_rename_next_set(void);
void s_rename_print(void);
int s_rename_search(char *src, char *dest, int quiet_flag);
void s_rename_add(char *src, char *dest);
void s_rename_all_lowlevel(NETLIST * netlist_head, char *src, char *dest);
void s_rename_all(TOPLEVEL * pr_current, NETLIST * netlist_head);

/* ------------- s_misc.c ------------- */
void verbose_print(char *string);
void verbose_done(void);
void verbose_reset_index(void);

/* ------------- i_vars.c ------------- */
void i_vars_set(TOPLEVEL * pr_current);
void i_window_vars_set(TOPLEVEL * w_current);
void i_vars_setnames(TOPLEVEL * w_current);


/* ------------- x_dialog.c ------------- */
void x_dialog_newattrib_get_name();
int x_dialog_newattrib_keypress_callback(GtkWidget * widget, 
					 GdkEventKey * event,
					 GtkWidget *window);
void x_dialog_newattrib_close_callback(GtkWidget * widget, 
				      GtkWidget *window);
void x_dialog_newattrib_ok_callback(GtkWidget * widget, 
				      GtkWidget *window);

void x_dialog_delattrib_confirm();
int x_dialog_delattrib_keypress_callback(GtkWidget * widget, 
					 GdkEventKey * event,
					 GtkWidget *window);
void x_dialog_delattrib_close_callback(GtkWidget *buttonclose, 
				      GtkWidget *window);
void x_dialog_delattrib_yes_callback(GtkWidget *buttonok, 
				    GtkWidget *window);

void x_dialog_missing_sym();
int x_dialog_missing_sym_donothing_callback(GtkWidget * widget,
					    GdkEventKey * event,
					    GtkWidget *window);
void x_dialog_missing_sym_continue_callback(GtkWidget *buttonclose,
					   GtkWidget *window);
void x_dialog_missing_sym_abort_callback(GtkWidget *buttonyes,
					GtkWidget *window);

void x_dialog_unsaved_data();
int x_dialog_unsaved_data_donothing_callback(GtkWidget * widget,
					    GdkEventKey * event,
					    GtkWidget *window);
void x_dialog_unsaved_data_continue_callback(GtkWidget *buttonclose,
					   GtkWidget *window);
void x_dialog_unsaved_data_abort_callback(GtkWidget *buttonyes,
					GtkWidget *window);

void x_dialog_unimplemented_feature();

int x_dialog_about_keypress_callback(GtkWidget * widget, GdkEventKey * event,
				     GtkWidget * window);
void x_dialog_about_close_callback(GtkWidget * widget, GtkWidget *window);
void x_dialog_about_dialog();

GtkWidget *x_dialog_create_dialog_box(GtkWidget ** out_vbox,
				      GtkWidget ** out_action_area);
void x_dialog_close_window(GtkWidget * window);


/* ------------- x_gtksheet.c ------------- */
void x_gtksheet_init();
void x_notebook_init();
void x_gtksheet_add_row_labels(GtkSheet *sheet, int count, STRING_LIST *list_head);
void x_gtksheet_add_col_labels(GtkSheet *sheet, int count, STRING_LIST *list_head);
void x_gtksheet_add_cell_item(GtkSheet *sheet, int i, int j, char *text);
int x_gtksheet_get_min_col(GtkSheet *sheet);
int x_gtksheet_get_max_col(GtkSheet *sheet);


void format_text (GtkSheet *sheet, gchar *text, gint *justification, char *label);
void alarm_change(GtkWidget *widget, gint row, gint col,
                  gpointer data);
void alarm_activate(GtkWidget *widget, gint row, gint col,
                    gpointer data);
void alarm_deactivate(GtkWidget *widget, gint row, gint col,
                      gpointer data);
gint alarm_traverse(GtkWidget *widget,
                    gint row, gint col, gint *new_row, gint *new_col,
                    gpointer data);
void clipboard_handler(GtkWidget *widget, GdkEventKey *key);
void parse_numbers(GtkWidget *widget, gpointer data);
void resize_handler(GtkWidget *widget, GtkSheetRange *old_range, 
		    GtkSheetRange *new_range, 
		    gpointer data);
void move_handler(GtkWidget *widget, GtkSheetRange *old_range, 
		  GtkSheetRange *new_range, 
		  gpointer data);
gint change_entry(GtkWidget *widget, 
		  gint row, gint col, gint *new_row, gint *new_col,
		  gpointer data);
void set_cell(GtkWidget *widget, gchar *insert, gint text_legth, gint position, 
	      gpointer data);
void show_sheet_entry(GtkWidget *widget, gpointer data);
void activate_sheet_entry(GtkWidget *widget, gpointer data);
void show_entry(GtkWidget *widget, gpointer data);
void justify_left(GtkWidget *widget);
void justify_center(GtkWidget *widget);
void justify_right(GtkWidget *widget);
gint activate_sheet_cell(GtkWidget *widget, gint row, 
			 gint column, gpointer data);
void change_border (GtkWidget *widget, gint border);
void change_fg(GtkWidget *widget, gint i, gchar *color_name);
void change_bg(GtkWidget *widget, gint i, gchar *color_name);
void do_hide_row_titles(GtkWidget *widget);
void do_hide_column_titles(GtkWidget *widget);
void do_show_row_titles(GtkWidget *widget);
void do_show_column_titles(GtkWidget *widget);


/* ------------- x_fileselect.c ------------- */
void x_fileselect_destroy_window(GtkWidget * widget, FILEDIALOG * f_current);
int x_fileselect_keypress(GtkWidget * widget, GdkEventKey * event,
			  FILEDIALOG * f_current);
void x_fileselect_init_list_buffers(FILEDIALOG * f_current);
void x_fileselect_free_list_buffers(FILEDIALOG * f_current);
void x_fileselect_update_dirfile(FILEDIALOG * f_current, char *filename);
void x_fileselect_setup_list_buffers(FILEDIALOG * f_current,
                                int num_files, int num_directories);
int x_fileselect_include_file(char *filename, int filter_type);
void x_fileselect_fill_lists(FILEDIALOG * f_current);
gint x_fileselect_sch_files(GtkWidget * w, FILEDIALOG * f_current);
gint x_fileselect_sym_files(GtkWidget * w, FILEDIALOG * f_current);
gint x_fileselect_both_files(GtkWidget * w, FILEDIALOG * f_current);
gint x_fileselect_all_files(GtkWidget * w, FILEDIALOG * f_current);
/* static GtkWidget *x_fileselect_filter_menu(FILEDIALOG * f_current); */
int
x_fileselect_preview_checkbox(GtkWidget * widget, FILEDIALOG * f_current);
void x_fileselect_saveas_close(GtkWidget * w, FILEDIALOG * f_current);
void x_fileselect_saveas(GtkWidget * w, FILEDIALOG * f_current);
void x_fileselect_change_dir(FILEDIALOG * f_current, char *new_directory);
void x_fileselect_set_filename(TOPLEVEL * w_current, const char *string);
void x_fileselect_open_file(GtkWidget * w, FILEDIALOG * f_current);
void
x_fileselect_dir_button(GtkWidget * widget, gint row, gint column,
                        GdkEventButton * bevent, FILEDIALOG * f_current);
void
x_fileselect_file_button(GtkWidget * widget, gint row, gint column,
                         GdkEventButton * bevent, FILEDIALOG * f_current);
void
x_fileselect_update_dirfile_saveas(FILEDIALOG * f_current,
                                   char *new_filename);
void x_fileselect_close(GtkWidget * w, FILEDIALOG * f_current);
void x_fileselect_search(GtkWidget * w, FILEDIALOG * f_current);
void x_fileselect_setup(TOPLEVEL *pr_current, int filesel_type);

/* ------------- x_window.c ------------- */
void x_window_init();
void x_window_create_menu(GtkWidget **menubar);
void x_window_add_items();



