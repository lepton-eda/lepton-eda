/* g_netlist.c */
void g_set_project_current(TOPLEVEL *pr_current);
SCM g_get_packages(SCM level);
SCM g_get_pins(SCM uref);
SCM g_get_all_nets(SCM scm_level);
SCM g_get_all_unique_nets(SCM scm_level);
SCM g_get_all_connections(SCM scm_netname);
SCM g_get_nets(SCM scm_uref, SCM scm_pin);
SCM g_get_pins_nets(SCM scm_uref);
SCM g_get_package_attribute(SCM scm_uref, SCM scm_wanted_attrib);
SCM g_get_pin_attribute(SCM scm_uref, SCM scm_pin, SCM scm_wanted_attrib);
SCM g_get_toplevel_attribute(SCM scm_wanted_attrib);
/* g_rc.c */
void set_static_project_current(TOPLEVEL *pr_current);
void g_rc_parse(TOPLEVEL *pr_current);
SCM g_rc_gnetlist_version(SCM version);
SCM g_rc_default_series_name(SCM name);
SCM g_rc_untitled_name(SCM name);
SCM g_rc_component_library(SCM path);
SCM g_rc_component_library_search(SCM path);
SCM g_rc_source_library(SCM path);
SCM g_rc_source_library_search(SCM path);
SCM g_rc_scheme_directory(SCM path);
SCM g_rc_font_directory(SCM path);
SCM g_rc_paper_size(SCM width, SCM height, SCM border);
SCM g_rc_net_naming_priority(SCM mode);
SCM g_rc_hierarchy_traversal(SCM mode);
SCM g_rc_hierarchy_uref_mangle(SCM mode);
SCM g_rc_hierarchy_netname_mangle(SCM mode);
SCM g_rc_hierarchy_netattrib_mangle(SCM mode);
SCM g_rc_hierarchy_netname_separator(SCM name);
SCM g_rc_hierarchy_netattrib_separator(SCM name);
SCM g_rc_hierarchy_uref_separator(SCM name);
SCM g_rc_hierarchy_netattrib_order(SCM mode);
SCM g_rc_hierarchy_netname_order(SCM mode);
SCM g_rc_hierarchy_uref_order(SCM mode);
/* g_register.c */
void g_register_funcs(void);
SCM g_quit(void);
/* globals.c */
/* gnetlist.c */
void gnetlist_quit(void);
void main_prog(int argc, char *argv[]);
int main(int argc, char *argv[]);
/* i_vars.c */
void i_vars_set(TOPLEVEL *pr_current);
void i_vars_setnames(TOPLEVEL *w_current);
/* parsecmd.c */
void usage(char *cmd);
int parse_commandline(int argc, char *argv[]);
/* s_cpinlist.c */
CPINLIST *s_cpinlist_return_tail(CPINLIST *head);
CPINLIST *s_cpinlist_return_head(CPINLIST *tail);
CPINLIST *s_cpinlist_add(CPINLIST *ptr);
void s_cpinlist_print(CPINLIST *ptr);
CPINLIST *s_cpinlist_search_pin(CPINLIST *ptr, char *pin_number);
/* s_hierarchy.c */
void s_hierarchy_traverse(TOPLEVEL *pr_current, OBJECT *o_current, NETLIST *netlist);
void s_hierarchy_post_process(TOPLEVEL *pr_current, NETLIST *head);
int s_hierarchy_setup_rename(TOPLEVEL *pr_current, NETLIST *head, char *uref, char *label, char *new_name);
void s_hierarchy_remove_urefconn(NETLIST *head, char *uref_disable);
void s_hierarchy_remove_compsite_all(NETLIST *head);
char *s_hierarchy_create_uref(TOPLEVEL *pr_current, char *basename, char *hierarchy_tag);
char *s_hierarchy_create_netname(TOPLEVEL *pr_current, char *basename, char *hierarchy_tag);
char *s_hierarchy_create_netattrib(TOPLEVEL *pr_current, char *basename, char *hierarchy_tag);
void s_hierarchy_remove_uref_mangling(TOPLEVEL *pr_current, NETLIST *head);
char *s_hierarchy_return_baseuref(TOPLEVEL *pr_current, char *uref);
/* s_misc.c */
void verbose_print(char *string);
void verbose_done(void);
void verbose_reset_index(void);
/* s_net.c */
NET *s_net_return_tail(NET *head);
NET *s_net_return_head(NET *tail);
NET *s_net_add(NET *ptr);
void s_net_print(NET *ptr);
char *s_net_return_connected_string(TOPLEVEL *pr_current, OBJECT *object, char *hierarchy_tag);
int s_net_find(NET *net_head, NET *node);
char *s_net_name_search(TOPLEVEL *pr_current, NET *net_head);
char *s_net_name(TOPLEVEL *pr_current, NETLIST *netlist_head, NET *net_head, char *hierarchy_tag);
/* s_netattrib.c */
char *s_netattrib_extract_netname(char *value);
void s_netattrib_create_pins(TOPLEVEL *pr_current, OBJECT *o_current, NETLIST *netlist, char *value, char *hierarchy_tag);
void s_netattrib_handle(TOPLEVEL *pr_current, OBJECT *o_current, NETLIST *netlist, char *hierarchy_tag);
char *s_netattrib_net_search(OBJECT *o_current, char *wanted_pin);
char *s_netattrib_return_netname(TOPLEVEL *pr_current, OBJECT *o_current, char *pinnumber, char *hierarchy_tag);
/* s_netlist.c */
NETLIST *s_netlist_return_tail(NETLIST *head);
NETLIST *s_netlist_return_head(NETLIST *tail);
NETLIST *s_netlist_add(NETLIST *ptr);
void s_netlist_print(NETLIST *ptr);
void s_netlist_post_process(TOPLEVEL *pr_current, NETLIST *head);
/* s_project.c */
TOPLEVEL *s_project_add(TOPLEVEL *w_head, TOPLEVEL *pr_current);
void s_project_add_head(void);
void s_project_free_head(void);
void s_project_delete(TOPLEVEL *w_head, TOPLEVEL *pr_current);
void s_project_setup_world(TOPLEVEL *pr_current);
void s_project_setup_rest(TOPLEVEL *pr_current);
TOPLEVEL *s_project_create_new(void);
void s_project_close(TOPLEVEL *pr_current);
void s_project_close_all(void);
TOPLEVEL *s_project_get_ptr(int wid);
/* s_rename.c */
void s_rename_init(void);
void s_rename_destroy_all(void);
void s_rename_next_set(void);
void s_rename_print(void);
int s_rename_search(char *src, char *dest, int quiet_flag);
void s_rename_add(char *src, char *dest);
void s_rename_all_lowlevel(NETLIST *netlist_head, char *src, char *dest);
void s_rename_all(TOPLEVEL *pr_current, NETLIST *netlist_head);
SCM g_get_renamed_nets(SCM scm_level);
/* s_traverse.c */
void s_traverse_init(void);
void s_traverse_start(TOPLEVEL *pr_current);
void s_traverse_sheet(TOPLEVEL *pr_current, OBJECT *start, char *hierarchy_tag);
CPINLIST *s_traverse_component(TOPLEVEL *pr_current, OBJECT *component, char *hierarchy_tag);
void s_traverse_clear_all_visited(OBJECT *object_head);
NET *s_traverse_net(TOPLEVEL *pr_current, OBJECT *previous_object, NET *nets, OBJECT *object, char *hierarchy_tag);
NET *s_traverse_midpoints(TOPLEVEL *pr_current, OBJECT *object, NET *nets, char *hierarchy_tag);
/* vams_misc.c */
char *vams_get_attribs_list(OBJECT *object, SCM *list, OBJECT **return_found);
SCM vams_get_package_attributes(SCM scm_uref);
