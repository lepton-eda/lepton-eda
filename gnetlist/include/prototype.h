/* g_netlist.c */
void g_set_project_current(TOPLEVEL *pr_current);
SCM g_scm_c_get_uref (OBJECT *object);
SCM g_get_verbosity ();
SCM g_get_backend_arguments ();
SCM g_get_input_files();
SCM g_get_pins(SCM uref);
SCM g_get_all_nets(SCM scm_level);
SCM g_get_all_unique_nets(SCM scm_level);
SCM g_get_all_package_attributes(SCM scm_uref, SCM scm_wanted_attrib);
SCM g_get_attribute_by_pinseq(SCM scm_uref, SCM scm_pinseq, SCM scm_wanted_attrib);
SCM g_get_attribute_by_pinnumber(SCM scm_uref, SCM scm_pin, SCM scm_wanted_attrib);
SCM g_get_toplevel_attribute(SCM scm_wanted_attrib);
SCM g_graphical_objs_in_net_with_attrib_get_attrib(SCM scm_netname, SCM scm_has_attribute, SCM scm_wanted_attribute);
/* g_rc.c */
SCM g_rc_gnetlist_version(SCM version);
/* g_register.c */
void g_register_funcs(void);
SCM g_quit(void);
/* globals.c */
/* gnetlist.c */
void gnetlist_quit(void);
void main_prog(void *closure, int argc, char *argv[]);
int main(int argc, char *argv[]);
/* i_vars.c */
void i_vars_set(TOPLEVEL *pr_current);
void i_vars_init_gnetlist_defaults (void);
/* parsecmd.c */
void usage(char *cmd);
GSList *create_input_files_list(int argi, int argc, char *argv[]);
int parse_commandline(int argc, char *argv[]);
/* s_cpinlist.c */
CPINLIST *s_cpinlist_return_tail(CPINLIST *head);
CPINLIST *s_cpinlist_add(CPINLIST *ptr);
void s_cpinlist_print(CPINLIST *ptr);
CPINLIST *s_cpinlist_search_pin(CPINLIST *ptr, char *pin_number);
SCM scm_from_pin_list (CPINLIST *pin_list);
/* s_hierarchy.c */
void s_hierarchy_traverse(TOPLEVEL *pr_current, OBJECT *o_current, NETLIST *netlist);
void s_hierarchy_post_process (NETLIST *head);
int s_hierarchy_setup_rename (NETLIST *head, char *uref, char *label, char *new_name);
void s_hierarchy_remove_urefconn(NETLIST *head, char *uref_disable);
void s_hierarchy_remove_compsite_all(NETLIST *head);
char *s_hierarchy_create_uref (char *basename, char *hierarchy_tag);
char *s_hierarchy_create_netname (char *basename, char *hierarchy_tag);
char *s_hierarchy_create_netattrib (char *basename, char *hierarchy_tag);
void s_hierarchy_remove_uref_mangling (NETLIST *head);
char *s_hierarchy_return_baseuref (char *uref);
int s_hierarchy_graphical_search(OBJECT* o_current, int count);
/* s_misc.c */
void verbose_print(char *string);
void verbose_done(void);
void verbose_reset_index(void);
/* s_net.c */
NET *s_net_add(NET *ptr);
void s_net_print(NET *ptr);
char *s_net_return_connected_string (OBJECT *object, char *hierarchy_tag);
int s_net_find(NET *net_head, NET *node);
char *s_net_name_search (NET *net_head);
char *s_net_name (NETLIST *netlist_head, NET *net_head, char *hierarchy_tag, int type);
SCM scm_from_net_list (NET *net_list);
/* s_netattrib.c */
gchar *s_netattrib_pinnum_get_connected_string (const gchar *pinnum) G_GNUC_WARN_UNUSED_RESULT;
const gchar *s_netattrib_connected_string_get_pinnum (const gchar *str);
void s_netattrib_check_connected_string (const gchar *str);
char *s_netattrib_extract_netname (char *value);
void s_netattrib_create_pins (OBJECT *o_current, NETLIST *netlist, char *value, char *hierarchy_tag);
void s_netattrib_handle (OBJECT *o_current, NETLIST *netlist, char *hierarchy_tag);
char *s_netattrib_net_search (OBJECT *o_current, const gchar *wanted_pin);
char *s_netattrib_return_netname (OBJECT *o_current, char *pinnumber, char *hierarchy_tag);
/* s_netlist.c */
NETLIST *s_netlist_return_tail(NETLIST *head);
NETLIST *s_netlist_add(NETLIST *ptr);
void s_netlist_print(NETLIST *ptr);
void s_netlist_post_process (NETLIST *head);
void s_netlist_name_named_nets (NETLIST *named_netlist, NETLIST *unnamed_netlist);
char *s_netlist_netname_of_netid (NETLIST *netlist_head, int net_id);
SCM scm_from_netlist_list (NETLIST *netlist_list);
/* s_rename.c */
void s_rename_init(void);
void s_rename_destroy_all(void);
void s_rename_next_set(void);
void s_rename_print(void);
int s_rename_search(char *src, char *dest, int quiet_flag);
void s_rename_add(char *src, char *dest);
void s_rename_all_lowlevel(NETLIST *netlist_head, char *src, char *dest);
void s_rename_all (NETLIST *netlist_head);
SCM g_get_renamed_nets(SCM scm_level);
/* s_traverse.c */
void s_traverse_sheet(TOPLEVEL *pr_current, const GList *obj_list, char *hierarchy_tag);
CPINLIST *s_traverse_component(TOPLEVEL *pr_current, OBJECT *component, char *hierarchy_tag);
NET *s_traverse_net (NET *nets, int starting, OBJECT *object, char *hierarchy_tag, int type);
void s_init_traverse (void);
/* vams_misc.c */
SCM vams_get_attribs_list(OBJECT *object);
SCM vams_get_package_attributes(SCM scm_uref);
