/* g_netlist.c */
SCM g_scm_c_get_uref (OBJECT *object);
/* g_register.c */
void g_register_funcs(void);
SCM g_quit(void);
/* globals.c */
/* gnetlist.c */
void gnetlist_quit(void);
void main_prog(void *closure, int argc, char *argv[]);
int main(int argc, char *argv[]);
/* i_vars.c */
void i_vars_init_gnetlist_defaults (void);
/* parsecmd.c */
void usage(char *cmd);
int parse_commandline(int argc, char *argv[]);
/* s_cpinlist.c */
CPINLIST *s_cpinlist_add(CPINLIST *ptr);
SCM scm_from_pin_list (CPINLIST *pin_list);
/* s_hierarchy.c */
void s_hierarchy_traverse(TOPLEVEL *pr_current, OBJECT *o_current, NETLIST *netlist);
void s_hierarchy_post_process (NETLIST *head);
int s_hierarchy_setup_rename (NETLIST *head, char *uref, char *label, char *new_name);
void s_hierarchy_remove_urefconn(NETLIST *head, char *uref_disable);
int s_hierarchy_graphical_search(OBJECT* o_current, int count);
/* s_misc.c */
void verbose_print(char *string);
void verbose_done(void);
void verbose_reset_index(void);
/* s_net.c */
NET *s_net_add(NET *ptr);
int s_net_find(NET *net_head, NET *node);
char *s_net_name_search (NET *net_head);
char *s_net_name (NETLIST *netlist_head, NET *net_head, char *hierarchy_tag, int type, SCM netlist_mode);
SCM scm_from_net_list (NET *net_list);
/* s_netlist.c */
NETLIST *s_netlist_return_tail(NETLIST *head);
NETLIST *s_netlist_add(NETLIST *ptr);
void s_netlist_post_process (NETLIST *head, SCM netlist_mode);
SCM scm_from_netlist_list (NETLIST *netlist_list);
/* s_rename.c */
void init_rename_procs (void);
void s_rename_init(void);
int s_rename_search(char *src, char *dest, int quiet_flag);
void s_rename_add(char *src, char *dest);
/* s_traverse.c */
void s_traverse_sheet(TOPLEVEL *pr_current, const GList *obj_list, char *hierarchy_tag);
CPINLIST *s_traverse_component(TOPLEVEL *pr_current, OBJECT *component, char *hierarchy_tag);
NET *s_traverse_net (NET *nets, int starting, OBJECT *object, char *hierarchy_tag, int type);
void s_init_traverse (void);
