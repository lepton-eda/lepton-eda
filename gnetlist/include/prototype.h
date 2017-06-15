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
/* s_hierarchy.c */
void s_hierarchy_traverse(TOPLEVEL *pr_current, OBJECT *o_current, NETLIST *netlist);
int s_hierarchy_graphical_search(OBJECT* o_current, int count);
/* s_misc.c */
void verbose_print(char *string);
void verbose_done(void);
void verbose_reset_index(void);
/* s_netlist.c */
NETLIST *s_netlist_return_tail(NETLIST *head);
NETLIST *s_netlist_add(NETLIST *ptr);
SCM scm_from_netlist_list (NETLIST *netlist_list);
/* s_traverse.c */
void s_traverse_sheet(TOPLEVEL *pr_current, const GList *obj_list, char *hierarchy_tag);
void s_init_traverse (void);
