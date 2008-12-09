/* g_rc.c */
SCM g_rc_gsymcheck_version(SCM version);
/* g_register.c */
void g_register_funcs(void);
SCM g_quit(void);
/* gsymcheck.c */
void gsymcheck_quit(void);
void main_prog(void *closure, int argc, char *argv[]);
int main(int argc, char *argv[]);
/* i_vars.c */
void i_vars_set(TOPLEVEL *pr_current);
/* parsecmd.c */
void usage(char *cmd);
int parse_commandline(int argc, char *argv[]);
/* s_check.c */
int s_check_all(TOPLEVEL *pr_current);
int s_check_symbol(TOPLEVEL *pr_current, PAGE *p_current, GList *obj_list);
gboolean s_check_list_has_item(char **list , char *item);
void s_check_symbol_structure(GList *obj_list, SYMCHECK *s_current);
void s_check_graphical(GList *obj_list, SYMCHECK *s_current);
void s_check_device(GList *obj_list, SYMCHECK *s_current);
void s_check_pinseq(GList *obj_list, SYMCHECK *s_current);
void s_check_pinnumber(GList *obj_list, SYMCHECK *s_current);
void s_check_pin_ongrid(GList *obj_list, SYMCHECK *s_current);
void s_check_slotdef(GList *obj_list, SYMCHECK *s_current);
void s_check_oldpin(GList *obj_list, SYMCHECK *s_current);
void s_check_oldslot(GList *obj_list, SYMCHECK *s_current);
void s_check_nets_buses(GList *obj_list, SYMCHECK *s_current);
void s_check_connections(GList *obj_list, SYMCHECK *s_current);
void s_check_missing_attribute(OBJECT *object, char *attribute, SYMCHECK *s_current);
void s_check_missing_attributes(GList *obj_list, SYMCHECK *s_current);
void s_check_pintype(GList *obj_list, SYMCHECK *s_current);
/* s_log.c */
void s_log_update (const gchar *log_domain, GLogLevelFlags log_level, const gchar *buf);
/* s_symstruct.c */
SYMCHECK *s_symstruct_init(void);
void s_symstruct_print(SYMCHECK *s_current);
void s_symstruct_free(SYMCHECK *s_current);
