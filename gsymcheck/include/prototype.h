/* g_rc.c */
SCM g_rc_gsymcheck_version(SCM version);
/* g_register.c */
void g_register_funcs(void);
SCM g_quit(void);
/* globals.c */
void s_log_update(char *buf);
/* gsymcheck.c */
void gsymcheck_quit(void);
void main_prog(int argc, char *argv[]);
int main(int argc, char *argv[]);
/* i_vars.c */
void i_vars_set(TOPLEVEL *pr_current);
/* parsecmd.c */
void usage(char *cmd);
int parse_commandline(int argc, char *argv[]);
/* s_check.c */
int s_check_all(TOPLEVEL *pr_current);
int s_check_symbol(TOPLEVEL *pr_current, PAGE *p_current, OBJECT *object_head);
void s_check_graphical(OBJECT *o_current, SYMCHECK *s_current);
void s_check_device(OBJECT *o_current, SYMCHECK *s_current);
void s_check_pinseq(OBJECT *object_head, SYMCHECK *s_current);
void s_check_pinnumber(OBJECT *object_head, SYMCHECK *s_current);
void s_check_slotdef(OBJECT *object_head, SYMCHECK *s_current);
void s_check_oldpin(OBJECT *object_head, SYMCHECK *s_current);
void s_check_oldslot(OBJECT *object_head, SYMCHECK *s_current);
void s_check_nets_buses(OBJECT *object_head, SYMCHECK *s_current);
void s_check_connections(OBJECT *object_head, SYMCHECK *s_current);
void s_check_obsolete_forbidden_attributes(OBJECT *object_head, SYMCHECK *s_current);
void s_check_missing_attribute(OBJECT *object, char *attribute, SYMCHECK *s_current);
void s_check_missing_attributes(OBJECT *object_head, SYMCHECK *s_current);
void s_check_totalpins(OBJECT *object_head, SYMCHECK *s_current);
/* s_symstruct.c */
SYMCHECK *s_symstruct_init(void);
void s_symstruct_print(SYMCHECK *s_current);
void s_symstruct_free(SYMCHECK *s_current);
