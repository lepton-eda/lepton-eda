/* g_rc.c */
SCM g_rc_gschlas_version(SCM version);
SCM g_rc_force_boundingbox(SCM mode);
/* g_register.c */
void g_register_funcs(void);
SCM g_quit(void);
/* globals.c */
void o_select_dummy(TOPLEVEL *w_current, OBJECT *o_current, int type, int count);
/* gschlas.c */
void gschlas_quit(void);
void main_prog(int argc, char *argv[]);
int main(int argc, char *argv[]);
/* i_vars.c */
void i_vars_set(TOPLEVEL *pr_current);
void i_vars_setnames(TOPLEVEL *w_current);
/* parsecmd.c */
void usage(char *cmd);
int parse_commandline(int argc, char *argv[]);
