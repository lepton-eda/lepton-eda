/* g_register.c */
void g_register_funcs(void);
SCM g_quit(void);
/* lepton-symcheck.c */
void gsymcheck_quit(void);
void main_prog(void *closure, int argc, char *argv[]);
int main(int argc, char *argv[]);
/* i_vars.c */
void i_vars_set(TOPLEVEL *pr_current);
/* parsecmd.c */
void usage(char *cmd);
int parse_commandline(int argc, char *argv[]);
/* s_check.c */
void s_init_check (void);
/* s_log.c */
void s_log_update (const gchar *log_domain, GLogLevelFlags log_level, const gchar *buf);
