/* gnetlist.c */
void gnetlist_quit(void);
void main_prog(void *closure, int argc, char *argv[]);
int main(int argc, char *argv[]);
/* i_vars.c */
void i_vars_init_gnetlist_defaults (void);
/* parsecmd.c */
void usage(char *cmd);
int parse_commandline(int argc, char *argv[]);
