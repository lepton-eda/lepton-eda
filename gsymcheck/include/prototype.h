/* g_rc.c */
void set_static_project_current(TOPLEVEL *pr_current);
char *g_rc_parse_path(void);
void g_rc_parse(TOPLEVEL *pr_current);
SCM g_rc_gsymcheck_version(SCM version);
SCM g_rc_default_series_name(SCM name);
SCM g_rc_untitled_name(SCM name);
SCM g_rc_component_library(SCM path);
SCM g_rc_component_library_search(SCM path);
SCM g_rc_source_library(SCM path);
SCM g_rc_source_library_search(SCM path);
SCM g_rc_scheme_directory(SCM path);
SCM g_rc_font_directory(SCM path);
SCM g_rc_bitmap_directory(SCM path);
SCM g_rc_paper_size(SCM width, SCM height, SCM border);
/* g_register.c */
void g_register_funcs(void);
SCM g_quit(void);
/* globals.c */
void s_log_update(char *buf);
/* gsymcheck.c */
void gsymcheck_quit(void);
void main_prog(int argc, char *argv[]);
int main(int argc, char *argv[]);
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
/* s_symstruct.c */
SYMCHECK *s_symstruct_init(void);
void s_symstruct_print(SYMCHECK *s_current);
void s_symstruct_free(SYMCHECK *s_current);
