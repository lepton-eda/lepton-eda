#include <gtk/gtk.h>

  
void
on_optionmenu1_enter                   (GtkButton       *button,
                                        gpointer         user_data);

void
on_help_clicked                        (GtkButton       *button,
                                        gpointer         user_data);

void
on_cancel_clicked                      (GtkButton       *button,
                                        gpointer         user_data);

void
on_previous_clicked                    (GtkButton       *button,
                                        gpointer         user_data);

void
on_next_clicked                        (GtkButton       *button,
                                        gpointer         user_data);

void
on_ok_clicked                          (GtkButton       *button,
                                        gpointer         user_data);


gboolean
on_ComponentTree_show                  (GtkWidget       *widget,
                                        GdkEvent        *event,
                                        gpointer         user_data);

void
on_ComponentTree_draw                  (GtkWidget       *widget,
                                        GdkRectangle    *area,
                                        gpointer         user_data);

void
on_ok_clicked                          (GtkButton       *button,
                                        gpointer         user_data);

void
on_InstallLocalButton_clicked          (GtkButton       *button,
                                        gpointer         user_data);

void
on_InstallGlobalButton_clicked         (GtkButton       *button,
                                        gpointer         user_data);

void
on_InstallCustomButton_clicked         (GtkButton       *button,
                                        gpointer         user_data);

void
on_AgreeButton_clicked                 (GtkButton       *button,
                                        gpointer         user_data);

void
on_DismissButton_clicked               (GtkButton       *button,
                                        gpointer         user_data);

void
on_ComponentTree_tree_select_row       (GtkCTree        *ctree,
                                        GList           *node,
                                        gint             column,
                                        gpointer         user_data);

void
on_ComponentTree_tree_unselect_row     (GtkCTree        *ctree,
                                        GList           *node,
                                        gint             column,
                                        gpointer         user_data);

void
on_ComponentTree_tree_expand           (GtkCTree        *ctree,
                                        GList           *node,
                                        gpointer         user_data);

void
on_ComponentTree_tree_collapse         (GtkCTree        *ctree,
                                        GList           *node,
                                        gpointer         user_data);

void
on_ComponentTree_change_focus_row_expansion
                                        (GtkCList        *clist,
                                        gpointer         user_data);

void
on_ComponentTree_tree_select_row       (GtkCTree        *ctree,
                                        GList           *node,
                                        gint             column,
                                        gpointer         user_data);

void
on_ComponentTree_tree_unselect_row     (GtkCTree        *ctree,
                                        GList           *node,
                                        gint             column,
                                        gpointer         user_data);

void
on_ComponentTree_tree_expand           (GtkCTree        *ctree,
                                        GList           *node,
                                        gpointer         user_data);

void
on_ComponentTree_tree_collapse         (GtkCTree        *ctree,
                                        GList           *node,
                                        gpointer         user_data);

void
on_InstallDirectoryEntry_changed       (GtkEditable     *editable,
                                        gpointer         user_data);

gboolean
on_MainWindow_delete_event             (GtkWidget       *widget,
                                        GdkEvent        *event,
                                        gpointer         user_data);

void
on_CancelButton_clicked                (GtkButton       *button,
                                        gpointer         user_data);
