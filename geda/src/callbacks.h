#include <gtk/gtk.h>
#include "file.h"



void
on_MenuProjectExit_activate            (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_MenuProjectNew_activate             (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_MenuProjectOpen_activate            (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_MenuProjectSave_activate            (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_MenuProjectClose_activate           (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

gboolean
on_MainWindow_delete_event             (GtkWidget       *widget,
                                        GdkEvent        *event,
                                        gpointer         user_data);

void
on_MenuFileNew_activate                (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_MenuFileOpen_activate               (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_MenuFileSave_activate               (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_MenuFileSaveAs_activate             (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_MenuFilePrint_activate              (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_MenuFileClose_activate              (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_MenuTextEditor_activate             (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_FileSelectOk_clicked                (GtkButton       *button,
                                        gpointer         user_data);

void
on_FileSelectCancel_clicked            (GtkButton       *button,
                                        gpointer         user_data);

gboolean
on_FileSelect_delete_event             (GtkWidget       *widget,
                                        GdkEvent        *event,
                                        gpointer         user_data);

void
Doc_Selection                          (GtkCTree        *ctree,
                                        GList           *node,
                                        gint             column,
                                        gpointer         user_data);

void
MenuFileOpen_Activation                (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
MenuFileImport_Activation              (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
MenuProjectNew_Activation              (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
MenuProjectOpen_Activation             (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
MenuProjectSave_Activation             (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
MenuProjectClose_Activation            (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
MenuProjectExit_Activation             (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
MenuFileEdit_Activation                (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
Doc_Selection                          (GtkCTree        *ctree,
                                        GList           *node,
                                        gint             column,
                                        gpointer         user_data);

gboolean
FlyingVPressed                         (GtkWidget       *widget,
                                        GdkEventButton  *event,
                                        gpointer         user_data);

gboolean
FlyingVReleased                        (GtkWidget       *widget,
                                        GdkEventButton  *event,
                                        gpointer         user_data);

gboolean
FlyingHPressed                         (GtkWidget       *widget,
                                        GdkEventButton  *event,
                                        gpointer         user_data);

gboolean
FlyingHReleased                        (GtkWidget       *widget,
                                        GdkEventButton  *event,
                                        gpointer         user_data);

void
Doc_Selection                          (GtkCTree        *ctree,
                                        GList           *node,
                                        gint             column,
                                        gpointer         user_data);

void
Doc_Selection                          (GtkCTree        *ctree,
                                        GList           *node,
                                        gint             column,
                                        gpointer         user_data);

void
on_button1_pressed                     (GtkButton       *button,
                                        gpointer         user_data);

void
on_button1_released                    (GtkButton       *button,
                                        gpointer         user_data);

gboolean
on_button1_motion_notify_event         (GtkWidget       *widget,
                                        GdkEventMotion  *event,
                                        gpointer         user_data);

void
on_button2_pressed                     (GtkButton       *button,
                                        gpointer         user_data);

void
on_button2_released                    (GtkButton       *button,
                                        gpointer         user_data);

void
WindowSwitch                           (GtkNotebook     *notebook,
                                        GtkNotebookPage *page,
                                        gint             page_num,
                                        gpointer         user_data);

gboolean
on_DocModulesTree_button_press_event   (GtkWidget       *widget,
                                        GdkEventButton  *event,
                                        gpointer         user_data);

gboolean
on_DocModulesTree_button_press_event   (GtkWidget       *widget,
                                        GdkEventButton  *event,
                                        gpointer         user_data);

void
on_StatusText_changed                  (GtkEditable     *editable,
                                        gpointer         user_data);
