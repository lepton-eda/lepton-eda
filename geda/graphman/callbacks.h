#include <gtk/gtk.h>
#include "value.h"



void
MainNotebookPlotsNewButton_clicked     (GtkButton       *button,
                                        gpointer         user_data);

void
MainNotebookPlotsModifyButton_clicked  (GtkButton       *button,
                                        gpointer         user_data);

void
MainNotebookPlotsDeleteButton_clicked  (GtkButton       *button,
                                        gpointer         user_data);

void
MainNotebookGraphBorderButton_clicked  (GtkButton       *button,
                                        gpointer         user_data);

gboolean
MainDeleteEvent_clicked                (GtkWidget       *widget,
                                        GdkEvent        *event,
                                        gpointer         user_data);

void
MainNotebookPlotsNewButton_clicked     (GtkButton       *button,
                                        gpointer         user_data);

void
MainNotebookPlotsModifyButton_clicked  (GtkButton       *button,
                                        gpointer         user_data);

void
MainNotebookPlotsDeleteButton_clicked  (GtkButton       *button,
                                        gpointer         user_data);

void
MainNotebookGraphBorderButton_clicked  (GtkButton       *button,
                                        gpointer         user_data);

void
MainNotebookXAxisScaleXminCheck_toggled
                                        (GtkToggleButton *togglebutton,
                                        gpointer         user_data);

void
MainNotebookXAxisMainDxCheck_toggled   (GtkToggleButton *togglebutton,
                                        gpointer         user_data);

void
MainNotebookXAxisAddNumberCheck_toggled
                                        (GtkToggleButton *togglebutton,
                                        gpointer         user_data);

void
MainNotebookYAxisScaleYminCheck_toggled
                                        (GtkToggleButton *togglebutton,
                                        gpointer         user_data);

void
MainNotebookYAxisMainDyButton_toggled  (GtkToggleButton *togglebutton,
                                        gpointer         user_data);

void
MainNotebookYAxisAddNumberCheck_toggled
                                        (GtkToggleButton *togglebutton,
                                        gpointer         user_data);

void
MainButtonOk_clicked                   (GtkButton       *button,
                                        gpointer         user_data);

void
MainButtonCancel_clicked               (GtkButton       *button,
                                        gpointer         user_data);

void
MainButtonPlot_clicked                 (GtkButton       *button,
                                        gpointer         user_data);

void
PlotButtonOkButton_clicked             (GtkButton       *button,
                                        gpointer         user_data);

void
PlotButtonCancelButton_clicked         (GtkButton       *button,
                                        gpointer         user_data);
