/*******************************************************************************/
/*                                                                             */
/* gEDA Suite Project Manager                                                  */
/*                                                                             */
/* Copyright (C) 2002 Piotr Miarecki, sp9rve@radioam.net                       */
/*                                                                             */
/* This program is free software; you can redistribute it and/or               */
/* modify it under the terms of the GNU General Public License                 */
/* as published by the Free Software Foundation version 2.                     */
/*                                                                             */
/* This program is distributed in the hope that it will be useful,             */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of              */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               */
/* GNU General Public License for more details.                                */
/*                                                                             */
/* You should have received a copy of the GNU General Public License           */
/* along with this program; if not, email to the author                        */
/*                                                                             */
/*******************************************************************************/

#include <gtk/gtk.h>


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
MainNotebookXAxisScaleXmaxCheck_toggled
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
MainNotebookYAxisScaleYmaxCheck_toggled
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
