/*******************************************************************************/
/*                                                                             */
/* Setup                                                                       */
/*                                                                             */
/* Copyright (C) 2002 Piotr Miarecki, sp9rve@eter.ariadna.pl                   */
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
/* along with this program; if not, write to the Free Software                 */
/* Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. */
/*                                                                             */
/*******************************************************************************/

#ifndef __CALLBACKS_H_INCLUDED
#define __CALLBACKS_H_INCLUDED

#include <gtk/gtk.h>
#include "comps.h"
#include "dirs.h"



void on_optionmenu1_enter(GtkButton *pButton, gpointer pUserData);
void on_help_clicked(GtkButton *pButton, gpointer user_data);
void on_cancel_clicked(GtkButton *button, gpointer user_data);
void on_previous_clicked(GtkButton *button, gpointer user_data);
void on_next_clicked(GtkButton *button, gpointer user_data);
void on_ok_clicked(GtkButton *button, gpointer user_data);
void on_ok_clicked(GtkButton *button, gpointer user_data);
void on_AgreeButton_clicked(GtkButton *button, gpointer user_data);
void on_DismissButton_clicked(GtkButton *button, gpointer user_data);
gboolean on_MainWindow_delete_event(GtkWidget *widget, GdkEvent *event, gpointer user_data);
void on_CancelButton_clicked(GtkButton *button, gpointer user_data);



#endif /* __CALLBACKS_H_INCLUDED */
