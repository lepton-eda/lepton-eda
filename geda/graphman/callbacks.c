/* $Id$ */

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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif

#include <gtk/gtk.h>

#include "callbacks.h"
#include "global.h"
#include "graph.h"
#include "interface.h"
#include "libstring.h"
#include "support.h"
#include "value.h"


void
MainNotebookPlotsNewButton_clicked     (GtkButton       *button,
                                        gpointer         user_data)
{

}


void
MainNotebookPlotsModifyButton_clicked  (GtkButton       *button,
                                        gpointer         user_data)
{

}


void
MainNotebookPlotsDeleteButton_clicked  (GtkButton       *button,
                                        gpointer         user_data)
{

}


void
MainNotebookGraphBorderButton_clicked  (GtkButton       *button,
                                        gpointer         user_data)
{
}


void PlotButtonOkButton_clicked(GtkButton *pButton, gpointer pUserData)
{
}


void PlotButtonCancelButton_clicked(GtkButton *pButton, gpointer pUserData)
{
	
}






void MainButtonOk_clicked(GtkButton *pButton, gpointer pUserData)
{
	GraphSave(szFilename);
	gtk_main_quit();
}


void MainButtonCancel_clicked(GtkButton *pButton, gpointer pUserData)
{
	gtk_main_quit();
}


void MainButtonPlot_clicked(GtkButton *pButton, gpointer pUserData)
{
	char *pCommand = NULL;

	pCommand = StringCreate();
	StringCopy(&pCommand, Graph.szFileName);
	StringCat(&pCommand, ".gw");

	execlp("gwave", "-s", pCommand, NULL);
}


gboolean MainDeleteEvent_clicked(GtkWidget *pWidget, GdkEvent *pEvent, gpointer pUserData)
{
	gtk_main_quit();
	return FALSE;
}



