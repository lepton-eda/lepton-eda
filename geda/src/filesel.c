/*******************************************************************************/
/*                                                                             */
/* gEDA Suite Project Manager                                                  */
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

#ifdef HAVE_CONFIG_H
#include "../config.h"
#endif
#include <gtk/gtk.h>
#include <string.h>
#include "filesel.h"
#include "global.h"



/* file selection phase */
#define FILESEL_GOING        0
#define FILESEL_DONE         1
#define FILESEL_CANCELED     2


/* private functions and variables */
static GtkWidget *pFileSelection;
static char szFileSelection[TEXTLEN];
static int iFileSelectionStatus;
static void FileSelectionOkClicked(GtkButton *pButtonClicked, gpointer pUserData);
static void FileSelectionCancelClicked(GtkButton *pButtonClicked, gpointer pUserData);



int FileSelection(char *szPattern, char *szFileName)
{
	GtkWidget *pButtonOk, *pButtonCancel;
	char szMask[TEXTLEN];
	
	/* prepare variables */
	strcpy(szMask, "*.");
	strcat(szMask, szPattern);
	

	/* create main widget */
	pFileSelection = gtk_file_selection_new("Select file");
	gtk_object_set_data(GTK_OBJECT(pFileSelection), "FileSelectionWindow", pFileSelection);
	gtk_container_set_border_width(GTK_CONTAINER(pFileSelection), 10);
	gtk_window_set_position(GTK_WINDOW(pFileSelection), GTK_WIN_POS_CENTER);
	gtk_window_set_modal(GTK_WINDOW(pFileSelection), TRUE);
	gtk_window_set_policy(GTK_WINDOW(pFileSelection), FALSE, FALSE, FALSE);
	gtk_window_set_wmclass(GTK_WINDOW(pFileSelection), "*.prj", "");
	gtk_file_selection_hide_fileop_buttons(GTK_FILE_SELECTION(pFileSelection));
	gtk_window_set_transient_for(GTK_WINDOW(pFileSelection), GTK_WINDOW(pWindowMain));
	gtk_widget_show(pFileSelection);
	
	/* create Ok button widget */
	pButtonOk = GTK_FILE_SELECTION(pFileSelection)->ok_button;
	gtk_object_set_data(GTK_OBJECT(pFileSelection), "FileSelectionButtonOk", pButtonOk);
	gtk_widget_show(pButtonOk);
	GTK_WIDGET_SET_FLAGS(pButtonOk, GTK_CAN_DEFAULT);

	/* create Cancel button widget */
	pButtonCancel = GTK_FILE_SELECTION(pFileSelection)->cancel_button;
	gtk_object_set_data(GTK_OBJECT(pFileSelection), "FileSelectionButtonCancel", pButtonCancel);
	gtk_widget_show(pButtonCancel);
	GTK_WIDGET_SET_FLAGS(pButtonCancel, GTK_CAN_DEFAULT);

	/* connect signals */
	gtk_signal_connect(GTK_OBJECT(pFileSelection), "delete_event", GTK_SIGNAL_FUNC(FileSelectionCancelClicked), NULL);
	gtk_signal_connect(GTK_OBJECT(pButtonOk), "clicked", GTK_SIGNAL_FUNC(FileSelectionOkClicked), NULL);
	gtk_signal_connect(GTK_OBJECT(pButtonCancel), "clicked", GTK_SIGNAL_FUNC(FileSelectionCancelClicked), NULL);

	/* wait until selection is done */
	iFileSelectionStatus = FILESEL_GOING;
	while (iFileSelectionStatus == FILESEL_GOING)
		while (g_main_iteration(FALSE)) ;
	strcpy(szFileName, szFileSelection);
	
	/* hide file selection window */
	gtk_widget_destroy(GTK_WIDGET(pFileSelection));
	while (g_main_iteration(FALSE)) ;
	
	return (iFileSelectionStatus == FILESEL_DONE) 
		? SUCCESS
		: FAILURE;
}



/*
	Private functions
*/

static void FileSelectionOkClicked(GtkButton *pButtonClicked, gpointer pUserData)
{
	strcpy(szFileSelection, gtk_file_selection_get_filename((GtkFileSelection *) pFileSelection));
	iFileSelectionStatus = FILESEL_DONE;
}


static void FileSelectionCancelClicked(GtkButton *pButtonClicked, gpointer pUserData)
{
	iFileSelectionStatus = FILESEL_CANCELED;
}
