/* $Id$ */

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
#include "config.h"
#endif

#include <gtk/gtk.h>

#ifdef HAVE_STRING_H  
#include <string.h>
#endif

#include "filechoose.h"
#include "global.h"



/* file choose phase */
#define FILECHOOSE_GOING        0
#define FILECHOOSE_DONE         1
#define FILECHOOSE_CANCELED     2


/* private functions and variables */
static GtkWidget *pFileChoose;
static char szFileChoose[TEXTLEN];
static int iFileChooseStatus;

int FileChoose(char *szPattern, char *szFileName)
{
	//GtkWidget *pButtonOk, *pButtonCancel;
	char szMask[TEXTLEN];
	
	/* prepare variables */
	strcpy(szMask, "*.");
	strcat(szMask, szPattern);

	/* create main widget */
	pFileChoose = gtk_file_chooser_dialog_new(
		"Choose file",
		NULL,
		GTK_FILE_CHOOSER_ACTION_OPEN,
		GTK_STOCK_CANCEL,
		GTK_RESPONSE_CANCEL,
		GTK_STOCK_OPEN,
		GTK_RESPONSE_ACCEPT,
		NULL
	);
	gtk_object_set_data(GTK_OBJECT(pFileChoose), "FileChooseWindow", pFileChoose);
	gtk_container_set_border_width(GTK_CONTAINER(pFileChoose), 10);
	gtk_window_set_position(GTK_WINDOW(pFileChoose), GTK_WIN_POS_CENTER);
	gtk_window_set_modal(GTK_WINDOW(pFileChoose), TRUE);
	gtk_window_set_policy(GTK_WINDOW(pFileChoose), FALSE, FALSE, FALSE);
	gtk_window_set_wmclass(GTK_WINDOW(pFileChoose), "*.prj", "");
	gtk_window_set_transient_for(GTK_WINDOW(pFileChoose), GTK_WINDOW(pWindowMain));
	gtk_widget_show(pFileChoose);

	gtk_widget_show(pFileChoose);
	if (gtk_dialog_run(GTK_DIALOG (pFileChoose)) == GTK_RESPONSE_ACCEPT) {
		char *filename;
		filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (pFileChoose));
		strcpy(szFileName, filename);
		strcpy(szFileChoose, filename);
		iFileChooseStatus = FILECHOOSE_DONE;
		g_free (filename);
	} else {
		iFileChooseStatus = FILECHOOSE_CANCELED;
	}
	gtk_widget_destroy (pFileChoose);
	
	return (iFileChooseStatus == FILECHOOSE_DONE) 
		? SUCCESS
		: FAILURE;
}
