/*******************************************************************************/
/*                                                                             */
/* Setup - version 0.2.1                                                       */
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

#include <gtk/gtk.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"
#include "dirs.h"
#include "file.h"
#include "package.h"
#include "support.h"
#include "xpm.h"



int iDirsType = DIRS_LOCAL;



void dirs_local(GtkWidget *Entry)
{
	char *szHomeDirectory;
	
	szHomeDirectory = getenv("HOME");
	strcpy(szInstallDirectory, szHomeDirectory);
	strcat(szInstallDirectory, "/");
	strcat(szInstallDirectory, Software.szDirname);
	
	gtk_entry_set_text(GTK_ENTRY(Entry), szInstallDirectory);
	gtk_entry_set_editable(GTK_ENTRY(Entry), FALSE);
	gtk_widget_set_sensitive(Entry, FALSE);
	
	iDirsType = DIRS_LOCAL;
}



void dirs_global(GtkWidget *Entry)
{
	strcpy(szInstallDirectory, "/usr/local/");
	strcat(szInstallDirectory, Software.szDirname);
	
	gtk_entry_set_text(GTK_ENTRY(Entry), szInstallDirectory);
	gtk_entry_set_editable(GTK_ENTRY(Entry), FALSE);
	gtk_widget_set_sensitive(Entry, FALSE);

	iDirsType = DIRS_GLOBAL;
}



void dirs_custom(GtkWidget *Entry)
{
	static char szCustomDirectory[TEXTLEN];
	static int iDone = 0;
	
	if (!iDone)
	{
		strcpy(szCustomDirectory, "/usr/local/");
		strcat(szCustomDirectory, Software.szDirname);
		iDone = 1;
	}
	
	strcpy(szInstallDirectory, szCustomDirectory);
	
	gtk_entry_set_text(GTK_ENTRY(Entry), szInstallDirectory);
	gtk_entry_set_editable(GTK_ENTRY(Entry), TRUE);
	gtk_widget_set_sensitive(Entry, TRUE);
	
	iDirsType = DIRS_CUSTOM;
}



int DirectoryInitialize(void)
{
	DirectoryShow(pWindowMain);
	
	return SUCCESS;
}



void DirectoryRelease(GtkWidget *pMainWindow)
{
}



void DirectoryShow(GtkWidget *pMainWindow)
{
	GdkPixmap *pPixmap;
    GdkBitmap *pMask;
	GtkPixmap *pPixmapDirectory;
    GtkStyle *pStyle;
	int iResult;
 	
	iResult = (strlen(Software.szPictName) == 0)
		? FAILURE
		: FileTest(Software.szPictName);
	pStyle = gtk_widget_get_style(pWindowMain);
	pPixmap = (iResult != SUCCESS)
		? gdk_pixmap_create_from_xpm_d(pWindowMain->window, &pMask, &pStyle->bg[GTK_STATE_NORMAL], (gchar **) XpmDefault)
		: gdk_pixmap_create_from_xpm(pWindowMain->window, &pMask, &pStyle->bg[GTK_STATE_NORMAL], Software.szPictName);
	pPixmapDirectory = GTK_PIXMAP(lookup_widget(pMainWindow, "DirectoryPixmap"));
	gtk_pixmap_set(pPixmapDirectory, pPixmap, pMask);
}



void DirectoryHide(GtkWidget *pMainWindow)
{
}
