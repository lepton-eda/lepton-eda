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
#include <string.h>
#include "file.h"
#include "log.h"
#include "global.h"
#include "package.h"
#include "setup.h"
#include "start.h"
#include "support.h"
#include "xpm.h"



/*
	functions called at the start and end of application
*/

int StartInitialize(void)
{
	GdkPixmap *pPixmap;
    GdkBitmap *pMask;
    GtkStyle *pStyle;
	GtkWidget *pWidget;
	int iResult;
	char szString[TEXTLEN];
	
	/* display software title */
	pWidget = lookup_widget(pWindowMain, "SoftwareName");
	if (pWidget == NULL)
	{
		Log(LOG_FATAL, LOG_START, "Cannot find widget: 'SoftwareName'");
		return FAILURE;
	}
	strcpy(szString, Software.szName);
	strcat(szString, " version ");
	strcat(szString, Software.szVersion);
	if (strlen(Software.szRelease) != 0)
	{
		strcat(szString, ".");
		strcat(szString, Software.szRelease);
	}
	gtk_label_set_text((GtkLabel *)pWidget, szString);

	/* display software description */
	pWidget = lookup_widget(pWindowMain, "SoftwareDesc2");
	if (pWidget == NULL)
	{
		Log(LOG_FATAL, LOG_START, "Cannot find widget: 'SoftwareDesc2'");
		return FAILURE;
	}
	gtk_label_set_text((GtkLabel *)pWidget, Software.szDesc);

	/* display icon */
	iResult = (strlen(Software.szPictName) == 0)
		? FAILURE
		: FileTest(Software.szPictName);
	pStyle = gtk_widget_get_style(pWindowMain);
	pPixmap = (iResult != SUCCESS)
		? gdk_pixmap_create_from_xpm_d(pWindowMain->window, &pMask, &pStyle->bg[GTK_STATE_NORMAL], (gchar **) XpmDefault)
		: gdk_pixmap_create_from_xpm(pWindowMain->window, &pMask, &pStyle->bg[GTK_STATE_NORMAL], Software.szPictName);
	pWidget = lookup_widget(pWindowMain, "StartPixmap");
	if (pWidget == NULL)
	{
		Log(LOG_FATAL, LOG_START, "Cannot find widget: 'StartPixmap'");
		return FAILURE;
	}
	gtk_pixmap_set((GtkPixmap *)pWidget, pPixmap, pMask);
	gtk_widget_show(pWidget);
	while (g_main_iteration(FALSE));

	StartShow();

	return SUCCESS;
}


int StartRelease(void)
{
	return SUCCESS;
}



/*
	functions called at the displaying / hiding START notebook's page
*/

int StartShow(void)
{
	GdkPixmap *pPixmap;
    GdkBitmap *pMask;
	GtkPixmap *pPixmapStart;
    GtkStyle *pStyle;
 	int iResult;
	
	iResult = (strlen(Software.szPictName) == 0)
		? FAILURE
		: FileTest(Software.szPictName);
	pStyle = gtk_widget_get_style(pWindowMain);
	pPixmap = (iResult != SUCCESS)
		? gdk_pixmap_create_from_xpm_d(pWindowMain->window, &pMask, &pStyle->bg[GTK_STATE_NORMAL], (gchar **) XpmDefault)
		: gdk_pixmap_create_from_xpm(pWindowMain->window, &pMask, &pStyle->bg[GTK_STATE_NORMAL], Software.szPictName);
	pPixmapStart = GTK_PIXMAP(lookup_widget(pWindowMain, "StartPixmap"));
	gtk_pixmap_set(pPixmapStart, pPixmap, pMask);
	setup_buttons(pWindowMain);
	
	return SUCCESS;
}



int StartHide(void)
{
	return SUCCESS;
}
