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

#include <gtk/gtk.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "comps.h"
#include "file.h"
#include "install.h"
#include "package.h"
#include "support.h"
#include "summary.h"
#include "xpm.h"



static char szSummary[8192];



int SummaryInitialize(void)
{
	SummaryShow(pWindowMain);

	return SUCCESS;
}


void SummaryPrepare(void)
{
	struct CompsTable_s *pComp;
	int bRequired;
	
	szSummary[0] = 0;
	
	mark_to_be_installed();
	
	sprintf(szSummary + strlen(szSummary), "INSTALLATION SUMMARY\n");
	sprintf(szSummary + strlen(szSummary), "\n");

	/* list components on screen */
	sprintf(szSummary + strlen(szSummary), "List of selected components:\n");
	sprintf(szSummary + strlen(szSummary), "\n");
	for (pComp = pCompsTable; pComp != NULL; pComp = pComp->pNextComp)
	{
		if (pComp == get_component_by_name("SOFTWARE"))
			continue;
		if (strlen(pComp->szFileName) == 0)
			continue;

		if (pComp->iToBeInstalled == PACKAGE_SELECTED)
		{
			sprintf(szSummary + strlen(szSummary), " -\t%s\n", pComp->szFileName);
		}
	}
	sprintf(szSummary + strlen(szSummary), "\n");

	/* list components not selected but required by selected ones */
	bRequired = FALSE;
	for (pComp = pCompsTable; pComp != NULL; pComp = pComp->pNextComp)
	{
		if (pComp->iToBeInstalled == PACKAGE_REQUIRED)
			bRequired = TRUE;
	}
	if (bRequired == TRUE)
	{
		sprintf(szSummary + strlen(szSummary), "List of required components:\n");
		sprintf(szSummary + strlen(szSummary), "\n");
		for (pComp = pCompsTable; pComp != NULL; pComp = pComp->pNextComp)
		{
			if (pComp == get_component_by_name("SOFTWARE"))
				continue;
			if (strlen(pComp->szFileName) == 0)
				continue;

			if (pComp->iToBeInstalled == PACKAGE_REQUIRED)
			{
				sprintf(szSummary + strlen(szSummary), " -\t%s\n", pComp->szFileName);
			}
		}
		sprintf(szSummary + strlen(szSummary), "\n");
	}

	sprintf(szSummary + strlen(szSummary), "Software will be installed into the directory:\n");
	sprintf(szSummary + strlen(szSummary), "\n");
	sprintf(szSummary + strlen(szSummary), "\t%s\n", szInstallDirectory);
	sprintf(szSummary + strlen(szSummary), "\n");
}



void SummaryShow(GtkWidget *pMainWindow)
{
	GdkPixmap *pPixmap;
    GdkBitmap *pMask;
	GtkPixmap *pPixmapSummary;
    GtkStyle *pStyle;
	GtkText *pText;
 	int iResult;
	
	iResult = (strlen(Software.szPictName) == 0)
		? FAILURE
		: FileTest(Software.szPictName);
	pStyle = gtk_widget_get_style(pWindowMain);
	pPixmap = (iResult != SUCCESS)
		? gdk_pixmap_create_from_xpm_d(pWindowMain->window, &pMask, &pStyle->bg[GTK_STATE_NORMAL], (gchar **) XpmDefault)
		: gdk_pixmap_create_from_xpm(pWindowMain->window, &pMask, &pStyle->bg[GTK_STATE_NORMAL], Software.szPictName);
	pPixmapSummary = GTK_PIXMAP(lookup_widget(pMainWindow, "SummaryPixmap"));
	gtk_pixmap_set(pPixmapSummary, pPixmap, pMask);

	
	pText = (GtkText *) lookup_widget(pMainWindow, "SummaryText");
	if (pText == NULL)
	{
		/* TODO: error handling */
	}
	
	gtk_text_set_point(pText, 0);  
	gtk_text_forward_delete(pText, gtk_text_get_length(pText));
	gtk_text_insert(pText, NULL, NULL, NULL, szSummary, strlen(szSummary));
	gtk_text_set_point(pText, 0);
	gtk_widget_show(GTK_WIDGET(pText));
}
