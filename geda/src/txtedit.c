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

#include <gtk/gtk.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "doc.h"
#include "global.h"
#include "window.h"



static void EditChanged(GtkEditable *pEditable, gpointer pUserData);



struct Edit_s
{
	char *szPath;
	GtkWidget *pParent;
	GtkText *pText;
	int bChanged;
	struct Edit_s *pNext;
};
static struct Edit_s *pEditList = NULL;



void EditOpen(const char *szPath)
{
	GdkFont *pFixedFont;
	FILE *hFile;
	struct Doc_s *pDoc;
	struct Edit_s *pEdit, *pPtr;
	int i;
	char szText[2];

	for (pEdit = pEditList; pEdit != NULL; pEdit = pEdit->pNext)
	{
		if (strcmp(pEdit->szPath, szPath) == 0)
			break;
	}
	if (pEdit != NULL) /* file already opened */
	{
		gmanager_window_select(szPath);
		return;
	}
	
	pEdit = (struct Edit_s *) malloc(sizeof(struct Edit_s));
	if (pEdit == NULL)
		return;
	pEdit->pNext = NULL;
	
	pEdit->pParent = gmanager_window_new(szPath);
	if (pEdit->pParent == NULL)
	{
		free(pEdit);
		return;
	}
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(pEdit->pParent), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

	pFixedFont = gdk_font_load("-misc-fixed-medium-r-*-*-*-120-*-*-*-*-*-*");

	pEdit->pText = GTK_TEXT(gtk_text_new(NULL, NULL));
	gtk_widget_show(GTK_WIDGET(pEdit->pText));
	gtk_container_add(GTK_CONTAINER (pEdit->pParent), GTK_WIDGET(pEdit->pText));
	gtk_text_set_editable(pEdit->pText, TRUE);
	gtk_text_set_word_wrap(pEdit->pText, TRUE);
	gtk_text_set_line_wrap(pEdit->pText, TRUE);

	pEdit->szPath = (char *) malloc(strlen(szPath) + 1);
	if (pEdit->szPath == NULL)
	{
		gtk_widget_destroy(GTK_WIDGET(pEdit->pText));
		free(pEdit);
		return;
	}
	strcpy(pEdit->szPath, szPath);
	
	if (pEditList == NULL)
	{
		pEditList = pEdit;
	}
	else
	{
		for (pPtr = pEditList; pPtr->pNext != NULL; pPtr = pPtr->pNext)
			;
		pPtr->pNext = pEdit;
	}

	hFile = fopen(pEdit->szPath, "r");
	if (hFile == NULL)
	{
		gtk_widget_destroy(GTK_WIDGET(pEdit->pText));
		free(pEdit->szPath);
		return;
	}
	
	while (!feof(hFile)) 
	{
		i = fgetc(hFile);
		if (i < 0)
			break;

		szText[0] = (char) i;
		szText[1] = 0;
		gtk_text_insert(pEdit->pText, pFixedFont, NULL, NULL, szText, strlen(szText));
	}
	
	fclose(hFile);
	
	for (pDoc = pDocList; pDoc != NULL; pDoc = pDoc->pNext)
	{
		if (strcmp(szPath, pDoc->szFileName) == 0)
			break;
	}
	if (pDoc == NULL)
		return;
	pDoc->bChanged = FALSE;
	ProjectTitle();
	gtk_signal_connect(GTK_OBJECT(pEdit->pText), "changed", GTK_SIGNAL_FUNC(EditChanged), pEdit->szPath);
}



void EditSave(const char *szPath)
{
	FILE *hFile;
	struct Doc_s *pDoc;
	struct Edit_s *pEdit;
	
	for (pEdit = pEditList; pEdit->pNext != NULL; pEdit = pEdit->pNext)
		if (strcmp(szPath, pEdit->szPath) == 0)
			break;
	if (pEdit == NULL)
		return;
	
	hFile = fopen(pEdit->szPath, "w");
	if (hFile == NULL)
		return;

	fprintf(hFile, "%s", gtk_editable_get_chars(GTK_EDITABLE(pEdit->pText), 0, -1));
	
	fclose(hFile);
	
	for (pDoc = pDocList; pDoc != NULL; pDoc = pDoc->pNext)
	{
		if (strcmp(szPath, pDoc->szFileName) == 0)
			break;
	}
	if (pDoc == NULL)
		return;
	pDoc->bChanged = FALSE;
	ProjectTitle();
}



void EditPrint(const char *szPath)
{
}



void EditClose(const char *szPath)
{
	struct Edit_s *pEdit, *pPtr;

	for (pEdit = pEditList; pEdit != NULL; pEdit = pEdit->pNext)
	{
		if (strcmp(szPath, pEdit->szPath) == 0)
			break;
	}
	if (pEdit == NULL)
		return;

	gtk_signal_disconnect_by_func(GTK_OBJECT(pEdit->pText), GTK_SIGNAL_FUNC(EditChanged), pEdit->szPath);
	gmanager_window_remove(pEdit->pParent);
	free(pEdit->szPath);

	if (pEdit == pEditList)
	{
		pEditList = (pEdit->pNext == NULL)
			? NULL
			: pEdit->pNext;
	}
	else
	{
		for (pPtr = pEditList; pPtr != NULL; pPtr = pPtr->pNext)
			if (pPtr->pNext == pEdit)
				break;
		pPtr->pNext = pEdit->pNext;
	}

	free((void *) pEdit);
}



static void EditChanged(GtkEditable *pEditable, gpointer pUserData)
{
	struct Doc_s *pDoc;

	for (pDoc = pDocList; pDoc != NULL; pDoc = pDoc->pNext)
	{
		if (strcmp(pUserData, pDoc->szFileName) == 0)
			break;
	}
	if (pDoc == NULL)
		return;

	pDoc->bChanged = TRUE;
	ProjectTitle();
}
