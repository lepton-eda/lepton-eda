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

#include <ctype.h>
#include <gtk/gtk.h>
#include <stdio.h>
#include <string.h>
#include "file.h"
#include "install.h"
#include "log.h"
#include "support.h"
#include "comps.h"
#include "package.h"
#include "xpm.h"



/* list of components and their properties */
//struct CompsTable_s *pCompsTable = NULL;

/* icons for marking selection state */
static const char *PixmapNoneXpm[] = XPM_ICON_NONE;
static const char *PixmapPartXpm[] = XPM_ICON_PART;
static const char *PixmapFullXpm[] = XPM_ICON_FULL;
static GdkPixmap *pGdkPixmapNone = NULL;
static GdkPixmap *pGdkPixmapPart = NULL;
static GdkPixmap *pGdkPixmapFull = NULL;
//, *MaskOpened = NULL, *MaskClosed = NULL;



extern char cSelectFlag;



/* static functions */
static int BuildTree(GtkCTree *pTree, char *szParent);



/*
	Initializing and releasing resources used by COMPONENTS module
*/

int ComponentsInitialize(void)
{
    GtkStyle *pStyle;
	GtkCTree *pTree;
	GtkPixmap *pPixmapComponents;
	GdkBitmap *pGdkMask;
	GdkPixmap *pGdkPixmap;
	int iResult;
	
	/* prepare pixmaps for marking selection */
	pStyle = gtk_widget_get_style(pWindowMain);
    pGdkPixmapNone = gdk_pixmap_create_from_xpm_d(
		pWindowMain->window, 
		&pGdkMask, 
		&pStyle->bg[GTK_STATE_NORMAL], 
		(gchar **)PixmapNoneXpm
		);
    pGdkPixmapPart = gdk_pixmap_create_from_xpm_d(
		pWindowMain->window,  
		&pGdkMask,
		&pStyle->bg[GTK_STATE_NORMAL],
        (gchar **)PixmapPartXpm
		);
    pGdkPixmapFull = gdk_pixmap_create_from_xpm_d(
		pWindowMain->window,
		&pGdkMask,
		&pStyle->bg[GTK_STATE_NORMAL],
		(gchar **)PixmapFullXpm );
	
	/* show a picture on a components page */
	iResult = (strlen(Software.szPictName) == 0)
		? FAILURE
		: FileTest(Software.szPictName);
	pStyle = gtk_widget_get_style(pWindowMain);
	pGdkPixmap = (iResult != SUCCESS)
		? gdk_pixmap_create_from_xpm_d(pWindowMain->window, &pGdkMask, &pStyle->bg[GTK_STATE_NORMAL], (gchar **) XpmDefault)
		: gdk_pixmap_create_from_xpm(pWindowMain->window, &pGdkMask, &pStyle->bg[GTK_STATE_NORMAL], Software.szPictName);
	pPixmapComponents = GTK_PIXMAP(lookup_widget(pWindowMain, "ComponentsPixmap"));
	if (pPixmapComponents == NULL)
	{
		/* TODO: make error handling */
	}
	gtk_pixmap_set(pPixmapComponents, pGdkPixmap, pGdkMask);

	/* build and show a tree */
	pTree = GTK_CTREE(lookup_widget(pWindowMain, "ComponentTree"));
	if (pTree == NULL)
	{
		/* TODO: make error handling */
	}
	BuildTree(pTree, "");
/* here was set_pixmaps() */
	CompsShow("");
	gtk_widget_show(GTK_WIDGET(pTree));
	
	ComponentsPrepare(pCompsTable);
	
	return SUCCESS;
}


int ComponentsRelease(void)
{
		return SUCCESS;
}



int ComponentsPrepare(struct CompsTable_s *pPkg)
{
	GtkWidget *pWidget;
	char szString[TEXTLEN];
	
	strcpy(szString, "");
	
	/* set pixmaps */
	pWidget = lookup_widget(pWindowMain, "ComponentTree");
	if (pWidget == NULL)
	{
		Log(LOG_FATAL, LOG_START, "Cannot find widget: 'ComponentTree'");
		return FAILURE;
	}
	/* was: set_pixmaps() */
	CompsShow("");
	
	/* prepare text to show */
	if (pPkg != NULL)
	{
#if 0
		strcpy(szString, pPkg->szCodeName);
		strcat(szString, " ");
		strcat(szString, pPkg->szVersion);
		if (strlen(pPkg->szRelease) != 0)
		{
			strcat(szString, ".");
			strcat(szString, pPkg->szRelease);
		}
		strcat(szString, ":\n");
		strcat(szString, pPkg->szDesc);
#else
		strcpy(szString, pPkg->szDesc);
#endif
	}
	
	/* show text */
	pWidget = lookup_widget(pWindowMain, "ComponentsDesc");
	if (pWidget == NULL)
	{
		Log(LOG_FATAL, LOG_START, "Cannot find widget: 'ComponentsDesc'");
		return FAILURE;
	}
	gtk_label_set_text((GtkLabel *)pWidget, szString);
	
	return SUCCESS;
}


int ComponentsHide(void)
{
	return SUCCESS;
}



/*
	TODO: correct it
*/

void mark_components(struct CompsTable_s *pMaster, int iFlag)
{
	struct CompsTable_s *pComp;
		
	pMaster->bToBeInstalled = (iFlag == MARK_SELECT)
		? TRUE
		: FALSE;
	
	if (iFlag == MARK_SELECT)
	{
		for (pComp = pCompsTable; pComp != NULL; pComp = pComp->pNextComp)
			if (pComp->pCompParent == pMaster)
				mark_components(pComp, MARK_SELECT);
	}
	
	else
	{
		for (pComp = pCompsTable; pComp != NULL; pComp = pComp->pNextComp)
			if (pComp->pCompParent == pMaster)
				mark_components(pComp, MARK_UNSELECT);
	}
}



int is_expanded(GtkCTree *pTree, GtkCTreeNode *pNode)
{
	char **text = NULL;
	guint8 *spacing = NULL;
	GdkPixmap **pixmap_closed = NULL;
	GdkBitmap **mask_closed = NULL;
    GdkPixmap **pixmap_opened = NULL;
    GdkBitmap **mask_opened = NULL;
	gboolean *is_leaf = NULL;
	gboolean *expanded = NULL;
	
	gtk_ctree_get_node_info(
		pTree, 
		pNode, 
        text,
        spacing,
        pixmap_closed,
        mask_closed,
        pixmap_opened,
        mask_opened,
        is_leaf,
        expanded);
		
	return *expanded;
}


static int BuildTree(GtkCTree *pTree, char *szParent)
{
	struct CompsTable_s *pComp, *pParent;
	GtkCTreeNode *Sibling = NULL;
	GdkBitmap *pGdkMask = NULL;
	int i, j;
	char szPkgList[TEXTLEN], szCodeName[TEXTLEN], szValue[TEXTLEN], szMessage[TEXTLEN];

	/* get package list */
	pParent = (strlen(szParent) != 0)
		? get_component_by_name(szParent)
		: get_component_by_name("SOFTWARE");
	strcpy(szPkgList, (strlen(szParent) == 0)
		? Software.szPackage
		: pParent->szTopLevel
		);
	
	/* display subsequent components */
	for (i = strlen(szPkgList) - 1; i >= 0; i --)
	{
		/* get component name */
		for (; i >= 0 && isspace(szPkgList[i]); i --)
			;
		for (j = 0; i >= 0 && !isspace(szPkgList[i]); i --)
			szValue[j ++] = szPkgList[i];
		szValue[j] = 0;
		if (strlen(szValue) == 0)
			continue;
		for (j = 0; j < strlen(szValue); j ++)
			szCodeName[strlen(szValue) - j - 1] = szValue[j];
		szCodeName[strlen(szValue)] = 0;

		/* look for the component description */
		pComp = get_component_by_name(szCodeName);
		if (pComp == NULL)
		{
			sprintf(szMessage, "Cannot find component '%s'", szCodeName);
			Log(LOG_ERROR, LOG_MODULE_COMPS, szMessage);
			return FAILURE;
		}
		
		/* omit not displayed components */
		if (pComp->bToBeDisplayed != TRUE)
			continue;
		
		/* show component */
		pComp->pNode = gtk_ctree_insert_node(
			pTree,
			(strlen(szParent) == 0) ? NULL : pParent->pNode,
			Sibling,
			pComp->szNode,
			1,
			pComp->Icon,
			pGdkMask,
			pComp->Icon,
			pGdkMask,
			FALSE,
			FALSE
			);
		
		Sibling = pComp->pNode;
		pComp->pCompParent = pParent;
		
		/* build tree treating the component as parent */
		BuildTree(pTree, pComp->szCodeName);
	}
	
	return SUCCESS;
}



int CompsShow(char *szParent)
{
	struct CompsTable_s *pComp, *pParent;
	GtkCTree *pTree;
	int iFoundSelected = FALSE, iFoundUnselected = FALSE, iFoundChildren = FALSE, i, j;
	char szPkgList[TEXTLEN], szCodeName[TEXTLEN], szValue[TEXTLEN], szMessage[TEXTLEN];

	/* look for the component tree */
	pTree = (GtkCTree *) lookup_widget(pWindowMain, "ComponentTree");
	if (pTree == NULL)
	{
		Log(LOG_FATAL, LOG_MODULE_COMPS, "Cannot find widget 'ComponentTree'");
		return FAILURE;
	}

	/* get package list */
	pParent = (strlen(szParent) != 0)
		? get_component_by_name(szParent)
		: get_component_by_name("SOFTWARE");
	strcpy(szPkgList, (strlen(szParent) == 0)
		? Software.szPackage
		: pParent->szTopLevel
		);
	
	/* set pixmap for the subsequent components */
	for (i = strlen(szPkgList) - 1; i >= 0; i --)
	{
		/* one or more children found */
		iFoundChildren = TRUE;
		
		/* get component name */
		for (; i >= 0 && isspace(szPkgList[i]); i --)
			;
		for (j = 0; i >= 0 && !isspace(szPkgList[i]); i --)
			szValue[j ++] = szPkgList[i];
		szValue[j] = 0;
		if (strlen(szValue) == 0)
			continue;
		for (j = 0; j < strlen(szValue); j ++)
			szCodeName[strlen(szValue) - j - 1] = szValue[j];
		szCodeName[strlen(szValue)] = 0;

		/* look for the component description */
		pComp = get_component_by_name(szCodeName);
		if (pComp == NULL)
		{
			sprintf(szMessage, "Cannot find component '%s'", szCodeName);
			Log(LOG_ERROR, LOG_MODULE_COMPS, szMessage);
			return FAILURE;
		}
		
		/* omit not displayed components */
		if (pComp->bToBeDisplayed != TRUE)
			continue;
		
		/* determine icons for all children */
		CompsShow(pComp->szCodeName);
		if (pComp->Icon == pGdkPixmapFull)
			iFoundSelected = TRUE;
		else if (pComp->Icon == pGdkPixmapNone || pComp->Icon == pGdkPixmapPart)
			iFoundUnselected = TRUE;
	}

	/* determine icon */
	if (iFoundChildren == TRUE)
	{
		if (iFoundSelected == TRUE && iFoundUnselected == TRUE)
		{
			pParent->Icon = pGdkPixmapPart;
			pParent->bToBeInstalled = FALSE;
		}
		else if (iFoundSelected == TRUE && iFoundUnselected == FALSE)
		{
			pParent->Icon = pGdkPixmapFull;
			pParent->bToBeInstalled = TRUE;
		}
		else if (iFoundSelected == FALSE && iFoundUnselected == TRUE)
		{
			pParent->Icon = pGdkPixmapNone;
			pParent->bToBeInstalled = FALSE;
		}
	}
	else
	{
		if (pParent->bToBeInstalled == TRUE)
			pParent->Icon = pGdkPixmapFull;
		else
			pParent->Icon = pGdkPixmapNone;
	}
	if (pParent != get_component_by_name("SOFTWARE"))
		gtk_ctree_node_set_pixtext(pTree, pParent->pNode, 0, pParent->szName, 1, pParent->Icon, NULL);
	
	/* unselect all nodes */
	cSelectFlag = 0;
	for (pComp = pCompsTable; pComp != NULL; pComp = pComp->pNextComp)
	{
		if (pComp->bToBeDisplayed == FALSE)
			continue;
		if (pComp->pNode == NULL)
			continue;
		
		gtk_ctree_unselect(pTree, pComp->pNode);
	}
	cSelectFlag = 1;
	gtk_widget_show((GtkWidget *) pTree);
	
	return SUCCESS;
}



