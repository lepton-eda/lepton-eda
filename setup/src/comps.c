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
#include "msgbox.h"
#include "support.h"
#include "comps.h"
#include "package.h"
#include "xpm.h"



int bShowIconsHidden = FALSE;
int bCompsAlwaysMarkFailed = FALSE;



static char cSelectFlag = 1;

/* icons for marking selection state */
static const char *PixmapNoneXpm[] = XPM_ICON_NONE;
static const char *PixmapPartXpm[] = XPM_ICON_PART;
static const char *PixmapFullXpm[] = XPM_ICON_FULL;
static const char *PixmapPartFailedXpm[] = XPM_ICON_PART_FAILED;
static const char *PixmapFullFailedXpm[] = XPM_ICON_FULL_FAILED;
static const char *PixmapReqXpm[] = XPM_ICON_REQUIRED;
static const char *PixmapReqFailedXpm[] = XPM_ICON_REQUIRED_FAILED;
static GdkPixmap *pGdkPixmapNone = NULL;
static GdkPixmap *pGdkPixmapPart = NULL;
static GdkPixmap *pGdkPixmapFull = NULL;
static GdkPixmap *pGdkPixmapPartFailed = NULL;
static GdkPixmap *pGdkPixmapFullFailed = NULL;
static GdkPixmap *pGdkPixmapReq = NULL;
static GdkPixmap *pGdkPixmapReqFailed = NULL;

/* static functions */
static int ShowDesc(struct CompsTable_s *pPkg);
static int ShowIcons(const char *szParent);
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
	pGdkPixmapPartFailed = gdk_pixmap_create_from_xpm_d(
		pWindowMain->window,
		&pGdkMask,
		&pStyle->bg[GTK_STATE_NORMAL],
		(gchar **)PixmapPartFailedXpm
		);
	pGdkPixmapFullFailed = gdk_pixmap_create_from_xpm_d(
		pWindowMain->window,
		&pGdkMask,
		&pStyle->bg[GTK_STATE_NORMAL],
		(gchar **)PixmapFullFailedXpm
		 );
	pGdkPixmapReq = gdk_pixmap_create_from_xpm_d(
		pWindowMain->window,
		&pGdkMask,
		&pStyle->bg[GTK_STATE_NORMAL],
		(gchar **)PixmapReqXpm
		);
	pGdkPixmapReqFailed = gdk_pixmap_create_from_xpm_d(
		pWindowMain->window,
		&pGdkMask,
		&pStyle->bg[GTK_STATE_NORMAL],
		(gchar **)PixmapReqFailedXpm
		 );

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
	ShowIcons("");
	gtk_widget_show(GTK_WIDGET(pTree));

	ShowDesc(pCompsTable);

	return SUCCESS;
}


int ComponentsRelease(void)
{
		return SUCCESS;
}



void CompsPrepare(void)
{
	struct CompsTable_s *pComp;

	/* check what has been installed and what tgz's exist */
	MsgBox(
		GTK_WINDOW(pWindowMain),
		"Please wait ...",
		"Checking dependencies ...",
		0
		);
	for (pComp = pCompsTable; pComp != NULL; pComp = pComp->pNextComp)
	{
		pComp->bCanBeInstalled = (!strlen(PackageWhatIsMissing(pComp->szCodeName)))
			? TRUE
			: FALSE;

		while (g_main_iteration(FALSE)) ;
	}
	MsgBoxDestroy();

	/* unmark components that cannot be installed */
	for (pComp = pCompsTable; pComp != NULL; pComp = pComp->pNextComp)
	{
		if (!pComp->bCanBeInstalled)
			pComp->iToBeInstalled = PACKAGE_IGNORED;
	}

	ShowIcons("");
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

	pMaster->iToBeInstalled = (iFlag == MARK_SELECT)
		? PACKAGE_SELECTED
		: PACKAGE_IGNORED;

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
		if (!pComp->bToBeDisplayed && !bShowIconsHidden)
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



static int ShowDesc(struct CompsTable_s *pPkg)
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
	ShowIcons("");

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



static int ShowIcons(const char *szParent)
{
	struct CompsTable_s *pComp, *pParent;
	GtkCTree *pTree;
	int iFoundSelected = FALSE, iFoundUnselected = FALSE, iFoundChildren = FALSE, iFoundFailed = FALSE, i, j;
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
		if (!pComp->bToBeDisplayed && !bShowIconsHidden)
			continue;
		
		/* determine icons for all children */
		ShowIcons(pComp->szCodeName);
		if (pComp->Icon == pGdkPixmapFull
			|| pComp->Icon == pGdkPixmapFullFailed
			|| pComp->Icon == pGdkPixmapReq
			|| pComp->Icon == pGdkPixmapReqFailed
			)
			iFoundSelected = TRUE;
		else if (pComp->Icon == pGdkPixmapNone
			|| pComp->Icon == pGdkPixmapPart
			|| pComp->Icon == pGdkPixmapPartFailed
			)
			iFoundUnselected = TRUE;
		if (pComp->Icon == pGdkPixmapFullFailed
			|| pComp->Icon == pGdkPixmapPartFailed
			|| pComp->Icon == pGdkPixmapReqFailed
			)
			iFoundFailed = TRUE;
	}

	/* determine icon */
	if (iFoundChildren == TRUE)
	{
		if (iFoundSelected == TRUE && iFoundUnselected == TRUE)
		{
			pParent->Icon = (iFoundFailed == TRUE)
				? pGdkPixmapPartFailed
				: pGdkPixmapPart;
			pParent->iToBeInstalled = PACKAGE_IGNORED;
		}
		else if (iFoundSelected == TRUE && iFoundUnselected == FALSE)
		{
			pParent->Icon = (iFoundFailed == TRUE)
				? pGdkPixmapFullFailed
				: pGdkPixmapFull;
			pParent->iToBeInstalled = PACKAGE_SELECTED;
		}
		else if (iFoundSelected == FALSE && iFoundUnselected == TRUE)
		{
			pParent->Icon = pGdkPixmapNone;
			pParent->iToBeInstalled = PACKAGE_IGNORED;
		}
	}
	else
	{
		mark_to_be_installed();

		if (pParent->iToBeInstalled == PACKAGE_SELECTED)
		{
			pParent->Icon =  (pParent->bCanBeInstalled == TRUE)
				? pGdkPixmapFull
				: pGdkPixmapFullFailed;
		}
		else if (pParent->iToBeInstalled == PACKAGE_REQUIRED)
		{
			pParent->Icon =  (pParent->bCanBeInstalled == TRUE)
				? pGdkPixmapReq
				: pGdkPixmapReqFailed;
		}
		else
			pParent->Icon = pGdkPixmapNone;
	}
	if (pParent != get_component_by_name("SOFTWARE"))
		gtk_ctree_node_set_pixtext(pTree, pParent->pNode, 0, pParent->szName, 1, pParent->Icon, NULL);
	
	/* unselect all nodes */
	cSelectFlag = 0;
	for (pComp = pCompsTable; pComp != NULL; pComp = pComp->pNextComp)
	{
		if (pComp->pNode == NULL)
			continue;
		
		gtk_ctree_unselect(pTree, pComp->pNode);
	}
	cSelectFlag = 1;
	gtk_widget_show((GtkWidget *) pTree);
	
	return SUCCESS;
}



/*
	Module COMPONENTS - callbacks
*/

void on_ComponentTree_tree_select_row(GtkCTree *ctree, GList *node, gint column, gpointer user_data)
{
	struct CompsTable_s *pComp;
	int iResult = MSGBOX_NO;
	char szMessage[TEXTLEN], *szMissingFile;

	if (cSelectFlag == 0)
		return;

	/* search component */
	for (pComp = pCompsTable; pComp != NULL; pComp = pComp->pNextComp)
	{
		if (GTK_CTREE_NODE(node) == pComp->pNode)
			break;
	}
	if (pComp == NULL)
	{
		/* TODO: error handling */
	}

	/* toggle installation flag */
	if (pComp->iToBeInstalled == PACKAGE_SELECTED)
	{
		pComp->iToBeInstalled = PACKAGE_IGNORED;
		mark_components(pComp, MARK_UNSELECT);
	}
	else
	{
		if (!pComp->bCanBeInstalled && !bCompsAlwaysMarkFailed)
		{
			szMissingFile = PackageWhatIsMissing(pComp->szCodeName);

			sprintf(
				szMessage,
				"%s cannot be installed.\nMissing file: %s !\n\nForcing installation will cause errors. Continue ?",
				pComp->szName,
				szMissingFile
				);
			iResult = MsgBox(
				GTK_WINDOW(pWindowMain),
				"Error !",
				szMessage,
				MSGBOX_ERROR | MSGBOX_YES | MSGBOX_ALWAYSYES | MSGBOX_NOD
				);
			if (iResult == MSGBOX_ALWAYSYES)
				bCompsAlwaysMarkFailed = TRUE;
		}
		/* TODO: iResult should be implicitly initialized in any case */
		if (pComp->bCanBeInstalled == TRUE || iResult == MSGBOX_YES || bCompsAlwaysMarkFailed)
		{
			pComp->iToBeInstalled = PACKAGE_SELECTED;
			mark_components(pComp, MARK_SELECT);
		}
	}

	/* activate display changes */
	iResult = ShowDesc(pComp);
	if (iResult != SUCCESS)
	{
		Log(LOG_FATAL, LOG_MAIN, "Cannot show COMPONENTS page");
		return;
	}
}



void on_ComponentTree_tree_unselect_row(GtkCTree *ctree, GList *node, gint column, gpointer user_data)
{
	on_ComponentTree_tree_select_row(ctree, node, column, user_data);
}



void on_ComponentTree_tree_expand(GtkCTree *ctree, GList *node, gpointer user_data)
{
	ShowIcons("");
}


void on_ComponentTree_tree_collapse(GtkCTree *ctree, GList *node, gpointer user_data)
{
	ShowIcons("");
}
