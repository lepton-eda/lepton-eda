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
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "doc.h"
#include "filesel.h"
#include "filetool.h"
#include "global.h"
#include "m_action.h"
#include "m_project.h"
#include "msgbox.h"
#include "project.h"
#include "support.h"



/* private functions */
void ProjectWidgetsHide(void);
void ProjectWidgetsShow(void);



/*
	Project initialization
*/

void ProjectInitialize(void)
{
	char szDir[TEXTLEN];
	
	/* change current directory to $HOME */
	strcpy(szDir, getenv("HOME"));
	chdir(szDir);
	
	/* initialize project properties */
	strcpy(Project.szName, "");
	strcpy(Project.szExt, "");
	strcpy(Project.szDir, "");
	Project.bChanged = FALSE;
	ProjectWidgetsHide();
}



/*
	Menu PROJECT handlers
	(New, Open, Save, Close, Exit)
*/

void MenuProjectNew_Activation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	int iResult;
	char szProjectFileName[TEXTLEN];
	
	/* firstly ask for name of a new project */
	strcpy(szProjectFileName, "unnamed.prj");
/*	ProjectProperties(); */
	iResult = FileSelection("prj", szProjectFileName);
	if (iResult != SUCCESS)
	{
		strcpy(szProjectFileName, "");
		return;
	}

	ProjectNew(szProjectFileName);
}


void MenuProjectOpen_Activation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	int iResult;
	char szProjectFileName[TEXTLEN];
	
	/* ask for project name */
	strcpy(szProjectFileName, "unnamed.prj");
	iResult = FileSelection("prj", szProjectFileName);
	if (iResult != SUCCESS)
	{
		strcpy(szProjectFileName, "");
		return;
	}
	
	ProjectOpen(szProjectFileName);
}


void MenuProjectSave_Activation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	int iResult;
	char szFileName[TEXTLEN];
	
	if (strlen(Project.szName) == 0)
	{
		MsgBox(
			pWindowMain,
			"FATAL ERROR !",
			"Project has no name.",
			MSGBOX_FATAL | MSGBOX_OKD
			);
		return;
	}

	/* save project data */
	strcpy(szFileName, Project.szDir);
	strcat(szFileName, "/");
	strcat(szFileName, Project.szName);
	strcat(szFileName, ".");
	strcat(szFileName, Project.szExt);
	iResult = DocSave(szFileName);
	if (iResult != SUCCESS)
	{
		MsgBox(
			pWindowMain,
			"Error !",
			"Cannot save project !",
			MSGBOX_ERROR | MSGBOX_OKD
			);
		return;
	}
}


void MenuProjectClose_Activation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	int iResult;
	char szFileName[TEXTLEN], szValue[TEXTLEN];
	
	/* check if project changed */
	if (Project.bChanged)
	{
		iResult = MsgBox(
			pWindowMain,
			"Question ...",
			"Project changed. Save it now ?",
			MSGBOX_QUESTION | MSGBOX_YESD | MSGBOX_NO | MSGBOX_CANCEL
			);
		if (iResult == MSGBOX_CANCEL)
			return;
		if (iResult == MSGBOX_YES)
			MenuProjectSave_Activation(pMenuItem, pUserData);
	}
	
	/* unload files */
	iResult = SUCCESS;
	strcpy(szFileName, "");
	while (iResult == SUCCESS)
	{
		iResult = DocGetProperty(DOC_NEXT, szFileName, (void *) szValue);
		if (iResult != SUCCESS)
			break;
		DocDestroy(szValue);
	}

	/* unload project */
	strcpy(Project.szName, "");
	ProjectWidgetsHide();
	ProjectTitle();
	Project.bChanged = FALSE;
}


void MenuProjectExit_Activation(GtkMenuItem *pMenuItem, gpointer pUserData)
{
	MenuProjectClose_Activation(pMenuItem, pUserData);
	/* no gtk_main_loop(), so not gtk_main_quit(); */
	bRunning = FALSE;
}



/*
	Private functions
*/

void ProjectWidgetsHide(void)
{
	GtkWidget *pWidget;
	
	/* do menu positions not sensitive */
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuProjectNew");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuProjectOpen");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuProjectSave");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuProjectClose");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuFile");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuAction");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuTool");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuWindow");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);

	/* hide window fields */
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "ProjectArea");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
//	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "DocArea");
//	if (pWidget == NULL)
//		/**/;
//	gtk_widget_set_sensitive(pWidget, FALSE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "StatusArea");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
}


void ProjectWidgetsShow(void)
{
	GtkWidget *pWidget;

	/* do menu positions sensitive */
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuProjectNew");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuProjectOpen");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, FALSE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuProjectSave");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuProjectClose");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuFile");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuAction");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
	MenuActionRefresh("");
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuTool");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "MenuWindow");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);

	/* show window fields */
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "ProjectArea");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
//	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "DocArea");
//	if (pWidget == NULL)
//		/**/;
//	gtk_widget_set_sensitive(pWidget, TRUE);
	pWidget = lookup_widget(GTK_WIDGET(pWindowMain), "StatusArea");
	if (pWidget == NULL)
		/**/;
	gtk_widget_set_sensitive(pWidget, TRUE);
}



void ProjectTitle(void)
{
	int iResult, bChanged;
	char szTitle[TEXTLEN], szFileName[TEXTLEN];

	/* add software name to title bar */
	strcpy(szTitle, GEDA_TITLE);

	/* add project name to title bar */
	if (strlen(Project.szName) > 0)
	{
		strcat(szTitle, " - ");
		strcat(szTitle, Project.szName);
		strcat(szTitle, ".");
		strcat(szTitle, Project.szExt);
		if (Project.bChanged)
			strcat(szTitle, " *");
	}

	/* add current file name to title bar */
	iResult = DocGetProperty(DOC_SELECTED, NULL, (void *) szFileName);
	if (iResult == SUCCESS && strlen(szFileName) > 0)
	{
		strcat(szTitle, " (");
		strcat(szTitle, szFileName);
		DocGetProperty(DOC_CHANGED, szFileName, (void *) &bChanged);
		if (bChanged)
			strcat(szTitle, " *");
		strcat(szTitle, ")");
	}

	gtk_window_set_title(pWindowMain, szTitle);
}



void ProjectChanged(int bValue)
{
	if (bValue != TRUE && bValue !=FALSE)
		return;
	
	Project.bChanged = bValue;
	ProjectTitle();
}


#if 0

/*
    This is not ended project manager window.
    Development breaked by gaf release.
    To be continued ...
*/

void ProjectProperties(BOOL bIsNew)
{
	GtkWindow *pWindow = NULL;
	GtkVBox *pVBox;
	GtkFrame *pFrameDesc, *pFrameDir;
	GtkTable *pTableDesc;
	GtkLabel *pLabelName;
	GtkEntry *pEntryName;
	int iWidth, iHeight, iW, iH, iX, iY;

	pWindow = GTK_WINDOW(gtk_window_new(GTK_WINDOW_DIALOG));
	iWidth = 300; //(iNumber > 3 ? iNumber : 4) * (MSGBOX_BTN_WIDTH + 2 * MSGBOX_BTN_BORDER);
	iHeight = 400; // - (iNumber == 0 ? MSGBOX_BTN_HEIGHT + 2 * MSGBOX_BTN_BORDER : 0);
	gtk_widget_set_usize(GTK_WIDGET(pWindow), iWidth, iHeight);
	gtk_window_set_modal(GTK_WINDOW(pWindow), TRUE);
	gtk_window_set_position(GTK_WINDOW(pWindow), GTK_WIN_POS_MOUSE);
	gtk_window_set_transient_for(GTK_WINDOW(pWindow), GTK_WINDOW(pWindowMain));
	gtk_window_set_title(GTK_WINDOW(pWindow), "Project properties");
	gtk_window_set_policy(GTK_WINDOW(pWindow), FALSE, FALSE, FALSE);
	gtk_widget_show(GTK_WIDGET(pWindow));
	while (g_main_iteration(FALSE)) ;
	gdk_window_get_size(GTK_WIDGET(pWindowMain)->window, &iW, &iH);
	gdk_window_get_position(GTK_WIDGET(pWindowMain)->window, &iX, &iY);
	gdk_window_move(
		GTK_WIDGET(pWindow)->window,
		iX + (iWidth >= iW ? iWidth - iW : iW - iWidth) / 2,
		iY + (iHeight >= iH ? iHeight - iH : iH - iHeight) /2
		);

	/* create vertical container */
	pVBox = GTK_VBOX(gtk_vbox_new(FALSE, 0));
	gtk_container_add(GTK_CONTAINER(pWindow), GTK_WIDGET(pVBox));

	/* create frame in project description area */
	pFrameDesc = gtk_frame_new("Project description");
	gtk_box_pack_start(GTK_BOX(pVBox), pFrameDesc, TRUE, TRUE, 0);
	//gtk_widget_set_usize (StartFrame, 400, -2);
	gtk_container_set_border_width(GTK_CONTAINER(pFrameDesc), 8);

	pTableDesc = gtk_table_new(8, 2, FALSE);
	gtk_container_add(GTK_CONTAINER(pFrameDesc), pTableDesc);
	gtk_container_set_border_width(GTK_CONTAINER(pTableDesc), 8);

	/* project name */
	pLabelName = gtk_label_new("ABC");
	gtk_table_attach(GTK_TABLE(pTableDesc), pLabelName, 0, 1, 0, 1, (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0, 0);
	gtk_misc_set_alignment(GTK_MISC(pLabelName), 0, 0.5);
	pEntryName = gtk_entry_new();
	gtk_table_attach(GTK_TABLE(pTableDesc), pEntryName, 1, 2, 0, 1, (GtkAttachOptions) (GTK_EXPAND/* | GTK_FILL*/), (GtkAttachOptions) (0), 0, 0);

	/* create frame in directory area */
	pFrameDir = gtk_frame_new("Project Directory");
	gtk_box_pack_start(GTK_BOX(pVBox), pFrameDir, TRUE, TRUE, 0);
	//gtk_widget_set_usize (StartFrame, 400, -2);
	gtk_container_set_border_width(GTK_CONTAINER(pFrameDir), 8);


	gtk_widget_show_all(GTK_WIDGET(pWindow));
	//gtk_signal_connect_object_after(GTK_OBJECT(pWindow), "delete-event", GTK_SIGNAL_FUNC(MsgBoxButtonClicked), GTK_OBJECT(pWindow));
	while (g_main_iteration(FALSE)) ;
}
#endif
