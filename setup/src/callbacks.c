#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <gtk/gtk.h>
#include <signal.h>
#include <stdio.h>

#include "callbacks.h"
#include "interface.h"
#include "log.h"
#include "support.h"

#include "comps.h"
#include "dirs.h"
#include "msgbox.h"
#include "package.h"
#include "setup.h"



/* TODO: remove it */
extern int iQueueId;
#define WINDOW_START         0x0100
#define WINDOW_COMPONENTS    0x0200
#define WINDOW_LICENSE       0x0400
#define WINDOW_DIRECTORY     0x0800
#define WINDOW_INFORMATION   0x1000
#define WINDOW_FINISH        0x2000

unsigned int current_window = WINDOW_START;



void on_next_clicked(GtkButton *pButton, gpointer user_data)
{
	GtkNotebook *pNotebook;
	int iPage;
	
	pNotebook = (GtkNotebook *) lookup_widget(pButton, "Container");
	if (pNotebook == NULL)
	{
		/* TODO: error handling */
	}

	gtk_notebook_next_page(pNotebook);

	iPage = gtk_notebook_get_current_page(pNotebook);
	switch (iPage)
	{
		case 0: /* start page */
			
			break;
		
		case 1: /* component list selection */

			CompsPrepare();
			break;
		
		case 2: /* license agreement */
			
			break;
		
		case 3: /* directory selection */
			
			break;

		case 4: /* summary information */
			
			SummaryPrepare();
			SummaryShow(pButton);
			break;
		
		case 5: /* installation status */
			
			break;
		
		default:
		
			/* TODO: error handling */
	}
	
	setup_buttons(pButton);

	gtk_widget_show(pNotebook);	
}



void on_previous_clicked(GtkButton *pButton, gpointer user_data)
{
	GtkNotebook *pNotebook;
	int iPage;
	
	pNotebook = (GtkNotebook *) lookup_widget(pButton, "Container");
	if (pNotebook == NULL)
	{
		/* TODO: error handling */
	}

	gtk_notebook_prev_page(pNotebook);

	iPage = gtk_notebook_get_current_page(pNotebook);
	switch (iPage)
	{
		case 0: /* start page */
			
			break;
		
		case 1: /* component list selection */
			
			break;
		
		case 2: /* license agreement */
			
			break;
		
		case 3: /* directory selection */
			
			break;
		
		case 4: /* summary information */
			
			SummaryPrepare();
			SummaryShow(pButton);
			break;
		
		case 5: /* installation status */
			
			break;
		
		default:
		
			/* TODO: error handling */
	}
	
	setup_buttons(pButton);

	gtk_widget_show(pNotebook);
}

extern int iSoftwareInstalled;
//extern int OkPressed;
void on_ok_clicked(GtkButton *pButton, gpointer user_data)
{
	GtkNotebook *pNotebook;
	int iPage;

	/* look for notebook */
	pNotebook = (GtkNotebook *) lookup_widget(pButton, "Container");
	if (pNotebook == NULL)
	{
		Log(LOG_FATAL, LOG_MODULE_CALLBACK, "lookup_widget() cannot find widget 'Container'");
		return;
	}

	/* determine notebook's page */
	iPage = gtk_notebook_get_current_page(pNotebook);
	switch (iPage)
	{
		case PAGE_SUMMARY:
			
			InstallSoftware();
			break;
		
		case PAGE_STATUS:
			
			gtk_main_quit();
			break;
	}

	return;
}



void on_cancel_clicked(GtkButton *pButton, gpointer user_data)
{
	/* TODO: dialog "Are you sure ?" */
	gtk_main_quit();
}











void
on_optionmenu1_enter                   (GtkButton       *button,
                                        gpointer         user_data)
{

}


void
on_help_clicked                        (GtkButton       *button,
                                        gpointer         user_data)
{

}


extern int iQueueId;




gboolean
on_ComponentTree_show                  (GtkWidget       *widget,
                                        GdkEvent        *event,
                                        gpointer         user_data)
{

  return FALSE;
}


void
on_ComponentTree_draw                  (GtkWidget       *widget,
                                        GdkRectangle    *area,
                                        gpointer         user_data)
{

}

void
on_InstallLocalButton_clicked          (GtkButton       *button,
                                        gpointer         user_data)
{
	GtkWidget *w;
	w = lookup_widget(button, "InstallDirectoryEntry");

	dirs_local(w);
}


void
on_InstallGlobalButton_clicked         (GtkButton       *button,
                                        gpointer         user_data)
{
	GtkWidget *w;
	w = lookup_widget(button, "InstallDirectoryEntry");

	dirs_global(w);
}


void
on_InstallCustomButton_clicked         (GtkButton       *button,
                                        gpointer         user_data)
{
	GtkWidget *w;
	w = lookup_widget(button, "InstallDirectoryEntry");

	dirs_custom(w);
}


void
on_AgreeButton_clicked                 (GtkButton       *button,
                                        gpointer         user_data)
{
	iLicenseAgreement = 1;
	setup_buttons(button);
}


void
on_DismissButton_clicked               (GtkButton       *button,
                                        gpointer         user_data)
{
	iLicenseAgreement = 0;
	setup_buttons(button);
}

char cSelectFlag = 1;

extern GtkPixmap *PixmapNone, *PixmapFull;



/*
	Module COMPONENTS - callbacks
*/

void on_ComponentTree_tree_select_row       (GtkCTree        *ctree,
                                        GList           *node,
                                        gint             column,
                                        gpointer         user_data)
{
	struct CompsTable_s *pComp;
	int iResult;
	char szMessage[TEXTLEN], *szMissingFile;
		
	if (cSelectFlag == 0)
		return;
	
//printf("onSelect() >\n");
	
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
	
//printf("onSelect() > package '%s'\n", pComp->szCodeName);
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


void
on_ComponentTree_tree_unselect_row     (GtkCTree        *ctree,
                                        GList           *node,
                                        gint             column,
                                        gpointer         user_data)
{
	on_ComponentTree_tree_select_row(ctree, node, column, user_data);
}

void
on_ComponentTree_tree_expand           (GtkCTree        *ctree,
                                        GList           *node,
                                        gpointer         user_data)
{
	//printf("expand() >\n");
	ShowIcons("");
}


void
on_ComponentTree_tree_collapse         (GtkCTree        *ctree,
                                        GList           *node,
                                        gpointer         user_data)
{
	//printf("collapse() >\n");
	ShowIcons("");
}


void
on_ComponentTree_change_focus_row_expansion
                                        (GtkCList        *clist,
                                        gpointer         user_data)
{
}

void
on_InstallDirectoryEntry_changed       (GtkEditable     *editable,
                                        gpointer         user_data)
{
	GtkEntry *pEntry;
	
	pEntry = lookup_widget(editable, "InstallDirectoryEntry");
	if (pEntry == NULL)
	{
		/* TODO: error handling */
	}

	strcpy(szInstallDirectory, gtk_entry_get_text(pEntry));
}


gboolean
on_MainWindow_delete_event             (GtkWidget       *widget,
                                        GdkEvent        *event,
                                        gpointer         user_data)
{

	gtk_main_quit();
  return FALSE;
}


void
on_CancelButton_clicked                (GtkButton       *button,
                                        gpointer         user_data)
{
	_exit(0);
}

