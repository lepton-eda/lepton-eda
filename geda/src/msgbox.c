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
#include "global.h"
#include "msgbox.h"



/* dialog buttons */
#define MSGBOX_MAXBUTTONS    4
#define MSGBOX_BTN_WIDTH     96
#define MSGBOX_BTN_HEIGHT    22
#define MSGBOX_BTN_BORDER    8
static GtkButton *pButton[MSGBOX_MAXBUTTONS];
static int iNumber = 0;

/* dialog status */
#define MSGBOX_GOING         1
#define MSGBOX_DONE          2
#define MSGBOX_CANCELED      3
static int iMsgBoxStatus;
static int iMsgBoxSelection; 

/* private functions */
static void MsgBoxButtonClicked(GtkButton *pButtonClicked, gpointer pUserData);



int MsgBox(char *szLabel, DWORD dwButtons)
{
	GtkDialog *pDialog;
	GtkLabel *pLabel;
	GtkHBox *pHBox;
	int i;
	
	/* prepare buttons */
	iNumber = 0;
	if (dwButtons & MSGBOX_YES)
		pButton[iNumber ++] = GTK_BUTTON(gtk_button_new_with_label("Yes"));
	if (dwButtons & MSGBOX_NO)
		pButton[iNumber ++] = GTK_BUTTON(gtk_button_new_with_label("No"));
	if (dwButtons & MSGBOX_OK)
		pButton[iNumber ++] = GTK_BUTTON(gtk_button_new_with_label("Ok"));
	if (dwButtons & MSGBOX_CANCEL)
		pButton[iNumber ++] = GTK_BUTTON(gtk_button_new_with_label("Cancel"));
	if (dwButtons & MSGBOX_ALWAYSOK)
		pButton[iNumber ++] = GTK_BUTTON(gtk_button_new_with_label("Always Ok"));
	if (dwButtons & MSGBOX_ALWAYSCANCEL)
		pButton[iNumber ++] = GTK_BUTTON(gtk_button_new_with_label("Always Cancel"));
	if (dwButtons & MSGBOX_ALWAYSYES)
		pButton[iNumber ++] = GTK_BUTTON(gtk_button_new_with_label("Always Yes"));
	if (dwButtons & MSGBOX_ALWAYSNO)
		pButton[iNumber ++] = GTK_BUTTON(gtk_button_new_with_label("Always No"));
	
	/* create widgets */
	pDialog = GTK_DIALOG(gtk_dialog_new());
	pLabel = GTK_LABEL(gtk_label_new(szLabel));
	
	/* create area for buttons */
	pHBox = GTK_HBOX(gtk_hbox_new(FALSE, 0));
	gtk_widget_ref(GTK_WIDGET(pHBox));
	gtk_box_pack_start(GTK_BOX(GTK_DIALOG(pDialog)->action_area), GTK_WIDGET(pHBox), FALSE, FALSE, 0);

	/* place and show widgets */
	for (i = 0; i < iNumber; i ++)
	{
		gtk_widget_set_usize(GTK_WIDGET(pButton[i]), MSGBOX_BTN_WIDTH + 2 * MSGBOX_BTN_BORDER, MSGBOX_BTN_HEIGHT + 2 * MSGBOX_BTN_BORDER);
		gtk_signal_connect_object(GTK_OBJECT(pButton[i]), "clicked", GTK_SIGNAL_FUNC(MsgBoxButtonClicked), GTK_OBJECT(pButton[i]));
		gtk_box_pack_start(GTK_BOX(pHBox), GTK_WIDGET(pButton[i]), FALSE, FALSE, 0);
		gtk_container_set_border_width(GTK_CONTAINER(pButton[i]), 8);
	}
	gtk_container_add(GTK_CONTAINER(GTK_DIALOG(pDialog)->vbox), GTK_WIDGET(pLabel));
	gtk_label_set_line_wrap(pLabel, TRUE);
	gtk_widget_set_usize(GTK_WIDGET(pDialog), (iNumber > 2 ? iNumber : 3) * (MSGBOX_BTN_WIDTH + 2 * MSGBOX_BTN_BORDER), 150);
	gtk_window_set_modal(GTK_WINDOW(pDialog), TRUE);
	gtk_window_set_position(GTK_WINDOW(pDialog), GTK_WIN_POS_CENTER);
	gtk_window_set_transient_for(GTK_WINDOW(pDialog), GTK_WINDOW(pWindowMain));
	gtk_window_set_title(GTK_WINDOW(pDialog), "Message");
	gtk_window_set_policy(GTK_WINDOW(pDialog), FALSE, FALSE, FALSE);
	gtk_widget_show_all(GTK_WIDGET(pDialog));
	gtk_signal_connect_object(GTK_OBJECT(pDialog), "delete-event", GTK_SIGNAL_FUNC(MsgBoxButtonClicked), GTK_OBJECT(pDialog));

	/* wait until selection is done */
	iMsgBoxStatus = MSGBOX_GOING;
	while (iMsgBoxStatus == MSGBOX_GOING)
		while (g_main_iteration(FALSE)) ;
	
	/* hide message box window */
	gtk_widget_destroy(GTK_WIDGET(pDialog));
	while (g_main_iteration(FALSE)) ;
	
	/* return selection */
	return iMsgBoxSelection;
}



static void MsgBoxButtonClicked(GtkButton *pButtonClicked, gpointer pUserData)
{
	int i;

	for (i = 0; i < iNumber; i ++)
		if (pButtonClicked == pButton[i])
			break;
	if (i == iNumber)
		iMsgBoxSelection = -1;
	else
		iMsgBoxSelection = i;
	iMsgBoxStatus = MSGBOX_DONE;
}
