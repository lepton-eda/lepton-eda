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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <gtk/gtk.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "callbacks.h"
#include "interface.h"
#include "log.h"
#include "support.h"

#include "comps.h"
#include "dirs.h"
#include "install.h"
#include "msgbox.h"
#include "package.h"
#include "setup.h"
#include "summary.h"



/* TODO: remove it */
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
	
	pNotebook = (GtkNotebook *) lookup_widget(GTK_WIDGET(pButton), "Container");
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
			SummaryShow(GTK_WIDGET(pButton));
			break;
		
		case 5: /* installation status */
			
			break;
	}
	
	setup_buttons(GTK_WIDGET(pButton));

	gtk_widget_show(GTK_WIDGET(pNotebook));
}



void on_previous_clicked(GtkButton *pButton, gpointer user_data)
{
	GtkNotebook *pNotebook;
	int iPage;
	
	pNotebook = (GtkNotebook *) lookup_widget(GTK_WIDGET(pButton), "Container");
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
			SummaryShow(GTK_WIDGET(pButton));
			break;
		
		case 5: /* installation status */
			
			break;
	}
	
	setup_buttons(GTK_WIDGET(pButton));

	gtk_widget_show(GTK_WIDGET(pNotebook));
}

extern int iSoftwareInstalled;
void on_ok_clicked(GtkButton *pButton, gpointer user_data)
{
	GtkNotebook *pNotebook;
	int iPage;

	/* look for notebook */
	pNotebook = (GtkNotebook *) lookup_widget(GTK_WIDGET(pButton), "Container");
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


void
on_AgreeButton_clicked                 (GtkButton       *button,
                                        gpointer         user_data)
{
	iLicenseAgreement = 1;
	setup_buttons(GTK_WIDGET(button));
}


void
on_DismissButton_clicked               (GtkButton       *button,
                                        gpointer         user_data)
{
	iLicenseAgreement = 0;
	setup_buttons(GTK_WIDGET(button));
}



extern GtkPixmap *PixmapNone, *PixmapFull;



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

