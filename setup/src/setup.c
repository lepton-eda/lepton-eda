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

#include <errno.h>
#include <gtk/gtk.h>
#include <sys/types.h>
#include <unistd.h>
#include "comps.h"
#include "package.h"
#include "setup.h"
#include "support.h"

/* TODO: remove it */
#include <stdio.h>
int iQueueId;
#include <glib.h>




int iLicenseAgreement = 0;


GtkWidget *pStartWidget;



void setup_buttons(GtkWidget *pSourceWidget)
{
	GtkWidget *pWidget;
	int iPage;
	
	pWidget = lookup_widget(pSourceWidget, "Container");
	iPage = gtk_notebook_get_current_page(GTK_NOTEBOOK(pWidget));


	pWidget = lookup_widget(pSourceWidget, "PreviousButton");
	gtk_widget_set_sensitive(pWidget, 
		(iPage>0 && iPage<=4)
		? TRUE
		: FALSE
		);

	pWidget = lookup_widget(pSourceWidget, "NextButton");
	gtk_widget_set_sensitive(pWidget, 
		(iPage<2 || (iPage<4 && iLicenseAgreement==1))
		? TRUE
		: FALSE
		);

	pWidget = lookup_widget(pSourceWidget, "OkButton");
	gtk_widget_set_sensitive(pWidget, 
		(iLicenseAgreement==1 && iPage==4) 
		? TRUE 
		: FALSE
		);
}



