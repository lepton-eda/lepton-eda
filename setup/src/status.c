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
#include "global.h"
#include "package.h"
#include "status.h"
#include "support.h"
#include "xpm.h"



int StatusInitialize(void)
{
	StatusShow(pWindowMain);

	return SUCCESS;
}


void StatusShow(GtkWidget *pMainWindow)
{
	GdkPixmap *pPixmap;
    GdkBitmap *pMask;
	GtkPixmap *pPixmapStatus;
    GtkStyle *pStyle;
 	int iResult;
	
	iResult = (strlen(Software.szPictName) == 0)
		? FAILURE
		: FileTest(Software.szPictName);
	pStyle = gtk_widget_get_style(pWindowMain);
	pPixmap = (iResult != SUCCESS)
		? gdk_pixmap_create_from_xpm_d(pWindowMain->window, &pMask, &pStyle->bg[GTK_STATE_NORMAL], (gchar **) XpmDefault)
		: gdk_pixmap_create_from_xpm(pWindowMain->window, &pMask, &pStyle->bg[GTK_STATE_NORMAL], Software.szPictName);
	pPixmapStatus = GTK_PIXMAP(lookup_widget(pMainWindow, "StatusPixmap"));
	gtk_pixmap_set(pPixmapStatus, pPixmap, pMask);
}
