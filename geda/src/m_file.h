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

#ifndef __M_FILE_H_INCLUDED
#define __M_FILE_H_INCLUDED



/*******************************************************************************

	Public functions and variables

*******************************************************************************/

/* menu callbacks */
void FileEdit_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData);
void FileSave_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData);
void FilePrint_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData);
void FileClose_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData);
void FileNew_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData);
void FileImport_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData);
void FileUnlink_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData);
void FileDelete_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData);

/* other callbacks */
void FileNew_ButtonOk(GtkButton *pButton, gpointer pUserData);
void FileNew_ButtonCancel(GtkButton *pButton, gpointer pUserData);
gboolean FileNew_Destroy(GtkWidget *pWidget, GdkEvent *pEvent, gpointer pUserData);



#endif /* __M_FILE_H_INCLUDED */
