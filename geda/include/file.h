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

#ifndef __FILE_H_INCLUDED
#define __FILE_H_INCLUDED



/* callback public functions */
void MenuFileEdit_Activation(GtkMenuItem *pMenuItem, gpointer pUserData);
void MenuFileSave_Activation(GtkMenuItem *pMenuItem, gpointer pUserData);
void MenuFilePrint_Activation(GtkMenuItem *pMenuItem, gpointer pUserData);
void MenuFileClose_Activation(GtkMenuItem *pMenuItem, gpointer pUserData);
/* TODO: void MenuFileNew_Activation(GtkMenuItem *pMenuItem, gpointer pUserData);*/
void MenuFileImport_Activation(GtkMenuItem *pMenuItem, gpointer pUserData);
void MenuFileUnlink_Activation(GtkMenuItem *pMenuItem, gpointer pUserData);
void MenuFileDelete_Activation(GtkMenuItem *pMenuItem, gpointer pUserData);



#endif
