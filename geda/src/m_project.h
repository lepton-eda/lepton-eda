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

#ifndef __M_PROJECT_H_INCLUDED
#define __M_PROJECT_H_INCLUDED



/*******************************************************************************

	Public functions and variables

*******************************************************************************/

void MenuProjectNew_Activation(GtkMenuItem *pMenuItem, gpointer pUserData);
void MenuProjectOpen_Activation(GtkMenuItem *pMenuItem, gpointer pUserData);
void MenuProjectSave_Activation(GtkMenuItem *pMenuItem, gpointer pUserData);
void MenuProjectClose_Activation(GtkMenuItem *pMenuItem, gpointer pUserData);
void MenuProjectExit_Activation(GtkMenuItem *pMenuItem, gpointer pUserData);



#endif /* __M_PROJECT_H_INCLUDED */
