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

#include "global.h"



/* public functions */
int ProjectProperties(BOOL bIsNew);

/* menu callbacks */
void ProjectNew_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData);
void ProjectOpen_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData);
void ProjectProperties_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData);
void ProjectSave_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData);
void ProjectClose_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData);
void ProjectExit_MenuActivation(GtkMenuItem *pMenuItem, gpointer pUserData);

/* other callbacks */
void ProjectProperties_ButtonClicked(GtkButton *pButtonClicked, gpointer pUserData);
void ProjectProperties_DirChanged(GtkEditable *pEditable, gint pStartPos, gint pEndPos, gpointer pUserData);
void ProjectProperties_NameChanged(GtkEditable *pEditable, GdkEventKey *pEvent, gpointer pUserData);



#endif /* __M_PROJECT_H_INCLUDED */
