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

#ifndef __PROJECT_H_INCLUDED
#define __PROJECT_H_INCLUDED



/*******************************************************************************

	Project description structure

*******************************************************************************/

struct Project_s
{
	char szName[TEXTLEN];
	char szExt[TEXTLEN];
	char szDir[TEXTLEN];
	char szAuthor[TEXTLEN];
	char szDesc[TEXTLEN];
	int bChanged;
};

extern struct Project_s Project;	



/*******************************************************************************

	Public functions and variables

*******************************************************************************/

void ProjectInitialize(void);
void ProjectNew(const char *szPath);
void ProjectOpen(const char *szPath);
int ProjectSave(void);

void ProjectWidgetsShow(void);
void ProjectWidgetsHide(void);
void ProjectTitle(void);
void ProjectChanged(BOOL bValue);



#endif /* __PROJECT_H_INCLUDED */
