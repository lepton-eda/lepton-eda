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

#ifndef __TASK_H_INCLUDED
#define __TASK_H_INCLUDED

#include <stdio.h>
#include <sys/types.h>
#include "global.h"



/* task entry structure */
struct Task_s
{
	int iType;
	char *szValue;
	DWORD fFlags;
	pid_t Id;
	GtkMenuItem *pMenuItem;
	struct Task_s *pNext;
};



/* task types */
#define TASK_ACTION          1           /* external command, pParams[0]=pAction, pParams[1]=szFilename */



/* task activity flags */
#define TASK_NOFLAG          0x00000000L
#define TASK_BLOCKING        0x00000001L
#define TASK_INTERNAL        0x00000002L
#define ACTION_DEFAULT       0x00000004L



int TaskInitialize(void);
int TaskNew(const int iType, const void **pParams);
int TaskDelete(const struct Task_s *pTask);
void TaskProcess(void);



#endif
