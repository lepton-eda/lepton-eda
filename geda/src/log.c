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

#include <stdio.h>
#include "log.h"



void Log(int iType, char *szFileName, int iLine, char *szMessage)
{
	printf("gEDA Suite %s > %s\n\tFile: %s\n\tLine: %d\n",
		(iType == LOG_FATAL) ? "FATAL ERROR" : (iType == LOG_ERROR) ? "ERROR" : (iType == LOG_WARNING) ? "WARNING" : (iType == LOG_MESSAGE) ? "MESSAGE" : "???",
		szMessage,
		szFileName,
		iLine
		);
}
