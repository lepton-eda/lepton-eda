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

#ifndef __LOG_H_INCLUDED
#define __LOG_H_INCLUDED

#include "global.h"



/* public functions */
void Log(int iImportance, int iModule, char *pValue);

/* public variables */
extern char szLogMessage[];
extern int iLogVerbose;



/* name of a log file */
#define LOG_FILENAME         SETUP_LOGFILE

/* importance types */
#define LOG_MESSAGE          1
#define LOG_WARNING          2
#define LOG_ERROR            3
#define LOG_FATAL            4

/* module types */
#define LOG_MAIN             1
#define MODULE_PACKAGE       2
#define LOG_START            3
#define LOG_MODULE_COMPS     4
#define MODULE_LICENSE       5
#define MODULE_DIRS          6
#define MODULE_SUMMARY       7
#define MODULE_STATUS        8
#define LOG_MODULE_INSTALL   9
#define LOG_FILE             10
#define LOG_MODULE_CALLBACK  11


#endif
