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

#ifdef HAVE_CONFIG_H
#include "../config.h"
#endif
#include <ctype.h>
#include <stdio.h>
#include <string.h>

#include "config.h"
#include "global.h"
#include "libstring.h"


/*******************************************************************************

	Static functions and variables

*******************************************************************************/

#define MAXLINELEN           256

static FILE *fp = NULL;
static char szLineBuf[MAXLINELEN];

static int ConfigReadLine(char *szLine);
static BOOL IsSeparator(const char c);



int ConfigOpen(const char *szName)
{
	fp = fopen(szName, "r");
	if (fp == NULL)
		return FAILURE;
	
	return SUCCESS;
}



int ConfigClose(void)
{
	int iReturn;
	
	iReturn = fclose(fp);
	if (iReturn != 0)
		return FAILURE;
	
	fp = NULL;
	
	return SUCCESS;
}



int ConfigSection(const char *szName)
{
	int iFound = 0, iResult, i;
	char szLine[MAXLINELEN], szPattern[MAXLINELEN];
	
	rewind(fp);
	sprintf(szPattern, "[%s]", szName);
	for (i = 0; i < strlen(szPattern); i ++)
		szPattern[i] = toupper(szPattern[i]);
		
	
	while (!feof(fp) && iFound == 0)
	{
		iResult = ConfigReadLine(szLine);
		if (iResult != SUCCESS)
			return FAILURE;
		for (i = 0; i < strlen(szLine); i ++)
			szLine[i] = toupper(szLine[i]);

		if (strncmp(szPattern, szLine, strlen(szPattern)) == 0)
		{
			iFound = 1;
			break;
		}
	}
	
	if (iFound == 0)
		return FAILURE;
	
	return SUCCESS;
}



int ConfigGetNext(char *szName, char *szValue)
{
	int iFound = 0, iResult, i, j = 0;
	char szLine[MAXLINELEN];
	
	while (!feof(fp) && iFound == 0)
	{
		iResult = ConfigReadLine(szLine);
		if (iResult != SUCCESS)
			return FAILURE;

		if (strlen(szLine) == 0)
			continue;
		if (szLine[0] == '[')
			break;
		if (isascii(szLine[0]))
			iFound = 1;
		break;
	}
	
	if (iFound == 0)
		return FAILURE;

	for (i = 0; i < strlen(szLine) && isalnum(szLine[i]); i ++)
		szName[i] = toupper(szLine[i]);
	szName[i] = 0;
	for (; i < strlen(szLine) && (szLine[i] == '=' || isspace(szLine[i])); i ++)
		;
	for (; i < strlen(szLine); i ++)
		szValue[j ++] = szLine[i];
	szValue[j] = 0;
	

	return SUCCESS;
}



/*******************************************************************************

	Get value of a parameter

	Function reads a string (pointed out) from a parameter line 
	and return a pointer to it. It uses a static buffer.

*******************************************************************************/

#define PARSE_SPACES        1
#define PARSE_STRING        2
#define PARSE_ALNUM         3
#define PARSE_STOP          4

char *ConfigValue(const int iValue)
{
	static char *pString = NULL;

	int iCount, iState, iStart = 0, iStop, i;

	if (pString == NULL)
		StringCreate();


	/* omit beginning of a parameter line, like "KEY = " */
	for (i = 0; i < strlen(szLineBuf) && szLineBuf[i] != '='; i ++)
		;
	i ++;


	for (iCount = 1; iCount <= iValue; iCount ++)
	{
		for (iState = PARSE_SPACES; i < strlen(szLineBuf) && iState != PARSE_STOP; i ++)
		{
			switch (iState)
			{
				case PARSE_SPACES:
					if (!isspace(szLineBuf[i]))
					{
						if (szLineBuf[i] == '"')
						{
							iStart = i + 1;
							iStop = iStart;
							iState = PARSE_STRING;
						}

						else if (!IsSeparator(szLineBuf[i]))
						{
							iStart = i;
							iStop = iStart;
							iState = PARSE_ALNUM;
						}
					}
					break;

				case PARSE_STRING:
					if (szLineBuf[i] == '"')
					{
						iStop = i - 1;
						iState = PARSE_STOP;
						StringCopyNum(&pString, szLineBuf + iStart, iStop - iStart + 1);
					}
					else
						iStop = i;
					break;

				case PARSE_ALNUM:
					if (isspace(szLineBuf[i]) || IsSeparator(szLineBuf[i]))
					{
						iState = PARSE_STOP;
					}
					else
						iStop = i;
					break;

				case PARSE_STOP:
					break;
			}
		}

		if (iCount == iValue)
			StringCopyNum(&pString, szLineBuf + iStart, iStop - iStart + 1);
	}


	return (strlen(pString) > 0)
		? pString
		: NULL;
}



static int ConfigReadLine(char *szLine)
{
	int c = 0, j = 0;
	
	while (!feof(fp) && j < MAXLINELEN)
	{
		c = fgetc(fp);
		
		if (c == '\n' || c == '\r')
		{
			//j = 0;
			break;
		}
		
		szLine[j ++] = (char) c;
	}
	
	while (!feof(fp) && (c == '\n' || c == '\r'))
		c = fgetc(fp);
	ungetc(c, fp);
	szLine[j] = 0;

	strcpy(szLineBuf, szLine);
	
	return SUCCESS;
}



/*******************************************************************************

	Separator recognizing

*******************************************************************************/

static char cSeparator = ',';


static BOOL IsSeparator(const char c)
{
	return (c == cSeparator)
		? TRUE
		: FALSE;
}
