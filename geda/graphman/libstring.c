/* $Id$ */

/*******************************************************************************/
/*                                                                             */
/* gEDA Suite Project Manager                                                  */
/*                                                                             */
/* Copyright (C) 2002 Piotr Miarecki, sp9rve@radioam.net                       */
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
/* along with this program; if not, email to the author                        */
/*                                                                             */
/*******************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "libstring.h"



/********************************************************************************

	Create a new string

********************************************************************************/

char *StringCreate(void)
{
	char *pString = NULL;

	pString = (char *) malloc(sizeof(char));
	if (pString != NULL)
		strcpy(pString, "");

	return pString;
}



/********************************************************************************

	Copy a string

********************************************************************************/

void StringCopy(char **pString, const char *pSource)
{
	*pString = (char *) realloc(*pString, strlen(pSource) + sizeof(char));
		strcpy(*pString, pSource);
}



/********************************************************************************

	Cat to a string

********************************************************************************/

void StringCat(char **pString, const char *pSource)
{
	*pString = (char *) realloc(*pString, strlen(*pString) + strlen(pSource) + sizeof(char));
		strcat(*pString, pSource);
}



/*******************************************************************************

	Copy a string (limited number of characters)

********************************************************************************/

void StringCopyNum(char **pString, const char *pSource, const int iNumber)
{
	*pString = (char *) realloc(*pString, (strlen(pSource) > iNumber ? iNumber : strlen(pSource)) + sizeof(char));
	strncpy(*pString, pSource, iNumber);
	(*pString)[strlen(pSource) > iNumber ? iNumber : strlen(pSource)] = 0;
}



/*******************************************************************************/
/*	Length of a string                                                     */
/*******************************************************************************/

int StringLength(char **pString)
{
	return strlen(*pString);
}



/*******************************************************************************/
/*	Convert String to float                                                */
/*******************************************************************************/

float String2Float(char *pString)
{
	float f;

	sscanf(pString, "%f", &f);

	return f;
}



/*******************************************************************************/
/*	Convert String to integer                                              */
/*******************************************************************************/

int String2Integer(char *pString)
{
	int i;

	sscanf(pString, "%d", &i);

	return i;
}



/*******************************************************************************/
/*	Destroy a string                                                       */
/*******************************************************************************/

void StringDestroy(char **pString)
{
	free((void *) *pString);
}
