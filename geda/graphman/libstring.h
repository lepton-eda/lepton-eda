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

#ifndef __STRING_H_INCLUDED
#define __STRING_H_INCLUDED



/* public functions */
char *StringCreate(void);
void StringCopy(char **pString, const char *pSource);
void StringCat(char **pString, const char *pSource);
void StringCopyNum(char **pString, const char *pSource, const int iNumber);
int StringLength(char **pString);
float String2Float(char *pString);
int String2Integer(char *pString);
void StringDestroy(char **pString);



#endif /* __STRING_H_INCLUDED */
