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
#  include "config.h"
#endif

#include <locale.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <time.h>

#include "configfile.h"
#include "filetool.h"
#include "global.h"
#include "graph.h"
#include "libstring.h"
#include "msg.h"
#include "value.h"



/*******************************************************************************/
/*	Graph                                                                  */
/*******************************************************************************/

struct Graph_t Graph = 
{
	NULL,
	NULL,
	NULL,
	NULL,

	NULL,

	0.0,
	0.0,
	TRUE,
	NULL,
	0.0,
	TRUE,
	NULL, 
	NULL, 
	0,
	TRUE,
	NULL, 
	NULL, 

	0.0,
	0.0,
	FALSE,
	NULL,
	0.0,
	TRUE,
	NULL, 
	NULL, 
	0,
	TRUE,
	NULL, 
	NULL
};



/*******************************************************************************/
/*	Create new graph                                                       */
/*******************************************************************************/

BOOL GraphNew(const char *szFileName)
{
	StringCopy(&Graph.szFileName, szFileName);
	StringCopy(&Graph.szDataSource, "");
	StringCopy(&Graph.szViewer, VIEWER_GWAVE);
	StringCopy(&Graph.szBorderColor, COLOR_WHITE);

	if (Graph.PlotList != NULL)
		free(Graph.PlotList);
	Graph.PlotList = NULL;

	Graph.dfXmin = 0.0;
	Graph.dfXmax = 0.0;
	Graph.bXAuto = TRUE;
	StringCopy(&Graph.szXScale, SCALE_LIN);
	Graph.dfXmainDiv = 1.0;
	Graph.bXmainAuto = TRUE;
	StringCopy(&Graph.szXmainStyle, STYLE_SOLID);
	StringCopy(&Graph.szXmainColor, COLOR_BLACK);
	Graph.iXaddNumber = 0;
	Graph.bXaddAuto = TRUE;
	StringCopy(&Graph.szXaddStyle, STYLE_SOLID);
	StringCopy(&Graph.szXaddColor, COLOR_BLACK);

	Graph.dfYmin = 0.0;
	Graph.dfYmax = 0.0;
	Graph.bYAuto = FALSE;
	StringCopy(&Graph.szYScale, SCALE_LIN);
	Graph.dfYmainDiv = 1.0;
	Graph.bYmainAuto = TRUE;
	StringCopy(&Graph.szYmainStyle, STYLE_SOLID);
	StringCopy(&Graph.szYmainColor, COLOR_BLACK);
	Graph.iYaddNumber = 0;
	Graph.bYaddAuto = TRUE;
	StringCopy(&Graph.szYaddStyle, STYLE_SOLID);
	StringCopy(&Graph.szYaddColor, COLOR_BLACK);

	return FALSE;
}



/*******************************************************************************/
/*	Load a graph                                                           */
/*******************************************************************************/

BOOL GraphLoad(const char *szFileName)
{
	int iResult;
	char szName[256], szValue[256];

	ValueSet(VALUE_FILENAME, FileGetName(szFileName));

	iResult = ConfigOpen(szFileName);
	if (iResult == FAILURE)
	{
		Error(MSG_CANNOT_OPEN_FILE);
		return TRUE;
	}

	iResult = ConfigSection("MAIN");
	if (iResult == FAILURE)
	{
		Error(MSG_WRONG_FILE);
		return TRUE;
	}
	for (iResult = SUCCESS; iResult == SUCCESS; )
	{
		iResult = ConfigGetNext(szName, szValue);
		if (iResult == FAILURE)
			break;
		if (strcmp(szName, "VERSION") == 0)
		{
			ValueSet(VALUE_VERSION, ConfigValue(1));
		}
		else if (strcmp(szName, "SIMULATION") == 0)
		{
			ValueSet(VALUE_SOURCE, ConfigValue(1));
		}
		else if (strcmp(szName, "TOOL") == 0)
		{
			ValueSet(VALUE_VIEWER, ConfigValue(1));
		}
		else if (strcmp(szName, "BORDER") == 0)
		{
			ValueSet(VALUE_BORDER, ConfigValue(1));
		}
	}

	iResult = ConfigSection("XAXIS");
	if (iResult == FAILURE)
	{
		Error(MSG_WRONG_FILE);
		return TRUE;
	}
	for (iResult = SUCCESS; iResult == SUCCESS; )
	{
		iResult = ConfigGetNext(szName, szValue);
		if (iResult == FAILURE)
			break;
		if (strcmp(szName, "X") == 0)
		{
			if (!strcmp(ConfigValue(1), "AUTO"))
			{
				ValueSet(VALUE_XMIN, "8");
				ValueSet(VALUE_XMAX, "9");
				ValueSet(VALUE_XAUTO, MSG_CHECKED);
			}
			else
			{
				ValueSet(VALUE_XMIN, ConfigValue(1));
				ValueSet(VALUE_XMAX, ConfigValue(2));
				ValueSet(VALUE_XAUTO, MSG_UNCHECKED);
			}
		}
		else if (strcmp(szName, "SCALE") == 0)
		{
			ValueSet(VALUE_XSCALE, ConfigValue(1));
		}
		else if (strcmp(szName, "XDIVM") == 0)
		{
			if (!strcmp(ConfigValue(1), "AUTO"))
			{
				ValueSet(VALUE_XMAINDIV, "0.1");
				ValueSet(VALUE_XMAINAUTO, MSG_CHECKED);
			}
			else
			{
				ValueSet(VALUE_XMAINDIV, ConfigValue(1));
				ValueSet(VALUE_XMAINAUTO, MSG_UNCHECKED);
			}
		}
		else if (strcmp(szName, "COLORM") == 0)
		{
			ValueSet(VALUE_XMAINCOLOR, ConfigValue(1));
		}
		else if (strcmp(szName, "STYLEM") == 0)
		{
			ValueSet(VALUE_XMAINSTYLE, ConfigValue(1));
		}
		else if (strcmp(szName, "XDIVA") == 0)
		{
			if (!strcmp(ConfigValue(1), "AUTO"))
			{
				ValueSet(VALUE_XADDNUMBER, "2");
				ValueSet(VALUE_XADDAUTO, MSG_CHECKED);
			}
			else
			{
				ValueSet(VALUE_XADDNUMBER, ConfigValue(1));
				ValueSet(VALUE_XADDAUTO, MSG_UNCHECKED);
			}
		}
		else if (strcmp(szName, "COLORA") == 0)
		{
			ValueSet(VALUE_XADDCOLOR, ConfigValue(1));
		}
		else if (strcmp(szName, "STYLEA") == 0)
		{
			ValueSet(VALUE_XADDSTYLE, ConfigValue(1));
		}
	}

	iResult = ConfigSection("YAXIS");
	if (iResult == FAILURE)
	{
		Error(MSG_WRONG_FILE);
		return TRUE;
	}
	for (iResult = SUCCESS; iResult == SUCCESS; )
	{
		iResult = ConfigGetNext(szName, szValue);
		if (iResult == FAILURE)
			break;
		if (strcmp(szName, "Y") == 0)
		{
			if (!strcmp(ConfigValue(1), "AUTO"))
			{
				ValueSet(VALUE_YMIN, "0");
				ValueSet(VALUE_YMAX, "1");
				ValueSet(VALUE_YAUTO, MSG_CHECKED);
			}
			else
			{
				ValueSet(VALUE_YMIN, ConfigValue(1));
				ValueSet(VALUE_YMAX, ConfigValue(2));
				ValueSet(VALUE_YAUTO, MSG_UNCHECKED);
			}
		}
		else if (strcmp(szName, "SCALE") == 0)
		{
			ValueSet(VALUE_YSCALE, ConfigValue(1));
		}
		else if (strcmp(szName, "YDIVM") == 0)
		{
			if (!strcmp(ConfigValue(1), "AUTO"))
			{
				ValueSet(VALUE_YMAINDIV, "0.1");
				ValueSet(VALUE_YMAINAUTO, MSG_CHECKED);
			}
			else
			{
				ValueSet(VALUE_YMAINDIV, ConfigValue(1));
				ValueSet(VALUE_YMAINAUTO, MSG_UNCHECKED);
			}
		}
		else if (strcmp(szName, "COLORM") == 0)
		{
			ValueSet(VALUE_YMAINCOLOR, ConfigValue(1));
		}
		else if (strcmp(szName, "STYLEM") == 0)
		{
			ValueSet(VALUE_YMAINSTYLE, ConfigValue(1));
		}
		else if (strcmp(szName, "YDIVA") == 0)
		{
			if (!strcmp(ConfigValue(1), "AUTO"))
			{
				ValueSet(VALUE_YADDNUMBER, "2");
				ValueSet(VALUE_YADDAUTO, MSG_CHECKED);
			}
			else
			{
				ValueSet(VALUE_YADDNUMBER, ConfigValue(1));
				ValueSet(VALUE_YADDAUTO, MSG_UNCHECKED);
			}
		}
		else if (strcmp(szName, "COLORA") == 0)
		{
			ValueSet(VALUE_YADDCOLOR, ConfigValue(1));
		}
		else if (strcmp(szName, "STYLEA") == 0)
		{
			ValueSet(VALUE_YADDSTYLE, ConfigValue(1));
		}
	}

	iResult = ConfigClose();
	if (iResult == FAILURE)
	{
		return TRUE;
	}

	return FALSE;
}

#if 0
BOOL ReadMain(const char *szFileName)
{

	return FALSE;
}
#endif



/*******************************************************************************/
/*	Save a graph                                                           */
/*******************************************************************************/

BOOL GraphSave(const char *szFileName)
{
	FILE *fp;
	time_t Time;
	struct Plot_t *Plot = NULL;
	int i;
	char *pLocale = NULL;

	fp = fopen(szFileName, "w");
	if (fp == NULL)
	{
		Error(MSG_CANNOT_CREATE_FILE);
		return TRUE;
	}

	pLocale = StringCreate();
	StringCopy(&pLocale, setlocale(LC_NUMERIC, NULL));
	setlocale(LC_NUMERIC, "C");

	Time = time(NULL);
	fprintf(fp, "# File created by Graph Manager %s\n", VERSION);
	fprintf(fp, "# Date: %s\n", ctime(&Time));
	fprintf(fp, "\n");

	fprintf(fp, "[MAIN]\n");
	fprintf(fp, "VERSION = \"%s\"\n", VERSION);
	fprintf(fp, "SIMULATION = \"%s\"\n", Graph.szDataSource);
	fprintf(fp, "TOOL = \"%s\"\n", Graph.szViewer);
	fprintf(fp, "BORDER = \"%s\"\n", "*** TODO ***");
	fprintf(fp, "\n");

	fprintf(fp, "[XAXIS]\n");
	switch (Graph.bXAuto)
	{
		case FALSE:   fprintf(fp, "X = %f, %f\n", Graph.dfXmin, Graph.dfXmax);   break;
		default:      fprintf(fp, "X = %s\n", AUTO_ON);                          break;
	}
	fprintf(fp, "SCALE = \"%s\"\n", Graph.szXScale);
	switch (Graph.bXmainAuto)
	{
		case FALSE:   fprintf(fp, "XDIVM = %f\n", Graph.dfXmainDiv);   break;
		default:      fprintf(fp, "XDIVM = %s\n", AUTO_ON);            break;
	}
	fprintf(fp, "COLORM = \"%s\"\n", Graph.szXmainColor);
	fprintf(fp, "STYLEM = \"%s\"\n", Graph.szXmainStyle);
	switch (Graph.bXaddAuto)
	{
		case FALSE:   fprintf(fp, "XDIVA = %d\n", Graph.iXaddNumber);   break;
		default:      fprintf(fp, "XDIVA = %s\n", AUTO_ON);             break;
	}
	fprintf(fp, "COLORA = \"%s\"\n", Graph.szXaddColor);
	fprintf(fp, "STYLEA = \"%s\"\n", Graph.szXaddStyle);
	fprintf(fp, "\n");

	fprintf(fp, "[YAXIS]\n");
	switch (Graph.bXAuto)
	{
		case FALSE:   fprintf(fp, "Y = %f, %f\n", Graph.dfYmin, Graph.dfYmax);   break;
		default:      fprintf(fp, "Y = %s\n", AUTO_ON);                          break;

	}
	fprintf(fp, "SCALE = \"%s\"\n", Graph.szYScale);
	switch (Graph.bYmainAuto)
	{
		case FALSE:   fprintf(fp, "YDIVM = %f\n", Graph.dfYmainDiv);   break;
		default:      fprintf(fp, "YDIVM = %s\n", AUTO_ON);            break;
	}
	
	fprintf(fp, "COLORM = \"%s\"\n", Graph.szYmainColor);
	fprintf(fp, "STYLEM = \"%s\"\n", Graph.szYmainStyle);
	switch (Graph.bYaddAuto)
	{
		case FALSE:   fprintf(fp, "YDIVA = %d\n",  Graph.iYaddNumber);   break;
		default:      fprintf(fp, "YDIVA = %s\n", AUTO_ON);              break;
	}
	fprintf(fp, "COLORA = \"%s\"\n", Graph.szYaddColor);
	fprintf(fp, "STYLEA = \"%s\"\n", Graph.szYaddStyle);
	fprintf(fp, "\n");

	for (Plot = Graph.PlotList, i = 1; Plot != NULL; Plot = Plot->pNext, i ++)
	{
		fprintf(fp, "[PLOT%d]\n", i);
		fprintf(fp, "FORMULA = \"%s\"\n", Plot->szFormula);
		fprintf(fp, "COLOR = \"%s\"\n", Plot->szColor);
		fprintf(fp, "STYLE = \"%s\"\n", Plot->szStyle);
		fprintf(fp, "\n");
	}

	setlocale(LC_NUMERIC, pLocale);

	fclose(fp);
	return FALSE;
}
