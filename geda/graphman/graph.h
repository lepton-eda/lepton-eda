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

#ifndef __GRAPH_H_INCLUDED
#define __GRAPH_H_INCLUDED

#include "global.h"



/* plot definition */
struct Plot_t
{
	char *szFormula;
	char *szColor;
	char *szStyle;

	struct Plot_t *pNext;
};

/* graph definition */
struct Graph_t
{
	char *szFileName;               /* graph filename, without extenstion */
	char *szDataSource;             /* data source filename, without ext. */
	char *szViewer;                    /* graph viewer identifier (as below) */
	char *szBorderColor;
                   

	struct Plot_t *PlotList;        /* pointer to a list of graph plots */

	double dfXmin;
	double dfXmax;
	BOOL bXAuto;
	char *szXScale;                    /* see below */
	double dfXmainDiv;
	BOOL bXmainAuto;
	char *szXmainStyle;                 /* see below */
	char *szXmainColor;              /* see below */
	int iXaddNumber;
	BOOL bXaddAuto;
	char *szXaddStyle;                 /* see below */
	char *szXaddColor;              /* see below */

	double dfYmin;
	double dfYmax;
	BOOL bYAuto;
	char *szYScale;                    /* see below */
	double dfYmainDiv;
	BOOL bYmainAuto;
	char *szYmainStyle;                 /* see below */
	char *szYmainColor;              /* see below */
	int iYaddNumber;
	BOOL bYaddAuto;
	char *szYaddStyle;                 /* see below */
	char *szYaddColor;              /* see below */
};



/* graph viewers */
#define VIEWER_NOT_SELECTED ""
#define VIEWER_GWAVE        "GWAVE"

/* scale types */
#define SCALE_NOT_SELECTED  ""
#define SCALE_LIN           "LIN"
#define SCALE_LOG           "LOG"

/* styles */
#define STYLE_NOT_SELECTED  ""
#define STYLE_SOLID         "SOLID"

/* colors */
#define COLOR_NOT_SELECTED  ""
#define COLOR_WHITE         "White"
#define COLOR_BLACK         "Black"
#define COLOR_YELLOW        "Yellow"
#define COLOR_RED           "Red"
#define COLOR_BLUE          "Blue"
#define COLOR_GREEN         "Green"
#define COLOR_ORANGE        "Orange"
#define COLOR_MAGENTA       "Pink"
#define COLOR_OTHER         "0x"

#define AUTO_ON             "AUTO"


/* public variables */
extern struct Graph_t Graph;



/* public functions */
BOOL GraphNew(const char *szFileName);
BOOL GraphLoad(const char *szFileName);
BOOL GraphSave(const char *szFileName);



#endif /* __GRAPH_H_INCLUDED */
