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

#include <stdio.h>
#include <stdlib.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <time.h>

#include "export2gwave.h"
#include "global.h"
#include "graph.h"
#include "msg.h"



static BOOL CreateScript(const char *szExtFilename);
static BOOL CreatePlot(const char *szExtFilename);



/*******************************************************************************/
/*	Export to GWave                                                        */
/*******************************************************************************/

void Export2Gwave(const char *szExtFilename)
{
	BOOL bError;

	bError = CreateScript(szExtFilename);
	if (!bError)
		return;

	bError = CreatePlot(szExtFilename);
	if (!bError)
		return;
}



static BOOL CreateScript(const char *szExtFilename)
{
	FILE *fp;
	time_t Time;
	struct Plot_t *Plot = NULL;
	int i;

	Time = time(NULL);

	fp = fopen(szExtFilename, "w");
	if (fp == NULL)
	{
		Error(MSG_CANNOT_CREATE_FILE);
		return TRUE;
	}

	fprintf(fp, "#!gwave -s\n");   /* BUG - path to gwave ?! */
	fprintf(fp, "\n");

	fprintf(fp, "# File created by Graph Manager %s\n", VERSION);
	fprintf(fp, "# Date: %s\n", ctime(&Time));
	fprintf(fp, "\n");

	fprintf(fp, "(require-n-wavepanels %d)\n", 
		1
		); 
	fprintf(fp, "(set! default-measure1-function %d)\n",          /* ??? */
		5
		);
	fprintf(fp, "(let ((df (if script-target-datafile\n");
	fprintf(fp, "           script-target-datafile\n");
	fprintf(fp, "           (find-or-load-wavefile \"%s.gwd\"))))\n",
		Graph.szDataSource
		);
	for (Plot = Graph.PlotList, i = 0; Plot != NULL; Plot = Plot->pNext, i ++)
	{
		fprintf(fp, " (wavepanel-add-var-setup df (nth-wavepanel %d) \"%s\" %d)\n",
			i,
			Plot->szFormula,
			(!strcmp(Plot->szColor, COLOR_YELLOW)) ? 1
			: (!strcmp(Plot->szColor, COLOR_RED)) ? 0
			: (!strcmp(Plot->szColor, COLOR_BLUE)) ? 3
			: (!strcmp(Plot->szColor, COLOR_GREEN)) ? 2
			: (!strcmp(Plot->szColor, COLOR_MAGENTA)) ? 5
			: 4 /* light blue, for not supported */
			);
	}
	fprintf(fp, ")\n");

	fprintf(fp, "(x-zoom! %f %f)\n", 
		Graph.dfXmin, 
		Graph.dfXmax);
	fprintf(fp, "(wtable-set-xlogscale! #%c)\n", 
		(!strcmp(Graph.szXScale, SCALE_LIN)) ? 'f'
		: (!strcmp(Graph.szXScale, SCALE_LOG)) ? 't'
		: 'f' /* for not supported */
		);
	fprintf(fp, "(set! default-wavepanel-type %d)\n",             /* ??? */
		0
		);

/*	fprintf(fp, "(set-wtable-vcursor! %d %f)\n",
		0,
		19500000.8050001
		); */
	fprintf(fp, "(gtk-tooltips-enable gwave-tooltips)\n");

	fprintf(fp, "(let ((wp (nth-wavepanel 0)))\n");
	fprintf(fp, " (set-wavepanel-ylogscale! wp #%c)\n",
		(!strcmp(Graph.szYScale, SCALE_LIN)) ? 'f'
		: (!strcmp(Graph.szYScale, SCALE_LOG)) ? 't'
		: 'f' /* for not supported */
		);
	fprintf(fp, " (set-wavepanel-type! wp %d)\n",                   /* ??? */
		0
		);
	if (Graph.bYAuto)
	{
		fprintf(fp, " (wavepanel-y-zoom! wp %f %f)\n",
			Graph.dfYmin,
			Graph.dfYmax
			);
	}

	fprintf(fp, ")\n");

	fclose(fp);
	return FALSE;
}


static BOOL CreatePlot(const char *szExtFilename)
{
	return FALSE;
}
