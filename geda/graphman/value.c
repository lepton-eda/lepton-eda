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

#include <gtk/gtk.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "global.h"
#include "graph.h"
#include "libstring.h"
#include "msg.h"
#include "support.h"
#include "value.h"



/********************************************************************************

	Static functions 

********************************************************************************/

static void DisplayEntry(const char *szWidget, const char *szValue);
static char *ReadEntry(const char *szWidget);

static void DisplayCombo(const char *szWidget, const char *szValue);
static char *ReadCombo(const char *szWidget);

static void DisplayCheck(const char *szWidget, const char *szValue);
static BOOL ReadCheck(const char *szWidget);

static void DisplayValidity(const char *szWidget, BOOL bValue);
static void DisplaySensitivity(const char *szWidget, const BOOL bSensitive);

static char *Man2File_Color(const char *szColor);
static char *File2Man_Color(const char *szColor);
static BOOL IsValid_Color(const char *szColor);
static void Init_Color(const char *szWidget);

static char *Man2File_Style(const char *szStyle);
static char *File2Man_Style(const char *szStyle);
static BOOL IsValid_Style(const char *szStyle);
static void Init_Style(const char *szWidget);

static char *Man2File_Scale(const char *szScale);
static char *File2Man_Scale(const char *szScale);
static BOOL IsValid_Scale(const char *szScale);
static void Init_Scale(const char *szWidget);

static char *Man2File_Viewer(const char *szViewer);
static char *File2Man_Viewer(const char *szViewer);
static BOOL IsValid_Viewer(const char *szViewer);
static void Init_Viewer(const char *szWidget);

static char *Man2File_Number(const char *szNumber);
static char *File2Man_Number(const char *szNumber);

/* not used yet */
/* static BOOL IsValid_Number(const char *szNumber); */



/********************************************************************************

	Initialize value sets

********************************************************************************/

void ValueInitialize(void)
{
	Init_Viewer("pMainNotebookGraphViewerCombo");

	Init_Scale("pMainNotebookXaxisScaleCombo");
	Init_Scale("pMainNotebookYAxisScaleCombo");

	Init_Style("pMainNotebookXAxisMainStyleCombo");
	Init_Style("pMainNotebookXAxisAddStyleCombo");
	Init_Style("pMainNotebookYAxisMainStyleCombo");
	Init_Style("pMainNotebookYAxisAddStyleCombo");

	Init_Color("pMainNotebookXAxisMainColorCombo");
	Init_Color("pMainNotebookXAxisAddColorCombo");
	Init_Color("pMainNotebookYAxisMainColorCombo");
	Init_Color("pMainNotebookYAxisAddColorCombo");
}



/********************************************************************************

	Set a value of a graph parameter

********************************************************************************/

int ValueSet(const int iVariable, const char *szValue)
{
	GtkWidget *pWidget = NULL;
	int iResult = FALSE;

	switch (iVariable)
	{
		case VALUE_FILENAME:
			DisplayEntry(
				"pMainNameEntry", 
				szValue
				);
			break;

		case VALUE_VERSION:
			/* TODO */
			break;

		case VALUE_SOURCE:
			DisplayCombo(
				"pMainNotebookGraphSourceCombo", 
				szValue
				);
			break;

		case VALUE_VIEWER:
			DisplayCombo(
				"pMainNotebookGraphViewerCombo", 
				File2Man_Viewer(szValue)
				);
			break;

		case VALUE_BORDER:
			/* TODO */
			break;

		case VALUE_XMIN:
			DisplayEntry(
				"pMainNotebookXAxisScaleXminEntry", 
				File2Man_Number(szValue)
				);
			break;
		
		case VALUE_XMAX:
			DisplayEntry(
				"pMainNotebookXAxisScaleXmaxEntry", 
				File2Man_Number(szValue)
				);
			break;
			
		case VALUE_XAUTO:
			DisplayCheck(
				"pMainNotebookXAxisScaleXminCheck",
				szValue
				);
			break;
			
		case VALUE_XSCALE:
			DisplayCombo(
				"pMainNotebookXaxisScaleCombo", 
				File2Man_Scale(szValue)
				);
			break;

		case VALUE_XMAINDIV:
			DisplayEntry(
				"pMainNotebookXAxisMainDxEntry", 
				File2Man_Number(szValue)
				);
			break;
			
		case VALUE_XMAINAUTO:
			DisplayCheck(
				"pMainNotebookXAxisMainDxCheck",
				szValue
				);
			break;
			
		case VALUE_XMAINSTYLE:
			DisplayCombo(
				"pMainNotebookXAxisMainStyleCombo", 
				File2Man_Style(szValue)
				);
			break;
			
		case VALUE_XMAINCOLOR:
			DisplayCombo(
				"pMainNotebookXAxisMainColorCombo", 
				File2Man_Color(szValue)
				);
			break;
			
		case VALUE_XADDNUMBER:
			DisplayEntry(
				"pMainNotebookXAxisAddNumberEntry", 
				File2Man_Number(szValue)
				);
			break;
			
		case VALUE_XADDAUTO:
			DisplayCheck(
				"pMainNotebookXAxisAddNumberCheck",
				szValue
				);
			break;

		case VALUE_XADDSTYLE:
			DisplayCombo(
				"pMainNotebookXAxisAddStyleCombo", 
				File2Man_Style(szValue)
				);
			break;
			
		case VALUE_XADDCOLOR:
			DisplayCombo(
				"pMainNotebookXAxisAddColorCombo", 
				File2Man_Color(szValue)
				);
			break;
			
		case VALUE_YMIN:
			DisplayEntry(
				"pMainNotebookYAxisScaleYminEntry", 
				File2Man_Number(szValue)
				);
			break;
		
		case VALUE_YMAX:
			DisplayEntry(
				"pMainNotebookYAxisScaleYmaxEntry", 
				File2Man_Number(szValue)
				);
			break;
			
		case VALUE_YAUTO:
			DisplayCheck(
				"pMainNotebookYAxisScaleYminCheck",
				szValue
				);
			break;
			
		case VALUE_YSCALE:
			DisplayCombo(
				"pMainNotebookYAxisScaleCombo", 
				File2Man_Scale(szValue)
				);
			break;

		case VALUE_YMAINDIV:
			DisplayEntry(
				"pMainNotebookYAxisMainDyEntry", 
				File2Man_Number(szValue)
				);
			break;
			
		case VALUE_YMAINAUTO:
			DisplayCheck(
				"pMainNotebookYAxisMainDyButton",
				szValue
				);
			break;
			
		case VALUE_YMAINSTYLE:
			DisplayCombo(
				"pMainNotebookYAxisMainStyleCombo", 
				File2Man_Style(szValue)
				);
			break;
			
		case VALUE_YMAINCOLOR:
			DisplayCombo(
				"pMainNotebookYAxisMainColorCombo", 
				File2Man_Color(szValue)
				);
			break;
			
		case VALUE_YADDNUMBER:
			DisplayEntry(
				"pMainNotebookYAxisAddNumberEntry", 
				File2Man_Number(szValue)
				);
			break;
			
		case VALUE_YADDAUTO:
			DisplayCheck(
				"pMainNotebookYAxisAddNumberCheck",
				szValue
				);
			pWidget = lookup_widget(pMainWindow, "");
			break;

		case VALUE_YADDSTYLE:
			DisplayCombo(
				"pMainNotebookYAxisAddStyleCombo", 
				File2Man_Style(szValue)
				);
			break;
			
		case VALUE_YADDCOLOR:
			DisplayCombo(
				"pMainNotebookYAxisAddColorCombo", 
				File2Man_Color(szValue)
				);
			break;
	}

	ValueValidate(iVariable);

	return iResult;
}



/*******************************************************************************/
/*	Validate a value of a graph parameter                                  */
/*******************************************************************************/

int ValueValidate(const int iVariable)
{
	char *pString = StringCreate();
	int iResult = FALSE;
	BOOL bChecked;


	switch (iVariable)
	{
		case VALUE_FILENAME:
			StringCopy(&pString, ReadEntry("pMainNameEntry"));
			StringCopy(&Graph.szFileName, pString);
			iResult = TRUE;
			break;

		case VALUE_VERSION:
			/* TODO */
			break;

		case VALUE_SOURCE:
			StringCopy(&pString, ReadCombo("pMainNotebookGraphSourceCombo"));
			StringCopy(&Graph.szDataSource, pString);
			DisplayValidity("pMainNotebookGraphSourceCombo", TRUE);
			iResult = TRUE;
			break;

		case VALUE_VIEWER:
			StringCopy(&pString, ReadCombo("pMainNotebookGraphViewerCombo"));
			if (!IsValid_Viewer(pString))
				DisplayValidity("pMainNotebookGraphSourceCombo", FALSE);
			else
			{
				DisplayValidity("pMainNotebookGraphSourceCombo", TRUE);
				StringCopy(&Graph.szViewer, Man2File_Viewer(pString));
				iResult = TRUE;
			}
			break;

		case VALUE_BORDER:
			/* TODO */
			break;

		case VALUE_XMIN:
			StringCopy(&pString, ReadEntry("pMainNotebookXAxisScaleXminEntry"));
			if (Graph.dfXmin < Graph.dfXmax)
				DisplayValidity("pMainNotebookXAxisScaleXminEntry", FALSE);
			else
			{
				DisplayValidity("pMainNotebookXAxisScaleXminEntry", TRUE);
				DisplayValidity("pMainNotebookXAxisScaleXmaxEntry", TRUE);
				Graph.dfXmin = String2Float(Man2File_Number(pString));
				iResult = TRUE;
			}
			break;
		
		case VALUE_XMAX:
			StringCopy(&pString, ReadEntry("pMainNotebookXAxisScaleXmaxEntry"));
			if (Graph.dfXmin < Graph.dfXmax)
				DisplayValidity("pMainNotebookXAxisScaleXmaxEntry", FALSE);
			else
			{
				DisplayValidity("pMainNotebookXAxisScaleXminEntry", TRUE);
				DisplayValidity("pMainNotebookXAxisScaleXmaxEntry", TRUE);
				Graph.dfXmax = String2Float(Man2File_Number(pString));
				iResult = TRUE;
			}
			break;
			
		case VALUE_XAUTO:
			bChecked = ReadCheck("pMainNotebookXAxisScaleXminCheck");
			if (bChecked)
			{
				DisplaySensitivity("pMainNotebookXAxisScaleXminEntry", FALSE);
				DisplaySensitivity("pMainNotebookXAxisScaleXmaxEntry", FALSE);
			}
			else
			{
				DisplaySensitivity("pMainNotebookXAxisScaleXminEntry", TRUE);
				DisplaySensitivity("pMainNotebookXAxisScaleXmaxEntry", TRUE);
			}
			Graph.bXAuto = bChecked;
			iResult = TRUE;
			break;
			
		case VALUE_XSCALE:
			StringCopy(&pString, ReadCombo("pMainNotebookXaxisScaleCombo"));
			if (!IsValid_Scale(pString))
				DisplayValidity("pMainNotebookXaxisScaleCombo", FALSE);
			else
			{
				DisplayValidity("pMainNotebookXaxisScaleCombo", TRUE);
				StringCopy(&Graph.szXScale, Man2File_Scale(pString));
				iResult = TRUE;
			}
			break;

		case VALUE_XMAINDIV:
			StringCopy(&pString, ReadEntry("pMainNotebookXAxisMainDxEntry"));
			Graph.dfXmainDiv = String2Float(Man2File_Number(pString));
			iResult = TRUE;
			break;
			
		case VALUE_XMAINAUTO:
			bChecked = ReadCheck("pMainNotebookXAxisMainDxCheck");
			if (bChecked)
				DisplaySensitivity("pMainNotebookXAxisMainDxEntry", FALSE);
			else
				DisplaySensitivity("pMainNotebookXAxisMainDxEntry", TRUE);
			Graph.bXmainAuto = bChecked;
			iResult = TRUE;
			break;
			
		case VALUE_XMAINSTYLE:
			StringCopy(&pString, ReadCombo("pMainNotebookXAxisMainStyleCombo"));
			if (!IsValid_Style(pString))
				DisplayValidity("pMainNotebookXAxisMainStyleCombo", FALSE);
			else
			{
				DisplayValidity("pMainNotebookXAxisMainStyleCombo", TRUE);
				StringCopy(&Graph.szXmainStyle, Man2File_Style(pString));
				iResult = TRUE;
			}
			break;
			
		case VALUE_XMAINCOLOR:
			StringCopy(&pString, ReadCombo("pMainNotebookXAxisMainColorCombo"));
			if (!IsValid_Color(pString))
				DisplayValidity("pMainNotebookXAxisMainColorCombo", FALSE);
			else
			{
				DisplayValidity("pMainNotebookXAxisMainColorCombo", TRUE);
				StringCopy(&Graph.szXmainColor, Man2File_Color(pString));
				iResult = TRUE;
			}
			break;
			
		case VALUE_XADDNUMBER:
			StringCopy(&pString, ReadEntry("pMainNotebookXAxisAddNumberEntry"));
			Graph.iXaddNumber = String2Integer(Man2File_Number(pString));
			iResult = TRUE;
			break;
			
		case VALUE_XADDAUTO:
			bChecked = ReadCheck("pMainNotebookXAxisAddNumberCheck");
			if (bChecked)
				DisplaySensitivity("pMainNotebookXAxisAddNumberEntry", FALSE);
			else
				DisplaySensitivity("pMainNotebookXAxisAddNumberEntry", TRUE);
			Graph.bXaddAuto = bChecked;
			iResult = TRUE;
			break;

		case VALUE_XADDSTYLE:
			StringCopy(&pString, ReadCombo("pMainNotebookXAxisAddStyleCombo"));
			if (!IsValid_Style(pString))
				DisplayValidity("pMainNotebookXAxisAddStyleCombo", FALSE);
			else
			{
				DisplayValidity("pMainNotebookXAxisAddStyleCombo", TRUE);
				StringCopy(&Graph.szXaddStyle, Man2File_Style(pString));
				iResult = TRUE;
			}
			break;
			
		case VALUE_XADDCOLOR:
			StringCopy(&pString, ReadCombo("pMainNotebookXAxisAddColorCombo"));
			if (!IsValid_Color(pString))
				DisplayValidity("pMainNotebookXAxisAddColorCombo", FALSE);
			else
			{
				DisplayValidity("pMainNotebookXAxisAddColorCombo", TRUE);
				StringCopy(&Graph.szXaddColor, Man2File_Color(pString));
				iResult = TRUE;
			}
			break;
			
		case VALUE_YMIN:
			StringCopy(&pString, ReadEntry("pMainNotebookYAxisScaleYminEntry"));
			if (Graph.dfYmin < Graph.dfYmax)
				DisplayValidity("pMainNotebookYAxisScaleYminEntry", FALSE);
			else
			{
				DisplayValidity("pMainNotebookYAxisScaleYminEntry", TRUE);
				DisplayValidity("pMainNotebookYAxisScaleYmaxEntry", TRUE);
				Graph.dfYmin = String2Float(Man2File_Number(pString));
				iResult = TRUE;
			}
			break;
		
		case VALUE_YMAX:
			StringCopy(&pString, ReadEntry("pMainNotebookYAxisScaleYmaxEntry"));
			if (Graph.dfYmin < Graph.dfYmax)
				DisplayValidity("pMainNotebookYAxisScaleYmaxEntry", FALSE);
			else
			{
				DisplayValidity("pMainNotebookYAxisScaleYminEntry", TRUE);
				DisplayValidity("pMainNotebookYAxisScaleYmaxEntry", TRUE);
				Graph.dfYmax = String2Float(Man2File_Number(pString));
				iResult = TRUE;
			}
			break;
			
		case VALUE_YAUTO:
			bChecked = ReadCheck("pMainNotebookYAxisScaleYminCheck");
			if (bChecked)
			{
				DisplaySensitivity("pMainNotebookYAxisScaleYminEntry", FALSE);
				DisplaySensitivity("pMainNotebookYAxisScaleYmaxEntry", FALSE);
			}
			else
			{
				DisplaySensitivity("pMainNotebookYAxisScaleYminEntry", TRUE);
				DisplaySensitivity("pMainNotebookYAxisScaleYmaxEntry", TRUE);
			}
			Graph.bYAuto = bChecked;
			iResult = TRUE;
			break;
			
		case VALUE_YSCALE:
			StringCopy(&pString, ReadCombo("pMainNotebookYAxisScaleCombo"));
			if (!IsValid_Scale(pString))
				DisplayValidity("pMainNotebookYAxisScaleCombo", FALSE);
			else
			{
				DisplayValidity("pMainNotebookYAxisScaleCombo", TRUE);
				StringCopy(&Graph.szYScale, Man2File_Scale(pString));
				iResult = TRUE;
			}
			break;

		case VALUE_YMAINDIV:
			StringCopy(&pString, ReadEntry("pMainNotebookYAxisMainDyEntry"));
			Graph.dfYmainDiv = String2Float(Man2File_Number(pString));
			iResult = TRUE;
			break;
			
		case VALUE_YMAINAUTO:
			bChecked = ReadCheck("pMainNotebookYAxisMainDyButton");
			if (bChecked)
				DisplaySensitivity("pMainNotebookYAxisMainDyEntry", FALSE);
			else
				DisplaySensitivity("pMainNotebookYAxisMainDyEntry", TRUE);
			Graph.bYmainAuto = bChecked;
			iResult = TRUE;
			break;
			
		case VALUE_YMAINSTYLE:
			StringCopy(&pString, ReadCombo("pMainNotebookYAxisMainStyleCombo"));
			if (!IsValid_Style(pString))
				DisplayValidity("pMainNotebookYAxisMainStyleCombo", FALSE);
			else
			{
				DisplayValidity("pMainNotebookYAxisMainStyleCombo", TRUE);
				StringCopy(&Graph.szYmainStyle, Man2File_Style(pString));
				iResult = TRUE;
			}
			break;
			
		case VALUE_YMAINCOLOR:
			StringCopy(&pString, ReadCombo("pMainNotebookYAxisMainColorCombo"));
			if (!IsValid_Color(pString))
				DisplayValidity("pMainNotebookYAxisMainColorCombo", FALSE);
			else
			{
				DisplayValidity("pMainNotebookYAxisMainColorCombo", TRUE);
				StringCopy(&Graph.szYmainColor, Man2File_Color(pString));
				iResult = TRUE;
			}
			break;
			
		case VALUE_YADDNUMBER:
			StringCopy(&pString, ReadEntry("pMainNotebookYAxisAddNumberEntry"));
			Graph.iYaddNumber = String2Integer(Man2File_Number(pString));
			iResult = TRUE;
			break;
			
		case VALUE_YADDAUTO:
			bChecked = ReadCheck("pMainNotebookYAxisAddNumberCheck");
			if (bChecked)
				DisplaySensitivity("pMainNotebookYAxisAddNumberEntry", FALSE);
			else
				DisplaySensitivity("pMainNotebookYAxisAddNumberEntry", TRUE);
			Graph.bYaddAuto = bChecked;
			iResult = TRUE;
			break;

		case VALUE_YADDSTYLE:
			StringCopy(&pString, ReadCombo("pMainNotebookYAxisAddStyleCombo"));
			if (!IsValid_Style(pString))
				DisplayValidity("pMainNotebookYAxisAddStyleCombo", FALSE);
			else
			{
				DisplayValidity("pMainNotebookYAxisAddStyleCombo", TRUE);
				StringCopy(&Graph.szYaddStyle, Man2File_Style(pString));
				iResult = TRUE;
			}
			break;
			
		case VALUE_YADDCOLOR:
			StringCopy(&pString, ReadCombo("pMainNotebookYAxisAddColorCombo"));
			if (!IsValid_Color(pString))
				DisplayValidity("pMainNotebookYAxisAddColorCombo", FALSE);
			else
			{
				DisplayValidity("pMainNotebookYAxisAddColorCombo", TRUE);
				StringCopy(&Graph.szYaddColor, Man2File_Color(pString));
				iResult = TRUE;
			}
			break;
	}


	return iResult;
}



/********************************************************************************

	Functions for displaying, reading and validating widgets

********************************************************************************/

static void DisplayEntry(const char *szWidget, const char *szValue)
{
	GtkWidget *pWidget;

	pWidget = lookup_widget(pMainWindow, szWidget);
	if (pWidget == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);

	gtk_entry_set_text(GTK_ENTRY(pWidget), szValue);
	gtk_widget_show(pWidget);
}


static char *ReadEntry(const char *szWidget)
{
	GtkWidget *pWidget;
	static char *pString = NULL;

	if (pString == NULL)
		pString = StringCreate();

	pWidget = lookup_widget(pMainWindow, szWidget);
	if (pWidget == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);

	StringCopy(&pString, gtk_entry_get_text(GTK_ENTRY(pWidget)));

	return pString;
}


static void DisplayCombo(const char *szWidget, const char *szValue)
{
	GtkWidget *pWidget;

	pWidget = lookup_widget(pMainWindow, szWidget);
	if (pWidget == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);

	gtk_entry_set_text(GTK_ENTRY(GTK_COMBO(pWidget)->entry), szValue);
	gtk_widget_show(pWidget);
}


static char *ReadCombo(const char *szWidget)
{
	GtkWidget *pWidget;
	static char *pString = NULL;

	if (pString == NULL)
		pString = StringCreate();

	pWidget = lookup_widget(pMainWindow, szWidget);
	if (pWidget == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);

	StringCopy(&pString, gtk_entry_get_text(GTK_ENTRY(GTK_COMBO(pWidget)->entry)));

	return pString;
}


static void DisplayCheck(const char *szWidget, const char *szValue)
{
	GtkWidget *pWidget;

	pWidget = lookup_widget(pMainWindow, szWidget);
	if (pWidget == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);

	gtk_toggle_button_set_active(
		GTK_TOGGLE_BUTTON(&GTK_CHECK_BUTTON(pWidget)->toggle_button),
		!strcmp(szValue, MSG_CHECKED) ? TRUE : FALSE
		);
	gtk_widget_show(pWidget);
}


static BOOL ReadCheck(const char *szWidget)
{
	GtkWidget *pWidget;
	BOOL bChecked;

	pWidget = lookup_widget(pMainWindow, szWidget);
	if (pWidget == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);

	bChecked = gtk_toggle_button_get_active(
		GTK_TOGGLE_BUTTON(&GTK_CHECK_BUTTON(pWidget)->toggle_button)
		);

	return bChecked;
}


static void DisplayValidity(const char *szWidget, BOOL bValue)
{
	GtkWidget *pWidget;

	pWidget = lookup_widget(pMainWindow, szWidget);
	if (pWidget == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);

	/* TODO */
//	if (!bValue)
//	gtk_entry_set_text(GTK_ENTRY(pWidget), "???");
//	gtk_widget_show(pWidget);
		
}


static void DisplaySensitivity(const char *szWidget, const BOOL bSensitive)
{
	GtkWidget *pWidget;

	pWidget = lookup_widget(pMainWindow, szWidget);
	if (pWidget == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);

	gtk_widget_set_sensitive(
		pWidget,
		bSensitive
		);
}



/********************************************************************************

	Functions for translating colors used in graph files to/from human words

********************************************************************************/

static char *Man2File_Color(const char *szColor)
{
	static char *pColor = NULL;
	if (pColor == NULL)
		pColor = StringCreate();

	if      (!strcmp(szColor, MSG_WHITE  )) StringCopy(&pColor, COLOR_WHITE  );
	else if (!strcmp(szColor, MSG_BLACK  )) StringCopy(&pColor, COLOR_BLACK  );
	else if (!strcmp(szColor, MSG_YELLOW )) StringCopy(&pColor, COLOR_YELLOW );
	else if (!strcmp(szColor, MSG_RED    )) StringCopy(&pColor, COLOR_RED    );
	else if (!strcmp(szColor, MSG_BLUE   )) StringCopy(&pColor, COLOR_BLUE   );
	else if (!strcmp(szColor, MSG_GREEN  )) StringCopy(&pColor, COLOR_GREEN  );
	else if (!strcmp(szColor, MSG_ORANGE )) StringCopy(&pColor, COLOR_ORANGE );
	else if (!strcmp(szColor, MSG_MAGENTA)) StringCopy(&pColor, COLOR_MAGENTA);
	else StringCopy(&pColor, "");

	return pColor;
}


static char *File2Man_Color(const char *szColor)
{
	static char *pColor = NULL;
	if (pColor == NULL)
		pColor = StringCreate();

	if      (!strcmp(szColor, COLOR_WHITE  )) StringCopy(&pColor, MSG_WHITE  );
	else if (!strcmp(szColor, COLOR_BLACK  )) StringCopy(&pColor, MSG_BLACK  );
	else if (!strcmp(szColor, COLOR_YELLOW )) StringCopy(&pColor, MSG_YELLOW );
	else if (!strcmp(szColor, COLOR_RED    )) StringCopy(&pColor, MSG_RED    );
	else if (!strcmp(szColor, COLOR_BLUE   )) StringCopy(&pColor, MSG_BLUE   );
	else if (!strcmp(szColor, COLOR_GREEN  )) StringCopy(&pColor, MSG_GREEN  );
	else if (!strcmp(szColor, COLOR_ORANGE )) StringCopy(&pColor, MSG_ORANGE );
	else if (!strcmp(szColor, COLOR_MAGENTA)) StringCopy(&pColor, MSG_MAGENTA);
	else StringCopy(&pColor, "???");

	return pColor;
}


static BOOL IsValid_Color(const char *szColor)
{
	BOOL bValidity;

	bValidity = (
		   !strcmp(szColor, MSG_WHITE  ) 
		|| !strcmp(szColor, MSG_BLACK  )
		|| !strcmp(szColor, MSG_YELLOW ) 
		|| !strcmp(szColor, MSG_RED    )
		|| !strcmp(szColor, MSG_BLUE   )
		|| !strcmp(szColor, MSG_GREEN  )
		|| !strcmp(szColor, MSG_ORANGE )
		|| !strcmp(szColor, MSG_MAGENTA)
		) 
		? TRUE 
		: FALSE;

	return bValidity;
}


static void Init_Color(const char *szWidget)
{
	GtkWidget *pWidget;
	GList *pList = NULL;

	pWidget = lookup_widget(pMainWindow, szWidget);
	if (pWidget == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);

	pList = g_list_append(pList, MSG_WHITE  );
	pList = g_list_append(pList, MSG_BLACK  );
	pList = g_list_append(pList, MSG_YELLOW );
	pList = g_list_append(pList, MSG_RED    );
	pList = g_list_append(pList, MSG_BLUE   );
	pList = g_list_append(pList, MSG_GREEN  );
	pList = g_list_append(pList, MSG_ORANGE );
	pList = g_list_append(pList, MSG_MAGENTA);
	gtk_combo_set_popdown_strings(GTK_COMBO(pWidget), pList);
}



/********************************************************************************

	Functions for translating styles used in graph files to/from human words

********************************************************************************/

static char *Man2File_Style(const char *szStyle)
{
	static char *pStyle = NULL;
	if (pStyle == NULL)
		pStyle = StringCreate();

	if      (!strcmp(szStyle, MSG_SOLID  )) StringCopy(&pStyle, STYLE_SOLID  );
	else StringCopy(&pStyle, "");

	return pStyle;
}


static char *File2Man_Style(const char *szStyle)
{
	static char *pStyle = NULL;
	if (pStyle == NULL)
		pStyle = StringCreate();

	if      (!strcmp(szStyle, STYLE_SOLID  )) StringCopy(&pStyle, MSG_SOLID  );
	else StringCopy(&pStyle, "???");

	return pStyle;
}


static BOOL IsValid_Style(const char *szStyle)
{
	BOOL bValidity;

	bValidity = (
		   !strcmp(szStyle, MSG_SOLID  ) 
		) 
		? TRUE 
		: FALSE;

	return bValidity;
}


static void Init_Style(const char *szWidget)
{
	GtkWidget *pWidget;
	GList *pList = NULL;

	pWidget = lookup_widget(pMainWindow, szWidget);
	if (pWidget == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);

	pList = g_list_append(pList, MSG_SOLID);
	gtk_combo_set_popdown_strings(GTK_COMBO(pWidget), pList);
}



/********************************************************************************

	Functions for translating scales used in graph files to/from human words

********************************************************************************/

static char *Man2File_Scale(const char *szScale)
{
	static char *pScale = NULL;
	if (pScale == NULL)
		pScale = StringCreate();

	if      (!strcmp(szScale, MSG_LIN    )) StringCopy(&pScale, SCALE_LIN    );
	else if (!strcmp(szScale, MSG_LOG    )) StringCopy(&pScale, SCALE_LOG    );
	else StringCopy(&pScale, "");

	return pScale;
}


static char *File2Man_Scale(const char *szScale)
{
	static char *pScale = NULL;
	if (pScale == NULL)
		pScale = StringCreate();

	if      (!strcmp(szScale, SCALE_LIN    )) StringCopy(&pScale, MSG_LIN    );
	else if (!strcmp(szScale, SCALE_LOG    )) StringCopy(&pScale, MSG_LOG    );
	else StringCopy(&pScale, "???");

	return pScale;
}


static BOOL IsValid_Scale(const char *szScale)
{
	BOOL bValidity;

	bValidity = (
		   !strcmp(szScale, MSG_LIN    ) 
		|| !strcmp(szScale, MSG_LOG    )
		) 
		? TRUE 
		: FALSE;

	return bValidity;
}


static void Init_Scale(const char *szWidget)
{
	GtkWidget *pWidget;
	GList *pList = NULL;

	pWidget = lookup_widget(pMainWindow, szWidget);
	if (pWidget == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);

	pList = g_list_append(pList, MSG_LIN    );
	pList = g_list_append(pList, MSG_LOG    );
	gtk_combo_set_popdown_strings(GTK_COMBO(pWidget), pList);
}



/********************************************************************************

	Functions for translating viewers used in graph files to/from human words

********************************************************************************/

static char *Man2File_Viewer(const char *szViewer)
{
	static char *pViewer = NULL;
	if (pViewer == NULL)
		pViewer = StringCreate();

	if      (!strcmp(szViewer, MSG_GWAVE)) StringCopy(&pViewer, VIEWER_GWAVE);
	else StringCopy(&pViewer, "");

	return pViewer;
}


static char *File2Man_Viewer(const char *szViewer)
{
	static char *pViewer = NULL;
	if (pViewer == NULL)
		pViewer = StringCreate();

	if      (!strcmp(szViewer, VIEWER_GWAVE)) StringCopy(&pViewer, MSG_GWAVE);
	else StringCopy(&pViewer, "???");

	return pViewer;
}


static BOOL IsValid_Viewer(const char *szViewer)
{
	BOOL bValidity;

	bValidity = (
		   !strcmp(szViewer, MSG_GWAVE) 
		) 
		? TRUE 
		: FALSE;

	return bValidity;
}


static void Init_Viewer(const char *szWidget)
{
	GtkWidget *pWidget;
	GList *pList = NULL;

	pWidget = lookup_widget(pMainWindow, szWidget);
	if (pWidget == NULL)
		FatalError(__FILE__, __LINE__, __DATE__);

	pList = g_list_append(pList, MSG_GWAVE);
	gtk_combo_set_popdown_strings(GTK_COMBO(pWidget), pList);
}



/********************************************************************************

	Functions for translating numbers used in graph files to/from human words

********************************************************************************/

static char *Man2File_Number(const char *szNumber)
{
	/* TODO */
	return (char *) szNumber;
}


static char *File2Man_Number(const char *szNumber)
{
	/* TODO */
	return (char *) szNumber;
}

/* not currently used */
#if 0
static BOOL IsValid_Number(const char *szNumber)
{
	BOOL bValidity;

	bValidity = TRUE;
	/* TODO */

	return bValidity;
}
#endif


/********************************************************************************

	Callbacks called when a value in a field changes

********************************************************************************/

void
on_pMainNotebookGraphSourceEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_SOURCE);
}


void
on_pMainNotebookGraphTypeEntry_changed (GtkEditable     *editable,
                                        gpointer         user_data)
{
	/* TODO */
}


void
on_pMainNotebookGraphViewerEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_VIEWER);
}


void
on_pMainNotebookXAxisScaleXminCheck_toggled
                                        (GtkToggleButton *togglebutton,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_XAUTO);
}


void
on_pMainNotebookXAxisScaleXmaxEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_XMAX);
}


void
on_pMainNotebookXaxisScaleEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_XSCALE);
}


void
on_pMainNotebookXAxisScaleXminEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_XMIN);
}


void
on_pMainNotebookXAxisMainDxCheck_toggled
                                        (GtkToggleButton *togglebutton,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_XMAINAUTO);
}


void
on_pMainNotebookXAxisMainColorEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_XMAINCOLOR);
}


void
on_pMainNotebookXAxisMainStyleEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_XMAINSTYLE);
}


void
on_pMainNotebookXAxisMainDxEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_XMAINDIV);
}


void
on_pMainNotebookXAxisAddNumberCheck_toggled
                                        (GtkToggleButton *togglebutton,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_XADDAUTO);
}


void
on_pMainNotebookXAxisAddStyleEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_XADDSTYLE);
}


void
on_pMainNotebookXAxisAddColorEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_XADDCOLOR);
}


void
on_pMainNotebookXAxisAddNumberEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_XADDNUMBER);
}


void
on_pMainNotebookYAxisScaleYminCheck_toggled
                                        (GtkToggleButton *togglebutton,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_YAUTO);
}


void
on_pMainNotebookYAxisScaleYmaxEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_YMAX);
}


void
on_pMainNotebookYAxisScaleEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_YSCALE);
}


void
on_pMainNotebookYAxisScaleYminEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_YMIN);
}


void
on_pMainNotebookYAxisMainDyButton_toggled
                                        (GtkToggleButton *togglebutton,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_YMAINAUTO);
}


void
on_pMainNotebookYAxisMainColorEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_YMAINCOLOR);
}


void
on_pMainNotebookYAxisMainStyleEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_YMAINSTYLE);
}


void
on_pMainNotebookYAxisMainDyEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_YMAINDIV);
}


void
on_pMainNotebookYAxisAddNumberCheck_toggled
                                        (GtkToggleButton *togglebutton,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_YADDAUTO);
}


void
on_pMainNotebookYAxisAddStyleEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_YADDSTYLE);
}


void
on_pMainNotebookYAxisAddColorEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_YADDCOLOR);
}


void
on_pMainNotebookYAxisAddNumberEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data)
{
	ValueValidate(VALUE_YADDNUMBER);
}
