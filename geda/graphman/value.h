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

#ifndef __VALUE_H_INCLUDED
#define __VALUE_H_INCLUDED



/* list of variables */
#define VALUE_FILENAME      1
#define VALUE_VERSION       2
#define VALUE_SOURCE        3
#define VALUE_VIEWER        4
#define VALUE_BORDER        5
#define VALUE_XMIN          6
#define VALUE_XMAX          7
#define VALUE_XAUTO         8
#define VALUE_XSCALE        9
#define VALUE_XMAINDIV      10
#define VALUE_XMAINAUTO     11
#define VALUE_XMAINSTYLE    12
#define VALUE_XMAINCOLOR    13
#define VALUE_XADDNUMBER    14
#define VALUE_XADDAUTO      15
#define VALUE_XADDSTYLE     16
#define VALUE_XADDCOLOR     17
#define VALUE_YMIN          18
#define VALUE_YMAX          19
#define VALUE_YAUTO         20
#define VALUE_YSCALE        21
#define VALUE_YMAINDIV      22
#define VALUE_YMAINAUTO     23
#define VALUE_YMAINSTYLE    24
#define VALUE_YMAINCOLOR    25
#define VALUE_YADDNUMBER    26
#define VALUE_YADDAUTO      27
#define VALUE_YADDSTYLE     28
#define VALUE_YADDCOLOR     29



/* public functions */
void ValueInitialize(void);
int ValueSet(const int iVariable, const char *szValue);
char *ValueGet(const int iVariable);
int ValueValidate(const int iVariable);



void
on_pMainNotebookGraphSourceEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data);

void
on_pMainNotebookGraphTypeEntry_changed (GtkEditable     *editable,
                                        gpointer         user_data);

void
on_pMainNotebookGraphViewerEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data);

void
on_pMainNotebookXAxisScaleXminCheck_toggled
                                        (GtkToggleButton *togglebutton,
                                        gpointer         user_data);

void
on_pMainNotebookXAxisScaleXmaxEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data);

void
on_pMainNotebookXaxisScaleEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data);

void
on_pMainNotebookXAxisScaleXminEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data);

void
on_pMainNotebookXAxisMainDxCheck_toggled
                                        (GtkToggleButton *togglebutton,
                                        gpointer         user_data);

void
on_pMainNotebookXAxisMainColorEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data);

void
on_pMainNotebookXAxisMainStyleEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data);

void
on_pMainNotebookXAxisMainDxEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data);

void
on_pMainNotebookXAxisAddNumberCheck_toggled
                                        (GtkToggleButton *togglebutton,
                                        gpointer         user_data);

void
on_pMainNotebookXAxisAddStyleEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data);

void
on_pMainNotebookXAxisAddColorEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data);

void
on_pMainNotebookXAxisAddNumberEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data);

void
on_pMainNotebookYAxisScaleYminCheck_toggled
                                        (GtkToggleButton *togglebutton,
                                        gpointer         user_data);

void
on_pMainNotebookYAxisScaleYmaxEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data);

void
on_pMainNotebookYAxisScaleEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data);

void
on_pMainNotebookYAxisScaleYminEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data);

void
on_pMainNotebookYAxisMainDyButton_toggled
                                        (GtkToggleButton *togglebutton,
                                        gpointer         user_data);

void
on_pMainNotebookYAxisMainColorEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data);

void
on_pMainNotebookYAxisMainStyleEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data);

void
on_pMainNotebookYAxisMainDyEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data);

void
on_pMainNotebookYAxisAddNumberCheck_toggled
                                        (GtkToggleButton *togglebutton,
                                        gpointer         user_data);

void
on_pMainNotebookYAxisAddStyleEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data);

void
on_pMainNotebookYAxisAddColorEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data);

void
on_pMainNotebookYAxisAddNumberEntry_changed
                                        (GtkEditable     *editable,
                                        gpointer         user_data);



#endif /* __VALUE_H_INCLUDED */
