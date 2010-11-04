/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003-2010 Stuart D. Brorson.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */

/*! \file
 *  \brief Import/export functions
 *
 * This file holds fcns used for import/export of attribute sheets.
 * At the moment, this is only component sheets.
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <math.h>

/*------------------------------------------------------------------
 * Gattrib specific includes
 *------------------------------------------------------------------*/
#include <libgeda/libgeda.h>       /* geda library fcns  */
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


/* ===================  Public Functions  ====================== */
/* ------------------------------------------------------------- */
/* \brief Export components to CSV
 *
 * This function is invoked when the user selects file ->
 * export from the pull-down menu.  It writes out a CSV file 
 * of the design for external processing.
 *
 * \param filename The name of the file to export to
 */
void f_export_components(gchar *filename)
{
  gint cur_page;
  gint num_rows;
  gint num_cols;
  gint i,j;

  gchar *text;
  FILE *fp;

  /* -----  Check that we have a component ----- */
  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  if (cur_page != 0) {
    /* We only export the component table */
    /* XXXXX  Maybe throw up error message in window instead? */
    x_dialog_unimplemented_feature(); 
    return;    

  }

  /* -----  First try to open file for writing ----- */

#ifdef DEBUG
  printf("In f_export_components, trying to open %s.\n", filename);
#endif
  fp = fopen(filename, "wb");
  if (fp == NULL) {
    s_log_message("o_save: Could not open [%s]\n", filename);
    /* XXXXX Throw up error message  in window */
    return;
  }


  /* -----  Now write out data  ----- */    
  num_rows = sheet_head->comp_count;
  num_cols = sheet_head->comp_attrib_count;

  /*  First export top row -- attribute names  */
  /*  Print out "refdes" since that's always the first column  */
  fprintf(fp, "refdes, ");
  /*  Print out optional attrib names  */
  for (j = 0; j < num_cols-1; j++) {
    text = g_strdup( s_string_list_get_data_at_index(
		       sheet_head->master_comp_attrib_list_head, j) );
    fprintf(fp, "%s, ", text);
    g_free(text);
  }
  /*  Print out last attrib name with no comma and with \n.  */
  text = g_strdup( s_string_list_get_data_at_index(
		     sheet_head->master_comp_attrib_list_head, j) );
  fprintf(fp, "%s\n", text);
  g_free(text);


  /*  Now export the contents of the sheet  */
  for (i = 0; i < num_rows; i++) {

    /*  First output the component refdes  */
    text = g_strdup( s_string_list_get_data_at_index(
		       sheet_head->master_comp_list_head, i) );
#ifdef DEBUG
  printf("In f_export_components, getting refes, i = %d.\n", i);
  printf("In f_export_components, output component refdes %s.\n", text);
#endif
    fprintf(fp, "%s, ",text);
    g_free(text);

    /*  Now export the attrib values for first n-1 cols */
    for (j = 0; j < num_cols-1; j++) {
      if ( (sheet_head->component_table)[i][j].attrib_value ) { /* found a string */
        /* make a copy of the text, escaping any special chars, like " */
        text = (gchar *) g_strescape( (sheet_head->component_table)[i][j].attrib_value, "" );
#ifdef DEBUG
  printf("In f_export_components, output attribute %s.\n", text);
#endif
        /* if there's a comma anywhere in the field, wrap the field in " */
        gboolean havecomma = ( g_strstr_len(text, -1, ",") != NULL );
        if(havecomma) fprintf(fp, "\"");
        fprintf(fp, "%s", text);
        if(havecomma) fprintf(fp, "\"");
        fprintf(fp, ", ");

	g_free(text);
      } else {                                                  /* no attrib string */
#ifdef DEBUG
  printf("In f_export_components, output blank attrib space\n");
#endif
	fprintf(fp, ", ");
      }
    }  /* end of for over cols  */
    /* Now export attrib value for last col (with no "," and with "\n" */
    if ( (sheet_head->component_table)[i][j].attrib_value ) { /* found a string */
      /* make a copy of the text, escaping any special chars, like " */
      text = (gchar *) g_strescape( (sheet_head->component_table)[i][j].attrib_value, "" );
#ifdef DEBUG
  printf("In f_export_components, output final attribute %s.\n", text);
#endif
      /* if there's a comma anywhere in the field, wrap the field in " */
      gboolean havecomma = ( g_strstr_len(text, -1, ",") != NULL );
      if(havecomma) fprintf(fp, "\"");
      fprintf(fp, "%s", text);
      if(havecomma) fprintf(fp, "\"");
      fprintf(fp, "\n");

      g_free(text);
    } else {                                                  /* no attrib string */
#ifdef DEBUG
  printf("In f_export_components, output blank at end of line.\n");
#endif
      fprintf(fp, "\n");
    }
#ifdef DEBUG
  printf("In f_export_components, Go to next row.\n");
#endif
  }  /* close of for over rows */

  fclose(fp);
  
return;
}
