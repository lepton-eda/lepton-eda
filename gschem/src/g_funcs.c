/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2000 Ales V. Hvezda
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

#include <config.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

SCM g_funcs_print(SCM filename)
{
	char *string = gh_scm2newstr(filename, NULL);

	if (string == NULL) {
		return SCM_BOOL_T;
	}

	if (output_filename) {
		f_print(global_window_current, output_filename);
	} else  {
		f_print(global_window_current, string);
	}

	free(string);
	return SCM_BOOL_T;
}

SCM g_funcs_exit(void)
{
	exit(0);
}

SCM g_funcs_use_rc_values(void)
{
	i_vars_set(global_window_current);
	return SCM_BOOL_T;
}

static char *key_value_string = NULL;

/* there is no string size checking here... so if it core dumps... DOH! */
/* TODO: fix this ^^^^^^^^^^^^^ */
/* it's actually pretty usable right now, but needs to be reviewed again */
SCM g_funcs_key_name(SCM keystring)
{
	char *string = gh_scm2newstr(keystring, NULL);

	if (string == NULL) {
		return SCM_BOOL_T;
	}

	if (key_value_string != NULL) {
		x_dialog_hotkeys_fill(key_value_string);
		free(key_value_string); 
		key_value_string = NULL;
	}

	/* the 25 is: null char, a few spaces, and the characters */
	key_value_string = (char *) malloc(sizeof(char)*(
				            strlen(string)+25));

	sprintf(key_value_string, "%s :", string);

	free(string);
	return SCM_BOOL_T;
}


SCM g_funcs_key_value(SCM keystring)
{
	char *temp;
	char *string = gh_scm2newstr(keystring, NULL);

	if (string == NULL) {
		return SCM_BOOL_T;
	}


	if (key_value_string == NULL) {
		fprintf(stderr, "Ack! something got fouled up with the keymappings!\n");
		exit(-1);
	}

	temp = (char *) malloc(sizeof(char)*(strlen(key_value_string)+
			        strlen(string)+5));

	sprintf(temp, "%s %s", key_value_string, string);

	free(key_value_string);
	key_value_string = temp;
	
	free(string);
	return SCM_BOOL_T;
}

SCM g_funcs_key_done(void)
{
	x_dialog_hotkeys_fill(key_value_string);
	free(key_value_string);
	key_value_string = NULL;
	return SCM_BOOL_T;
}

/* all of the declaration part is copied from some other c-code of
 * gEDA gschem. 
 * I don't really know, whether this all are necessary or not, but 
 * it works :-). */

/* get names from all objects of current_page, which */
/* selected-flags are true. */

SCM get_selected_component_attributes(gpointer data,		
				      guint callback_action,	
				      GtkWidget *widget)
{
  OBJECT *obj;
  PAGE *p;
  
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  SCM list = SCM_EOL;
  /*NETLIST *nl_current = NULL; */

  /*nl_current = netlist_head; */
  p = w_current->page_current;
  obj = p->object_head;

  s_scratch_string_init();
  
  while (obj != NULL)
    {
      if (obj->selected) 
	{
	  
	  /*if (obj->type == OBJ_COMPLEX)
	    {
	      if (s_scratch_string_fill(obj->complex_basename)) 
		{
		  list = gh_cons( gh_str2scm (obj->complex_basename, 
					      strlen(obj->complex_basename)), 
				  list);	
		}
	    }
	    else*/
	    {
	      if (obj->text && obj->text->string)
		{
		  if (s_scratch_string_fill(obj->text->string))
		    {
		      list = gh_cons( gh_str2scm (obj->text->string, 
						  strlen(obj->text->string)), 
				      list);	
		    } 
		}
	    }
	}
      obj = obj->next;
    }
  s_scratch_string_free();
  return(list);
}



/* this function returns the whole filename of the current schematic. */
/* specially, the page_filename of the current_page */

SCM get_selected_filename(gpointer data,		
			  guint callback_action,	
			  GtkWidget *widget)
{
  SCM return_value;
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  
  exit_if_null(w_current);
  
  return_value = gh_str2scm(w_current->page_current->page_filename,
		  	    strlen(w_current->page_current->page_filename));
  return(return_value);
}

