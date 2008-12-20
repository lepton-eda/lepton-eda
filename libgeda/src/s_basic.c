/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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
#include <ctype.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! this is modified here and in o_list.c */
int global_sid=0;

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void error_if_called(void)
{
	fprintf(stderr, "Somebody called error_if_called!\n");
	g_assert(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void exit_if_null(void *ptr) 
{
  if (ptr == NULL) {
    fprintf(stderr, "gEDA: Got NULL ptr!, please e-mail maintainer\n");
    g_assert(0);
    exit(-1);
  }	
}


/*! \brief Initialize an already-allocated object.
 *  \par Function Description
 *  Initializes the members of the OBJECT structure.
 *
 *  \param [in] new_node  A pointer to an allocated OBJECT
 *  \param [in] type      The object type; one of the OBJ_* constants.
 *  \param [in] name      A prefix for the object's session-unique name.
 *  \return A pointer to the initialized object.
 */
OBJECT *s_basic_init_object(OBJECT *new_node, int type, char const *name)
{
  /* setup sid */
  new_node->sid = global_sid++;
  new_node->type = type;

  /* Setup the name */
  new_node->name = g_strdup_printf("%s.%d", name, new_node->sid);

  /* Setup the bounding box */
  new_node->w_top = 0;
  new_node->w_left = 0;
  new_node->w_right = 0;
  new_node->w_bottom = 0;
  new_node->w_bounds_valid = FALSE;

  /* Setup line/circle structs */
  new_node->line = NULL;
  new_node->path = NULL;
  new_node->circle = NULL;
  new_node->arc = NULL;
  new_node->box = NULL;
  new_node->picture = NULL;
  new_node->text = NULL;
  new_node->complex = NULL;

  new_node->tiles = NULL;

  new_node->conn_list = NULL;

  new_node->visited = 0;
	
  new_node->complex_basename = NULL;
  new_node->complex_parent = NULL;
		
  /* Setup the color */
  new_node->color = WHITE;
  new_node->saved_color = -1;
  new_node->selected = FALSE;
  new_node->dont_redraw = FALSE;
  new_node->locked_color = -1;

  new_node->bus_ripper_direction = 0;

  new_node->action_func = error_if_called; 
  new_node->sel_func = error_if_called; 
  new_node->draw_func = error_if_called; 

  new_node->line_end = END_NONE;
  new_node->line_type = TYPE_SOLID;
  new_node->line_width = 0;
  new_node->line_space = 0;
  new_node->line_length = 0;
  new_node->fill_width = 0;
  new_node->fill_angle1 = 0;
  new_node->fill_angle2 = 0;
  new_node->fill_pitch1 = 0;
  new_node->fill_pitch2 = 0;
	
  new_node->attribs = NULL;
  new_node->attached_to = NULL;
  new_node->copied_to = NULL;
  new_node->show_name_value = SHOW_NAME_VALUE;
  new_node->visibility = VISIBLE;

  new_node->pin_type = PIN_TYPE_NET;
  new_node->whichend = -1;

  return(new_node);
}


/*! \brief Helper to allocate and initialise an object.
 *
 *  \par Function Description
 *  Allocates memory for an OBJECT and then calls s_basic_init_object() on it.
 *
 *  \param [in] type      The sub-type of the object to create; one of the OBJ_* constants.
 *  \param [in] prefix    The name prefix for the session-unique object name.
 *  \return A pointer to the fully constructed OBJECT.
 */
OBJECT *s_basic_new_object(int type, char const *prefix)
{
  return s_basic_init_object(g_malloc(sizeof (OBJECT)), type, prefix);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void print_struct_forw (GList *list)
{
  OBJECT *o_current=NULL;
  GList *iter;

  iter = list;
  printf("TRYING to PRINT\n");
  while (iter != NULL) {
    o_current = (OBJECT *)iter->data;
    printf("Name: %s\n", o_current->name);
    printf("Type: %d\n", o_current->type);
    printf("Sid: %d\n", o_current->sid);

    if (o_current->type == OBJ_COMPLEX || o_current->type == OBJ_PLACEHOLDER) {
      print_struct_forw(o_current->complex->prim_objs);
    }

    o_attrib_print (o_current->attribs);

    printf("----\n");
    iter = g_list_next (iter);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void print_struct(OBJECT *ptr)
{
  OBJECT *o_current=NULL;

  o_current = ptr;

  if (o_current != NULL) {
    printf("Name: %s\n", o_current->name);
    printf("Type: %d\n", o_current->type);
    printf("Sid: %d\n", o_current->sid);
    if (o_current->line != NULL) {
      printf("Line points.x1: %d\n", o_current->line->x[0]);
      printf("Line points.y1: %d\n", o_current->line->y[0]);
      printf("Line points.x2: %d\n", o_current->line->x[1]);
      printf("Line points.y2: %d\n", o_current->line->y[1]);
    }

    o_attrib_print (o_current->attribs);

    printf("----\n");
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
s_delete_object(TOPLEVEL *toplevel, OBJECT *o_current)
{
  if (o_current != NULL) {
    s_conn_remove_object (toplevel, o_current);

    if (o_current->attached_to != NULL) {
      /* do the actual remove */
      o_attrib_remove(&o_current->attached_to->attribs, o_current);
    }

    if (toplevel->page_current->object_lastplace == o_current) {
      toplevel->page_current->object_lastplace = NULL;
    }

    if (o_current->line) {
      /*	printf("sdeleting line\n");*/
      g_free(o_current->line);

      /* yes this object might be in the tile system */
      s_tile_remove_object(o_current);
    }
    o_current->line = NULL;

    if (o_current->path) {
      g_free(o_current->path);
    }
    o_current->path = NULL;

    /*	printf("sdeleting circle\n");*/
    g_free(o_current->circle);
    o_current->circle = NULL;

    /*	printf("sdeleting arc\n");*/
    g_free(o_current->arc);
    o_current->arc = NULL;

    /*	printf("sdeleting box\n");*/
    g_free(o_current->box);
    o_current->box = NULL;

    if (o_current->picture) {
      /*	printf("sdeleting picture\n");*/

      g_free(o_current->picture->file_content);
      if (o_current->picture->original_picture)
	g_object_unref(o_current->picture->original_picture);
      if (o_current->picture->displayed_picture)
	g_object_unref(o_current->picture->displayed_picture);

      g_free(o_current->picture->filename);
      g_free(o_current->picture);
    }
    o_current->picture = NULL;

    if (o_current->text) {
      /*printf("sdeleting text->string\n");*/
      g_free(o_current->text->string); 
      o_current->text->string = NULL;
      g_free(o_current->text->disp_string);

      if (o_current->text->prim_objs) {
				/*printf("sdeleting text complex\n");*/
        s_delete_object_glist (toplevel, o_current->text->prim_objs);
        o_current->text->prim_objs = NULL;
      }

      /*	printf("sdeleting text\n");*/
      g_free(o_current->text);
    }
    o_current->text = NULL;

    /*	printf("sdeleting name\n");*/
    g_free(o_current->name);
    o_current->name = NULL;


    /*	printf("sdeleting complex_basename\n");*/
    g_free(o_current->complex_basename); 
    o_current->complex_basename = NULL;

    if (o_current->complex) {

      if (o_current->complex->prim_objs) {
        /* printf("sdeleting complex->primitive_objects\n");*/
        s_delete_object_glist (toplevel, o_current->complex->prim_objs);
        o_current->complex->prim_objs = NULL;
      }

      g_free(o_current->complex);
      o_current->complex = NULL;
    }

    if (o_current->attribs) {
      o_attrib_free_all(toplevel, o_current->attribs);
    }
    o_current->attribs = NULL;

    g_free(o_current);	/* assuming it is not null */

    o_current=NULL;		/* misc clean up */
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* deletes everything include the GList */
void
s_delete_object_glist(TOPLEVEL *toplevel, GList *list)
{
  OBJECT *o_current=NULL;
  GList *ptr;

  ptr = g_list_last(list);

  /* do the delete backwards */
  while(ptr != NULL) {
    o_current = (OBJECT *) ptr->data;
    s_delete_object(toplevel, o_current);
    ptr = g_list_previous (ptr);
  }
  g_list_free(list);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* used by o_text_read */
char *remove_nl(char *string)
{
  int i;

  if (!string)
    return NULL;
  
  i = 0;
  while(string[i] != '\0' && string[i] != '\n' && string[i] != '\r') {
    i++; 
  }

  string[i] = '\0';

  return(string);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* used by o_text_read */
char *remove_last_nl(char *string)
{
  int len;

  if (!string)
    return NULL;		

  len = strlen(string);
  if (string[len-1] == '\n' || string[len-1] == '\r')
    string[len-1] = '\0';
     
  return(string);
}

/*! \brief Expand environment variables in string.
 *  \par Function Description
 *  This function returns the passed string with environment variables
 *  expanded.
 *
 *  The invocations of environment variable MUST be in the form
 *  '${variable_name}', '$variable_name' is not valid here. Environment
 *  variable names consists solely of letters, digits and '_'. It is
 *  possible to escape a '$' character in the string by repeating it
 *  twice.
 *
 *  It outputs error messages to console and leaves the malformed and
 *  bad variable names in the returned string.
 *
 *  \param [in] string The string with variables to expand.
 *  \return A newly-allocated string with variables expanded or NULL
 *  if input string was NULL.
 */
gchar*
s_expand_env_variables (const gchar *string)
{
  GString *gstring;
  gint i;

  if (string == NULL) {
    return NULL;
  }

  gstring = g_string_sized_new (strlen (string));
  i = 0;
  while (TRUE) {
    gint start;

    start = i;
    /* look for end of string or possible variable name start */
    while (string[i] != '\0' && string[i] != '$') i++;
    g_string_append_len (gstring, string + start, i - start);
    if (string[i] == '\0') {
      /* end of string, return built string */
      return g_string_free (gstring, FALSE);
    }

    i++;
    switch (string[i]) {
        case ('{'):
          /* look for the end of the variable name */
          start = i;
          while (string[i] != '\0' && string[i] != '}') i++;
          if (string[i] == '\0') {
            /* problem: no closing '}' to variable */
            fprintf (stderr,
                     "Found malformed environment variable in '%s'\n",
                     string);
            g_string_append (gstring, "$");
            g_string_append_len (gstring, string + start, i - start + 1);
          } else {
            gint j;

            /* discard "${" */
            start = start + 1;
            /* test characters of variable name */
            for (j = start;
                 j < i && (g_ascii_isalnum (string[j]) || string[j] == '_');
                 j++);
            if (i != j) {
              /* illegal character detected in variable name */
              fprintf (stderr,
                       "Found bad character [%c] in variable name.\n",
                       string[j]);
              g_string_append (gstring, "${");
              g_string_append_len (gstring, string + start, i - start + 1);
            } else {
              /* extract variable name from string and expand it */
              gchar *variable_name = g_strndup (string + start, i - start);
              const gchar *env = g_getenv (variable_name);
              g_free (variable_name);
              g_string_append (gstring, (env == NULL) ? "" : env);
            }
            i++;
          }
          break;

        case ('$'):
          /* an escaped '$' */
          g_string_append_c (gstring, string[i++]);
          break;
          
        default:
          /* an isolated '$', put it in output */
          g_string_append_c (gstring, '$');
    }
  }

  /* never reached */
  return NULL;
}
