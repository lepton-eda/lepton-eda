/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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
#include <ctype.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif
#ifndef HAVE_VSNPRINTF
#include <stdarg.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <gtk/gtk.h>
#include <libguile.h>

#include "defines.h"
#include "struct.h"
#include "defines.h"
#include "globals.h"
#include "o_types.h"
#include "colors.h"

#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! this is modified here and in o_list.c */
int global_sid=0;

#define NUMCOLORS 9  /*!< */

/*! \brief */
struct st_old_colors {
        char *name;
        int value;
};

/*! \brief
 * Colors must be in alphabetical order
 * be sure that you update above define
 */
struct st_old_colors old_colors[] = {
	{ "black", 0 },
	{ "blue", 4 },
	{ "cyan", 6 },
	{ "green", 3 },
	{ "grey", 7 }, 
	{ "grey90", 8 },
	{ "red", 2 },
	{ "white", 1 },
	{ "yellow", 5 },
};

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void error_if_called(void)
{
	fprintf(stderr, "Somebody called error_if_called!\n");
	assert(0);
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
    assert(0);
    exit(-1);
  }	
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* hack rename this to be s_return_tail */
/* update object_tail or any list of that matter */
OBJECT *return_tail(OBJECT *head)
{
  OBJECT *o_current=NULL;
  OBJECT *ret_struct=NULL;

  o_current = head;
  while ( o_current != NULL ) { /* goto end of list */
    ret_struct = o_current;	
    o_current = o_current->next;
  }
	
  return(ret_struct);	
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* hack rename this to be s_return_head */
/* update object_tail or any list of that matter */
OBJECT *return_head(OBJECT *tail)
{
  OBJECT *o_current=NULL;
  OBJECT *ret_struct=NULL;

  o_current = tail;
  while ( o_current != NULL ) { /* goto end of list */
    ret_struct = o_current;	
    o_current = o_current->prev;
  }
	
  return(ret_struct);	
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
OBJECT *s_basic_init_object( char *name ) 
{
  OBJECT *new_node;

  new_node = (OBJECT *) g_malloc(sizeof(OBJECT));	

  if (new_node == NULL) {
    fprintf(stderr, "Could not perform malloc; something is broken or increase your process limits\n");
    exit(-1);
  }

  /* setup sid */
  new_node->sid = global_sid++;
  new_node->type = -1;

  /* Setup the name */
  /*! \todo get rid of magic number 16 that's the size of new_node->sid, */
  new_node->name = (char *) g_malloc(sizeof(char)*(strlen(name)+16));
  sprintf(new_node->name, "%s.%d", name, new_node->sid);

  /* Setup the bounding box */
  new_node->w_top = 0;
  new_node->w_left = 0;
  new_node->w_right = 0;
  new_node->w_bottom = 0;

  /* Setup line/circle structs */
  new_node->line = NULL;
  new_node->circle = NULL;
  new_node->arc = NULL;
  new_node->box = NULL;
  new_node->picture = NULL;
  new_node->text = NULL;
  new_node->complex = NULL;

  new_node->tile_locs = NULL;

  new_node->conn_list = NULL;

  new_node->visited = 0;
	
  new_node->complex_basename = NULL;
  new_node->complex_parent = NULL;
		
  /* Setup the color */
  new_node->color = WHITE;
  new_node->saved_color = -1;
  new_node->selected = FALSE;
  new_node->locked_color = -1;
  new_node->draw_grips = FALSE;

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
  new_node->attribute = 0; 
  new_node->show_name_value = SHOW_NAME_VALUE;
  new_node->visibility = VISIBLE;

  new_node->pin_type = PIN_TYPE_NET;
  new_node->whichend = -1;
	
  /* Setup link list stuff */
  new_node->prev = NULL;
  new_node->next = NULL;

  return(new_node);
}

OBJECT *s_basic_link_object( OBJECT *new_node, OBJECT *ptr ) 
{
  /* should never happen, but could */
  if (new_node == NULL) {
    fprintf(stderr, "Got a null new_node in link_object\n");
    return(ptr);
  }

  if (ptr == NULL) {
    new_node->prev = NULL; /* setup previous link */
    return(new_node);
  } else {
    new_node->prev = ptr; /* setup previous link */
    ptr->next = new_node;
    return(ptr->next);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void print_struct_forw(OBJECT *ptr)
{
  OBJECT *o_current=NULL;
  ATTRIB *attr=NULL;
  int i;

  o_current = ptr;

  if (o_current == NULL) {

    printf("AGGGGGGGGGGG NULLLLL PRINT\n");
  }
  printf("TRYING to PRINT\n");
  while (o_current != NULL) {
    printf("Name: %s\n", o_current->name);
    printf("Type: %d\n", o_current->type);
    printf("Sid: %d\n", o_current->sid);

    if (o_current->type == OBJ_COMPLEX || o_current->type == OBJ_PLACEHOLDER) {
      print_struct_forw(o_current->complex->prim_objs);
    }

	
    if (o_current->attribs) {
      attr = o_current->attribs;
      i = 0;
      while (attr != NULL) {
        if (attr->object != NULL) 
          printf("%d attribute %s\n", i, attr->object->name);
        attr = attr->next;
      }    
    }

    printf("----\n");
    o_current = o_current->next;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void print_struct_back(OBJECT *ptr)
{
  OBJECT *o_current=NULL;

  o_current = ptr;

  while (o_current != NULL) {
    printf("Name: %s\n", o_current->name);
    printf("Type: %d\n", o_current->type);
    printf("Sid: %d\n", o_current->sid);
    printf("----\n");
    o_current = o_current->prev;
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
  ATTRIB *attr=NULL;
  int i;

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

    if (o_current->attribs) {
      attr = o_current->attribs;
      i = 0;
      while (attr != NULL) {
        printf("%d attribute %s\n", i, attr->object->name);
        attr = attr->next;
      }
		
    }
    printf("----\n");
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
s_delete_object(TOPLEVEL *w_current, OBJECT *o_current)
{
  if (o_current != NULL) {
    s_conn_remove(w_current, o_current);
	
    /* second half of if is odd that we need it? hack */
    /* need to do this early so we can do the printfs */
    if (o_current->attached_to != NULL && o_current->attribute == 1) {
      if (o_current->attached_to->object) {
				/*printf("removing %s\n", o_current->attached_to->object->name);*/
      } else {
        printf("found a null I didn't expect!!!!!!!!!\n");
      }

      /* do the actual remove */
      o_attrib_delete(o_current->attached_to);
    }

    if (w_current->page_current->object_lastplace == o_current) {
      w_current->page_current->object_lastplace = NULL; 
    }

    if (o_current->line) {
      /*	printf("sdeleting line\n");*/
      g_free(o_current->line);

      /* yes this object might be in the tile system */
      s_tile_remove_object_all(w_current,
                               w_current->page_current,
                               o_current);
    }
    o_current->line = NULL;

    if (o_current->circle) {
      /*	printf("sdeleting circle\n");*/
      g_free(o_current->circle);
    }
    o_current->circle = NULL;

    if (o_current->arc) {
      /*	printf("sdeleting arc\n");*/
      g_free(o_current->arc);
    }
    o_current->arc = NULL;

    if (o_current->box) {
      /*	printf("sdeleting box\n");*/
      g_free(o_current->box);
    }
    o_current->box = NULL;

    if (o_current->picture) {
      /*	printf("sdeleting picture\n");*/

      if (o_current->picture->original_picture)
	g_object_unref(o_current->picture->original_picture);
      if (o_current->picture->displayed_picture)
	g_object_unref(o_current->picture->displayed_picture);

      if (o_current->picture->filename)
	g_free(o_current->picture->filename);
      g_free(o_current->picture);
    }
    o_current->picture = NULL;

    if (o_current->text) {
      if (o_current->text->string) {
				/*printf("sdeleting text->string\n");*/
        g_free(o_current->text->string); 
      }
      o_current->text->string = NULL;

      if (o_current->text->prim_objs) {
				/*printf("sdeleting text complex\n");*/
        s_delete_list_fromstart(w_current, 
                                o_current->text->prim_objs);
      }
      o_current->text->prim_objs = NULL;

      /*	printf("sdeleting text\n");*/
      g_free(o_current->text);
    }
    o_current->text = NULL;

    if (o_current->name) {
      /*	printf("sdeleting name\n");*/
      g_free(o_current->name);
    }
    o_current->name = NULL;


    if (o_current->complex_basename) {
      /*	printf("sdeleting complex_basename\n");*/
      g_free(o_current->complex_basename); 
    }
    o_current->complex_basename = NULL;

    if (o_current->complex) {

      if (o_current->complex->prim_objs) {
        /* printf("sdeleting complex->primitive_objects\n");*/
        s_delete_list_fromstart(w_current, 
                                o_current->complex->prim_objs);
      }
      o_current->complex->prim_objs = NULL;

      g_free(o_current->complex);
      o_current->complex = NULL;
    }

    if (o_current->attribs) {
      o_attrib_free_all(w_current, o_current->attribs);
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
void
s_delete(TOPLEVEL *w_current, OBJECT *o_current)
{
  if (o_current != NULL) {


#if DEBUG
    printf("sdel: %s\n", o_current->name);
    printf("sdel: %d\n", o_current->sid);
#endif

    if (o_current->next) 
    o_current->next->prev = o_current->prev;
    else
    o_current->next = NULL;

    if (o_current->prev) 
    o_current->prev->next = o_current->next;
    else
    o_current->prev = NULL;
    
    s_delete_object(w_current, o_current);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* deletes everything include the head */
void s_delete_list_fromstart(TOPLEVEL *w_current, OBJECT *start)
{
  OBJECT *temp=NULL; /* literally is a temp */
  OBJECT *current=NULL; /* ugg... you have both o_current and current? */
  OBJECT *o_current=NULL; /* hack */

  temp = start;
  current = return_tail(start);

  /* do the delete backwards */
  /*while(current != NULL && current->type != OBJ_HEAD ) {*/
  while(current != NULL) {
    o_current = current->prev;
    s_delete(w_current, current);
    current = o_current;
  }

  /* now delete the head node */
  /* might not need this but what the hell */
  /* no longer needed, since it's deleted above */
  /*s_delete_head(w_current, start);*/
}

#if 0 /* old way of doing this */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_delete_list_fromstart(OBJECT *start)
{
  OBJECT *traverse=NULL;
  OBJECT *o_current=NULL;

  for (traverse = start; traverse ; traverse = o_current) {
    o_current = traverse->next;
    if (traverse->type != OBJ_HEAD) /* don't delete any head nodes */
      s_delete(traverse);
    else 
      break; /* found a head node */
  }
  s_delete(traverse);
}
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* deletes everything include the head */
void
s_delete_object_glist(TOPLEVEL *w_current, GList *list)
{
  OBJECT *o_current=NULL;
  GList *ptr;

  ptr = g_list_last(list);

  /* do the delete backwards */
  while(ptr != NULL) {
    o_current = (OBJECT *) ptr->data;
    s_delete_object(w_current, o_current);
    ptr = ptr->prev;
  }

}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  This function removes one object pointed by parameter <B>object</B> from
 *  a list as far as it does not represents a head. If so the function returns
 *  NULL. If not it returns the pointer on the object, i.e. the same as the
 *  parameter.
 *
 *  This function must be followed by a call to #return_tail() on the
 *  list it belonged to as it can be the last object. Therefore the tail
 *  of the list is modified.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] object
 *  \return OBJECT *
 */
OBJECT *s_remove(TOPLEVEL *w_current, OBJECT *object)
{
  if(object->type == OBJ_HEAD)
  return NULL;
	
  if(object->prev != NULL)
  object->prev->next = object->next;
  if(object->next != NULL)
  object->next->prev = object->prev;

  object->next = NULL;
  object->prev = NULL;

  return object;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* Done */
void string_toupper(char *in, char *out)
{
  int len;
  int i;

  len = strlen(in);

  for (i = 0 ; i < len ; i++) {
    out[i] = toupper(in[i]);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void string_tolower(char *in, char *out)
{
	int len;
	int i;

	len = strlen(in);

	for (i = 0 ; i < len ; i++) {
		out[i] = tolower(in[i]);
	}
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int colornametovalue(char *string)
{
	
  int lower = 0; 
  int upper = NUMCOLORS - 1;
  int middle;
  int val;
  struct st_old_colors *ptr=NULL;

  if (!string) {
    return(-1);
  }

  string_tolower(string, string);
  while (lower <= upper) {
    middle = (lower + upper) / 2;

    ptr = &old_colors[middle];
    val = strcmp (ptr->name, string);

    if (val < 0) {
      lower = middle + 1;
    } else if (val == 0) {
      return(ptr->value);
    } else {
      upper = middle - 1;
    }
  }                
  return(-1);
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

#ifndef HAVE_VSNPRINTF
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void vsnprintf(char *buff, size_t bufsiz, const char *fmt, va_list ap)
{
    char *tmpbuf = buff;

    vsprintf(tmpbuf, fmt, ap);
}
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* this function is called by expand_env_variables */
/* changes and returns new string, frees the one that was passed in */
char *remove_string(char *string, int start, int end) 
{
  char *return_string;
  int i;
  int len;
  int j;

  if (!string) {
    return(NULL);
  }

  len = strlen(string);

  return_string = (char *) g_malloc(sizeof(char)*(len+1));

  j = 0;
  for (i = 0 ; i < len; i++) {
    if (i >= start && i <= end) {
      /* do nothing */
      /* removing characters */
    } else {
      return_string[j] = string[i];
      j++;
    }
  }
  return_string[j] = '\0';

  /* free original string */
  g_free(string);

  return(return_string);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* this function is called by expand_env_variables */
/* changes and returns new string, frees the one that was passed in */
char *insert_string(char *string, int start, char *insert_string)
{
  char *new_string=NULL;
  int i;
  int len;
  int insert_len;
  int total_len;
  int j;
  int orig_count=0;

  /* this should never happen */
  if (!insert_string) {
    return(NULL);	
  }

  /* this should never happen either */
  if (!string) {
    return(NULL);	
  }

  len = strlen(string);
  insert_len = strlen(insert_string);
  total_len = len+insert_len;

  new_string = (char *) g_malloc(sizeof(char)*(total_len+1));

  i = 0;
  while (i < total_len) {
    if (i == start) {
      for (j = 0 ; j < insert_len; j++) {
        new_string[i+j] = insert_string[j];
      }
      i = j+i;
    } else {
      new_string[i] = string[orig_count];
      i++;
      orig_count++;
    }
  }

  new_string[i] = '\0';

  /* now free the original string */
  g_free(string);

  return(new_string);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* this function changes and then returns the string which has the 
 * expanded environment variables, frees passed in string */
/* Environment variables MUST be in the form ${variable_name} */
/* $variable_name is not valid here */
char *expand_env_variables(char *string)
{
  char wanted_var[80]; /* size is hack */
  char *return_string=NULL;
  char *environment_string=NULL;
  int changed=1;
  int found_dollar=0;
  int found_lbrac=0; 
  int found_rbrac=0;
  int start_of_variable= -1;
  int end_of_variable= -1;
  int count=0;
  int i,j;

  if (!string) {
    return(NULL);
  }

  return_string = string;	

  while(changed) {
	
    changed=0;
    j=0;
    for (i = 0 ; i < strlen(return_string); i++) {

      switch(return_string[i]) {

        case('$'):

#if DEBUG
          printf("found a $\n");
#endif
          found_dollar=1;	
          start_of_variable=i;
          break;

        case('{'):
          if (found_dollar) {
            found_lbrac=1;
            count=1;
          }
          break;

        case('}'):
          if (found_dollar) {
            found_rbrac=1;
            /* ends filling of wanted_var */	
            found_lbrac=0;
            end_of_variable=i;
          }
          break;

      }

      /* the > 1 bit is so that we don't store the { */
      if (found_dollar && found_lbrac && (count > 1)) {
        wanted_var[j] = return_string[i];
        j++; /* check for size */
      }

      /* skip over initial { */ 
      count++;

      if (found_rbrac && !found_lbrac) {
        wanted_var[j] = '\0';
#if DEBUG
        printf("variable wanted: _%s_\n",  wanted_var);
        printf("Between index: %d and %d\n", 
               start_of_variable,
               end_of_variable);
#endif

		
        environment_string = getenv(wanted_var);	

#if DEBUG
        if (environment_string) {
          printf("%s = _%s_\n", wanted_var, 
                 environment_string);
        }
#endif

        return_string = remove_string(return_string,
                                      start_of_variable,
                                      end_of_variable);

#if DEBUG
        printf("removed string: _%s_\n", return_string);
#endif

        if (environment_string) {
          return_string = insert_string(
                                        return_string,
                                        start_of_variable,
                                        environment_string);

        }

#if DEBUG
        printf("final string: _%s_\n", return_string);
#endif
        changed=1;

				/* end of search */
        found_dollar=0;
        found_rbrac=0;
        count=0;
        start_of_variable= -1;
        end_of_variable= -1;

        break;
      }
    }
  }

  if (found_dollar) {
    fprintf(stderr, "Found malformed environment variable (use ${varname})!\n");
  }

  return(return_string);
}
