/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
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

  /* Don't associate with a page, initially */
  new_node->page = NULL;

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

  new_node->complex_basename = NULL;
  new_node->parent = NULL;
		
  /* Setup the color */
  new_node->color = DEFAULT_COLOR;
  new_node->dont_redraw = FALSE;
  new_node->selectable = TRUE;
  new_node->selected = FALSE;
  new_node->locked_color = -1;

  new_node->bus_ripper_direction = 0;

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

  new_node->weak_refs = NULL;

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
    /* If currently attached to a page, remove it from the page */
    if (o_current->page != NULL) {
      s_page_remove (toplevel, o_current->page, o_current);
    }

    s_conn_remove_object (toplevel, o_current);

    if (o_current->attached_to != NULL) {
      /* do the actual remove */
      o_attrib_remove(toplevel, &o_current->attached_to->attribs, o_current);
    }

    if (o_current->line) {
      /*	printf("sdeleting line\n");*/
      g_free(o_current->line);
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
      if (o_current->picture->pixbuf)
        g_object_unref (o_current->picture->pixbuf);

      g_free(o_current->picture->filename);
      g_free(o_current->picture);
    }
    o_current->picture = NULL;

    if (o_current->text) {
      /*printf("sdeleting text->string\n");*/
      g_free(o_current->text->string); 
      o_current->text->string = NULL;
      g_free(o_current->text->disp_string);
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

    o_attrib_detach_all (toplevel, o_current);

    s_weakref_notify (o_current, o_current->weak_refs);

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

/*! \brief Add a weak reference watcher to an OBJECT.
 * \par Function Description
 * Adds the weak reference callback \a notify_func to \a object.  When
 * \a object is destroyed, \a notify_func will be called with two
 * arguments: the \a object, and the \a user_data.
 *
 * \sa s_object_weak_unref
 *
 * \param [in,out] object     Object to weak-reference.
 * \param [in] notify_func    Weak reference notify function.
 * \param [in] user_data      Data to be passed to \a notify_func.
 */
void
s_object_weak_ref (OBJECT *object,
                   void (*notify_func)(void *, void *),
                   void *user_data)
{
  g_return_if_fail (object != NULL);
  object->weak_refs = s_weakref_add (object->weak_refs, notify_func, user_data);
}

/*! \brief Remove a weak reference watcher from an OBJECT.
 * \par Function Description
 * Removes the weak reference callback \a notify_func from \a object.
 *
 * \sa s_object_weak_ref()
 *
 * \param [in,out] object     Object to weak-reference.
 * \param [in] notify_func    Notify function to search for.
 * \param [in] user_data      Data to to search for.
 */
void
s_object_weak_unref (OBJECT *object,
                     void (*notify_func)(void *, void *),
                     void *user_data)
{
  g_return_if_fail (object != NULL);
  object->weak_refs = s_weakref_remove (object->weak_refs,
                                        notify_func, user_data);
}

/*! \brief Add a weak pointer to an OBJECT.
 * \par Function Description
 * Adds the weak pointer at \a weak_pointer_loc to \a object. The
 * value of \a weak_pointer_loc will be set to NULL when \a object is
 * destroyed.
 *
 * \sa s_object_remove_weak_ptr
 *
 * \param [in,out] object        Object to weak-reference.
 * \param [in] weak_pointer_loc  Memory address of a pointer.
 */
void
s_object_add_weak_ptr (OBJECT *object,
                       void *weak_pointer_loc)
{
  g_return_if_fail (object != NULL);
  object->weak_refs = s_weakref_add_ptr (object->weak_refs, weak_pointer_loc);
}

/*! \brief Remove a weak pointer from an OBJECT.
 * \par Function Description
 * Removes the weak pointer at \a weak_pointer_loc from \a object.
 *
 * \sa s_object_add_weak_ptr()
 *
 * \param [in,out] object        Object to weak-reference.
 * \param [in] weak_pointer_loc  Memory address of a pointer.
 */
void
s_object_remove_weak_ptr (OBJECT *object,
                          void *weak_pointer_loc)
{
  g_return_if_fail (object != NULL);
  object->weak_refs = s_weakref_remove_ptr (object->weak_refs,
                                            weak_pointer_loc);
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


/* -------------------------------------------------- */

/*! \brief Get the directory with the gEDA system data.
 *  \par Function description
 *  Returns the path to be searched for gEDA data shared between all
 *  users. If the GEDADATA environment variable is set, returns its
 *  value; otherwise, uses a compiled-in path.
 *
 *  On Windows, the compiled in path is *not* used, as it might not
 *  match the path where the user has installed gEDA.
 *
 *  \warning The returned string is owned by libgeda and should not be
 *  modified or free'd.
 *
 *  \return the gEDA shared data path, or NULL if none could be found.
 */
const char *s_path_sys_data () {
  static const char *p = NULL;
  if (p == NULL) {
    p = g_getenv ("GEDADATA");
  }
  if (p == NULL) {
# if !defined (_WIN32)
    p = GEDADATADIR;
# else
    p = g_get_current_dir ();
# endif
    g_setenv ("GEDADATA", p, FALSE);
  }
  return p;
}

/*! \brief Get the directory with the gEDA system configuration.
 *  \par Function description
 *  Returns the path to be searched for gEDA configuration shared
 *  between all users. If the GEDADATARC environment variable is set,
 *  returns its value; otherwise, uses a compiled-in path. Finally
 *  fallback to using the system data path.
 *
 *  \warning The returned string is owned by libgeda and should not be
 *  modified or free'd.
 *
 *  \return the gEDA shared config path, or NULL if none could be
 *  found.
 */
const char *s_path_sys_config () {
  static const char *p = NULL;

  /* If GEDADATARC is set in the environment, use that path */
  if (p == NULL) {
    p = g_getenv ("GEDADATARC");
  }
  if (p == NULL) {
#if defined (GEDARCDIR) && !defined(_WIN32)
    /* If available, use the rc directory set during configure. */
    p = GEDARCDIR;
#else
    /* Otherwise, just use the data directory */
    p = s_path_sys_data ();
#endif
  }
  if (p != NULL) g_setenv("GEDADATARC", p, FALSE);
  return p;
}

/*! \brief Get the directory with the gEDA user configuration.
 *  \par Function description
 *  Returns the path to be searched for the current user's gEDA
 *  configuration. Currently defaults to a directory ".gEDA" in the
 *  user's home directory.
 *
 *  \warning The returned string is owned by libgeda and should not be
 *  modified or free'd.
 *
 *  \todo On Windows, we should use APPDATA.
 */
const char *s_path_user_config () {
  static const char *p = NULL;

  if (p == NULL) {
    const char *home = g_getenv ("HOME");
    if (home == NULL) home = g_get_home_dir ();
    p = g_build_filename(home, ".gEDA", NULL);
  }
  return p;
}
