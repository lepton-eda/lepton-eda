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
#include <sys/stat.h>
#include <ctype.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"
#include "../include/x_dialog.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_funcs_print(SCM filename)
{
  SCM_ASSERT (SCM_NIMP (filename) && SCM_STRINGP (filename), filename,
              SCM_ARG1, "gschem-print");

  if (output_filename) {
    f_print_file (global_window_current, output_filename);
  } else  {
    f_print_file (global_window_current, SCM_STRING_CHARS (filename));
  }
  
  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_funcs_postscript(SCM filename)
{
  SCM_ASSERT (SCM_NIMP (filename) && SCM_STRINGP (filename), filename,
              SCM_ARG1, "gschem-postscript");

  if (output_filename) {
    f_print_file (global_window_current, output_filename);
  } else  {
    f_print_file (global_window_current, SCM_STRING_CHARS (filename));
  }
  
  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_funcs_image(SCM filename)
{
  SCM_ASSERT (SCM_NIMP (filename) && SCM_STRINGP (filename), filename,
              SCM_ARG1, "gschem-image");

  if (output_filename) {
    x_image_lowlevel (global_window_current, output_filename);
  } else  {
    x_image_lowlevel (global_window_current, SCM_STRING_CHARS (filename));
  }
  
  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_funcs_exit(void)
{
  exit(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_funcs_log(SCM msg)
{

  SCM_ASSERT (SCM_NIMP (msg) && SCM_STRINGP (msg), msg,
              SCM_ARG1, "gschem-log");

  s_log_message (SCM_STRING_CHARS (msg));

  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_funcs_msg(SCM msg)
{

  SCM_ASSERT (SCM_NIMP (msg) && SCM_STRINGP (msg), msg,
              SCM_ARG1, "gschem-msg");

  generic_msg_dialog (SCM_STRING_CHARS (msg));

  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_funcs_confirm(SCM msg)
{
  int r;

  SCM_ASSERT (SCM_NIMP (msg) && SCM_STRINGP (msg), msg,
	      SCM_ARG1, "gschem-msg");
  
  r = generic_confirm_dialog (SCM_STRING_CHARS (msg));

  if (r)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_funcs_filesel(SCM msg, SCM templ, SCM flags)
{
  int c_flags;
  char * r;
  SCM v;

  SCM_ASSERT (SCM_NIMP (msg) && SCM_STRINGP (msg), msg,
	      SCM_ARG1, "gschem-filesel");
  
  SCM_ASSERT (SCM_NIMP (templ) && SCM_STRINGP (templ), templ,
	      SCM_ARG1, "gschem-filesel");
  
  /*! \bug FIXME -- figure out the magic SCM_ASSERT for the flags */

  /*! \bug FIXME -- how to deal with conflicting flags? 
   * Should I throw a scheme error?  Just deal in the c code?
   */
  for (c_flags = 0; scm_pair_p (flags) == SCM_BOOL_T; flags = SCM_CDR (flags)) {
    SCM f = SCM_CAR (flags);
    if (strcmp (SCM_STRING_CHARS (f), "may_exist") == 0) {
      c_flags |= FSB_MAY_EXIST;

    } else if (strcmp (SCM_STRING_CHARS (f), "must_exist") == 0) {
      c_flags |= FSB_MUST_EXIST;
      
    } else if (strcmp (SCM_STRING_CHARS (f), "must_not_exist") == 0) {
      c_flags |= FSB_SHOULD_NOT_EXIST;

    } else if (strcmp (SCM_STRING_CHARS (f), "save") == 0) {
      c_flags |= FSB_SAVE;

    } else if (strcmp (SCM_STRING_CHARS (f), "open") == 0) {
      c_flags |= FSB_LOAD;

    } else {
      scm_wrong_type_arg ("gschem-filesel", 1, f);
    }
  }

  r = generic_filesel_dialog (SCM_STRING_CHARS (msg),
			      SCM_STRING_CHARS (templ),
			      c_flags
			      );

  v = scm_makfrom0str (r);
  g_free (r);

  return v;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_funcs_use_rc_values(void)
{
  i_vars_set(global_window_current);
  return SCM_BOOL_T;
}

/*! \brief */
static gchar *key_value_string = NULL;

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \bug there is no string size checking here... so if it core dumps... DOH!
 *  it's actually pretty usable right now, but needs to be reviewed again
 */
SCM g_funcs_key_name(SCM keystring)
{
  SCM_ASSERT (SCM_STRINGP (keystring), keystring, 1, "gschem-key-name");

  if (key_value_string != NULL) {
    x_dialog_hotkeys_fill (key_value_string);
    g_free (key_value_string); 
    key_value_string = NULL;
  }

  /* the 25 is for a few spaces and the characters */
  key_value_string = g_strdup_printf ("%s :%25c",
                                      SCM_STRING_CHARS (keystring), ' ');

  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_funcs_key_value(SCM keystring)
{
  gchar *temp;

  SCM_ASSERT (SCM_STRINGP (keystring), keystring, 1, "gschem-key-value");

  if (key_value_string == NULL) {
    fprintf(stderr, _("Ack! something got fouled up with the keymappings!\n"));
    exit(-1);
  }

  temp = g_strdup_printf ("%s %s",
                          key_value_string,
                          SCM_STRING_CHARS (keystring));
  g_free (key_value_string);
  key_value_string = temp;
	
  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_funcs_key_done(void)
{
  x_dialog_hotkeys_fill (key_value_string);
  g_free(key_value_string);
  key_value_string = NULL;
  return SCM_BOOL_T;
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/*
 * Gets names from all objects of current page which selected-flags are true.
 */
/* all of the declaration part is copied from some other c-code of
 * gEDA gschem. 
 * I don't really know, whether this all are necessary or not, but 
 * it works :-). */
static void
hash_table_2_list (gpointer key,
                   gpointer value,
                   gpointer user_data)
{
  SCM *plist = (SCM*)user_data;
  *plist = scm_cons (scm_makfrom0str ((char*)value), *plist);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM get_selected_component_attributes(TOPLEVEL *toplevel)
{
  SCM list = SCM_EOL;
  OBJECT *obj;
  GHashTable *ht;
 
  /* build a hash table */
  ht = g_hash_table_new (g_str_hash, g_str_equal);
  for (obj = toplevel->page_current->object_head; obj != NULL;
       obj = obj->next) {
    if (obj->selected &&
        obj->type == OBJ_TEXT &&
        obj->text->string != NULL) {
      /* add text string in the hash table */
      g_hash_table_insert (ht,
                           obj->text->string,
                           obj->text->string);
     }
   }
  /* now create a scheme list of the entries in the hash table */
  g_hash_table_foreach (ht, hash_table_2_list, &list);
  /* and get ride of the hast table */
  g_hash_table_destroy (ht);

  return list;
}

/*! \todo Finish function documentation!!!
 *  \brief Get selected filename of current schematic.
 *  \par Function Description
 *  This function gets the whole filename of the current schematic.
 *  Specifically, the <B>page_filename</B> of the current page.
 *
 *  \param [in] w_current  The TOPLEVEL object to get filename from.
 *  \return whole filename of current schematic.
 */
SCM get_selected_filename(TOPLEVEL *w_current)
{
  SCM return_value;
  
  exit_if_null(w_current);
  
  return_value = scm_take0str (w_current->page_current->page_filename);

  return(return_value);
}
