/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2008 Ales Hvezda
 * Copyright (C) 1998-2008 gEDA Contributors (see ChangeLog for details)
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

#include "gschem.h"

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
  SCM_ASSERT (scm_is_string (filename), filename,
              SCM_ARG1, "gschem-print");

  if (output_filename) {
    if (f_print_file (global_window_current->toplevel, output_filename))
      return SCM_BOOL_F;
  } else  {
    if (f_print_file (global_window_current->toplevel, SCM_STRING_CHARS (filename)))
      return SCM_BOOL_F;
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
  SCM_ASSERT (scm_is_string (filename), filename,
              SCM_ARG1, "gschem-postscript");

  if (output_filename) {
    if (f_print_file (global_window_current->toplevel, output_filename))
      return SCM_BOOL_F;
  } else  {
    if (f_print_file (global_window_current->toplevel, SCM_STRING_CHARS (filename)))
      return SCM_BOOL_F;
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
  SCM_ASSERT (scm_is_string (filename), filename,
              SCM_ARG1, "gschem-image");

  if (output_filename) {
    x_image_lowlevel (global_window_current, output_filename,
                      global_window_current->image_width,
                      global_window_current->image_height,
		      g_strdup("png"));
  } else  {
    x_image_lowlevel (global_window_current, SCM_STRING_CHARS (filename),
                      global_window_current->image_width,
                      global_window_current->image_height,
		      g_strdup("png"));
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

  SCM_ASSERT (scm_is_string (msg), msg,
              SCM_ARG1, "gschem-log");

  s_log_message ("%s", SCM_STRING_CHARS (msg));

  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_funcs_msg(SCM msg)
{

  SCM_ASSERT (scm_is_string (msg), msg,
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

  SCM_ASSERT (scm_is_string (msg), msg,
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

  SCM_ASSERT (scm_is_string (msg), msg,
	      SCM_ARG1, "gschem-filesel");
  
  SCM_ASSERT (scm_is_string (templ), templ,
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
SCM get_selected_component_attributes(GSCHEM_TOPLEVEL *w_current)
{
  SCM list = SCM_EOL;
  OBJECT *obj;
  GHashTable *ht;
  const GList *iter;
 
  /* build a hash table */
  ht = g_hash_table_new (g_str_hash, g_str_equal);
  for (iter = s_page_objects (w_current->toplevel->page_current);
       iter != NULL;
       iter = g_list_next (iter)) {
    obj = (OBJECT *)iter->data;
    if (obj->selected && obj->type == OBJ_TEXT) {
      const gchar *str = o_text_get_string (w_current->toplevel, obj);
      if (str == NULL) continue;
      /* add text string in the hash table */
      g_hash_table_insert (ht, (gchar *) str, (gchar *) str);
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
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object to get filename from.
 *  \return whole filename of current schematic.
 */
SCM get_selected_filename(GSCHEM_TOPLEVEL *w_current)
{
  SCM return_value;
  
  exit_if_null(w_current);
  
  return_value = scm_take0str (w_current->toplevel->page_current->page_filename);

  return(return_value);
}

/*! \brief Use gschemdoc to open a browser to a specific wiki page
 *
 * \param [in] wikiname the name of the wiki page
 *
 * \par Function Description
 * Invokes gschemdoc with its -w switch to open a browser to the wiki
 * page specified by wikiname.  If wikiname is empty or not a string, 
 * will browse to the main wiki page.
 */
SCM g_funcs_browse_wiki(SCM wikiname)
{
  char *wikistr;
  int pid;

  /* Extract wiki name string from Scheme value structure.
   * If not a string, use the empty string */
  if (scm_is_string (wikiname)) {
    wikistr = SCM_STRING_CHARS(wikiname);
  } else {
    wikistr = "";
  }

  #ifndef __MINGW32__

  pid = fork();

  if (pid < 0) {
    /* Fork failed. Still in parent process, so can use the log
     * window */
    s_log_message(_("Could not fork\n"));
    return SCM_BOOL_F;
  } else if (pid > 0) {
    /* Parent process, we're finished here */
    return SCM_BOOL_T;
  }
  
  /* begin daughter process stuff */
  
  /* assume gschemdoc is part of path */
  char *gschemdoc = "gschemdoc";
  char *wikiarg = "-w";
  
  execlp(gschemdoc, gschemdoc, wikiarg, wikistr, NULL);

  /* if we return, then nothing happened */
  fprintf(stderr, _("Could not invoke %s\n"), gschemdoc);
  _exit(0);

  /* end daughter process stuff */

#else /* __MINGW32__ */
  s_log_message(_("Documentation commands not supported under MinGW.\n"));
  return SCM_BOOL_F;
#endif /* __MINGW32__ */
}
