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
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \brief */
#define DELIMITERS ", "

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* Kazu Hirata <kazu@seul.org> on July 25, 1999 - Returns a pointer to
 * the last '.' in the given string. If there is none, the function
 * returns a pointer to the first null character in the string. If you
 * want to change the extention using the return value of the
 * function, you need to do pointer arithmetic, assuming your fname is
 * defined as a constant. :-) Note that, if the only '.' appears as
 * the first character, it is ignored. */
static const char *fnameext_get(const char* fname)
{
  const char *p = strrchr(fname, '.');

  if((p == NULL) || (p == fname)) {
    p = &fname[strlen(fname)];
  }
  return p;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* Kazu Hirata <kazu@seul.org> on July 25, 1999 - The function removes
 * an extention including a '.' if any and returns the new string in a
 * newly allocated memory. If there is no '.' after the first
 * character, then the function simply returns a copy of fname. If
 * memory allocation fails, the function returns NULL. */
static char *fnameext_remove(const char *fname)
{
  const char *p = fnameext_get(fname);
  char *fname_new;
  int len;

  if(*p == '\0') {
    fname_new = g_strdup (p);
  } else {
    len = (p - fname); /*  + 1; this extra was causing grief */
    fname_new = (char *) g_malloc(sizeof(char) * (len + 1));
    if(fname_new == NULL) {
      return NULL;
    }
    strncpy(fname_new, fname, len);
    fname_new[len] = '\0';
  }
  return fname_new;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* Kazu Hirata <kazu@seul.org> on July 25, 1999 - The function adds an
 * extention and returns the new string in a newly allocated
 * memory. ext must have '.'  as the first character. If memory
 * allocation fails, the function returns NULL. */
static char *fnameext_add(const char *fname, const char* ext)
{
  return g_strconcat (fname, ext, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* Egil Kvaleberg <egil@kvaleberg.no> on October 7, 2002 - 
 * Initiate the gschemdoc utility to provide the used with as much
 * documentation on the symbol (i.e. component) as we can manage.
 */
static void initiate_gschemdoc(const char* documentation,const char *device,
			       const char *value,
			       const char* symfile, const char *sympath)
{

#ifndef __MINGW32__

  int pid;

  if (!documentation) documentation="";
  if (!device) device="";
  if (!value) value="";
  if (!symfile) symfile="";
  if (!sympath) sympath="";

  s_log_message( _("Documentation for [%s,%s,%s,%s]\n"),
		    documentation,device,value,symfile);


  if ((pid = fork()) < 0) {
    fprintf(stderr, _("Could not fork\n"));
  } else if (pid == 0) {
    /* daughter process */

    /* assume gschemdoc is part of path */
    char *gschemdoc = "gschemdoc";

    execlp(gschemdoc, gschemdoc, documentation, device, value, symfile, sympath, NULL);

    /* if we return, then nothing happened */
    fprintf(stderr, _("Could not invoke %s\n"), gschemdoc);
    _exit(0);
  }
#else
  s_log_message(_("Documentation commands not supported under MinGW.\n"));
#endif
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* evey i_callback functions have the same footprint */
#define DEFINE_I_CALLBACK(name)				\
	void i_callback_ ## name(gpointer data,		\
			         guint callback_action,	\
			         GtkWidget *widget)

/*! \section callback-intro Callback Functions
 * right now, all callbacks execpt for the ones on the File menu have
 * the middle button shortcut. Let me (Ales) know if we should also
 * shortcut the File button
 */

/*! \section file-menu File Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current hack)
 *  \todo This should be renamed to page_new perhaps...
 */
DEFINE_I_CALLBACK(file_new)
{
  TOPLEVEL *toplevel = (TOPLEVEL*)data;
  PAGE *page;
  gchar *filename;

  exit_if_null (toplevel);

  g_free (toplevel->cwd);
  toplevel->cwd = g_get_current_dir ();
#ifdef __MINGW32__
  u_basic_strip_trailing(toplevel->cwd, G_DIR_SEPARATOR);
#endif
  filename = g_strdup_printf ("%s%c%s_%d.sch",
                              toplevel->cwd,
                              G_DIR_SEPARATOR,
                              toplevel->series_name,
                              ++toplevel->num_untitled);

  /* create a new page */
  page = s_page_new (toplevel, filename);
  /* change current page to page */
  s_page_goto (toplevel, page);
  g_free (filename);

  s_log_message (_("New page created [%s]\n"),
                 toplevel->page_current->page_filename);
  
  x_pagesel_update (toplevel);
  i_update_menus (toplevel);
  i_set_filename (toplevel, toplevel->page_current->page_filename);
  x_manual_resize (toplevel);
  x_hscrollbar_update (toplevel);
  x_vscrollbar_update (toplevel);
  x_repaint_background (toplevel);
  o_undo_savestate (toplevel, UNDO_ALL);
  
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current hack)
 */
void i_callback_toolbar_file_new(GtkWidget* widget, gpointer data)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  exit_if_null(w_current);
  if (!w_current->window) return;
    
  i_callback_file_new((TOPLEVEL*) data, 0, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(file_new_window)
{
  TOPLEVEL *w_current;
       
  w_current = s_toplevel_new ();
  x_window_setup (w_current);

  exit_if_null(w_current);

  g_free (w_current->cwd);
  w_current->cwd = g_get_current_dir ();
#ifdef __MINGW32__
  u_basic_strip_trailing(w_current->cwd, G_DIR_SEPARATOR);
#endif
  g_free (w_current->page_current->page_filename);
  w_current->page_current->page_filename = g_strdup_printf (
    "%s%c%s_%d.sch",
    w_current->cwd,
    G_DIR_SEPARATOR,
    w_current->series_name,
    ++w_current->num_untitled);

  s_log_message(_("New Window created\n"));
  i_set_filename(w_current, w_current->page_current->page_filename);
  x_repaint_background(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some
 *  checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current)
 *  \todo This should be renamed to page_open perhaps...
 */
DEFINE_I_CALLBACK(file_open)
{

  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  x_fileselect_setup(w_current, FILESELECT, OPEN);

#if 0 /* replaced by above */
  setup_open_file_selector(w_current);
#endif
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some
 *  checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current)
 *  \todo This should be renamed to page_open perhaps...
 */
void i_callback_toolbar_file_open(GtkWidget* widget, gpointer data)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  exit_if_null(w_current);
  if (!w_current->window) return;

  i_callback_file_open((TOPLEVEL*) data, 0, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(file_script)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);
  setup_script_selector(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some
 *  checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current)
 */
DEFINE_I_CALLBACK(file_save)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  /*! \todo probably there should be a flag that says whether
   *   page_filename is derived from untitled_name or specified by
   *   a user. Some twisted people might name their files like
   *   untitled_name. :-)
   */
  if (strstr(w_current->page_current->page_filename,
             w_current->untitled_name)) {
    x_fileselect_setup(w_current, FILESELECT, SAVEAS_NONE);
#if 0 /* replaced with x_fileselect_setup */
    setup_saveas_file_selector(
                               w_current,
                               SAVEAS,
                               w_current->page_current->page_filename);
#endif
  } else {

    if (f_save(w_current, w_current->page_current->page_filename) ) {
	    s_log_message(_("Saved [%s]\n"),w_current->page_current->page_filename);
	    /* don't know if should be kept going to select mode... */
	    w_current->page_current->CHANGED = 0;
	    i_set_state_msg(w_current, SELECT, _("Saved"));
	    i_update_toolbar(w_current);
            i_update_menus(w_current);
      x_pagesel_update (w_current);
    } else {
      	    s_log_message(_("Could NOT save [%s]\n"), w_current->page_current->page_filename);

	    i_set_state_msg(w_current, SELECT, _("Error while trying to save"));
	    i_update_toolbar(w_current);
            i_update_menus(w_current);
    } 
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some
 *  checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current)
 *  \todo This should be renamed to page_open perhaps...
 */
void i_callback_toolbar_file_save(GtkWidget* widget, gpointer data)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  exit_if_null(w_current);
  if (!w_current->window) return;

  i_callback_file_save((TOPLEVEL*) data, 0, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current)
 */
DEFINE_I_CALLBACK(file_save_all)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  if (s_page_save_all(w_current)) {
     i_set_state_msg(w_current, SELECT, _("Failed to Save All"));
  } else {
     i_set_state_msg(w_current, SELECT, _("Saved All"));
  }

  i_update_toolbar(w_current);
  x_pagesel_update (w_current);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(file_save_as)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);
  x_fileselect_setup(w_current, FILESELECT, SAVEAS_NONE);

#if 0 /* replaced with above */
  setup_saveas_file_selector(w_current,
                             SAVEAS,
                             w_current->page_current->page_filename);
#endif
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(file_print)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  char *base=NULL;
  char *ps_filename=NULL;
  
  exit_if_null(w_current);

  /* get the base file name */
  if (strcmp(fnameext_get(w_current->page_current->page_filename),
             ".sch") == 0) {
    /* the filename ends with .sch */
    base = fnameext_remove(w_current->page_current->page_filename);
  } else {
    /* the filename does not end with .sch */
    base = g_strdup (w_current->page_current->page_filename);
  }
  if(base == NULL) {
    /*! \todo do something */
  }

  /* add ".ps" tp the base filename */
  ps_filename = fnameext_add(base, ".ps");
  g_free(base);

  if (output_filename) {
    x_print_setup(w_current, output_filename);
  } else {
    x_print_setup(w_current, ps_filename);
  }

  if (ps_filename) {
    g_free(ps_filename);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(file_write_png)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  char *base=NULL;
  char *img_filename=NULL;

  exit_if_null(w_current);

#if 0
#ifndef HAS_LIBGDGEDA
  /*! \todo integrate these to messages */
  fprintf(stderr,
          _("libgdgeda not installed or disabled, "
          "so this feature is disabled\n"));
  s_log_message(
		_("libgdgeda not installed or disabled, "
		"so this feature is disabled\n"));
  return;
#endif
#endif
  /* get the base file name */
  if (strcmp(fnameext_get(w_current->page_current->page_filename),
             ".sch") == 0) {
    /* the filename ends with .sch */
    base = fnameext_remove(w_current->page_current->page_filename);
  } else {
    /* the filename does not end with .sch */
    base = g_strdup (w_current->page_current->page_filename);
  }
  if(base == NULL) {
    /*! \todo do something */
  }

  /* add ".png" tp the base filename */
  img_filename = fnameext_add(base, ".png");
  g_free(base);

  if (output_filename) {
    x_image_setup(w_current, output_filename);
  } else {
    x_image_setup(w_current, img_filename);
  }

  if (img_filename) {
    g_free(img_filename);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some
 *  checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current)
 *  this function closes a window
 */
DEFINE_I_CALLBACK(file_close)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  s_log_message(_("Closing Window\n"));
  x_window_close(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  This function is called when you send a delete event to gschem
 *
 *  \note
 *  Also DON'T ref the widget parameter since they can be null
 *  \todo Need a cleaner way of doing this. This routine is used by the
 *  delete event signals
 */
int i_callback_close(gpointer data, guint callback_action, GtkWidget *widget)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);
  i_callback_file_close(w_current, 0, widget);
  return(FALSE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(file_quit)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);
  x_window_close_all(w_current);
}

/*! \section edit-menu Edit Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_undo)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  w_current->DONT_REDRAW = 0;
  o_undo_callback(w_current, UNDO_ACTION);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current hack)
 */
void i_callback_toolbar_edit_undo(GtkWidget* widget, gpointer data)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  exit_if_null(w_current);
  if (!w_current->window) return;

  i_callback_edit_undo((TOPLEVEL*) data, 0, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_redo)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  w_current->DONT_REDRAW = 0;
  o_undo_callback(w_current, REDO_ACTION);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current hack)
 */
void i_callback_toolbar_edit_redo(GtkWidget* widget, gpointer data)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  exit_if_null(w_current);
  if (!w_current->window) return;

  i_callback_edit_redo((TOPLEVEL*) data, 0, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  Select also does not update the middle button shortcut.
 */
DEFINE_I_CALLBACK(edit_select)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  o_redraw_cleanstates(w_current);	

  /* this is probably the only place this should be */
  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);
  w_current->inside_action = 0;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some checking...
 * since there is a call: widget = NULL, data = 0 (will be w_current hack)
 */
void i_callback_toolbar_edit_select(GtkWidget* widget, gpointer data)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  exit_if_null(w_current);
  if (!w_current->window) return;

  if (GTK_TOGGLE_BUTTON (widget)->active) {
    if (!o_erase_rubber(w_current)) {
      i_callback_cancel(w_current, 0, NULL);
    }
    i_callback_edit_select((TOPLEVEL*) data, 0, NULL);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_copy)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_edit_copy, _("Copy"));
  if (o_select_return_first_object(w_current)) {
    i_set_state(w_current, STARTCOPY);
  } else {
    i_set_state_msg(w_current, SELECT, _("Select objs first"));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_copy_hotkey)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_edit_copy_hotkey, _("Copy"));
  if (o_select_return_first_object(w_current)) {
    w_current->event_state = COPY; 
    o_redraw_cleanstates(w_current);	
    o_copy_start(w_current, mouse_x, mouse_y);
    w_current->event_state = ENDCOPY;
    w_current->inside_action = 1;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_mcopy)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_edit_copy, _("Multiple Copy"));
  if (o_select_return_first_object(w_current)) {
    i_set_state(w_current, STARTMCOPY);
  } else {
    i_set_state_msg(w_current, SELECT, _("Select objs first"));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_mcopy_hotkey)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_edit_mcopy_hotkey, _("Multiple Copy"));
  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);	
    w_current->event_state = MCOPY; 
    o_copy_start(w_current, mouse_x, mouse_y);
    w_current->event_state = ENDMCOPY;
    w_current->inside_action = 1;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_move)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_edit_move, _("Move"));
  if (o_select_return_first_object(w_current)) {
    i_set_state(w_current, STARTMOVE);
  } else {
    i_set_state_msg(w_current, SELECT, _("Select objs first"));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_move_hotkey)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_edit_move_hotkey, _("Move"));
  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);	
    o_move_start(w_current, mouse_x, mouse_y);
    w_current->event_state = ENDMOVE;
    w_current->inside_action = 1;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_delete)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_edit_delete, _("Delete"));

  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);	
    o_delete(w_current);
    /* if you delete the objects you must go into select
     * mode after the delete */
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
    i_update_toolbar(w_current);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_edit)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_edit_edit, _("Edit"));
  o_edit(w_current, w_current->page_current->selection2_head->next);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_text)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  OBJECT *object;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_edit_text, _("Edit Text"));
  object = o_select_return_first_object(w_current);
  if (object) {
    if (object->type == OBJ_TEXT) {
      o_text_edit(w_current, object);
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_slot)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  OBJECT *object;

  exit_if_null(w_current);

  object = o_select_return_first_object(w_current);

  i_update_middle_button(w_current, i_callback_edit_slot, _("Slot"));
  if (object) {
    o_slot_start(w_current, object);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_color)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_edit_color, _("Color"));

  color_edit_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  This function rotate all objects in the selection list by 90 degrees.
 *
 */
DEFINE_I_CALLBACK(edit_rotate_90)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  i_set_state(w_current, ENDROTATEP);
  i_update_middle_button(w_current, i_callback_edit_rotate_90, _("Rotate"));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  This function rotate all objects in the selection list by 90 degrees.
 *
 */
DEFINE_I_CALLBACK(edit_rotate_90_hotkey)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  SELECTION *s_current;

  exit_if_null(w_current);
  o_redraw_cleanstates(w_current);	

  if (w_current->page_current->selection2_head) {
    s_current = w_current->page_current->selection2_head->next;
    i_update_middle_button(w_current,
                           i_callback_edit_rotate_90_hotkey, _("Rotate"));
    /* Allow o_rotate_90 to redraw the objects */
    w_current->DONT_REDRAW = 0;
    o_rotate_90(w_current, s_current, mouse_x, mouse_y);
  }

  w_current->event_state = SELECT;
  w_current->inside_action = 0;
  i_update_toolbar(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_mirror)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  i_set_state(w_current, ENDMIRROR);
  i_update_middle_button(w_current, i_callback_edit_mirror, _("Mirror"));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_mirror_hotkey)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  OBJECT *object;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  object = o_select_return_first_object(w_current);

  if (object) {
    i_update_middle_button(w_current,
                           i_callback_edit_mirror_hotkey, _("Mirror"));

    o_mirror(w_current, 
             w_current->page_current->selection2_head->next, 
             mouse_x, mouse_y);
  }

  w_current->event_state = SELECT;
  w_current->inside_action = 0;
  i_update_toolbar(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  This function locks all objects in selection list.
 *
 */
DEFINE_I_CALLBACK(edit_lock)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_edit_lock, _("Lock"));

  if (o_select_return_first_object(w_current)) {
    o_lock(w_current);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  Thus function unlocks all objects in selection list.
 */
DEFINE_I_CALLBACK(edit_unlock)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_edit_unlock, _("Unlock"));
  if (o_select_return_first_object(w_current)) {
    o_unlock(w_current);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_translate)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current,
                         i_callback_edit_translate, _("Translate"));

  if (w_current->snap == 0) {
    s_log_message(_("WARNING: Do not translate with snap off!\n"));
    s_log_message(_("WARNING: Turning snap on and continuing "
                  "with translate.\n"));
    w_current->snap = 1;
    i_show_state(w_current, NULL); /* update status on screen */
  }

  if (w_current->snap_size != 100) {
    s_log_message(_("WARNING: Snap grid size is "
                  "not equal to 100!\n"));
    s_log_message(_("WARNING: If you are translating a symbol "
                  "to the origin, the snap grid size should be "
                  "set to 100\n"));
  }

  translate_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  This function embedds all objects in selection list
 *
 */
DEFINE_I_CALLBACK(edit_embed)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_edit_embed, _("Embed"));
  /* anything selected ? */
  if (o_select_selected(w_current)) {
    /* yes, embed each selected component */
    SELECTION *s_current =
      w_current->page_current->selection2_head->next;

    while (s_current != NULL) {
      g_assert (s_current->selected_object != NULL);
      if ( (s_current->selected_object->type == OBJ_COMPLEX) ||
	   (s_current->selected_object->type == OBJ_PICTURE) ) {
        o_embed (w_current, s_current->selected_object);
      }
      s_current = s_current->next;
    }
  } else {
    /* nothing selected, go back to select state */
    o_redraw_cleanstates(w_current);	
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
  }
  
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  This function unembedds all objects in selection list.
 *
 */
DEFINE_I_CALLBACK(edit_unembed)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_edit_unembed, _("Unembed"));
  /* anything selected ? */
  if (o_select_selected(w_current)) {
    /* yes, unembed each selected component */
    SELECTION *s_current =
      w_current->page_current->selection2_head->next;

    while (s_current != NULL) {
      g_assert (s_current->selected_object != NULL);
      if ( (s_current->selected_object->type == OBJ_COMPLEX) ||
           (s_current->selected_object->type == OBJ_PICTURE) ) {
        o_unembed (w_current, s_current->selected_object);
      }
      s_current = s_current->next;
    }
  } else {
    /* nothing selected, go back to select state */
    o_redraw_cleanstates(w_current);	
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
  }

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  This function updates components
 *
 */
DEFINE_I_CALLBACK(edit_update)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_edit_update, _("Update"));
  /* anything selected ? */
  if (o_select_selected(w_current)) {
    /* yes, update each selected component */
    SELECTION *s_current =
      w_current->page_current->selection2_head->next;

    while (s_current != NULL) {
      g_assert (s_current->selected_object != NULL);
      if (s_current->selected_object->type == OBJ_COMPLEX) {
        o_update_component (w_current,
                            s_current->selected_object);
      }
      s_current = s_current->next;
    }
  } else {
    /* nothing selected, go back to select state */
    o_redraw_cleanstates(w_current);	
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
  }

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_show_hidden)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action)
    return;

  i_update_middle_button(w_current,
                         i_callback_edit_show_hidden,
                         _("ShowHidden"));

  o_edit_show_hidden(w_current, w_current->page_current->object_head);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_make_visible)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action)
    return;

  i_update_middle_button(w_current,
                         i_callback_edit_make_visible,
                         _("MakeVisible"));

  o_edit_make_visible(w_current, w_current->page_current->object_head);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_find)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action)
    return;

  find_text_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_hide_text)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action)
    return;

  hide_text_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_show_text)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action)
    return;

  show_text_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_autonumber_text)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action)
    return;

  autonumber_text_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_linetype)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  /* anything selected ? */
  if (o_select_selected(w_current)) {
    SELECTION *s_current =
      w_current->page_current->selection2_head->next;
    GList *objects = NULL;

    /* yes, build a list of relevant objects */
    while (s_current != NULL) {
      OBJECT *o_current = s_current->selected_object;
          
      if (o_current->type == OBJ_LINE   ||
          o_current->type == OBJ_BOX    ||
          o_current->type == OBJ_CIRCLE ||
          o_current->type == OBJ_ARC) {
        objects = g_list_prepend (objects, o_current);
      }

      s_current = s_current->next;
    }

    if (objects != NULL) {
      /* at least one object in the list */
      i_update_middle_button(w_current,
                             i_callback_edit_color,
                             _("Edit Line Type"));
      /* enter edition of parameters of selected objects */
      line_type_dialog(w_current, objects);
    }
  }
  
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_filltype)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  /* anything selected ? */
  if (o_select_selected(w_current)) {
    SELECTION *s_current =
      w_current->page_current->selection2_head->next;
    GList *objects = NULL;

    /* yes, build a list of relevant objects */
    while (s_current != NULL) {
      OBJECT *o_current = s_current->selected_object;
          
      if (o_current->type == OBJ_BOX ||
          o_current->type == OBJ_CIRCLE) {
        objects = g_list_prepend (objects, o_current);
      }

      s_current = s_current->next;
    }

    if (objects != NULL) {
      /* at least one object in the list */
      i_update_middle_button(w_current,
                             i_callback_edit_color,
                             _("Edit Fill Type"));
      /* enter edition of parameters of selected objects */
      fill_type_dialog(w_current, objects);
    }
  }
  
}

/*! \section view-menu View Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat middle shortcut doesn't make sense on redraw, just hit right
 *  button
 */
DEFINE_I_CALLBACK(view_redraw)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);
  o_redraw_all(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat middle shortcut would get into the way of what user is try to do
 */
DEFINE_I_CALLBACK(view_zoom_full)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  /* scroll bar stuff */
  a_zoom(w_current, ZOOM_FULL, DONTCARE, 0);
  o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat middle shortcut would get into the way of what user is try to do
 */
DEFINE_I_CALLBACK(view_zoom_extents)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  /* scroll bar stuff */
  a_zoom_extents(w_current, w_current->page_current->object_head, 0);
  o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat middle shortcut would get into the way of what user is try to do
 */
DEFINE_I_CALLBACK(view_zoom_box)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  w_current->inside_action = 0;
  i_allow_expose();
  i_set_state(w_current, ZOOMBOXSTART);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(view_zoom_box_hotkey)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  a_zoom_box_start(w_current, mouse_x, mouse_y);

  w_current->inside_action = 1;
  i_allow_expose();
  i_set_state(w_current, ZOOMBOXEND);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat middle shortcut would get into the way of what user is try to do
 */
DEFINE_I_CALLBACK(view_zoom_in)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  a_zoom(w_current, ZOOM_IN, MENU, 0);
  o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat middle shortcut would get into the way of what user is try to do
 */
DEFINE_I_CALLBACK(view_zoom_out)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  a_zoom(w_current, ZOOM_OUT, MENU, 0);
  o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat middle shortcut would get into the way of what user is try
 *  to do
 */
DEFINE_I_CALLBACK(view_zoom_in_hotkey)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  a_zoom(w_current, ZOOM_IN, HOTKEY, 0);
  o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat middle shortcut would get into the way of what user is try to do
 */
DEFINE_I_CALLBACK(view_zoom_out_hotkey)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  a_zoom(w_current, ZOOM_OUT, HOTKEY, 0);
  o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(view_pan)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  w_current->inside_action = 0;
  i_set_state(w_current, STARTPAN);

  /* I don't know if this would get in the way */
  i_update_middle_button(w_current, i_callback_view_pan, _("Pan"));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(view_pan_hotkey)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  /* I don't know if this would get in the way */
  i_update_middle_button(w_current, i_callback_view_pan_hotkey, _("Pan"));

  /* I have NO idea what ramifications removing the next line has,
   * only that it makes the code work when drawing a net and panning
   * at the same time.  Jeff McNeal - 11-19-98
   * w_current->inside_action = 0;
   * I think it's okay - Ales 12/13/98 */

  a_pan(w_current, mouse_x, mouse_y);

  /* Jeff McNeal on Nov 19, 1998 - if we are drawing a net,
   * don't change the event state, because we want to continue
   * drawing a net. If we are just panning, then continue in
   * select mode.  */
  /* removed the limitations of the pan mode (werner) */
  /* if(!(w_current->event_state == DRAWNET ||
       w_current->event_state == NETCONT ||
       w_current->event_state == STARTDRAWNET )) {
    i_set_state(w_current, SELECT);
    i_update_toolbar(w_current);
    } */
  o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(view_update_cues)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  i_update_middle_button(w_current,
                         i_callback_view_update_cues, _("Update Cues"));

  o_redraw_all(w_current);
}

/*! \section page-menu Page Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(page_manager)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  x_pagesel_open (w_current);
  
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(page_next)
{
  TOPLEVEL *w_current = (TOPLEVEL*)data;
  PAGE *p_current = w_current->page_current;
  PAGE *p_new;

  exit_if_null(w_current);

  if (p_current->next == NULL) {
    return;
  }

  if (w_current->enforce_hierarchy) {
    p_new = s_hierarchy_find_next_page(p_current, 
                                       p_current->page_control);
  } else {
    p_new = p_current->next;
  }

  if (p_new == NULL || p_new == p_current) {
    return;
  }

  s_page_goto (w_current, p_new);
  
  i_set_filename(w_current, w_current->page_current->page_filename);
  x_scrollbars_update(w_current);
  o_redraw_all(w_current);
  x_pagesel_update (w_current);
  i_update_menus(w_current);

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(page_prev)
{
  TOPLEVEL *w_current = (TOPLEVEL*)data;
  PAGE *p_current = w_current->page_current;
  PAGE *p_new;

  exit_if_null(w_current);

  if (p_current->prev == NULL || p_current->prev->pid == -1) {
    return;
  }

  if (w_current->enforce_hierarchy == TRUE) {
    p_new = s_hierarchy_find_prev_page(p_current, 
                                       p_current->page_control);
  } else {
    p_new = p_current->prev;
  }

  if (p_new == NULL || p_new == p_current) {
    return;
  }

  s_page_goto (w_current, p_new);
  
  i_set_filename(w_current, w_current->page_current->page_filename);
  x_scrollbars_update(w_current);
  o_redraw_all(w_current);
  x_pagesel_update (w_current);
  i_update_menus(w_current);
  
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(page_new)
{
  TOPLEVEL *toplevel = (TOPLEVEL*)data;
  PAGE *page;
  gchar *filename;

  exit_if_null(toplevel);

  g_free (toplevel->cwd);
  toplevel->cwd = g_get_current_dir ();
#ifdef __MINGW32__
  u_basic_strip_trailing(toplevel->cwd, G_DIR_SEPARATOR);
#endif
  filename = g_strdup_printf ("%s%c%s_%d.sch",
                              toplevel->cwd,
                              G_DIR_SEPARATOR,
                              toplevel->series_name,
                              ++toplevel->num_untitled);

  /* create the new page */
  page = s_page_new (toplevel, filename);
  /* change current page to page */
  s_page_goto (toplevel, page);
  g_free (filename);

  s_log_message(_("New Page created [%s]\n"),
                toplevel->page_current->page_filename);
  
  x_pagesel_update (toplevel);
  i_update_menus (toplevel);
  i_set_filename (toplevel, toplevel->page_current->page_filename);
  o_redraw_all (toplevel);
  
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(page_close)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  PAGE *p_current;
  PAGE *p_save;

  exit_if_null(w_current);

  if (w_current->page_current->CHANGED) {
    x_fileselect_setup(w_current, FILESELECT, SAVEAS_CLOSE);
    return;
  }

  /* Can we go up in the hierarchy first? */
  p_current = s_hierarchy_find_page(w_current->page_head, 
                                    w_current->page_current->up);
  if (p_current) {
    s_log_message(_("Closing [%s]\n"),
                  w_current->page_current->page_filename);
    s_page_delete (w_current, w_current->page_current);

    s_page_goto (w_current, p_current);

    i_set_filename(w_current, w_current->page_current->page_filename);
    x_scrollbars_update(w_current);
    o_redraw_all(w_current);
    x_pagesel_update (w_current);
    i_update_menus(w_current);
    return;
  }

  /* is there a page before current? */
  if (w_current->page_current->prev &&
      w_current->page_current->prev->pid != -1) {
        /* yes, change to previous page */
        p_current = w_current->page_current->prev;
        s_log_message(_("Closing [%s]\n"),
                      w_current->page_current->page_filename);
        s_page_delete (w_current, w_current->page_current);
        
        s_page_goto (w_current, p_current);
        
        i_set_filename(w_current, w_current->page_current->page_filename);
        x_scrollbars_update(w_current);
        o_redraw_all(w_current);
        x_pagesel_update (w_current);
        i_update_menus(w_current);
        return;
      }

  /* is there a page after current? */
  if (w_current->page_current->next) {
    /* yes, change to next page */
    g_assert (w_current->page_current->next->pid != -1);
    
    p_current = w_current->page_current->next;
    s_log_message(_("Closing [%s]\n"),
                  w_current->page_current->page_filename);
    s_page_delete (w_current, w_current->page_current);
    
    s_page_goto (w_current, p_current);
    
    i_set_filename(w_current, w_current->page_current->page_filename);
    x_scrollbars_update(w_current);
    o_redraw_all(w_current);
    x_pagesel_update (w_current);
    i_update_menus(w_current);
    return;
  }

  /* finally go here if you can't delete the page */
  /* because it's the last page being displayed */
  /* s_log_message("Cannot close current page\n");*/
  /* now the code creates a new page, and closes the old one */
  p_save = w_current->page_current;
  i_callback_page_new(w_current, 0, NULL);
  w_current->page_current = p_save;	

  g_assert (w_current->page_current->next != NULL);
  g_assert (w_current->page_current->next->pid != -1);
  
  p_current = w_current->page_current->next;
  s_log_message(_("Closing [%s]\n"),
                w_current->page_current->page_filename);
  s_page_delete (w_current, w_current->page_current);

  s_page_goto (w_current, p_current);
  
  i_set_filename(w_current, w_current->page_current->page_filename);
  x_scrollbars_update(w_current);
  o_redraw_all(w_current);
  x_pagesel_update (w_current);
  i_update_menus(w_current);

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \bug may have memory leak?
 */
DEFINE_I_CALLBACK(page_revert)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  PAGE *page;
  gchar *filename;
  int page_control;
  int up;
#ifdef HAS_GTK22
  int response;
  GtkWidget* dialog;
#endif

  exit_if_null(w_current);

#ifdef HAS_GTK22
 dialog = gtk_message_dialog_new ((GtkWindow*) w_current->main_window,
                                  GTK_DIALOG_DESTROY_WITH_PARENT,
                                  GTK_MESSAGE_QUESTION,
                                  GTK_BUTTONS_YES_NO,
                                  _("Really revert page?"));
 response = gtk_dialog_run (GTK_DIALOG (dialog));
 gtk_widget_destroy (dialog);
 
 switch (response) 
 {
   case GTK_RESPONSE_NO:
     return; /* don't continue */
   break; 

   case GTK_RESPONSE_YES:
   /* just fall through */
   break;
 }
#endif

  /* save this for later */
  filename = g_strdup (w_current->page_current->page_filename);
  page_control = w_current->page_current->page_control;
  up = w_current->page_current->up;

  /* delete the page, create a new one and make it the new current */
  s_page_delete (w_current, w_current->page_current);
  page = s_page_new (w_current, filename);
  s_page_goto (w_current, page);
  g_free (filename);

  /* now re open it */
  f_open(w_current, w_current->page_current->page_filename);
  i_set_filename(w_current, w_current->page_current->page_filename);

  /* make sure we maintain the hierarchy info */
  w_current->page_current->page_control = page_control;
  w_current->page_current->up = up;

  x_repaint_background(w_current);
  x_manual_resize(w_current);
  a_zoom_extents(w_current, w_current->page_current->object_head,
                A_PAN_DONT_REDRAW);
  o_undo_savestate(w_current, UNDO_ALL);

  /* now update the scrollbars */
  x_hscrollbar_update(w_current);
  x_vscrollbar_update(w_current);
  x_pagesel_update (w_current);
  i_update_menus(w_current);

  o_redraw_all(w_current);
  
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(page_discard)
{
  TOPLEVEL *w_current = (TOPLEVEL*)data;
  PAGE *p_current;
  PAGE *p_save;

  exit_if_null(w_current);

  /* Can we go up in the hierarchy first? */
  p_current = s_hierarchy_find_page(w_current->page_head, 
                                    w_current->page_current->up);
  if (p_current) {
    s_log_message(_("Closing [%s]\n"),
                  w_current->page_current->page_filename);
    s_page_delete (w_current, w_current->page_current);

    s_page_goto (w_current, p_current);

    i_set_filename(w_current, w_current->page_current->page_filename);
    x_scrollbars_update(w_current);
    o_redraw_all(w_current);
    x_pagesel_update (w_current);
    i_update_menus(w_current);
    return;
  }

  /* is there a page before current? */
  if (w_current->page_current->prev &&
      w_current->page_current->prev->pid != -1) {
        /* yes, change to previous page */
        p_current = w_current->page_current->prev;
        s_log_message(_("Discarding page [%s]\n"),
                      w_current->page_current->page_filename);
        s_page_delete (w_current, w_current->page_current);

        s_page_goto (w_current, p_current);
        
        i_set_filename(w_current, w_current->page_current->page_filename);
        x_scrollbars_update(w_current);
        o_redraw_all(w_current);
        x_pagesel_update (w_current);
        i_update_menus(w_current);
        return;
      }

  /* is there a page after current? */
  if (w_current->page_current->next) {
    /* yes, change to next page */
    g_assert (w_current->page_current->next->pid != -1);

    p_current = w_current->page_current->next;
    s_log_message(_("Discarding page [%s]\n"),
                  w_current->page_current->page_filename);
    s_page_delete (w_current, w_current->page_current);

    s_page_goto (w_current, p_current);
    
    i_set_filename(w_current, w_current->page_current->page_filename);
    x_scrollbars_update(w_current);
    o_redraw_all(w_current);
    x_pagesel_update (w_current);
    i_update_menus(w_current);
    return;
  }

  /* finally go here if you can't delete the page */
  /* because it's the last page being displayed */
  /* s_log_message("Cannot close current page\n");*/
  /* now the code creates a new page, and closes the old one */
  p_save = w_current->page_current;
  i_callback_page_new(w_current, 0, NULL);
  w_current->page_current = p_save;	

  g_assert (w_current->page_current->next != NULL);
  g_assert (w_current->page_current->next->pid != -1);
  
  p_current = w_current->page_current->next;
  s_log_message(_("Discarding page [%s]\n"),
                w_current->page_current->page_filename);
  s_page_delete (w_current, w_current->page_current);

  s_page_goto (w_current, p_current);

  i_set_filename(w_current, w_current->page_current->page_filename);
  x_scrollbars_update(w_current);
  o_redraw_all(w_current);
  x_pagesel_update (w_current);
  i_update_menus(w_current);
  
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(page_print)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  s_page_print_all(w_current);
}

/*! \section buffer-menu Buffer Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_copy1)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  if (w_current->page_current->selection2_head->next == NULL)
  return;

  i_update_middle_button(w_current, i_callback_buffer_copy1, _("Copy 1"));
  o_buffer_copy(w_current, 0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_copy2)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  if (w_current->page_current->selection2_head->next == NULL)
  return;

  i_update_middle_button(w_current, i_callback_buffer_copy2, _("Copy 2"));
  o_buffer_copy(w_current, 1);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_copy3)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  if (w_current->page_current->selection2_head->next == NULL)
  return;

  i_update_middle_button(w_current, i_callback_buffer_copy3, _("Copy 3"));
  o_buffer_copy(w_current, 2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_copy4)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  if (w_current->page_current->selection2_head->next == NULL)
  return;

  i_update_middle_button(w_current, i_callback_buffer_copy4, _("Copy 4"));
  o_buffer_copy(w_current, 3);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_copy5)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  if (w_current->page_current->selection2_head->next == NULL)
  return;

  i_update_middle_button(w_current, i_callback_buffer_copy5, _("Copy 5"));
  o_buffer_copy(w_current, 4);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_cut1)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  if (w_current->page_current->selection2_head->next == NULL)
  return;

  i_update_middle_button(w_current, i_callback_buffer_cut1, _("Cut 1"));
  o_buffer_cut(w_current, 0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_cut2)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  if (w_current->page_current->selection2_head->next == NULL)
  return;

  i_update_middle_button(w_current, i_callback_buffer_cut2, _("Cut 2"));
  o_buffer_cut(w_current, 1);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_cut3)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  if (w_current->page_current->selection2_head->next == NULL)
  return;

  i_update_middle_button(w_current, i_callback_buffer_cut3, _("Cut 3"));
  o_buffer_cut(w_current, 2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_cut4)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  if (w_current->page_current->selection2_head->next == NULL)
  return;

  i_update_middle_button(w_current, i_callback_buffer_cut4, _("Cut 4"));
  o_buffer_cut(w_current, 3);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_cut5)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  if (w_current->page_current->selection2_head->next == NULL)
  return;

  i_update_middle_button(w_current, i_callback_buffer_cut5, _("Cut 5"));
  o_buffer_cut(w_current, 4);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_paste1)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_buffer_paste1, _("Paste 1"));
  if (object_buffer[0] != NULL) {
    if (object_buffer[0]->next != NULL) {
      o_redraw_cleanstates(w_current);	
      w_current->buffer_number = 0;
      w_current->inside_action = 1;
      i_set_state(w_current, STARTPASTE);
    }
  } else { 
    i_set_state_msg(w_current, SELECT, _("Empty buffer"));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_paste2)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_buffer_paste2, _("Paste 2"));
  if (object_buffer[1] != NULL) {
    if (object_buffer[1]->next != NULL) {
      o_redraw_cleanstates(w_current);	
      w_current->buffer_number = 1;
      w_current->inside_action = 1;
      i_set_state(w_current, STARTPASTE);
    }
  } else {
    i_set_state_msg(w_current, SELECT, _("Empty buffer"));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_paste3)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_buffer_paste3, _("Paste 3"));
  if (object_buffer[2] != NULL) {
    if (object_buffer[2]->next != NULL) {
      o_redraw_cleanstates(w_current);	
      w_current->buffer_number = 2;
      w_current->inside_action = 1;
      i_set_state(w_current, STARTPASTE);
    }
  } else { 
    i_set_state_msg(w_current, SELECT, _("Empty buffer"));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_paste4)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_buffer_paste4, _("Paste 4"));
  if (object_buffer[3] != NULL) {
    if (object_buffer[3]->next != NULL) {
      o_redraw_cleanstates(w_current);	
      w_current->buffer_number = 3;
      w_current->inside_action = 1;
      i_set_state(w_current, STARTPASTE);
    }
  } else {
    i_set_state_msg(w_current, SELECT, _("Empty buffer"));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_paste5)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_buffer_paste5, _("Paste 5"));
  if (object_buffer[4] != NULL) {
    if (object_buffer[4]->next != NULL) {
      o_redraw_cleanstates(w_current);	
      w_current->buffer_number = 4;
      w_current->inside_action = 1;
      i_set_state(w_current, STARTPASTE);
    }
  } else {
    i_set_state_msg(w_current, SELECT, _("Empty buffer"));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_paste1_hotkey)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  if (object_buffer[0] == NULL) {
    return;
  }

  if (object_buffer[0]->next == NULL)  {
    return;
	
  }
	
  o_buffer_paste_start(w_current, mouse_x, mouse_y, 0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_paste2_hotkey)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  if (object_buffer[1] == NULL) {
    return;
  }

  if (object_buffer[1]->next == NULL)  {
    return;
	
  }

  o_buffer_paste_start(w_current, mouse_x, mouse_y, 1);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_paste3_hotkey)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  if (object_buffer[2] == NULL) {
    return;
  }

  if (object_buffer[2]->next == NULL)  {
    return;
	
  }

  o_buffer_paste_start(w_current, mouse_x, mouse_y, 2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_paste4_hotkey)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  if (object_buffer[3] == NULL) {
    return;
  }

  if (object_buffer[3]->next == NULL)  {
    return;
	
  }

  o_buffer_paste_start(w_current, mouse_x, mouse_y, 3);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_paste5_hotkey)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  if (object_buffer[4] == NULL) {
    return;
  }

  if (object_buffer[4]->next == NULL)  {
    return;
	
  }

  o_buffer_paste_start(w_current, mouse_x, mouse_y, 4);
}

/*! \section add-menu Add Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_component)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  x_fileselect_setup(w_current, COMPSELECT, -1);

#if 0 /* replaced by above */
  setup_place_file_selector(w_current);
#endif
  i_update_middle_button(w_current,
                         i_callback_add_component, _("Component"));

  i_allow_expose();
  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some checking... 
 *  since there is a call: widget = NULL, data = 0 (will be w_current hack)
 */
void i_callback_toolbar_add_component(GtkWidget* widget, gpointer data)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  exit_if_null(w_current);
  if (!w_current->window) return;

  i_callback_add_component((TOPLEVEL*) data, 0, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_attribute)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  attrib_edit_dialog(w_current, NULL, FROM_MENU);
  i_update_middle_button(w_current, i_callback_add_attribute,
                         _("Attribute"));

  i_allow_expose();
  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_attribute_hotkey)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  attrib_edit_dialog(w_current, NULL, FROM_HOTKEY);
  i_update_middle_button(w_current, i_callback_add_attribute_hotkey,
                         _("Attribute"));

  i_allow_expose();
  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_net)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  o_erase_rubber(w_current);

  /* need to click */
  i_update_middle_button(w_current, i_callback_add_net, _("Net"));
  i_allow_expose();
  i_set_state(w_current, STARTDRAWNET);
  i_update_toolbar(w_current);
  /* somewhere you need to nearest point locking... */
  w_current->inside_action = 0;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_net_hotkey)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  o_erase_rubber(w_current);

  /* need to click */
  i_update_middle_button(w_current, i_callback_add_net_hotkey, _("Net"));
  i_allow_expose();
  i_set_state(w_current, STARTDRAWNET);
  i_update_toolbar(w_current);

  o_net_start(w_current, mouse_x, mouse_y);

  w_current->event_state=DRAWNET;
  w_current->inside_action = 1;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current hack)
 */
void i_callback_toolbar_add_net(GtkWidget* widget, gpointer data)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  exit_if_null(w_current);
  if (!w_current->window) return;

  if (GTK_TOGGLE_BUTTON (widget)->active) {
    i_callback_add_net((TOPLEVEL*) data, 0, NULL);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_bus)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  o_erase_rubber(w_current);

  /* need to click */
  i_update_middle_button(w_current, i_callback_add_bus, _("Bus"));
  i_allow_expose();
  i_set_state(w_current, STARTDRAWBUS);
  i_update_toolbar(w_current);

  /* somewhere you need to nearest point locking... */
  w_current->inside_action = 0;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_bus_hotkey)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  o_erase_rubber(w_current);

  /* need to click */
  i_update_middle_button(w_current, i_callback_add_bus_hotkey, _("Bus"));
  i_allow_expose();
  i_set_state(w_current, STARTDRAWBUS);
  i_update_toolbar(w_current);

  o_bus_start(w_current, mouse_x, mouse_y);

  w_current->event_state=DRAWBUS;
  w_current->inside_action = 1;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current hack)
 */
void i_callback_toolbar_add_bus(GtkWidget* widget, gpointer data)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  exit_if_null(w_current);
  if (!w_current->window) return;

  if (GTK_TOGGLE_BUTTON (widget)->active) {
     i_callback_add_bus((TOPLEVEL*) data, 0, NULL);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_text)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);
  
  o_redraw_cleanstates(w_current);	
  o_erase_rubber(w_current);

  w_current->inside_action = 0;
  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);

  text_input_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use the widget parameter on this function, or do some checking...
 *  since there is a call: widget = NULL, data = 0 (will be w_current hack)
 */
void i_callback_toolbar_add_text(GtkWidget* widget, gpointer data)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  exit_if_null(w_current);
  if (!w_current->window) return;

  i_callback_add_text((TOPLEVEL*) data, 0, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_line)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  o_erase_rubber(w_current);

  i_update_middle_button(w_current, i_callback_add_line, _("Line"));
  i_allow_expose();
  i_set_state(w_current, DRAWLINE);
  w_current->inside_action = 0;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_line_hotkey)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  o_erase_rubber(w_current);

  i_update_middle_button(w_current, i_callback_add_line_hotkey, _("Line"));

  o_line_start(w_current, mouse_x, mouse_y);

  w_current->inside_action = 1;
  i_allow_expose();
  i_set_state(w_current, ENDLINE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_box)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  o_erase_rubber(w_current);

  i_update_middle_button(w_current, i_callback_add_box, _("Box"));
  w_current->inside_action = 0;
  i_allow_expose();
  i_set_state(w_current, DRAWBOX);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_box_hotkey)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  o_erase_rubber(w_current);

  i_update_middle_button(w_current, i_callback_add_box_hotkey, _("Box"));

  o_box_start(w_current, mouse_x, mouse_y);

  w_current->inside_action = 1;
  i_allow_expose();
  i_set_state(w_current, ENDBOX);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_picture)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  o_erase_rubber(w_current);

  w_current->inside_action = 0;
  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);

  picture_selection_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_picture_hotkey)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  /* If this function necessary? Yes, if you want the hotkey to work. */
  i_callback_add_picture(w_current, 0, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_circle)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  o_erase_rubber(w_current);

  i_update_middle_button(w_current, i_callback_add_circle, _("Circle"));
  w_current->inside_action = 0;
  i_allow_expose();
  i_set_state(w_current, DRAWCIRCLE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_circle_hotkey)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  o_erase_rubber(w_current);

  i_update_middle_button(w_current, i_callback_add_circle_hotkey,
                         _("Circle"));

  o_circle_start(w_current, mouse_x, mouse_y);

  w_current->inside_action = 1;
  i_allow_expose();
  i_set_state(w_current, ENDCIRCLE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_arc)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);
  
  o_redraw_cleanstates(w_current);	
  o_erase_rubber(w_current);

  i_update_middle_button(w_current, i_callback_add_arc, _("Arc"));
  w_current->inside_action = 0;
  i_allow_expose();
  i_set_state(w_current, DRAWARC);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_arc_hotkey)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  o_erase_rubber(w_current);

  i_update_middle_button(w_current, i_callback_add_arc_hotkey, _("Arc"));

  o_arc_start(w_current, mouse_x, mouse_y);

  w_current->inside_action = 1;
  i_allow_expose();
  i_set_state(w_current, ENDARC);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_pin)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  o_erase_rubber(w_current);

  i_update_middle_button(w_current, i_callback_add_pin, _("Pin"));
  w_current->inside_action = 0;
  i_allow_expose();
  i_set_state(w_current, DRAWPIN);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_pin_hotkey)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  o_erase_rubber(w_current);

  i_update_middle_button(w_current, i_callback_add_pin_hotkey, _("Pin"));

  o_pin_start(w_current, mouse_x, mouse_y);

  w_current->inside_action = 1;
  i_allow_expose();
  i_set_state(w_current, ENDPIN);
}

/*! \section hierarchy-menu Hierarchy Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(hierarchy_down_schematic)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  char *attrib=NULL;
  char *current_filename=NULL;
  int count=0;
  OBJECT *object=NULL;
  PAGE *save_first_page=NULL;
  PAGE *parent=NULL;
  int loaded_flag=FALSE;
  int page_control = 0;
  int saved_page_control = 0;
  int pcount = 0;
  int looking_inside=FALSE;

  exit_if_null(w_current);

  object = o_select_return_first_object(w_current);
  if (object != NULL) {
    /* only allow going into symbols */
    if (object->type == OBJ_COMPLEX) {

      parent = w_current->page_current;
      attrib = o_attrib_search_name_single_count(object,
                                                 "source",
                                                 count);

      /* if above is null, then look inside symbol */
      if (attrib == NULL) {
        attrib = o_attrib_search_name(object->
                                      complex->
                                      prim_objs,
                                      "source", 
                                      count);
        looking_inside = TRUE;
#if DEBUG
        printf("going to look inside now\n");
#endif
      }

      while (attrib) {

				/* look for source=filename,filename, ... */
        pcount = 0;
        
        current_filename = u_basic_breakup_string(attrib, ',', pcount);

				/* loop over all filenames */
        while(current_filename != NULL) {

          s_log_message(
                        _("Searching for source [%s]\n"), 
                        current_filename);
          saved_page_control = page_control;
          page_control = 
            s_hierarchy_down_schematic_single(
                                              w_current, 
                                              current_filename, 
                                              parent,
                                              page_control,
                                              HIERARCHY_NORMAL_LOAD);

          if (page_control != -1)  {
            a_zoom_extents(w_current, 
                          w_current->
                          page_current->
                          object_head,
                          A_PAN_DONT_REDRAW);
            o_undo_savestate(w_current, 
                             UNDO_ALL);
          }


          /* save the first page */
          if (!loaded_flag && 
              page_control != -1 && 
              page_control != 0) {
            save_first_page = w_current->
              page_current;
          }

          /* now do some error fixing */
          if (page_control == -1) {
            s_log_message(
                          _("Cannot find source [%s]\n"), 
                          current_filename);
            fprintf(stderr, 
                    _("Cannot find source [%s]\n"), 
                    current_filename); 

            /* restore this for the next */
            /* page */
            page_control = 
              saved_page_control;
          } else {
            /* this only signifies that */
            /* we tried */
            loaded_flag = TRUE;
          }

          g_free(current_filename);
          pcount++;
          current_filename = u_basic_breakup_string(attrib, ',', pcount);
        }

        if (attrib) {
          g_free(attrib);
        }

        if (current_filename) {
          g_free(current_filename);
        }

        count++;

				/* continue looking outside first */
        if (!looking_inside) {
          attrib = 
            o_attrib_search_name_single_count(object, "source", count);
        } 

				/* okay we were looking outside and didn't */
				/* find anything, so now we need to look */
				/* inside the symbol */
        if (!looking_inside && attrib == NULL && !loaded_flag ) {
          looking_inside = TRUE;
#if DEBUG
          printf("switching to go to look inside\n");
#endif
        }

        if (looking_inside) {
#if DEBUG
          printf("looking inside\n");
#endif
          attrib = o_attrib_search_name(
                                        object->complex->
                                        prim_objs,
                                        "source",
                                        count);
        }
      } 

      if (loaded_flag) {
	
        if (save_first_page) {
          w_current->page_current = 
            save_first_page;
        }
        i_set_filename(w_current, w_current->
                       page_current->page_filename);
        a_zoom_extents(w_current, 
                      w_current->page_current->object_head,
                      A_PAN_DONT_REDRAW);
        o_redraw_all(w_current);
	x_scrollbars_update(w_current);
        x_pagesel_update (w_current);
        i_update_menus(w_current);
      }
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(hierarchy_down_symbol)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  OBJECT *object;
  char *filename;

  exit_if_null(w_current);

  object = o_select_return_first_object(w_current);
  if (object != NULL) {
    /* only allow going into symbols */
    if (object->type == OBJ_COMPLEX &&
        !o_complex_is_embedded (object)) {
      filename = g_strconcat (object->complex_clib, 
                              G_DIR_SEPARATOR_S,
                              object->complex_basename, NULL);
      s_log_message(_("Searching for symbol [%s]\n"), filename);
      s_hierarchy_down_symbol(w_current, filename, 
                              w_current->page_current);
      i_set_filename(w_current,
                     w_current->page_current->page_filename);
      a_zoom_extents(w_current, 
                    w_current->page_current->object_head,
                    A_PAN_DONT_REDRAW);
      x_scrollbars_update(w_current);
      o_undo_savestate(w_current, UNDO_ALL);
      o_redraw_all(w_current);
      x_pagesel_update (w_current);
      i_update_menus(w_current);
      g_free(filename);
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(hierarchy_up)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  
  exit_if_null(w_current);
  
  s_hierarchy_up(w_current, w_current->page_current->up);
  i_set_filename(w_current, w_current->page_current->page_filename);
  x_scrollbars_update(w_current);
  o_redraw_all(w_current);
  x_pagesel_update (w_current);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  Egil Kvaleberg <egil@kvaleberg.no> on October 7, 2002 - 
 *  Provide documentation for symbol (i.e. component)
 */
DEFINE_I_CALLBACK(hierarchy_documentation)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  char *attrib_doc = NULL;
  char *attrib_device = NULL;
  char *attrib_value = NULL;
  OBJECT *object = NULL;

  exit_if_null(w_current);

  object = o_select_return_first_object(w_current);
  if (object != NULL) {
    /* only allow going into symbols */
    if (object->type == OBJ_COMPLEX) {

      /* look for "documentation" first outside, then inside symbol */
      attrib_doc = o_attrib_search_name_single_count(object, "documentation", 0);
      if (!attrib_doc) {
	attrib_doc = o_attrib_search_name(object->complex->prim_objs,
								"documentation", 0);
      }
      /* look for "device" */
      attrib_device = o_attrib_search_name_single_count(object, "device", 0);
      if (!attrib_device) {
	attrib_device = o_attrib_search_name(object->complex->prim_objs,
								"device", 0);
      }
      /* look for "value" */
      attrib_value = o_attrib_search_name_single_count(object, "value", 0);
      if (!attrib_value) {
	attrib_value = o_attrib_search_name(object->complex->prim_objs,
								"value", 0);
      }
      initiate_gschemdoc(attrib_doc,
			attrib_device,
			attrib_value,
			object->complex_basename,
			object->complex_clib);

      if (attrib_doc) g_free(attrib_doc);
      if (attrib_device) g_free(attrib_device);
      if (attrib_value) g_free(attrib_value);
    }
  }
}

/*! \section attributes-menu Attributes Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(attributes_attach)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  OBJECT *first_object;
  SELECTION *s_current;

  exit_if_null(w_current);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action) {
    return;
  }

  /* do we want to update the shortcut outside of the ifs? */
  /* probably, if this fails the user may want to try again */
  i_update_middle_button(w_current, i_callback_attributes_attach,
                         _("Attach"));

  /* skip over head */
  s_current = w_current->page_current->selection2_head->next;
  if (!s_current) {
    return;
  }

  first_object = s_current->selected_object; 
  if (!first_object) {
    return;	
  }

  /* skip over first object */
  s_current = s_current->next;
  while (s_current != NULL) {
    if (s_current->selected_object) {
      o_attrib_attach(w_current,
                      w_current->page_current->object_head,
                      s_current->selected_object,
                      first_object);
      w_current->page_current->CHANGED=1;
    }
    s_current = s_current->next;
  }
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(attributes_detach)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  SELECTION *s_current;
  OBJECT *o_current;

  exit_if_null(w_current);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action) {
    return;
  }

  /* same note as above on i_update_middle_button */
  i_update_middle_button(w_current, i_callback_attributes_detach,
                         _("Detach"));

  /* skip over head */
  s_current = w_current->page_current->selection2_head->next;
  while (s_current != NULL) {
    o_current = s_current->selected_object;
    if (o_current) {
      if (o_current->attribs) {
        o_attrib_free_all(w_current, 
                          o_current->attribs);
        o_current->attribs = NULL;
        w_current->page_current->CHANGED=1;
      }
    }
    s_current = s_current->next;
  }
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(attributes_show_name)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  OBJECT *object;

  exit_if_null(w_current);

  object = o_select_return_first_object(w_current);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action) {
    return;
  }

  i_update_middle_button(w_current, i_callback_attributes_show_name,
                         _("ShowN"));

  if (object != NULL) {
    o_attrib_toggle_show_name_value(w_current, 
                                    w_current->page_current->
                                    selection2_head->next,
                                    SHOW_NAME);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(attributes_show_value)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  OBJECT *object;

  exit_if_null(w_current);

  object = o_select_return_first_object(w_current);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action) {
    return;
  }

  i_update_middle_button(w_current, i_callback_attributes_show_value,
                         _("ShowV"));

  if (object != NULL) {
    o_attrib_toggle_show_name_value(w_current, 
                                    w_current->page_current->
                                    selection2_head->next,
                                    SHOW_VALUE);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(attributes_show_both)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  OBJECT *object;

  exit_if_null(w_current);

  object = o_select_return_first_object(w_current);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action) {
    return;
  }

  i_update_middle_button(w_current, i_callback_attributes_show_both,
                         _("ShowB"));

  if (object != NULL) {
    o_attrib_toggle_show_name_value(w_current, 
                                    w_current->page_current->
                                    selection2_head->next,
                                    SHOW_NAME_VALUE);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(attributes_visibility_toggle)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  OBJECT *object;

  exit_if_null(w_current);

  object = o_select_return_first_object(w_current);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action) {
    return;
  }

  i_update_middle_button(w_current,
                         i_callback_attributes_visibility_toggle,
                         _("VisToggle"));

  if (object != NULL) {
    o_attrib_toggle_visibility(w_current, 
                               w_current->page_current->
                               selection2_head->next);
  }
}

/*! \section script-menu Script Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  not currently implemented
 */
DEFINE_I_CALLBACK(script_console)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);
  printf(_("Sorry but this is a non-functioning menu option\n"));
}

/*! \section layers-menu Layers Menu Callback Functions */

/*! \section options-menu Options Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat last command doesn't make sense on options either??? (does it?)
 */
DEFINE_I_CALLBACK(options_text_size)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);
  text_size_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(options_snap_size)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);
  snap_size_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  repeat last command doesn't make sense on options either??? (does
 *  it?)
 */
DEFINE_I_CALLBACK(options_afeedback)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  if (w_current->actionfeedback_mode == BOUNDINGBOX) {
    w_current->actionfeedback_mode = OUTLINE;
    s_log_message(_("Action feedback mode set to OUTLINE\n"));
  } else {
    w_current->actionfeedback_mode = BOUNDINGBOX;
    s_log_message(_("Action feedback mode set to BOUNDINGBOX\n"));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(options_grid)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  if (w_current->grid) {
    w_current->grid = 0;
    s_log_message(_("Grid OFF\n"));
  } else {
    w_current->grid = 1;
    s_log_message(_("Grid ON\n"));
  }

  o_redraw_all(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(options_snap)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  if (w_current->snap) {
    w_current->snap = 0;
    s_log_message(_("Snap OFF (CAUTION!)\n"));
  } else {
    w_current->snap = 1;
    s_log_message(_("Snap ON\n"));
  }
  i_show_state(w_current, NULL); /* update status on screen */
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  Rubber band is cool !
 *  Added on/off option from the pull down menu
 *  Chris Ellec - January 2001
 */
DEFINE_I_CALLBACK(options_rubberband)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  if (w_current->netconn_rubberband) {
    w_current->netconn_rubberband = 0;
    s_log_message(_("Rubber band OFF \n"));
  } else {
    w_current->netconn_rubberband = 1;
    s_log_message(_("Rubber band ON\n"));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(options_show_log_window)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);
  x_log_open ();
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  this is Ales' catch all misc callback
 */
DEFINE_I_CALLBACK(misc)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  s_tile_print(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  this is Ales' second catch all misc callback
 */
DEFINE_I_CALLBACK(misc2)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;
  OBJECT *first = o_select_return_first_object(w_current);

  if (first) {
    /*o_cue_draw_single(w_current, first);*/
    o_cue_undraw(w_current, first);
    s_conn_print(first->conn_list);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  this is Ales' third catch all misc callback
 */
DEFINE_I_CALLBACK(misc3)
{

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  HACK: be sure that you don't use the widget parameter in this one,
 *  since it is being called with a null, I suppose we should call it
 *  with the right param.
 */
DEFINE_I_CALLBACK(cancel)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);

  if ( (w_current->inside_action) && 
       (w_current->rotated_inside != 0)) {
    o_undo_callback(w_current, UNDO_ACTION);	 
    w_current->rotated_inside = 0;
  }

  /* leave this on for now... but it might have to change */
  /* this is problematic since we don't know what the right mode */
  /* (when you cancel inside an action) should be */
  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);

  /* clear the key guile command-sequence */
  scm_c_eval_string ("(set! current-command-sequence '())");

  /* see above comment hack */
#if 0
  set_cursor_normal();
#endif

  if (w_current->inside_action) { 
     o_redraw_all(w_current); 
  }

  /* it is possible to cancel in the middle of a complex place
   * so lets be sure to clean up the complex_place_head
   * structure and also clean up the attrib_place_head.
   * remember these don't remove the head structure */
  o_list_delete_rest(w_current,
                     w_current->page_current->complex_place_head);
  o_list_delete_rest(w_current,
                     w_current->page_current->attrib_place_head);

  /* also free internal current_attribute */
  o_attrib_free_current(w_current);

  w_current->inside_action=0;
}

/*! \section help-menu Help Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(help_about)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);
  about_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(help_manual)
{
  initiate_gschemdoc("-m", NULL, NULL, NULL, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(help_hotkeys)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);
  x_dialog_hotkeys(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(options_show_coord_window)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);
  coord_dialog (w_current, mouse_x, mouse_y);
}

#if 0 /* experimental */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(preview)
{
  TOPLEVEL *w_current = (TOPLEVEL *) data;

  exit_if_null(w_current);
  setup_preview(w_current);
}
#endif

/* these is a special wrapper function which cannot use the above */
/* DEFINE_I_CALLBACK macro */

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  When invoked (via signal delete_event), closes the current window
 *  if this is the last window, quit gschem
 *  used when you click the close button on the window which sends a DELETE
 *  signal to the app
 */
void i_callback_close_wm ( GtkWidget *widget, GdkEvent *event, 
	                   gpointer data ) 
{

  TOPLEVEL *w_current = (TOPLEVEL *) data;
  exit_if_null(w_current);

  x_window_close(w_current);
}
