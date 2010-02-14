/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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

#include "gschem.h"

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
/* every i_callback functions have the same footprint */
#define DEFINE_I_CALLBACK(name)				\
	void i_callback_ ## name(gpointer data,		\
			         guint callback_action,	\
			         GtkWidget *widget)

/*! \section callback-intro Callback Functions
 * right now, all callbacks except for the ones on the File menu have
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*)data;
  PAGE *page;

  exit_if_null (w_current);

  /* create a new page */
  page = x_window_open_page (w_current, NULL);
  x_window_set_current_page (w_current, page);
  s_log_message (_("New page created [%s]\n"), page->page_filename);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  exit_if_null(w_current);
  if (!w_current->window) return;
    
  i_callback_file_new(data, 0, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(file_new_window)
{
  GSCHEM_TOPLEVEL *w_current;
  PAGE *page;

  w_current = gschem_toplevel_new ();
  w_current->toplevel = s_toplevel_new ();

  w_current->toplevel->load_newer_backup_func = x_fileselect_load_backup;
  w_current->toplevel->load_newer_backup_data = w_current;

  o_text_set_rendered_bounds_func (w_current->toplevel,
                                   o_text_get_rendered_bounds, w_current);
  x_window_setup (w_current);

  page = x_window_open_page (w_current, NULL);
  x_window_set_current_page (w_current, page);
  s_log_message (_("New Window created [%s]\n"), page->page_filename);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  x_fileselect_open (w_current);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  exit_if_null(w_current);
  if (!w_current->window) return;

  i_callback_file_open(data, 0, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(file_script)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  /*! \todo probably there should be a flag that says whether
   *   page_filename is derived from untitled_name or specified by
   *   a user. Some twisted people might name their files like
   *   untitled_name. :-)
   */
  if (strstr(w_current->toplevel->page_current->page_filename,
             w_current->toplevel->untitled_name)) {
    x_fileselect_save (w_current);
  } else {
    x_window_save_page (w_current,
                        w_current->toplevel->page_current,
                        w_current->toplevel->page_current->page_filename);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  exit_if_null(w_current);
  if (!w_current->window) return;

  i_callback_file_save(data, 0, NULL);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  if (s_page_save_all(w_current->toplevel)) {
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);
  x_fileselect_save (w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(file_print)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  char *base=NULL, *filename;
  char *ps_filename=NULL;
  
  exit_if_null(w_current);
  exit_if_null(w_current->toplevel->page_current->page_filename);

  /* shortcut */
  filename = w_current->toplevel->page_current->page_filename;

  /* get the base file name */
  if (g_str_has_suffix(filename, ".sch")) {
    /* the filename ends with ".sch", remove it */
    base = g_strndup(filename, strlen(filename) - strlen(".sch"));
  } else {
    /* the filename does not end with .sch */
    base = g_strdup (filename);
  }

  /* add ".ps" tp the base filename */
  ps_filename = g_strconcat (base, ".ps", NULL);
  g_free(base);

  if (output_filename) {
    x_print_setup(w_current, output_filename);
  } else {
    x_print_setup(w_current, ps_filename);
  }

  g_free(ps_filename);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(file_write_png)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  x_image_setup(w_current);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  /* If we're cancelling from a move action, re-wind the
   * page contents back to their state before we started.
   *
   * It "might" be nice to sub-undo rotates / zoom changes
   * made whilst moving components, but when the undo code
   * hits s_page_delete(), the place list objects are free'd.
   * Since they are also contained in the schematic page, a
   * crash occurs when the page objects are free'd.
   * */
  if (w_current->inside_action &&
      (w_current->event_state == MOVE ||
       w_current->event_state == ENDMOVE)) {
    i_callback_cancel (w_current, 0, NULL);
  } else {
    w_current->toplevel->DONT_REDRAW = 0;
    o_undo_callback(w_current, UNDO_ACTION);
  }
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  exit_if_null(w_current);
  if (!w_current->window) return;

  i_callback_edit_undo(data, 0, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_redo)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  w_current->toplevel->DONT_REDRAW = 0;
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  exit_if_null(w_current);
  if (!w_current->window) return;

  i_callback_edit_redo(data, 0, NULL);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  exit_if_null(w_current);
  if (!w_current->window) return;

  if (GTK_TOGGLE_BUTTON (widget)->active) {
    if (!o_invalidate_rubber (w_current)) {
      i_callback_cancel(w_current, 0, NULL);
    }
    i_callback_edit_select(data, 0, NULL);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_copy)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_edit_copy, _("Copy"));
  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  gint wx, wy; 

  exit_if_null(w_current);

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  i_update_middle_button(w_current, i_callback_edit_copy_hotkey, _("Copy"));
  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);
    w_current->event_state = COPY;
    o_copy_start(w_current, wx, wy);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_edit_copy, _("Multiple Copy"));
  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  gint wx, wy; 

  exit_if_null(w_current);

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  i_update_middle_button(w_current, i_callback_edit_mcopy_hotkey, _("Multiple Copy"));
  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);	
    w_current->event_state = MCOPY; 
    o_copy_start(w_current, wx, wy);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_edit_move, _("Move"));
  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  gint wx, wy; 

  exit_if_null(w_current);

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  i_update_middle_button(w_current, i_callback_edit_move_hotkey, _("Move"));
  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);
    o_move_start(w_current, wx, wy);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_edit_delete, _("Delete"));

  if (o_select_return_first_object(w_current)) {
    o_redraw_cleanstates(w_current);	
    o_delete_selected(w_current);
    /* if you delete the objects you must go into select
     * mode after the delete */
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
    i_update_toolbar(w_current);
    i_update_menus(w_current);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_edit)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_edit_edit, _("Edit"));
  o_edit(w_current, geda_list_get_glist( w_current->toplevel->page_current->selection_list ) );
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_pin_type)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  i_update_middle_button (w_current, i_callback_edit_pin_type, _("Edit pin type"));

  x_dialog_edit_pin_type (w_current,
                          geda_list_get_glist (w_current->toplevel->
                                                 page_current->selection_list));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_text)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  /* If inside an appropriate action, send a button 2 released,
   * so rotating will be handled by x_event.c */
  if ( w_current->inside_action &&
       (w_current->event_state == ENDCOMP ||
        w_current->event_state == ENDTEXT ||
        w_current->event_state == ENDMOVE ||
        w_current->event_state == ENDCOPY ||
        w_current->event_state == ENDMCOPY ||
        w_current->event_state == ENDPASTE )) {
      GdkEvent* event;

      event = gdk_event_new(GDK_BUTTON_RELEASE);
      ((GdkEventButton*) event)->button = 2;
      x_event_button_released (NULL, (GdkEventButton *) event, w_current);
      gdk_event_free(event);

      return;
    }

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  GList *object_list;
  gint wx, wy; 

  exit_if_null(w_current);

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  /* If inside an appropriate action, send a button 2 released,
   * so rotating will be handled by x_event.c */
  if ( w_current->inside_action &&
       (w_current->event_state == ENDCOMP ||
        w_current->event_state == ENDTEXT ||
        w_current->event_state == ENDMOVE ||
        w_current->event_state == ENDCOPY ||
        w_current->event_state == ENDMCOPY ||
        w_current->event_state == ENDPASTE )) {
      GdkEvent* event;

      event = gdk_event_new(GDK_BUTTON_RELEASE);
      ((GdkEventButton*) event)->button = 2;
      x_event_button_released (NULL, (GdkEventButton *) event, w_current);
      gdk_event_free(event);

      return;
    }

  o_redraw_cleanstates(w_current);

  object_list = geda_list_get_glist( w_current->toplevel->page_current->selection_list );

  if (object_list) {
    i_update_middle_button(w_current,
                           i_callback_edit_rotate_90_hotkey, _("Rotate"));
    /* Allow o_rotate_world_update to redraw the objects */
    w_current->toplevel->DONT_REDRAW = 0;
    o_rotate_world_update(w_current, wx, wy, 90, object_list);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  GList *object_list;
  gint wx, wy; 

  exit_if_null(w_current);

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_redraw_cleanstates(w_current);	

  object_list = geda_list_get_glist( w_current->toplevel->page_current->selection_list );

  if (object_list) {
    i_update_middle_button(w_current,
                           i_callback_edit_mirror_hotkey, _("Mirror"));
    o_mirror_world_update(w_current, wx, wy, object_list);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current,
                         i_callback_edit_translate, _("Translate"));

  if (w_current->toplevel->snap == SNAP_OFF) {
    s_log_message(_("WARNING: Do not translate with snap off!\n"));
    s_log_message(_("WARNING: Turning snap on and continuing "
                  "with translate.\n"));
    w_current->toplevel->snap = SNAP_GRID;
    i_show_state(w_current, NULL); /* update status on screen */
  }

  if (w_current->toplevel->snap_size != 100) {
    s_log_message(_("WARNING: Snap grid size is "
                  "not equal to 100!\n"));
    s_log_message(_("WARNING: If you are translating a symbol "
                  "to the origin, the snap grid size should be "
                  "set to 100\n"));
  }

  translate_dialog(w_current);
}

DEFINE_I_CALLBACK(edit_invoke_macro)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  gtk_widget_show(w_current->macro_box);
  gtk_widget_grab_focus(w_current->macro_entry);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  This function embedds all objects in selection list
 *
 */
DEFINE_I_CALLBACK(edit_embed)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  OBJECT *o_current;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_edit_embed, _("Embed"));
  /* anything selected ? */
  if (o_select_selected(w_current)) {
    /* yes, embed each selected component */
    GList *s_current =
      geda_list_get_glist( w_current->toplevel->page_current->selection_list );

    while (s_current != NULL) {
      o_current = (OBJECT *) s_current->data;
      g_assert (o_current != NULL);
      if ( (o_current->type == OBJ_COMPLEX) ||
	   (o_current->type == OBJ_PICTURE) ) {
        o_embed (w_current->toplevel, o_current);
      }
      s_current = g_list_next(s_current);
    }
    o_invalidate_glist (w_current, geda_list_get_glist (
                          w_current->toplevel->page_current->selection_list));
    o_undo_savestate(w_current, UNDO_ALL);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  OBJECT *o_current;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_edit_unembed, _("Unembed"));
  /* anything selected ? */
  if (o_select_selected(w_current)) {
    /* yes, unembed each selected component */
    GList *s_current =
      geda_list_get_glist( w_current->toplevel->page_current->selection_list );

    while (s_current != NULL) {
      o_current = (OBJECT *) s_current->data;
      g_assert (o_current != NULL);
      if ( (o_current->type == OBJ_COMPLEX) ||
           (o_current->type == OBJ_PICTURE) ) {
        o_unembed (w_current->toplevel, o_current);
      }
      s_current = g_list_next(s_current);
    }
    o_invalidate_glist (w_current, geda_list_get_glist (
                          w_current->toplevel->page_current->selection_list));
    o_undo_savestate(w_current, UNDO_ALL);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *o_current;
  GList* selection_copy;
  GList* s_current;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_edit_update, _("Update"));
  /* anything selected ? */
  if (o_select_selected(w_current)) {

    /* yes, update each selected component, but operate from a copy of the */
    /* selection list, since o_update_component will modify the selection */

    /* After the following code executes, only OBJ_COMPLEX object will be */
    /* left selected. */

    /* g_list_copy does a shallow copy which is exactly what we need here */
    selection_copy = g_list_copy (
              geda_list_get_glist (toplevel->page_current->selection_list));
    s_current = selection_copy;
    while (s_current != NULL) {
      o_current = (OBJECT *) s_current->data;
      g_assert (o_current != NULL);
      if (o_current->type == OBJ_COMPLEX) {
        o_update_component (w_current, o_current);
      }
      else
      {
        /* object was not a OBJ_COMPLEX, so unselect it. */
        o_selection_remove (toplevel,
                            toplevel->page_current->selection_list, o_current);
      }
      s_current = g_list_next(s_current);
    }
    g_list_free(selection_copy);

    /* Make sure the display is up to date */
    o_invalidate_all (w_current);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action)
    return;

  i_update_middle_button(w_current,
                         i_callback_edit_show_hidden,
                         _("ShowHidden"));

  o_edit_show_hidden (w_current,
                      s_page_objects (w_current->toplevel->page_current));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_make_visible)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action)
    return;

  i_update_middle_button(w_current,
                         i_callback_edit_make_visible,
                         _("MakeVisible"));

  o_edit_make_visible (w_current,
                       s_page_objects (w_current->toplevel->page_current));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_find)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  line_type_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(edit_filltype)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  fill_type_dialog(w_current);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);
  o_invalidate_all (w_current);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  /* scroll bar stuff */
  a_zoom(w_current, ZOOM_FULL, DONTCARE, 0);

  if (w_current->undo_panzoom) {
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY); 
  }
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  /* scroll bar stuff */
  a_zoom_extents (w_current,
                  s_page_objects (w_current->toplevel->page_current), 0);
  if (w_current->undo_panzoom) {
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
  }
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  w_current->inside_action = 0;
  i_set_state(w_current, ZOOMBOXSTART);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(view_zoom_box_hotkey)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  gint wx, wy; 

  exit_if_null(w_current);

  if (!x_event_get_pointer_position(w_current, FALSE, &wx, &wy))
    return;

  o_redraw_cleanstates(w_current);
  a_zoom_box_start(w_current, wx, wy);

  w_current->inside_action = 1;
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  a_zoom(w_current, ZOOM_IN, MENU, 0);

  if (w_current->undo_panzoom) {
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY); 
  }
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  a_zoom(w_current, ZOOM_OUT, MENU, 0);
 
  if (w_current->undo_panzoom) {
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY); 
  }
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  a_zoom(w_current, ZOOM_IN, HOTKEY, 0);

  if (w_current->undo_panzoom) {
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY); 
  }
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  a_zoom(w_current, ZOOM_OUT, HOTKEY, 0);

  if (w_current->undo_panzoom) {
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY); 
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(view_pan)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  w_current->inside_action = 0;
  i_set_state(w_current, STARTPAN);

  /* I don't know if this would get in the way */
  i_update_middle_button(w_current, i_callback_view_pan, _("Pan"));
}

/*! \brief Scheme callback function that moves the viewport to the left.
 *
 * The distance can be set with "keyboardpan-gain" scheme callback.
 */
DEFINE_I_CALLBACK(view_pan_left)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  a_pan_mouse(w_current, w_current->keyboardpan_gain, 0);
}

/*! \brief Scheme callback function that moves the viewport to the right.
 *
 * The distance can be set with "keyboardpan-gain" scheme callback.
 */
DEFINE_I_CALLBACK(view_pan_right)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  /* yes, that's a negative sign there */
  a_pan_mouse(w_current, -w_current->keyboardpan_gain, 0);
}

/*! \brief Scheme callback function that moves the viewport up.
 *
 * The distance can be set with "keyboardpan-gain" scheme callback.
 */
DEFINE_I_CALLBACK(view_pan_up)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  a_pan_mouse(w_current, 0, w_current->keyboardpan_gain);
}

/*! \brief Scheme callback function that moves the viewport down.
 *
 * The distance can be set with "keyboardpan-gain" scheme callback.
 */
DEFINE_I_CALLBACK(view_pan_down)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  /* yes, that's a negative sign there */
  a_pan_mouse(w_current, 0, -w_current->keyboardpan_gain);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(view_pan_hotkey)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  gint wx, wy; 

  exit_if_null(w_current);

  if (!x_event_get_pointer_position(w_current, FALSE, &wx, &wy))
    return;

  i_update_middle_button(w_current, i_callback_view_pan_hotkey, _("Pan"));

  a_pan(w_current, wx, wy);

  if (w_current->undo_panzoom) {
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY); 
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(view_update_cues)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  i_update_middle_button(w_current,
                         i_callback_view_update_cues, _("Update Cues"));

  o_invalidate_all (w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK (view_dark_colors)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  x_color_free ();
  /* Change the scheme here */
  g_scm_c_eval_string_protected ("(load (build-path geda-rc-path \"gschem-colormap-darkbg\"))");
  x_color_allocate ();

  o_invalidate_all (w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK (view_light_colors)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  x_color_free ();
  /* Change the scheme here */
  g_scm_c_eval_string_protected ("(load (build-path geda-rc-path \"gschem-colormap-lightbg\"))");
  x_color_allocate ();

  o_invalidate_all (w_current);
}

/*! \section page-menu Page Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(page_manager)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*)data;
  TOPLEVEL *toplevel = w_current->toplevel;
  PAGE *p_current = toplevel->page_current;
  PAGE *p_new;
  GList *iter;

  exit_if_null(w_current);

  iter = g_list_find( geda_list_get_glist( toplevel->pages ), p_current );
  iter = g_list_next( iter );

  if (iter == NULL) {
    return;
  }

  if (w_current->enforce_hierarchy) {
    p_new = s_hierarchy_find_next_page(toplevel->pages, p_current);
  } else {
    p_new = (PAGE *)iter->data;
  }

  if (p_new == NULL || p_new == p_current) {
    return;
  }

  x_window_set_current_page (w_current, p_new);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(page_prev)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*)data;
  TOPLEVEL *toplevel = w_current->toplevel;
  PAGE *p_current = toplevel->page_current;
  PAGE *p_new;
  GList *iter;

  exit_if_null(w_current);

  iter = g_list_find( geda_list_get_glist( toplevel->pages ), p_current );
  iter = g_list_previous( iter );

  if ( iter == NULL  )
    return;

  p_new = (PAGE *)iter->data;

  if (w_current->enforce_hierarchy) {
    p_new = s_hierarchy_find_prev_page(toplevel->pages, p_current);
  } else {
    p_new = (PAGE *)iter->data;
  }

  if (p_new == NULL || p_new == p_current) {
    return;
  }

  x_window_set_current_page (w_current, p_new);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(page_new)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*)data;
  PAGE *page;

  exit_if_null(w_current);

  /* create a new page */
  page = x_window_open_page (w_current, NULL);
  x_window_set_current_page (w_current, page);
  s_log_message (_("New page created [%s]\n"), page->page_filename);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(page_close)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  if (w_current->toplevel->page_current->CHANGED) {
    x_dialog_close_changed_page (w_current, w_current->toplevel->page_current);
  } else {
    x_window_close_page (w_current, w_current->toplevel->page_current);
  }

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \bug may have memory leak?
 */
DEFINE_I_CALLBACK(page_revert)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  PAGE *page;
  gchar *filename;
  int page_control;
  int up;
  int response;
  GtkWidget* dialog;

  exit_if_null(w_current);

  dialog = gtk_message_dialog_new ((GtkWindow*) w_current->main_window,
                                   GTK_DIALOG_DESTROY_WITH_PARENT,
                                   GTK_MESSAGE_QUESTION,
                                   GTK_BUTTONS_YES_NO,
                                   _("Really revert page?"));

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
					  GTK_RESPONSE_YES,
					  GTK_RESPONSE_NO,
					  -1);

  response = gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);

  if (response != GTK_RESPONSE_YES )
    return;

  /* save this for later */
  filename = g_strdup (w_current->toplevel->page_current->page_filename);
  page_control = w_current->toplevel->page_current->page_control;
  up = w_current->toplevel->page_current->up;

  /* delete the page, then re-open the file as a new page */
  s_page_delete (w_current->toplevel, w_current->toplevel->page_current);

  page = x_window_open_page (w_current, filename);

  /* make sure we maintain the hierarchy info */
  page->page_control = page_control;
  page->up = up;

  x_window_set_current_page (w_current, page);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(page_discard)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*)data;

  exit_if_null(w_current);

  x_window_close_page (w_current, w_current->toplevel->page_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(page_print)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  s_page_print_all(w_current->toplevel);
}

/*! \section clipboard-menu Clipboard Menu Callback Functions */
/*! \brief Copy selection to clipboard.
 *  \par Function Description
 * Copies the current selection to the clipboard, via buffer 0.
 */
DEFINE_I_CALLBACK(clipboard_copy)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null (w_current);
  if (!o_select_selected (w_current)) return;

  i_update_middle_button (w_current, i_callback_clipboard_copy,
                          _("Copy to clipboard"));

  o_buffer_copy (w_current, 0);
  x_clipboard_set (w_current, object_buffer[0]);
}

/*! \brief Cut selection to clipboard.
 *  \par Function Description
 * Cut the current selection to the clipboard, via buffer 0.
 */
DEFINE_I_CALLBACK(clipboard_cut)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null (w_current);
  if (!o_select_selected (w_current)) return;

  i_update_middle_button (w_current, i_callback_clipboard_cut,
                          _("Cut to clipboard"));

  o_buffer_cut (w_current, 0);
  x_clipboard_set (w_current, object_buffer[0]);
}

/*! \brief Start pasting clipboard contents.
 *  \par Function Description
 * Cut the current selection to the clipboard, via buffer 0.
 */
DEFINE_I_CALLBACK(clipboard_paste)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL *) data;
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *object_list = NULL;

  exit_if_null(w_current);

  i_update_middle_button (w_current, i_callback_buffer_paste1, _("Paste from clipboard"));

  object_list = x_clipboard_get (w_current);

  if (object_list != NULL) {
    s_delete_object_glist (toplevel, object_buffer[0]);
    object_buffer[0] = object_list;
    o_redraw_cleanstates (w_current);
    w_current->buffer_number = 0;
    w_current->inside_action = 1;
    i_set_state (w_current, STARTPASTE);
  } else {
    i_set_state_msg (w_current, SELECT, _("Empty buffer"));
  }
}

/*! \brief Start pasting clipboard contents (hotkey version)
 *  \par Function Description
 *  It's not entirely clear what the difference is between this and
 *  i_callback_clipboard_paste()...
 */
DEFINE_I_CALLBACK(clipboard_paste_hotkey)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL *) data;
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *object_list = NULL;
  gint wx, wy;

  exit_if_null (w_current);

  if (!x_event_get_pointer_position (w_current, TRUE, &wx, &wy))
    return;

  object_list = x_clipboard_get (w_current);

  if (object_list == NULL) return;
  s_delete_object_glist (toplevel, object_buffer[0]);
  object_buffer[0] = object_list;

  o_buffer_paste_start (w_current, wx, wy, 0);
}

/*! \section buffer-menu Buffer Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_copy1)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  if (!o_select_selected (w_current))
    return;

  i_update_middle_button(w_current, i_callback_buffer_copy1, _("Copy 1"));
  o_buffer_copy(w_current, 0);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_copy2)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  if (!o_select_selected (w_current))
    return;

  i_update_middle_button(w_current, i_callback_buffer_copy2, _("Copy 2"));
  o_buffer_copy(w_current, 1);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_copy3)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  if (!o_select_selected (w_current))
    return;

  i_update_middle_button(w_current, i_callback_buffer_copy3, _("Copy 3"));
  o_buffer_copy(w_current, 2);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_copy4)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  if (!o_select_selected (w_current))
    return;

  i_update_middle_button(w_current, i_callback_buffer_copy4, _("Copy 4"));
  o_buffer_copy(w_current, 3);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_copy5)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  if (!o_select_selected (w_current))
    return;

  i_update_middle_button(w_current, i_callback_buffer_copy5, _("Copy 5"));
  o_buffer_copy(w_current, 4);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_cut1)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  if (!o_select_selected (w_current))
    return;

  i_update_middle_button(w_current, i_callback_buffer_cut1, _("Cut 1"));
  o_buffer_cut(w_current, 0);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_cut2)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  if (!o_select_selected (w_current))
    return;

  i_update_middle_button(w_current, i_callback_buffer_cut2, _("Cut 2"));
  o_buffer_cut(w_current, 1);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_cut3)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  if (!o_select_selected (w_current))
    return;

  i_update_middle_button(w_current, i_callback_buffer_cut3, _("Cut 3"));
  o_buffer_cut(w_current, 2);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_cut4)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  if (!o_select_selected (w_current))
    return;

  i_update_middle_button(w_current, i_callback_buffer_cut4, _("Cut 4"));
  o_buffer_cut(w_current, 3);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_cut5)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  if (!o_select_selected (w_current))
    return;

  i_update_middle_button(w_current, i_callback_buffer_cut5, _("Cut 5"));
  o_buffer_cut(w_current, 4);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_paste1)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_buffer_paste1, _("Paste 1"));
  if (object_buffer[0] != NULL) {
    o_redraw_cleanstates(w_current);
    w_current->buffer_number = 0;
    w_current->inside_action = 1;
    i_set_state(w_current, STARTPASTE);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_buffer_paste2, _("Paste 2"));
  if (object_buffer[1] != NULL) {
    o_redraw_cleanstates(w_current);
    w_current->buffer_number = 1;
    w_current->inside_action = 1;
    i_set_state(w_current, STARTPASTE);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_buffer_paste3, _("Paste 3"));
  if (object_buffer[2] != NULL) {
    o_redraw_cleanstates(w_current);
    w_current->buffer_number = 2;
    w_current->inside_action = 1;
    i_set_state(w_current, STARTPASTE);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_buffer_paste4, _("Paste 4"));
  if (object_buffer[3] != NULL) {
    o_redraw_cleanstates(w_current);
    w_current->buffer_number = 3;
    w_current->inside_action = 1;
    i_set_state(w_current, STARTPASTE);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  i_update_middle_button(w_current, i_callback_buffer_paste5, _("Paste 5"));
  if (object_buffer[4] != NULL) {
    o_redraw_cleanstates(w_current);
    w_current->buffer_number = 4;
    w_current->inside_action = 1;
    i_set_state(w_current, STARTPASTE);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  gint wx, wy; 

  exit_if_null(w_current);

  if (object_buffer[0] == NULL) {
    return;
  }

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_buffer_paste_start(w_current, wx, wy, 0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_paste2_hotkey)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  gint wx, wy; 

  exit_if_null(w_current);

  if (object_buffer[1] == NULL) {
    return;
  }

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_buffer_paste_start(w_current, wx, wy, 1);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_paste3_hotkey)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  gint wx, wy; 

  exit_if_null(w_current);

  if (object_buffer[2] == NULL) {
    return;
  }

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_buffer_paste_start(w_current, wx, wy, 2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_paste4_hotkey)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  gint wx, wy; 

  exit_if_null(w_current);

  if (object_buffer[3] == NULL) {
    return;
  }

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_buffer_paste_start(w_current, wx, wy, 3);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(buffer_paste5_hotkey)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  gint wx, wy; 

  exit_if_null(w_current);

  if (object_buffer[4] == NULL) {
    return;
  }

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_buffer_paste_start(w_current, wx, wy, 4);
}

/*! \section add-menu Add Menu Callback Functions */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_component)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  o_redraw_cleanstates (w_current);
  x_compselect_open (w_current);

  i_update_middle_button(w_current,
                         i_callback_add_component, _("Component"));

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  exit_if_null(w_current);
  if (!w_current->window) return;

  i_callback_add_component(data, 0, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_attribute)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  attrib_edit_dialog(w_current, NULL, FROM_MENU);
  i_update_middle_button(w_current, i_callback_add_attribute,
                         _("Attribute"));

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  attrib_edit_dialog(w_current, NULL, FROM_HOTKEY);
  i_update_middle_button(w_current, i_callback_add_attribute_hotkey,
                         _("Attribute"));

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  o_invalidate_rubber (w_current);
  o_net_reset(w_current);

  /* need to click */
  i_update_middle_button(w_current, i_callback_add_net, _("Net"));
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  gint wx, wy; 

  exit_if_null(w_current);

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_redraw_cleanstates(w_current);	
  o_invalidate_rubber (w_current);
  o_net_reset(w_current);

  /* need to click */
  i_update_middle_button(w_current, i_callback_add_net_hotkey, _("Net"));
  i_set_state(w_current, STARTDRAWNET);
  i_update_toolbar(w_current);

  o_net_start(w_current, wx, wy);

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  exit_if_null(w_current);
  if (!w_current->window) return;

  if (GTK_TOGGLE_BUTTON (widget)->active) {
    i_callback_add_net(data, 0, NULL);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_bus)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  o_invalidate_rubber (w_current);

  /* need to click */
  i_update_middle_button(w_current, i_callback_add_bus, _("Bus"));
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  gint wx, wy; 

  exit_if_null(w_current);

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_redraw_cleanstates(w_current);	
  o_invalidate_rubber (w_current);

  /* need to click */
  i_update_middle_button(w_current, i_callback_add_bus_hotkey, _("Bus"));
  i_set_state(w_current, STARTDRAWBUS);
  i_update_toolbar(w_current);

  o_bus_start(w_current, wx, wy);

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  exit_if_null(w_current);
  if (!w_current->window) return;

  if (GTK_TOGGLE_BUTTON (widget)->active) {
     i_callback_add_bus(data, 0, NULL);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_text)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);
  
  o_redraw_cleanstates(w_current);	
  o_invalidate_rubber (w_current);

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  exit_if_null(w_current);
  if (!w_current->window) return;

  i_callback_add_text(data, 0, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_line)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  o_invalidate_rubber (w_current);

  i_update_middle_button(w_current, i_callback_add_line, _("Line"));
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  gint wx, wy;
  
  exit_if_null(w_current);

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_redraw_cleanstates(w_current);	
  o_invalidate_rubber (w_current);

  i_update_middle_button(w_current, i_callback_add_line_hotkey, _("Line"));

  o_line_start(w_current, wx, wy);

  w_current->inside_action = 1;
  i_set_state(w_current, ENDLINE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_box)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  o_invalidate_rubber (w_current);

  i_update_middle_button(w_current, i_callback_add_box, _("Box"));
  w_current->inside_action = 0;
  i_set_state(w_current, DRAWBOX);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_box_hotkey)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  gint wx, wy; 

  exit_if_null(w_current);

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_redraw_cleanstates(w_current);	
  o_invalidate_rubber (w_current);

  i_update_middle_button(w_current, i_callback_add_box_hotkey, _("Box"));

  o_box_start(w_current, wx, wy);

  w_current->inside_action = 1;
  i_set_state(w_current, ENDBOX);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_picture)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  o_invalidate_rubber (w_current);

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  o_invalidate_rubber (w_current);

  i_update_middle_button(w_current, i_callback_add_circle, _("Circle"));
  w_current->inside_action = 0;
  i_set_state(w_current, DRAWCIRCLE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_circle_hotkey)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  gint wx, wy; 

  exit_if_null(w_current);

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_redraw_cleanstates(w_current);	
  o_invalidate_rubber (w_current);

  i_update_middle_button(w_current, i_callback_add_circle_hotkey,
                         _("Circle"));

  o_circle_start(w_current, wx, wy);

  w_current->inside_action = 1;
  i_set_state(w_current, ENDCIRCLE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_arc)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);
  
  o_redraw_cleanstates(w_current);	
  o_invalidate_rubber (w_current);

  i_update_middle_button(w_current, i_callback_add_arc, _("Arc"));
  w_current->inside_action = 0;
  i_set_state(w_current, DRAWARC);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_arc_hotkey)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  gint wx, wy; 

  exit_if_null(w_current);

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_redraw_cleanstates(w_current);	
  o_invalidate_rubber (w_current);

  i_update_middle_button(w_current, i_callback_add_arc_hotkey, _("Arc"));

  o_arc_start(w_current, wx, wy);

  w_current->inside_action = 1;
  i_set_state(w_current, ENDARC);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_pin)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  o_redraw_cleanstates(w_current);	
  o_invalidate_rubber (w_current);

  i_update_middle_button(w_current, i_callback_add_pin, _("Pin"));
  w_current->inside_action = 0;
  i_set_state(w_current, DRAWPIN);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(add_pin_hotkey)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  gint wx, wy; 

  exit_if_null(w_current);

  if (!x_event_get_pointer_position(w_current, TRUE, &wx, &wy))
    return;

  o_redraw_cleanstates(w_current);	
  o_invalidate_rubber (w_current);

  i_update_middle_button(w_current, i_callback_add_pin_hotkey, _("Pin"));

  o_pin_start(w_current, wx, wy);

  w_current->inside_action = 1;
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
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

  /* only allow going into symbols */
  if (object == NULL || object->type != OBJ_COMPLEX)
    return;

  parent = w_current->toplevel->page_current;
  attrib = o_attrib_search_attached_attribs_by_name (object, "source", count);

  /* if above is null, then look inside symbol */
  if (attrib == NULL) {
    attrib =
      o_attrib_search_inherited_attribs_by_name (object, "source", count);
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

      s_log_message(_("Searching for source [%s]\n"), current_filename);
      saved_page_control = page_control;
      page_control =
        s_hierarchy_down_schematic_single(w_current->toplevel,
                                          current_filename,
                                          parent,
                                          page_control,
                                          HIERARCHY_NORMAL_LOAD);

      /* s_hierarchy_down_schematic_single() will not zoom the loaded page */
      if (page_control != -1) {
        a_zoom_extents(w_current,
                       s_page_objects (w_current->toplevel->page_current),
                       A_PAN_DONT_REDRAW);
        o_undo_savestate(w_current, UNDO_ALL);
      }

      /* save the first page */
      if ( !loaded_flag && page_control > 0 ) {
        save_first_page = w_current->toplevel->page_current;
      }

      /* now do some error fixing */
      if (page_control == -1) {
        s_log_message(_("Cannot find source [%s]\n"), current_filename);

        /* restore this for the next page */
        page_control = saved_page_control;
      } else {
        /* this only signifies that we tried */
        loaded_flag = TRUE;
      }

      g_free(current_filename);
      pcount++;
      current_filename = u_basic_breakup_string(attrib, ',', pcount);
    }

    g_free(attrib);
    g_free(current_filename);

    count++;

    /* continue looking outside first */
    if (!looking_inside) {
      attrib =
        o_attrib_search_attached_attribs_by_name (object, "source", count);
    }

    /* okay we were looking outside and didn't find anything,
     * so now we need to look inside the symbol */
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
      attrib =
        o_attrib_search_inherited_attribs_by_name (object, "source", count);
    }
  }

  if (loaded_flag) {

    if (save_first_page) {
      w_current->toplevel->page_current = save_first_page;
    }
    x_window_set_current_page( w_current, w_current->toplevel->page_current );
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  \bug may cause problems with non-directory symbols
 */
DEFINE_I_CALLBACK(hierarchy_down_symbol)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  OBJECT *object;
  const CLibSymbol *sym;

  exit_if_null(w_current);

  object = o_select_return_first_object(w_current);
  if (object != NULL) {
    /* only allow going into symbols */
    if (object->type == OBJ_COMPLEX) {
      s_log_message(_("Searching for symbol [%s]\n"), 
		    object->complex_basename);
      sym = s_clib_get_symbol_by_name (object->complex_basename);
      if (sym == NULL)
	return;
      if (s_clib_symbol_get_filename(sym) == NULL) {
	s_log_message(_("Symbol is not a real file."
			" Symbol cannot be loaded.\n"));
	return;
      }
      s_hierarchy_down_symbol(w_current->toplevel, sym,
			      w_current->toplevel->page_current);
      /* s_hierarchy_down_symbol() will not zoom the loaded page */
      a_zoom_extents(w_current,
                     s_page_objects (w_current->toplevel->page_current),
                     A_PAN_DONT_REDRAW);
      o_undo_savestate(w_current, UNDO_ALL);
      x_window_set_current_page(w_current, w_current->toplevel->page_current);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  PAGE *up_page;

  exit_if_null(w_current);

  up_page = s_hierarchy_find_up_page (w_current->toplevel->pages,
                                      w_current->toplevel->page_current);
  if (up_page == NULL) {
    s_log_message(_("Cannot find any schematics above the current one!\n"));
  } else {
    x_window_set_current_page(w_current, up_page);
  }
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  char *attrib_doc = NULL;
  char *attrib_device = NULL;
  char *attrib_value = NULL;
  OBJECT *object = NULL;
  const gchar *sourcename = NULL;
  const CLibSymbol *sym = NULL;

  exit_if_null(w_current);

  object = o_select_return_first_object(w_current);
  if (object != NULL) {
    /* only allow going into symbols */
    if (object->type == OBJ_COMPLEX) {

      /* look for "documentation" */
      attrib_doc = o_attrib_search_object_attribs_by_name (object, "documentation", 0);
      /* look for "device" */
      attrib_device = o_attrib_search_object_attribs_by_name (object, "device", 0);
      /* look for "value" */
      attrib_value = o_attrib_search_object_attribs_by_name (object, "value", 0);

      sym = s_clib_get_symbol_by_name (object->complex_basename);
      if (sym != NULL) {
        sourcename = s_clib_source_get_name (s_clib_symbol_get_source(sym));
      }

      initiate_gschemdoc(attrib_doc,
                         attrib_device,
                         attrib_value,
                         object->complex_basename,
                         sourcename);

      g_free(attrib_doc);
      g_free(attrib_device);
      g_free(attrib_value);
    }
  } else {
    generic_msg_dialog(_("This command retrieves the component documentation from the web, but there is no component selected"));

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  OBJECT *first_object;
  GList *s_current;

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
  s_current = geda_list_get_glist( w_current->toplevel->page_current->selection_list );
  if (!s_current) {
    return;
  }

  first_object = (OBJECT *) s_current->data; 
  if (!first_object) {
    return;	
  }

  /* skip over first object */
  s_current = g_list_next(s_current);
  while (s_current != NULL) {
    OBJECT *object = s_current->data;
    if (object != NULL) {
      o_attrib_attach (w_current->toplevel, object, first_object, TRUE);
      w_current->toplevel->page_current->CHANGED=1;
    }
    s_current = g_list_next(s_current);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  GList *s_current;
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

  s_current = geda_list_get_glist( w_current->toplevel->page_current->selection_list );
  while (s_current != NULL) {
    o_current = (OBJECT *) s_current->data;
    if (o_current) {
      if (o_current->attribs) {
        o_attrib_detach_all (w_current->toplevel, o_current);
        w_current->toplevel->page_current->CHANGED=1;
      }
    }
    s_current = g_list_next(s_current);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  TOPLEVEL *toplevel = w_current->toplevel;

  exit_if_null(w_current);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action) {
    return;
  }

  i_update_middle_button(w_current, i_callback_attributes_show_name,
                         _("ShowN"));

  if (o_select_selected (w_current)) {
    SELECTION *selection = toplevel->page_current->selection_list;
    GList *s_current;

    for (s_current = geda_list_get_glist (selection);
         s_current != NULL;
         s_current = g_list_next (s_current)) {
      OBJECT *object = (OBJECT*)s_current->data;
      o_attrib_toggle_show_name_value (w_current, object, SHOW_NAME);
    }

    o_undo_savestate (w_current, UNDO_ALL);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(attributes_show_value)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  TOPLEVEL *toplevel = w_current->toplevel;

  exit_if_null(w_current);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action) {
    return;
  }

  i_update_middle_button(w_current, i_callback_attributes_show_value,
                         _("ShowV"));

  if (o_select_selected (w_current)) {
    SELECTION *selection = toplevel->page_current->selection_list;
    GList *s_current;

    for (s_current = geda_list_get_glist (selection);
         s_current != NULL;
         s_current = g_list_next (s_current)) {
      OBJECT *object = (OBJECT*)s_current->data;
      o_attrib_toggle_show_name_value (w_current, object, SHOW_VALUE);
    }

    o_undo_savestate (w_current, UNDO_ALL);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(attributes_show_both)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  TOPLEVEL *toplevel = w_current->toplevel;

  exit_if_null(w_current);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action) {
    return;
  }

  i_update_middle_button(w_current, i_callback_attributes_show_both,
                         _("ShowB"));

  if (o_select_selected (w_current)) {
    SELECTION *selection = toplevel->page_current->selection_list;
    GList *s_current;

    for (s_current = geda_list_get_glist (selection);
         s_current != NULL;
         s_current = g_list_next (s_current)) {
      OBJECT *object = (OBJECT*)s_current->data;
      o_attrib_toggle_show_name_value (w_current, object, SHOW_NAME_VALUE);
    }

    o_undo_savestate (w_current, UNDO_ALL);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(attributes_visibility_toggle)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  TOPLEVEL *toplevel = w_current->toplevel;

  exit_if_null(w_current);

  /* This is a new addition 3/15 to prevent this from executing
   * inside an action */
  if (w_current->inside_action) {
    return;
  }

  i_update_middle_button(w_current,
                         i_callback_attributes_visibility_toggle,
                         _("VisToggle"));

  if (o_select_selected (w_current)) {
    SELECTION *selection = toplevel->page_current->selection_list;
    GList *s_current;

    for (s_current = geda_list_get_glist (selection);
         s_current != NULL;
         s_current = g_list_next (s_current)) {
      OBJECT *object = (OBJECT*)s_current->data;
      o_attrib_toggle_visibility (w_current, object);
    }

    o_undo_savestate (w_current, UNDO_ALL);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);
  snap_size_dialog(w_current);
}

/*! \brief Multiply by two the snap grid size.
 *  \par Function Description
 *  Callback function for the scale-up snap grid size hotkey.
 *  Multiply by two the snap grid size.
 */
DEFINE_I_CALLBACK(options_scale_up_snap_size)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  w_current->toplevel->snap_size *= 2;
  w_current->toplevel->page_current->CHANGED=1;  /* maybe remove those two lines */
  o_undo_savestate(w_current, UNDO_ALL);

  i_update_grid_info (w_current);
  o_invalidate_all (w_current);
}

/*! \brief Divide by two the snap grid size.
 *  \par Function Description
 *  Callback function for the scale-down snap grid size hotkey.
 *  Divide by two the snap grid size (if it's and even number).
 */
DEFINE_I_CALLBACK(options_scale_down_snap_size)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  if (w_current->toplevel->snap_size % 2 == 0)
    w_current->toplevel->snap_size /= 2;
  w_current->toplevel->page_current->CHANGED=1;  /* maybe remove those two lines */
  o_undo_savestate(w_current, UNDO_ALL);

  i_update_grid_info (w_current);
  o_invalidate_all (w_current);

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  if (w_current->actionfeedback_mode == BOUNDINGBOX) {
    w_current->actionfeedback_mode = OUTLINE;
    s_log_message(_("Action feedback mode set to OUTLINE\n"));
  } else {
    w_current->actionfeedback_mode = BOUNDINGBOX;
    s_log_message(_("Action feedback mode set to BOUNDINGBOX\n"));
  }
  if (w_current->inside_action &&
      w_current->toplevel->page_current->place_list != NULL)
    o_place_invalidate_rubber (w_current, FALSE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(options_grid)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);

  switch (w_current->grid) {
    case GRID_NONE: w_current->grid = GRID_DOTS; break;
    case GRID_DOTS: w_current->grid = GRID_MESH; break;
    case GRID_MESH: w_current->grid = GRID_NONE; break;
  }

  switch (w_current->grid) {
    case GRID_NONE: s_log_message (_("Grid OFF\n"));           break;
    case GRID_DOTS: s_log_message (_("Dot grid selected\n"));  break;
    case GRID_MESH: s_log_message (_("Mesh grid selected\n")); break;
  }

  i_update_grid_info (w_current);
  o_invalidate_all (w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(options_snap)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  /* toggle to the next snap state */
  w_current->toplevel->snap = (w_current->toplevel->snap+1) % SNAP_STATE_COUNT;

  switch (w_current->toplevel->snap) {
  case SNAP_OFF:
    s_log_message(_("Snap OFF (CAUTION!)\n"));
    break;
  case SNAP_GRID:
    s_log_message(_("Snap ON\n"));
    break;
  case SNAP_RESNAP:
    s_log_message(_("Snap back to the grid (CAUTION!)\n"));
    break;
  default:
    g_critical("options_snap: toplevel->snap out of range: %d\n",
               w_current->toplevel->snap);
  }

  i_show_state(w_current, NULL); /* update status on screen */
  i_update_grid_info (w_current);
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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  if (w_current->netconn_rubberband) {
    w_current->netconn_rubberband = 0;
    s_log_message(_("Rubber band OFF \n"));
  } else {
    w_current->netconn_rubberband = 1;
    s_log_message(_("Rubber band ON\n"));
  }
}


/*! \brief callback function for setting the magnetic net option
 *  \par Function Description
 *  This function just toggles a variable to switch the magnetic net
 *  mode ON and OFF
 */
DEFINE_I_CALLBACK(options_magneticnet)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  if ((w_current->magneticnet_mode = !w_current->magneticnet_mode)) {
    s_log_message(_("magnetic net mode: ON\n"));
  }
  else {
    s_log_message(_("magnetic net mode: OFF\n"));
  }
  i_show_state(w_current, NULL);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(options_show_log_window)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  TOPLEVEL *toplevel = w_current->toplevel;
  GValue value = { 0, };

  exit_if_null(w_current);

  if (w_current->event_state == ENDCOMP &&
      w_current->cswindow) {
    /* user hit escape key when placing components */

    /* Undraw any outline of the place list */
    o_place_invalidate_rubber (w_current, FALSE);
    w_current->rubber_visible = 0;

    /* De-select the lists in the component selector */
    x_compselect_deselect (w_current);

    /* Present the component selector again */
    g_value_init (&value, G_TYPE_BOOLEAN);
    g_value_set_boolean (&value, FALSE);
    g_object_set_property (G_OBJECT(w_current->cswindow), "hidden", &value);
  }

  if (w_current->inside_action) {
    /* If we're cancelling from a move action, re-wind the
     * page contents back to their state before we started */
    if (w_current->event_state == MOVE ||
        w_current->event_state == ENDMOVE)
      o_move_cancel (w_current);

    /* If we're cancelling from a grip action, call the specific cancel
     * routine to reset the visibility of the object being modified */
    if (w_current->event_state == GRIPS)
      o_grips_cancel (w_current);
  }

  /* Free the place list and its contents. If we were in a move
   * action, the list (refering to objects on the page) would
   * already have been cleared in o_move_cancel(), so this is OK. */
  s_delete_object_glist(toplevel, toplevel->page_current->place_list);
  toplevel->page_current->place_list = NULL;

  /* leave this on for now... but it might have to change */
  /* this is problematic since we don't know what the right mode */
  /* (when you cancel inside an action) should be */
  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);

  /* clear the key guile command-sequence */
  scm_c_eval_string ("(set! current-command-sequence '())");

  if (w_current->inside_action) { 
     o_invalidate_all (w_current);
  }

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);
  about_dialog(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
DEFINE_I_CALLBACK(help_hotkeys)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

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
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;

  exit_if_null(w_current);
  coord_dialog (w_current, 0, 0);
}

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
gboolean i_callback_close_wm ( GtkWidget *widget, GdkEvent *event, 
	                   gpointer data ) 
{

  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*) data;
  exit_if_null(w_current);

  x_window_close(w_current);

  /* stop further propagation of the delete_event signal for window: */
  /*   - if user has cancelled the close the window should obvioulsy */
  /*   not be destroyed */
  /*   - otherwise window has already been destroyed, nothing more to */
  /*   do */
  return TRUE;
}
