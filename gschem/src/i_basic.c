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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */
#include <config.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \brief Update status bar string 
 *
 *  \par Function Description
 *  This function actually updates the status bar 
 *  widget with the new string.
 *
 *  \param [in] w_current GSCHEM_TOPLEVEL structure
 *  \param [in] string The new string to be shown in the status bar
 */
static void i_update_status(GSCHEM_TOPLEVEL *w_current, const char *string)
{
  if (!w_current->status_label)
    return;

  if (string) {
    /* NOTE: consider optimizing this if same label */
    gtk_label_set(GTK_LABEL(w_current->status_label),
                  (char *) string);
  }
}

/*! \brief Get string corresponding to the currently selected mode
 *
 *  \par Function Description
 *  Returns a string describing the currently
 *  selected mode.
 *
 *  \param [in] w_current GSCHEM_TOPLEVEL structure
 *  \returns a string that will only last until the next time
 *   the function is called (which is probably just fine, really)
 *   *EK* Egil Kvaleberg
 */
static const char *i_status_string(GSCHEM_TOPLEVEL *w_current)
{
  static char *buf = 0;

  switch ( w_current->event_state ) {
    case NONE:
    case STARTROUTENET: /*! \todo */
    case ENDROUTENET: /*! \todo */
      return "";
    case STARTSELECT:
    case SELECT:
    case SBOX:
    case GRIPS:
      return _("Select Mode");
    case ENDCOMP:
      return _("Component Mode"); /*EK* new */
    case ENDTEXT:
      return _("Text Mode"); /*EK* new */
    case STARTCOPY:
    case ENDCOPY:
      return _("Copy Mode");
    case STARTMOVE:
    case ENDMOVE:
      return _("Move Mode");
    case ENDROTATEP:
      return _("Rotate Mode");
    case ENDMIRROR:
      return _("Mirror Mode");
    case ZOOM:
    case ZOOMBOXEND:
    case ZOOMBOXSTART:
      return _("Zoom Box");
    case STARTPAN:
    case PAN:
    case MOUSEPAN:
      return _("Pan Mode");
    case STARTPASTE:
    case ENDPASTE:
      g_free(buf);
      buf = g_strdup_printf(_("Paste %d Mode"), w_current->buffer_number+1);
      return buf;
    case STARTDRAWNET:
    case DRAWNET:
    case NETCONT:
      if (w_current->magneticnet_mode)
	return _("Magnetic Net Mode");
      else
	return _("Net Mode");
    case STARTDRAWBUS:
    case DRAWBUS:
    case BUSCONT:
      return _("Bus Mode");
    case DRAWLINE:
    case ENDLINE:
      return _("Line Mode");
    case DRAWBOX:
    case ENDBOX:
      return _("Box Mode");
    case DRAWPICTURE:
    case ENDPICTURE:
      return _("Picture Mode");
    case DRAWCIRCLE:
    case ENDCIRCLE:
      return _("Circle Mode");
    case DRAWARC:
    case ENDARC:
      return _("Arc Mode");
    case DRAWPIN:
    case ENDPIN:
      return _("Pin Mode");
    case COPY:
      return _("Copy");
    case MOVE:
      return _("Move");
    case MCOPY:
      return _("Multiple Copy");
    case STARTMCOPY:
    case ENDMCOPY:
      return _("Multiple Copy Mode");
  }
  g_assert_not_reached();
  return ""; /* should not happen */
}

/*! \brief Show state field
 *
 *  \par Function Description
 *  Show state field on screen, possibly with the 
 *  addition of an extra message
 *
 *  \param [in] w_current GSCHEM_TOPLEVEL structure
 *  \param [in] message The string to be displayed 
 */
void i_show_state(GSCHEM_TOPLEVEL *w_current, const char *message)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  gchar *what_to_say;
  const gchar *array[5] = { NULL };
  int i = 3; /* array[4] must be NULL */

  /* Fill in the string array */
  array[i--] = i_status_string(w_current);
  
  if(toplevel->show_hidden_text)
    array[i--] = _("Show Hidden");
  
  if(w_current->snap == SNAP_OFF)
    array[i--] = _("Snap Off");
  else if (w_current->snap == SNAP_RESNAP)
    array[i--] = _("Resnap Active");
  
  if(message && message[0])
    array[i] = message;
  
  /* Skip over NULLs */
  while(array[i] == NULL)
    i++;

  what_to_say = g_strjoinv(" - ", (gchar **) array + i);

  if(w_current->keyaccel_string) {
     gchar *p = what_to_say;

     what_to_say = g_strdup_printf("%s \t\t %s", w_current->keyaccel_string,
           what_to_say);
     g_free(p);
  }

  i_update_status(w_current, what_to_say);
  g_free(what_to_say);
}

/*! \brief Set new state, then show state field
 *
 *  \par Function Description
 *  Set new state, then show state field.
 *
 *  \param [in] w_current GSCHEM_TOPLEVEL structure
 *  \param [in] newstate The new state
 *   *EK* Egil Kvaleberg
 */
void i_set_state(GSCHEM_TOPLEVEL *w_current, enum x_states newstate)
{
  i_set_state_msg(w_current, newstate, NULL);
}

/*! \brief Set new state, then show state field including some
 *         message
 *
 *  \par Function Description
 *  Set new state, then show state field including some
 *  message.
 *
 *  \param [in] w_current GSCHEM_TOPLEVEL structure
 *  \param [in] newstate The new state
 *  \param [in] message Message to be shown
 *   *EK* Egil Kvaleberg
 */
void i_set_state_msg(GSCHEM_TOPLEVEL *w_current, enum x_states newstate,
		     const char *message)
{
  w_current->event_state = newstate;
  i_show_state(w_current, message);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void i_update_middle_button(GSCHEM_TOPLEVEL *w_current,
			    void (*func_ptr)(),
			    const char *string)
{
  char *temp_string;

  if (func_ptr == NULL)
    return;

  if (string == NULL)
    return;

  if (!w_current->middle_label)
    return;

  switch(w_current->middle_button) {

    /* remove this case eventually and make it a null case */
    case(ACTION):
    gtk_label_set(GTK_LABEL(w_current->middle_label),
                  _("Action"));
    break;

#ifdef HAVE_LIBSTROKE
    case(STROKE):
    gtk_label_set(GTK_LABEL(w_current->middle_label),
                  _("Stroke"));
    break;
#else 
    /* remove this case eventually and make it a null case */
    case(STROKE):
    gtk_label_set(GTK_LABEL(w_current->middle_label),
                  _("none"));
    break;
#endif
		
    case(REPEAT):
    temp_string = g_strconcat (_("Repeat/"), string, NULL);

    gtk_label_set(GTK_LABEL(w_current->middle_label),
                  temp_string);
    w_current->last_callback = func_ptr;
    g_free(temp_string);
    break;

  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \param [in] w_current GSCHEM_TOPLEVEL structure
 *
 */
void i_update_toolbar(GSCHEM_TOPLEVEL *w_current)
{
  if (!w_current->toolbars) 
	return;

  switch(w_current->event_state) {
    case(NONE):
    case(SELECT):
    case(STARTSELECT): 
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(
				   w_current->toolbar_select), TRUE);
      break;
      
    case(DRAWNET): 
    case(STARTDRAWNET): 
    case(NETCONT): 
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(
				   w_current->toolbar_net), TRUE);
      break;
      
    case(DRAWBUS): 
    case(STARTDRAWBUS): 
    case(BUSCONT): 
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(
				   w_current->toolbar_bus), TRUE);
      break;
      
    case(DRAWLINE): /*! \todo */
    case(DRAWBOX): /*! \todo */
    case(DRAWPICTURE): /*! \todo */
    case(DRAWPIN): /*! \todo */
    case(DRAWCIRCLE): /*! \todo */
    case(DRAWARC): /*! \todo */
    case(MOVE): /*! \todo */
    case(COPY): /*! \todo */
    case(ZOOM): /*! \todo */
    case(PAN): /*! \todo */
    case(STARTPAN): /*! \todo */
    case(STARTCOPY): /*! \todo */
    case(STARTMOVE): /*! \todo */
    case(ENDCOPY): /*! \todo */
    case(ENDMOVE): /*! \todo */
    case(ENDLINE): /*! \todo */
    case(ENDBOX): /*! \todo */
    case(ENDPICTURE): /*! \todo */
    case(ENDCIRCLE): /*! \todo */
    case(ENDARC): /*! \todo */
    case(ENDPIN): /*! \todo */
    case(ENDCOMP): /*! \todo */
    case(ENDTEXT): /*! \todo */
    case(ENDROTATEP): /*! \todo */
    case(ENDMIRROR): /*! \todo */
    case(ZOOMBOXSTART): /*! \todo */
    case(ZOOMBOXEND): /*! \todo */
    case(STARTROUTENET): /*! \todo */
    case(ENDROUTENET): /*! \todo */
    case(MOUSEPAN): /*! \todo */
    case(STARTPASTE): /*! \todo */
    case(ENDPASTE): /*! \todo */
    case(GRIPS): /*! \todo */
    case(MCOPY): /*! \todo */
    case(STARTMCOPY): /*! \todo */
    case(ENDMCOPY): /*! \todo */
    default:
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(
				   w_current->toolbar_select), TRUE);
      break;
  }
}


/*! \brief Update sensitivity of the Edit/Paste menu item
 *
 *  \par Function Description
 *  Asynchronous callback to update sensitivity of the Edit/Paste
 *  menu item.
 */
static void clipboard_usable_cb (int usable, void *userdata)
{
  GSCHEM_TOPLEVEL *w_current = userdata;
  x_menus_sensitivity (w_current, "_Edit/_Paste", usable);
}

static gboolean
selected_at_least_one_text_object(GSCHEM_TOPLEVEL *w_current)
{
  OBJECT *obj;
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *list = geda_list_get_glist(toplevel->page_current->selection_list);

  while(list != NULL) {
    obj = (OBJECT *) list->data;
    if (obj->type == OBJ_TEXT)
      return TRUE;
    list = g_list_next(list);
  }
  return FALSE;
}


/*! \brief Update sensitivity of relevant menu items
 *
 *  \par Function Description
 *  Update sensitivity of relevant menu items.
 *
 *  \param [in] w_current GSCHEM_TOPLEVEL structure
 */
void i_update_menus(GSCHEM_TOPLEVEL *w_current)
{
  gboolean have_text_selected;
  TOPLEVEL *toplevel = w_current->toplevel;
  /* 
   * This is very simplistic.  Right now it just disables all menu
   * items which get greyed out when a component is not selected.
   * Eventually what gets enabled/disabled
   * should be based on what is in the selection list 
   */

  g_assert(w_current != NULL);
  g_assert(toplevel->page_current != NULL);

  x_clipboard_query_usable (w_current, clipboard_usable_cb, w_current);

  if (o_select_selected (w_current)) {
    have_text_selected = selected_at_least_one_text_object(w_current);

    /* since one or more things are selected, we set these TRUE */
    /* These strings should NOT be internationalized */
    x_menus_sensitivity(w_current, "_Edit/Cu_t", TRUE);
    x_menus_sensitivity(w_current, "_Edit/_Copy", TRUE);
    x_menus_sensitivity(w_current, "_Edit/_Delete", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Copy Mode", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Multiple Copy Mode", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Move Mode", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Rotate 90 Mode", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Mirror Mode", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Edit...", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Edit Text...", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Slot...", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Color...", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Lock", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Unlock", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Line Width & Type...", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Fill Type...", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Embed Component/Picture", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Unembed Component/Picture", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Update Component", TRUE);
    x_menus_sensitivity(w_current, "_Buffer/Copy into 1", TRUE);
    x_menus_sensitivity(w_current, "_Buffer/Copy into 2", TRUE);
    x_menus_sensitivity(w_current, "_Buffer/Copy into 3", TRUE);
    x_menus_sensitivity(w_current, "_Buffer/Copy into 4", TRUE);
    x_menus_sensitivity(w_current, "_Buffer/Copy into 5", TRUE);
    x_menus_sensitivity(w_current, "_Buffer/Cut into 1", TRUE);
    x_menus_sensitivity(w_current, "_Buffer/Cut into 2", TRUE);
    x_menus_sensitivity(w_current, "_Buffer/Cut into 3", TRUE);
    x_menus_sensitivity(w_current, "_Buffer/Cut into 4", TRUE);
    x_menus_sensitivity(w_current, "_Buffer/Cut into 5", TRUE);
    x_menus_sensitivity(w_current, "Hie_rarchy/_Down Schematic", TRUE);
    x_menus_sensitivity(w_current, "Hie_rarchy/Down _Symbol", TRUE);
    x_menus_sensitivity(w_current, "Hie_rarchy/D_ocumentation...", TRUE);
    x_menus_sensitivity(w_current, "A_ttributes/_Attach", TRUE);
    x_menus_sensitivity(w_current, "A_ttributes/_Detach", TRUE);
    x_menus_sensitivity(w_current, "A_ttributes/Show _Value", have_text_selected);
    x_menus_sensitivity(w_current, "A_ttributes/Show _Name", have_text_selected);
    x_menus_sensitivity(w_current, "A_ttributes/Show _Both", have_text_selected);
    x_menus_sensitivity(w_current, "A_ttributes/_Toggle Visibility", have_text_selected);

    /*  Menu items for hierarchy added by SDB 1.9.2005.  */
    x_menus_popup_sensitivity(w_current, "/Down Schematic", TRUE);
    x_menus_popup_sensitivity(w_current, "/Down Symbol", TRUE);
    /* x_menus_popup_sensitivity(w_current, "/Up", TRUE); */

  } else {
    /* Nothing is selected, grey these out */
    /* These strings should NOT be internationalized */
    x_menus_sensitivity(w_current, "_Edit/Cu_t", FALSE);
    x_menus_sensitivity(w_current, "_Edit/_Copy", FALSE);
    x_menus_sensitivity(w_current, "_Edit/_Delete", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Copy Mode", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Multiple Copy Mode", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Move Mode", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Rotate 90 Mode", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Mirror Mode", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Edit...", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Edit Text...", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Slot...", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Color...", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Lock", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Unlock", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Line Width & Type...", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Fill Type...", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Embed Component/Picture", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Unembed Component/Picture", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Update Component", FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Copy into 1", FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Copy into 2", FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Copy into 3", FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Copy into 4", FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Copy into 5", FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Cut into 1", FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Cut into 2", FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Cut into 3", FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Cut into 4", FALSE);
    x_menus_sensitivity(w_current, "_Buffer/Cut into 5", FALSE);
    x_menus_sensitivity(w_current, "Hie_rarchy/_Down Schematic", FALSE);
    x_menus_sensitivity(w_current, "Hie_rarchy/Down _Symbol", FALSE);
    x_menus_sensitivity(w_current, "Hie_rarchy/D_ocumentation...", FALSE);
    x_menus_sensitivity(w_current, "A_ttributes/_Attach", FALSE);
    x_menus_sensitivity(w_current, "A_ttributes/_Detach", FALSE);
    x_menus_sensitivity(w_current, "A_ttributes/Show _Value", FALSE);
    x_menus_sensitivity(w_current, "A_ttributes/Show _Name", FALSE);
    x_menus_sensitivity(w_current, "A_ttributes/Show _Both", FALSE);
    x_menus_sensitivity(w_current, "A_ttributes/_Toggle Visibility", FALSE);

    /*  Menu items for hierarchy added by SDB 1.9.2005.  */
    x_menus_popup_sensitivity(w_current, "/Down Schematic", FALSE);
    x_menus_popup_sensitivity(w_current, "/Down Symbol", FALSE);
    /* x_menus_popup_sensitivity(w_current, "/Up", FALSE);	*/
  }

  x_menus_sensitivity(w_current, "_Buffer/Paste from 1", (object_buffer[0] != NULL));
  x_menus_sensitivity(w_current, "_Buffer/Paste from 2", (object_buffer[1] != NULL));
  x_menus_sensitivity(w_current, "_Buffer/Paste from 3", (object_buffer[2] != NULL));
  x_menus_sensitivity(w_current, "_Buffer/Paste from 4", (object_buffer[3] != NULL));
  x_menus_sensitivity(w_current, "_Buffer/Paste from 5", (object_buffer[4] != NULL));

}

/*! \brief Set filename as gschem window title
 *  
 *  \par Function Description
 *  Set filename as gschem window title using
 *  the gnome HID format style.
 *
 *  \param [in] w_current GSCHEM_TOPLEVEL structure
 *  \param [in] string The filename
 */
void i_set_filename(GSCHEM_TOPLEVEL *w_current, const gchar *string)
{
  gchar *print_string=NULL;
  gchar *filename=NULL;

  if (!w_current->main_window)
    return;
  if (string == NULL)
    return;

  filename = g_path_get_basename(string);
  
  print_string = g_strdup_printf("%s - gschem", filename);
  
  gtk_window_set_title(GTK_WINDOW(w_current->main_window),
		       print_string);
  
  g_free(print_string);
  g_free(filename);
}

/*! \brief Write the grid settings to the gschem status bar
 *
 *  \par Function Description
 *  Write the grid settings to the gschem status bar.
 *  The function takes the current grid paramters of gschem
 *  and prints it to the status bar.
 *  The format is "Grid([SnapGridSize],[CurrentGridSize])"
 *
 *  \param [in] w_current GSCHEM_TOPLEVEL structure
 */
void i_update_grid_info (GSCHEM_TOPLEVEL *w_current)
{
  gchar *print_string=NULL;
  gchar *snap=NULL;
  gchar *grid=NULL;

  if (!w_current->grid_label)
    return;

  switch (w_current->snap) {
  case SNAP_OFF:
    snap = g_strdup(_("OFF"));
    break;
  case SNAP_GRID:
    snap = g_strdup_printf("%d", w_current->snap_size);
    break;
  case SNAP_RESNAP:
    snap = g_strdup_printf("%dR", w_current->snap_size);
    break;
  default:
    g_critical("i_set_grid: w_current->snap out of range: %d\n",
               w_current->snap);
  }

  if (w_current->grid == GRID_NONE) {
    grid = g_strdup(_("OFF"));
  } else {
    int visible_grid = x_grid_query_drawn_spacing (w_current);
    if (visible_grid == -1)
      grid = g_strdup (_("NONE"));
    else
      grid = g_strdup_printf("%d", visible_grid);
  }

  print_string = g_strdup_printf(_("Grid(%s, %s)"), snap, grid);
  gtk_label_set(GTK_LABEL(w_current->grid_label), print_string);
  
  g_free(print_string);
  g_free(grid);
  g_free(snap);
}
