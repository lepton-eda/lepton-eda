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

/*! \brief Update status bar string
 *
 *  \par Function Description
 *  This function actually updates the status bar
 *  widget with the new string.
 *
 *  \param [in] w_current GschemToplevel structure
 *  \param [in] string The new string to be shown in the status bar
 */
static void i_update_status(GschemToplevel *w_current, const char *string)
{
  if (!w_current->bottom_widget) {
    return;
  }

  if (string) {
    /* NOTE: consider optimizing this if same label */
    gschem_bottom_widget_set_status_text (GSCHEM_BOTTOM_WIDGET (w_current->bottom_widget), string);
  }
}

/*! \brief Get string corresponding to the currently selected mode
 *
 *  \par Function Description
 *  Returns a string describing the currently
 *  selected mode.
 *
 *  \param [in] w_current GschemToplevel structure
 *  \returns a string that will only last until the next time
 *   the function is called (which is probably just fine, really)
 *   *EK* Egil Kvaleberg
 */
static const char *i_status_string(GschemToplevel *w_current)
{
  static char *buf = 0;

  switch ( w_current->event_state ) {
    case NONE:
      return "";
    case STARTSELECT:
    case SELECT:
    case SBOX:
    case GRIPS:
      return _("Select Mode");
    case DRAWCOMP:
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
    case PAN:
      return _("Pan Mode");
    case STARTPASTE:
    case ENDPASTE:
      g_free(buf);
      buf = g_strdup_printf(_("Paste %d Mode"), w_current->buffer_number+1);
      return buf;
    case STARTDRAWNET:
    case DRAWNET:
    case NETCONT:
      if (gschem_options_get_magnetic_net_mode (w_current->options))
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
    case DRAWPATH:
    case PATHCONT:
    case ENDPATH:
      return _("Path Mode");
    case ARCMODE    : return _("Arc Mode");
    case DRAWBOX:
    case ENDBOX:
      return _("Box Mode");
    case DRAWPICTURE:
    case ENDPICTURE:
      return _("Picture Mode");
    case DRAWCIRCLE:
    case ENDCIRCLE:
      return _("Circle Mode");
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
 *  \param [in] w_current GschemToplevel structure
 *  \param [in] message The string to be displayed
 */
void i_show_state(GschemToplevel *w_current, const char *message)
{
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
  gchar *what_to_say;
  const gchar *array[5] = { NULL };
  int i = 3; /* array[4] must be NULL */
  SNAP_STATE snap_mode;

  /* Fill in the string array */
  array[i--] = i_status_string(w_current);

  snap_mode = gschem_options_get_snap_mode (w_current->options);

  if(toplevel->show_hidden_text)
    array[i--] = _("Show Hidden");

  if(snap_mode == SNAP_OFF)
    array[i--] = _("Snap Off");
  else if (snap_mode == SNAP_RESNAP)
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
 *  \param [in] w_current GschemToplevel structure
 *  \param [in] newstate The new state
 *   *EK* Egil Kvaleberg
 */
void i_set_state(GschemToplevel *w_current, enum x_states newstate)
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
 *  \param [in] w_current GschemToplevel structure
 *  \param [in] newstate The new state
 *  \param [in] message Message to be shown
 *   *EK* Egil Kvaleberg
 */
void i_set_state_msg(GschemToplevel *w_current, enum x_states newstate,
		     const char *message)
{
  if ((newstate != w_current->event_state) || (message != NULL)) {
    w_current->event_state = newstate;
    i_update_toolbar (w_current);
  }
  i_show_state(w_current, message);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void i_update_middle_button (GschemToplevel *w_current,
                             void (*func_ptr)(),
                             const char *string)
{
  g_return_if_fail (w_current != NULL);
  g_return_if_fail (w_current->bottom_widget != NULL);

  switch(w_current->middle_button) {

    /* remove this case eventually and make it a null case */
    case(ACTION):
      gschem_bottom_widget_set_middle_button_text (
          GSCHEM_BOTTOM_WIDGET (w_current->bottom_widget),
          _("Action"));
      w_current->last_callback = NULL;
      break;

#ifdef HAVE_LIBSTROKE
    case(STROKE):
      gschem_bottom_widget_set_middle_button_text (
          GSCHEM_BOTTOM_WIDGET (w_current->bottom_widget),
          _("Stroke"));
      w_current->last_callback = NULL;
    break;
#else
    /* remove this case eventually and make it a null case */
    case(STROKE):
      gschem_bottom_widget_set_middle_button_text (
          GSCHEM_BOTTOM_WIDGET (w_current->bottom_widget),
          _("none"));
      w_current->last_callback = NULL;
      break;
#endif

    case(REPEAT):
      if ((string != NULL) && (func_ptr != NULL))
      {
        char *temp_string = g_strconcat (_("Repeat/"), string, NULL);

        gschem_bottom_widget_set_middle_button_text (
            GSCHEM_BOTTOM_WIDGET (w_current->bottom_widget),
            temp_string);

        g_free(temp_string);
        w_current->last_callback = func_ptr;
      } else {
        gschem_bottom_widget_set_middle_button_text (
            GSCHEM_BOTTOM_WIDGET (w_current->bottom_widget),
            _("Repeat/none"));
        w_current->last_callback = NULL;
      }
      break;

    case(MID_MOUSEPAN_ENABLED):
      gschem_bottom_widget_set_middle_button_text (
          GSCHEM_BOTTOM_WIDGET (w_current->bottom_widget),
          _("Pan"));
      w_current->last_callback = NULL;
      break;

    default:
      gschem_bottom_widget_set_middle_button_text (
          GSCHEM_BOTTOM_WIDGET (w_current->bottom_widget),
          _("none"));
      w_current->last_callback = NULL;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \param [in] w_current GschemToplevel structure
 *
 */
void i_update_toolbar(GschemToplevel *w_current)
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
    case(ARCMODE): /*! \todo */
    case(DRAWBOX): /*! \todo */
    case(DRAWPICTURE): /*! \todo */
    case(DRAWPIN): /*! \todo */
    case(DRAWCIRCLE): /*! \todo */
    case(MOVE): /*! \todo */
    case(COPY): /*! \todo */
    case(ZOOM): /*! \todo */
    case(PAN): /*! \todo */
    case(STARTCOPY): /*! \todo */
    case(STARTMOVE): /*! \todo */
    case(ENDCOPY): /*! \todo */
    case(ENDMOVE): /*! \todo */
    case(ENDLINE): /*! \todo */
    case(ENDBOX): /*! \todo */
    case(ENDPICTURE): /*! \todo */
    case(ENDCIRCLE): /*! \todo */
    case(ENDPIN): /*! \todo */
    case(ENDCOMP): /*! \todo */
    case(ENDTEXT): /*! \todo */
    case(ENDROTATEP): /*! \todo */
    case(ENDMIRROR): /*! \todo */
    case(ZOOMBOXSTART): /*! \todo */
    case(ZOOMBOXEND): /*! \todo */
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
  GschemToplevel *w_current = userdata;
  x_menus_sensitivity (w_current, "_Edit/_Paste", usable);
}

static gboolean
selected_at_least_one_text_object(GschemToplevel *w_current)
{
  OBJECT *obj;
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
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
 *  \param [in] w_current GschemToplevel structure
 */
void i_update_menus(GschemToplevel *w_current)
{
  gboolean have_text_selected;
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
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
    x_menus_sensitivity(w_current, "_Edit/Slot...", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Lock", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Unlock", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Embed Component/Picture", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Unembed Component/Picture", TRUE);
    x_menus_sensitivity(w_current, "_Edit/Update Component", TRUE);
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
    x_menus_popup_sensitivity(w_current, "Down Schematic", TRUE);
    x_menus_popup_sensitivity(w_current, "Down Symbol", TRUE);
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
    x_menus_sensitivity(w_current, "_Edit/Slot...", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Lock", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Unlock", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Embed Component/Picture", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Unembed Component/Picture", FALSE);
    x_menus_sensitivity(w_current, "_Edit/Update Component", FALSE);
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
    x_menus_popup_sensitivity(w_current, "Down Schematic", FALSE);
    x_menus_popup_sensitivity(w_current, "Down Symbol", FALSE);
    /* x_menus_popup_sensitivity(w_current, "/Up", FALSE);	*/
  }


}

/*! \brief Set filename as gschem window title
 *
 *  \par Function Description
 *  Set filename as gschem window title using
 *  the gnome HID format style.
 *
 *  \param [in] w_current GschemToplevel structure
 *  \param [in] string The filename
 */
void i_set_filename(GschemToplevel *w_current, const gchar *string)
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
 *  \param [in] w_current GschemToplevel structure
 */
void
i_update_grid_info (GschemToplevel *w_current)
{
  g_return_if_fail (w_current != NULL);

  if (w_current->bottom_widget != NULL) {
    g_object_set (GSCHEM_BOTTOM_WIDGET (w_current->bottom_widget),
        "snap-mode", gschem_options_get_snap_mode (w_current->options),
        "snap-size", gschem_options_get_snap_size (w_current->options),
        "grid-mode", gschem_options_get_grid_mode (w_current->options),
        "grid-size", x_grid_query_drawn_spacing (w_current),
        NULL);
  }
}



/*! \brief Write the grid settings to the gschem status bar
 *
 *  \param [in] view The page view originating the signal
 *  \param [in] w_current GschemToplevel structure
 */
void
i_update_grid_info_callback (GschemPageView *view, GschemToplevel *w_current)
{
  i_update_grid_info (w_current);
}
