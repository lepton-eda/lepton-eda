/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2019 Lepton EDA Contributors
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
    case SELECT     : return _("Select Mode");
    case SBOX       : return _("Select Box Mode");
    case TEXTMODE   : return _("Text Mode");
    case PAN        : return _("Pan Mode");
    case PASTEMODE:
      g_free(buf);
      buf = g_strdup_printf(_("Paste %d Mode"), w_current->buffer_number+1);
      return buf;
    case NETMODE:
      if (gschem_options_get_magnetic_net_mode (w_current->options))
        return _("Magnetic Net Mode");
      else
        return _("Net Mode");
    case ARCMODE    : return _("Arc Mode");
    case BOXMODE    : return _("Box Mode");
    case BUSMODE    : return _("Bus Mode");
    case CIRCLEMODE : return _("Circle Mode");
    case COMPMODE   : return _("Component Mode");
    case COPYMODE   : return _("Copy Mode");
    case MCOPYMODE  : return _("Multiple Copy Mode");
    case LINEMODE   : return _("Line Mode");
    case MIRRORMODE : return _("Mirror Mode");
    case MOVEMODE   : return _("Move Mode");
    case PATHMODE   : return _("Path Mode");
    case PICTUREMODE: return _("Picture Mode");
    case PINMODE    : return _("Pin Mode");
    case ROTATEMODE : return _("Rotate Mode");
    case GRIPS      : return _("Modify Mode");
    case ZOOMBOX    : return _("Zoom Box");
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


/*! \brief Mark start of an editing action
 *
 *  \par Function Description
 *  Calls i_action_update_status() informing it that the new
 *  editing action is started.
 *
 *  \param [in] w_current GschemToplevel structure
 */
void i_action_start (GschemToplevel *w_current)
{
  i_action_update_status (w_current, TRUE);
}


/*! \brief Mark end of an editing action
 *
 *  \par Function Description
 *  Calls i_action_update_status() informing it that the current
 *  editing action is finished.
 *
 *  \param [in] w_current GschemToplevel structure
 */
void i_action_stop (GschemToplevel *w_current)
{
  i_action_update_status (w_current, FALSE);
}


/*! \brief Update status of an editing action
 *
 *  \par Function Description
 *  Checks if the current action state has been changed (an action
 *  was started or finished) and informs the bottom widget to make
 *  it update the status text color accordingly
 *
 *  \param [in] w_current GschemToplevel structure
 */
void i_action_update_status (GschemToplevel *w_current, gboolean inside_action)
{
  if (w_current->inside_action != inside_action) {
    w_current->inside_action = inside_action;
    gschem_bottom_widget_set_status_text_color (GSCHEM_BOTTOM_WIDGET (w_current->bottom_widget),
                                                inside_action);
  }
}


/*! \brief Set new state, then show state field
 *
 *  \par Function Description
 *  Set new state and show it in the state field. Then run change
 *  action mode hook to notify Scheme code about the change.
 *
 *  \param [in] w_current GschemToplevel structure
 *  \param [in] newstate The new state
 *   *EK* Egil Kvaleberg
 */
void i_set_state(GschemToplevel *w_current, enum x_states newstate)
{
  i_set_state_msg(w_current, newstate, NULL);

  const gchar* mode = "select-mode";
    switch (newstate) {
      case SELECT: mode="select-mode"; break;
      case GRIPS: mode="grips-mode"; break;
      case ARCMODE: mode="arc-mode"; break;
      case BOXMODE: mode="box-mode"; break;
      case BUSMODE: mode="bus-mode"; break;
      case CIRCLEMODE: mode="circle-mode"; break;
      case LINEMODE: mode="line-mode"; break;
      case NETMODE: mode="net-mode"; break;
      case PATHMODE: mode="path-mode"; break;
      case PICTUREMODE: mode="picture-mode"; break;
      case PINMODE: mode="pin-mode"; break;
      case COMPMODE: mode="component-mode"; break;
      case COPYMODE: mode="copy-mode"; break;
      case MCOPYMODE: mode="multiple-copy-mode"; break;
      case MOVEMODE: mode="move-mode"; break;
      case PASTEMODE: mode="paste-mode"; break;
      case TEXTMODE: mode="text-mode"; break;
      case SBOX: mode="box-select-mode"; break;
      case ZOOMBOX: mode="zoom-box-mode"; break;
      case PAN: mode="pan-mode"; break;
      case MIRRORMODE: mode="mirror-mode"; break;
      case ROTATEMODE: mode="rotate-mode"; break;
    }

  g_run_hook_action_mode (w_current, "%switch-action-mode-hook", mode);
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
 *  \param [in] w_current GschemToplevel structure
 *
 */
void i_update_toolbar(GschemToplevel *w_current)
{
  if (!w_current->toolbars)
	return;

  switch(w_current->event_state) {
    case(SELECT):
      gtk_toggle_tool_button_set_active (GTK_TOGGLE_TOOL_BUTTON (w_current->toolbar_select),
                                         TRUE);
      break;

    case(NETMODE):
      gtk_toggle_tool_button_set_active (GTK_TOGGLE_TOOL_BUTTON (w_current->toolbar_net),
                                         TRUE);
      break;

    case(BUSMODE):
      gtk_toggle_tool_button_set_active (GTK_TOGGLE_TOOL_BUTTON (w_current->toolbar_bus),
                                         TRUE);
      break;

    case(ARCMODE): /*! \todo */
    case(BOXMODE): /*! \todo */
    case(CIRCLEMODE): /*! \todo */
    case(LINEMODE): /*! \todo */
    case(PICTUREMODE): /*! \todo */
    case(PINMODE): /*! \todo */
    case(PAN): /*! \todo */
    case(COPYMODE): /*! \todo */
    case(MCOPYMODE): /*! \todo */
    case(MOVEMODE): /*! \todo */
    case(COMPMODE): /*! \todo */
    case(ROTATEMODE): /*! \todo */
    case(TEXTMODE): /*! \todo */
    case(MIRRORMODE): /*! \todo */
    case(ZOOMBOX): /*! \todo */
    case(PASTEMODE): /*! \todo */
    case(GRIPS): /*! \todo */
    default:
      gtk_toggle_tool_button_set_active (GTK_TOGGLE_TOOL_BUTTON (w_current->toolbar_select),
                                         TRUE);
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
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (userdata);
  x_menus_sensitivity (w_current, "_Edit/_Paste", usable);
}



/*! \brief Return TRUE if at least 1 text object is selected
 */
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



/*! \brief Return TRUE if at least 1 component object is selected
 */
static gboolean
selected_at_least_one_comp_object(GschemToplevel *w_current)
{
  gboolean result = FALSE;
  TOPLEVEL* toplevel = gschem_toplevel_get_toplevel (w_current);
  GList* selected = geda_list_get_glist (toplevel->page_current->selection_list);

  for ( ; selected != NULL; selected = g_list_next (selected) )
  {
    OBJECT* obj = (OBJECT*) selected->data;
    if (obj->complex != NULL)
    {
      result = TRUE;
      break;
    }
  }

  return result;
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
  gboolean have_text_selected = FALSE;
  gboolean have_comp_selected = FALSE;
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
    have_comp_selected = selected_at_least_one_comp_object(w_current);

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
    x_menus_sensitivity(w_current, "_Help/Find Component D_ocumentation",
                        have_comp_selected);
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
    x_menus_sensitivity(w_current, "_Help/Find Component D_ocumentation", FALSE);
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

/*! \brief Set the main window's title
 *
 *  \par Function Description
 *  Set the main window's title using \a filename.
 *  Prepend an asterisk ("*") to indicate that the page
 *  is modified if \a changed is TRUE.
 *  Depending on [schematic.gui]::title-show-path configuration
 *  value, either full path or basename of \a filename is shown
 *  in the title.
 *
 *  \param [in] w_current GschemToplevel structure
 *  \param [in] filename  The filename
 *  \param [in] changed   Page changed status
 */
void i_set_filename (GschemToplevel* w_current,
                     const gchar* filename,
                     gboolean changed)
{
  g_return_if_fail (w_current != NULL);
  g_return_if_fail (w_current->main_window != NULL);
  g_return_if_fail (filename);

  gchar* cwd = g_get_current_dir();
  EdaConfig* cfg = eda_config_get_context_for_path (cwd);
  g_free (cwd);

  gboolean show_fullpath = FALSE;

  if (cfg != NULL)
  {
    GError*  err = NULL;
    gboolean val = eda_config_get_boolean (cfg,
                                           "schematic.gui",
                                           "title-show-path",
                                           &err);
    if (err == NULL)
    {
      show_fullpath = val;
    }

    g_clear_error (&err);
  }


  gchar* fname = show_fullpath
                 ? g_strdup (filename)
                 : g_path_get_basename (filename);

  gchar* title = g_strdup_printf ("%s%s - lepton-schematic",
                                  changed ? "* " : "",
                                  fname);

  gtk_window_set_title (GTK_WINDOW (w_current->main_window),
                        title);

  g_free (title);
  g_free (fname);
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



/*! \brief Update the status bar: rubber band, magnetic net modes
 *
 *  \param [in] w_current GschemToplevel structure
 */
void
i_update_net_options_status (GschemToplevel* w_current)
{
  gschem_bottom_widget_set_rubber_band_mode(
    GSCHEM_BOTTOM_WIDGET (w_current->bottom_widget),
    gschem_options_get_net_rubber_band_mode (w_current->options));

  gschem_bottom_widget_set_magnetic_net_mode(
    GSCHEM_BOTTOM_WIDGET (w_current->bottom_widget),
    gschem_options_get_magnetic_net_mode (w_current->options));
}

