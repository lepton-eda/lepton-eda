/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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

#include "schematic.h"

/*! \brief Update status bar string
 *
 *  \par Function Description
 *  This function actually updates the status bar
 *  widget with the new string.
 *
 *  \param [in] w_current SchematicWindow structure
 *  \param [in] string The new string to be shown in the status bar
 */
static void
i_update_status (SchematicWindow *w_current,
                 const char *string)
{
  GtkWidget *bottom_widget =
    schematic_window_get_bottom_widget (w_current);

  if (bottom_widget == NULL)
  {
    return;
  }

  if (string) {
    /* NOTE: consider optimizing this if same label */
    schematic_bottom_widget_set_status_text (SCHEMATIC_BOTTOM_WIDGET (bottom_widget),
                                             string);
  }
}

/*! \brief Get string corresponding to the currently selected mode
 *
 *  \par Function Description
 *  Returns a string describing the currently
 *  selected mode.
 *
 *  \param [in] w_current SchematicWindow structure
 *  \returns a string that will only last until the next time
 *   the function is called (which is probably just fine, really)
 *   *EK* Egil Kvaleberg
 */
static const char*
i_status_string (SchematicWindow *w_current)
{
  static char *buf = 0;

  switch (schematic_window_get_action_mode (w_current))
  {
    case SELECT     : return _("Select Mode");
    case SBOX       : return _("Select Box Mode");
    case TEXTMODE   : return _("Text Mode");
    case PAN        : return _("Pan Mode");
    case PASTEMODE:
      g_free(buf);
      buf = g_strdup_printf(_("Paste %d Mode"), w_current->buffer_number+1);
      return buf;
    case NETMODE:
      if (schematic_options_get_magnetic_net_mode (w_current->options))
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
 *  \param [in] w_current SchematicWindow structure
 *  \param [in] message The string to be displayed
 */
void
i_show_state (SchematicWindow *w_current,
              const char *message)
{
  gchar *what_to_say;
  const gchar *array[5] = { NULL };
  int i = 3; /* array[4] must be NULL */
  SchematicSnapMode snap_mode;

  gboolean show_hidden_text =
    schematic_window_get_show_hidden_text (w_current);

  /* Fill in the string array */
  array[i--] = i_status_string(w_current);

  snap_mode = schematic_options_get_snap_mode (w_current->options);

  if (show_hidden_text)
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


  if (schematic_window_get_keyaccel_string (w_current))
  {
     gchar *p = what_to_say;

     what_to_say =
       g_strdup_printf("%s \t\t %s",
                       schematic_window_get_keyaccel_string (w_current),
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
 *  \param [in] w_current SchematicWindow structure
 */
void
i_action_start (SchematicWindow *w_current)
{
  i_action_update_status (w_current, TRUE);
}


/*! \brief Mark end of an editing action
 *
 *  \par Function Description
 *  Calls i_action_update_status() informing it that the current
 *  editing action is finished.
 *
 *  \param [in] w_current SchematicWindow structure
 */
void
i_action_stop (SchematicWindow *w_current)
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
 *  \param [in] w_current The \c SchematicWindow structure.
 *  \param [in] inside_action The new action state.
 */
void
i_action_update_status (SchematicWindow *w_current,
                        gboolean inside_action)
{
  GtkWidget *bottom_widget =
    schematic_window_get_bottom_widget (w_current);

  if (schematic_window_get_inside_action (w_current) != inside_action)
  {
    schematic_window_set_inside_action (w_current, inside_action);
    schematic_bottom_widget_set_status_text_color (SCHEMATIC_BOTTOM_WIDGET (bottom_widget),
                                                   inside_action);
  }
}



/*! \brief Update sensitivity of menu items according to current event state.
 *
 *  \param [in] w_current  SchematicWindow structure
 *  \param [in] newstate   The new state
 */
static void
update_state_menu_items (SchematicWindow* w_current,
                         SchematicActionMode newstate)
{
  GtkWidget *menubar = schematic_window_get_menubar (w_current);
  x_menus_sensitivity (menubar, "&edit-select", newstate != SELECT);
}



/*! \brief Set new state, then show state field
 *
 *  \par Function Description
 *  Set new state and show it in the state field. Then run change
 *  action mode hook to notify Scheme code about the change.
 *
 *  \param [in] w_current SchematicWindow structure
 *  \param [in] newstate The new state
 *   *EK* Egil Kvaleberg
 */
void
i_set_state (SchematicWindow *w_current,
             SchematicActionMode newstate)
{
  i_set_state_msg(w_current, newstate, NULL);

  update_state_menu_items (w_current, newstate);

  const gchar* mode = schematic_action_mode_to_string (newstate);

  g_run_hook_action_mode (w_current, "switch-action-mode-hook", mode);
}

/*! \brief Set new state, then show state field including some
 *         message
 *
 *  \par Function Description
 *  Set new state, then show state field including some
 *  message.
 *
 *  \param [in] w_current SchematicWindow structure
 *  \param [in] newstate The new state
 *  \param [in] message Message to be shown
 *   *EK* Egil Kvaleberg
 */
void
i_set_state_msg (SchematicWindow *w_current,
                 SchematicActionMode newstate,
                 const char *message)
{
  if ((newstate != schematic_window_get_action_mode (w_current)) || (message != NULL))
  {
    schematic_window_set_action_mode (w_current, newstate);
    if (w_current->toolbars)
    {
      GtkWidget *toolbar = schematic_window_get_toolbar (w_current);
      schematic_toolbar_update (toolbar, newstate);
    }
  }
  i_show_state(w_current, message);
}


/*! \brief Update sensitivity of the Edit/Paste menu item
 *
 *  \par Function Description
 *  Asynchronous callback to update sensitivity of the Edit/Paste
 *  menu item.
 */
static void clipboard_usable_cb (int usable, void *userdata)
{
  SchematicWindow *w_current = SCHEMATIC_WINDOW (userdata);
  GtkWidget *menubar = schematic_window_get_menubar (w_current);
  x_menus_sensitivity (menubar, "&clipboard-paste", usable);
  GtkWidget *popup_menu =
    schematic_window_get_popup_menu (w_current);
  x_menus_sensitivity (popup_menu, "&clipboard-paste", usable);
}



/*! \brief Return the first object of type \a type if it is selected.
 *
 *  \param page  The page to search for selected object.
 *  \param type      object type constant (OBJ_TEXT, OBJ_COMPONENT, etc.) (o_types.h)
 */
static LeptonObject*
obj_selected (LeptonPage *page,
              int type)
{
  LeptonObject* result = FALSE;
  LeptonSelection* selection = page->selection_list;

  GList* gl = lepton_list_get_glist (selection);
  for ( ; gl != NULL; gl = g_list_next (gl) )
  {
    LeptonObject* obj = (LeptonObject*) gl->data;
    if (lepton_object_get_type (obj) == type)
    {
#ifdef DEBUG
      printf (" >> obj_selected(): obj->type: [%c]\n", lepton_object_get_type (obj));
#endif
      result = obj;
      break;
    }
  }

  return result;
}



/*! \brief Return TRUE if a component with "source" attribute is selected.
 *
 *  \param [in] page The page to search for selected components.
 */
static gboolean
parent_comp_selected (LeptonPage* page)
{
  gboolean result = FALSE;
  LeptonObject* obj = obj_selected (page, OBJ_COMPONENT);

  if (obj != NULL)
  {
    char* attr =
      lepton_attrib_search_attached_attribs_by_name (obj, "source", 0);
    if (attr == NULL)
    {
      attr = lepton_attrib_search_inherited_attribs_by_name (obj, "source", 0);
    }

    result = attr != NULL;
    g_free (attr);
  }

  return result;
}



/*! \brief Update menu items sensitivity for the main and popup menus.
 *
 *  \param [in] w_current SchematicWindow structure
 */
void
i_update_menus (SchematicWindow* w_current)
{
  g_return_if_fail (w_current != NULL);

  LeptonPage* page = schematic_window_get_active_page (w_current);
  g_return_if_fail (page != NULL);

  /* update Edit->Paste sensitivity in clipboard_usable_cb():
  */
  x_clipboard_query_usable (w_current, clipboard_usable_cb, w_current);

  update_state_menu_items (w_current, schematic_window_get_action_mode (w_current));

  gboolean selected      = o_select_selected (w_current);
  gboolean text_selected = selected && obj_selected (page, OBJ_TEXT);
  gboolean comp_selected = selected && obj_selected (page, OBJ_COMPONENT);
  gboolean pic_selected  = selected && obj_selected (page, OBJ_PICTURE);
  gboolean embeddable    = comp_selected || pic_selected;
  gboolean has_parent    = s_hierarchy_find_up_page (page) != NULL;
  gboolean parent        = comp_selected && parent_comp_selected (page);

  GtkWidget* mmenu = schematic_window_get_menubar (w_current);

  x_menus_sensitivity (mmenu, "&clipboard-cut", selected);
  x_menus_sensitivity (mmenu, "&clipboard-copy", selected);
  x_menus_sensitivity (mmenu, "&edit-delete", selected);
  x_menus_sensitivity (mmenu, "&edit-copy", selected);
  x_menus_sensitivity (mmenu, "&edit-mcopy", selected);
  x_menus_sensitivity (mmenu, "&edit-move", selected);
  x_menus_sensitivity (mmenu, "&edit-rotate-90", selected);
  x_menus_sensitivity (mmenu, "&edit-mirror", selected);
  x_menus_sensitivity (mmenu, "&edit-edit", selected);
  x_menus_sensitivity (mmenu, "&edit-text", text_selected);
  x_menus_sensitivity (mmenu, "&edit-object-properties", selected);
  x_menus_sensitivity (mmenu, "&edit-slot", comp_selected);
  x_menus_sensitivity (mmenu, "&edit-lock", selected);
  x_menus_sensitivity (mmenu, "&edit-unlock", selected);
  x_menus_sensitivity (mmenu, "&edit-embed", embeddable);
  x_menus_sensitivity (mmenu, "&edit-unembed", embeddable);
  x_menus_sensitivity (mmenu, "&edit-update", comp_selected);
  x_menus_sensitivity (mmenu, "&edit-deselect", selected);

  x_menus_sensitivity (mmenu, "&hierarchy-down-schematic", parent);
  x_menus_sensitivity (mmenu, "&hierarchy-down-symbol", comp_selected);
  x_menus_sensitivity (mmenu, "&hierarchy-up", has_parent);

  x_menus_sensitivity (mmenu, "&attributes-attach", selected);
  x_menus_sensitivity (mmenu, "&attributes-detach", selected);
  x_menus_sensitivity (mmenu, "&attributes-show-value", text_selected);
  x_menus_sensitivity (mmenu, "&attributes-show-name", text_selected);
  x_menus_sensitivity (mmenu, "&attributes-show-both", text_selected);
  x_menus_sensitivity (mmenu, "&attributes-visibility-toggle", text_selected);

  x_menus_sensitivity (mmenu, "&hierarchy-documentation", comp_selected);

  x_menus_sensitivity (mmenu, "&page-revert", !x_window_untitled_page (page));


  GtkWidget* pmenu = schematic_window_get_popup_menu (w_current);

  x_menus_sensitivity (pmenu, "&clipboard-cut", selected);
  x_menus_sensitivity (pmenu, "&clipboard-copy", selected);
  x_menus_sensitivity (pmenu, "&edit-delete", selected);

  x_menus_sensitivity (pmenu, "&edit-edit", selected);
  x_menus_sensitivity (pmenu, "&edit-text", text_selected);
  x_menus_sensitivity (pmenu, "&edit-object-properties", selected);

  x_menus_sensitivity (pmenu, "&hierarchy-down-schematic", parent);
  x_menus_sensitivity (pmenu, "&hierarchy-down-symbol", comp_selected);
  x_menus_sensitivity (pmenu, "&hierarchy-up", has_parent);

} /* i_update_menus() */



/*! \brief Set the main window's title
 *
 *  \par Function Description
 *  Set the main window's title using \a filename.
 *  Prepend an asterisk ("*") to indicate that the page
 *  is modified if \a changed is TRUE.
 *  Depending on [schematic.gui]\::title-show-path configuration
 *  value, either full path or basename of \a filename is shown
 *  in the title.
 *
 *  \param [in] w_current SchematicWindow structure
 *  \param [in] filename  The filename
 *  \param [in] changed   Page changed status
 */
void
i_set_filename (SchematicWindow* w_current,
                const gchar* filename,
                gboolean changed)
{
  g_return_if_fail (w_current != NULL);

  GtkWidget *main_window =
    schematic_window_get_main_window (w_current);
  g_return_if_fail (main_window != NULL);
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

  gtk_window_set_title (GTK_WINDOW (main_window), title);

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
 *  \param [in] w_current SchematicWindow structure
 */
void
i_update_grid_info (SchematicWindow *w_current)
{
  g_return_if_fail (w_current != NULL);

  GtkWidget *bottom_widget =
    schematic_window_get_bottom_widget (w_current);

  if (bottom_widget != NULL)
  {
    g_object_set (SCHEMATIC_BOTTOM_WIDGET (bottom_widget),
                  "snap-mode", schematic_options_get_snap_mode (w_current->options),
                  "snap-size", schematic_options_get_snap_size (w_current->options),
                  "grid-mode", schematic_options_get_grid_mode (w_current->options),
                  "grid-size", x_grid_query_drawn_spacing (w_current),
                  NULL);
  }
}



/*! \brief Write the grid settings to the gschem status bar
 *
 *  \param [in] view The page view originating the signal
 *  \param [in] w_current SchematicWindow structure
 */
void
i_update_grid_info_callback (SchematicCanvas *view,
                             SchematicWindow *w_current)
{
  i_update_grid_info (w_current);
}



/*! \brief Update the status bar: rubber band, magnetic net modes
 *
 *  \param [in] w_current SchematicWindow structure
 */
void
i_update_net_options_status (SchematicWindow* w_current)
{
  GtkWidget *bottom_widget =
    schematic_window_get_bottom_widget (w_current);

  schematic_bottom_widget_set_rubber_band_mode(
    SCHEMATIC_BOTTOM_WIDGET (bottom_widget),
    schematic_options_get_net_rubber_band_mode (w_current->options));

  schematic_bottom_widget_set_magnetic_net_mode(
    SCHEMATIC_BOTTOM_WIDGET (bottom_widget),
    schematic_options_get_magnetic_net_mode (w_current->options));
}
