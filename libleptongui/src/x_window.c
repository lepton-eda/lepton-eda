/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2016 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2017-2026 Lepton EDA Contributors
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
#include "schematic.h"


static GtkWidget*
create_notebook_right (SchematicWindow *w_current);

static GtkWidget*
create_notebook_bottom (SchematicWindow *w_current);

static int
untitled_next_index (SchematicWindow* w_current);


/*! \brief Create a schematic drawing canvas.
 *  \par Function Description
 *  Creates and initializes a schematic drawing canvas widget and
 *  adds it to the scrolled container of the parent
 *  #SchematicWindow instance The page of the canvas is set to the
 *  active page of the window.
 *
 *  \param [in] scrolled The scrolled container.
 *  \param [in] w_current The parent window.
 */
void x_window_create_drawing(GtkWidget *scrolled, SchematicWindow *w_current)
{
  LeptonPage* page = schematic_window_get_active_page (w_current);
  SchematicCanvas* view = schematic_canvas_new_with_page (page);

#ifdef ENABLE_GTK3
  gtk_widget_set_hexpand (GTK_WIDGET (view), TRUE);
  gtk_widget_set_vexpand (GTK_WIDGET (view), TRUE);
  gtk_widget_set_halign (GTK_WIDGET (view), GTK_ALIGN_FILL);
  gtk_widget_set_valign (GTK_WIDGET (view), GTK_ALIGN_FILL);
#endif

  GtkWidget *drawing_area = GTK_WIDGET (view);
  schematic_window_set_drawing_area (w_current, drawing_area);

  gtk_container_add (GTK_CONTAINER(scrolled), drawing_area);

  gtk_widget_set_can_focus (drawing_area, TRUE);
  gtk_widget_grab_focus (drawing_area);
  gtk_widget_show (drawing_area);
}


/*! \brief Set up callbacks for the drawing area.
 *  \par Function Description
 *
 * Installs GTK+ callback handlers for signals that are emitted by
 * the drawing area
 *
 * \param [in] w_current    The toplevel environment.
 * \param [in] drawing_area The drawing area (page view).
 */
void x_window_setup_draw_events_drawing_area (SchematicWindow* w_current,
                                              SchematicCanvas* drawing_area)
{
  /* gtk_widget_set_events() can be called on unrealized widgets only.
  *  Since with tabbed GUI (see x_tabs.c) we need to setup events
  *  for already created page view widgets, use
  *  gtk_widget_add_events() instead.
  */
  gtk_widget_add_events (GTK_WIDGET (drawing_area),
                         GDK_EXPOSURE_MASK |
                         GDK_POINTER_MOTION_MASK |
                         GDK_BUTTON_PRESS_MASK   |
                         GDK_ENTER_NOTIFY_MASK |
                         GDK_KEY_PRESS_MASK |
                         GDK_BUTTON_RELEASE_MASK);

#ifdef ENABLE_GTK3
  gint events;

  if (w_current->warp_cursor)
  {
    events = GDK_SCROLL_MASK;
  }
  else
  {
    events = GDK_SMOOTH_SCROLL_MASK | GDK_SCROLL_MASK;
  }

  gtk_widget_add_events (GTK_WIDGET (drawing_area), events);
#endif

} /* x_window_setup_draw_events_drawing_area() */


static void
x_window_hide_text (GtkWidget *widget, gint response, SchematicWindow *w_current)
{
  LeptonPage *page = NULL;

  g_return_if_fail (w_current != NULL);

  if (response == GTK_RESPONSE_OK) {
    page = schematic_window_get_active_page (w_current);
    o_edit_hide_specific_text (w_current,
                               lepton_page_objects (page),
                               schematic_show_hide_text_widget_get_text_string (SCHEMATIC_SHOW_HIDE_TEXT_WIDGET (widget)));
  }

  GtkWidget *drawing_area =
    schematic_window_get_drawing_area (w_current);
  gtk_widget_grab_focus (drawing_area);
  gtk_widget_hide (GTK_WIDGET (widget));
}


static void
x_window_show_text (GtkWidget *widget,
                    gint response,
                    SchematicWindow *w_current)
{
  LeptonPage *page = NULL;

  g_return_if_fail (w_current != NULL);

  if (response == GTK_RESPONSE_OK) {
    page = schematic_window_get_active_page (w_current);
    o_edit_show_specific_text (w_current,
                               lepton_page_objects (page),
                               schematic_show_hide_text_widget_get_text_string (SCHEMATIC_SHOW_HIDE_TEXT_WIDGET (widget)));
  }

  GtkWidget *drawing_area =
    schematic_window_get_drawing_area (w_current);
  gtk_widget_grab_focus (drawing_area);
  gtk_widget_hide (GTK_WIDGET (widget));
}


void
x_window_select_object (SchematicFindTextState *state,
                        LeptonObject *object,
                        SchematicWindow *w_current,
                        SchematicCanvas *view,
                        LeptonPage *object_page,
                        gboolean same_page)
{
  if (!same_page)
  {
    /* tabbed GUI: current page view may be different here: */
    view = schematic_window_get_current_canvas (w_current);
  }

  schematic_canvas_zoom_object (view, object);
}

static void
x_window_translate_response (SchematicTranslateWidget *widget,
                             gint response,
                             SchematicWindow *w_current)
{
  if (response == GTK_RESPONSE_OK) {
    o_component_translate_all (w_current,
                               schematic_translate_widget_get_value (widget));
  }

  i_set_state (w_current, SELECT);
  GtkWidget *drawing_area =
    schematic_window_get_drawing_area (w_current);
  gtk_widget_grab_focus (drawing_area);
  gtk_widget_hide (GTK_WIDGET (widget));
}


/*! \brief Creates a new main window widget.
 *  \par Function Description
 * Creates a new lepton-schematic window and initializes some of
 * its properties.
 *
 * \param app Pointer to the GtkApplication (for GTK3).
 * \return Pointer to the new GtkWidget object.
 */
GtkWidget*
schematic_window_create_app_window (gpointer app)
{
  GtkWidget *main_window = NULL;

#ifdef ENABLE_GTK3
  g_return_val_if_fail (app != NULL, NULL);
  main_window = gtk_application_window_new (GTK_APPLICATION (app));
#else
  main_window = GTK_WIDGET (schematic_main_window_new ());
#endif

  gtk_widget_set_name (main_window, "lepton-schematic");
  gtk_window_set_resizable (GTK_WINDOW (main_window), TRUE);

  return main_window;
}


/*! \brief Create a top level box container for widgets.
 *  \par Function Description
 * In the main window widget of a lepton-schematic window, creates
 * a top level box container which will contain the menubar,
 * toolbar and other widgets.
 *
 * \param main_window The main window widget.
 * \return Pointer to the new GtkWidget object.
 */
GtkWidget*
schematic_window_create_main_box (GtkWidget *main_window)
{
  GtkWidget *main_box = NULL;

  g_return_val_if_fail (main_window != NULL, NULL);

#ifdef ENABLE_GTK3
  main_box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 1);
#else
  main_box = gtk_vbox_new (FALSE, 1);
#endif
  gtk_container_set_border_width (GTK_CONTAINER (main_box), 0);
  gtk_container_add (GTK_CONTAINER (main_window), main_box);

  return main_box;
}


/*! \brief Create a container for scrolled window and bottom infowidgets
 *  \par Function Description
 *  Creates a container for scrolled canvas and bottom
 *  infowidgets.  When tabbed GUI is enabled, it will contain the
 *  tabs notebook.
 *
 * \return Pointer to the new GtkWidget object.
 */
GtkWidget*
schematic_window_create_work_box ()
{
  GtkWidget *work_box = NULL;

#ifdef ENABLE_GTK3
  work_box = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
  work_box = gtk_vbox_new (FALSE, 0);
#endif

  return work_box;
}


/*! \brief Create a page view
 *  \par Function Description
 *  Creates a scrolled #SchematicCanvas widget in the working area
 *  \a work_box.  This function is used when tabs are disabled.
 *
 * \param w_current The #SchematicWindow object.
 * \param work_box The working area widget.
 * \return Pointer to the new GtkWidget object.
 */
SchematicCanvas*
schematic_window_create_canvas (SchematicWindow *w_current,
                                GtkWidget *work_box)
{
  GtkWidget *scrolled = NULL;

  g_return_val_if_fail (w_current != NULL, NULL);
  g_return_val_if_fail (work_box != NULL, NULL);

  /* scrolled window (parent of page view): */
  scrolled = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_add (GTK_CONTAINER (work_box), scrolled);

  /* create page view: */
  x_window_create_drawing (scrolled, w_current);
  x_window_setup_scrolling (w_current, scrolled);

  return schematic_window_get_current_canvas (w_current);
}


/*! \brief Create paned widgets with notebooks
 *  \par Function Description
 *  Creates bottom and right notebooks and two paned widgets for
 *  them.
 *
 * \param [in] w_current The #SchematicWindow object.
 * \param [in] main_box The top level box container widget.
 * \param [in] work_box The working area widget.
 */
void
schematic_window_create_notebooks (SchematicWindow *w_current,
                                   GtkWidget *main_box,
                                   GtkWidget *work_box)
{
  GtkWidget *hpaned = NULL;
  GtkWidget *vpaned = NULL;

#ifdef ENABLE_GTK3
  vpaned = gtk_paned_new (GTK_ORIENTATION_VERTICAL);
  hpaned = gtk_paned_new (GTK_ORIENTATION_HORIZONTAL);
#else
  vpaned = gtk_vpaned_new ();
  hpaned = gtk_hpaned_new ();
#endif

  GtkWidget *right_notebook = create_notebook_right (w_current);
  schematic_window_set_right_notebook (w_current, right_notebook);

  GtkWidget *bottom_notebook = create_notebook_bottom (w_current);
  schematic_window_set_bottom_notebook (w_current, bottom_notebook);

  gtk_container_add (GTK_CONTAINER (main_box), vpaned);


  gtk_paned_pack1 (GTK_PANED (vpaned), hpaned,
                   TRUE, TRUE);

  gtk_paned_pack2 (GTK_PANED (vpaned), bottom_notebook,
                   FALSE, TRUE);


  gtk_paned_pack1 (GTK_PANED (hpaned), work_box,
                   TRUE, TRUE);

  gtk_paned_pack2 (GTK_PANED (hpaned), right_notebook,
                   FALSE, TRUE);
}


/*! \brief Show widgets of schematic window
 *  \par Function Description
 *  Shows widgets of schematic window, sets visibility of right
 *  and bottom notebooks, and sets focus to the drawing area.
 *
 * \param [in] w_current The #SchematicWindow object.
 * \param [in] main_window The main window widget.
 */
void
schematic_window_show_all (SchematicWindow *w_current,
                           GtkWidget *main_window)
{
  g_return_if_fail (w_current != NULL);
  g_return_if_fail (main_window != NULL);

  /* show all widgets: */
  gtk_widget_show_all (main_window);


  if ( !x_widgets_use_docks() )
  {
    GtkWidget *bottom_notebook =
      schematic_window_get_bottom_notebook (w_current);
    GtkWidget *right_notebook =
      schematic_window_get_right_notebook (w_current);
    gtk_widget_set_visible (right_notebook, FALSE);
    gtk_widget_set_visible (bottom_notebook, FALSE);
  }


  GtkWidget *drawing_area =
    schematic_window_get_drawing_area (w_current);
  /* focus page view: */
  gtk_widget_grab_focus (drawing_area);
}


/*! \brief Changes the current page.
 *  \private
 *  \par Function Description
 *  This function displays the specified page <B>page</B> in the
 *  window attached to <B>toplevel</B>.
 *
 *  It changes the <B>toplevel</B>'s current page to <B>page</B>,
 *  draws it and updates the user interface.
 *
 *  <B>page</B> has to be in the list of PAGEs attached to <B>toplevel</B>.
 *
 *  \param [in] w_current The toplevel environment.
 *  \param [in] page      The page to become current page.
 */
void
x_window_set_current_page (SchematicWindow *w_current,
                           LeptonPage *page)
{
  SchematicCanvas *page_view = schematic_window_get_current_canvas (w_current);
  g_return_if_fail (page_view != NULL);

  g_return_if_fail (page != NULL);

  o_redraw_cleanstates (w_current);

  schematic_canvas_set_page (page_view, page);

  i_update_menus (w_current);
  /* i_set_filename (w_current, page->page_filename); */

  page_select_widget_update (w_current);
  schematic_multiattrib_widget_update (w_current);

} /* x_window_set_current_page() */



/*! \brief Saves a page to a file.
 *  \par Function Description
 *  This function saves the page <B>page</B> to a file named
 *  <B>filename</B>.
 *
 *  It returns the value returned by function <B>f_save()</B> trying
 *  to save page <B>page</B> to file <B>filename</B> (1 on success, 0
 *  on failure).
 *
 *  \param [in] w_current The current #SchematicWindow environment.
 *  \param [in] page      The page to save.
 *  \param [in] filename  The name of the file in which to save page.
 *  \returns 1 on success, 0 otherwise.
 */
gint
x_window_save_page (SchematicWindow *w_current,
                    LeptonPage *page,
                    const gchar *filename)
{
  const gchar *log_msg, *state_msg;
  gint ret;
  GError *err = NULL;

  g_return_val_if_fail (page     != NULL, 0);
  g_return_val_if_fail (filename != NULL, 0);

  /* try saving page to filename */
  ret = (gint)f_save (page, filename, &err);

  if (ret != 1) {
    log_msg   = _("Could NOT save page [%1$s]");
    state_msg = _("Error while trying to save");

    GtkWidget *main_window =
      schematic_window_get_main_window (w_current);

    GtkWidget *dialog;
    dialog = gtk_message_dialog_new (GTK_WINDOW (main_window),
                                     GTK_DIALOG_DESTROY_WITH_PARENT,
                                     GTK_MESSAGE_ERROR,
                                     GTK_BUTTONS_CLOSE,
                                     "%s",
                                     err->message);
    gtk_window_set_title (GTK_WINDOW (dialog), _("Failed to save file"));
    gtk_dialog_run (GTK_DIALOG (dialog));
    gtk_widget_destroy (dialog);
    g_clear_error (&err);
  } else {
    /* successful save of page to file, update page... */
    /* change page name if necessary and prepare log message */
    if (g_ascii_strcasecmp (lepton_page_get_filename (page), filename) != 0)
    {
      lepton_page_set_filename (page, filename);

      log_msg = _("Saved as [%1$s]");
    } else {
      log_msg = _("Saved [%1$s]");
    }
    state_msg = _("Saved");

    /* reset page CHANGED flag */
    lepton_page_set_changed (page, 0);

    /* add to recent file list */
    recent_manager_add (w_current, filename);

    page_select_widget_update (w_current);
  }

  /* log status of operation */
  g_message (log_msg, filename);

  i_set_state_msg  (w_current, SELECT, state_msg);

  return ret;

} /* x_window_save_page() */


LeptonPage*
schematic_window_find_new_current_page (LeptonToplevel *toplevel,
                                        LeptonPage *page)
{
  GList *iter;
  LeptonPageList* pages = lepton_toplevel_get_pages (toplevel);

  /* as it will delete current page, select new current page */
  /* first look up in page hierarchy */
  int id = lepton_page_get_up (page);
  LeptonPage *new_current = lepton_toplevel_search_page_by_id (pages, id);

  if (new_current == NULL) {
    /* no up in hierarchy, choice is prev, next, new page */
    iter = g_list_find (lepton_list_get_glist (pages), page);

    if ( g_list_previous( iter ) ) {
      new_current = (LeptonPage *)g_list_previous( iter )->data;
    } else if ( g_list_next( iter ) ) {
      new_current = (LeptonPage *)g_list_next( iter )->data;
    } else {
      /* need to add a new untitled page */
      new_current = NULL;
    }
  }

  return new_current;
}


/*! \brief Creates and initializes a new lepton-schematic window.
 *
 * \return Pointer to the new SchematicWindow object.
 */
SchematicWindow* x_window_new (LeptonToplevel *toplevel)
{
  SchematicWindow *w_current = schematic_window_new ();
  schematic_window_set_toplevel (w_current, toplevel);

  /* Damage notifications should invalidate the object on screen */
  lepton_object_add_change_notify (toplevel,
                                   (ChangeNotifyFunc) o_invalidate,
                                   (ChangeNotifyFunc) o_invalidate,
                                   w_current);

  /* Initialize tabbed GUI: */
  x_tabs_init();

  return w_current;
}



/*! \brief Add a menubar widget to the main container of a window.
 *  \par Function Description
 *  Adds a menubar at the top of the program window.  GTK2 version
 *  of the widget may have 'handle boxes' depending on configuration.
 *
 *  \param [in] w_current The toplevel GUI window structure.
 *  \param [in] main_box The main container of the app window.
 *  \param [in] menubar The menubar widget created elsewhere.
 */
void
schematic_window_create_menubar (SchematicWindow *w_current,
                                 GtkWidget *main_box,
                                 GtkWidget *menubar)
{
  g_return_if_fail (w_current != NULL);

#ifdef ENABLE_GTK3
  gtk_box_pack_start (GTK_BOX (main_box), menubar, FALSE, FALSE, 0);
#else
  if (w_current->handleboxes)
  {
    GtkWidget *handlebox = gtk_handle_box_new ();
    gtk_box_pack_start (GTK_BOX (main_box), handlebox, FALSE, FALSE, 0);
    gtk_container_add (GTK_CONTAINER (handlebox), menubar);
  }
  else
  {
    gtk_box_pack_start (GTK_BOX (main_box), menubar, FALSE, FALSE, 0);
  }
#endif

  schematic_window_set_menubar (w_current, menubar);
}


void
schematic_window_pack_widget (GtkWidget *parent_widget,
                              GtkWidget *child_widget)
{
  gtk_box_pack_start (GTK_BOX (parent_widget), child_widget, FALSE, FALSE, 0);
}



void
schematic_window_create_hide_text_widget (SchematicWindow *w_current,
                                          GtkWidget *work_box)
{
  gpointer obj = g_object_new (SCHEMATIC_TYPE_SHOW_HIDE_TEXT_WIDGET,
                               "button-text", _("Hide"),
                               "label-text", _("Hide text starting with:"),
                               NULL);

  GtkWidget *hide_text_widget = GTK_WIDGET (obj);
  schematic_window_set_hide_text_widget (w_current, hide_text_widget);

  gtk_box_pack_start (GTK_BOX (work_box),
                      hide_text_widget,
                      FALSE, FALSE, 0);

  g_signal_connect (hide_text_widget, "response",
                    G_CALLBACK (&x_window_hide_text), w_current);
}



void
schematic_window_create_show_text_widget (SchematicWindow *w_current,
                                          GtkWidget *work_box)
{
  gpointer obj = g_object_new (SCHEMATIC_TYPE_SHOW_HIDE_TEXT_WIDGET,
                               "button-text", _("Show"),
                               "label-text", _("Show text starting with:"),
                               NULL);

  GtkWidget *show_text_widget = GTK_WIDGET (obj);
  schematic_window_set_show_text_widget (w_current, show_text_widget);

  gtk_box_pack_start (GTK_BOX (work_box),
                      show_text_widget,
                      FALSE, FALSE, 0);

  g_signal_connect (show_text_widget, "response",
                    G_CALLBACK (&x_window_show_text), w_current);
}


void
schematic_window_create_translate_widget (SchematicWindow *w_current,
                                          GtkWidget *work_box)
{
  gpointer obj = g_object_new (SCHEMATIC_TYPE_TRANSLATE_WIDGET, NULL);

  GtkWidget *widget = GTK_WIDGET (obj);
  schematic_window_set_translate_widget (w_current, widget);

  gtk_box_pack_start (GTK_BOX (work_box), widget, FALSE, FALSE, 0);

  g_signal_connect (widget, "response",
                    G_CALLBACK (&x_window_translate_response), w_current);
}


void
schematic_window_show_translate_widget (SchematicWindow *w_current)
{
  GtkWidget *widget =
    schematic_window_get_translate_widget (w_current);

  gtk_widget_show (widget);
  gtk_widget_grab_focus (schematic_translate_widget_get_entry (SCHEMATIC_TRANSLATE_WIDGET (widget)));
}



void
schematic_window_create_statusbar (SchematicWindow *w_current,
                                   GtkWidget *main_box)
{
  const char* text_mid_button = _("none");

#ifdef HAVE_LIBSTROKE
  if (w_current->middle_button == MOUSEBTN_DO_STROKE)
    text_mid_button = _("Stroke");
#endif

  if (w_current->middle_button == MOUSEBTN_DO_ACTION)
    text_mid_button = _("Action");
  else
  if (w_current->middle_button == MOUSEBTN_DO_REPEAT)
      text_mid_button = _("Repeat");
  else
  if (w_current->middle_button == MOUSEBTN_DO_PAN)
      text_mid_button = _("Pan");
  else
  if (w_current->middle_button == MOUSEBTN_DO_POPUP)
      text_mid_button = _("Menu");


  const char* text_right_button_action = NULL;
  const char* text_right_button_cancel = NULL;
  char*       text_right_button        = NULL;

  if (w_current->third_button == MOUSEBTN_DO_POPUP)
  {
    text_right_button_action = _("Menu");
  }
  else
  if (w_current->third_button == MOUSEBTN_DO_PAN)
  {
    text_right_button_action = _("Pan");
  }
  else
  {
    text_right_button_action = _("none");
  }

  if (w_current->third_button_cancel)
  {
    text_right_button_cancel = _("/Cancel");
  }
  else
  {
    text_right_button_cancel = "";
  }

  text_right_button = g_strdup_printf ("%s%s",
                                       text_right_button_action,
                                       text_right_button_cancel);

  gpointer obj = g_object_new (SCHEMATIC_TYPE_BOTTOM_WIDGET,
                               "toplevel",
                               w_current,
                               "grid-mode",
                               schematic_options_get_grid_mode (w_current->options),
                               "grid-size",
                               schematic_options_get_snap_size (w_current->options),
                               /* x_grid_query_drawn_spacing (w_current), -- occurs before the page is set */
                               "left-button-text",
                               _("Pick"),
                               "middle-button-text",
                               text_mid_button,
                               "right-button-text",
                               text_right_button,
                               "snap-mode",
                               schematic_options_get_snap_mode (w_current->options),
                               "snap-size",
                               schematic_options_get_snap_size (w_current->options),
                               "status-text",
                               _("Select Mode"),
                               "net-rubber-band-mode",
                               schematic_options_get_net_rubber_band_mode (w_current->options),
                               "magnetic-net-mode",
                               schematic_options_get_magnetic_net_mode (w_current->options),
                               NULL);

  g_free (text_right_button);

  GtkWidget *bottom_widget = GTK_WIDGET (obj);
  schematic_window_set_bottom_widget (w_current, bottom_widget);

  gtk_box_pack_start (GTK_BOX (main_box), bottom_widget, FALSE, FALSE, 0);

} /* schematic_window_create_statusbar */



/*! \brief Setup scrolling parameters
 *
 *  \param [in] w_current The current #SchematicWindow environment.
 *  \param [in] scrolled  Scrolled widget - a parent of page view widget
 */
void
x_window_setup_scrolling (SchematicWindow *w_current,
                          GtkWidget *scrolled)
{
  GtkAdjustment *hadjustment = GTK_ADJUSTMENT(
    gtk_adjustment_new (0.0,
                        WORLD_DEFAULT_LEFT,
                        WORLD_DEFAULT_RIGHT,
                        100.0,
                        100.0,
                        10.0));

  GtkAdjustment *vadjustment = GTK_ADJUSTMENT(
    gtk_adjustment_new (WORLD_DEFAULT_BOTTOM,
                        0.0,
                        WORLD_DEFAULT_BOTTOM - WORLD_DEFAULT_TOP,
                        100.0,
                        100.0,
                        10.0));

  gtk_scrolled_window_set_hadjustment (GTK_SCROLLED_WINDOW (scrolled), hadjustment);
  gtk_scrolled_window_set_vadjustment (GTK_SCROLLED_WINDOW (scrolled), vadjustment);

  GtkPolicyType policy = schematic_window_get_scrollbars_flag (w_current) ?
                         GTK_POLICY_ALWAYS :
                         GTK_POLICY_NEVER;

  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled),
                                  policy, policy);

} /* x_window_setup_scrolling() */



static GtkWidget*
create_notebook_right (SchematicWindow* w_current)
{
  GtkWidget *notebook = gtk_notebook_new ();

  if ( x_widgets_use_docks() )
  {
    GtkWidget *object_properties =
      schematic_window_get_object_properties_widget (w_current);
    GtkWidget *text_properties =
      schematic_window_get_text_properties_widget (w_current);
    GtkWidget *options_widget =
      schematic_window_get_options_widget (w_current);
    gtk_notebook_append_page (GTK_NOTEBOOK (notebook),
                              object_properties,
                              gtk_label_new(_("Object")));

    gtk_notebook_append_page (GTK_NOTEBOOK (notebook),
                              text_properties,
                              gtk_label_new(_("Text")));

    gtk_notebook_append_page (GTK_NOTEBOOK (notebook),
                              options_widget,
                              gtk_label_new(_("Options")));



    gtk_container_set_border_width (GTK_CONTAINER (notebook),
                                    DIALOG_BORDER_SPACING);
  }

  return notebook;
}



static GtkWidget*
create_notebook_bottom (SchematicWindow* w_current)
{
  GtkWidget *notebook = gtk_notebook_new ();

  if ( x_widgets_use_docks() )
  {
    GtkWidget *find_text_state =
      schematic_window_get_find_text_state_widget (w_current);
    GtkWidget *log_widget =
      schematic_window_get_log_widget (w_current);
    gtk_notebook_append_page (GTK_NOTEBOOK (notebook),
                              find_text_state,
                              gtk_label_new(_("Find Text")));

    gtk_notebook_append_page (GTK_NOTEBOOK (notebook),
                              log_widget,
                              gtk_label_new(_("Log")));


    gtk_container_set_border_width (GTK_CONTAINER (notebook),
                                    DIALOG_BORDER_SPACING);
  }

  return notebook;
}



/*! \brief Show "Failed to load file" dialog.
 *
 *  \param w_current The toplevel environment.
 *  \param filename  File path that failed to load.
 *  \param error_message Associated error message.
 */
void
open_page_error_dialog (SchematicWindow* w_current,
                        const gchar *filename,
                        char *error_message)
{
  g_return_if_fail (w_current != NULL);

  const gchar* msg =
    _("<b>An error occurred while loading the requested file.</b>"
      "\n\n"
      "Loading from '%1$s' failed. Error message:"
      "\n\n"
      "%2$s."
      "\n\n"
      "The lepton-schematic log may contain more information.\n"
      "You may also launch lepton-schematic with --verbose command"
      " line switch and monitor program's output in terminal window.");

  GtkWidget *main_window =
    schematic_window_get_main_window (w_current);

  GtkWidget* dialog = gtk_message_dialog_new_with_markup
    (GTK_WINDOW (main_window),
    GTK_DIALOG_DESTROY_WITH_PARENT,
    GTK_MESSAGE_ERROR,
    GTK_BUTTONS_CLOSE,
    msg,
    filename,
    error_message);

  gtk_window_set_title (GTK_WINDOW (dialog), _("Failed to load file"));

  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);

} /* open_page_error_dialog() */



/*! \brief Add \a filename to the recent files list.
 *
 * \todo gtk_recent_manager_add_item() also used in x_menus.c.
 *       Consider making this function public.
 *
 *  \param w_current The toplevel environment.
 *  \param filename  File name to add.
 */
void
recent_manager_add (SchematicWindow* w_current,
                    const gchar*    filename)
{
  g_return_if_fail (w_current != NULL);

  GtkRecentManager* manager = w_current->recent_manager;
  if (manager != NULL)
  {
    gchar* uri = g_filename_to_uri (filename, NULL, NULL);
    gtk_recent_manager_add_item (manager, uri);
    g_free (uri);
  }

} /* recent_manager_add() */



/*! \brief Get next number to be part of the untitled file name.
 */
static int
untitled_next_index (SchematicWindow* w_current)
{
  return ++w_current->num_untitled;
}



/*! \brief Get untitled file name.
 *  \par Function Description
 *
 * Determine "untitled" schematic file name (used for new pages)
 * and build full path from this name and current working
 * directory.  When constructing this name, avoid reusing names of
 * already opened files and existing files in current directory.
 * Such (avoided) names are reported to the log.
 *
 *  \param  w_current   The toplevel environment.
 *  \return             Newly-allocated untitled file path.
 */
gchar*
untitled_filename (SchematicWindow* w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  /* Determine default file name (without a number appended)
  *  for a new page:
  */
  gchar*     cwd = g_get_current_dir ();
  EdaConfig* cfg = eda_config_get_context_for_path (cwd);

  gchar* name = eda_config_get_string (cfg,
                                       "schematic",
                                       "default-filename",
                                       NULL);

  gchar* fname = NULL;
  gchar* fpath = NULL;

  LeptonToplevel* toplevel = schematic_window_get_toplevel (w_current);

  for (;;)
  {
    /* Build file name (default name + number appended):
    */
    fname = g_strdup_printf ("%s_%d.sch",
                             name ? name : UNTITLED_FILENAME_PREFIX,
                             untitled_next_index (w_current));

    /* Build full path for file name:
    */
    fpath = g_build_filename (cwd, fname, NULL);

    /* Avoid reusing names of already opened files:
    *  Avoid reusing names of existing files in current directory:
    */
    if ( lepton_toplevel_search_page_by_basename (toplevel, fname) ||
         g_file_test (fpath, G_FILE_TEST_EXISTS) )
    {
      g_message (_("Skipping existing file [%s]"), fname);

      g_free (fname);
      g_free (fpath);
    }
    else
    {
      break;
    }
  }

  g_free (cwd);
  g_free (name);
  g_free (fname);

  return fpath;

} /* untitled_filename() */



/*! \brief Determine if a given \a page is "untitled" one.
 *  \par Function Description
 *
 *"Untitled" pages are newly created pages with the default
 * file name and not yet saved to disk.
 * This function checks if the \a page meets these conditions.
 *
 *  \param  page The page to check.
 *  \return      TRUE if the \a page looks like "untitled" one.
 */
gboolean
x_window_untitled_page (LeptonPage* page)
{
  g_return_val_if_fail (page != NULL, TRUE);

  const gchar* fname = lepton_page_get_filename (page);
  gchar* uname = NULL;

  EdaConfig* cfg = eda_config_get_context_for_path (fname);
  if (cfg != NULL)
  {
    uname = eda_config_get_string (cfg,
                                   "schematic",
                                   "default-filename",
                                   NULL);
  }

  if (uname == NULL)
  {
    uname = g_strdup (UNTITLED_FILENAME_PREFIX);
  }

  gboolean named_like_untitled = strstr (fname, uname) != NULL;
  gboolean file_exists = g_file_test (fname, G_FILE_TEST_EXISTS);

  g_free (uname);

  /*
   * consider page as "untitled" if it is named like untitled
   * and associated file does not exist:
  */
  return named_like_untitled && !file_exists;

} /* untitled_page() */



/*! \brief Save main window's geometry to the CACHE config context.
 *
 *  \param w_current The toplevel environment.
 */
void
schematic_window_save_geometry (SchematicWindow* w_current)
{
  GtkWidget *main_window =
    schematic_window_get_main_window (w_current);

  gint x = 0;
  gint y = 0;
  gtk_window_get_position (GTK_WINDOW (main_window), &x, &y);

  gint width = 0;
  gint height = 0;
  gtk_window_get_size (GTK_WINDOW (main_window), &width, &height);

  EdaConfig* cfg = eda_config_get_cache_context();

  eda_config_set_int (cfg, "schematic.window-geometry", "x", x);
  eda_config_set_int (cfg, "schematic.window-geometry", "y", y);
  eda_config_set_int (cfg, "schematic.window-geometry", "width", width);
  eda_config_set_int (cfg, "schematic.window-geometry", "height", height);

  eda_config_save (cfg, NULL);
}



/*! \brief Restore main window's geometry.
 *
 *  \par Function Description
 *  If [schematic.gui]\::restore-window-geometry configuration key is
 *  set to true, read main window's geometry from the CACHE config
 *  context and restore it.
 *  Unless valid configuration values are read, use default width
 *  and height.
 *
 *  \param [in] w_current   The #SchematicWindow object.
 *  \param [in] main_window The main window widget of lepton-schematic.
 */
void
schematic_window_restore_geometry (SchematicWindow *w_current,
                                   GtkWidget* main_window)
{
  gchar* cwd = g_get_current_dir();
  EdaConfig* cfg = eda_config_get_context_for_path (cwd);
  g_free (cwd);

  gboolean restore = TRUE; /* by default, restore geometry */
  GError*  err = NULL;
  gboolean val = eda_config_get_boolean (cfg,
                                         "schematic.gui",
                                         "restore-window-geometry",
                                         &err);
  if (err == NULL)
  {
    restore = val;
  }

  g_clear_error (&err);


  gint width  = -1;
  gint height = -1;

  if (restore)
  {
    EdaConfig* ccfg = eda_config_get_cache_context();

    gint x = eda_config_get_int (ccfg, "schematic.window-geometry", "x", NULL);
    gint y = eda_config_get_int (ccfg, "schematic.window-geometry", "y", NULL);

    if (x > 0 && y > 0)
    {
      gtk_window_move (GTK_WINDOW (main_window), x, y);
    }

    width  = eda_config_get_int (ccfg, "schematic.window-geometry", "width",  NULL);
    height = eda_config_get_int (ccfg, "schematic.window-geometry", "height", NULL);
  }


  if (width <= 0 || height <= 0)
  {
    width  = default_width;
    height = default_height;
  }

  gtk_window_resize (GTK_WINDOW (main_window), width, height);


  if (x_widgets_use_docks())
  {
    GtkWidget *find_text_state =
      schematic_window_get_find_text_state_widget (w_current);
    gtk_widget_set_size_request (find_text_state, -1, height / 4);
  }
}
