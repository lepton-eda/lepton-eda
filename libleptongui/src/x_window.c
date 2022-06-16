/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2016 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2017-2022 Lepton EDA Contributors
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
#include "gschem.h"


static void
create_toolbar_button (GschemToplevel *w_current,
                       GtkWidget *toolbar,
                       const gchar *pixmap_name,
                       const gchar *label,
                       const gchar *tooltip,
                       GCallback callback,
                       gint pos);

static GtkWidget*
create_toolbar_radio_button (GSList** group,
                             GschemToplevel *w_current,
                             GtkWidget *toolbar,
                             const gchar *pixmap_name,
                             const gchar *label,
                             const gchar *tooltip,
                             GCallback callback,
                             gint pos);

static void
create_toolbar_separator (GtkWidget *toolbar, gint pos);

static void
create_bottom_widget (GschemToplevel *w_current, GtkWidget *main_box);


static GtkWidget*
create_notebook_right (GschemToplevel *w_current);

static GtkWidget*
create_notebook_bottom (GschemToplevel *w_current);


static void
geometry_save (GschemToplevel* w_current);

static void
geometry_restore (GtkWidget* main_window,
                  GtkWidget *find_text_state);

static void
open_page_error_dialog (GschemToplevel* w_current,
                        const gchar*    filename,
                        GError*         err);

static void
recent_manager_add (GschemToplevel* w_current,
                    const gchar*    filename);

static int
untitled_next_index (GschemToplevel* w_current);

static gchar*
untitled_filename (GschemToplevel* w_current, gboolean log_skipped);

static LeptonPage*
x_window_new_page (GschemToplevel* w_current);



/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
GschemToplevel*
x_window_setup (GschemToplevel *w_current)
{
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);

  /* immediately setup user params */
  i_vars_set(w_current);

  /* Initialize the autosave callback */
  lepton_toplevel_init_autosave (toplevel);

  /* Initialize the clipboard callback */
  x_clipboard_init (w_current);

  /* Add to the list of windows */
  global_window_list = g_list_append (global_window_list, w_current);

  return w_current;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_window_create_drawing(GtkWidget *scrolled, GschemToplevel *w_current)
{
  LeptonPage* page = schematic_window_get_active_page (w_current);
  GschemPageView* view = gschem_page_view_new_with_page (page);

#ifdef ENABLE_GTK3
  gtk_widget_set_hexpand (GTK_WIDGET (view), TRUE);
  gtk_widget_set_vexpand (GTK_WIDGET (view), TRUE);
  gtk_widget_set_halign (GTK_WIDGET (view), GTK_ALIGN_FILL);
  gtk_widget_set_valign (GTK_WIDGET (view), GTK_ALIGN_FILL);
#endif

  w_current->drawing_area = GTK_WIDGET (view);

  gtk_container_add(GTK_CONTAINER(scrolled), w_current->drawing_area);

  gtk_widget_set_can_focus (w_current->drawing_area, TRUE);
  gtk_widget_grab_focus (w_current->drawing_area);
  gtk_widget_show (w_current->drawing_area);
}



/*! \brief Set up callbacks for window events that affect drawing.
 *  \par Function Description
 *
 * Installs GTK+ callback handlers for the main window
 * that affect the drawing area.
 *
 * \param [in] w_current   The toplevel environment.
 * \param [in] main_window The main window.
 */
void x_window_setup_draw_events_main_wnd (GschemToplevel* w_current,
                                          GtkWidget* main_window)
{
  struct event_reg_t
  {
    const gchar* detailed_signal;
    GCallback    c_handler;
  };

  struct event_reg_t main_window_events[] =
  {
    { "enter_notify_event", G_CALLBACK(x_event_enter) },
    { NULL,                 NULL                      }
  };

  struct event_reg_t* tmp = NULL;

  for (tmp = main_window_events; tmp->detailed_signal != NULL; tmp++)
  {
    g_signal_connect (main_window,
                      tmp->detailed_signal,
                      tmp->c_handler,
                      w_current);
  }

} /* x_window_setup_draw_events_main_wnd() */


gpointer _key_event_callback = NULL;

/*! \brief Set key event callback
 *  \par Function Description
 *  Sets key event processing callback to a given function
 *  pointer.  Currently it is necessary as key event processing is
 *  handled in Scheme.
 *
 * \param [in] key_event_callback The pointer to the callback.
 */
void
schematic_window_set_key_event_callback (gpointer key_event_callback)
{
  _key_event_callback = key_event_callback;
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
void x_window_setup_draw_events_drawing_area (GschemToplevel* w_current,
                                              GschemPageView* drawing_area)
{
  struct event_reg_t
  {
    const gchar* detailed_signal;
    GCallback    c_handler;
  };

  struct event_reg_t drawing_area_events[] =
  {
#ifdef ENABLE_GTK3
    { "draw",                 G_CALLBACK(x_event_draw)                         },
#else
    { "expose_event",         G_CALLBACK(x_event_expose)                       },
#endif
    { "button_press_event",   G_CALLBACK(x_event_button_pressed)               },
    { "button_release_event", G_CALLBACK(x_event_button_released)              },
    { "motion_notify_event",  G_CALLBACK(x_event_motion)                       },
    { "configure_event",      G_CALLBACK(x_event_configure)                    },
    { "key_press_event",      G_CALLBACK(_key_event_callback)                   },
    { "key_release_event",    G_CALLBACK(_key_event_callback)                   },
    { "scroll_event",         G_CALLBACK(x_event_scroll)                       },
    { "update-grid-info",     G_CALLBACK(i_update_grid_info_callback)          },
    { "notify::page",         G_CALLBACK(gschem_toplevel_notify_page_callback) },
    { NULL,                   NULL                                             }
  };


  /* is the configure event type missing here? hack */


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

  struct event_reg_t* tmp = NULL;

  for (tmp = drawing_area_events; tmp->detailed_signal != NULL; tmp++)
  {
    g_signal_connect (drawing_area,
                      tmp->detailed_signal,
                      tmp->c_handler,
                      w_current);
  }

} /* x_window_setup_draw_events_drawing_area() */



/*! \brief Creates a new GtkImage displaying a GTK stock icon if available.
 *
 * If a stock GTK icon with the requested name was not found, this function
 * falls back to the bitmap icons provided in the distribution.
 *
 * \param stock Name of the stock icon ("new", "open", etc.)
 * \param w_current Schematic top level
 * \return Pointer to the new GtkImage object.
 */
static GtkWidget *x_window_stock_pixmap(const char *stock, GschemToplevel *w_current)
{
  GtkWidget *wpixmap = NULL;
#ifdef ENABLE_GTK3
  /* Look up the icon in the icon theme. */
  wpixmap = gtk_image_new_from_icon_name (stock,
                                          GTK_ICON_SIZE_LARGE_TOOLBAR);
#else
  GtkStockItem item;

  gchar *stockid=g_strconcat("gtk-", stock, NULL);

  /* First check if GTK knows this icon */
  if(gtk_stock_lookup(stockid, &item)) {
    wpixmap = gtk_image_new_from_stock(stockid,
                                       GTK_ICON_SIZE_LARGE_TOOLBAR);
  } else {
    /* Look up the icon in the icon theme */
    wpixmap = gtk_image_new_from_icon_name (stock,
                                            GTK_ICON_SIZE_LARGE_TOOLBAR);
  }

  g_free(stockid);
#endif

  return wpixmap;
}


static void
x_window_find_text (GtkWidget *widget, gint response, GschemToplevel *w_current)
{
  gint close = FALSE;
  int count;

  g_return_if_fail (w_current != NULL);
  g_return_if_fail (w_current->toplevel != NULL);

  gboolean show_hidden_text =
    gschem_toplevel_get_show_hidden_text (w_current);

  switch (response) {
  case GTK_RESPONSE_OK:
    count = gschem_find_text_state_find (
        w_current,
        GSCHEM_FIND_TEXT_STATE (w_current->find_text_state),
        lepton_list_get_glist (w_current->toplevel->pages),
        gschem_find_text_widget_get_find_type (GSCHEM_FIND_TEXT_WIDGET (w_current->find_text_widget)),
        gschem_find_text_widget_get_find_text_string (GSCHEM_FIND_TEXT_WIDGET (w_current->find_text_widget)),
        gschem_find_text_widget_get_descend (GSCHEM_FIND_TEXT_WIDGET (w_current->find_text_widget)),
        show_hidden_text);

    if (count > 0)
    {
      x_widgets_show_find_text_state (w_current);
      close = TRUE;
    }

    break;

  case GTK_RESPONSE_CANCEL:
  case GTK_RESPONSE_DELETE_EVENT:
    close = TRUE;
    break;

  default:
    printf("x_window_find_text(): strange signal %d\n", response);
  }

  if (close) {
    gtk_widget_grab_focus (w_current->drawing_area);
    gtk_widget_hide (GTK_WIDGET (widget));
  }
}


static void
x_window_hide_text (GtkWidget *widget, gint response, GschemToplevel *w_current)
{
  LeptonPage *page = NULL;

  g_return_if_fail (w_current != NULL);

  if (response == GTK_RESPONSE_OK) {
    page = schematic_window_get_active_page (w_current);
    o_edit_hide_specific_text (w_current,
                               lepton_page_objects (page),
                               gschem_show_hide_text_widget_get_text_string (GSCHEM_SHOW_HIDE_TEXT_WIDGET (widget)));
  }

  gtk_widget_grab_focus (w_current->drawing_area);
  gtk_widget_hide (GTK_WIDGET (widget));
}


static void
x_window_show_text (GtkWidget *widget, gint response, GschemToplevel *w_current)
{
  LeptonPage *page = NULL;

  g_return_if_fail (w_current != NULL);

  if (response == GTK_RESPONSE_OK) {
    page = schematic_window_get_active_page (w_current);
    o_edit_show_specific_text (w_current,
                               lepton_page_objects (page),
                               gschem_show_hide_text_widget_get_text_string (GSCHEM_SHOW_HIDE_TEXT_WIDGET (widget)));
  }

  gtk_widget_grab_focus (w_current->drawing_area);
  gtk_widget_hide (GTK_WIDGET (widget));
}


void
x_window_select_object (GschemFindTextState *state,
                        LeptonObject *object,
                        GschemToplevel *w_current)
{
  GschemPageView *view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (view != NULL);

  LeptonPage *page = gschem_page_view_get_page (view);
  g_return_if_fail (page != NULL);

  g_return_if_fail (object != NULL);
  g_return_if_fail (object->page != NULL);

  if (page != object->page)
  {
    /* open object's page: */
    x_window_set_current_page (w_current, object->page);

    /* tabbed GUI: current page view may be different here: */
    view = gschem_toplevel_get_current_page_view (w_current);
  }

  gschem_page_view_zoom_object (view, object);
}

static void
x_window_translate_response (GschemTranslateWidget *widget, gint response, GschemToplevel *w_current)
{
  if (response == GTK_RESPONSE_OK) {
    o_component_translate_all (w_current,
                               gschem_translate_widget_get_value (widget));
  }

  i_set_state (w_current, SELECT);
  gtk_widget_grab_focus (w_current->drawing_area);
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
  main_window = GTK_WIDGET (gschem_main_window_new ());
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
 *  Creates a scrolled #GschemPageView widget in the working area
 *  \a work_box.  This function is used when tabs are disabled.
 *
 * \param w_current The #GschemToplevel object.
 * \param work_box The working area widget.
 * \return Pointer to the new GtkWidget object.
 */
GschemPageView*
schematic_window_create_page_view (GschemToplevel *w_current,
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

  return GSCHEM_PAGE_VIEW (w_current->drawing_area);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
GschemToplevel*
x_window_create_main (GtkWidget *main_window,
                      GtkWidget *main_box,
                      GtkWidget *work_box,
                      GschemToplevel *w_current)
{
  GtkWidget *hpaned = NULL;
  GtkWidget *vpaned = NULL;

  /* We want the widgets to flow around the drawing area, so we don't
   * set a size of the main window.  The drawing area's size is fixed,
   * see below
   */


  /*
  *  widgets:
  */
  x_widgets_init();
  x_widgets_create (w_current);


  /*
  *  windows layout:
  */
#ifdef ENABLE_GTK3
  vpaned = gtk_paned_new (GTK_ORIENTATION_VERTICAL);
  hpaned = gtk_paned_new (GTK_ORIENTATION_HORIZONTAL);
#else
  vpaned = gtk_vpaned_new ();
  hpaned = gtk_hpaned_new ();
#endif

  w_current->right_notebook = create_notebook_right (w_current);
  w_current->bottom_notebook = create_notebook_bottom (w_current);


  gtk_container_add (GTK_CONTAINER (main_box), vpaned);


  gtk_paned_pack1 (GTK_PANED (vpaned), hpaned,
                   TRUE, TRUE);

  gtk_paned_pack2 (GTK_PANED (vpaned), w_current->bottom_notebook,
                   FALSE, TRUE);


  gtk_paned_pack1 (GTK_PANED (hpaned), work_box,
                   TRUE, TRUE);

  gtk_paned_pack2 (GTK_PANED (hpaned), w_current->right_notebook,
                   FALSE, TRUE);


  /*
  *  status bar aka 'bottom widget':
  */
  create_bottom_widget (w_current, main_box);

  geometry_restore (main_window, w_current->find_text_state);

  /* show all widgets: */
  gtk_widget_show_all (main_window);


  if ( !x_widgets_use_docks() )
  {
    gtk_widget_set_visible (GTK_WIDGET (w_current->right_notebook),  FALSE);
    gtk_widget_set_visible (GTK_WIDGET (w_current->bottom_notebook), FALSE);
  }


  /* focus page view: */
  gtk_widget_grab_focus (w_current->drawing_area);

  w_current->main_window = main_window;

  return w_current;
} /* x_window_create_main() */



/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_window_close(GschemToplevel *w_current)
{
  gboolean last_window = FALSE;

  /* If we're closing whilst inside an action, re-wind the
   * page contents back to their state before we started */
  if (w_current->inside_action) {
    i_callback_cancel (NULL, w_current);
  }

  /* last chance to save possible unsaved pages */
  if (!x_dialog_close_window (w_current)) {
    /* user somehow cancelled the close */
    return;
  }

  x_clipboard_finish (w_current);

  w_current->dont_invalidate = TRUE;

  x_widgets_destroy_dialogs (w_current);

  /* close all the dialog boxes */
  if (w_current->cswindow)
  gtk_widget_destroy(w_current->cswindow);

  if (w_current->tiwindow)
  gtk_widget_destroy(w_current->tiwindow);

  if (w_current->aawindow)
  gtk_widget_destroy(w_current->aawindow);

  x_multiattrib_close (w_current);

  if (w_current->aewindow)
  gtk_widget_destroy(w_current->aewindow);

  if (w_current->hkwindow)
  gtk_widget_destroy(w_current->hkwindow);

  if (w_current->cowindow)
  gtk_widget_destroy(w_current->cowindow);

  if (w_current->sewindow)
  gtk_widget_destroy(w_current->sewindow);

  if (g_list_length (global_window_list) == 1) {
    /* no more window after this one, remember to quit */
    last_window = TRUE;
  }

  if (last_window)
  {
    geometry_save (w_current);
  }

  /* stuff that has to be done before we free w_current */
  if (last_window) {
    /* close the log file */
    s_log_close ();
    /* free the buffers */
    o_buffer_free (w_current);
  }

  /* finally close the main window */
  gtk_widget_destroy(w_current->main_window);

  global_window_list = g_list_remove (global_window_list, w_current);
  gschem_toplevel_free (w_current);

  /* just closed last window, so quit */
  if (last_window) {
    gschem_quit();
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_window_close_all(GschemToplevel *w_current)
{
  GschemToplevel *current;
  GList *list_copy, *iter;

  iter = list_copy = g_list_copy (global_window_list);
  while (iter != NULL ) {
    current = (GschemToplevel *)iter->data;
    iter = g_list_next (iter);
    x_window_close (current);
  }
  g_list_free (list_copy);
}



/*! \brief Opens a new page from a file.
 *  \private
 *  \par Function Description
 *  This function opens the file whose name is <B>filename</B> in a
 *  new LeptonPage of <B>toplevel</B>.
 *
 *  If there is no page for <B>filename</B> in <B>toplevel</B>'s
 *  list of pages, it creates a new LeptonPage, loads the file in
 *  it and returns a pointer on the new page. Otherwise it returns
 *  a pointer on the existing page.
 *
 *  If the filename passed is NULL, this function creates an empty,
 *  untitled page.  The name of the untitled page is build from
 *  configuration data ('untitled-name') and a counter for uniqueness.
 *
 *  The opened page becomes the current page of <B>toplevel</B>.
 *
 *  \param [in] w_current The toplevel environment.
 *  \param [in] filename The name of the file to open or NULL for a blank page.
 *  \returns A pointer on the new page.
 *
 *  \bug This code should check to make sure any untitled filename
 *  does not conflict with a file on disk.
 */
LeptonPage*
x_window_open_page_impl (GschemToplevel *w_current,
                         const gchar *filename)
{
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
  g_return_val_if_fail (toplevel != NULL, NULL);

  /* New blank page requested: */
  if (filename == NULL)
    return x_window_new_page (w_current);


  /* Return existing page if it is already loaded: */
  LeptonPage* page = lepton_toplevel_search_page (toplevel, filename);
  if (page != NULL)
    return page;


  /* Create a new page: */
  page = lepton_page_new (toplevel, filename);

  /* Switch to a new page: */
  lepton_toplevel_goto_page (toplevel, page); /* NOTE: sets current active page of toplevel */
  gschem_toplevel_page_changed (w_current);

  if (!quiet_mode)
    g_message (_("Loading schematic [%1$s]"), filename);


  /* Try to load [filename]: */
  GError* err = NULL;
  if (!schematic_file_open (w_current, page, filename, &err))
  {
    g_warning ("%s\n", err->message);
    open_page_error_dialog (w_current, filename, err);
    g_clear_error (&err);

    /* Loading failed: delete page and open a blank one: */
    lepton_page_delete (toplevel, page);
    return x_window_new_page (w_current);
  }


  /* Run hook: */
  g_run_hook_page (w_current, "open-page-hook", page);

  /* Add page file name to the recent file list: */
  recent_manager_add (w_current, filename);

  /* Save current state of the page: */
  o_undo_savestate (w_current, page, UNDO_ALL);

  return page;

} /* x_window_open_page_impl() */



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
x_window_set_current_page_impl (GschemToplevel *w_current,
                                LeptonPage *page)
{
  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  g_return_if_fail (page != NULL);

  o_redraw_cleanstates (w_current);

  gschem_page_view_set_page (page_view, page);

  i_update_menus (w_current);
  /* i_set_filename (w_current, page->page_filename); */

  page_select_widget_update (w_current);
  x_multiattrib_update (w_current);

} /* x_window_set_current_page_impl() */



/*! \brief Saves a page to a file.
 *  \par Function Description
 *  This function saves the page <B>page</B> to a file named
 *  <B>filename</B>.
 *
 *  It returns the value returned by function <B>f_save()</B> trying
 *  to save page <B>page</B> to file <B>filename</B> (1 on success, 0
 *  on failure).
 *
 *  \param [in] page      The page to save.
 *  \param [in] filename  The name of the file in which to save page.
 *  \returns 1 on success, 0 otherwise.
 */
gint
x_window_save_page (GschemToplevel *w_current,
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
    log_msg   = _("Could NOT save page [%1$s]\n");
    state_msg = _("Error while trying to save");

    GtkWidget *dialog;

    dialog = gtk_message_dialog_new (GTK_WINDOW (w_current->main_window),
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



/*! \brief Closes a page.
 *  \private
 *  \par Function Description
 *  This function closes the page <B>page</B> of toplevel
 *  <B>toplevel</B>.
 *
 *  The current page of <B>toplevel</B> is changed to
 *  the next valid page.
 *  If necessary, a new untitled page is created
 *  (unless tabbed GUI is enabled: return NULL in that case).
 *
 *  \param [in] w_current The toplevel environment.
 *  \param [in] page      The page to close.
 *  \return               Pointer to a new current LeptonPage object.
 */
LeptonPage*
x_window_close_page_impl (GschemToplevel *w_current,
                          LeptonPage *page)
{
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
  LeptonPage *new_current = NULL;
  GList *iter;

  g_return_val_if_fail (toplevel != NULL, NULL);
  g_return_val_if_fail (page     != NULL, NULL);

  g_assert (page->pid != -1);

  /* If we're closing whilst inside an action, re-wind the
   * page contents back to their state before we started */
  if (w_current->inside_action) {
    i_callback_cancel (NULL, w_current);
  }

  if (page == lepton_toplevel_get_page_current (toplevel))
  {
    /* as it will delete current page, select new current page */
    /* first look up in page hierarchy */
    new_current = lepton_toplevel_search_page_by_id (toplevel->pages, page->up);

    if (new_current == NULL) {
      /* no up in hierarchy, choice is prev, next, new page */
      iter = g_list_find( lepton_list_get_glist( toplevel->pages ), page );

      if ( g_list_previous( iter ) ) {
        new_current = (LeptonPage *)g_list_previous( iter )->data;
      } else if ( g_list_next( iter ) ) {
        new_current = (LeptonPage *)g_list_next( iter )->data;
      } else {
        /* need to add a new untitled page */
        new_current = NULL;
      }
    }
    /* new_current will be the new current page at the end of the function */
  }

  g_message (lepton_page_get_changed (page) ?
             _("Discarding page [%1$s]") : _("Closing [%1$s]"),
             lepton_page_get_filename (page));
  /* remove page from toplevel list of page and free */
  lepton_page_delete (toplevel, page);
  gschem_toplevel_page_changed (w_current);

  /* Switch to a different page if we just removed the current */
  if (lepton_toplevel_get_page_current (toplevel) == NULL)
  {

    /* Create a new page if there wasn't another to switch to */
    if (new_current == NULL && !x_tabs_enabled())
    {
      new_current = x_window_open_page_impl (w_current, NULL);
    }

    /* change to new_current and update display */
    if (!x_tabs_enabled())
    {
      x_window_set_current_page_impl (w_current, new_current);
    }

  }

  return new_current;

} /* x_window_close_page_impl() */


/*! \brief Creates and initializes a new lepton-schematic window.
 *
 * \return Pointer to the new GschemToplevel object.
 */
GschemToplevel* x_window_new (LeptonToplevel *toplevel)
{
  GschemToplevel *w_current = gschem_toplevel_new ();
  gschem_toplevel_set_toplevel (w_current, toplevel);

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
schematic_window_create_menubar (GschemToplevel *w_current,
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

  w_current->menubar = menubar;
}



static void
create_toolbar_button (GschemToplevel *w_current,
                       GtkWidget *toolbar,
                       const gchar *pixmap_name,
                       const gchar *label,
                       const gchar *tooltip,
                       GCallback callback,
                       gint pos)
{
  GtkWidget *pixmap = x_window_stock_pixmap (pixmap_name, w_current);

  GtkToolButton *button = (GtkToolButton*) gtk_tool_button_new (pixmap, label);

  gtk_widget_set_tooltip_text (GTK_WIDGET (button), tooltip);

  gtk_toolbar_insert (GTK_TOOLBAR (toolbar), GTK_TOOL_ITEM (button), pos);

  g_signal_connect (button, "clicked", callback, w_current);
}



static GtkWidget*
create_toolbar_radio_button (GSList** group,
                             GschemToplevel *w_current,
                             GtkWidget *toolbar,
                             const gchar *pixmap_name,
                             const gchar *label,
                             const gchar *tooltip,
                             GCallback callback,
                             gint pos)
{
  GtkWidget *button = GTK_WIDGET (gtk_radio_tool_button_new (*group));

  gtk_tool_button_set_label (GTK_TOOL_BUTTON (button), label);
  gtk_widget_set_tooltip_text (GTK_WIDGET (button), tooltip);

  GtkWidget *pixmap = x_window_stock_pixmap (pixmap_name, w_current);
  gtk_tool_button_set_icon_widget (GTK_TOOL_BUTTON (button), pixmap);

  gtk_toolbar_insert (GTK_TOOLBAR (toolbar), GTK_TOOL_ITEM (button), pos);

  g_signal_connect (button, "toggled", callback, w_current);

  *group = gtk_radio_tool_button_get_group (GTK_RADIO_TOOL_BUTTON (button));

  return button;
}



static void
create_toolbar_separator (GtkWidget *toolbar, gint pos)
{
  gtk_toolbar_insert (GTK_TOOLBAR (toolbar),
                      GTK_TOOL_ITEM (gtk_separator_tool_item_new ()),
                      pos);
}



void
schematic_window_create_toolbar (GschemToplevel *w_current,
                                 GtkWidget *main_box)
{
  if (w_current->toolbars == 0)
  {
    return;
  }

  GtkWidget *toolbar = gtk_toolbar_new ();

  gtk_orientable_set_orientation (GTK_ORIENTABLE (toolbar),
                                  GTK_ORIENTATION_HORIZONTAL);
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_ICONS);

#ifdef ENABLE_GTK3
  gtk_box_pack_start (GTK_BOX (main_box), toolbar, FALSE, FALSE, 0);
#else
  if (w_current->handleboxes)
  {
    GtkWidget *handlebox = gtk_handle_box_new ();
    gtk_box_pack_start (GTK_BOX (main_box), handlebox, FALSE, FALSE, 0);
    gtk_container_add (GTK_CONTAINER (handlebox), toolbar);
  }
  else
  {
    gtk_box_pack_start (GTK_BOX (main_box), toolbar, FALSE, FALSE, 0);
  }
#endif

  create_toolbar_button (w_current, toolbar,
                         "document-new", _("New"), _("New file"),
                         G_CALLBACK (&i_callback_file_new), 0);

  create_toolbar_button (w_current, toolbar,
                         "document-open", _("Open"), _("Open file"),
                         G_CALLBACK (&i_callback_file_open), 1);

  create_toolbar_button (w_current, toolbar,
                         "document-save", _("Save"), _("Save file"),
                         G_CALLBACK (&i_callback_file_save), 2);

  create_toolbar_separator (toolbar, 3);

  create_toolbar_button (w_current, toolbar,
                         "edit-undo", _("Undo"), _("Undo last operation"),
                         G_CALLBACK (&i_callback_edit_undo), 4);

  create_toolbar_button (w_current, toolbar,
                         "edit-redo", _("Redo"), _("Redo last undo"),
                         G_CALLBACK (&i_callback_edit_redo), 5);

  create_toolbar_separator (toolbar, 6);

  const gchar *text = _("Add component...\n"
                        "Select library and component from list, move the mouse into main window, click to place\n"
                        "Right mouse button to cancel");

  create_toolbar_button (w_current, toolbar,
                         "insert-symbol", _("Component"), text,
                         G_CALLBACK (&i_callback_add_component), 7);


  GSList *radio_group = NULL;

  text = _("Add nets mode\n"
           "Right mouse button to cancel");

  w_current->toolbar_net =
    create_toolbar_radio_button (&radio_group, w_current, toolbar,
                                 "insert-net", _("Nets"), text,
                                 G_CALLBACK (&i_callback_toolbar_add_net), 8);

  text = _("Add buses mode\n"
           "Right mouse button to cancel");

  w_current->toolbar_bus =
    create_toolbar_radio_button (&radio_group, w_current, toolbar,
                                 "insert-bus", _("Bus"), text,
                                 G_CALLBACK (&i_callback_toolbar_add_bus), 9);

  create_toolbar_button (w_current, toolbar,
                         "insert-text", _("Text"), _("Add Text..."),
                         G_CALLBACK (&i_callback_add_text), 10);

  create_toolbar_separator (toolbar, 11);

  w_current->toolbar_select =
    create_toolbar_radio_button (&radio_group, w_current, toolbar,
                                 "select", _("Select"), _("Select mode"),
                                 G_CALLBACK (&i_callback_toolbar_edit_select), 12);

  create_toolbar_separator (toolbar, 13);


  /* activate 'select' button at start-up */
  gtk_toggle_tool_button_set_active(
    GTK_TOGGLE_TOOL_BUTTON (w_current->toolbar_select), TRUE);

} /* schematic_window_create_toolbar() */



void
schematic_window_create_find_text_widget (GschemToplevel *w_current,
                                          GtkWidget *work_box)
{
  gpointer obj = g_object_new (GSCHEM_TYPE_FIND_TEXT_WIDGET, NULL);

  w_current->find_text_widget = GTK_WIDGET (obj);

  gtk_box_pack_start (GTK_BOX (work_box),
                      w_current->find_text_widget,
                      FALSE, FALSE, 0);

  g_signal_connect (w_current->find_text_widget, "response",
                    G_CALLBACK (&x_window_find_text), w_current);
}



void
schematic_window_create_hide_text_widget (GschemToplevel *w_current,
                                          GtkWidget *work_box)
{
  gpointer obj = g_object_new (GSCHEM_TYPE_SHOW_HIDE_TEXT_WIDGET,
                               "button-text", _("Hide"),
                               "label-text", _("Hide text starting with:"),
                               NULL);

  w_current->hide_text_widget = GTK_WIDGET (obj);

  gtk_box_pack_start (GTK_BOX (work_box),
                      w_current->hide_text_widget,
                      FALSE, FALSE, 0);

  g_signal_connect (w_current->hide_text_widget, "response",
                    G_CALLBACK (&x_window_hide_text), w_current);
}



void
schematic_window_create_show_text_widget (GschemToplevel *w_current,
                                          GtkWidget *work_box)
{
  gpointer obj = g_object_new (GSCHEM_TYPE_SHOW_HIDE_TEXT_WIDGET,
                               "button-text", _("Show"),
                               "label-text", _("Show text starting with:"),
                               NULL);

  w_current->show_text_widget = GTK_WIDGET (obj);

  gtk_box_pack_start (GTK_BOX (work_box),
                      w_current->show_text_widget,
                      FALSE, FALSE, 0);

  g_signal_connect (w_current->show_text_widget, "response",
                    G_CALLBACK (&x_window_show_text), w_current);
}



void
schematic_window_create_macro_widget (GschemToplevel *w_current,
                                      GtkWidget *work_box)
{
  w_current->macro_widget = macro_widget_new (w_current);

  gtk_box_pack_start (GTK_BOX (work_box),
                      w_current->macro_widget,
                      FALSE, FALSE, 0);
}



void
schematic_window_create_translate_widget (GschemToplevel *w_current,
                                          GtkWidget *work_box)
{
  gpointer obj = g_object_new (GSCHEM_TYPE_TRANSLATE_WIDGET, NULL);

  w_current->translate_widget = GTK_WIDGET (obj);

  gtk_box_pack_start( GTK_BOX (work_box),
                      w_current->translate_widget,
                      FALSE, FALSE, 0 );

  g_signal_connect (w_current->translate_widget, "response",
                    G_CALLBACK (&x_window_translate_response), w_current);
}



static void
create_bottom_widget (GschemToplevel *w_current, GtkWidget *main_box)
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

  gpointer obj = g_object_new (GSCHEM_TYPE_BOTTOM_WIDGET,
                               "toplevel",
                               w_current,
                               "grid-mode",
                               gschem_options_get_grid_mode (w_current->options),
                               "grid-size",
                               gschem_options_get_snap_size (w_current->options),
                               /* x_grid_query_drawn_spacing (w_current), -- occurs before the page is set */
                               "left-button-text",
                               _("Pick"),
                               "middle-button-text",
                               text_mid_button,
                               "right-button-text",
                               text_right_button,
                               "snap-mode",
                               gschem_options_get_snap_mode (w_current->options),
                               "snap-size",
                               gschem_options_get_snap_size (w_current->options),
                               "status-text",
                               _("Select Mode"),
                               "net-rubber-band-mode",
                               gschem_options_get_net_rubber_band_mode (w_current->options),
                               "magnetic-net-mode",
                               gschem_options_get_magnetic_net_mode (w_current->options),
                               NULL);

  g_free (text_right_button);

  w_current->bottom_widget = GTK_WIDGET (obj);

  gtk_box_pack_start (GTK_BOX (main_box),
                      w_current->bottom_widget,
                      FALSE, FALSE, 0);

} /* create_bottom_widget */



/*! \brief Setup scrolling parameters
 *
 *  \param [in] scrolled  Scrolled widget - a parent of page view widget
 */
void
x_window_setup_scrolling (GschemToplevel *w_current, GtkWidget *scrolled)
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

  GtkPolicyType policy = w_current->scrollbars_flag ?
                         GTK_POLICY_ALWAYS :
                         GTK_POLICY_NEVER;

  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled),
                                  policy, policy);

} /* x_window_setup_scrolling() */



static GtkWidget*
create_notebook_right (GschemToplevel* w_current)
{
  GtkWidget *notebook = gtk_notebook_new ();

  if ( x_widgets_use_docks() )
  {
    gtk_notebook_append_page (GTK_NOTEBOOK (notebook),
                              GTK_WIDGET (w_current->object_properties),
                              gtk_label_new(_("Object")));

    gtk_notebook_append_page (GTK_NOTEBOOK (notebook),
                              GTK_WIDGET (w_current->text_properties),
                              gtk_label_new(_("Text")));

    gtk_notebook_append_page (GTK_NOTEBOOK (notebook),
                              GTK_WIDGET (w_current->options_widget),
                              gtk_label_new(_("Options")));



    gtk_container_set_border_width (GTK_CONTAINER (notebook),
                                    DIALOG_BORDER_SPACING);
  }

  return notebook;
}



static GtkWidget*
create_notebook_bottom (GschemToplevel* w_current)
{
  GtkWidget *notebook = gtk_notebook_new ();

  if ( x_widgets_use_docks() )
  {
    gtk_notebook_append_page (GTK_NOTEBOOK (notebook),
                              GTK_WIDGET (w_current->find_text_state),
                              gtk_label_new(_("Find Text")));

    gtk_notebook_append_page (GTK_NOTEBOOK (notebook),
                              GTK_WIDGET (w_current->log_widget),
                              gtk_label_new(_("Log")));


    gtk_container_set_border_width (GTK_CONTAINER (notebook),
                                    DIALOG_BORDER_SPACING);
  }

  return notebook;
}



/*! \brief Opens a new page from a file or a blank one if \a filename is NULL.
 *
 *  \see x_window_open_page_impl()
 *  \see x_tabs_page_open()
 */
LeptonPage*
x_window_open_page (GschemToplevel* w_current, const gchar* filename)
{
  LeptonPage* page = NULL;

  if (x_tabs_enabled())
  {
    page = x_tabs_page_open (w_current, filename);
  }
  else
  {
    page = x_window_open_page_impl (w_current, filename);
  }

  if (filename != NULL && page != NULL)
  {
    /* check for symbol version changes, display
     * an error dialog box, if necessary:
    */
    major_changed_dialog (w_current);
  }

  return page;
}



/*! \brief Changes the current page.
 *
 *  \see x_window_set_current_page_impl()
 *  \see x_tabs_page_set_cur()
 */
void
x_window_set_current_page (GschemToplevel* w_current,
                           LeptonPage* page)
{
  if (x_tabs_enabled())
  {
    x_tabs_page_set_cur (w_current, page);
  }
  else
  {
    x_window_set_current_page_impl (w_current, page);
  }
}



/*! \brief Closes a page.
 *
 *  \see x_window_close_page_impl()
 *  \see x_tabs_page_close()
 */
void
x_window_close_page (GschemToplevel* w_current,
                     LeptonPage* page)
{
  if (x_tabs_enabled())
  {
    x_tabs_page_close (w_current, page);
  }
  else
  {
    x_window_close_page_impl (w_current, page);
  }
}



/*! \brief Create new blank page.
 *
 * \todo Do further refactoring: this function should be used
 *       instead of x_window_open_page() when a new page is reqested.
 *
 *  \param w_current The toplevel environment.
 */
static LeptonPage*
x_window_new_page (GschemToplevel* w_current)
{
  g_return_val_if_fail (w_current != NULL, NULL);

  LeptonToplevel* toplevel = gschem_toplevel_get_toplevel (w_current);
  g_return_val_if_fail (toplevel != NULL, NULL);

  /* New page file name: */
  gchar* filename = untitled_filename (w_current, TRUE);

  /* Create a new page: */
  LeptonPage* page = lepton_page_new (toplevel, filename);

  /* Switch to a new page: */
  lepton_toplevel_goto_page (toplevel, page);
  gschem_toplevel_page_changed (w_current);

  if (!quiet_mode)
    g_message (_("New file [%s]"), filename);

  g_free (filename);

  /* Run hook: */
  g_run_hook_page (w_current, "new-page-hook", page);

  /* Save current state of the page: */
  o_undo_savestate (w_current, page, UNDO_ALL);

  return page;

} /* x_window_new_page() */



/*! \brief Show "Failed to load file" dialog.
 *
 *  \param w_current The toplevel environment.
 *  \param filename  File path that failed to load.
 *  \param err       Associated GError.
 */
static void
open_page_error_dialog (GschemToplevel* w_current,
                        const gchar*    filename,
                        GError*         err)
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

  GtkWidget* dialog = gtk_message_dialog_new_with_markup
    (GTK_WINDOW (w_current->main_window),
    GTK_DIALOG_DESTROY_WITH_PARENT,
    GTK_MESSAGE_ERROR,
    GTK_BUTTONS_CLOSE,
    msg,
    filename,
    err != NULL ? err->message : "");

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
static void
recent_manager_add (GschemToplevel* w_current,
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
untitled_next_index (GschemToplevel* w_current)
{
  return ++w_current->num_untitled;
}



/*! \brief Get untitled file name.
 *  \par Function Description
 *
 * Determine "untitled" schematic file name (used for new pages)
 * and build full path from this name and current working directory.
 * When constructing this name, avoid reusing names of already opened
 * files and existing files in current directory; if \a log_skipped
 * is TRUE, report such (avoided) names to the log.
 *
 *  \param  w_current   The toplevel environment.
 *  \param  log_skipped Print skipped file names to the log.
 *  \return             Newly-allocated untitled file path.
 */
static gchar*
untitled_filename (GschemToplevel* w_current, gboolean log_skipped)
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

  LeptonToplevel* toplevel = gschem_toplevel_get_toplevel (w_current);

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
      if (log_skipped)
      {
        g_message (_("Skipping existing file [%s]"), fname);
      }

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
 * This function check if a \a page meets these conditions.
 *
 *  \param  w_current Page to check.
 *  \return           TRUE if a \a page looks like "untitled" one.
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
static void
geometry_save (GschemToplevel* w_current)
{
  gint x = 0;
  gint y = 0;
  gtk_window_get_position (GTK_WINDOW (w_current->main_window), &x, &y);

  gint width = 0;
  gint height = 0;
  gtk_window_get_size (GTK_WINDOW (w_current->main_window), &width, &height);

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
 *  If [schematic.gui]::restore-window-geometry configuration key is
 *  set to true, read main window's geometry from the CACHE config
 *  context and restore it.
 *  Unless valid configuration values are read, use default width
 *  and height.
 *
 *  \param main_window The main window widget of lepton-schematic.
 *  \param find_text_state The find text state widget.
 */
static void
geometry_restore (GtkWidget* main_window,
                  GtkWidget *find_text_state)
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
    gtk_widget_set_size_request (find_text_state,
                                 -1,
                                 height / 4);
  }
}
