/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
 * Copyright (C) 2016 Peter Brett <peter@peter-b.co.uk>
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

#include <stdio.h>

#include "gschem.h"

#define GSCHEM_THEME_ICON_NAME "geda-gschem"

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_window_setup (GschemToplevel *w_current)
{
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);

  /* immediately setup user params */
  i_vars_set(w_current);

  /* Initialize the autosave callback */
  s_page_autosave_init(toplevel);

  /* Initialize the clipboard callback */
  x_clipboard_init (w_current);

  /* Add to the list of windows */
  global_window_list = g_list_append (global_window_list, w_current);

  /* X related stuff */
  x_window_create_main (w_current);

  x_menu_attach_recent_files_submenu(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_window_create_drawing(GtkWidget *scrolled, GschemToplevel *w_current)
{
  /* drawing next */
  w_current->drawing_area = GTK_WIDGET (gschem_page_view_new_with_page (w_current->toplevel->page_current));
  /* Set the size here.  Be sure that it has an aspect ratio of 1.333
   * We could calculate this based on root window size, but for now
   * lets just set it to:
   * Width = root_width*3/4   Height = Width/1.3333333333
   * 1.3333333 is the desired aspect ratio!
   */

  gtk_widget_set_size_request (w_current->drawing_area,
                               default_width,
                               default_height);

  gtk_container_add(GTK_CONTAINER(scrolled), w_current->drawing_area);

  GTK_WIDGET_SET_FLAGS (w_current->drawing_area, GTK_CAN_FOCUS );
  gtk_widget_grab_focus (w_current->drawing_area);
  gtk_widget_show (w_current->drawing_area);
}

/*! \brief Set up callbacks for window events that affect drawing.
 *  \par Function Description
 *
 * Installs GTK+ callback handlers for signals that are emitted by
 * the drawing area, and some for the main window that affect the drawing
 * area.
 *
 * \param [in] w_current The toplevel environment.
 */
void x_window_setup_draw_events(GschemToplevel *w_current)
{
  struct event_reg_t {
    gchar *detailed_signal;
    GCallback c_handler;
  };

  struct event_reg_t drawing_area_events[] = {
    { "expose_event",         G_CALLBACK(x_event_expose)                       },
    { "expose_event",         G_CALLBACK(x_event_raise_dialog_boxes)           },
    { "button_press_event",   G_CALLBACK(x_event_button_pressed)               },
    { "button_release_event", G_CALLBACK(x_event_button_released)              },
    { "motion_notify_event",  G_CALLBACK(x_event_motion)                       },
    { "configure_event",      G_CALLBACK(x_event_configure)                    },
    { "key_press_event",      G_CALLBACK(x_event_key)                          },
    { "key_release_event",    G_CALLBACK(x_event_key)                          },
    { "scroll_event",         G_CALLBACK(x_event_scroll)                       },
    { "update-grid-info",     G_CALLBACK(i_update_grid_info_callback)          },
    { "notify::page",         G_CALLBACK(gschem_toplevel_notify_page_callback) },
    { NULL,                   NULL                                             } };
  struct event_reg_t main_window_events[] = {
    { "enter_notify_event",   G_CALLBACK(x_event_enter)              },
    { NULL,                   NULL                                   } };
  struct event_reg_t *tmp;

  /* is the configure event type missing here? hack */
  gtk_widget_set_events (w_current->drawing_area,
                         GDK_EXPOSURE_MASK |
                         GDK_POINTER_MOTION_MASK |
                         GDK_BUTTON_PRESS_MASK   |
                         GDK_ENTER_NOTIFY_MASK |
                         GDK_KEY_PRESS_MASK |
                         GDK_BUTTON_RELEASE_MASK);

  for (tmp = drawing_area_events; tmp->detailed_signal != NULL; tmp++) {
    g_signal_connect (w_current->drawing_area,
                      tmp->detailed_signal,
                      tmp->c_handler,
                      w_current);
  }

  for (tmp = main_window_events; tmp->detailed_signal != NULL; tmp++) {
    g_signal_connect (w_current->main_window,
                      tmp->detailed_signal,
                      tmp->c_handler,
                      w_current);
  }
}


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

  return wpixmap;
}


static void
x_window_find_text (GtkWidget *widget, gint response, GschemToplevel *w_current)
{
  gint close = FALSE;
  int count;

  g_return_if_fail (w_current != NULL);
  g_return_if_fail (w_current->toplevel != NULL);

  switch (response) {
  case GTK_RESPONSE_OK:
    count = gschem_find_text_state_find (
        w_current->find_text_state,
        geda_list_get_glist (w_current->toplevel->pages),
        gschem_find_text_widget_get_find_type (GSCHEM_FIND_TEXT_WIDGET (w_current->find_text_widget)),
        gschem_find_text_widget_get_find_text_string (GSCHEM_FIND_TEXT_WIDGET (w_current->find_text_widget)),
        gschem_find_text_widget_get_descend (GSCHEM_FIND_TEXT_WIDGET (w_current->find_text_widget)));
    if (count > 0) {
      /* switch the notebook page to the find results */
      int page = gtk_notebook_page_num (GTK_NOTEBOOK (w_current->bottom_notebook),
                                        GTK_WIDGET (w_current->find_text_state));

      if (page >= 0) {
        int current = gtk_notebook_get_current_page (GTK_NOTEBOOK (w_current->bottom_notebook));

        if (page != current) {
          gtk_notebook_set_current_page (GTK_NOTEBOOK (w_current->bottom_notebook), page);
        }
      }
      gtk_widget_set_visible (GTK_WIDGET (w_current->bottom_notebook), TRUE);
      close = TRUE;
    };
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
  g_return_if_fail (w_current != NULL);
  g_return_if_fail (w_current->toplevel != NULL);

  if (response == GTK_RESPONSE_OK) {
    o_edit_hide_specific_text (w_current,
                               s_page_objects (w_current->toplevel->page_current),
                               gschem_show_hide_text_widget_get_text_string (GSCHEM_SHOW_HIDE_TEXT_WIDGET (widget)));
  }

  gtk_widget_grab_focus (w_current->drawing_area);
  gtk_widget_hide (GTK_WIDGET (widget));
}


static void
x_window_show_text (GtkWidget *widget, gint response, GschemToplevel *w_current)
{
  g_return_if_fail (w_current != NULL);
  g_return_if_fail (w_current->toplevel != NULL);

  if (response == GTK_RESPONSE_OK) {
    o_edit_show_specific_text (w_current,
                               s_page_objects (w_current->toplevel->page_current),
                               gschem_show_hide_text_widget_get_text_string (GSCHEM_SHOW_HIDE_TEXT_WIDGET (widget)));
  }

  gtk_widget_grab_focus (w_current->drawing_area);
  gtk_widget_hide (GTK_WIDGET (widget));
}


static void
x_window_invoke_macro (GschemMacroWidget *widget, int response, GschemToplevel *w_current)
{
  if (response == GTK_RESPONSE_OK) {
    const char *macro = gschem_macro_widget_get_macro_string (widget);

    SCM interpreter = scm_list_2(scm_from_utf8_symbol("invoke-macro"),
                                 scm_from_utf8_string(macro));

    scm_dynwind_begin (0);
    g_dynwind_window (w_current);
    g_scm_eval_protected(interpreter, SCM_UNDEFINED);
    scm_dynwind_end ();
  }

  gtk_widget_grab_focus (w_current->drawing_area);
  gtk_widget_hide (GTK_WIDGET (widget));
}

static void
x_window_select_text (GschemFindTextState *state, OBJECT *object, GschemToplevel *w_current)
{
  GschemPageView *view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (view != NULL);

  PAGE *page = gschem_page_view_get_page (view);
  g_return_if_fail (page != NULL);

  g_return_if_fail (object != NULL);
  g_return_if_fail (object->page != NULL);

  if (page != object->page) {
    gschem_page_view_set_page (view, object->page);
  }

  gschem_page_view_zoom_text (view, object);
}

static void
x_window_translate_response (GschemTranslateWidget *widget, gint response, GschemToplevel *w_current)
{
  if (response == GTK_RESPONSE_OK) {
    o_complex_translate_all (w_current,
                             gschem_translate_widget_get_value (widget));
  }

  i_set_state (w_current, SELECT);
  gtk_widget_grab_focus (w_current->drawing_area);
  gtk_widget_hide (GTK_WIDGET (widget));
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_window_create_main(GschemToplevel *w_current)
{
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);

  GtkPolicyType policy;
  GtkWidget *main_box=NULL;
  GtkWidget *menubar=NULL;
  GtkWidget *toolbar=NULL;
  GtkWidget *handlebox=NULL;
  GtkWidget *scrolled;
  GtkAdjustment *hadjustment;
  GtkAdjustment *vadjustment;
  char *right_button_text;
  GtkWidget *hpaned;
  GtkWidget *vpaned;
  GtkWidget *work_box;
  GtkToolButton *button = NULL;

  w_current->main_window = GTK_WIDGET (gschem_main_window_new ());

  gtk_widget_set_name (w_current->main_window, "gschem");
  gtk_window_set_policy (GTK_WINDOW (w_current->main_window), TRUE, TRUE, TRUE);

  /* We want the widgets to flow around the drawing area, so we don't
   * set a size of the main window.  The drawing area's size is fixed,
   * see below
   */

   /*
    * normally we let the window manager handle locating and sizing
    * the window.  However, for some batch processing of schematics
    * (generating a pdf of all schematics for example) we want to
    * override this.  Hence "auto_place_mode".
    */
   if( auto_place_mode )
   	gtk_widget_set_uposition (w_current->main_window, 10, 10);

  /* this should work fine */
  g_signal_connect (G_OBJECT (w_current->main_window), "delete_event",
                    G_CALLBACK (i_callback_close_wm),
                    w_current);

  /* Containers first */
  main_box = gtk_vbox_new(FALSE, 1);
  gtk_container_set_border_width (GTK_CONTAINER (main_box), 0);
  gtk_container_add(GTK_CONTAINER(w_current->main_window), main_box);

  menubar = get_main_menu (w_current);
  if (w_current->handleboxes) {
  	handlebox = gtk_handle_box_new ();
  	gtk_box_pack_start(GTK_BOX(main_box), handlebox, FALSE, FALSE, 0);
  	gtk_container_add (GTK_CONTAINER (handlebox), menubar);
  } else {
  	gtk_box_pack_start(GTK_BOX(main_box), menubar, FALSE, FALSE, 0);
  }

  w_current->menubar = menubar;
  gtk_widget_realize (w_current->main_window);

  if (w_current->handleboxes && w_current->toolbars) {
  	handlebox = gtk_handle_box_new ();
  	gtk_box_pack_start (GTK_BOX (main_box), handlebox, FALSE, FALSE, 0);
  }

  if (w_current->toolbars) {
    toolbar = gtk_toolbar_new();
    gtk_orientable_set_orientation (GTK_ORIENTABLE (toolbar),
                                    GTK_ORIENTATION_HORIZONTAL);
    gtk_toolbar_set_style (GTK_TOOLBAR(toolbar), GTK_TOOLBAR_ICONS);

    if (w_current->handleboxes) {
      gtk_container_add (GTK_CONTAINER (handlebox), toolbar);
    } else {
      gtk_box_pack_start(GTK_BOX(main_box), toolbar, FALSE, FALSE, 0);
    }

    button =
      (GtkToolButton*) gtk_tool_button_new (x_window_stock_pixmap ("new", w_current),
                                            _("New"));
    gtk_widget_set_tooltip_text (GTK_WIDGET (button), _("New file"));
    gtk_toolbar_insert (GTK_TOOLBAR (toolbar), GTK_TOOL_ITEM (button), 0);
    g_signal_connect (button,
                      "clicked",
                      G_CALLBACK (i_callback_toolbar_file_new),
                      w_current);

    button =
      (GtkToolButton*) gtk_tool_button_new (x_window_stock_pixmap ("open", w_current),
                                            _("Open"));
    gtk_widget_set_tooltip_text (GTK_WIDGET (button), _("Open file"));
    gtk_toolbar_insert (GTK_TOOLBAR (toolbar), GTK_TOOL_ITEM (button), 1);
    g_signal_connect (button,
                      "clicked",
                      G_CALLBACK (i_callback_toolbar_file_open),
                      w_current);

    button =
      (GtkToolButton*) gtk_tool_button_new (x_window_stock_pixmap ("save", w_current),
                                            _("Save"));
    gtk_widget_set_tooltip_text (GTK_WIDGET (button), _("Save file"));
    gtk_toolbar_insert (GTK_TOOLBAR (toolbar), GTK_TOOL_ITEM (button), 2);
    g_signal_connect (button,
                      "clicked",
                      G_CALLBACK (i_callback_toolbar_file_save),
                      w_current);

    gtk_toolbar_insert (GTK_TOOLBAR (toolbar),
                        GTK_TOOL_ITEM (gtk_separator_tool_item_new ()), 3);

    button =
      (GtkToolButton*) gtk_tool_button_new (x_window_stock_pixmap ("undo", w_current),
                                            _("Undo"));
    gtk_widget_set_tooltip_text (GTK_WIDGET (button), _("Undo last operation"));
    gtk_toolbar_insert (GTK_TOOLBAR (toolbar), GTK_TOOL_ITEM (button), 4);
    g_signal_connect (button,
                      "clicked",
                      G_CALLBACK (i_callback_toolbar_edit_undo),
                      w_current);

    button =
      (GtkToolButton*) gtk_tool_button_new (x_window_stock_pixmap ("redo", w_current),
                                            _("Redo"));
    gtk_widget_set_tooltip_text (GTK_WIDGET (button), _("Redo last undo"));
    gtk_toolbar_insert (GTK_TOOLBAR (toolbar), GTK_TOOL_ITEM (button), 5);
    g_signal_connect (button,
                      "clicked",
                      G_CALLBACK (i_callback_toolbar_edit_redo),
                      w_current);

    gtk_toolbar_insert (GTK_TOOLBAR (toolbar),
                        GTK_TOOL_ITEM (gtk_separator_tool_item_new ()), 6);

    /* not part of any radio button group */
    button =
      (GtkToolButton*) gtk_tool_button_new (x_window_stock_pixmap ("insert-symbol", w_current),
                                            _("Component"));
    gtk_widget_set_tooltip_text (GTK_WIDGET (button),
                                 _("Add component...\n"
                                   "Select library and component from list, move the mouse into main window, click to place\n"
                                   "Right mouse button to cancel"));
    gtk_toolbar_insert (GTK_TOOLBAR (toolbar), GTK_TOOL_ITEM (button), 7);
    g_signal_connect (button,
                      "clicked",
                      G_CALLBACK (i_callback_toolbar_add_component),
                      w_current);

    /* init radio tool buttons, add first of them */
    w_current->toolbar_net = GTK_WIDGET (gtk_radio_tool_button_new (NULL));
    gtk_tool_button_set_label (GTK_TOOL_BUTTON (w_current->toolbar_net),
                               _("Nets"));
    gtk_widget_set_tooltip_text (GTK_WIDGET (w_current->toolbar_net),
                                 _("Add nets mode\n"
                                   "Right mouse button to cancel"));
    gtk_toolbar_insert (GTK_TOOLBAR (toolbar), GTK_TOOL_ITEM (w_current->toolbar_net), 8);
    gtk_tool_button_set_icon_widget (GTK_TOOL_BUTTON (w_current->toolbar_net),
                                     x_window_stock_pixmap ("insert-net", w_current));
    g_signal_connect (w_current->toolbar_net,
                      "toggled",
                      G_CALLBACK (i_callback_toolbar_add_net),
                      w_current);

    /* add a radio tool button */
    w_current->toolbar_bus = GTK_WIDGET (gtk_radio_tool_button_new (
      gtk_radio_tool_button_get_group (GTK_RADIO_TOOL_BUTTON (w_current->toolbar_net))));

    gtk_tool_button_set_label (GTK_TOOL_BUTTON (w_current->toolbar_bus),
                               _("Bus"));
    gtk_widget_set_tooltip_text (GTK_WIDGET (w_current->toolbar_bus),
                                 _("Add buses mode\n"
                                   "Right mouse button to cancel"));
    gtk_toolbar_insert (GTK_TOOLBAR (toolbar), GTK_TOOL_ITEM (w_current->toolbar_bus), 9);
    gtk_tool_button_set_icon_widget (GTK_TOOL_BUTTON (w_current->toolbar_bus),
                                     x_window_stock_pixmap ("insert-bus", w_current));
    g_signal_connect (w_current->toolbar_bus,
                      "toggled",
                      G_CALLBACK (i_callback_toolbar_add_bus),
                      w_current);

    /* not part of any radio button group */
    button =
      (GtkToolButton*) gtk_tool_button_new (x_window_stock_pixmap ("insert-text", w_current),
                                            _("Text"));
    gtk_widget_set_tooltip_text (GTK_WIDGET (button), _("Add Text..."));
    gtk_toolbar_insert (GTK_TOOLBAR (toolbar), GTK_TOOL_ITEM (button), 10);
    g_signal_connect (button,
                      "clicked",
                      G_CALLBACK (i_callback_toolbar_add_text),
                      w_current);

    gtk_toolbar_insert (GTK_TOOLBAR (toolbar),
                        GTK_TOOL_ITEM (gtk_separator_tool_item_new ()), 11);

    /* add a radio tool button */
    w_current->toolbar_select = GTK_WIDGET (gtk_radio_tool_button_new (
      gtk_radio_tool_button_get_group (GTK_RADIO_TOOL_BUTTON (w_current->toolbar_bus))));

    gtk_tool_button_set_label (GTK_TOOL_BUTTON (w_current->toolbar_select),
                               _("Select"));
    gtk_widget_set_tooltip_text (GTK_WIDGET (w_current->toolbar_select),
                                 _("Select mode"));
    gtk_toolbar_insert (GTK_TOOLBAR (toolbar), GTK_TOOL_ITEM (w_current->toolbar_select), 12);
    gtk_tool_button_set_icon_widget (GTK_TOOL_BUTTON (w_current->toolbar_select),
                                     x_window_stock_pixmap ("select", w_current));
    g_signal_connect (w_current->toolbar_select,
                      "toggled",
                      G_CALLBACK (i_callback_toolbar_edit_select),
                      w_current);

    gtk_toolbar_insert (GTK_TOOLBAR (toolbar),
                        GTK_TOOL_ITEM (gtk_separator_tool_item_new ()), 13);

    /* activate 'select' button at start-up */
    gtk_toggle_tool_button_set_active (GTK_TOGGLE_TOOL_BUTTON (w_current->toolbar_select), TRUE);
  }

  vpaned = gtk_vpaned_new ();
  gtk_container_add(GTK_CONTAINER(main_box), vpaned);

  hpaned = gtk_hpaned_new ();
  gtk_paned_pack1 (GTK_PANED (vpaned),
                   hpaned,
                   TRUE,
                   TRUE);

  work_box = gtk_vbox_new (FALSE, 0);
  gtk_paned_pack1 (GTK_PANED (hpaned),
                   work_box,
                   TRUE,
                   TRUE);

  w_current->right_notebook = gtk_notebook_new ();
  gtk_paned_pack2 (GTK_PANED (hpaned),
                   w_current->right_notebook,
                   FALSE,
                   TRUE);

  gtk_container_set_border_width (GTK_CONTAINER (w_current->right_notebook),
                                  DIALOG_BORDER_SPACING);

  /*  Try to create popup menu (appears in right mouse button  */
  w_current->popup_menu = (GtkWidget *) get_main_popup(w_current);


  /* Setup a GtkScrolledWindow for the drawing area */
  hadjustment = GTK_ADJUSTMENT (gtk_adjustment_new (0.0,
                                                    toplevel->init_left,
                                                    toplevel->init_right,
                                                    100.0,
                                                    100.0,
                                                    10.0));

  vadjustment = GTK_ADJUSTMENT (gtk_adjustment_new (toplevel->init_bottom,
                                                    0.0,
                                                    toplevel->init_bottom - toplevel->init_top,
                                                    100.0,
                                                    100.0,
                                                    10.0));

  scrolled = gtk_scrolled_window_new (hadjustment, vadjustment);
  gtk_container_add (GTK_CONTAINER (work_box), scrolled);
  x_window_create_drawing(scrolled, w_current);
  x_window_setup_draw_events(w_current);

  policy = (w_current->scrollbars_flag) ? GTK_POLICY_ALWAYS : GTK_POLICY_NEVER;
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled), policy, policy);

  /* find text box */
  w_current->find_text_widget = GTK_WIDGET (g_object_new (GSCHEM_TYPE_FIND_TEXT_WIDGET, NULL));

  gtk_box_pack_start (GTK_BOX (work_box),
                      w_current->find_text_widget,
                      FALSE,
                      FALSE,
                      0);

  g_signal_connect (w_current->find_text_widget,
                    "response",
                    G_CALLBACK (&x_window_find_text),
                    w_current);

  /* hide text box */
  w_current->hide_text_widget = GTK_WIDGET (g_object_new (GSCHEM_TYPE_SHOW_HIDE_TEXT_WIDGET,
                                                          "button-text", _("Hide"),
                                                          "label-text",  _("Hide text starting with:"),
                                                          NULL));

  gtk_box_pack_start (GTK_BOX (work_box),
                      w_current->hide_text_widget,
                      FALSE,
                      FALSE,
                      0);

  g_signal_connect (w_current->hide_text_widget,
                    "response",
                    G_CALLBACK (&x_window_hide_text),
                    w_current);

  /* show text box */
  w_current->show_text_widget = GTK_WIDGET (g_object_new (GSCHEM_TYPE_SHOW_HIDE_TEXT_WIDGET,
                                                          "button-text", _("Show"),
                                                          "label-text",  _("Show text starting with:"),
                                                          NULL));

  gtk_box_pack_start (GTK_BOX (work_box),
                      w_current->show_text_widget,
                      FALSE,
                      FALSE,
                      0);

  g_signal_connect (w_current->show_text_widget,
                    "response",
                    G_CALLBACK (&x_window_show_text),
                    w_current);

  /* macro box */
  w_current->macro_widget = GTK_WIDGET (g_object_new (GSCHEM_TYPE_MACRO_WIDGET, NULL));

  gtk_box_pack_start (GTK_BOX (work_box),
                      w_current->macro_widget,
                      FALSE,
                      FALSE,
                      0);

  g_signal_connect (w_current->macro_widget,
                    "response",
                    G_CALLBACK (&x_window_invoke_macro),
                    w_current);

  /* translate widget */
  w_current->translate_widget = GTK_WIDGET (g_object_new (GSCHEM_TYPE_TRANSLATE_WIDGET, NULL));

  gtk_box_pack_start (GTK_BOX (work_box),
                      w_current->translate_widget,
                      FALSE,
                      FALSE,
                      0);

  g_signal_connect (w_current->translate_widget,
                    "response",
                    G_CALLBACK (&x_window_translate_response),
                    w_current);

  /* object properties editor */
  w_current->object_properties = gschem_object_properties_widget_new (w_current);

  gtk_notebook_append_page (GTK_NOTEBOOK (w_current->right_notebook),
                            GTK_WIDGET (w_current->object_properties),
                            gtk_label_new (_("Object")));

  /* text properties editor */
  w_current->text_properties = gschem_text_properties_widget_new (w_current);

  gtk_notebook_append_page (GTK_NOTEBOOK (w_current->right_notebook),
                            GTK_WIDGET (w_current->text_properties),
                            gtk_label_new (_("Text")));

  /* options editor */
  w_current->options_widget = gschem_options_widget_new (w_current);

  gtk_notebook_append_page (GTK_NOTEBOOK (w_current->right_notebook),
                            GTK_WIDGET (w_current->options_widget),
                            gtk_label_new (_("Options")));

  /* status notebook */
  w_current->bottom_notebook = gtk_notebook_new ();
  gtk_paned_pack2 (GTK_PANED (vpaned),
                   w_current->bottom_notebook,
                   FALSE,
                   TRUE);

  gtk_container_set_border_width (GTK_CONTAINER (w_current->bottom_notebook),
                                  DIALOG_BORDER_SPACING);

  w_current->find_text_state = gschem_find_text_state_new ();

  gtk_notebook_append_page (GTK_NOTEBOOK (w_current->bottom_notebook),
                            GTK_WIDGET (w_current->find_text_state),
                            gtk_label_new (_("Find Text")));

  gtk_widget_set_size_request (GTK_WIDGET (w_current->find_text_state),
                               default_width,
                               default_height / 4);

  g_signal_connect (w_current->find_text_state,
                    "select-object",
                    G_CALLBACK (&x_window_select_text),
                    w_current);


  w_current->log_widget = gschem_log_widget_new ();

  gtk_notebook_append_page (GTK_NOTEBOOK (w_current->bottom_notebook),
                            GTK_WIDGET (w_current->log_widget),
                            gtk_label_new (_("Status")));

  /* bottom box */
  if (default_third_button == POPUP_ENABLED) {
    right_button_text = _("Menu/Cancel");
  } else {
    right_button_text = _("Pan/Cancel");
  }

  w_current->bottom_widget = GTK_WIDGET (g_object_new (GSCHEM_TYPE_BOTTOM_WIDGET,
      "grid-mode",          gschem_options_get_grid_mode (w_current->options),
      "grid-size",          gschem_options_get_snap_size (w_current->options), /* x_grid_query_drawn_spacing (w_current), -- occurs before the page is set */
      "left-button-text",   _("Pick"),
      "middle-button-text", _("none"),
      "right-button-text",  right_button_text,
      "snap-mode",          gschem_options_get_snap_mode (w_current->options),
      "snap-size",          gschem_options_get_snap_size (w_current->options),
      "status-text",        _("Select Mode"),
      NULL));

  i_update_middle_button (w_current, NULL, NULL);

  gtk_box_pack_start (GTK_BOX (main_box), w_current->bottom_widget, FALSE, FALSE, 0);

  gtk_widget_show_all (w_current->main_window);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_window_close(GschemToplevel *w_current)
{
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
  gboolean last_window = FALSE;

  /* If we're closing whilst inside an action, re-wind the
   * page contents back to their state before we started */
  if (w_current->inside_action) {
    i_callback_cancel (w_current, 0, NULL);
  }

  /* last chance to save possible unsaved pages */
  if (!x_dialog_close_window (w_current)) {
    /* user somehow cancelled the close */
    return;
  }

  x_clipboard_finish (w_current);

#if DEBUG
  o_conn_print_hash(w_current->page_current->conn_table);
#endif

  w_current->dont_invalidate = TRUE;

  /* close all the dialog boxes */
  if (w_current->sowindow)
  gtk_widget_destroy(w_current->sowindow);

  if (w_current->cswindow)
  gtk_widget_destroy(w_current->cswindow);

  if (w_current->tiwindow)
  gtk_widget_destroy(w_current->tiwindow);

  if (w_current->aawindow)
  gtk_widget_destroy(w_current->aawindow);

  x_multiattrib_close (w_current);

  if (w_current->aewindow)
  gtk_widget_destroy(w_current->aewindow);

  x_pagesel_close (w_current);

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

  if (toplevel->major_changed_refdes) {
    GList* current = toplevel->major_changed_refdes;
    while (current)
    {
      /* printf("yeah freeing: %s\n", (char*) current->data); */
      g_free(current->data);
      current = g_list_next(current);
    }
    g_list_free(toplevel->major_changed_refdes);
  }

  /* stuff that has to be done before we free w_current */
  if (last_window) {
    /* close the log file */
    s_log_close ();
    /* free the buffers */
    o_buffer_free (w_current);
  }

  /* Allow Scheme value for this window to be garbage-collected */
  if (!scm_is_eq (w_current->smob, SCM_UNDEFINED)) {
    SCM_SET_SMOB_DATA (w_current->smob, NULL);
    scm_gc_unprotect_object (w_current->smob);
    w_current->smob = SCM_UNDEFINED;
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
 *  \par Function Description
 *  This function opens the file whose name is <B>filename</B> in a
 *  new PAGE of <B>toplevel</B>.
 *
 *  If there is no page for <B>filename</B> in <B>toplevel</B>'s list
 *  of pages, it creates a new PAGE, loads the file in it and returns
 *  a pointer on the new page. Otherwise it returns a pointer on the
 *  existing page.
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
PAGE*
x_window_open_page (GschemToplevel *w_current, const gchar *filename)
{
  PAGE *page;
  gchar *fn;

  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
  g_return_val_if_fail (toplevel != NULL, NULL);

  /* Generate untitled filename if none was specified */
  if (filename == NULL) {
    gchar *cwd, *tmp, *untitled_name;
    EdaConfig *cfg;
    cwd = g_get_current_dir ();
    cfg = eda_config_get_context_for_path (cwd);
    untitled_name = eda_config_get_string (cfg, "gschem", "default-filename", NULL);
    tmp = g_strdup_printf ("%s_%d.sch",
                           untitled_name,
                           ++w_current->num_untitled);
    fn = g_build_filename (cwd, tmp, NULL);
    g_free (untitled_name);
    g_free(cwd);
    g_free(tmp);
  } else {
    fn = g_strdup (filename);
  }

  /* Return existing page if it is already loaded */
  page = s_page_search (toplevel, fn);
  if ( page != NULL ) {
    g_free(fn);
    return page;
  }

  page = s_page_new (toplevel, fn);
  s_page_goto (toplevel, page);
  gschem_toplevel_page_changed (w_current);

  /* Load from file if necessary, otherwise just print a message */
  if (filename != NULL) {
    GError *err = NULL;
    if (!quiet_mode)
      s_log_message (_("Loading schematic [%s]\n"), fn);

    if (!f_open (toplevel, page, (gchar *) fn, &err)) {
      GtkWidget *dialog;

      g_warning ("%s\n", err->message);
      dialog = gtk_message_dialog_new_with_markup
        (GTK_WINDOW (w_current->main_window),
         GTK_DIALOG_DESTROY_WITH_PARENT,
         GTK_MESSAGE_ERROR,
         GTK_BUTTONS_CLOSE,
         _("<b>An error occurred while loading the requested file.</b>\n\nLoading from '%s' failed: %s. The gschem log may contain more information."),
         fn, err->message);
      gtk_window_set_title (GTK_WINDOW (dialog), _("Failed to load file"));
      gtk_dialog_run (GTK_DIALOG (dialog));
      gtk_widget_destroy (dialog);
      g_error_free (err);
    } else {
      gtk_recent_manager_add_item (recent_manager, g_filename_to_uri(fn, NULL, NULL));
    }
  } else {
    if (!quiet_mode)
      s_log_message (_("New file [%s]\n"),
                     toplevel->page_current->page_filename);

    g_run_hook_page (w_current, "%new-page-hook", toplevel->page_current);
  }

  o_undo_savestate (w_current, toplevel->page_current, UNDO_ALL);

  g_free (fn);

  return page;
}

/*! \brief Changes the current page.
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
x_window_set_current_page (GschemToplevel *w_current, PAGE *page)
{
  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  g_return_if_fail (page != NULL);

  o_redraw_cleanstates (w_current);

  gschem_page_view_set_page (page_view, page);

  i_update_menus (w_current);
  /* i_set_filename (w_current, page->page_filename); */

  x_pagesel_update (w_current);
  x_multiattrib_update (w_current);
}

/*! \brief Saves a page to a file.
 *  \par Function Description
 *  This function saves the page <B>page</B> to a file named
 *  <B>filename</B>.
 *
 *  It returns the value returned by function <B>f_save()</B> trying
 *  to save page <B>page</B> to file <B>filename</B> (1 on success, 0
 *  on failure).
 *
 *  <B>page</B> may not be the current page of <B>toplevel</B>. The
 *  current page of <B>toplevel</B> is not affected by this function.
 *
 *  \param [in] w_current The toplevel environment.
 *  \param [in] page      The page to save.
 *  \param [in] filename  The name of the file in which to save page.
 *  \returns 1 on success, 0 otherwise.
 */
gint
x_window_save_page (GschemToplevel *w_current, PAGE *page, const gchar *filename)
{
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
  const gchar *log_msg, *state_msg;
  gint ret;
  GError *err = NULL;

  g_return_val_if_fail (toplevel != NULL, 0);
  g_return_val_if_fail (page     != NULL, 0);
  g_return_val_if_fail (filename != NULL, 0);

  /* try saving page to filename */
  ret = (gint)f_save (toplevel, page, filename, &err);

  if (ret != 1) {
    log_msg   = _("Could NOT save page [%s]\n");
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
    if (g_ascii_strcasecmp (page->page_filename, filename) != 0) {
      g_free (page->page_filename);
      page->page_filename = g_strdup (filename);

      log_msg = _("Saved as [%s]\n");
    } else {
      log_msg = _("Saved [%s]\n");
    }
    state_msg = _("Saved");

    /* reset page CHANGED flag */
    page->CHANGED = 0;

    /* add to recent file list */
    gtk_recent_manager_add_item (recent_manager, g_filename_to_uri(filename, NULL, NULL));

    /* i_set_filename (w_current, page->page_filename); */
    x_pagesel_update (w_current);
  }

  /* log status of operation */
  s_log_message (log_msg, filename);

  i_set_state_msg  (w_current, SELECT, state_msg);

  return ret;
}

/*! \brief Closes a page.
 *  \par Function Description
 *  This function closes the page <B>page</B> of toplevel
 *  <B>toplevel</B>.
 *
 *  If necessary, the current page of <B>toplevel</B> is changed to
 *  the next valid page or to a new untitled page.
 *
 *  \param [in] w_current The toplevel environment.
 *  \param [in] page      The page to close.
 */
void
x_window_close_page (GschemToplevel *w_current, PAGE *page)
{
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
  PAGE *new_current = NULL;
  GList *iter;

  g_return_if_fail (toplevel != NULL);
  g_return_if_fail (page     != NULL);

  g_assert (page->pid != -1);

  /* If we're closing whilst inside an action, re-wind the
   * page contents back to their state before we started */
  if (w_current->inside_action) {
    i_callback_cancel (w_current, 0, NULL);
  }

  if (page == toplevel->page_current) {
    /* as it will delete current page, select new current page */
    /* first look up in page hierarchy */
    new_current = s_page_search_by_page_id (toplevel->pages, page->up);

    if (new_current == NULL) {
      /* no up in hierarchy, choice is prev, next, new page */
      iter = g_list_find( geda_list_get_glist( toplevel->pages ), page );

      if ( g_list_previous( iter ) ) {
        new_current = (PAGE *)g_list_previous( iter )->data;
      } else if ( g_list_next( iter ) ) {
        new_current = (PAGE *)g_list_next( iter )->data;
      } else {
        /* need to add a new untitled page */
        new_current = NULL;
      }
    }
    /* new_current will be the new current page at the end of the function */
  }

  s_log_message (page->CHANGED ?
                 _("Discarding page [%s]\n") : _("Closing [%s]\n"),
                 page->page_filename);
  /* remove page from toplevel list of page and free */
  s_page_delete (toplevel, page);
  gschem_toplevel_page_changed (w_current);

  /* Switch to a different page if we just removed the current */
  if (toplevel->page_current == NULL) {

    /* Create a new page if there wasn't another to switch to */
    if (new_current == NULL) {
      new_current = x_window_open_page (w_current, NULL);
    }

    /* change to new_current and update display */
    x_window_set_current_page (w_current, new_current);
  }
}


/*! \brief Setup default icon for GTK windows
 *
 *  \par Function Description
 *  Sets the default window icon by name, to be found in the current icon
 *  theme. The name used is \#defined above as GSCHEM_THEME_ICON_NAME.
 */
void x_window_set_default_icon( void )
{
  gtk_window_set_default_icon_name( GSCHEM_THEME_ICON_NAME );
}

/*! \brief Setup icon search paths.
 * \par Function Description
 * Add the icons installed by gschem to the search path for the
 * default icon theme, so that they can be automatically found by GTK.
 */
void
x_window_init_icons (void)
{
	/* System icons */
	/* FIXME this shouldn't be necessary; gschem should just install its
	 * icons in the system hicolor icon theme and they'll be picked up
	 * automatically. */
	const gchar * const * sys_dirs = eda_get_system_data_dirs();
	for (gint i = 0; sys_dirs[i]; ++i) {
		gchar *icon_dir;
		icon_dir = g_build_filename (sys_dirs[i], "icons", NULL);
		gtk_icon_theme_append_search_path (gtk_icon_theme_get_default(),
		                                   icon_dir);
		g_free (icon_dir);
	}
}

/*! \brief Creates a new X window.
 *
 * \par Function description
 *
 * Creates and initializes new GschemToplevel object and then sets
 * and setups its libgeda \a toplevel.
 *
 * \param toplevel The libgeda TOPLEVEL object.
 * \return Pointer to the new GschemToplevel object.
 */
GschemToplevel* x_window_new (TOPLEVEL *toplevel)
{
  GschemToplevel *w_current;

  w_current = gschem_toplevel_new ();
  gschem_toplevel_set_toplevel (w_current,
                                (toplevel != NULL) ? toplevel : s_toplevel_new ());

  gschem_toplevel_get_toplevel (w_current)->load_newer_backup_func = x_fileselect_load_backup;
  gschem_toplevel_get_toplevel (w_current)->load_newer_backup_data = w_current;

  o_text_set_rendered_bounds_func (gschem_toplevel_get_toplevel (w_current),
                                   o_text_get_rendered_bounds, w_current);

  /* Damage notifications should invalidate the object on screen */
  o_add_change_notify (gschem_toplevel_get_toplevel (w_current),
                       (ChangeNotifyFunc) o_invalidate,
                       (ChangeNotifyFunc) o_invalidate, w_current);

  x_window_setup (w_current);

  return w_current;
}
