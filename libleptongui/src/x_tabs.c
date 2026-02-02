/* Lepton EDA Schematic Capture
 * Copyright (C) 2018 dmn <graahnul.grom@gmail.com>
 * Copyright (C) 2018-2026 Lepton EDA Contributors
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

/*!
 *
 * \file x_tabs.c
 *
 * \brief Tabbed GUI support
 *
 * The code in this file makes it possible to display each schematic
 * in its own tab within GtkNotebook widget in the main window.
 * It maintains 1:1 relationship between page and page view objects
 * by intercepting requests to open, close, and set current page.
 * When switching between tabs, current page view
 * (SchematicWindow::drawing_area) is updated to point to a
 * page view appropriate for the current page.
 * Pages and corresponding page view objects are stored as a list
 * of TabInfo structures (SchematicWindow::xtabs_info_list).
 * GtkNotebook widget is stored in SchematicWindow::xtabs_nbook.
 *
 * Public interface:
 *
 * - x_tabs_enabled()      // whether tabbed GUI is currently enabled
 * - x_tabs_init()         // initialize tabbed GUI, read config
 * - x_tabs_hdr_update()   // update tab's header widget
 *
 * The behaviour is controlled by the following
 * configuration settings:
 *
 * 1) Enable/disable tabbed GUI:
 * key:         use-tabs
 * group:       schematic.gui
 * type:        boolean
 * default val: true
 *
 * 2) Whether to show "close" button on tabs:
 * key:         show-close-button
 * group:       schematic.tabs
 * type:        boolean
 * default val: true
 *
 * 3) Whether to show "hierarchy up" button on tabs:
 * key:         show-up-button
 * group:       schematic.tabs
 * type:        boolean
 * default val: true
 *
 * 4) Whether to show tabs tooltips:
 * key:         show-tooltips
 * group:       schematic.tabs
 * type:        boolean
 * default val: true
 *
 */

#include "config.h"

#include "schematic.h"




static gboolean g_x_tabs_enabled;

static gboolean g_x_tabs_show_close_button;

static gboolean g_x_tabs_show_up_button;

static gboolean g_x_tabs_show_tooltips;



/*! \brief Whether tabbed GUI is enabled.
 *  \public
 */
gboolean
x_tabs_enabled()
{
  return g_x_tabs_enabled;
}



static gboolean
x_tabs_show_close_button()
{
  return g_x_tabs_show_close_button;
}



static gboolean
x_tabs_show_up_button()
{
  return g_x_tabs_show_up_button;
}



static gboolean
x_tabs_show_tooltips()
{
  return g_x_tabs_show_tooltips;
}


/*! \brief Initialize tabbed GUI; read configuration
 *  \public
 *
 *  \par Function Description
 *  Call this function before any other functions in this file.
*/
void
x_tabs_init()
{
  cfg_read_bool ("schematic.gui", "use-tabs",
                 default_tabs_enabled, &g_x_tabs_enabled);

  cfg_read_bool ("schematic.tabs", "show-close-button",
                 default_tabs_show_close_button, &g_x_tabs_show_close_button);

  cfg_read_bool ("schematic.tabs", "show-up-button",
                 default_tabs_show_up_button, &g_x_tabs_show_up_button);

  cfg_read_bool ("schematic.tabs", "show-tooltips",
                 default_tabs_show_tooltips, &g_x_tabs_show_tooltips);
} /* x_tabs_init() */




/* --------------------------------------------------------
 *
 * forward declarations:
 *
 */

/* tab info: */

static gint
x_tabs_info_cmp_page (gconstpointer elem, gconstpointer data);

static gint
x_tabs_info_cmp_pview (gconstpointer elem, gconstpointer data);

static TabInfo*
x_tabs_info_find_by_pview (GList* nfos, SchematicCanvas* pview);



/* notebook: */

static GtkMenu*
x_tabs_menu_create (TabInfo* nfo);
static void
x_tabs_menu_create_item (SchematicWindow* toplevel,
                         GtkWidget*      menu,
                         const gchar*    action_name,
                         const gchar*    action_label,
                         const gchar*    icon_name);
static void
x_tabs_menu_create_item_separ (GtkWidget* menu);

#ifdef ENABLE_GTK3
static void
x_tabs_menu_action_on_activate (GSimpleAction* action,
                                GVariant *parameter,
                                gpointer data);
static void
x_tabs_menu_item_on_activate (GtkMenuItem *item,
                              gpointer data);
#else /* GTK2 */
static void
x_tabs_menu_action_on_activate (GtkAction* action,
                                gpointer data);
#endif



/* Callbacks */

static GCallback callback_hierarchy_up = NULL;
static GCallback callback_page_close = NULL;
static GCallback callback_file_save = NULL;

void
schematic_tabs_set_callback (char *name,
                             GCallback callback)
{
  g_return_if_fail (name != NULL);
  g_return_if_fail (callback != NULL);

  if (strcmp (name, "page-close") == 0) {callback_page_close = callback;}
  else if (strcmp (name, "hierarchy-up") == 0) {callback_hierarchy_up = callback;}
  else if (strcmp (name, "file-save") == 0) {callback_file_save = callback;}
}


/* --------------------------------------------------------
 *
 * implementation: tab info:
 *
 */

/*! \brief Get current tab info.
*/
TabInfo*
x_tabs_info_cur (SchematicWindow* w_current)
{
  SchematicCanvas* pview = schematic_window_get_current_canvas (w_current);
  GList *info_list = schematic_window_get_tab_info_list (w_current);
  TabInfo* nfo = x_tabs_info_find_by_pview (info_list, pview);
  return nfo;
}



TabInfo*
x_tabs_info_add (SchematicWindow* w_current,
                 gint            ndx,
                 LeptonPage*     page,
                 SchematicCanvas* pview,
                 GtkWidget*      wtab)
{
  TabInfo* nfo = (TabInfo*) g_malloc (sizeof (TabInfo));

  nfo->ndx_   = ndx;

  nfo->page_  = page;
  nfo->pview_ = pview;
  nfo->wtab_  = wtab;
  nfo->tl_    = w_current;

  GList *current_info_list =
    schematic_window_get_tab_info_list (w_current);
  GList *new_info_list = g_list_append (current_info_list, nfo);
  schematic_window_set_tab_info_list (w_current, new_info_list);

  return nfo;
}



static gint
x_tabs_info_cmp_page (gconstpointer elem, gconstpointer data)
{
  TabInfo*    nfo  = (TabInfo*)    elem;
  LeptonPage* page = (LeptonPage*) data;

  if (nfo->page_ == page)
    return 0;

  return 1;
}



static gint
x_tabs_info_cmp_pview (gconstpointer elem, gconstpointer data)
{
  TabInfo*        nfo   = (TabInfo*)        elem;
  SchematicCanvas* pview = (SchematicCanvas*) data;

  if (nfo->pview_ == pview)
    return 0;

  return 1;
}



TabInfo*
x_tabs_info_find_by_page (GList* nfos,
                          LeptonPage* page)
{
  GList* ptr = g_list_find_custom (nfos,
                                   (gconstpointer) page,
                                   &x_tabs_info_cmp_page);
  return ptr ? (TabInfo*) ptr->data : NULL;
}



static TabInfo*
x_tabs_info_find_by_pview (GList* nfos,
                           SchematicCanvas* pview)
{
  GList* ptr = g_list_find_custom (nfos,
                                   (gconstpointer) pview,
                                   &x_tabs_info_cmp_pview);
  return ptr ? (TabInfo*) ptr->data : NULL;
}



/* --------------------------------------------------------
 *
 * implementation: notebook:
 *
 */

GtkWidget*
x_tabs_nbook_create (SchematicWindow* w_current,
                     GtkWidget* work_box)
{
  GtkWidget* nbook = gtk_notebook_new();
  GtkNotebook *notebook = GTK_NOTEBOOK (nbook);

  schematic_window_set_tab_notebook (w_current, notebook);

  gtk_container_add (GTK_CONTAINER (work_box), nbook);

  gtk_notebook_set_scrollable (notebook, TRUE);

  /* TODO: [ask folks]: configurable tabs position:
  *
  * gtk_notebook_set_tab_pos (notebook, GTK_POS_BOTTOM);
  *
  */

  gtk_widget_set_name (nbook, "lepton-nbook");

#ifdef ENABLE_GTK3
  GtkStyleContext *context;
  GtkCssProvider *provider;

  const char* style_string =
    "#lepton-nbook tab {\n"
    "  min-width: 0;\n"
    "  border-color: #777777;\n"
    "  border-top-left-radius: 10px;\n"
    "  border-top-right-radius: 10px;\n"
    "  border-style: outset;\n"
    "}\n";

  context = gtk_widget_get_style_context (nbook);
  provider = gtk_css_provider_new ();

  gtk_css_provider_load_from_data (GTK_CSS_PROVIDER (provider),
                                   style_string,
                                   -1,
                                   NULL);
  gtk_style_context_add_provider (context,
                                  GTK_STYLE_PROVIDER (provider),
                                  GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
  g_object_unref (provider);

#else
  /* wider horizontal space:
  */
  gtk_rc_parse_string
  (
    "style \"lepton-nbook-style\"\n"
    "{\n"
    "  xthickness = 0\n"
    "}\n"
    "\n"
    "widget \"*.lepton-nbook\" style \"lepton-nbook-style\""
  );
#endif

  return nbook;

} /* x_tabs_nbook_create() */



/* --------------------------------------------------------
 *
 * implementation: page view:
 *
 */


void
schematic_tabs_add_canvas (SchematicCanvas *pview,
                           GtkWidget *wtab)
{
#ifdef ENABLE_GTK3
  gtk_widget_set_hexpand (GTK_WIDGET (pview), TRUE);
  gtk_widget_set_vexpand (GTK_WIDGET (pview), TRUE);
  gtk_widget_set_halign (GTK_WIDGET (pview), GTK_ALIGN_FILL);
  gtk_widget_set_valign (GTK_WIDGET (pview), GTK_ALIGN_FILL);
#endif

  gtk_container_add (GTK_CONTAINER (wtab), GTK_WIDGET (pview));
  gtk_widget_show_all (wtab);

  gtk_widget_set_can_focus (GTK_WIDGET (pview), TRUE);

} /* schematic_tabs_add_canvas() */




/* --------------------------------------------------------
 *
 * implementation: tab header widget:
 *
 */

GtkWidget*
x_tabs_hdr_create (TabInfo* nfo)
{
  g_return_val_if_fail (nfo != NULL, NULL);
  g_return_val_if_fail (nfo->page_ != NULL, NULL);

#ifdef ENABLE_GTK3
  GtkWidget* box_hdr        = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
  GtkWidget* box_btns_left  = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
  GtkWidget* box_lab        = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
  GtkWidget* box_btns_right = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
  GtkWidget* box_hdr        = gtk_hbox_new (FALSE, 0);
  GtkWidget* box_btns_left  = gtk_hbox_new (FALSE, 0);
  GtkWidget* box_lab        = gtk_hbox_new (FALSE, 0);
  GtkWidget* box_btns_right = gtk_hbox_new (FALSE, 0);
#endif


  /* label:
  */
  const gchar* fname = lepton_page_get_filename (nfo->page_);
  g_return_val_if_fail (fname != NULL, NULL);

  gchar* bname = g_path_get_basename (fname);
  gchar* lab_txt = NULL;

  if (lepton_page_get_changed (nfo->page_))
    lab_txt = g_strdup_printf ("<b>%s</b>", bname);
  else
    lab_txt = g_strdup (bname);

  GtkWidget* lab = gtk_label_new (NULL);
  gtk_widget_set_name (lab, "lepton-tab-label" );
  gtk_label_set_markup (GTK_LABEL (lab), lab_txt);
  gtk_box_pack_start (GTK_BOX (box_lab), lab, TRUE, TRUE, 0);

  g_free (bname);
  g_free (lab_txt);


  /* tab's tooltip:
  */
  if (x_tabs_show_tooltips())
  {
    /* the full path of the schematic file:
    */
    gtk_widget_set_tooltip_text (box_hdr, fname);
  }

  /* "close" btn:
  */
  GtkWidget* btn_close = gtk_button_new();
  gtk_widget_set_name (btn_close, "lepton-tab-btn" );
  gtk_button_set_relief (GTK_BUTTON (btn_close), GTK_RELIEF_NONE);
#ifdef ENABLE_GTK3
  gtk_widget_set_focus_on_click (btn_close, FALSE);
#else /* GTK2 */
  gtk_button_set_focus_on_click (GTK_BUTTON (btn_close), FALSE);
#endif


#ifdef ENABLE_GTK3
  GtkStyleContext *context;
  GtkCssProvider *provider;

  const char* style_string =
    "#lepton-tab-label {\n"
    "  padding-right: 5px;\n"
    "}\n";

  context = gtk_widget_get_style_context (lab);
  provider = gtk_css_provider_new ();

  gtk_css_provider_load_from_data (GTK_CSS_PROVIDER (provider),
                                   style_string,
                                   -1,
                                   NULL);
  gtk_style_context_add_provider (context,
                                  GTK_STYLE_PROVIDER (provider),
                                  GTK_STYLE_PROVIDER_PRIORITY_APPLICATION);
  g_object_unref (provider);

#else
  /* make tab btns smaller => smaller tabs:
  */
  gtk_rc_parse_string
  (
    "style \"lepton-tab-btn-style\"\n"
    "{\n"
    "  xthickness = 0\n"
    "  ythickness = 0\n"
    "  GtkWidget::focus-padding = 0\n"
    "  GtkWidget::focus-line-width = 0\n"
    "}\n"
    "\n"
    "widget \"*.lepton-tab-btn\" style \"lepton-tab-btn-style\""
  );
#endif

  GtkWidget* img_close = gtk_image_new_from_icon_name ("window-close",
                                                       GTK_ICON_SIZE_MENU);
  gtk_container_add (GTK_CONTAINER (btn_close), img_close);
  gtk_widget_set_tooltip_text (btn_close, _("Close"));


  /* "up" btn:
  */
  GtkWidget* btn_up = gtk_button_new();
  gtk_widget_set_name (btn_up, "lepton-tab-btn");
  gtk_button_set_relief (GTK_BUTTON (btn_up), GTK_RELIEF_NONE);
#ifdef ENABLE_GTK3
  gtk_widget_set_focus_on_click (btn_up, FALSE);
#else /* GTK2 */
  gtk_button_set_focus_on_click (GTK_BUTTON (btn_up), FALSE);
#endif

  GtkWidget* img_up = gtk_image_new_from_icon_name ("go-up",
                                                    GTK_ICON_SIZE_MENU);
  gtk_container_add (GTK_CONTAINER (btn_up), img_up);


  /* "save" btn:
  */
  GtkWidget* btn_save = gtk_button_new();
  gtk_widget_set_name (btn_save, "lepton-tab-btn");
  gtk_button_set_relief (GTK_BUTTON (btn_save), GTK_RELIEF_NONE);
#ifdef ENABLE_GTK3
  gtk_widget_set_focus_on_click (btn_save, FALSE);
#else /* GTK2 */
  gtk_button_set_focus_on_click (GTK_BUTTON (btn_save), FALSE);
#endif

  GtkWidget* img_save = gtk_image_new_from_icon_name ("document-save",
                                                      GTK_ICON_SIZE_MENU);
  gtk_container_add (GTK_CONTAINER (btn_save), img_save);
  gtk_widget_set_tooltip_text (btn_save, _("Save"));


  /* pack button boxes and label box to hdr box:
  */
  gtk_box_pack_start (GTK_BOX (box_hdr), box_btns_left,  FALSE, FALSE, 0);
  gtk_box_pack_start (GTK_BOX (box_hdr), box_lab,        TRUE,  TRUE,  0);
  gtk_box_pack_start (GTK_BOX (box_hdr), box_btns_right, FALSE, FALSE, 0);


  /* setup "save" btn:
  */
  if (lepton_page_get_changed (nfo->page_))
  {
    gtk_box_pack_end (GTK_BOX (box_btns_left), btn_save, FALSE, FALSE, 0);

    g_signal_connect (btn_save,
                      "clicked",
                      G_CALLBACK (callback_file_save),
                      nfo);
  }


  /* setup "up" btn:
  */
  LeptonPage* parent = s_hierarchy_find_up_page (nfo->page_);

  if (x_tabs_show_up_button() && parent != NULL)
  {
    const gchar* parent_fname = lepton_page_get_filename (parent);
    gchar*       parent_bname = NULL;
    gchar*       ttip_btn_up  = NULL;

    if (parent_fname)
      parent_bname = g_path_get_basename (parent_fname);

    if (parent_bname)
      ttip_btn_up = g_strdup_printf (_("Hierarchy up: %s"), parent_bname);

    gtk_widget_set_tooltip_text (btn_up,
                                 ttip_btn_up ? ttip_btn_up : _("Hierarchy up"));

    g_free (parent_bname);
    g_free (ttip_btn_up);

    gtk_box_pack_start (GTK_BOX (box_btns_left), btn_up, FALSE, FALSE, 0);

    g_signal_connect (btn_up,
                      "clicked",
                      G_CALLBACK (callback_hierarchy_up),
                      nfo);

  } /* if: show_btn_up && parent */


  /* setup "close" btn:
  */
  if (x_tabs_show_close_button())
  {
    gtk_box_pack_start (GTK_BOX (box_btns_right), btn_close, FALSE, FALSE, 0);

    g_signal_connect (btn_close,
                      "clicked",
                      G_CALLBACK (callback_page_close),
                      nfo);
  }


  gtk_widget_show_all (box_hdr);

  return box_hdr;

} /* x_tabs_hdr_create() */



/*! \brief Creates hdr widget, sets it as tab's label.
 */
void
x_tabs_hdr_set (GtkNotebook* nbook, TabInfo* nfo)
{
  g_return_if_fail (nbook != NULL);
  g_return_if_fail (nfo != NULL);

  GtkWidget* hdr = x_tabs_hdr_create (nfo);

  GtkWidget* ebox = gtk_event_box_new();
  gtk_event_box_set_visible_window (GTK_EVENT_BOX (ebox), FALSE);
  gtk_container_add (GTK_CONTAINER (ebox), hdr);
  gtk_widget_show_all (ebox);

  g_signal_connect (ebox,
                    "button-press-event",
                    G_CALLBACK (&x_tabs_hdr_on_mouse_click),
                    nfo);

  gtk_notebook_set_tab_label (nbook, nfo->wtab_, ebox);
}



/*! \brief Updates the tab's header for a given \a page.
 *  \public
 *
 *  \par Function Description
 *  For now, it simply recreates the header.
 */
void
x_tabs_hdr_update (SchematicWindow* w_current,
                   LeptonPage* page)
{
  g_return_if_fail (w_current != NULL);
  g_return_if_fail (page != NULL);

  TabInfo* nfo = x_tabs_info_find_by_page (schematic_window_get_tab_info_list (w_current),
                                           page);
  g_return_if_fail (nfo != NULL);

  x_tabs_hdr_set (schematic_window_get_tab_notebook (w_current), nfo);
}


SchematicWindow*
schematic_tab_info_get_window (TabInfo *tab_info)
{
  g_return_val_if_fail (tab_info != NULL, NULL);

  return tab_info->tl_;
}


LeptonPage*
schematic_tab_info_get_page (TabInfo *tab_info)
{
  g_return_val_if_fail (tab_info != NULL, NULL);

  return tab_info->page_;
}

void
schematic_tab_info_set_page (TabInfo *tab_info,
                             LeptonPage* page)
{
  g_return_if_fail (tab_info != NULL);

  tab_info->page_ = page;
}


SchematicCanvas*
schematic_tab_info_get_canvas (TabInfo *tab_info)
{
  g_return_val_if_fail (tab_info != NULL, NULL);

  return tab_info->pview_;
}


void
schematic_tab_info_set_canvas (TabInfo *tab_info,
                               SchematicCanvas *canvas)
{
  g_return_if_fail (tab_info != NULL);

  tab_info->pview_ = canvas;
}


GtkWidget*
schematic_tab_info_get_tab_widget (TabInfo *tab_info)
{
  g_return_val_if_fail (tab_info != NULL, NULL);

  return tab_info->wtab_;
}


void
schematic_tab_info_set_tab_widget (TabInfo *tab_info,
                                   GtkWidget *tab_widget)
{
  g_return_if_fail (tab_info != NULL);

  tab_info->wtab_ = tab_widget;
}


/* --------------------------------------------------------
 *
 * implementation: core and public functions:
 *
 */


/*! \brief Create popup menu for tab's header.
 */
static GtkMenu*
x_tabs_menu_create (TabInfo* nfo)
{
  g_return_val_if_fail (nfo != NULL, NULL);

  SchematicWindow* tl = nfo->tl_;
  g_return_val_if_fail (tl != NULL, NULL);

  GtkWidget* menu = gtk_menu_new();
  x_tabs_menu_create_item (tl, menu, "&file-new", _("_New"), "gtk-new");
  x_tabs_menu_create_item (tl, menu, "&file-open", _("_Open..."), "gtk-open");
  x_tabs_menu_create_item_separ (menu);
  x_tabs_menu_create_item (tl, menu, "&file-save", _("_Save"), "gtk-save");
  x_tabs_menu_create_item (tl, menu, "&file-save-as", _("Save _As..."), "gtk-save-as");
  x_tabs_menu_create_item_separ (menu);
  x_tabs_menu_create_item (tl, menu, "&page-manager", _("Page _Manager..."), NULL);
  x_tabs_menu_create_item_separ (menu);
  x_tabs_menu_create_item (tl, menu, "&page-close", _("_Close"), "gtk-close");

  gtk_widget_show_all (menu);
  return GTK_MENU (menu);

} /* x_tabs_menu_create() */



/*! \brief Tab's header widget "button-press-event" signal handler.
 *  \todo  Consider switching to clicked tab
 */
gboolean
x_tabs_hdr_on_mouse_click (GtkWidget* hdr, GdkEvent* e, gpointer data)
{
  g_return_val_if_fail (data != NULL, FALSE);

  TabInfo* nfo    = (TabInfo*) data;
  TabInfo* nfocur = x_tabs_info_cur (nfo->tl_);

  /* show menu for current tab only:
  */
  if (nfo != nfocur)
  {
    return FALSE;
  }

#ifdef DEBUG
  printf ("p: [%s]\n",   g_path_get_basename (lepton_page_get_filename(nfo->page_)));
  printf ("C: [%s]\n\n", g_path_get_basename (lepton_page_get_filename(nfocur->page_)));
#endif

  GdkEventButton* ebtn = (GdkEventButton*) e;

  if (ebtn->type == GDK_BUTTON_PRESS && ebtn->button == 3) /* 3: RMB */
  {
    GtkMenu* menu = x_tabs_menu_create (nfo);
    gtk_menu_attach_to_widget (menu, hdr, NULL);

#ifdef ENABLE_GTK3
    gtk_menu_popup_at_pointer (menu, e);
#else
    int etime = gtk_get_current_event_time();
    gtk_menu_popup (menu, NULL, NULL, NULL, NULL, ebtn->button, etime);
#endif

    return TRUE;
  }

  return FALSE; /* FALSE => propagate the event further */

} /* x_tabs_page_on_mouse_click() */



/*! \brief "activate" signal handler for context menu item action.
 */
#ifdef ENABLE_GTK3
static void
x_tabs_menu_action_on_activate (GSimpleAction* action,
                                GVariant *parameter,
                                gpointer data)
{
  SchematicWindow* toplevel    = (SchematicWindow*) data;
  const gchar*    action_name = g_action_get_name (G_ACTION (action));

  g_action_eval_by_name (toplevel, action_name);
}

static void
x_tabs_menu_item_on_activate (GtkMenuItem *item,
                              gpointer data)
{
  g_signal_emit_by_name (G_ACTION (data), "activate", NULL);
}

#else /* GTK2 */

static void
x_tabs_menu_action_on_activate (GtkAction* action,
                                gpointer data)
{
  SchematicWindow* toplevel    = (SchematicWindow*) data;
  const gchar*    action_name = gtk_action_get_name (action);

  g_action_eval_by_name (toplevel, action_name);
}
#endif



/*! \brief Create and add popup menu item separator.
 */
static void
x_tabs_menu_create_item_separ (GtkWidget* menu)
{
  gtk_menu_shell_append (GTK_MENU_SHELL (menu),
                         gtk_separator_menu_item_new());
}



/*! \brief Create and add popup menu item.
 */
static void
x_tabs_menu_create_item (SchematicWindow* toplevel,
                         GtkWidget*      menu,
                         const gchar*    action_name,
                         const gchar*    action_label,
                         const gchar*    icon_name)
{
#ifdef ENABLE_GTK3
  GSimpleAction* action = g_simple_action_new (action_name, NULL);

  GtkWidget* item = gtk_menu_item_new_with_mnemonic (action_label);

  g_signal_connect (item,
                    "activate",
                    G_CALLBACK (&x_tabs_menu_item_on_activate),
                    action);
#else /* GTK2 */

  SchematicAction* action = schematic_action_new (action_name,  /* name */
                                                  action_label, /* label */
                                                  NULL,         /* tooltip */
                                                  icon_name,    /* stock_id */
                                                  NULL);        /* multikey_accel */

  GtkWidget* item = gtk_action_create_menu_item (GTK_ACTION (action));
#endif

  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);

  g_signal_connect (action,
                    "activate",
                    G_CALLBACK (&x_tabs_menu_action_on_activate),
                    toplevel);

} /* x_tabs_menu_create_item() */
