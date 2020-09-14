/* Lepton EDA Schematic Capture
 * Copyright (C) 2018 dmn <graahnul.grom@gmail.com>
 * Copyright (C) 2018-2022 Lepton EDA Contributors
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
 * (GschemToplevel::drawing_area) is updated to point to a
 * page view appropriate for the current page.
 * Pages and corresponding page view objects are stored as a list
 * of TabInfo structures (GschemToplevel::xtabs_info_list).
 * GtkNotebook widget is stored in GschemToplevel::xtabs_nbook.
 *
 * Public interface:
 *
 * - x_tabs_enabled()      // whether tabbed GUI is currently enabled
 * - x_tabs_init()         // initialize tabbed GUI, read config
 * - x_tabs_create()       // create notebook
 * - x_tabs_page_open()    // open tab
 * - x_tabs_page_set_cur() // set current tab
 * - x_tabs_page_close()   // close tab
 * - x_tabs_next()         // go to next tab
 * - x_tabs_prev()         // go to prev tab
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

#include "gschem.h"




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
 * Tab data:
 *
 */

/*!
 *  \struct _TabInfo
 *  \brief Represents relationship between page, page view, tab widget
 */
struct _TabInfo
{

  gint            ndx_;   /* just for debugging; will be removed */

  LeptonPage*     page_;
  GschemPageView* pview_;
  GtkWidget*      wtab_;  /* tab widget, i.e. scrolled wnd, parent of pview_ */

  GschemToplevel* tl_;

};

typedef struct _TabInfo TabInfo;




/* --------------------------------------------------------
 *
 * forward declarations:
 *
 */

/* tab info: */

static TabInfo*
x_tabs_info_cur (GschemToplevel* w_current);

static TabInfo*
x_tabs_info_add (GschemToplevel* w_current,
                 gint            ndx,
                 LeptonPage*     page,
                 GschemPageView* pview,
                 GtkWidget*      wtab);

static void
x_tabs_info_rm (GschemToplevel* w_current, TabInfo* nfo);

static gint
x_tabs_info_cmp_page (gconstpointer elem, gconstpointer data);

static gint
x_tabs_info_cmp_pview (gconstpointer elem, gconstpointer data);

static gint
x_tabs_info_cmp_wtab (gconstpointer elem, gconstpointer data);

static TabInfo*
x_tabs_info_find_by_page (GList* nfos,
                          LeptonPage* page);
static TabInfo*
x_tabs_info_find_by_pview (GList* nfos, GschemPageView* pview);

static TabInfo*
x_tabs_info_find_by_wtab (GList* nfos, GtkWidget* wtab);



/* GschemToplevel accessors: */

static LeptonPage*
x_tabs_tl_page_cur (GschemToplevel* w_current);

static void
x_tabs_tl_page_cur_set (GschemToplevel* w_current,
                        LeptonPage* page);
static GschemPageView*
x_tabs_tl_pview_cur (GschemToplevel* w_current);

static void
x_tabs_tl_pview_cur_set (GschemToplevel* w_current, GschemPageView* pview);

static gboolean
x_tabs_tl_page_find (GschemToplevel* w_current,
                     LeptonPage* page);


/* notebook: */

static void
x_tabs_nbook_create (GschemToplevel* w_current, GtkWidget* work_box);

static gint
x_tabs_nbook_page_add (GschemToplevel* w_current,
                       LeptonPage*     page,
                       GschemPageView* pview,
                       GtkWidget*      wtab);

static void
x_tabs_nbook_page_close (GschemToplevel* w_current,
                         LeptonPage* page);
static void
x_tabs_page_on_sel (GtkNotebook* nbook,
                    GtkWidget*   wtab,
                    guint        ndx,
                    gpointer     data);

static void
x_tabs_page_on_reordered (GtkNotebook* nbook,
                          GtkWidget*   wtab,
                          guint        newindex,
                          gpointer     data);


static gboolean
x_tabs_hdr_on_mouse_click (GtkWidget* hdr, GdkEvent* e, gpointer data);
static GtkMenu*
x_tabs_menu_create (TabInfo* nfo);
static void
x_tabs_menu_create_item (GschemToplevel* toplevel,
                         GtkWidget*      menu,
                         const gchar*    action_name,
                         const gchar*    action_label,
                         const gchar*    icon_name);
static void
x_tabs_menu_create_item_separ (GtkWidget* menu);
static void
x_tabs_menu_item_on_activate (GtkAction* action, gpointer data);



/* page view: */

static GschemPageView*
x_tabs_pview_create (GschemToplevel* w_current,
                     LeptonPage*     page,
                     GtkWidget**     ppwtab);

/*
 *
 * NOTE: for now: nop
 *
 * static void
 * x_tabs_pview_rm (GschemPageView* pview);
 *
*/



/* tab header widget: */

static GtkWidget*
x_tabs_hdr_create (TabInfo* nfo);

static void
x_tabs_hdr_set (GtkNotebook* nbook, TabInfo* nfo);

static void
x_tabs_hdr_on_btn_close (GtkToolButton* btn, gpointer p);

static void
x_tabs_hdr_on_btn_up (GtkToolButton* btn, gpointer p);

static void
x_tabs_hdr_on_btn_save (GtkToolButton* btn, gpointer p);



/* helpers: */

static void
x_tabs_cancel_all (GschemToplevel* w_current);

static void
x_tabs_hier_up (GschemToplevel* w_current);




/* --------------------------------------------------------
 *
 * debug:
 *
 */

#ifdef DEBUG

static void
x_tabs_dbg_pview_geom_dump (GschemToplevel* w_current, const gchar* prefix)
{
  GschemPageView* pv = gschem_toplevel_get_current_page_view( w_current );
  g_return_if_fail( pv != NULL );

  GschemPageGeometry* geometry = gschem_page_view_get_page_geometry( pv );
  g_return_if_fail( geometry != NULL );

  printf( "%s: pv: %p, SH: %d, SW: %d\n",
          prefix,
          (void*) pv,
          geometry->screen_height,
          geometry->screen_width );
}

static void
x_tabs_dbg_info_dump (const TabInfo* nfo)
{
  printf( "\n" );

  printf( "    ndx:      [%d]\n",    nfo->ndx_ );
  printf( "    page->up:    [%d]\n", nfo->page_->up );
  printf( "    page:        [%p]\n", (void*) nfo->page_ );
  printf( "    pview::_page [%p]\n", (void*) nfo->pview_->_page );
  printf( "    pview:       [%p]\n", (void*) nfo->pview_ );
  printf( "    wtab:        [%p]\n", (void*) nfo->wtab_ );

  printf( "\n" );
}

void
x_tabs_dbg_infos_dump (GschemToplevel* w_current)
{
  printf( "\n vvvvvvvvvvvvvv nfos vvvvvvvvvvvvvvvvvv\n" );

  TabInfo* nfo = NULL;

  for ( GList* node = w_current->xtabs_info_list;
        node != NULL;
        node = g_list_next( node ) )
  {
    nfo = node->data;
    x_tabs_dbg_info_dump( nfo );
  }

  printf( " ^^^^^^^^^^^^^^ nfos ^^^^^^^^^^^^^^^^^^\n\n" );
}

void
x_tabs_dbg_pages_dump (GschemToplevel* w_current)
{
  GList* ptr = lepton_list_get_glist( w_current->toplevel->pages );

  printf( "\n vvvvvvvvvvvvvv pages vvvvvvvvvvvvvvvvvv\n" );

  for ( ; ptr != NULL; ptr = g_list_next( ptr ) )
  {
    LeptonPage* page = (LeptonPage*) ptr->data;
    printf( "\n" );
    printf( "    page:  [%p]\n", (void*) page );
    printf( "    fname: [%s]\n", page->_filename );
    printf( "    up:    [%d]\n", page->up );
    printf( "    ctl:   [%d]\n", page->page_control );
    printf( "\n" );
  }

  printf( " ^^^^^^^^^^^^^^ pages ^^^^^^^^^^^^^^^^^^\n\n" );
}

static void
x_tabs_dbg_pages_dump_simple (GschemToplevel* w_current)
{
  printf( " >> pages:\n" );
  g_return_if_fail( w_current != NULL );

  for ( GList* node = w_current->toplevel->pages->glist;
        node != NULL;
        node = g_list_next( node ) )
  {
    LeptonPage* p = node->data;
    printf ("    p: [%s]\n",
            g_path_get_basename (lepton_page_get_filename (p)));
  }

  printf( "\n" );
}

#endif /* DEBUG */




/* --------------------------------------------------------
 *
 * implementation: tab info:
 *
 */

/*! \brief Get current tab info.
*/
static TabInfo*
x_tabs_info_cur (GschemToplevel* w_current)
{
  GschemPageView* pview = x_tabs_tl_pview_cur (w_current);
  TabInfo* nfo = x_tabs_info_find_by_pview(w_current->xtabs_info_list, pview);
  return nfo;
}



static TabInfo*
x_tabs_info_add (GschemToplevel* w_current,
                 gint            ndx,
                 LeptonPage*     page,
                 GschemPageView* pview,
                 GtkWidget*      wtab)
{
  TabInfo* nfo = (TabInfo*) g_malloc (sizeof (TabInfo));

  nfo->ndx_   = ndx;

  nfo->page_  = page;
  nfo->pview_ = pview;
  nfo->wtab_  = wtab;
  nfo->tl_    = w_current;

  w_current->xtabs_info_list = g_list_append (w_current->xtabs_info_list, nfo);

  return nfo;
}



static void
x_tabs_info_rm (GschemToplevel* w_current, TabInfo* nfo)
{
  GList* node = g_list_find (w_current->xtabs_info_list, nfo);

  g_return_if_fail (node != NULL);

  if (node != NULL)
  {
    w_current->xtabs_info_list = g_list_delete_link (w_current->xtabs_info_list,
                                                     node);
    g_free (nfo);
  }
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
  GschemPageView* pview = (GschemPageView*) data;

  if (nfo->pview_ == pview)
    return 0;

  return 1;
}



static gint
x_tabs_info_cmp_wtab (gconstpointer elem, gconstpointer data)
{
  TabInfo*   nfo  = (TabInfo*)   elem;
  GtkWidget* wtab = (GtkWidget*) data;

  if (nfo->wtab_ == wtab)
    return 0;

  return 1;
}



static TabInfo*
x_tabs_info_find_by_page (GList* nfos,
                          LeptonPage* page)
{
  GList* ptr = g_list_find_custom (nfos,
                                   (gconstpointer) page,
                                   &x_tabs_info_cmp_page);
  return ptr ? (TabInfo*) ptr->data : NULL;
}



static TabInfo*
x_tabs_info_find_by_pview (GList* nfos, GschemPageView* pview)
{
  GList* ptr = g_list_find_custom (nfos,
                                   (gconstpointer) pview,
                                   &x_tabs_info_cmp_pview);
  return ptr ? (TabInfo*) ptr->data : NULL;
}



static TabInfo*
x_tabs_info_find_by_wtab (GList* nfos, GtkWidget* wtab)
{
  GList* ptr = g_list_find_custom (nfos,
                                   (gconstpointer) wtab,
                                   &x_tabs_info_cmp_wtab);
  return ptr ? (TabInfo*) ptr->data : NULL;
}




/* --------------------------------------------------------
 *
 * implementation: GschemToplevel accessors:
 *
 */

static LeptonPage*
x_tabs_tl_page_cur (GschemToplevel* w_current)
{
  return schematic_window_get_active_page (w_current);
}



static void
x_tabs_tl_page_cur_set (GschemToplevel* w_current,
                        LeptonPage* page)
{
  lepton_toplevel_goto_page (w_current->toplevel, page);

  /* NOTE: gschem_toplevel_page_changed() after
   * lepton_toplevel_goto_page():
  */
  gschem_toplevel_page_changed (w_current);
}



static GschemPageView*
x_tabs_tl_pview_cur (GschemToplevel* w_current)
{
  GtkWidget*      wview = w_current->drawing_area;
  GschemPageView* view  = GSCHEM_PAGE_VIEW (wview);

  return view;
}



static void
x_tabs_tl_pview_cur_set (GschemToplevel* w_current, GschemPageView* pview)
{
  w_current->drawing_area = GTK_WIDGET (pview);
}



/*! \brief Find a \a page in the list of loaded pages.
 *
 *  \return TRUE if found.
 *
 */
static gboolean
x_tabs_tl_page_find (GschemToplevel* w_current,
                     LeptonPage* page)
{
  GList* ptr = lepton_list_get_glist (w_current->toplevel->pages);

  for ( ; ptr != NULL; ptr = g_list_next (ptr) )
  {
    LeptonPage* pg = (LeptonPage*) ptr->data;
    if (pg == page)
      return TRUE;
  }

  return FALSE;

} /* x_tabs_tl_page_find() */




/* --------------------------------------------------------
 *
 * implementation: notebook:
 *
 */

static void
x_tabs_nbook_create (GschemToplevel* w_current, GtkWidget* work_box)
{
  GtkWidget* nbook = gtk_notebook_new();

  w_current->xtabs_nbook = GTK_NOTEBOOK (nbook);

  gtk_container_add (GTK_CONTAINER (work_box), nbook);

  gtk_notebook_set_scrollable (w_current->xtabs_nbook, TRUE);

  /* TODO: [ask folks]: configurable tabs position:
  *
  * gtk_notebook_set_tab_pos (w_current->xtabs_nbook, GTK_POS_BOTTOM);
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

  g_signal_connect (nbook, "switch-page",
                    G_CALLBACK (&x_tabs_page_on_sel), w_current);

  g_signal_connect (nbook,
                    "page-reordered",
                    G_CALLBACK (&x_tabs_page_on_reordered),
                    w_current);

} /* x_tabs_nbook_create() */



static gint
x_tabs_nbook_page_add (GschemToplevel* w_current,
                       LeptonPage*     page,
                       GschemPageView* pview,
                       GtkWidget*      wtab)
{
#ifdef DEBUG
  printf( "x_tabs_nbook_page_add()\n" );
#endif

  gint ndx = gtk_notebook_append_page (w_current->xtabs_nbook,
                                       wtab,
                                       NULL);

  return ndx;

} /* x_tabs_nbook_page_add() */



static void
x_tabs_nbook_page_close (GschemToplevel* w_current,
                         LeptonPage* page)
{
  TabInfo* nfo = x_tabs_info_find_by_page (w_current->xtabs_info_list, page);
  if (!nfo)
  {
    return;
  }

  gint ndx = gtk_notebook_page_num (w_current->xtabs_nbook, nfo->wtab_);

  gtk_notebook_remove_page (w_current->xtabs_nbook, ndx);

} /* x_tabs_nbook_page_close() */




/* --------------------------------------------------------
 *
 * implementation: page view:
 *
 */


static GschemPageView*
x_tabs_pview_create (GschemToplevel* w_current,
                     LeptonPage*     page,
                     GtkWidget**     ppwtab)
{
#ifdef DEBUG
  printf( "x_tabs_pview_create(): page: %p\n", page );
#endif

  *ppwtab = gtk_scrolled_window_new (NULL, NULL);
  GtkWidget* wtab = *ppwtab;

  x_window_setup_scrolling (w_current, wtab);

  GschemPageView* pview = gschem_page_view_new_with_page (page);

#ifdef ENABLE_GTK3
  gtk_widget_set_hexpand (GTK_WIDGET (pview), TRUE);
  gtk_widget_set_vexpand (GTK_WIDGET (pview), TRUE);
  gtk_widget_set_halign (GTK_WIDGET (pview), GTK_ALIGN_FILL);
  gtk_widget_set_valign (GTK_WIDGET (pview), GTK_ALIGN_FILL);
#endif

  gtk_container_add (GTK_CONTAINER (wtab), GTK_WIDGET (pview));
  gtk_widget_show_all (wtab);

  gtk_widget_set_can_focus (GTK_WIDGET (pview), TRUE);

  x_window_setup_draw_events_drawing_area (w_current, pview);

  return pview;

} /* x_tabs_pview_create() */



/*
*
* static void
* x_tabs_pview_rm (GschemPageView* pview)
* {
* }
*
*/




/* --------------------------------------------------------
 *
 * implementation: tab header widget:
 *
 */

static GtkWidget*
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
  gtk_button_set_focus_on_click (GTK_BUTTON (btn_close), FALSE);


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
  gtk_button_set_focus_on_click (GTK_BUTTON (btn_up), FALSE);

  GtkWidget* img_up = gtk_image_new_from_icon_name ("go-up",
                                                    GTK_ICON_SIZE_MENU);
  gtk_container_add (GTK_CONTAINER (btn_up), img_up);


  /* "save" btn:
  */
  GtkWidget* btn_save = gtk_button_new();
  gtk_widget_set_name (btn_save, "lepton-tab-btn");
  gtk_button_set_relief (GTK_BUTTON (btn_save), GTK_RELIEF_NONE);
  gtk_button_set_focus_on_click (GTK_BUTTON (btn_save), FALSE);

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
                      G_CALLBACK (&x_tabs_hdr_on_btn_save),
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
                      G_CALLBACK (&x_tabs_hdr_on_btn_up),
                      nfo);

  } /* if: show_btn_up && parent */


  /* setup "close" btn:
  */
  if (x_tabs_show_close_button())
  {
    gtk_box_pack_start (GTK_BOX (box_btns_right), btn_close, FALSE, FALSE, 0);

    g_signal_connect (btn_close,
                      "clicked",
                      G_CALLBACK (&x_tabs_hdr_on_btn_close),
                      nfo);
  }


  gtk_widget_show_all (box_hdr);

  return box_hdr;

} /* x_tabs_hdr_create() */



/*! \brief Creates hdr widget, sets it as tab's label.
 */
static void
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
x_tabs_hdr_update (GschemToplevel* w_current,
                   LeptonPage* page)
{
  g_return_if_fail (w_current != NULL);
  g_return_if_fail (page != NULL);

  TabInfo* nfo = x_tabs_info_find_by_page (w_current->xtabs_info_list,
                                           page);
  g_return_if_fail (nfo != NULL);

  x_tabs_hdr_set (w_current->xtabs_nbook, nfo);
}



static void
x_tabs_hdr_on_btn_close (GtkToolButton* btn, gpointer p)
{
  TabInfo* nfo = (TabInfo*) p;
  g_return_if_fail (nfo != NULL);

  if (nfo != NULL)
  {
    x_tabs_page_set_cur (nfo->tl_, nfo->page_);

    if (lepton_page_get_changed (nfo->page_))
    {
      if (!x_dialog_close_changed_page (nfo->tl_, nfo->page_))
      {
        return;
      }
    }

    x_tabs_page_close (nfo->tl_, nfo->page_);
  }

} /* x_tabs_hdr_on_btn_close() */



static void
x_tabs_hdr_on_btn_up (GtkToolButton* btn, gpointer p)
{
  TabInfo* nfo = (TabInfo*) p;
  g_return_if_fail (nfo != NULL);

  if (nfo != NULL)
  {
    x_tabs_page_set_cur (nfo->tl_, nfo->page_);
    x_tabs_hier_up (nfo->tl_);
  }

} /* x_tabs_hdr_on_btn_up() */



static void
x_tabs_hdr_on_btn_save (GtkToolButton* btn, gpointer p)
{
  TabInfo* nfo = (TabInfo*) p;
  g_return_if_fail (nfo != NULL);

  x_tabs_page_set_cur (nfo->tl_, nfo->page_);
  g_action_eval_by_name (nfo->tl_, "&file-save");

} /* x_tabs_hdr_on_btn_save() */




/* --------------------------------------------------------
 *
 * implementation: helpers:
 *
 */

/*! \brief Cancels all actions.
 *
 * \par Function Description
 * Cancel all actions that may be in progress
 * (e.g. move, component placement, etc.)
 * and return to default "SELECT" state.
 *
 * \note
 * Code taken from i_callback_cancel()
 *
*/
static void
x_tabs_cancel_all (GschemToplevel* w_current)
{
  if (w_current->event_state == COMPMODE && w_current->cswindow)
  {
    o_place_invalidate_rubber (w_current, FALSE);
    w_current->rubber_visible = 0;

    x_compselect_deselect (w_current);

    GValue value = G_VALUE_INIT;
    g_value_init (&value, G_TYPE_BOOLEAN);
    g_value_set_boolean (&value, FALSE);
    g_object_set_property (G_OBJECT (w_current->cswindow), "hidden", &value);
  }

  if (w_current->inside_action)
  {
    o_move_cancel (w_current);
  }

  if (w_current->event_state == GRIPS)
  {
    o_grips_cancel (w_current);
  }

  LeptonPage *active_page = schematic_window_get_active_page (w_current);
  if (active_page != NULL)
  {
    lepton_object_list_delete (active_page->place_list);
    active_page->place_list = NULL;
  }

  i_set_state (w_current, SELECT);

  schematic_keys_reset (w_current);

  GschemPageView* pview = gschem_toplevel_get_current_page_view (w_current);
  gschem_page_view_invalidate_all (pview);

  i_action_stop (w_current);

} /* x_tabs_cancel_all() */



/*! \brief Go to the upper hierarchy level page.
 *
 * \note
 * Code taken from i_callback_hierarchy_up()
 *
*/
static void
x_tabs_hier_up (GschemToplevel* w_current)
{
  LeptonPage* page = schematic_window_get_active_page (w_current);

  if (page == NULL)
  {
    return;
  }

  LeptonPage* parent = s_hierarchy_find_up_page (page);

  if (parent == NULL)
  {
    g_message (_("Cannot find any schematics above the current one!"));
    return;
  }

  if (lepton_page_get_changed (page))
  {
    if (!x_dialog_close_changed_page (w_current, page))
    {
      return;
    }
  }

  x_tabs_page_close (w_current, page);

  x_tabs_page_set_cur (w_current, parent);

} /* x_tabs_hier_up() */




/* --------------------------------------------------------
 *
 * implementation: core and public functions:
 *
 */


/*! \brief Switch to the next tab.
 *  \public
 *
 *  \todo: [ask folks]: (configurable?) cyclic tab change:
 *
 *  \param [in] w_current  The toplevel environment.
 */
void
x_tabs_next (GschemToplevel* w_current)
{
  if (!x_tabs_enabled())
    return;

  g_return_if_fail (w_current != NULL);

  gtk_notebook_next_page (w_current->xtabs_nbook);
}



/*! \brief Switch to the previous tab.
 *  \public
 *
 *  \todo: [ask folks]: (configurable?) cyclic tab change:
 *
 *  \param [in] w_current  The toplevel environment.
 */
void
x_tabs_prev (GschemToplevel* w_current)
{
  if (!x_tabs_enabled())
      return;

  g_return_if_fail (w_current != NULL);

  gtk_notebook_prev_page (w_current->xtabs_nbook);
}



/*! \brief Creates page view, TabInfo, adds a tab to the notebook.
 *
 *  \par Function Description
 *  After calling this function it may be necessary to
 *  wait to let page view creation to complete, for example:
 *
 *  while ( gtk_events_pending() )
 *  {
 *      gtk_main_iteration();
 *  }
 *
 *  \param  [in] w_current  The toplevel environment.
 *  \param  [in] page       The page.
 *  \return                 A pointer to the new TabInfo structure.
 */
static TabInfo*
x_tabs_page_new (GschemToplevel* w_current,
                 LeptonPage* page)
{
#ifdef DEBUG
  printf( "x_tabs_new_page(): page: %p\n", page);
#endif

  GtkWidget* wtab = NULL;
  GschemPageView* pview = x_tabs_pview_create (w_current, page, &wtab);
  x_tabs_tl_pview_cur_set (w_current, pview);
  gint ndx = x_tabs_nbook_page_add (w_current, page, pview, wtab);

  gtk_notebook_set_tab_reorderable (w_current->xtabs_nbook, wtab, TRUE);

  return x_tabs_info_add (w_current, ndx, page, pview, wtab);

} /* x_tabs_page_new() */



/*! \brief Creates notebook and initial tab.
 *  \public
 *
 *  \param [in] w_current  The toplevel environment.
 *  \param [in] work_box   Container widget for the notebook.
 */
void
x_tabs_create (GschemToplevel* w_current, GtkWidget* work_box)
{
  g_return_if_fail (w_current != NULL);
  g_return_if_fail (work_box != NULL);

  x_tabs_nbook_create (w_current, work_box);
  x_tabs_page_new (w_current, NULL);

} /* x_tabs_create() */



/*! \brief Opens a tab.
 *  \public
 *
 *  \par Function Description
 *  Create a new page, page view and tab for \a filename.
 *  If \a filename == NULL, the page will be blank.
 *  If \a page is already opened, switch to its tab.
 *
 *  \note
 *  The code is intentionally left unrefactored.
 *
 *  \param [in] w_current  The toplevel environment.
 *  \param [in] filename   File name or NULL.
 *  \return                The page opened.
 *
 */
LeptonPage*
x_tabs_page_open (GschemToplevel* w_current, const gchar* filename)
{
  g_return_val_if_fail (w_current != NULL, NULL);

#ifdef DEBUG
  printf( "x_tabs_page_open(): fn: %s\n", filename );
#endif

  /*
   * Find TabInfo for a page view that is set as current
   * for toplevel (w_current->drawing_area):
  */
  TabInfo* nfo_cur = x_tabs_info_cur (w_current);

  g_return_val_if_fail (nfo_cur != NULL && "no current TabInfo found", NULL);

  /* XXX: 1: [pview] [!page] - either:
   * - first blank page upon startup
   * - first cmd-line supplied page upon startup
   * - new page when the last page is closed
  */
  if (nfo_cur->page_ == NULL)
  {
#ifdef DEBUG
    printf( "    x_tabs_page_open(): #1: [pview] [!page], fn: [%s] \n\n", filename );
#endif

    nfo_cur->page_ = x_window_open_page_impl (w_current, filename);
    x_window_set_current_page_impl (w_current, nfo_cur->page_);

    x_tabs_hdr_set (w_current->xtabs_nbook, nfo_cur);
    gtk_widget_grab_focus (GTK_WIDGET (nfo_cur->pview_));
    return nfo_cur->page_;
  }


  LeptonPage* page = NULL;
  if (filename != NULL)
    page = lepton_toplevel_search_page (w_current->toplevel, filename);


  /* XXX: 2: [!pview] [!page] - either:
   * - file->open page
   * - file->new page
  */
  if (page == NULL)
  {
#ifdef DEBUG
    printf( "    x_tabs_page_open(): #2: [!pview] [!page] \n\n" );
#endif

    /* cancel all actions;
     * this prevents assertion triggering in o_place_invalidate_rubber()
     * if File->New is invoked while component selection mode is active:
    */
    x_tabs_cancel_all (w_current);

    TabInfo* nfo_new = x_tabs_page_new (w_current, NULL);

    nfo_new->page_ = x_window_open_page_impl (w_current, filename);
    x_window_set_current_page_impl (w_current, nfo_new->page_);

    x_tabs_hdr_set (w_current->xtabs_nbook, nfo_new);
    gtk_widget_grab_focus (GTK_WIDGET (nfo_new->pview_));

    /* x_tabs_page_new() just invoked,
     * let page view creation complete -
     * process pending events:
    */
    while (gtk_events_pending())
      gtk_main_iteration();

    return nfo_new->page_;
  }


  /* XXX: 3: [pview] [page]:
   * - switch to existing page view
  */
  if (page != NULL)
  {
#ifdef DEBUG
    printf( "    x_tabs_page_open(): #3: [pview] [page] \n\n" );
#endif

    TabInfo* nfo_exi = x_tabs_info_find_by_page( w_current->xtabs_info_list, page );

    g_return_val_if_fail (nfo_exi != NULL, NULL);

    gint ndx_exi = gtk_notebook_page_num (w_current->xtabs_nbook, nfo_exi->wtab_);
    gtk_notebook_set_current_page (w_current->xtabs_nbook, ndx_exi);
    gtk_widget_grab_focus (GTK_WIDGET (nfo_exi->pview_));

    return page;
  }

  g_return_val_if_fail (FALSE && "end of func", NULL);
  return NULL;

} /* x_tabs_page_open() */



/*! \brief Changes the current tab.
 *  \public
 *
 *  \par Function Description
 *  If there's a tab that contains \a page, it will be activated,
 *  otherwise a new tab for \a page will be created and set active.
 *
 *  \note
 *  The code is intentionally left unrefactored.
 *
 *  \param [in] w_current  The toplevel environment.
 *  \param [in] page       The page.
 *
 */
void
x_tabs_page_set_cur (GschemToplevel* w_current,
                     LeptonPage* page)
{
  g_return_if_fail (w_current != NULL);

#ifdef DEBUG
  printf( "x_tabs_page_set_cur()\n" );
#endif

  TabInfo* nfo = x_tabs_info_find_by_page (w_current->xtabs_info_list, page);

  gint ndx = -1;


  /* XXX: 3: [pview] [page]:
   * - switch to existing page view
  */
  if (nfo != NULL)
  {
#ifdef DEBUG
    printf( "    x_tabs_page_set_cur(): #3: [pview] [page] \n\n" );
#endif

    ndx = gtk_notebook_page_num (w_current->xtabs_nbook, nfo->wtab_);
    g_return_if_fail (ndx >= 0);

    gtk_notebook_set_current_page (w_current->xtabs_nbook, ndx);
    gtk_widget_grab_focus (GTK_WIDGET (nfo->pview_));
  }

  else

  /*  XXX: 4: [!pview] [page]:
  */
  if (nfo == NULL && x_tabs_tl_page_find (w_current, page))
  {
#ifdef DEBUG
      printf( "    x_tabs_page_set_cur(): #4: [!pview] [page] \n\n" );
#endif

      nfo = x_tabs_page_new (w_current, page);

      x_tabs_hdr_set (w_current->xtabs_nbook, nfo);
      gtk_notebook_set_current_page (w_current->xtabs_nbook, ndx);
      gtk_widget_grab_focus (GTK_WIDGET (nfo->pview_));

      /* x_tabs_page_new() just invoked,
       * let page view creation complete -
       * process pending events:
      */
      while (gtk_events_pending())
        gtk_main_iteration();

      /* new page view is created for existing page => zoom it:
      */
      gschem_page_view_zoom_extents (x_tabs_tl_pview_cur (w_current), NULL);
  }

} /* x_tabs_page_set_cur() */



/*! \brief Closes a tab which contains a \a page.
 *  \public
 *
 *  \par Function Description
 *  When the last tab is closed, a new tab with blank page will be opened.
 *
 *  \param [in] w_current  The toplevel environment.
 *  \param [in] page       The page.
 *
 */
void
x_tabs_page_close (GschemToplevel* w_current,
                   LeptonPage* page)
{
  g_return_if_fail (w_current != NULL);

#ifdef DEBUG
  printf( "x_tabs_page_close()\n" );
#endif

  TabInfo* nfo_cur = x_tabs_info_find_by_page (w_current->xtabs_info_list, page);
  g_return_if_fail (nfo_cur != NULL);

  gint cnt = gtk_notebook_get_n_pages (w_current->xtabs_nbook);
  g_return_if_fail (cnt >= 1);


  /* page to be set as current after the [page] is closed:
  */
  LeptonPage* new_cur_page = x_window_close_page_impl (w_current, nfo_cur->page_);

  x_tabs_nbook_page_close (w_current, nfo_cur->page_);

  /* x_tabs_pview_rm (nfo_cur->pview_); NOTE: for now: nop */

  x_tabs_info_rm (w_current, nfo_cur);

  if (new_cur_page != NULL)
  {
    x_tabs_page_set_cur (w_current, new_cur_page);
  }
  else
  {
#ifdef DEBUG
    printf( "x_tabs_page_close(): NEW LeptonPage\n" );
#endif

    x_tabs_page_new  (w_current, NULL);

    /*
     * x_tabs_page_new() just invoked, but
     * no need to process pending events here:
     * it will be done in x_tabs_page_open()
    */

    x_tabs_page_open (w_current, NULL);
  }

} /* x_tabs_page_close() */



/*! \brief GtkNotebook "switch-page" signal handler.
 *
 *  \param [in] nbook  Notebook widget.
 *  \param [in] wtab   Tab widget.
 *  \param [in] ndx    Tab index.
 *  \param [in] data   GschemToplevel*.
 *
 */
static void
x_tabs_page_on_sel (GtkNotebook* nbook,
                    GtkWidget*   wtab,
                    guint        ndx,
                    gpointer     data)
{
  GschemToplevel* w_current = (GschemToplevel*) data;

  LeptonPage*     p_cur  = x_tabs_tl_page_cur  (w_current);
  GschemPageView* pv_cur = x_tabs_tl_pview_cur (w_current);

  if (p_cur == NULL && pv_cur == NULL)
    return;

  TabInfo* nfo = x_tabs_info_find_by_wtab (w_current->xtabs_info_list, wtab);

  if (nfo == NULL)
    return;

#ifdef DEBUG
  printf( "x_tabs_page_on_sel()\n" );
#endif

  /* before changing toplevel's current page and page view,
  *  cancel all actions that may be in progress on previous page:
  */
  x_tabs_cancel_all (w_current);

  x_tabs_tl_pview_cur_set (w_current, nfo->pview_);
  x_tabs_tl_page_cur_set (w_current, nfo->page_);

  x_window_set_current_page_impl (w_current, nfo->page_);

} /* x_tabs_page_on_sel() */



/*! \brief GtkNotebook "page-reordered" signal handler.
 */
static void
x_tabs_page_on_reordered (GtkNotebook* nbook,
                          GtkWidget*   wtab,
                          guint        newindex,
                          gpointer     data)
{
  GschemToplevel* w_current = (GschemToplevel*) data;
  g_return_if_fail (w_current != NULL);
  g_return_if_fail (w_current->toplevel != NULL);
  g_return_if_fail (w_current->toplevel->pages != NULL);

  TabInfo* nfo = x_tabs_info_find_by_wtab (w_current->xtabs_info_list, wtab);
  g_return_if_fail (nfo != NULL);

  LeptonPageList* pages = w_current->toplevel->pages;
  lepton_list_move_item (pages, nfo->page_, newindex);

  gtk_widget_grab_focus (GTK_WIDGET (nfo->pview_));
  page_select_widget_update (w_current);

#ifdef DEBUG
  x_tabs_dbg_pages_dump_simple( w_current );
#endif

} /* x_tabs_page_on_reordered() */



/*! \brief Create popup menu for tab's header.
 */
static GtkMenu*
x_tabs_menu_create (TabInfo* nfo)
{
  g_return_val_if_fail (nfo != NULL, NULL);

  GschemToplevel* tl = nfo->tl_;
  g_return_val_if_fail (tl != NULL, NULL);

  GtkWidget* menu = gtk_menu_new();
  x_tabs_menu_create_item (tl, menu, "&file-new", _("_New"), "document-new");
  x_tabs_menu_create_item (tl, menu, "&file-open", _("_Open..."), "document-open");
  x_tabs_menu_create_item_separ (menu);
  x_tabs_menu_create_item (tl, menu, "&file-save", _("_Save"), "document-save");
  x_tabs_menu_create_item (tl, menu, "&file-save-as", _("Save _As..."), "document-save-as");
  x_tabs_menu_create_item_separ (menu);
  x_tabs_menu_create_item (tl, menu, "&page-manager", _("Page _Manager..."), NULL);
  x_tabs_menu_create_item_separ (menu);
  x_tabs_menu_create_item (tl, menu, "&page-close", _("_Close"), "window-close");

  gtk_widget_show_all (menu);
  return GTK_MENU (menu);

} /* x_tabs_menu_create() */



/*! \brief Tab's header widget "button-press-event" signal handler.
 *  \todo  Consider switching to clicked tab
 */
static gboolean
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
static void
x_tabs_menu_item_on_activate (GtkAction* action, gpointer data)
{
  GschemToplevel* toplevel    = (GschemToplevel*) data;
  const gchar*    action_name = gtk_action_get_name (action);

  g_action_eval_by_name (toplevel, action_name);
}



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
x_tabs_menu_create_item (GschemToplevel* toplevel,
                         GtkWidget*      menu,
                         const gchar*    action_name,
                         const gchar*    action_label,
                         const gchar*    icon_name)
{
  GschemAction* action = gschem_action_new (action_name,  /* name */
                                            action_label, /* label */
                                            NULL,         /* tooltip */
#ifdef ENABLE_GTK3
                                            icon_name,    /* icon_name */
#else /* GTK2 */
                                            icon_name,    /* stock_id */
#endif
                                            NULL);        /* multikey_accel */

  GtkWidget* item = gtk_action_create_menu_item (GTK_ACTION (action));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);

  g_signal_connect (action,
                    "activate",
                    G_CALLBACK (&x_tabs_menu_item_on_activate),
                    toplevel);

} /* x_tabs_menu_create_item() */
