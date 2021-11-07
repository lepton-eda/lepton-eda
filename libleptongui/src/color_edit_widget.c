/* Lepton EDA Schematic Capture
 * Copyright (C) 2018-2020 dmn <graahnul.grom@gmail.com>
 * Copyright (C) 2018-2021 Lepton EDA Contributors
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
 * \file color_edit_widget.c
 *
 * \brief Color scheme editor widget
 *
 */

#include "config.h"

#include "gschem.h"


/* convenience macro - gobject type implementation:
*/
G_DEFINE_TYPE (ColorEditWidget, color_edit_widget, GSCHEM_TYPE_BIN);


/* widget's property IDs:
*/
typedef enum
{

  PROP_0,       /* placeholder */
  PROP_TOPLEVEL

} ColorEditWidgetProps;


/* --------------------------------------------------------
 *
 * forward declarations:
 *
 */

static void
color_edit_widget_create (ColorEditWidget* widget);

/* \todo Currently unused; see todo in commented out function implementation
 *
static void
mk_opacity_box (GtkWidget* vbox);
 *
 */

static void
color_sel_update (ColorEditWidget* widget);

static void
on_color_cb_changed (GtkWidget* cb, gpointer p);

#ifdef ENABLE_GTK3
static void
on_color_activated (GtkColorChooser* csel, GdkRGBA *color, gpointer p);
#else
static void
on_color_sel_changed (GtkColorSelection* csel, gpointer p);
#endif

#ifdef ENABLE_GTK3
static void
on_btn_apply (GtkWidget* btn, gpointer p);
#endif

static void
on_btn_save(GtkWidget* btn, gpointer p);

static GtkWidget*
dlg_save_as(GtkWidget* parent);

static gboolean
dlg_confirm_overwrite (GtkWidget* parent, const gchar* fname);




/* --------------------------------------------------------
 *
 * object construction:
 *
 */

GtkWidget*
color_edit_widget_new (GschemToplevel* w_current)
{
  gpointer obj = g_object_new (COLOR_EDIT_WIDGET_TYPE,
                               "toplevel", w_current,
                               NULL);
  return GTK_WIDGET (obj);
}




/* --------------------------------------------------------
 *
 * gobject stuff:
 *
 */

static void
get_property (GObject* obj, guint id, GValue* val, GParamSpec* spec)
{
  ColorEditWidget* widget = COLOR_EDIT_WIDGET (obj);

  if (id == PROP_TOPLEVEL)
  {
    g_value_set_pointer (val, widget->toplevel_);
  }
  else
  {
    G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, id, spec);
  }
}



static void
set_property (GObject* obj, guint id, const GValue* val, GParamSpec* spec)
{
  ColorEditWidget* widget = COLOR_EDIT_WIDGET (obj);

  if (id == PROP_TOPLEVEL)
  {
    gpointer ptr = g_value_get_pointer (val);
    widget->toplevel_ = GSCHEM_TOPLEVEL (ptr);
  }
  else
  {
    G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, id, spec);
  }
}



static void
dispose (GObject* obj)
{
  ColorEditWidgetClass* cls = COLOR_EDIT_WIDGET_GET_CLASS (obj);
  GObjectClass* parent_cls = (GObjectClass*) g_type_class_peek_parent (cls);

  parent_cls->dispose (obj);
}



static void
color_edit_widget_class_init (ColorEditWidgetClass* cls)
{
  GObjectClass* gcls = G_OBJECT_CLASS (cls);

  gcls->dispose      = &dispose;
  gcls->get_property = &get_property;
  gcls->set_property = &set_property;

  GParamFlags flags = (GParamFlags) (G_PARAM_CONSTRUCT_ONLY | G_PARAM_READWRITE);
  GParamSpec* spec = g_param_spec_pointer ("toplevel", "", "", flags);
  g_object_class_install_property (gcls, PROP_TOPLEVEL, spec);
}



static void
color_edit_widget_init (ColorEditWidget* widget)
{
  color_edit_widget_create (widget);
}


static GtkWidget*
separator_new ()
{
#ifdef ENABLE_GTK3
  return gtk_separator_new (GTK_ORIENTATION_HORIZONTAL);
#else
  return gtk_hseparator_new ();
#endif
}


/* --------------------------------------------------------
 *
 * implementation:
 *
 */

static void
color_edit_widget_create (ColorEditWidget* widget)
{
#ifdef ENABLE_GTK3
  GtkWidget* vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
  GtkWidget* vbox = gtk_vbox_new (FALSE, 0);
#endif
  gtk_container_add (GTK_CONTAINER (widget), vbox);

#ifdef ENABLE_GTK3
  GtkWidget* hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
  GtkWidget* hbox = gtk_hbox_new (FALSE, 0);
#endif
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);

  /* color selection combo box: */
  widget->color_cb_ = x_colorcb_new();
  gtk_box_pack_start (GTK_BOX (hbox), widget->color_cb_, TRUE, TRUE, 0);

#ifdef ENABLE_GTK3
  /* "Apply" button: */
  widget->btn_apply = gtk_button_new_with_mnemonic (_("_Apply"));
  gtk_box_pack_start (GTK_BOX (hbox), widget->btn_apply, FALSE, FALSE, 0);
#endif

  /* "save as..." button: */
  widget->btn_save_ = gtk_button_new_with_mnemonic (_("Save As.._."));
  gtk_box_pack_start (GTK_BOX (hbox), widget->btn_save_, FALSE, FALSE, 0);

  /* separator: */
  gtk_box_pack_start (GTK_BOX (vbox), separator_new(), FALSE, FALSE, 5);


  /* \todo opacity control:
  *
  * An idea here is to make outline color scheme the same
  * as display one, but allow it to be slightly more transparent
  *
  mk_opacity_box (vbox);
  *
  */


#ifdef ENABLE_GTK3
  /* standard color chooser widget: */
  widget->color_chooser = gtk_color_chooser_widget_new ();
  gtk_box_pack_start (GTK_BOX (vbox), widget->color_chooser, TRUE, TRUE, 0);
#else
  /* standard color selection widget: */
  widget->color_sel_ = gtk_color_selection_new();
  gtk_color_selection_set_has_opacity_control(
    GTK_COLOR_SELECTION (widget->color_sel_),
    FALSE); /* do not support opacity yet */
  gtk_box_pack_start (GTK_BOX (vbox), widget->color_sel_, TRUE, TRUE, 0);
#endif

  /* separator: */
  gtk_box_pack_start (GTK_BOX (vbox), separator_new(), FALSE, FALSE, 5);


  /* informational label: */
  const gchar* msg =
    _("Save your color scheme to a file by clicking on the \"Save As...\"\n"
      "button. It can be loaded on startup with the following expression\n"
      "(including parentheses) in the gschemrc configuration file:\n"
      "( primitive-load \"/path/to/saved-color-scheme-file\" )");

  GtkWidget* label = gtk_label_new (msg);
  gtk_label_set_selectable (GTK_LABEL (label), TRUE);
  gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, FALSE, 0);


  g_signal_connect (G_OBJECT (widget->color_cb_),
                    "changed",
                    G_CALLBACK (&on_color_cb_changed),
                    widget);

#ifdef ENABLE_GTK3
  g_signal_connect (G_OBJECT (widget->color_chooser),
                    "color-activated",
                    G_CALLBACK (&on_color_activated),
                    (gpointer) widget);
#else
  g_signal_connect (G_OBJECT (widget->color_sel_),
                    "color-changed",
                    G_CALLBACK (&on_color_sel_changed),
                    widget);
#endif

  g_signal_connect (G_OBJECT (widget->btn_save_),
                    "clicked",
                    G_CALLBACK (&on_btn_save),
                    widget);

#ifdef ENABLE_GTK3
  g_signal_connect (G_OBJECT (widget->btn_apply),
                    "clicked",
                    G_CALLBACK (&on_btn_apply),
                    widget);
#endif

  x_colorcb_set_index (widget->color_cb_, BACKGROUND_COLOR);


  gtk_widget_show_all (GTK_WIDGET (widget));

} /* color_edit_widget_create() */



#ifdef ENABLE_GTK3
/*! \brief: Update color chooser widget to match the color chosen
 *  in the color combo box.
 */
static void
color_sel_update (ColorEditWidget* widget)
{
  g_return_if_fail (widget != NULL);

  int ndx = x_colorcb_get_index (widget->color_cb_);
  GdkRGBA* color = x_color_lookup_gdk_rgba (ndx);
  GtkColorChooser* chooser = GTK_COLOR_CHOOSER (widget->color_chooser);

  g_signal_handlers_block_by_func (G_OBJECT (chooser),
                                   (gpointer) &on_color_activated,
                                   widget);

  gtk_color_chooser_set_rgba (chooser, color);
  gdk_rgba_free (color);

  g_signal_handlers_unblock_by_func (G_OBJECT (chooser),
                                     (gpointer) &on_color_activated,
                                     widget);
}

#else /* GTK2 */

/*! \brief: Update color selection widget to match color in a combo box
 */
static void
color_sel_update (ColorEditWidget* widget)
{
  g_return_if_fail (widget != NULL);

  int ndx = x_colorcb_get_index (widget->color_cb_);
  GdkColor* color = x_color_lookup_gdk (ndx);

  GtkColorSelection* csel = GTK_COLOR_SELECTION (widget->color_sel_);

  g_signal_handlers_block_by_func (G_OBJECT (csel),
                                   (gpointer) &on_color_sel_changed,
                                   widget);

  gtk_color_selection_set_current_color (csel, color);
  gdk_color_free (color);

  g_signal_handlers_unblock_by_func (G_OBJECT (csel),
                                     (gpointer) &on_color_sel_changed,
                                     widget);
} /* color_sel_update() */
#endif



/*! \public
 *  \brief: Update color selection widget to match color in a combo box
 */
void
color_edit_widget_update (GschemToplevel* w_current)
{
  g_return_if_fail (w_current != NULL);

  ColorEditWidget* widget = COLOR_EDIT_WIDGET (w_current->color_edit_widget);
  g_return_if_fail (widget != NULL);

  color_sel_update (widget);

} /* color_edit_widget_update() */




/* --------------------------------------------------------
 *
 * signal handlers:
 *
 */

/*! \brief Color selection combo box "changed" signal handler
 */
static void
on_color_cb_changed (GtkWidget* color_cb, gpointer p)
{
  ColorEditWidget* widget = (ColorEditWidget*) p;
  g_return_if_fail (widget != NULL);

  color_sel_update (widget);

} /* on_color_cb_changed() */



#ifdef ENABLE_GTK3

/*! \brief GtkColorChooser "color-activated" signal handler.
 */
static void
on_color_activated (GtkColorChooser* csel,
                    GdkRGBA *color,
                    gpointer p)
{
  ColorEditWidget* widget = (ColorEditWidget*) p;
  g_return_if_fail (widget != NULL);
  g_return_if_fail (widget->toplevel_ != NULL);

  int color_index = x_colorcb_get_index (GTK_WIDGET (widget->color_cb_));
  g_return_if_fail (color_index >= 0);

  /* Adjust the color in display and outline color maps. */
  x_color_set_display_color (color_index, color);
  x_color_set_outline_color (color_index, color);

  /* Update current combo box color. */
  GtkComboBox* combo = GTK_COMBO_BOX (widget->color_cb_);
  GtkTreeIter iter;
  if (gtk_combo_box_get_active_iter (combo, &iter))
  {
    x_colorcb_set_rgba_color (&iter, color);
  }

  /* Refresh page view. */
  GschemPageView* pview =
    gschem_toplevel_get_current_page_view (widget->toplevel_);
  gschem_page_view_invalidate_all (pview);

}


#else /* GTK2 */


/*! \brief GtkColorSelection "color-changed" signal handler
 */
static void
on_color_sel_changed (GtkColorSelection* csel, gpointer p)
{
  ColorEditWidget* widget = (ColorEditWidget*) p;
  g_return_if_fail (widget != NULL);
  g_return_if_fail (widget->toplevel_ != NULL);

  int color_index = x_colorcb_get_index (GTK_WIDGET (widget->color_cb_));
  g_return_if_fail (color_index >= 0);

  GdkColor color;
  gtk_color_selection_get_current_color (csel, &color);

  /* adjust a color in display and outline color maps: */
  x_color_set_display (color_index, &color);
  x_color_set_outline (color_index, &color);


  /* update current combo box color: */
  GtkComboBox* combo = GTK_COMBO_BOX (widget->color_cb_);
  GtkTreeIter iter;
  if (gtk_combo_box_get_active_iter (combo, &iter))
  {
    x_colorcb_set_color (&iter, &color);
  }

  /* refresh page view: */
  GschemPageView* pview =
    gschem_toplevel_get_current_page_view (widget->toplevel_);
  gschem_page_view_invalidate_all (pview);

} /* on_color_sel_changed() */

#endif


#ifdef ENABLE_GTK3
/*! \brief "Apply" button "clicked" signal handler. */
static void
on_btn_apply (GtkWidget* btn, gpointer p)
{
  ColorEditWidget* widget = (ColorEditWidget*) p;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (widget->toplevel_ != NULL);

  GtkColorChooser* chooser = GTK_COLOR_CHOOSER (widget->color_chooser);
  GdkRGBA color;
  gtk_color_chooser_get_rgba (chooser, &color);

  int color_index = x_colorcb_get_index (GTK_WIDGET (widget->color_cb_));
  x_color_set_display_color (color_index, &color);
  gtk_widget_queue_draw (GTK_WIDGET (gschem_toplevel_get_current_page_view (widget->toplevel_)));
}
#endif


/*! \brief "Save As" button "clicked" signal handler
 */
static void
on_btn_save (GtkWidget* btn, gpointer p)
{
  ColorEditWidget* widget = (ColorEditWidget*) p;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (widget->toplevel_ != NULL);

  /* open "save as" dialog: */
  GtkWidget* dlg = dlg_save_as (widget->toplevel_->main_window);
  if (dlg == NULL)
    return;

  gchar* fname = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dlg));

  /* check if file already exist: */
  if (!dlg_confirm_overwrite (dlg, fname))
  {
    g_free (fname);
    gtk_widget_destroy (dlg);
    return;
  }


  /* generate scheme code string: */
  GString* str_1 = x_color_map2str_display();
  g_string_prepend (str_1, "(display-color-map ");
  g_string_append  (str_1, ")");
  g_string_append  (str_1, "\n");
  g_string_append  (str_1, "\n");

  GString* str_2 = x_color_map2str_outline();
  g_string_prepend (str_2, "(display-outline-color-map ");
  g_string_append  (str_2, ")");
  g_string_append  (str_2, "\n");
  g_string_append  (str_2, "\n");

  g_string_append  (str_1, str_2->str);


  /* save file: */
  GError* err = NULL;
  gboolean ok = g_file_set_contents (fname,
                                     str_1->str,
                                     strlen (str_1->str),
                                     &err);

  if (!ok)
  {
    GtkWidget* dlg = gtk_message_dialog_new(
      GTK_WINDOW (widget->toplevel_->main_window),
      GTK_DIALOG_MODAL,
      GTK_MESSAGE_ERROR,
      GTK_BUTTONS_OK,
      _("Could not save file [%s]:\n%s"),
      fname,
      err->message);

    gtk_window_set_title (GTK_WINDOW (dlg), _("Failed to save file"));
    gtk_dialog_run (GTK_DIALOG (dlg));
    gtk_widget_destroy (dlg);
  }

  g_clear_error (&err);
  g_string_free (str_1, TRUE);
  g_string_free (str_2, TRUE);
  g_free (fname);

  gtk_widget_destroy (dlg);

} /* on_btn_save() */




/* --------------------------------------------------------
 *
 * helpers:
 *
 */

/*! \brief: Open "save as" dialog
 *
 *  \note Caller must destroy returned dialog box widget
 *
 *  \param parent  A parent widget for dialog
 *  \return        Dialog box widget or NULL if dialog is cancelled
 *
 */
static GtkWidget*
dlg_save_as (GtkWidget* parent)
{
  GtkWidget* dlg = gtk_file_chooser_dialog_new(
    _("Save Color Scheme"),
    GTK_WINDOW (parent),
    GTK_FILE_CHOOSER_ACTION_SAVE,
    GTK_STOCK_SAVE,   GTK_RESPONSE_ACCEPT,
    GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
    NULL);

  gtk_dialog_set_alternative_button_order(
    GTK_DIALOG (dlg),
    GTK_RESPONSE_ACCEPT,
    GTK_RESPONSE_CANCEL,
    -1);

  gtk_dialog_set_default_response (GTK_DIALOG (dlg),
                                   GTK_RESPONSE_ACCEPT);

  gint res = gtk_dialog_run (GTK_DIALOG (dlg));
  if (res != GTK_RESPONSE_ACCEPT)
  {
    gtk_widget_destroy (dlg);
    return NULL;
  }

  return dlg;

} /* dlg_save_as() */



/*! \brief: If file \a fname exists, open confirmation dialog
 *
 *  \param parent  A parent widget for dialog
 *  \param fname   File name
 *
 *  \return  TRUE if no such file exist or it's OK to overwrite it
 *
 */
static gboolean
dlg_confirm_overwrite (GtkWidget* parent, const gchar* fname)
{
  if (!g_file_test (fname, G_FILE_TEST_EXISTS))
    return TRUE;

  GtkWidget* dlg = gtk_message_dialog_new(
    GTK_WINDOW (parent),
    (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT),
    GTK_MESSAGE_QUESTION,
    GTK_BUTTONS_YES_NO,
    _("The selected file `%1$s' already exists.\n\n"
      "Would you like to overwrite it?"),
    fname);

  gtk_window_set_title (GTK_WINDOW (dlg), _("Overwrite file?"));
  gtk_dialog_set_default_response (GTK_DIALOG (dlg), GTK_RESPONSE_NO);

  gint res = gtk_dialog_run (GTK_DIALOG (dlg));
  gtk_widget_destroy (dlg);

  return res == GTK_RESPONSE_YES;

} /* dlg_confirm_overwrite() */



/*! \brief Create GUI for transparency control
 *  \note  Currently unused
 *  \todo  Implement transparency for outline color map
 *
 *  \param vbox Parent widget
 *

static void
mk_opacity_box (GtkWidget* vbox)
{
#ifdef ENABLE_GTK3
  GtkWidget* hbox2 = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
  GtkWidget* hbox2 = gtk_hbox_new (FALSE, 0);
#endif
  gtk_box_pack_start (GTK_BOX (vbox), hbox2, TRUE, TRUE, 0);

  GtkWidget* label = gtk_label_new (_("Opacity for outline color scheme:"));

  GtkObject* adj = gtk_adjustment_new (255, 0, 255, 1, 10, 0);
  GtkWidget* scale = gtk_hscale_new (GTK_ADJUSTMENT (adj));

  gtk_scale_set_digits (GTK_SCALE (scale), 0);
  gtk_scale_set_value_pos (GTK_SCALE (scale), GTK_POS_LEFT);

  gtk_box_pack_start (GTK_BOX (hbox2), label, TRUE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (hbox2), scale, TRUE, TRUE, 0);

  gtk_box_pack_start (GTK_BOX (vbox), separator_new(), FALSE, FALSE, 5);

}

*/
