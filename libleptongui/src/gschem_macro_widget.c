/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2013 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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
 * \file gschem_macro_widget.c
 *
 * \brief A widget for entering and executing Guile macros
 */

#include <config.h>
#include "gschem.h"



static void
gschem_macro_widget_class_init (GschemMacroWidgetClass* klass);

static void
gschem_macro_widget_init (GschemMacroWidget* widget);

static void
get_property (GObject* object, guint param_id, GValue* value, GParamSpec* pspec);

static void
set_property (GObject* object, guint param_id, const GValue* value, GParamSpec* pspec);


static void
on_entry_activate (GtkEntry* entry, gpointer data);

static void
on_evaluate_clicked (GtkButton* button, gpointer data);

static void
on_cancel_clicked (GtkButton* button, gpointer data);

static void
on_entry_notify_text (GtkWidget* entry, GParamSpec* pspec, gpointer data);


static void
exec_macro (GschemToplevel* toplevel, const gchar* macro_text);

static void
macro_widget_exec_macro (GschemMacroWidget* widget, const gchar* macro_text);

static void
macro_widget_hide (GschemMacroWidget* widget);

static void
macro_widget_create (GschemMacroWidget* widget);

static void
history_add (GtkListStore* store, const gchar* line);

static void
history_save (GtkListStore* store);

static void
history_load (GtkListStore* store);

static void
history_truncate (GtkListStore* store);

static void
command_entry_set_font (GtkWidget* entry);



/*
 * GObject stuff:
 *
 */

enum
{
  PROP_0,
  PROP_TOPLEVEL
};



/*! \brief Convenience macro - gobject type implementation:
*/
G_DEFINE_TYPE (GschemMacroWidget, gschem_macro_widget, GTK_TYPE_INFO_BAR);



/*! \brief Initialize gobject class
 */
static void
gschem_macro_widget_class_init (GschemMacroWidgetClass* klass)
{
  G_OBJECT_CLASS (klass)->get_property = get_property;
  G_OBJECT_CLASS (klass)->set_property = set_property;

  GParamFlags flags = (GParamFlags) (G_PARAM_CONSTRUCT_ONLY | G_PARAM_READWRITE);
  GParamSpec* spec  = g_param_spec_pointer ("toplevel", "", "", flags);
  g_object_class_install_property (G_OBJECT_CLASS (klass), PROP_TOPLEVEL, spec);
}



/*! \brief Initialize gobject instance
 */
static void
gschem_macro_widget_init (GschemMacroWidget* widget)
{
  macro_widget_create (widget);
}



/*! \brief Get a gobject property
 */
static void
get_property (GObject* object,
              guint param_id,
              GValue* value,
              GParamSpec* pspec)
{
  GschemMacroWidget* widget = GSCHEM_MACRO_WIDGET (object);

  switch (param_id)
  {
    case PROP_TOPLEVEL:
      g_value_set_pointer (value, widget->toplevel);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}



/*! \brief Set a gobject property
 */
static void
set_property (GObject* object,
              guint param_id,
              const GValue* value,
              GParamSpec* pspec)
{
  GschemMacroWidget* widget = GSCHEM_MACRO_WIDGET (object);

  switch (param_id)
  {
    case PROP_TOPLEVEL:
      widget->toplevel = GSCHEM_TOPLEVEL (g_value_get_pointer (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}




/*
 * signal handlers:
 *
 */

/*! \brief Callback for when the user presses enter in the entry widget
 */
static void
on_entry_activate (GtkEntry* entry, gpointer data)
{
  GschemMacroWidget* widget = (GschemMacroWidget*) data;
  g_return_if_fail (widget != NULL);

  if (gtk_entry_get_text_length (entry) <= 0)
  {
    macro_widget_hide (widget);
    return;
  }

  const gchar* text = gtk_entry_get_text (entry);
  macro_widget_exec_macro (widget, text);
}



/*! \brief Callback for when the user clicks the evaluate button
 */
static void
on_evaluate_clicked (GtkButton* button, gpointer data)
{
  GschemMacroWidget* widget = (GschemMacroWidget*) data;
  g_return_if_fail (widget != NULL);

  const gchar* text = gtk_entry_get_text (GTK_ENTRY (widget->entry));
  macro_widget_exec_macro (widget, text);
}



/*! \brief Callback for when the user clicks the cancel button
 */
static void
on_cancel_clicked (GtkButton* button, gpointer data)
{
  GschemMacroWidget* widget = (GschemMacroWidget*) data;
  g_return_if_fail (widget != NULL);

  macro_widget_hide (widget);
}



/*! \brief GtkEntry's "text" property change notification signal handler
 */
static void
on_entry_notify_text (GtkWidget* entry, GParamSpec* pspec, gpointer data)
{
  GschemMacroWidget* widget = (GschemMacroWidget*) data;
  g_return_if_fail (widget != NULL);

  /* Update the sensitivity of the evaluate button:
  */
  guint16 len = gtk_entry_get_text_length (GTK_ENTRY (widget->entry));
  gtk_widget_set_sensitive (widget->evaluate_button, len > 0);
}




/*
 * implementation:
 *
 */

GtkWidget*
macro_widget_new (GschemToplevel* toplevel)
{
  g_return_val_if_fail (toplevel != NULL, NULL);

  gpointer obj = g_object_new (GSCHEM_TYPE_MACRO_WIDGET,
                               "toplevel", toplevel,
                               NULL);

  return GTK_WIDGET (obj);
}



void
macro_widget_show (GtkWidget* widget)
{
  g_return_if_fail (widget != NULL);

  GschemMacroWidget* macro_widget = GSCHEM_MACRO_WIDGET (widget);

  g_return_if_fail (macro_widget->entry != NULL);

  gtk_widget_show (widget);
  gtk_widget_grab_focus (macro_widget->entry);
}



static void
macro_widget_hide (GschemMacroWidget* widget)
{
  gtk_widget_hide (GTK_WIDGET (widget));
  gtk_widget_grab_focus (widget->toplevel->drawing_area);
}



/*! \brief Invoke liblepton code to execute macro [macro_text]
 * Execution output and result will be logged.
*/
static void
exec_macro (GschemToplevel* toplevel, const gchar* macro_text)
{
  scm_dynwind_begin ((scm_t_dynwind_flags) 0);
  g_dynwind_window (toplevel);

  gchar* cmd = g_strdup_printf(
    "(use-modules (lepton log)) (log! 'message (format #f \"~A\" %s))",
    macro_text);

  g_scm_c_eval_string_protected (cmd);

  g_free (cmd);

  scm_dynwind_end();
}



/*! \brief Execute Guile code passed in [macro_text]
*/
static void
macro_widget_exec_macro (GschemMacroWidget* widget, const gchar* macro_text)
{
  if (macro_text == NULL || strlen(macro_text) <= 0)
  {
    return;
  }

  /* save history and hide widget BEFORE executing macro code,
   * since that code may terminate the program:
  */
  history_add (widget->store, macro_text);
  history_truncate (widget->store);
  history_save (widget->store);

  macro_widget_hide (widget);

  exec_macro (widget->toplevel, macro_text);
}



/*! \brief Create the macro widget
*/
static void
macro_widget_create (GschemMacroWidget* widget)
{
  GtkWidget *action = gtk_info_bar_get_action_area (GTK_INFO_BAR (widget));
  GtkWidget *button_box;
  GtkWidget *cancel_button;
  GtkWidget *content = gtk_info_bar_get_content_area (GTK_INFO_BAR (widget));

  g_return_if_fail (widget != NULL);

  gtk_widget_set_no_show_all (GTK_WIDGET (widget), TRUE);

  GtkWidget* label = gtk_label_new (_("Macro:"));
  gtk_widget_set_visible (label, TRUE);
  gtk_box_pack_start (GTK_BOX (content), label, FALSE, FALSE, 0);


  /* command history list store:
  */
  widget->store = gtk_list_store_new (1, G_TYPE_STRING);
  GtkTreeModel* model = GTK_TREE_MODEL (widget->store);


  /* command entry combo box:
  */
  widget->combo = gtk_combo_box_new_with_model_and_entry (model);

  gtk_combo_box_set_entry_text_column (GTK_COMBO_BOX (widget->combo), 0);
  gtk_box_pack_start (GTK_BOX (content), widget->combo, TRUE, TRUE, 0);
  gtk_widget_set_visible (widget->combo, TRUE);


  /* GtkEntry inside the combo box:
  */
  widget->entry = gtk_bin_get_child (GTK_BIN (widget->combo));


  /* set font for the command entry:
  */
  command_entry_set_font (widget->entry);


  /* load command history:
  */
  history_load (widget->store);
  history_truncate (widget->store);


  /* enable text completion in the command entry:
  */
  GtkEntryCompletion* comp = gtk_entry_completion_new();
  gtk_entry_completion_set_model (comp, GTK_TREE_MODEL (widget->store));
  gtk_entry_completion_set_text_column (comp, 0);
  gtk_entry_set_completion (GTK_ENTRY (widget->entry), comp);


#ifdef ENABLE_GTK3
  button_box = gtk_button_box_new (GTK_ORIENTATION_HORIZONTAL);
#else
  button_box = gtk_hbutton_box_new ();
#endif
  gtk_widget_set_visible (button_box, TRUE);
  gtk_box_pack_start (GTK_BOX (content), button_box, FALSE, FALSE, 0);

  widget->evaluate_button = gtk_button_new_with_label (_("Evaluate"));
  gtk_widget_set_sensitive (widget->evaluate_button, FALSE);
  gtk_widget_set_visible (widget->evaluate_button, TRUE);
  gtk_box_pack_start (GTK_BOX (button_box), widget->evaluate_button, FALSE, FALSE, 0);

  cancel_button = gtk_button_new_with_label (_("_Cancel"));
  gtk_widget_set_visible (cancel_button, TRUE);
  gtk_box_pack_start (GTK_BOX (button_box), cancel_button, FALSE, FALSE, 0);

  gtk_widget_set_no_show_all (action, TRUE);
  gtk_widget_set_visible (action, FALSE);

  g_signal_connect (G_OBJECT (widget->entry),
                    "activate",
                    G_CALLBACK (&on_entry_activate),
                    widget);

  g_signal_connect (G_OBJECT (cancel_button),
                    "clicked",
                    G_CALLBACK (&on_cancel_clicked),
                    widget);

  g_signal_connect (G_OBJECT (widget->evaluate_button),
                    "clicked",
                    G_CALLBACK (&on_evaluate_clicked),
                    widget);

  g_signal_connect (G_OBJECT (widget->entry),
                    "notify::text",
                    G_CALLBACK (&on_entry_notify_text),
                    widget);

} /* macro_widget_create() */



/*! \brief Add a string to history
 *
 *  \param store GtkListStore history container
 *  \param line  string to be added to history
 */
static void
history_add (GtkListStore* store, const gchar* line)
{
  g_return_if_fail (store != NULL);
  g_return_if_fail (line != NULL);

  const gint column = 0;

  /* determine the most recent entry in history:
  */
  GtkTreeIter iter;
  gchar* last = NULL;

  GtkTreeModel* mod = GTK_TREE_MODEL (store);

  if (gtk_tree_model_get_iter_first (mod, &iter))
  {
    gtk_tree_model_get (mod, &iter, 0, &last, -1);
  }

  /* do not save duplicated consequent commands:
  */
  if (last == NULL || g_strcmp0 (last, line) != 0)
  {
    /* add current command to the list store:
    */
    gtk_list_store_prepend (store, &iter);
    gtk_list_store_set (store, &iter, column, line, -1);
  }

  g_free (last);

} /* history_add() */



/*! \brief Truncate the history list
 *
 *  \par Function Description
 *  Read max history size configuration from the "history-length"
 *  key in the "schematic.macro-widget" group (USER context).
 *  If that key is absent, maximum history size will be set
 *  to MACRO_WIDGET_HISTORY_MAX (\see gschem_macro_widget.h).
 *  Truncate history to be <= maximum size.
 *
 *  \param store GtkListStore history container
 */
static void
history_truncate (GtkListStore* store)
{
  g_return_if_fail (store != NULL);

  /* default history size:
  */
  gint count_max = MACRO_WIDGET_HISTORY_MAX;

  /* get USER config context:
  */
  EdaConfig* cfg = eda_config_get_user_context();

  /* schematic.macro-widget::history-length key
   * can be changed at run-time.
   * try to reload config:
  */
  GError* error = NULL;
  eda_config_load (cfg, &error);
  g_clear_error (&error);

  /* read configuration:
  */
  gint val = eda_config_get_int (cfg,
                                 "schematic.macro-widget",
                                 "history-length",
                                 &error);

  if (error == NULL && val > 0)
  {
    count_max = val;
  }

  g_clear_error (&error);


  GtkTreeModel* mod = GTK_TREE_MODEL (store);
  gint count = gtk_tree_model_iter_n_children (mod, NULL);

  /* remove excessive items from the history store:
  */
  GtkTreeIter iter;
  for (gint i = count; i > count_max; --i)
  {
    if (gtk_tree_model_iter_nth_child (mod, &iter, NULL, i - 1))
    {
      gtk_list_store_remove (store, &iter);
    }
  }

} /* history_truncate() */



/*! \brief Save history to configuration
 *
 *  \param store GtkListStore history container
 */
static void
history_save (GtkListStore* store)
{
  g_return_if_fail (store != NULL);

  GtkTreeModel* mod = GTK_TREE_MODEL (store);
  GtkTreeIter iter;

  if (!gtk_tree_model_get_iter_first (mod, &iter))
  {
    /* the history is empty */
    return;
  }


  /* allocate an array:
  */
  gint count = gtk_tree_model_iter_n_children (mod, NULL);

  gchar** lines = (gchar**) g_malloc0_n (count + 1, sizeof(gchar*));
  lines[ count ] = NULL;

  /* add elements from the list store to [lines] array:
  */
  gint i = 0;

  do
  {
    gtk_tree_model_get (mod, &iter, 0, &lines[ i++ ], -1);
  }
  while (gtk_tree_model_iter_next (mod, &iter));


  /* get config context:
  */
  EdaConfig* ctx = eda_config_get_cache_context();

  /* write configuration:
  */
  eda_config_set_string_list(ctx,
                             "schematic.macro-widget",
                             "history",
                             (const gchar**) lines,
                             count);
  eda_config_save (ctx, NULL);


  /* free() array and its elements:
  */
  g_strfreev (lines);

} /* history_save() */



/*! \brief Load history from configuration
 *
 *  \param store GtkListStore history container
 */
static void
history_load (GtkListStore* store)
{
  g_return_if_fail (store != NULL);

  const gint column = 0;

  /* get config context:
  */
  EdaConfig* ctx = eda_config_get_cache_context();

  /* read configuration:
  */
  gsize len = 0;
  gchar** lines = eda_config_get_string_list (ctx,
                                             "schematic.macro-widget",
                                             "history",
                                             &len,
                                             NULL);

  if (lines != NULL && len > 0)
  {
    GtkTreeIter iter;

    /* populate the list store:
    */
    for (gsize i = 0; i < len; ++i)
    {
      gtk_list_store_append (store, &iter);
      gtk_list_store_set (store, &iter, column, lines[i], -1);
    }

    g_strfreev (lines);
  }

} /* history_load() */



/*! \brief Set font for the command entry widget
 *
 *  \par Function Description
 *  Read custom font configuration from the "font" key
 *  in the "schematic.macro-widget" group. If the key
 *  exists and contains some font name (e.g. "Monospace 12"),
 *  set macro widget text entry's font to that value.
 *
 *
 *  \param entry  GtkEntry widget inside macro command combo box
 */
static void
command_entry_set_font (GtkWidget* entry)
{
  g_return_if_fail (entry != NULL);

  gchar* cwd = g_get_current_dir();
  EdaConfig* cfg = eda_config_get_context_for_path (cwd);
  g_free (cwd);

  if (cfg == NULL)
  {
    return;
  }

  GError* err = NULL;
  gchar* font = eda_config_get_string (cfg,
                                       "schematic.macro-widget",
                                       "font",
                                       &err);

  if (err == NULL)
  {
    PangoFontDescription* fdesc = pango_font_description_from_string (font);

#ifdef ENABLE_GTK3
    gtk_widget_override_font (entry, fdesc);
#else
    gtk_widget_modify_font (entry, fdesc);
#endif

    pango_font_description_free (fdesc);
    g_free (font);
  }

  g_clear_error (&err);

} /* command_entry_set_font() */
