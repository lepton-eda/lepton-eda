/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors
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
#include <glib.h>

#include <glib-object.h>
#include <glib/gstdio.h>

#include "schematic.h"
#include <gdk/gdkkeysyms.h>


#include "../include/dialog.h"

/* Signal marshaller based on generated code from glib-genmarshal */
static void
schematic_marshal_VOID__POINTER_STRING (GClosure     *closure,
                                        GValue       *return_value,
                                        guint         n_param_values,
                                        const GValue *param_values,
                                        gpointer      invocation_hint,
                                        gpointer      marshal_data)
{
  typedef void (*GMarshalFunc_VOID__POINTER_STRING) (gpointer     data1,
                                                     gpointer     arg_1,
                                                     gpointer     arg_2,
                                                     gpointer     data2);
  register GMarshalFunc_VOID__POINTER_STRING callback;
  register GCClosure *cc = (GCClosure*) closure;
  register gpointer data1, data2;

  g_return_if_fail (n_param_values == 3);

  if (G_CCLOSURE_SWAP_DATA (closure)) {
    data1 = closure->data;
    data2 = g_value_peek_pointer (param_values + 0);
  } else {
    data1 = g_value_peek_pointer (param_values + 0);
    data2 = closure->data;
  }
  callback = (GMarshalFunc_VOID__POINTER_STRING) (marshal_data ? marshal_data : cc->callback);

  callback (data1,
            g_value_get_pointer (param_values + 1),
            (gchar*)g_value_get_string (param_values + 2),
            data2);
}
/* End section based on generated code from glib-genmashal */


enum {
  PROP_SETTINGS_NAME = 1,
  PROP_SCHEMATIC_WINDOW
};


enum {
  GEOMETRY_SAVE,
  GEOMETRY_RESTORE,
  LAST_SIGNAL
};

static guint schematic_dialog_signals[ LAST_SIGNAL ] = { 0 };

G_DEFINE_TYPE (SchematicDialog,
               schematic_dialog,
               GTK_TYPE_DIALOG);

/*! \private
 *  \brief Initialize SchematicDialog instance
 *
 *  \param [in,out]  dialog  The SchematicDialog instance
 */
static void
schematic_dialog_init (SchematicDialog *dialog)
{
}


/*! \brief SchematicDialog "geometry_save" class method handler
 *
 *  \par Function Description
 *  Save the dialog's current position and size to the passed GKeyFile
 *
 *  \param [in] dialog     The SchematicDialog to save the position and size of.
 *  \param [in] cfg        The \c EdaConfig instance to save the geometry data to.
 *  \param [in] group_name The group name in the key file to store the data under.
 */
static void
geometry_save (SchematicDialog *dialog,
               EdaConfig *cfg,
               gchar* group_name)
{
  gint x, y, width, height;

  gtk_window_get_position (GTK_WINDOW (dialog), &x, &y);
  gtk_window_get_size (GTK_WINDOW (dialog), &width, &height);

  eda_config_set_int (cfg, group_name, "x", x);
  eda_config_set_int (cfg, group_name, "y", y);
  eda_config_set_int (cfg, group_name, "width", width);
  eda_config_set_int (cfg, group_name, "height", height);
}


/*! \brief SchematicDialog "geometry_restore" class method handler
 *
 *  \par Function Description
 *  Restore dialog's last position and size from the passed GKeyFile
 *
 *  \param [in] dialog     The SchematicDialog to restore the position and size of.
 *  \param [in] cfg        The \c EdaConfig instance to load the geometry data from.
 *  \param [in] group_name The group name in the key file to find the data under.
 */
static void
geometry_restore (SchematicDialog *dialog,
                  EdaConfig *cfg,
                  gchar* group_name)
{
  gint x, y, width, height;

  x      = eda_config_get_int (cfg, group_name, "x", NULL);
  y      = eda_config_get_int (cfg, group_name, "y", NULL);
  width  = eda_config_get_int (cfg, group_name, "width",  NULL);
  height = eda_config_get_int (cfg, group_name, "height", NULL);

  gtk_window_move (GTK_WINDOW (dialog), x, y);
  gtk_window_resize (GTK_WINDOW (dialog), width, height);
}


/*! \brief GtkWidget show signal handler
 *
 *  \par Function Description
 *  Just before the dialog widget is shown, call the hook
 *  to restore its previously saved position and size.
 *
 *  \param [in] widget  The GtkWidget being shown.
 */
static void show_handler (GtkWidget *widget)
{
  gchar *group_name;
  EdaConfig *cfg = eda_config_get_cache_context ();
  SchematicDialog *dialog = SCHEMATIC_DIALOG (widget);

  if (dialog->settings_name != NULL) {
    group_name = g_strdup_printf ("schematic.dialog-geometry.%s",
                                  dialog->settings_name);

    g_assert (cfg != NULL);
    if (eda_config_has_group (cfg, group_name)) {
      g_signal_emit (dialog, schematic_dialog_signals[ GEOMETRY_RESTORE ], 0,
                     cfg, group_name);
    }

    g_free (group_name);
  }

  /* Let GTK show the window */
  GTK_WIDGET_CLASS (schematic_dialog_parent_class)->show (widget);
}


/*! \brief GtkWidget unmap signal handler
 *
 *  \par Function Description
 *  Just before the dialog widget is unmapped, call the hook
 *  to save its current position and size.
 *
 *  This typically happens when you call gtk_widget_destroy().
 *
 *  \param [in] widget  The GtkWidget being unmapped.
 */
static void unmap_handler (GtkWidget *widget)
{
  gchar *group_name;
  EdaConfig *cfg = eda_config_get_cache_context ();
  SchematicDialog *dialog = SCHEMATIC_DIALOG (widget);

  if (dialog->settings_name != NULL) {
    group_name = g_strdup_printf ("schematic.dialog-geometry.%s",
                                  dialog->settings_name);

    g_assert (cfg != NULL);
    g_signal_emit (dialog, schematic_dialog_signals[ GEOMETRY_SAVE ], 0,
                   cfg, group_name);

    g_free (group_name);
  }

  /* Let GTK unmap the window */
  GTK_WIDGET_CLASS (schematic_dialog_parent_class)->unmap (widget);
}


/*! \brief GObject finalise handler
 *
 *  \par Function Description
 *  Just before the SchematicDialog GObject is finalized, free our
 *  allocated data, and then chain up to the parent's finalize handler.
 *
 *  \param [in] object The GObject being finalized.
 */
static void
schematic_dialog_finalize (GObject *object)
{
  SchematicDialog *dialog = SCHEMATIC_DIALOG (object);

  g_free (dialog->settings_name);

  G_OBJECT_CLASS (schematic_dialog_parent_class)->finalize (object);
}


/*! \brief GObject property setter function
 *
 *  \par Function Description
 *  Setter function for SchematicDialog's GObject properties,
 *  "settings-name" and "schematic-window".
 *
 *  \param [in]  object       The GObject whose properties we are setting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [in]  value        The GValue the property is being set from
 *  \param [in]  pspec        A GParamSpec describing the property being set
 */
static void
schematic_dialog_set_property (GObject *object,
                               guint property_id,
                               const GValue *value,
                               GParamSpec *pspec)
{
  SchematicDialog *dialog = SCHEMATIC_DIALOG (object);

  switch(property_id) {
    case PROP_SETTINGS_NAME:
      g_free (dialog->settings_name);
      dialog->settings_name = g_strdup (g_value_get_string (value));
      break;
    case PROP_SCHEMATIC_WINDOW:
      dialog->w_current = SCHEMATIC_WINDOW (g_value_get_pointer (value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }

}


/*! \brief GObject property getter function
 *
 *  \par Function Description
 *  Getter function for SchematicDialog's GObject properties,
 *  "settings-name" and "schematic-window".
 *
 *  \param [in]  object       The GObject whose properties we are getting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [out] value        The GValue in which to return the value of the property
 *  \param [in]  pspec        A GParamSpec describing the property being got
 */
static void
schematic_dialog_get_property (GObject *object,
                               guint property_id,
                               GValue *value,
                               GParamSpec *pspec)
{
  SchematicDialog *dialog = SCHEMATIC_DIALOG (object);

  switch(property_id) {
      case PROP_SETTINGS_NAME:
        g_value_set_string (value, dialog->settings_name);
        break;
      case PROP_SCHEMATIC_WINDOW:
        g_value_set_pointer (value, (gpointer)dialog->w_current);
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }

}


/*! \brief GType class initialiser for SchematicDialog
 *
 *  \par Function Description
 *  GType class initialiser for SchematicDialog. We override our parent
 *  virtual class methods as needed and register our GObject properties.
 *
 *  \param [in]  klass       The SchematicDialogClass we are initialising
 */
static void
schematic_dialog_class_init (SchematicDialogClass *klass)
{
  GObjectClass     *gobject_class = G_OBJECT_CLASS (klass);
  GtkWidgetClass *gtkwidget_class = GTK_WIDGET_CLASS (klass);

  klass->geometry_save         = geometry_save;
  klass->geometry_restore      = geometry_restore;

  gtkwidget_class->show        = show_handler;
  gtkwidget_class->unmap       = unmap_handler;

  gobject_class->finalize      = schematic_dialog_finalize;
  gobject_class->set_property  = schematic_dialog_set_property;
  gobject_class->get_property  = schematic_dialog_get_property;

  schematic_dialog_signals[ GEOMETRY_SAVE ] =
    g_signal_new ("geometry-save",
                  G_OBJECT_CLASS_TYPE( gobject_class ),
                  G_SIGNAL_RUN_FIRST,     /*signal_flags */
                  G_STRUCT_OFFSET (SchematicDialogClass, geometry_save),
                  NULL, /* accumulator */
                  NULL, /* accu_data */
                  schematic_marshal_VOID__POINTER_STRING,
                  G_TYPE_NONE,
                  2,    /* n_params */
                  G_TYPE_POINTER,
                  G_TYPE_STRING
                 );

  schematic_dialog_signals[ GEOMETRY_RESTORE ] =
    g_signal_new ("geometry-restore",
                  G_OBJECT_CLASS_TYPE( gobject_class ),
                  G_SIGNAL_RUN_FIRST,     /*signal_flags */
                  G_STRUCT_OFFSET (SchematicDialogClass, geometry_restore),
                  NULL, /* accumulator */
                  NULL, /* accu_data */
                  schematic_marshal_VOID__POINTER_STRING,
                  G_TYPE_NONE,
                  2,    /* n_params */
                  G_TYPE_POINTER,
                  G_TYPE_STRING
                 );

  g_object_class_install_property (
    gobject_class, PROP_SETTINGS_NAME,
    g_param_spec_string ("settings-name",
                         "",
                         "",
                         NULL,
                         (GParamFlags) (G_PARAM_CONSTRUCT_ONLY
                                        | G_PARAM_READWRITE)));
  g_object_class_install_property (
    gobject_class, PROP_SCHEMATIC_WINDOW,
    g_param_spec_pointer ("schematic-window",
                          "",
                          "",
                          (GParamFlags) (G_PARAM_CONSTRUCT_ONLY
                                         | G_PARAM_READWRITE)));
}


/*! \brief Internal GTK function modified from GTK+-2.4.14 gtkdialog.c
 *  to support schematic_dialog_new_with_buttons(...)
 *
 *  \par Function Description
 *  Convenience function which adds buttons to a pre-existing GtkDialog
 *
 *  \param [in]  dialog             The GtkDialog buttons are being added to
 *  \param [in]  first_button_text  The text string for the first button
 *  \param [in]  args               The va_list containging the remaining button strings
 */
static void
schematic_dialog_add_buttons_valist (GtkDialog *dialog,
                                     const gchar *first_button_text,
                                     va_list args)
{
  const gchar* text;
  gint response_id;

  g_return_if_fail (GTK_IS_DIALOG (dialog));

  if (first_button_text == NULL)
    return;

  text = first_button_text;
  response_id = va_arg (args, gint);

  while (text != NULL)
    {
      gtk_dialog_add_button (dialog, text, response_id);

      text = va_arg (args, gchar*);
      if (text == NULL)
        break;
      response_id = va_arg (args, int);
    }
}


/*! \brief Internal GTK function modified from GTK+-2.4.14 gtkdialog.c
 *  to support schematic_dialog_new_with_buttons(...)
 *
 *  \par Function Description
 *  Convenience function which creates a blank SchematicDialog
 *  with various options.
 *
 *  \param [in]  title              The title text of the dialog
 *  \param [in]  parent             The GtkWindow which will parent this dialog
 *  \param [in]  flags              The GtkDialogFlags to use when setting up the dialog
 *  \param [in]  settings_name      The name the program should use to store this dialog's settings
 *  \param [in]  w_current          The SchematicWindow object this dialog is associated with
 *
 *  \return  The SchematicDialog created.
 */
static GtkWidget*
schematic_dialog_new_empty (const gchar *title,
                            GtkWindow *parent,
                            GtkDialogFlags flags,
                            const gchar *settings_name,
                            SchematicWindow *w_current)
{
  SchematicDialog *dialog;

  dialog = SCHEMATIC_DIALOG (g_object_new (SCHEMATIC_TYPE_DIALOG,
                                           "settings-name", settings_name,
                                           "schematic-window", w_current,
                                           NULL));

  if (title)
    gtk_window_set_title (GTK_WINDOW (dialog), title);

  if (parent)
    gtk_window_set_transient_for (GTK_WINDOW (dialog), parent);

  if (flags & GTK_DIALOG_MODAL)
    gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);

  if (flags & GTK_DIALOG_DESTROY_WITH_PARENT)
    gtk_window_set_destroy_with_parent (GTK_WINDOW (dialog), TRUE);

  return GTK_WIDGET (dialog);
}


/*! \brief GTK function modified from GTK+-2.4.14 gtkdialog.c to
 *  provide a SchematicDialog equivalent of the convenience
 *  function gtk_dialog_new_with_buttons(...)
 *
 *  \par Function Description
 *  Convenience function which creates a SchematicDialog with
 *  buttons and options.
 *
 *  \param [in]  title              The title text of the dialog
 *  \param [in]  parent             The GtkWindow which will parent this dialog
 *  \param [in]  flags              The GtkDialogFlags to use when setting up the dialog
 *  \param [in]  settings_name      The name the program should use to store this dialog's settings
 *  \param [in]  w_current          The SchematicWindow object this dialog is associated with
 *  \param [in]  first_button_text  The text string for the first button
 *  \param [in]  ...                A variable number of arguments with the remaining button strings
 *
 *  \return  The SchematicDialog created.
 */
GtkWidget*
schematic_dialog_new_with_buttons (const gchar *title,
                                   GtkWindow *parent,
                                   GtkDialogFlags flags,
                                   const gchar *settings_name,
                                   SchematicWindow *w_current,
                                   const gchar *first_button_text,
                                   ...)
{
  SchematicDialog *dialog;
  va_list args;

  dialog = SCHEMATIC_DIALOG (schematic_dialog_new_empty (title,
                                                         parent,
                                                         flags,
                                                         settings_name,
                                                         w_current));

  va_start (args, first_button_text);

  schematic_dialog_add_buttons_valist (GTK_DIALOG (dialog),
                                       first_button_text,
                                       args);

  va_end (args);

  return GTK_WIDGET (dialog);
}
