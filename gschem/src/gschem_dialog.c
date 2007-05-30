/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2000 Ales V. Hvezda
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */

#include <config.h>

#include <glib/gstdio.h>

#include <libgeda/libgeda.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>

#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#include "../include/gschem_dialog.h"


#if !GLIB_CHECK_VERSION(2,6,0)

static inline void save_geometry (GschemDialog *dialog) { }
static inline void restore_geometry (GschemDialog *dialog) { }

#else

static GKeyFile *dialog_geometry = NULL;

#define DIALOG_GEOMETRY_STORE "gschem-dialog-geometry"


/*! \brief Save all geometry data into a file.
 *
 *  \par Function Description
 *  This is called at program exit to save all window geometry data into a file
 */
static void save_geometry_to_file()
{
  gchar *data, *file;

  g_assert( dialog_geometry != NULL );

  data = g_key_file_to_data(dialog_geometry, NULL, NULL);
  file = g_build_filename(g_get_home_dir (), ".gEDA", DIALOG_GEOMETRY_STORE,
        NULL);
  g_file_set_contents(file, data, -1, NULL);
  g_free(data);
  g_free(file);
}


/*! \brief Save dialog's current position and size.
 *
 *  \par Function Description
 *  Save the dialog's current position and size.
 *  The dialog is referenced by its unique name (the
 *  "settings-name" property).
 *
 *  \param [in] dialog  The GschemDialog to save the position and size of.
 */
static void save_geometry (GschemDialog *dialog)
{
  gint x, y, width, height;
  gchar *name;

  name = dialog->settings_name;
  if (name == NULL) return;

  g_assert( dialog_geometry != NULL );

  gtk_window_get_position (GTK_WINDOW (dialog), &x, &y);
  gtk_window_get_size (GTK_WINDOW (dialog), &width, &height);

  g_key_file_set_integer (dialog_geometry, name, "x", x);
  g_key_file_set_integer (dialog_geometry, name, "y", y);
  g_key_file_set_integer (dialog_geometry, name, "width",  width );
  g_key_file_set_integer (dialog_geometry, name, "height", height);
}


/*! \brief Restore dialog's last position and size.
 *
 *  \par Function Description
 *  Restore dialog's last position and size.
 *  If the dialog is unknown, do nothing.
 *
 *  \param [in] dialog  The GschemDialog to restore the position and size of.
 */
static void restore_geometry (GschemDialog *dialog)
{
  gchar *name;
  gint x, y, width, height;

  name = dialog->settings_name;
  if (name == NULL) return;

  if (!dialog_geometry) {
    gchar *file = g_build_filename (g_get_home_dir (), ".gEDA",
                                    DIALOG_GEOMETRY_STORE, NULL);

    dialog_geometry = g_key_file_new();

    /* Remember to save data on program exit */
    atexit(save_geometry_to_file);

    if (!g_file_test (file, G_FILE_TEST_EXISTS)) {
      gchar *dir = g_build_filename (g_get_home_dir (), ".gEDA", NULL);
      g_mkdir (dir, S_IRWXU | S_IRWXG);
      g_free (dir);

      g_file_set_contents (file, "", -1, NULL);
    }

    if (!g_key_file_load_from_file (dialog_geometry, file, G_KEY_FILE_NONE, NULL)) {
      /* error opening key file, create an empty one and try again */
      g_file_set_contents (file, "", -1, NULL);
      if ( !g_key_file_load_from_file (dialog_geometry, file, G_KEY_FILE_NONE, NULL)) {
         g_free (file);
         return;
      }
    }
    g_free (file);
  }

  if (g_key_file_has_group (dialog_geometry, name)) {
    x = g_key_file_get_integer (dialog_geometry, name, "x", NULL);
    y = g_key_file_get_integer (dialog_geometry, name, "y", NULL);
    width  = g_key_file_get_integer (dialog_geometry, name, "width",  NULL);
    height = g_key_file_get_integer (dialog_geometry, name, "height", NULL);

    gtk_window_move (GTK_WINDOW (dialog), x, y);
    gtk_window_resize (GTK_WINDOW (dialog), width, height);
  }
}

#endif   /* !GLIB_CHECK_VERSION(2,6,0) */


enum {
  PROP_SETTINGS_NAME = 1,
  PROP_TOPLEVEL
};


static GObjectClass *gschem_dialog_parent_class = NULL;


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
  GschemDialog *dialog = GSCHEM_DIALOG( widget );

  restore_geometry (dialog);

  /* Let GTK show the window */
  GTK_WIDGET_CLASS (gschem_dialog_parent_class)->show (widget);
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
  GschemDialog *dialog = GSCHEM_DIALOG (widget);

  save_geometry (dialog);

  /* Let GTK unmap the window */
  GTK_WIDGET_CLASS (gschem_dialog_parent_class)->unmap (widget);
}


/*! \brief GObject finalise handler
 *
 *  \par Function Description
 *  Just before the GschemDialog GObject is finalized, free our
 *  allocated data, and then chain up to the parent's finalize handler.
 *
 *  \param [in] widget  The GObject being finalized.
 */
static void gschem_dialog_finalize (GObject *object)
{
  GschemDialog *dialog = GSCHEM_DIALOG (object);

  if (dialog->settings_name) g_free (dialog->settings_name);

  G_OBJECT_CLASS (gschem_dialog_parent_class)->finalize (object);
}


/*! \brief GObject property setter function
 *
 *  \par Function Description
 *  Setter function for GschemDialog's GObject properties,
 *  "settings-name" and "toplevel".
 *
 *  \param [in]  object       The GObject whose properties we are setting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [in]  value        The GValue the property is being set from
 *  \param [in]  pspec        A GParamSpec describing the property being set
 */
static void gschem_dialog_set_property (GObject *object, guint property_id, const GValue *value, GParamSpec *pspec)
{
  GschemDialog *dialog = GSCHEM_DIALOG (object);

  switch(property_id) {
    case PROP_SETTINGS_NAME:
      if (dialog->settings_name) g_free (dialog->settings_name);
      dialog->settings_name = g_strdup (g_value_get_string (value));
      break;
    case PROP_TOPLEVEL:
      dialog->toplevel = (TOPLEVEL*)g_value_get_pointer (value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }

}


/*! \brief GObject property getter function
 *
 *  \par Function Description
 *  Getter function for GschemDialog's GObject properties,
 *  "settings-name" and "toplevel".
 *
 *  \param [in]  object       The GObject whose properties we are getting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [out] value        The GValue in which to return the value of the property
 *  \param [in]  pspec        A GParamSpec describing the property being got
 */
static void gschem_dialog_get_property (GObject *object, guint property_id, GValue *value, GParamSpec *pspec)
{
  GschemDialog *dialog = GSCHEM_DIALOG (object);

  switch(property_id) {
      case PROP_SETTINGS_NAME:
        g_value_set_string (value, dialog->settings_name);
        break;
      case PROP_TOPLEVEL:
        g_value_set_pointer (value, (gpointer)dialog->toplevel);
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }

}


/*! \brief GType instance initialiser for GschemDialog
 *
 *  \par Function Description
 *  GType instance initialiser for GschemDialog. Nothing to do here.
 *
 *  \param [in]  dialog       The GschemDialog we are initialising
 */
static void gschem_dialog_init (GschemDialog *dialog)
{
}


/*! \brief GType class initialiser for GschemDialog
 *
 *  \par Function Description
 *  GType class initialiser for GschemDialog. We override our parent
 *  virtual class methods as needed and register our GObject properties.
 *
 *  \param [in]  klass       The GschemDialogClass we are initialising
 */
static void gschem_dialog_class_init (GschemDialogClass *klass)
{
  GtkWidgetClass *gtkwidget_class = GTK_WIDGET_CLASS (klass);
  GObjectClass     *gobject_class =   G_OBJECT_CLASS (klass);

  gschem_dialog_parent_class = g_type_class_peek_parent (klass);

  gtkwidget_class->show        = show_handler;
  gtkwidget_class->unmap       = unmap_handler;

  gobject_class->finalize      = gschem_dialog_finalize;
  gobject_class->set_property  = gschem_dialog_set_property;
  gobject_class->get_property  = gschem_dialog_get_property;

  g_object_class_install_property (
    gobject_class, PROP_SETTINGS_NAME,
    g_param_spec_string ("settings-name",
                         "",
                         "",
                         NULL,
                         G_PARAM_CONSTRUCT_ONLY | G_PARAM_READWRITE));
  g_object_class_install_property (
    gobject_class, PROP_TOPLEVEL,
    g_param_spec_pointer ("toplevel",
                          "",
                          "",
                          G_PARAM_CONSTRUCT_ONLY | G_PARAM_READWRITE));
}


/*! \brief Function to retrieve GschemDialog's GType identifier.
 *
 *  \par Function Description
 *  Function to retrieve GschemDialog's GType identifier.
 *  Upon first call, this registers the GschemDialog in the GType system.
 *  Subsequently it returns the saved value from its first execution.
 *
 *  \return the GType identifier associated with GschemDialog.
 */
GType gschem_dialog_get_type ()
{
  static GType gschem_dialog_type = 0;

  if (!gschem_dialog_type) {
    static const GTypeInfo gschem_dialog_info = {
      sizeof(GschemDialogClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) gschem_dialog_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof(GschemDialog),
      0,    /* n_preallocs */
      (GInstanceInitFunc) gschem_dialog_init,
    };

    gschem_dialog_type = g_type_register_static (GTK_TYPE_DIALOG,
                                                 "GschemDialog",
                                                 &gschem_dialog_info, 0);
  }

  return gschem_dialog_type;
}


/*! \brief Internal GTK function modified from GTK+-2.4.14 gtkdialog.c
 *  to support gschem_dialog_new_with_buttons(...)
 *
 *  \par Function Description
 *  Convenience function which adds buttons to a pre-existing GtkDialog
 *
 *  \param [in]  dialog             The GtkDialog buttons are being added to
 *  \param [in]  first_button_text  The text string for the first button
 *  \param [in]  args               The va_list containging the remaining button strings
 */
static void gschem_dialog_add_buttons_valist (GtkDialog      *dialog,
                                              const gchar    *first_button_text,
                                              va_list         args)
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
 *  to support gschem_dialog_new_with_buttons(...)
 *
 *  \par Function Description
 *  Convenience function which creates a blank GschemDialog with various options.
 *
 *  \param [in]  title              The title text of the dialog
 *  \param [in]  parent             The GtkWindow which will parent this dialog
 *  \param [in]  flags              The GtkDialogFlags to use when setting up the dialog
 *  \param [in]  settings_name      The name gschem should use to store this dialog's settings
 *  \param [in]  toplevel           The TOPLEVEL object this dialog is associated with
 *
 *  \return  The GschemDialog created.
 */
static GtkWidget* gschem_dialog_new_empty (const gchar     *title,
                                           GtkWindow       *parent,
                                           GtkDialogFlags   flags,
                                           const gchar *settings_name,
                                           TOPLEVEL *toplevel)
{
  GschemDialog *dialog;

  dialog = g_object_new (GSCHEM_TYPE_DIALOG,
                         "settings-name", settings_name,
                         "toplevel", toplevel,
                         NULL);

  if (title)
    gtk_window_set_title (GTK_WINDOW (dialog), title);

  if (parent)
    gtk_window_set_transient_for (GTK_WINDOW (dialog), parent);

  if (flags & GTK_DIALOG_MODAL)
    gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);

  if (flags & GTK_DIALOG_DESTROY_WITH_PARENT)
    gtk_window_set_destroy_with_parent (GTK_WINDOW (dialog), TRUE);

  if (flags & GTK_DIALOG_NO_SEPARATOR)
    gtk_dialog_set_has_separator (GTK_DIALOG (dialog), FALSE);

  return GTK_WIDGET (dialog);
}


/*! \brief GTK function modified from GTK+-2.4.14 gtkdialog.c
 *  to provide a GschemDialog equivelant of the convenience function
 *  gtk_dialog_new_with_buttons(...)
 *
 *  \par Function Description
 *  Convenience function which creates a GschemDialog with buttons and options.
 *
 *  \param [in]  title              The title text of the dialog
 *  \param [in]  parent             The GtkWindow which will parent this dialog
 *  \param [in]  flags              The GtkDialogFlags to use when setting up the dialog
 *  \param [in]  settings_name      The name gschem should use to store this dialog's settings
 *  \param [in]  toplevel           The TOPLEVEL object this dialog is associated with
 *  \param [in]  first_button_text  The text string for the first button
 *  \param [in]  ...                A variable number of arguments with the remaining button strings
 *
 *  \return  The GschemDialog created.
 */
GtkWidget* gschem_dialog_new_with_buttons (const gchar *title, GtkWindow *parent, GtkDialogFlags flags,
                                           const gchar *settings_name, TOPLEVEL *toplevel,
                                           const gchar *first_button_text, ...)
{
  GschemDialog *dialog;
  va_list args;

  dialog = GSCHEM_DIALOG (gschem_dialog_new_empty (title, parent, flags, settings_name, toplevel));

  va_start (args, first_button_text);

  gschem_dialog_add_buttons_valist (GTK_DIALOG (dialog),
                                    first_button_text,
                                    args);

  va_end (args);

  return GTK_WIDGET (dialog);
}

