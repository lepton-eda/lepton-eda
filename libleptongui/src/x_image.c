/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
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

#include <config.h>
#include <math.h>
#include "gschem.h"


extern LeptonColorMap display_colors;

#define X_IMAGE_DEFAULT_SIZE "800x600"
#define X_IMAGE_DEFAULT_TYPE "PNG"

static const char *x_image_sizes[] =
{
  "320x240",
  "640x480",
  "800x600",
  "1024x768",
  "1200x768",
  "1280x960",
  "1600x1200",
  "3200x2400",
  NULL
};



/*! \brief Restore last selected item in \a combo combobox
 *
 *  \par Function Description
 *  Helper function used in settings_restore().
 */
static void
settings_restore_combo (EdaConfig*   cfg,
                        GtkComboBox* combo,
                        const gchar* group,
                        const gchar* key)
{
  GtkTreeModel* model = gtk_combo_box_get_model (combo);
  gint          count = gtk_tree_model_iter_n_children (model, NULL);

  GError* err = NULL;
  gint index = eda_config_get_int (cfg, group, key, &err);

  if (err == NULL && index >= 0 && index < count)
  {
    gtk_combo_box_set_active (combo, index);
  }

  g_clear_error (&err);
}



/*! \brief Restore "Write image" dialog settings
 *
 *  \par Function Description
 *  Load the following settings from the CACHE configuration
 *  context ("schematic.write-image-dialog" group):
 *  - selected directory
 *  - image size
 *  - image type
 *  - image color mode
 *
 *  \note Call this function after the dialog is fully constructed.
 *
*   \param dialog      "Write image" dialog widget
 *  \param size_combo  Combo box widget with list of image sizes
 *  \param type_combo  Combo box widget with list of image types
 *  \param color_combo Combo box widget with list of color modes
 */
static void
settings_restore (GtkFileChooser* dialog,
                  GtkComboBox*    size_combo,
                  GtkComboBox*    type_combo,
                  GtkComboBox*    color_combo)
{
  EdaConfig* cfg = eda_config_get_cache_context();
  GError*    err = NULL;

  gchar* dir = eda_config_get_string (cfg,
                                      "schematic.write-image-dialog",
                                      "save-path",
                                      &err);
  if (err == NULL && dir != NULL)
  {
    gtk_file_chooser_set_current_folder (dialog, dir);
    g_free (dir);
  }

  settings_restore_combo (cfg,
                          size_combo,
                          "schematic.write-image-dialog",
                          "image-size");
  settings_restore_combo (cfg,
                          type_combo,
                          "schematic.write-image-dialog",
                          "image-type");
  settings_restore_combo (cfg,
                          color_combo,
                          "schematic.write-image-dialog",
                          "image-color");
}



/*! \brief Save "Write image" dialog settings
 *
 *  \par Function Description
 *  Save the following settings to the CACHE configuration
 *  context ("schematic.write-image-dialog" group):
 *  - selected directory
 *  - image size
 *  - image type
 *  - image color mode
 *
*   \param dialog      "Write image" dialog widget
 *  \param size_combo  Combo box widget with list of image sizes
 *  \param type_combo  Combo box widget with list of image types
 *  \param color_combo Combo box widget with list of color modes
 */
static void
settings_save (GtkFileChooser* dialog,
               GtkComboBox*    size_combo,
               GtkComboBox*    type_combo,
               GtkComboBox*    color_combo)
{
  EdaConfig* cfg = eda_config_get_cache_context();

  gchar* dir = gtk_file_chooser_get_current_folder (dialog);
  if (dir != NULL)
  {
    eda_config_set_string (cfg,
                           "schematic.write-image-dialog",
                           "save-path",
                           dir);
    g_free (dir);
  }

  eda_config_set_int (cfg,
                      "schematic.write-image-dialog",
                      "image-size",
                      gtk_combo_box_get_active (size_combo));
  eda_config_set_int (cfg,
                      "schematic.write-image-dialog",
                      "image-type",
                      gtk_combo_box_get_active (type_combo));
  eda_config_set_int (cfg,
                      "schematic.write-image-dialog",
                      "image-color",
                      gtk_combo_box_get_active (color_combo));

  eda_config_save (cfg, NULL);
}



/*! \brief Create the options of the image size combobox
 *  \par This function adds the options of the image size to the given combobox.
 *  \param combo [in] the combobox to add the options to.
 *  \return nothing
 *  \note
 *  This function is only used in this file, there are other create_menus...
 */
static void create_size_menu (GtkComboBox *combo)
{
  char *buf;
  char *default_size;
  int i, default_index = 0;

  default_size = g_strdup (X_IMAGE_DEFAULT_SIZE);
  for (i=0; x_image_sizes[i] != NULL;i++) {
    /* Create a new string and add it as an option*/
    buf = g_strdup (x_image_sizes[i]);
    gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (combo), buf);

    /* Compare with the default size, to get the default index */
    if (strcasecmp(buf, default_size ) == 0) {
      default_index = i;
    }
    g_free(buf);
  }
  g_free(default_size);

  /* Set the default menu */
  gtk_combo_box_set_active(GTK_COMBO_BOX (combo), default_index);
}

/*! \brief Create the options of the image type combobox
 *  \par This function adds the options of the image type to the given combobox.
 *  \param combo [in] the combobox to add the options to.
 *  \return nothing
 *  \note
 *  This function is only used in this file, there are other create_menus...
 */
static void create_type_menu (GtkComboBoxText *combo)
{
  GSList *formats = gdk_pixbuf_get_formats ();
  GSList *ptr;
  char *buf;
  int i=0, default_index=0;

  ptr = formats;
  while (ptr) {
    if (gdk_pixbuf_format_is_writable ((GdkPixbufFormat*) ptr->data)) {
      /* Get the format description and add it to the menu */
      buf = g_strdup (gdk_pixbuf_format_get_description ((GdkPixbufFormat*) ptr->data));
      gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (combo), buf);

      /* Compare the name with "png" and store the index */
      buf = g_strdup (gdk_pixbuf_format_get_name ((GdkPixbufFormat*) ptr->data));
      if (strcasecmp(buf, X_IMAGE_DEFAULT_TYPE) == 0) {
        default_index = i;
      }
      i++;  /* this is the count of items added to the combo box */
      /* not the total number of pixbuf formats */
      g_free(buf);
    }
    ptr = ptr->next;
  }
  g_slist_free (formats);
  gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (combo), "Portable Document Format");

  /* Set the default menu */
  gtk_combo_box_set_active(GTK_COMBO_BOX(combo), default_index);
}

/*! \brief Given a gdk-pixbuf image type description, it returns the type,
 *  or extension of the image.
 *  \par Return the gdk-pixbuf image type, or extension, which has the
 *  given gdk-pixbuf description.
 *  \param description The gdk-pixbuf image type description.
 *  \return The gdk-pixbuf type, or extension, of the image.
 *  \note This function is only used in this file.
 *  \note The caller must g_free the result of this function.
 */
static gchar*
x_image_get_type_from_description (const char *description)
{
  GSList *ptr;
  gchar *image_type = NULL;

  if (strcmp (description, "Portable Document Format") == 0) {
    image_type = g_strdup ("pdf");
  } else {
    ptr = gdk_pixbuf_get_formats ();

    while (ptr != NULL) {
      gchar *ptr_descr = gdk_pixbuf_format_get_description ((GdkPixbufFormat*) ptr->data);

      if (ptr_descr && (strcasecmp (ptr_descr, description) == 0)) {
        image_type = gdk_pixbuf_format_get_name ((GdkPixbufFormat*) ptr->data);
      }

      ptr = g_slist_next (ptr);
    }
  }

  return image_type;
}

/*! \brief Update the filename of a file dialog, when the image type has changed.
 *  \par Given a combobox inside a file chooser dialog, this function updates
 *  the filename displayed by the dialog, removing the current extension, and
 *  adding the extension of the image type selected.
 *  \param combo     [in] A combobox inside a file chooser dialog, with gdk-pixbuf image type descriptions.
 *  \param w_current [in] the GschemToplevel structure.
 *  \return nothing.
 *
 */
static void
x_image_update_dialog_filename (GtkComboBoxText *combo,
                                GschemToplevel *w_current) {
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
  char* image_type_descr = NULL;
  gchar *image_type = NULL;
  const char *old_image_filename = NULL;
  char *file_basename = NULL;
  char *file_name = NULL ;
  char *new_image_filename = NULL;
  GtkWidget *file_chooser;

  /* Get the current image type */
  image_type_descr =
    gtk_combo_box_text_get_active_text (GTK_COMBO_BOX_TEXT (combo));
  image_type = x_image_get_type_from_description(image_type_descr);

  /* Get the parent dialog */
  file_chooser = gtk_widget_get_ancestor(GTK_WIDGET(combo),
      GTK_TYPE_FILE_CHOOSER);

  /* Get the previous file name. If none, revert to the page filename */
  old_image_filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(file_chooser));
  if (!old_image_filename) {
    old_image_filename = lepton_page_get_filename (toplevel->page_current);
  }

  /* Get the file name, without extension */
  if (old_image_filename) {
    file_basename = g_path_get_basename(old_image_filename);

    if (g_strrstr(file_basename, ".") != NULL) {
      file_name = g_strndup(file_basename,
          g_strrstr(file_basename, ".") - file_basename);
    }
  }

  /* Add the extension */
  if (file_name) {
    new_image_filename = g_strdup_printf("%s.%s", file_name,
        image_type);
  } else {
    new_image_filename = g_strdup_printf("%s.%s", file_basename,
        image_type);
  }

  g_free (image_type);

  /* Set the new filename */
  if (file_chooser) {
    gtk_file_chooser_set_current_name(GTK_FILE_CHOOSER(file_chooser),
        new_image_filename);
  } else {
    g_message ("x_image_update_dialog_filename: No parent file chooser found!.");
  }

  g_free(file_name);
  g_free(file_basename);
  g_free(new_image_filename);
}

/*! \brief Write the image file, with the desired options.
 *  \par This function writes the image file, with the options set in the
 *  dialog by the user.
 *  \param w_current [in] the GschemToplevel structure.
 *  \param filename  [in] the image filename.
 *  \param width     [in] the image width chosen by the user.
 *  \param height    [in] the image height chosen by the user.
 *  \param filetype  [in] image filetype.
 *  \param is_color  [in] write image using colors (TRUE) or in grayscale (FALSE).
 *  \return nothing
 *
 */
void x_image_lowlevel(GschemToplevel *w_current, const char* filename,
                       int width, int height, const char *filetype, gboolean is_color)
{
  int save_page_left, save_page_right, save_page_top, save_page_bottom;
  int page_width, page_height, page_center_left, page_center_top;
  GdkPixbuf *pixbuf;
  GError *gerror = NULL;
  GtkWidget *dialog;
  float prop;
  GschemPageView *view = gschem_toplevel_get_current_page_view (w_current);

  GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);
  g_return_if_fail (geometry != NULL);

  /* Save geometry */
  save_page_left = geometry->viewport_left;
  save_page_right = geometry->viewport_right;
  save_page_top = geometry->viewport_top;
  save_page_bottom = geometry->viewport_bottom;

  page_width = geometry->viewport_right - geometry->viewport_left;
  page_height = geometry->viewport_bottom - geometry->viewport_top;

  page_center_left = geometry->viewport_left + (page_width / 2);
  page_center_top = geometry->viewport_top + (page_height / 2);

  /* Preserve proportions */
  prop = (float)width / height;
  if (((float)page_width / page_height) > prop) {
    page_height = (page_width / prop);
  }else{
    page_width = (page_height * prop);
  }

  gschem_page_geometry_set_viewport_left   (geometry, page_center_left - (page_width / 2));
  gschem_page_geometry_set_viewport_right  (geometry, page_center_left + (page_width / 2));
  gschem_page_geometry_set_viewport_top    (geometry, page_center_top - (page_height / 2));
  gschem_page_geometry_set_viewport_bottom (geometry, page_center_top + (page_height / 2));

  /* de select everything first */
  o_select_unselect_all( w_current );

  if (strcmp(filetype, "pdf") == 0)
    x_print_export_pdf (w_current, filename, is_color);
  else {
    pixbuf = x_image_get_pixbuf(w_current, width, height, is_color);
    if (pixbuf != NULL) {
      if (!gdk_pixbuf_save(pixbuf, filename, filetype, &gerror, NULL)) {
        g_message ("x_image_lowlevel: ");
        g_message (_("Unable to write %1$s file %2$s."),
                   filetype, filename);
        g_message ("%s", gerror->message);

        /* Warn the user */
        dialog = gtk_message_dialog_new (GTK_WINDOW(w_current->main_window),
                                         (GtkDialogFlags) (GTK_DIALOG_MODAL
                                                           | GTK_DIALOG_DESTROY_WITH_PARENT),
                                         GTK_MESSAGE_ERROR,
                                         GTK_BUTTONS_OK,
                                         _("There was the following error when saving image with type %1$s to filename:\n%2$s\n\n%3$s.\n"),
                                         filetype,
                                         filename,
                                         gerror->message);

        gtk_dialog_run (GTK_DIALOG (dialog));
        gtk_widget_destroy (dialog);

        /* Free the gerror */
        g_error_free(gerror);
        gerror = NULL;

        /* Unlink the output file */
        /* It's not safe to unlink the file if there was an error.
           For example: if the operation was not allowed due to permissions,
           the _previous existing_ file will be removed */
        /* unlink(filename); */
      }
      else {
        if (is_color) {
          g_message (_("Wrote color image to [%1$s] [%2$d x %3$d]"), filename, width, height);
        } else {
          g_message (_("Wrote black and white image to [%1$s] [%2$d x %3$d]"), filename, width, height);
        }
      }

      if (pixbuf != NULL)
        g_object_unref(pixbuf);
    }
    else {
      g_message ("x_image_lowlevel: ");
      g_message (_("Unable to get pixbuf from lepton-schematic's window."));
    }
  }

  /* Restore geometry */
  gschem_page_geometry_set_viewport_left   (geometry, save_page_left  );
  gschem_page_geometry_set_viewport_right  (geometry, save_page_right );
  gschem_page_geometry_set_viewport_top    (geometry, save_page_top   );
  gschem_page_geometry_set_viewport_bottom (geometry, save_page_bottom);

  gschem_page_view_invalidate_all (view);
}

/*! \brief Display the image file selection dialog.
 *  \par Display the image file selection dialog, allowing the user to
 *  set several options, like image size and image type.
 *  When the user hits "ok", then it writes the image file.
 *  \param w_current [in] the GschemToplevel structure.
 *  \return nothing
 */
void x_image_setup (GschemToplevel *w_current)
{
  GtkWidget *dialog;
  GtkWidget *vbox1;
  GtkWidget *hbox;
  GtkWidget *label1;
  GtkWidget *size_combo;
  GtkWidget *vbox2;
  GtkWidget *label2;
  GtkWidget *type_combo;
  char *image_type_descr;
  gchar *filename, *image_type;
  char *image_size;
  int width, height;

#ifdef ENABLE_GTK3
  hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
  GtkWidget* vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
  GtkWidget* hbox2 = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
  hbox = gtk_hbox_new(FALSE, 0);
  GtkWidget* vbox = gtk_vbox_new (FALSE, 0);
  GtkWidget* hbox2 = gtk_hbox_new (FALSE, 0);
#endif

  GtkWidget* label = gtk_label_new(
    _("NOTE: print-color-map will be used for PDF export"));
  gtk_box_pack_start (GTK_BOX (hbox2), label, FALSE, FALSE, 10);
  gtk_box_pack_start (GTK_BOX (vbox),  hbox2, FALSE, FALSE, 5);
  gtk_box_pack_start (GTK_BOX (vbox),  hbox,  FALSE, FALSE, 0);
  gtk_widget_show_all (vbox);


  /* Image size selection */
#ifdef ENABLE_GTK3
  vbox1 = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
  vbox1 = gtk_vbox_new(TRUE, 0);
#endif
  label1 = gtk_label_new (_("Width x Height"));
  gtk_widget_show (label1);
  gtk_misc_set_alignment( GTK_MISC (label1), 0, 0);
  gtk_misc_set_padding (GTK_MISC (label1), 0, 0);
  gtk_box_pack_start (GTK_BOX (vbox1),
      label1, FALSE, FALSE, 0);

  size_combo =  gtk_combo_box_text_new ();
  create_size_menu (GTK_COMBO_BOX(size_combo));

  gtk_widget_show (size_combo);
  gtk_box_pack_start (GTK_BOX (vbox1), size_combo, TRUE, TRUE, 0);
  gtk_widget_show(vbox1);

  /* Image type selection */
#ifdef ENABLE_GTK3
  vbox2 = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
  vbox2 = gtk_vbox_new(TRUE, 0);
#endif
  label2 = gtk_label_new (_("Image type"));
  gtk_widget_show (label2);
  gtk_misc_set_alignment( GTK_MISC (label2), 0, 0);
  gtk_misc_set_padding (GTK_MISC (label2), 0, 0);
  gtk_box_pack_start (GTK_BOX (vbox2),
      label2, FALSE, FALSE, 0);

  type_combo = gtk_combo_box_text_new ();
  gtk_box_pack_start (GTK_BOX (vbox2), type_combo, TRUE, TRUE, 0);
  create_type_menu (GTK_COMBO_BOX_TEXT (type_combo));


  /* Color/grayscale selection:
  */
#ifdef ENABLE_GTK3
  GtkWidget* vbox3 = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
  GtkWidget* vbox3 = gtk_vbox_new (TRUE, 0);
#endif
  GtkWidget* label3 = gtk_label_new (_("Color mode"));
  gtk_misc_set_alignment (GTK_MISC (label3), 0, 0);
  gtk_misc_set_padding (GTK_MISC (label3), 0, 0);
  gtk_box_pack_start (GTK_BOX (vbox3), label3, FALSE, FALSE, 0);

  GtkWidget* color_combo = gtk_combo_box_text_new();
  gtk_box_pack_start (GTK_BOX (vbox3), color_combo, TRUE, TRUE, 0);
  gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (color_combo), _("Color"));
  gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (color_combo), _("Grayscale"));
  gtk_combo_box_set_active (GTK_COMBO_BOX (color_combo), 0);

  gtk_widget_show_all (vbox3);


  /* Connect the changed signal to the callback, so the filename
     gets updated every time the image type is changed */
  g_signal_connect (type_combo, "changed",
      G_CALLBACK(x_image_update_dialog_filename),
      w_current);

  gtk_widget_show (type_combo);
  gtk_widget_show(vbox2);

  /* Create the dialog */
  dialog = gtk_file_chooser_dialog_new (_("Write Image"),
      GTK_WINDOW(w_current->main_window),
      GTK_FILE_CHOOSER_ACTION_SAVE,
      GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
      GTK_STOCK_SAVE,   GTK_RESPONSE_ACCEPT,
      NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
      GTK_RESPONSE_ACCEPT,
      GTK_RESPONSE_CANCEL,
      -1);

  /* Add the extra widgets to the dialog*/
  gtk_box_pack_start(GTK_BOX(hbox), vbox1, FALSE, FALSE, 10);
  gtk_box_pack_start(GTK_BOX(hbox), vbox2, FALSE, FALSE, 10);
  gtk_box_pack_start(GTK_BOX(hbox), vbox3, FALSE, FALSE, 10);

  gtk_file_chooser_set_extra_widget (GTK_FILE_CHOOSER(dialog), vbox);

  g_object_set (dialog,
      /* GtkFileChooser */
      "select-multiple", FALSE,
      /* only in GTK 2.8 */
      "do-overwrite-confirmation", TRUE,
      NULL);

  /* Update the filename */
  x_image_update_dialog_filename (GTK_COMBO_BOX_TEXT (type_combo), w_current);

  gtk_dialog_set_default_response(GTK_DIALOG(dialog),
      GTK_RESPONSE_ACCEPT);

  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);

  gtk_container_set_border_width(GTK_CONTAINER(dialog),
      DIALOG_BORDER_SPACING);
  gtk_box_set_spacing (GTK_BOX (gtk_dialog_get_content_area (GTK_DIALOG (dialog))),
                       DIALOG_V_SPACING);


  settings_restore (GTK_FILE_CHOOSER (dialog),
                    GTK_COMBO_BOX (size_combo),
                    GTK_COMBO_BOX (type_combo),
                    GTK_COMBO_BOX (color_combo));


  gtk_widget_show (dialog);

  if (gtk_dialog_run((GTK_DIALOG(dialog))) == GTK_RESPONSE_ACCEPT) {
    image_size =
      gtk_combo_box_text_get_active_text (GTK_COMBO_BOX_TEXT (size_combo));

    image_type_descr =
      gtk_combo_box_text_get_active_text (GTK_COMBO_BOX_TEXT (type_combo));

    image_type = x_image_get_type_from_description(image_type_descr);
    sscanf(image_size, "%ix%i", &width, &height);
    filename = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog));

    /* the first item in the color_combo is "Color", 2nd = "Grayscale":
    */
    gboolean is_color = gtk_combo_box_get_active (GTK_COMBO_BOX (color_combo)) == 0;

    x_image_lowlevel(w_current, filename, width, height, image_type, is_color);

    g_free (filename);
    g_free (image_type);


    settings_save (GTK_FILE_CHOOSER (dialog),
                   GTK_COMBO_BOX (size_combo),
                   GTK_COMBO_BOX (type_combo),
                   GTK_COMBO_BOX (color_combo));
  }

  gtk_widget_destroy (dialog);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void x_image_convert_to_greyscale(GdkPixbuf *pixbuf)
{
  int width, height, rowstride, n_channels;
  guchar *pixels, *p, new_value;
  int i, j;

  n_channels = gdk_pixbuf_get_n_channels (pixbuf);

  if (n_channels != 3)
  {
    return;
  }

  if (gdk_pixbuf_get_colorspace (pixbuf) != GDK_COLORSPACE_RGB)
  {
    return;
  }

  if (gdk_pixbuf_get_bits_per_sample (pixbuf) != 8)
  {
    return;
  }

  width = gdk_pixbuf_get_width (pixbuf);
  height = gdk_pixbuf_get_height (pixbuf);

  rowstride = gdk_pixbuf_get_rowstride (pixbuf);
  pixels = gdk_pixbuf_get_pixels (pixbuf);

  for (j = 0; j < height; j++)
  {
    for (i = 0; i < width; i++)
    {
      p = pixels + j * rowstride + i * n_channels;

      new_value = 0.3 * p[0] + 0.59 * p[1] + 0.11 * p[2];
      p[0] = new_value;
      p[1] = new_value;
      p[2] = new_value;
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
#ifdef ENABLE_GTK3
GdkPixbuf
*x_image_get_pixbuf (GschemToplevel *w_current, int width, int height, gboolean is_color)
{
  GdkPixbuf *pixbuf;
  GschemPageView *page_view;
  LeptonPage *page;
  int origin_x, origin_y, bottom, right;
  GschemPageGeometry *old_geometry, *new_geometry;

  GList *obj_list;
  GList *iter;
  LeptonBox *world_rect;
  EdaRenderer *renderer;
  int render_flags;
  GArray *render_color_map = NULL;

  cairo_surface_t *cs;
  cairo_t *cr;

  g_return_val_if_fail (w_current != NULL, NULL);

  page_view = gschem_toplevel_get_current_page_view (w_current);

  g_return_val_if_fail (page_view != NULL, NULL);

  page = gschem_page_view_get_page (page_view);

  g_return_val_if_fail (page != NULL, NULL);

  old_geometry = gschem_page_view_get_page_geometry (page_view);

  cs = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, width, height);
  cr = cairo_create (cs);

  origin_x = origin_y = 0;
  right = width;
  bottom = height;

  new_geometry =
    gschem_page_geometry_new_with_values (width,
                                          height,
                                          old_geometry->viewport_left,
                                          old_geometry->viewport_top,
                                          old_geometry->viewport_right,
                                          old_geometry->viewport_bottom,
                                          WORLD_DEFAULT_LEFT,
                                          WORLD_DEFAULT_TOP,
                                          WORLD_DEFAULT_RIGHT,
                                          WORLD_DEFAULT_BOTTOM);

  cairo_set_matrix (cr, gschem_page_geometry_get_world_to_screen_matrix (new_geometry));

  world_rect = g_new (LeptonBox, 1);

  double lower_x = 0;
  double lower_y = height;
  double upper_x = width;
  double upper_y = 0;

  cairo_device_to_user (cr, &lower_x, &lower_y);
  cairo_device_to_user (cr, &upper_x, &upper_y);

  world_rect->lower_x = floor (lower_x);
  world_rect->lower_y = floor (lower_y);
  world_rect->upper_x = ceil (upper_x);
  world_rect->upper_y = ceil (upper_y);

  gboolean show_hidden_text =
    gschem_toplevel_get_show_hidden_text (w_current);

  obj_list = lepton_page_objects_in_regions (NULL, /* unused 'toplevel' parameter */
                                             page,
                                             world_rect,
                                             1,
                                             show_hidden_text);

  g_free (world_rect);

  /* Set up renderer based on configuration in w_current */
  render_flags = EDA_RENDERER_FLAG_HINTING;
  if (show_hidden_text)
    render_flags |= EDA_RENDERER_FLAG_TEXT_HIDDEN;

  /* This color map is used for "normal" rendering. */
  render_color_map =
    g_array_sized_new (FALSE, FALSE, sizeof(LeptonColor), colors_count());
  render_color_map =
    g_array_append_vals (render_color_map, display_colors, colors_count());

  /* Set up renderer */
  renderer = eda_renderer_new (NULL, NULL);
  g_object_set (G_OBJECT (renderer),
                "cairo-context", cr,
                "render-flags", render_flags,
                "color-map", render_color_map,
                NULL);

  /* Paint background */
  LeptonColor *color = x_color_lookup (BACKGROUND_COLOR);

  cairo_set_source_rgba (cr,
                         lepton_color_get_red_double (color),
                         lepton_color_get_green_double (color),
                         lepton_color_get_blue_double (color),
                         lepton_color_get_alpha_double (color));

  cairo_paint (cr);
  cairo_destroy (cr);

  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    LeptonObject *o_current = (LeptonObject*) iter->data;

    if (!o_current->dont_redraw) {
      eda_renderer_draw (renderer, o_current);
    }
  }

  g_list_free (obj_list);
  g_object_unref (G_OBJECT (renderer));
  g_array_free (render_color_map, TRUE);

  gschem_page_geometry_free (new_geometry);

  /* Get the pixbuf */
  pixbuf = gdk_pixbuf_get_from_surface (cs, 0, 0, right-origin_x, bottom-origin_y);
  cairo_surface_destroy (cs);

  if (!is_color)
  {
    x_image_convert_to_greyscale(pixbuf);
  }

  return(pixbuf);
}

#else /* GTK2 */

GdkPixbuf
*x_image_get_pixbuf (GschemToplevel *w_current, int width, int height, gboolean is_color)
{
  GdkPixbuf *pixbuf;
  GschemPageView *page_view;
  int origin_x, origin_y, bottom, right;
  GschemToplevel new_w_current;
  GschemOptions options;
  LeptonToplevel toplevel;
  GdkRectangle rect;
  GschemPageGeometry *old_geometry, *new_geometry;
  GdkPixmap *window = NULL;

  page_view = gschem_toplevel_get_current_page_view (w_current);

  old_geometry = gschem_page_view_get_page_geometry (page_view);

  /* Do a copy of the w_current struct and work with it */
  memcpy(&new_w_current, w_current, sizeof(GschemToplevel));
  /* Do a copy of the options struct and work with it */
  memcpy(&options, w_current->options, sizeof(GschemOptions));
  /* Do a copy of the toplevel struct and work with it */
  memcpy(&toplevel, w_current->toplevel, sizeof(LeptonToplevel));

  new_w_current.toplevel = &toplevel;
  new_w_current.options = &options;

  window = gdk_pixmap_new (gtk_widget_get_window (GTK_WIDGET(page_view)), width, height, -1);

  gschem_options_set_grid_mode (new_w_current.options, GRID_MODE_NONE);

  /*! \bug Need to handle image color setting properly.
   *       See gEDA Launchpad bug 1086530.
   *
   * if (toplevel.image_color == FALSE)
   * {
   * }
  */

  origin_x = origin_y = 0;
  right = width;
  bottom = height;

  rect.x = origin_x;
  rect.y = origin_y;
  rect.width = right - origin_x;
  rect.height = bottom - origin_y;

  new_geometry =
    gschem_page_geometry_new_with_values (width,
                                          height,
                                          old_geometry->viewport_left,
                                          old_geometry->viewport_top,
                                          old_geometry->viewport_right,
                                          old_geometry->viewport_bottom,
                                          WORLD_DEFAULT_LEFT,
                                          WORLD_DEFAULT_TOP,
                                          WORLD_DEFAULT_RIGHT,
                                          WORLD_DEFAULT_BOTTOM);

  o_redraw_rect (&new_w_current,
                 window,
                 toplevel.page_current,
                 new_geometry,
                 &rect);

  gschem_page_geometry_free (new_geometry);

  /* Get the pixbuf */
  pixbuf = gdk_pixbuf_get_from_drawable (NULL, window, NULL,
                                        origin_x, origin_y, 0, 0,
                                        right-origin_x,
                                        bottom-origin_y);

  if (!is_color)
  {
    x_image_convert_to_greyscale(pixbuf);
  }

  if (window != NULL) {
    g_object_unref(window);
  }

  return(pixbuf);
}
#endif
