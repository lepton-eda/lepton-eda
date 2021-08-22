/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
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

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <math.h>

#include "gschem.h"
#include <cairo-pdf.h>

#define DEFAULT_PDF_SIZE 256
/* PDF scale used by Adobe is fixed and equal to 72 ppi. */
#define DEFAULT_ADOBE_PDF_PPI 72
/* Gschem scale is not defined by any standard, so to make things
 * more definite as we must have reasonable real sizes while
 * exporting to PDF, let's use the proposition at
 * http://wiki.geda-project.org/geda:file_format_spec
 * and set 1 gschem point = 1 mil, i.e. 1000 points = 1 inch */
#define DEFAULT_GSCHEM_PPI 1000

#define CFG_GROUP_PRINTING "schematic.printing"
#define CFG_KEY_PRINTING_ORIENTATION "layout"
#define CFG_KEY_PRINTING_PAPER "paper"
#define CFG_KEY_PRINTING_MONOCHROME "monochrome"

/*! \brief Create a default page setup for a schematic page.
 * \par Function Description
 * Creates and returns a new #GtkPageSetup for \a page, taking into
 * account the requested \a paper_size_name.  If \a paper_size_name is
 * NULL, the system default paper size is used. The \a orientation may
 * be LANDSCAPE, PORTRAIT or AUTOLAYOUT.  If \a AUTOLAYOUT is chosen,
 * the page orientation that best fits the page contents is chosen.
 *
 * \param page     The #LeptonPage to generate a page setup for.
 * \param paper_size_name   The name of the paper size to use.
 * \param orientation       The paper orientation to use.
 *
 * \returns A newly-created page setup.
 */
static GtkPageSetup *
x_print_default_page_setup (LeptonPage *page)
{
  GtkPageSetup *setup = gtk_page_setup_new ();
  GtkPaperSize *papersize;
  int status, wx_min, wy_min, wx_max, wy_max;
  EdaConfig *cfg;
  gchar *paper, *orientation;

  /* Get configuration values */
  cfg = eda_config_get_context_for_path (lepton_page_get_filename (page));
  paper =       eda_config_get_string (cfg, CFG_GROUP_PRINTING,
                                       CFG_KEY_PRINTING_PAPER, NULL);
  orientation = eda_config_get_string (cfg, CFG_GROUP_PRINTING,
                                       CFG_KEY_PRINTING_ORIENTATION, NULL);

  /* If the paper size is valid, set it up with default margins. */
  papersize = gtk_paper_size_new (paper);
  if (papersize != NULL) {
    gtk_page_setup_set_paper_size_and_default_margins (setup, papersize);
  }

  if (g_strcmp0 (orientation, "landscape") == 0) {
    gtk_page_setup_set_orientation (setup, GTK_PAGE_ORIENTATION_LANDSCAPE);
  } else if (g_strcmp0 (orientation, "portrait") == 0) {
    gtk_page_setup_set_orientation (setup, GTK_PAGE_ORIENTATION_PORTRAIT);
  } else if (orientation == NULL
             || g_strcmp0 (orientation, "auto") == 0) {
    /* Automatically choose the orientation that fits best */
    status = world_get_object_glist_bounds (lepton_page_objects (page),
                                            /* Don't include hidden objects */
                                            FALSE,
                                            &wx_min, &wy_min, &wx_max, &wy_max);
    if (!status || (wx_max - wx_min) > (wy_max - wy_min)) {
      /* Default to landscape */
      gtk_page_setup_set_orientation (setup, GTK_PAGE_ORIENTATION_LANDSCAPE);
    } else {
      gtk_page_setup_set_orientation (setup, GTK_PAGE_ORIENTATION_PORTRAIT);
    }
  }

  g_free (paper);
  g_free (orientation);
  return setup;
}

/*! \brief Draw a page.
 * \par Function Description
 * Draws the \a page on the Cairo context \a cr, which should have
 * dimensions \a cr_width and \a cr_height.  If the Pango context \a
 * pc is provided, it is used for rendering of text.  The parameter \a
 * is_color controls whether to enable color printing, and \a
 * is_raster should be set if drawing to a raster surface such as an
 * image.
 *
 * \param toplevel A #LeptonToplevel structure.
 * \param page     The #LeptonPage to be rendered.
 * \param cr       The Cairo context to render to.
 * \param pc       A Pango context for text rendering, or NULL.
 * \param cr_width The width of the drawing area.
 * \param cr_height The height of the drawing area.
 * \param is_color TRUE if drawing should be in color; FALSE otherwise.
 * \param is_raster TRUE if drawing to a raster image surface; FALSE otherwise.
 */
static void
x_print_draw_page (LeptonToplevel *toplevel,
                   LeptonPage *page,
                   cairo_t *cr,
                   PangoContext *pc,
                   double cr_width,
                   double cr_height,
                   gboolean is_color,
                   gboolean is_raster)
{
  EdaRenderer *renderer;
  cairo_matrix_t mtx;
  GArray *color_map;
  int status, wx_min, wy_min, wx_max, wy_max;
  double w_width, w_height, scale;
  GList *iter;

  /* First, calculate a transformation matrix for the cairo
   * context. We want to center the extents of the page in the
   * available page area. */
  status = world_get_object_glist_bounds (lepton_page_objects (page),
                                          /* Don't include hidden objects. */
                                          FALSE,
                                          &wx_min,
                                          &wy_min,
                                          &wx_max,
                                          &wy_max);
  /* If there are no printable objects, draw nothing. */
  if (!status) return;

  w_width = wx_max - wx_min;
  w_height = wy_max - wy_min;
  scale = fmin (cr_width / w_width, cr_height / w_height);
  cairo_matrix_init (&mtx,
                     scale, 0,
                     0, -scale,
                     - (wx_min + 0.5*w_width) * scale + 0.5*cr_width,
                     (wy_min + 0.5*w_height) * scale + 0.5*cr_height);

  /* Second, build the color map.  If no color printing is desired,
   * transform the print color map into a black-and-white color map by
   * making the background color transparent and replacing all other
   * enabled colors with solid black. */
  color_map = g_array_sized_new (FALSE, FALSE, sizeof(LeptonColor), colors_count());
  LeptonColor *print_colors = print_colors_array ();
  color_map = g_array_append_vals (color_map, print_colors, colors_count());
  if (!is_color) {
    for (size_t i = 0; i < colors_count(); i++) {
      LeptonColor *c = &g_array_index (color_map, LeptonColor, i);
      if (!c->enabled) continue;

      /* Disable background color & fully-transparent colors */
      if (c->a == 0 || i == BACKGROUND_COLOR) {
        c->enabled = FALSE;
        continue;
      }

      /* Set any remaining colors solid black */
      c->r = 0;
      c->g = 0;
      c->b = 0;
      c->a = ~0;
    }
  }

  /* Thirdly, create and initialise a renderer */
  renderer = EDA_RENDERER (g_object_new (EDA_TYPE_RENDERER,
                                         "cairo-context", cr,
                                         "pango-context", pc,
                                         "color-map", color_map,
                                         "render-flags", is_raster ? EDA_RENDERER_FLAG_HINTING : 0,
                                         NULL));

  EdaConfig *cfg = eda_config_get_context_for_path (".");
  gchar *fontstr = eda_config_get_string (cfg, "schematic.gui", "font", NULL);

  if (fontstr != NULL) {
    g_object_set (renderer, "font-name", fontstr, NULL);
    g_free (fontstr);
  }

  /* Finally, actually do drawing */
  cairo_save (cr);
  cairo_transform (cr, &mtx);

  /* Draw background */
  eda_cairo_set_source_color (cr, BACKGROUND_COLOR, color_map);
  cairo_paint (cr);

  /* Draw all objects and cues */
  for (iter = (GList *) lepton_page_objects (page);
       iter != NULL;
       iter = g_list_next (iter)) {
    eda_renderer_draw (renderer, (LeptonObject *) iter->data);
  }
  for (iter = (GList *) lepton_page_objects (page);
       iter != NULL;
       iter = g_list_next (iter)) {
    eda_renderer_draw_cues (renderer, (LeptonObject *) iter->data);
  }

  cairo_restore (cr);

  g_object_unref (renderer);
  g_array_free (color_map, TRUE);
}

/*! Drawing callback for use with GtkPrintOperation. */
static void
draw_page__print_operation (GtkPrintOperation *print,
                            GtkPrintContext *context,
                            gint page_nr,
                            gpointer user_data)
{
  GschemToplevel *w_current = (GschemToplevel *) user_data;
  LeptonPage *page;
  cairo_t *cr;
  PangoContext *pc;
  double width, height;
  EdaConfig *cfg;
  gboolean is_color;

  /* Find the page data */
  g_return_if_fail (page_nr != 1);
  page = schematic_window_get_active_page (w_current);
  g_return_if_fail (page != NULL);

  /* Get cairo & pango contexts */
  cr = gtk_print_context_get_cairo_context (context);
  pc = gtk_print_context_create_pango_context (context);

  width = gtk_print_context_get_width (context);
  height = gtk_print_context_get_height (context);

  /* Find out if colour printing is enabled */
  cfg = eda_config_get_context_for_path (lepton_page_get_filename (page));
  is_color = !eda_config_get_boolean (cfg, CFG_GROUP_PRINTING,
                                      CFG_KEY_PRINTING_MONOCHROME, NULL);

  x_print_draw_page (w_current->toplevel, page, cr, pc,
                     width, height, is_color, FALSE);

  /* Clean up */
  g_object_unref (pc);
}

/*! \brief Export a print-style PDF file of the current page.
 * \par Function Description
 * Exports the current page as a PDF file to \a filename. The
 * export is carried out using a normal paper size and margins, as if
 * printing.
 *
 * \param w_current A #GschemToplevel structure.
 * \param filename  The filename for generated PDF.
 *
 * \returns TRUE if the operation was successful.
 */
gboolean
x_print_export_pdf_page (GschemToplevel *w_current,
                         const gchar *filename)
{
  LeptonPage *page;
  cairo_surface_t *surface;
  cairo_status_t status;
  cairo_t *cr;
  GtkPageSetup *setup;
  double width, height;
  EdaConfig *cfg;
  gboolean is_color;

  page = schematic_window_get_active_page (w_current);

  setup = x_print_default_page_setup (page);
  width = gtk_page_setup_get_paper_width (setup, GTK_UNIT_POINTS);
  height = gtk_page_setup_get_paper_height (setup, GTK_UNIT_POINTS);

  surface = cairo_pdf_surface_create (filename, width, height);
  cr = cairo_create (surface);
  cairo_translate (cr, gtk_page_setup_get_left_margin (setup, GTK_UNIT_POINTS),
                   gtk_page_setup_get_top_margin (setup, GTK_UNIT_POINTS));

  width = gtk_page_setup_get_page_width (setup, GTK_UNIT_POINTS);
  height = gtk_page_setup_get_page_height (setup, GTK_UNIT_POINTS);

  /* Find out if colour printing is enabled */
  cfg = eda_config_get_context_for_path (lepton_page_get_filename (page));
  is_color = !eda_config_get_boolean (cfg, CFG_GROUP_PRINTING,
                                      CFG_KEY_PRINTING_MONOCHROME, NULL);

  x_print_draw_page (w_current->toplevel, page,
                     cr, NULL, width, height, is_color, FALSE);

  cairo_destroy (cr);
  cairo_surface_finish (surface);

  status = cairo_surface_status (surface);
  if (status != CAIRO_STATUS_SUCCESS) {
    g_warning (_("Failed to write PDF to '%s': %s\n"),
               filename,
               cairo_status_to_string (status));
    return FALSE;
  }

  g_object_unref (setup);
  cairo_surface_destroy (surface);
  return TRUE;
}

/*! \brief Export a figure-style PDF file of the current page.
 * \par Function Description
 * Exports the current page as a PDF file to \a filename.  The export
 * is carried out using a page size matching the size of the visible
 * extents of the schematic page.
 *
 * \param w_current A #GschemToplevel structure.
 * \param filename  The filename for generated PDF.
 * \param is_color  Export using colors (TRUE) or in grayscale (FALSE).
 *
 * \returns TRUE if the operation was successful.
 */
gboolean
x_print_export_pdf (GschemToplevel *w_current,
                    const gchar *filename,
                    gboolean is_color)
{
  cairo_surface_t *surface;
  cairo_status_t cr_status;
  cairo_t *cr;
  int status, wx_min, wy_min, wx_max, wy_max;
  double width, height;

  LeptonPage *active_page = schematic_window_get_active_page (w_current);

  /* First, calculate a transformation matrix for the cairo
   * context. We want to center the extents of the page in the
   * available page area. */
  status = world_get_object_glist_bounds (lepton_page_objects (active_page),
                                          /* Don't include hidden objects. */
                                          FALSE,
                                          &wx_min, &wy_min, &wx_max, &wy_max);
  if (status) {
    width  = (wx_max - wx_min) * DEFAULT_ADOBE_PDF_PPI / DEFAULT_GSCHEM_PPI;
    height = (wy_max - wy_min) * DEFAULT_ADOBE_PDF_PPI / DEFAULT_GSCHEM_PPI;
  } else {
    /* Fallback size if there are no drawable objects */
    width = height = DEFAULT_PDF_SIZE;
  }

  surface = cairo_pdf_surface_create (filename, width, height);
  cr = cairo_create (surface);

  x_print_draw_page (w_current->toplevel,
                     active_page,
                     cr, NULL, width, height,
                     is_color, FALSE);

  cairo_destroy (cr);
  cairo_surface_finish (surface);

  cr_status = cairo_surface_status (surface);
  if (cr_status != CAIRO_STATUS_SUCCESS) {
    g_warning (_("Failed to write PDF to '%1$s': %2$s\n"),
               filename,
               cairo_status_to_string (cr_status));
    return FALSE;
  }

  cairo_surface_destroy (surface);
  return TRUE;
}

/*! \brief Show a print dialog and print current page if requested.
 * \par Function Description
 * Shows a standard print dialog, and allows the user to print the current page.
 *
 * \param w_current A #GschemToplevel structure.
 */

void
x_print (GschemToplevel *w_current)
{
  static GtkPrintSettings *settings = NULL;
  GtkPageSetup *setup;
  GtkPrintOperation *print;
  GtkPrintOperationResult res;
  GError *err = NULL;
  const int num_pages = 1;

  LeptonPage *active_page = schematic_window_get_active_page (w_current);

  /* Create the print operation and set it up */
  print = GTK_PRINT_OPERATION (g_object_new (GTK_TYPE_PRINT_OPERATION,
                                             "n-pages", num_pages,
                                             "use-full-page", FALSE,
                                             "unit", GTK_UNIT_POINTS,
                                             NULL));

  setup = x_print_default_page_setup (active_page);
  gtk_print_operation_set_default_page_setup (print, setup);

  g_signal_connect (print, "draw_page", G_CALLBACK (draw_page__print_operation),
                    w_current);


  /* create print settings:
  */
  if (settings == NULL)
  {
    settings = gtk_print_settings_new();
  }


#ifdef DEBUG
  const gchar* uri_prev = gtk_print_settings_get (settings, "output-uri");
  printf (" >> uri_prev = [%s]\n", uri_prev);
#endif


  /* derive output file name from the file name of the current page:
  */
  if (active_page != NULL)
  {
    const gchar* path = lepton_page_get_filename (active_page);
    gchar* uri = g_strdup_printf ("file://%s.pdf", path);

    gtk_print_settings_set (settings, "output-uri", uri);

    g_free (uri);
  }


  /* set print operation settings:
  */
  gtk_print_operation_set_print_settings (print, settings);

  /* enable the "paper size" and "orientation" combo boxes:
  */
  gtk_print_operation_set_embed_page_setup (print, TRUE);


  res = gtk_print_operation_run (print, GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG,
                                 GTK_WINDOW (w_current->main_window), &err);

  if (res == GTK_PRINT_OPERATION_RESULT_ERROR) {
    /* If printing failed due to an error, show an error dialog */
    GtkWidget *error_dialog =
      gtk_message_dialog_new (GTK_WINDOW (w_current->main_window),
                              GTK_DIALOG_DESTROY_WITH_PARENT,
                              GTK_MESSAGE_ERROR,
                              GTK_BUTTONS_CLOSE,
                              _("Error printing file:\n%1$s"),
                              err->message);
    g_signal_connect (error_dialog, "response",
                      G_CALLBACK (gtk_widget_destroy), NULL);
    gtk_widget_show (error_dialog);
    g_error_free (err);

  } else if (res == GTK_PRINT_OPERATION_RESULT_APPLY) {
    /* We're supposed to store the print settings, so do that */
    if (settings != NULL) {
      g_object_unref (settings);
    }
    settings = GTK_PRINT_SETTINGS (g_object_ref (gtk_print_operation_get_print_settings (print)));
  }

  /* Clean up */
  g_object_unref (print);
}
