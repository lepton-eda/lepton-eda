/*
 * Lepton EDA command-line utility
 * Copyright (C) 2012 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2014-2016 gEDA Contributors
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */
#include <config.h>

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <errno.h>

/* Gettext translation */
#include "gettext_priv.h"

#include <liblepton/liblepton.h>
#include <liblepton/libleptonguile.h>
#include <liblepton/edarenderer.h>
#include <liblepton/edacairo.h>

#include <gtk/gtk.h>
#include <cairo.h>
#include <cairo-svg.h>
#include <cairo-pdf.h>
#include <cairo-ps.h>

static void export_layout_page (LeptonPage *page,
                                cairo_rectangle_t *extents,
                                cairo_matrix_t *mtx,
                                gboolean postscript);
static void export_draw_page (LeptonPage *page,
                              EdaRenderer *renderer);

static void export_postscript (gboolean is_eps);

static gdouble export_parse_dist (const gchar *dist);

/* Default pixels-per-inch for raster outputs */
#define DEFAULT_DPI 96
/* Default margin width in points */
#define DEFAULT_MARGIN 18

enum ExportOrientation {
  ORIENTATION_AUTO,
  ORIENTATION_LANDSCAPE,
  ORIENTATION_PORTRAIT,
};

struct ExportSettings {
  /* Input & output */
  gchar *outfile; /* Filename encoding */
  gchar *format; /* UTF-8 */

  enum ExportOrientation layout;

  GtkPaperSize *paper;
  gdouble scale; /* Output scale; defaults to 1 mil per 1 lepton-schematic point*/
  gdouble size[2]; /* Points */
  gdouble margins[4]; /* Points. Top, right, bottom, left. */
  gdouble align[2]; /* 0.0 < align < 1.0 for halign and valign */
  gdouble dpi;

  gboolean color;
  gchar *font; /* UTF-8 */
};


static struct ExportSettings settings = {
  0,
  NULL,

  ORIENTATION_AUTO,

  NULL,
  72.0/1000,
  {-1, -1},
  {-1, -1, -1, -1},
  {0.5,0.5},
  DEFAULT_DPI,

  FALSE,
  NULL,
};

void
lepton_export_settings_set_outfile (const char *outfile)
{
  g_free (settings.outfile);
  settings.outfile = g_strdup (outfile);
}

void
lepton_export_settings_set_format (const char* format)
{
  g_free (settings.format);
  settings.format = g_strdup (format);
}

void
lepton_export_settings_set_layout (const char* layout)
{
  enum ExportOrientation result = ORIENTATION_AUTO;

  if      (strcmp (layout, "auto") == 0) { result = ORIENTATION_AUTO; }
  else if (strcmp (layout, "landscape") == 0) { result = ORIENTATION_LANDSCAPE; }
  else if (strcmp (layout, "portrait") == 0) { result = ORIENTATION_PORTRAIT; }

  settings.layout = result;
}

void
lepton_export_settings_set_scale (gdouble scale)
{
  settings.scale = scale;
}

void
lepton_export_settings_set_size (gdouble width,
                                 gdouble height)
{
  settings.size[0] = width;
  settings.size[1] = height;
}

/* Points. Top, right, bottom, left. */
void
lepton_export_settings_set_margins (gdouble top,
                                    gdouble right,
                                    gdouble bottom,
                                    gdouble left)
{
  settings.margins[0] = top;
  settings.margins[1] = right;
  settings.margins[2] = bottom;
  settings.margins[3] = left;
}

void
lepton_export_settings_set_align (gdouble halign,
                                  gdouble valign)
{
  settings.align[0] = halign;
  settings.align[1] = valign;
}

void
lepton_export_settings_set_dpi (gdouble dpi)
{
  settings.dpi = dpi;
}

void
lepton_export_settings_set_color (gboolean color)
{
  settings.color = color;
}

void
lepton_export_settings_set_font (const char *font)
{
  g_free (settings.font);
  settings.font = g_strdup (font);
}

void
lepton_export_settings_reset_paper_size ()
{
  if (settings.paper != NULL)
  {
    gtk_paper_size_free (settings.paper);
    settings.paper = NULL;
  }
}


static void
lepton_export_set_renderer_font (EdaRenderer *renderer,
                                 const char *font_name)
{
  g_object_set (G_OBJECT(renderer),
                "font-name",
                font_name,
                NULL);
}


static void
lepton_export_set_renderer_color_map (EdaRenderer *renderer,
                                      gboolean color)
{
  size_t i;
  GArray *render_color_map = NULL;

  /* Create color map */
  render_color_map =
    g_array_sized_new (FALSE, FALSE, sizeof(LeptonColor), colors_count());
  LeptonColor* print_colors = print_colors_array();
  render_color_map =
    g_array_append_vals (render_color_map, print_colors, colors_count());
  if (!color) {
    /* Create a black and white color map.  All non-background colors
     * are black. */
    LeptonColor white = {255, 255, 255, 255, TRUE};
    LeptonColor black = {0, 0, 0, 255, TRUE};
    for (i = 0; i < colors_count(); i++) {
      LeptonColor *c = &g_array_index (render_color_map, LeptonColor, i);
      if (!lepton_color_enabled (c)) continue;

      if (c->a == 0) {
        c->enabled = FALSE;
        continue;
      }
      if (i == OUTPUT_BACKGROUND_COLOR) {
        *c = white;
      } else {
        *c = black;
      }
    }
  }
  eda_renderer_set_color_map (renderer, render_color_map);
}


/* Prints a message and quits with error status if a cairo status
 * value is not "success". */
static inline void
export_cairo_check_error (cairo_status_t status)
{
  if (status != CAIRO_STATUS_SUCCESS) {
    fprintf (stderr, _("ERROR: %s.\n"), cairo_status_to_string (status));
    exit (1);
  }
}

/* Calculates a page layout.  If page is NULL, uses the first page
 * (this is convenient for single-page rendering).  The required size
 * of the page is returned in extents, and the cairo transformation
 * matrix needed to fit the drawing into the page is returned in mtx.
 * Takes into account all of the margin/orientation/paper settings,
 * and the size of the drawing itself. */
static void
export_layout_page (LeptonPage *page,
                    cairo_rectangle_t *extents,
                    cairo_matrix_t *mtx,
                    gboolean postscript)
{
  cairo_rectangle_t drawable;
  int wx_min, wy_min, wx_max, wy_max, w_width, w_height;
  gboolean landscape = FALSE;
  gdouble m[4]; /* Calculated margins */
  gdouble s; /* Calculated scale */
  gdouble slack[2]; /* Calculated alignment slack */

  LeptonToplevel *toplevel = edascm_c_current_toplevel();

  if (page == NULL) {
    const GList *pages = lepton_list_get_glist (toplevel->pages);
    g_assert (pages != NULL && pages->data != NULL);
    page = (LeptonPage *) pages->data;
  }

  /* Set the margins. If none were provided by the user, get them
   * from the paper size (if a paper size is being used) or just use a
   * sensible default. */
  if (settings.margins[0] >= 0) {
    memcpy (m, settings.margins, 4*sizeof(gdouble));
  } else if (settings.paper != NULL) {
    m[0] = gtk_paper_size_get_default_top_margin (settings.paper, GTK_UNIT_POINTS);
    m[1] = gtk_paper_size_get_default_left_margin (settings.paper, GTK_UNIT_POINTS);
    m[2] = gtk_paper_size_get_default_bottom_margin (settings.paper, GTK_UNIT_POINTS);
    m[3] = gtk_paper_size_get_default_right_margin (settings.paper, GTK_UNIT_POINTS);
  } else {
    m[0] = DEFAULT_MARGIN;
    m[1] = DEFAULT_MARGIN;
    m[2] = DEFAULT_MARGIN;
    m[3] = DEFAULT_MARGIN;
  }

  /* Now calculate extents of objects within page. Hidden text is
     not taken into account. */
  world_get_object_glist_bounds (lepton_page_objects (page),
                                 FALSE,
                                 &wx_min,
                                 &wy_min,
                                 &wx_max,
                                 &wy_max);
  w_width = wx_max - wx_min;
  w_height = wy_max - wy_min;

  /* If a size was specified, use it.  Otherwise, use paper size, if
   * provided.  Fall back to just using the size of the drawing. */
  extents->x = extents->y = 0;
  if (settings.size[0] >= 0) {
    /* get extents from size */

    extents->width = settings.size[0];
    extents->height = settings.size[1];

  } else if (settings.paper != NULL) {
    /* get extents from paper */

    gdouble p_width, p_height;

    /* Select orientation */
    switch (settings.layout) {
    case ORIENTATION_LANDSCAPE:
      landscape = TRUE;
      break;
    case ORIENTATION_PORTRAIT:
      landscape = FALSE;
      break;
    case ORIENTATION_AUTO:
    default:
      landscape = (w_width > w_height);
      break;
    }

    p_width = gtk_paper_size_get_width (settings.paper, GTK_UNIT_POINTS);
    p_height = gtk_paper_size_get_height (settings.paper, GTK_UNIT_POINTS);

    if (landscape) {
      extents->width = p_height;
      extents->height = p_width;
    } else {
      extents->width = p_width;
      extents->height = p_height;
    }
  } else {
    /* get extents from drawing */

    extents->width = w_width * settings.scale; /* in points */
    extents->height = w_height * settings.scale; /* in points */

    /* If the extents were obtained from the drawing, grow the extents
     * rather than shrinking the drawable area.  This ensures that the
     * overall aspect ratio of the image remains correct. */
    extents->width += m[1] + m[3];
    extents->height += m[0] + m[2];
  }

  drawable.x = m[1];
  drawable.y = m[0];

  drawable.width = extents->width - m[1] - m[3];
  drawable.height = extents->height - m[0] - m[2];

  /* Calculate optimum scale */
  s = fmin (drawable.width / w_width, drawable.height / w_height);

  /* Calculate alignment slack */
  slack[0] = fmin (1, fmax (0, settings.align[0])) * (drawable.width - w_width * s);
  slack[1] = fmin (1, fmax (0, settings.align[1])) * (drawable.height - w_height * s);

  /* Finally, create and set a cairo transformation matrix that
   * centres the drawing into the drawable area. */

  /* Create an initial matrix */
  cairo_matrix_init_identity (mtx);

  /* To understand how the following transformations work, read
   * them from bottom to top. */

  /* Turn page 90 degrees CCW. This is needed only for plain PS
   * output in landscape orientation. See comments in
   * export_postscript() for more information. */
  if (postscript && (extents->width > extents->height)) {
    cairo_matrix_translate (mtx, 0, extents->width);
    cairo_matrix_rotate (mtx, -M_PI/2);
  }

  /* Align the matrix accounting for margins and 'slack' space. */
  cairo_matrix_translate (mtx, drawable.x + slack[0], drawable.y + slack[1]);
  /* Reverse the Y axis since the drawable origin is top-left
   * while the schematic origin is bottom-left, and scale the
   * matrix using the optimum scale factor found above. */
  cairo_matrix_scale (mtx, s, -s);
  /* Translate matrix to the drawable origin (its left-top
     point) */
  cairo_matrix_translate (mtx, -wx_min, -(wy_min + w_height));
}

/* Actually draws a page.  If page is NULL, uses the first open page. */
static void
export_draw_page (LeptonPage *page,
                  EdaRenderer *renderer)
{
  const GList *contents;
  GList *iter;
  cairo_t *cr;

  LeptonToplevel *toplevel = edascm_c_current_toplevel();

  cr = eda_renderer_get_cairo_context (renderer);

  if (page == NULL) {
    const GList *pages = lepton_list_get_glist (toplevel->pages);
    g_assert (pages != NULL && pages->data != NULL);
    page = (LeptonPage *) pages->data;
  }

  /* Draw background */
  eda_cairo_set_source_color (cr, OUTPUT_BACKGROUND_COLOR,
                              eda_renderer_get_color_map (renderer));
  cairo_paint (cr);

  /* Draw objects & cues */
  contents = lepton_page_objects (page);
  for (iter = (GList *) contents; iter != NULL; iter = g_list_next (iter))
    eda_renderer_draw (renderer, (LeptonObject *) iter->data);
  for (iter = (GList *) contents; iter != NULL; iter = g_list_next (iter))
    eda_renderer_draw_cues (renderer, (LeptonObject *) iter->data);
}

void
lepton_export_png (void)
{
  cairo_surface_t *surface;
  cairo_t *cr;
  cairo_matrix_t mtx;
  cairo_rectangle_t extents;
  cairo_status_t status;
  double scale;

  /* Create renderer */
  EdaRenderer *renderer = eda_renderer_new (NULL, NULL);
  if (settings.font != NULL) {
    lepton_export_set_renderer_font (renderer, settings.font);
  }

  lepton_export_set_renderer_color_map (renderer, settings.color);

  /* Create a dummy context to permit calculating extents taking text
   * into account. */
  surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32, 0, 0);
  cr = cairo_create (surface);
  cairo_surface_destroy (surface);

  g_object_set (renderer,
                "cairo-context", cr,
                "render-flags", EDA_RENDERER_FLAG_HINTING,
                NULL);

  /* Calculate page layout */
  export_layout_page (NULL, &extents, &mtx, FALSE);
  cairo_destroy (cr);

  /* Create a rendering surface of the correct size.  'extents' is
   * measured in points, so we need to use the DPI setting to
   * transform to pixels. */
  scale = settings.dpi / 72.0;
  surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32,
                                        (int) ceil (extents.width * scale),
                                        (int) ceil (extents.height * scale));

  /* Create a cairo context and set the transformation matrix. */
  cr = cairo_create (surface);
  cairo_scale (cr, scale, scale);
  cairo_transform (cr, &mtx);

  /* Set up renderer. We need to enable subpixel hinting. */
  g_object_set (renderer, "cairo-context", cr, NULL);

  /* Draw */
  export_draw_page (NULL, renderer);
  export_cairo_check_error (cairo_surface_status (surface));

  /* Save to file */
  status = cairo_surface_write_to_png (surface, settings.outfile);
  export_cairo_check_error (status);

  g_object_unref (G_OBJECT (renderer));
}

/* Worker function used by both lepton_export_ps and export_eps */
static void
export_postscript (gboolean is_eps)
{
  cairo_surface_t *surface;
  cairo_rectangle_t extents;
  cairo_matrix_t mtx;
  cairo_t *cr;
  GList *iter;

  LeptonToplevel *toplevel = edascm_c_current_toplevel();

  /* Create renderer */
  EdaRenderer *renderer = eda_renderer_new (NULL, NULL);
  if (settings.font != NULL) {
    lepton_export_set_renderer_font (renderer, settings.font);
  }

  lepton_export_set_renderer_color_map (renderer, settings.color);

  /* Create a surface. To begin with, we don't know the size. */
  surface = cairo_ps_surface_create (settings.outfile, 1, 1);
  cairo_ps_surface_set_eps (surface, is_eps);
  cr = cairo_create (surface);
  g_object_set (renderer, "cairo-context", cr, NULL);

  for (iter = lepton_list_get_glist (toplevel->pages);
       iter != NULL;
       iter = g_list_next (iter)) {
    LeptonPage *page = (LeptonPage *) iter->data;

    export_layout_page (page, &extents, &mtx, !is_eps);

    /* Postscript output must always go in Portrait orientation to
     * pleasure printers, so we apply appropriate transformations
     * to extents here, and add DSC comments to notify Postscript
     * viewers of how to properly orient on-screen pages.  Please
     * see the following document for more information:
     * https://www.cairographics.org/documentation/using_the_postscript_surface/
     */
    if (!is_eps) {
      if (extents.width > extents.height) {
        /* Exchange width and height of the output extents. */
        cairo_ps_surface_set_size (surface, extents.height, extents.width);
        cairo_ps_surface_dsc_begin_page_setup (surface);
        cairo_ps_surface_dsc_comment (surface, "%%PageOrientation: Landscape");
      } else {
        cairo_ps_surface_set_size (surface, extents.width, extents.height);
        cairo_ps_surface_dsc_begin_page_setup (surface);
        cairo_ps_surface_dsc_comment (surface, "%%PageOrientation: Portrait");
      }
    } else {
      cairo_ps_surface_set_size (surface, extents.width, extents.height);
    }

    cairo_set_matrix (cr, &mtx);
    export_draw_page (page, renderer);
    cairo_show_page (cr);
  }

  cairo_surface_finish (surface);
  export_cairo_check_error (cairo_surface_status (surface));
  g_object_unref (G_OBJECT (renderer));
}

void
lepton_export_ps (void)
{
  export_postscript (FALSE);
}

void
lepton_export_eps (void)
{
  export_postscript (TRUE);
}

void
lepton_export_pdf (void)
{
  cairo_surface_t *surface;
  cairo_rectangle_t extents;
  cairo_matrix_t mtx;
  cairo_t *cr;
  GList *iter;

  LeptonToplevel *toplevel = edascm_c_current_toplevel();

  /* Create renderer */
  EdaRenderer *renderer = eda_renderer_new (NULL, NULL);
  if (settings.font != NULL) {
    lepton_export_set_renderer_font (renderer, settings.font);
  }

  lepton_export_set_renderer_color_map (renderer, settings.color);

  /* Create a surface. To begin with, we don't know the size. */
  surface = cairo_pdf_surface_create (settings.outfile, 1, 1);
  cr = cairo_create (surface);
  g_object_set (renderer, "cairo-context", cr, NULL);

  for (iter = lepton_list_get_glist (toplevel->pages);
       iter != NULL;
       iter = g_list_next (iter)) {
    LeptonPage *page = (LeptonPage *) iter->data;

    export_layout_page (page, &extents, &mtx, FALSE);
    cairo_pdf_surface_set_size (surface, extents.width, extents.height);
    cairo_set_matrix (cr, &mtx);
    export_draw_page (page, renderer);
    cairo_show_page (cr);
  }

  cairo_surface_finish (surface);
  export_cairo_check_error (cairo_surface_status (surface));
  g_object_unref (G_OBJECT (renderer));
}

void
lepton_export_svg ()
{
  cairo_surface_t *surface;
  cairo_rectangle_t extents;
  cairo_matrix_t mtx;
  cairo_t *cr;

  /* Create renderer */
  EdaRenderer *renderer = eda_renderer_new (NULL, NULL);
  if (settings.font != NULL) {
    lepton_export_set_renderer_font (renderer, settings.font);
  }

  lepton_export_set_renderer_color_map (renderer, settings.color);

  /* Create a surface and run export_layout_page() to figure out
   * the picture extents and set up the cairo transformation
   * matrix.  The surface is created only in order to force
   * eda_renderer_default_get_user_bounds() to behave quietly. */
  surface = cairo_svg_surface_create (settings.outfile, 0, 0);
  cr = cairo_create (surface);
  g_object_set (renderer, "cairo-context", cr, NULL);
  export_layout_page (NULL, &extents, &mtx, FALSE);
  cairo_destroy (cr);

  /* Now create a new surface with the known extents. */
  surface = cairo_svg_surface_create (settings.outfile,
                                      extents.width,
                                      extents.height);
  cr = cairo_create (surface);
  g_object_set (renderer, "cairo-context", cr, NULL);

  cairo_set_matrix (cr, &mtx);
  export_draw_page (NULL, renderer);

  cairo_show_page (cr);
  cairo_surface_finish (surface);
  export_cairo_check_error (cairo_surface_status (surface));

  g_object_unref (G_OBJECT (renderer));
}

/* Parse a distance specification. A distance specification consists
 * of a floating point value followed by an optional two-character
 * unit name (in, cm, mm, pc, px, or pt, same as CSS).  If no unit is
 * specified, assumes that the unit is pt.  This is used for the
 * --margins, --size and --scale command-line options. */
static gdouble
export_parse_dist (const gchar *dist)
{
  gdouble base, mult;
  gchar *unit;
  errno = 0;
  base = strtod(dist, &unit);

  if (errno != 0) return -1;

  if (g_strcmp0 (unit, "in") == 0) {
    mult = 72.0;
  } else if (g_strcmp0 (unit, "cm") == 0) {
    mult = 72.0 / 2.54;
  } else if (g_strcmp0 (unit, "mm") == 0) {
    mult = 72.0 / 25.4;
  } else if (g_strcmp0 (unit, "pc") == 0) { /* Picas */
    mult = 12.0;
  } else if (g_strcmp0 (unit, "px") == 0) {
    mult = 72.0 / settings.dpi;
  } else if (g_strcmp0 (unit, "pt") == 0
             || unit[0] == 0) {
    mult = 1.0;
  } else {
    return -1; /* Indicate that parsing unit failed */
  }

  return mult * base;
}

/* Parse the --align command line option. */
gboolean
lepton_export_parse_align (const gchar *align)
{
  int n;
  gchar **args;

  /* Automatic alignment case */
  if (g_strcmp0 (align, "auto") == 0 || align[0] == 0) {
    settings.align[0] = settings.align[1] = 0.5;
    return TRUE;
  }

  args = g_strsplit_set (align, ":; ", 2);
  for (n = 0; args[n] != NULL; n++) {
    gdouble d = strtod (args[n], NULL);
    if (d < 0 || d > 1) return FALSE;
    settings.align[n] = d;
  }
  g_strfreev (args);

  if (n != 2) return FALSE;
  return TRUE;
}

/* Parse the --layout command line option and the export.layout config
 * file setting. */
gboolean
lepton_export_parse_layout (const gchar *layout)
{
  if (g_strcmp0 (layout, "landscape") == 0) {
    settings.layout = ORIENTATION_LANDSCAPE;
  } else if (g_strcmp0 (layout, "portrait") == 0) {
    settings.layout = ORIENTATION_PORTRAIT;
  } else if (g_strcmp0 (layout, "auto") == 0
             || layout == NULL
             || layout[0] == 0) {
    settings.layout = ORIENTATION_AUTO;
  } else {
    return FALSE;
  }
  return TRUE;
}

/* Parse the --margins command-line option.  If the value is "auto" or
 * empty, sets margins to be determined automatically from paper size
 * or compiled-in defaults. Otherwise, expects a list of 1-4 distance
 * specs; see export_parse_dist().  Rules if <4 distances are
 * specified are as for 'margin' property in CSS. */
gboolean
lepton_export_parse_margins (const gchar *margins)
{
  gint n;
  gchar **dists;

  g_assert (margins != NULL);

  /* Automatic margins case */
  if (g_strcmp0 (margins, "auto") == 0 || margins[0] == 0) {
    for (n = 0; n < 4; n++) settings.margins[n] = -1;
    return TRUE;
  }

  dists = g_strsplit_set (margins, ":; ", 4);
  for (n = 0; dists[n] != NULL; n++) {
    gdouble d = export_parse_dist (dists[n]);
    if (d < 0) return FALSE;
    settings.margins[n] = d;
  }
  g_strfreev (dists);

  if (n == 1) {
    /* If only one value is specified, it applies to all four sides. */
    settings.margins[3] = settings.margins[2]
      = settings.margins[1] = settings.margins[0];
  } else if (n == 2) {
    /* If two values are specified, the first applies to the
       top/bottom, and the second to left/right. */
    settings.margins[2] = settings.margins[0];
    settings.margins[3] = settings.margins[1];
  } else if (n == 3) {
    /* If three values are specified, the first applies to the top,
       the second to left/right, and the third to the bottom. */
    settings.margins[3] = settings.margins[1];
  } else if (n != 4) {
    return FALSE; /* Must correctly specify 1-4 distances + units */
  }

  return TRUE;
}

/* Parse the --paper option.  Clears any size setting. */
gboolean
lepton_export_parse_paper (const gchar *paper)
{
  GtkPaperSize *paper_size = gtk_paper_size_new (paper);
  if (paper_size == NULL) return FALSE;

  if (settings.paper != NULL) gtk_paper_size_free (settings.paper);
  settings.paper = paper_size;
  /* Must reset size setting to invalid or it will override paper
   * setting */
  settings.size[0] = settings.size[1] = -1;
  return TRUE;
}

/* Parse the --size option, which must either be "auto" (i.e. obtain
 * size from drawing) or a list of two distances (width/height). */
gboolean
lepton_export_parse_size (const gchar *size)
{
  gint n;
  gchar **dists;

  /* Automatic size case */
  if (g_strcmp0 (size, "auto") == 0 || size[0] == 0) {
    settings.size[0] = settings.size[1] = -1;
    return TRUE;
  }

  dists = g_strsplit_set (size, ":; ", 2);
  for (n = 0; dists[n] != NULL; n++) {
    gdouble d = export_parse_dist (dists[n]);
    if (d < 0) return FALSE;
    settings.size[n] = d;
  }
  g_strfreev (dists);
  if (n != 2) return FALSE;

  return TRUE;
}

/* Parse the --scale option. The value should be a distance
 * corresponding to 100 points in lepton-schematic (1 default grid
 * spacing). */
gboolean
lepton_export_parse_scale (const gchar *scale)
{
  gdouble d = export_parse_dist (scale);
  if (d <= 0) return FALSE;
  settings.scale = d/100;
  return TRUE;
}

/* Initialise settings from config store. */
void
export_config (void)
{
  EdaConfig *cfg = eda_config_get_context_for_path (".");
  gchar *str;
  gdouble *lst;
  gdouble dval;
  gdouble bval;
  gsize n;
  GError *err = NULL;

  /* Parse orientation */
  str = eda_config_get_string (cfg, "export", "layout", NULL);
  lepton_export_parse_layout (str); /* Don't care if it works */
  g_free (str);

  /* Parse paper size */
  str = eda_config_get_string (cfg, "export", "paper", NULL);
  lepton_export_parse_paper (str);
  g_free (str);

  /* Parse specific size setting -- always in points */
  if (eda_config_has_key (cfg, "export", "size", NULL)) {
    lst = eda_config_get_double_list (cfg, "export", "size", &n, NULL);
    if (lst != NULL) {
      if (n >= 2) {
        memcpy (settings.size, lst, 2*sizeof(gdouble));
      }
      g_free (lst);
    }
    /* Since a specific size was provided, ditch the paper size
     * setting */
    if (settings.paper != NULL) {
      gtk_paper_size_free (settings.paper);
      settings.paper = NULL;
    }
  }

  /* Parse margins -- always in points */
  lst = eda_config_get_double_list (cfg, "export", "margins", &n, NULL);
  if (lst != NULL) {
    if (n >= 4) { /* In the config file all four sides must be specified */
      memcpy (settings.margins, lst, 4*sizeof(gdouble));
    }
    g_free (lst);
  }

  /* Parse alignment */
  lst = eda_config_get_double_list (cfg, "export", "align", &n, NULL);
  if (lst != NULL) {
    if (n >= 2) { /* Both halign and valign must be specified */
      memcpy (settings.align, lst, 2*sizeof(gdouble));
    }
    g_free (lst);
  }

  /* Parse dpi */
  dval = eda_config_get_double (cfg, "export", "dpi", &err);
  if (err == NULL) {
    settings.dpi = dval;
  } else {
    g_clear_error (&err);
  }

  bval = eda_config_get_boolean (cfg, "export", "monochrome", &err);
  if (err == NULL) {
    settings.color = !bval;
  } else {
    g_clear_error (&err);
  }

  str = eda_config_get_string (cfg, "export", "font", NULL);
  if (str != NULL) {
    g_free (settings.font);
    settings.font = str;
  }
}

void
lepton_export_list_paper_size_names ()
{
  GList* names = gtk_paper_size_get_paper_sizes (TRUE);

  for (GList* p = names; p != NULL; p = p->next)
  {
    printf ("%s\n", gtk_paper_size_get_name ((GtkPaperSize*) p->data));
    gtk_paper_size_free ((GtkPaperSize*) p->data);
  }

  g_list_free (names);
  exit (0);
}
