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

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <getopt.h>
#include <math.h>
#include <errno.h>

/* Gettext translation */
#include "gettext_priv.h"

#include <liblepton/liblepton.h>
#include <liblepton/libleptonguile.h>
#include <liblepton/edarenderer.h>
#include <liblepton/edacairo.h>

#include <gtk/gtk.h>
#include <glib/gstdio.h>
#include <cairo.h>
#include <cairo-svg.h>
#include <cairo-pdf.h>
#include <cairo-ps.h>

static void export_layout_page (LeptonPage *page,
                                cairo_rectangle_t *extents,
                                cairo_matrix_t *mtx,
                                gboolean postscript);
static void export_draw_page (LeptonPage *page);

static void export_png (void);
static void export_postscript (gboolean is_eps);
static void export_ps  (void);
static void export_eps (void);
static void export_pdf (void);
static void export_svg (void);

static gdouble export_parse_dist (const gchar *dist);
static gboolean export_parse_scale (const gchar *scale);
static gboolean export_parse_layout (const gchar *layout);
static gboolean export_parse_margins (const gchar *margins);
static gboolean export_parse_paper (const gchar *paper);
static gboolean export_parse_size (const gchar *size);
static void export_config (void);
static void export_usage (void);
static void export_command_line (int argc, char * const *argv);

/* Default pixels-per-inch for raster outputs */
#define DEFAULT_DPI 96
/* Default margin width in points */
#define DEFAULT_MARGIN 18

enum ExportFormatFlags {
  OUTPUT_MULTIPAGE = 1,
  OUTPUT_POINTS = 2,
  OUTPUT_PIXELS = 4,
};

struct ExportFormat {
  const gchar *name; /* UTF-8 */
  const gchar *alias; /* UTF-8 */
  gint flags;
  void (*func)(void);
};

enum ExportOrientation {
  ORIENTATION_AUTO,
  ORIENTATION_LANDSCAPE,
  ORIENTATION_PORTRAIT,
};

struct ExportSettings {
  /* Input & output */
  int infilec;
  char * const *infilev; /* Filename encoding */
  const char *outfile; /* Filename encoding */
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

static struct ExportFormat formats[] =
  {
    {"Portable Network Graphics (PNG)", "png", OUTPUT_PIXELS, export_png},
    {"Postscript (PS)", "ps", OUTPUT_POINTS | OUTPUT_MULTIPAGE, export_ps},
    {"Encapsulated Postscript (EPS)", "eps", OUTPUT_POINTS, export_eps},
    {"Portable Document Format (PDF)", "pdf", OUTPUT_POINTS | OUTPUT_MULTIPAGE, export_pdf},
    {"Scalable Vector Graphics (SVG)", "svg", OUTPUT_POINTS, export_svg},
    {NULL, NULL, 0, NULL},
  };

static EdaRenderer *renderer = NULL;
static LeptonToplevel *toplevel = NULL;

static struct ExportSettings settings = {
  0,
  NULL,
  NULL,
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

#define bad_arg_msg _("ERROR: Bad argument '%1$s' to %2$s option.\n")
#define see_help_msg _("\nRun `lepton-cli export --help' for more information.\n")

static void
cmd_export_impl (void *data, int argc, char **argv)
{
  size_t i;
  GError *err = NULL;
  gchar *tmp;
  const gchar *out_suffix;
  struct ExportFormat *exporter = NULL;
  GArray *render_color_map = NULL;
  gchar *original_cwd = g_get_current_dir ();

  gtk_init_check (&argc, &argv);
  scm_init_guile ();
  liblepton_init ();

  /* Enable rendering of placeholders. Otherwise the user won't
     see what's wrong. */
  set_render_placeholders ();

  scm_dynwind_begin ((scm_t_dynwind_flags) 0);
  toplevel = s_toplevel_new ();
  edascm_dynwind_toplevel (toplevel);

  /* Now load rc files, if necessary */
  if (getenv ("LEPTON_INHIBIT_RC_FILES") == NULL) {
    g_rc_parse ("lepton-cli export", NULL, NULL);
  }

  /* Parse configuration files */
  export_config ();

  /* Parse command-line arguments */
  export_command_line (argc, argv);

  /* If no format was specified, try and guess from output
   * filename. */
  if (settings.format == NULL) {
    out_suffix = strrchr (settings.outfile, '.');
    if (out_suffix != NULL) {
      out_suffix++; /* Skip '.' */
    } else {
      fprintf (stderr,
               _("ERROR: Cannot infer output format from filename '%1$s'.\n"),
               settings.outfile);
      exit (1);
    }
  }

  /* Try and find an exporter function */
  tmp = g_utf8_strdown ((settings.format == NULL) ? out_suffix : settings.format, -1);
  for (i = 0; formats[i].name != NULL; i++) {
    if (strcmp (tmp, formats[i].alias) == 0) {
      exporter = &formats[i];
      break;
    }
  }
  if (exporter == NULL) {
    if (settings.format == NULL) {
      fprintf (stderr,
               _("ERROR: Cannot find supported format for filename '%1$s'.\n"),
               settings.outfile);
      exit (1);
    } else {
      fprintf (stderr,
               _("ERROR: Unsupported output format '%1$s'.\n"),
               settings.format);
      fprintf (stderr, see_help_msg);
      exit (1);
    }
  }
  g_free (tmp);

  /* If more than one schematic/symbol file was specified, check that
   * exporter supports multipage output. */
  if ((settings.infilec > 1) && !(exporter->flags & OUTPUT_MULTIPAGE)) {
    fprintf (stderr,
             _("ERROR: Selected output format does not support multipage output\n"));
    exit (1);
  }

  /* Create renderer */
  renderer = eda_renderer_new (NULL, NULL);
  if (settings.font != NULL) {
    g_object_set (renderer, "font-name", settings.font, NULL);
  }

  /* Create color map */
  render_color_map =
    g_array_sized_new (FALSE, FALSE, sizeof(LeptonColor), colors_count());
  LeptonColor* print_colors = print_colors_array();
  render_color_map =
    g_array_append_vals (render_color_map, print_colors, colors_count());
  if (!settings.color) {
    /* Create a black and white color map.  All non-background colors
     * are black. */
    LeptonColor white = {255, 255, 255, 255, TRUE};
    LeptonColor black = {0, 0, 0, 255, TRUE};
    for (i = 0; i < colors_count(); i++) {
      LeptonColor *c = &g_array_index (render_color_map, LeptonColor, i);
      if (!c->enabled) continue;

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

  /* Load schematic files */
  while (optind < argc) {
    LeptonPage *page;
    tmp = argv[optind++];

    page = lepton_page_new (toplevel, tmp);
    if (!f_open (toplevel, page, tmp, &err)) {
      fprintf (stderr,
               /* TRANSLATORS: The first string is the filename, the second
                * is the detailed error message */
               _("ERROR: Failed to load '%1$s': %2$s\n"), tmp,
               err->message);
      exit (1);
    }
    if (g_chdir (original_cwd) != 0) {
      fprintf (stderr,
               _("ERROR: Failed to change directory to '%1$s': %2$s\n"),
               original_cwd, g_strerror (errno));
      exit (1);
    }
  }

  /* Render */
  exporter->func ();

  scm_dynwind_end ();
  exit (0);
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
export_draw_page (LeptonPage *page)
{
  const GList *contents;
  GList *iter;
  cairo_t *cr;

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

static void
export_png (void)
{
  cairo_surface_t *surface;
  cairo_t *cr;
  cairo_matrix_t mtx;
  cairo_rectangle_t extents;
  cairo_status_t status;
  double scale;

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
  export_draw_page (NULL);
  export_cairo_check_error (cairo_surface_status (surface));

  /* Save to file */
  status = cairo_surface_write_to_png (surface, settings.outfile);
  export_cairo_check_error (status);
}

/* Worker function used by both export_ps and export_eps */
static void
export_postscript (gboolean is_eps)
{
  cairo_surface_t *surface;
  cairo_rectangle_t extents;
  cairo_matrix_t mtx;
  cairo_t *cr;
  GList *iter;

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
    export_draw_page (page);
    cairo_show_page (cr);
  }

  cairo_surface_finish (surface);
  export_cairo_check_error (cairo_surface_status (surface));
}

static void
export_ps (void)
{
  export_postscript (FALSE);
}

static void
export_eps (void)
{
  export_postscript (TRUE);
}

static void
export_pdf (void)
{
  cairo_surface_t *surface;
  cairo_rectangle_t extents;
  cairo_matrix_t mtx;
  cairo_t *cr;
  GList *iter;

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
    export_draw_page (page);
    cairo_show_page (cr);
  }

  cairo_surface_finish (surface);
  export_cairo_check_error (cairo_surface_status (surface));
}

static void
export_svg ()
{
  cairo_surface_t *surface;
  cairo_rectangle_t extents;
  cairo_matrix_t mtx;
  cairo_t *cr;

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
  export_draw_page (NULL);

  cairo_show_page (cr);
  cairo_surface_finish (surface);
  export_cairo_check_error (cairo_surface_status (surface));
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
static gboolean
export_parse_align (const gchar *align)
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
static gboolean
export_parse_layout (const gchar *layout)
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
static gboolean
export_parse_margins (const gchar *margins)
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
static gboolean
export_parse_paper (const gchar *paper)
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
static gboolean
export_parse_size (const gchar *size)
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
static gboolean
export_parse_scale (const gchar *scale)
{
  gdouble d = export_parse_dist (scale);
  if (d <= 0) return FALSE;
  settings.scale = d/100;
  return TRUE;
}

/* Initialise settings from config store. */
static void
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
  export_parse_layout (str); /* Don't care if it works */
  g_free (str);

  /* Parse paper size */
  str = eda_config_get_string (cfg, "export", "paper", NULL);
  export_parse_paper (str);
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

#define export_short_options "a:cd:f:F:hl:m:o:p:Ps:k:"

static struct option export_long_options[] = {
  {"no-color", 0, NULL, 2},
  {"align", 1, NULL, 'a'},
  {"color", 0, NULL, 'c'},
  {"dpi", 1, NULL, 'd'},
  {"format", 1, NULL, 'f'},
  {"font", 1, NULL, 'F'},
  {"help", 0, NULL, 'h'},
  {"layout", 1, NULL, 'l'},
  {"margins", 1, NULL, 'm'},
  {"output", 1, NULL, 'o'},
  {"paper", 1, NULL, 'p'},
  {"paper-names", 0, NULL, 'P'},
  {"size", 1, NULL, 's'},
  {"scale", 1, NULL, 'k'},
  {NULL, 0, NULL, 0},
};

static void
export_usage (void)
{
  printf (_("Usage: lepton-cli export [OPTION ...] -o OUTPUT [--] FILE ...\n"
"\n"
"Export Lepton EDA files in various image formats.\n"
"\n"
"  -f, --format=TYPE                    output format (normally autodetected)\n"
"  -o, --output=OUTPUT                  output filename\n"
"  -p, --paper=NAME                     select paper size by name\n"
"  -P, --paper-names                    list paper size names and exit\n"
"  -s, --size=WIDTH;HEIGHT              specify exact paper size\n"
"  -k, --scale=FACTOR                   specify output scale factor\n"
"  -l, --layout=ORIENT                  page orientation\n"
"  -m, --margins=TOP;LEFT;BOTTOM;RIGHT  set page margins\n"
"  -a, --align=HALIGN;VALIGN            set alignment of drawing within page\n"
"  -d, --dpi=DPI                        pixels-per-inch for raster outputs\n"
"  -c, --color                          enable color output\n"
"  --no-color                           disable color output\n"
"  -F, --font=NAME                      set font family for printing text\n"
"  -h, --help                           display usage information and exit\n"
"\n"
"Report bugs at <%1$s>\n"
"Lepton EDA homepage: <%2$s>\n"),
    PACKAGE_BUGREPORT,
    PACKAGE_URL);

  exit (0);
}

static void
export_list_paper_size_names()
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

/* Helper function for checking that a command-line option value can
 * be successfully converted to UTF-8. */
static inline gchar *
export_command_line__utf8_check (gchar *str, const gchar *arg)
{
  GError *err = NULL;
  gchar *result;

  g_assert (str != NULL);
  g_assert (arg != NULL);
  result = g_locale_to_utf8 (str, -1, NULL, NULL, &err);
  if (result == NULL) {
    fprintf (stderr, bad_arg_msg, optarg, arg);
    fprintf (stderr, see_help_msg);
    exit (1);
  }
  return result;
}

static void
export_command_line (int argc, char * const *argv)
{
  int c;
  gchar *str;

  /* Parse command-line arguments */
  while ((c = getopt_long (argc, argv, export_short_options,
                           export_long_options, NULL)) != -1) {
    switch (c) {
    case 0:
      /* This is a long-form-only flag option, and has already been
       * dealt with by getopt_long(). */
      break;

    case 2: /* --no-color */
      settings.color = FALSE;
      break;

    case 'a':
      str = export_command_line__utf8_check (optarg, "-a,--align");
      if (!export_parse_align (str)) {
        fprintf (stderr, bad_arg_msg, optarg, "-a,--align");
        fprintf (stderr, see_help_msg);
        exit (1);
      }
      g_free (str);
      break;

    case 'c':
      settings.color = TRUE;
      break;

    case 'd':
      settings.dpi = strtod (optarg, NULL);
      if (settings.dpi <= 0) {
        fprintf (stderr, bad_arg_msg, optarg, "-d,--dpi");
        fprintf (stderr, see_help_msg);
        exit (1);
      }
      break;

    case 'f':
      g_free (settings.format);
      settings.format = export_command_line__utf8_check (optarg, "-f,--format");
      break;

    case 'F':
      str = export_command_line__utf8_check (optarg, "-F,--font");
      g_free (settings.font);
      settings.font = str;
      break;

    case 'h':
      export_usage ();
      break;

    case 'k':
      str = export_command_line__utf8_check (optarg, "-k,--scale");
      if (!export_parse_scale (str)) {
        fprintf (stderr, bad_arg_msg, optarg, "-k,--scale");
        fprintf (stderr, see_help_msg);
        exit (1);
      }
      g_free (str);
      /* Since a specific scale was provided, ditch the paper size
       * setting */
      if (settings.paper != NULL) {
        gtk_paper_size_free (settings.paper);
        settings.paper = NULL;
      }
      break;

    case 'l':
      if (!export_parse_layout (optarg)) {
        fprintf (stderr, bad_arg_msg,
                 optarg, "-l,--layout");
        fprintf (stderr, see_help_msg);
        exit (1);
      }
      break;

    case 'm':
      str = export_command_line__utf8_check (optarg, "-m,--margins");
      if (!export_parse_margins (str)) {
        fprintf (stderr, bad_arg_msg, optarg, "-m,--margins");
        fprintf (stderr, see_help_msg);
        exit (1);
      }
      g_free (str);
      break;

    case 'o':
      settings.outfile = optarg;
      break;

    case 'p':
      str = export_command_line__utf8_check (optarg, "-p,--paper");
      if (!export_parse_paper (str)) {
        fprintf (stderr, bad_arg_msg, optarg, "-p,--paper");
        fprintf (stderr, see_help_msg);
        exit (1);
      }
      g_free (str);
      break;

    case 'P':
      export_list_paper_size_names();
      break;

    case 's':
      str = export_command_line__utf8_check (optarg, "-s,--size");
      if (!export_parse_size (str)) {
        fprintf (stderr, bad_arg_msg, optarg, "-s,--size");
        fprintf (stderr, see_help_msg);
        exit (1);
      }
      g_free (str);
      /* Since a specific size was provided, ditch the paper size
       * setting */
      if (settings.paper != NULL) {
        gtk_paper_size_free (settings.paper);
        settings.paper = NULL;
      }
      break;

    case '?':
      /* getopt_long already printed an error message */
      fprintf (stderr, see_help_msg);
      exit (1);
      break;
    default:
      g_assert_not_reached ();
    }
  }

  /* Check that some schematic files to print were provided */
  if (argc <= optind) {
    fprintf (stderr,
             _("ERROR: You must specify at least one input filename.\n"));
    fprintf (stderr, see_help_msg);
    exit (1);
  }
  settings.infilec = argc - optind;
  settings.infilev = &argv[optind];

  if (settings.outfile == NULL) {
    fprintf (stderr,
             _("ERROR: You must specify an output filename.\n"));
    fprintf (stderr, see_help_msg);
    exit (1);
  }
}

/* Main function for `lepton-cli export' */
int
cmd_export (int argc, char **argv)
{
  set_guile_compiled_path();

  scm_boot_guile (argc, argv, cmd_export_impl, NULL); /* Doesn't return */
  return 0;
}
