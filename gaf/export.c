/*
 * gEDA/gaf command-line utility
 * Copyright (C) 2012 Peter Brett <peter@peter-b.co.uk>
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <version.h>

#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <getopt.h>
#include <math.h>
#include <errno.h>

/* Gettext translation */
#include "gettext.h"

#include <libgeda/libgeda.h>
#include <libgeda/libgedaguile.h>
#include <libgedacairo/libgedacairo.h>

#include <gtk/gtk.h>
#include <glib/gstdio.h>
#include <cairo.h>
#include <cairo-svg.h>
#include <cairo-pdf.h>
#include <cairo-ps.h>

#define DEFAULT_DPI 300

#define shell_short_options "f:o:p:s:ch"

static struct option shell_long_options[] =
  {
    {"format", 1, NULL, 'f'},
    {"output", 1, NULL, 'o'},
    {"paper", 1, NULL, 'p'},
    {"size", 1, NULL, 's'},
    {"dpi", 1, NULL, 2},
    {"color", 0, NULL, 'c'},
    {"portrait", 0, NULL, 3},
    {"landscape", 0, NULL, 4},
    {"help", 0, NULL, 'h'},
    {NULL, 0, NULL, 0},
  };

static void
export_usage (void)
{
  printf (_("Usage: gaf export [OPTION ...] -o OUTPUT [--] FILE ...\n"
"\n"
"Export gEDA files in various image formats.\n"
"\n"
"  -f, --format=TYPE      select output format [TYPE=auto]\n"
"  -o, --output=OUTPUT    output filename\n"
"  -p, --paper=SIZE       select paper size [SIZE=%s]\n"
"  -s, --size=ROWSxCOLS   select image size in pixels.\n"
"      --dpi=DPI          select image dpi [DPI=%i].\n"
"  -c, --color            enable color output\n"
"  --landscape            output in landscape format\n"
"  --portrait             output in portrait format\n"
"  -h, --help     display usage information and exit\n"
"\n"
"Please report bugs to %s.\n"),
          gtk_paper_size_get_default (),
          DEFAULT_DPI,
          PACKAGE_BUGREPORT);
  exit (0);
}


enum OutputFormatFlags {
  OUTPUT_MULTIPAGE = 1,
  OUTPUT_POINTS = 2,
  OUTPUT_PIXELS = 4,
};

struct OutputFormat {
  char *name;
  char *alias;
  gint flags;
  void (*func)(void);
};

static void export_page (PAGE *page);
static void export_simple_multipage (cairo_surface_t *surface);
static void export_png (void);
static void export_ps  (void);
static void export_eps (void);
static void export_pdf (void);
static void export_svg (void);

static struct OutputFormat formats[] =
  {
    {"Portable Network Graphics (PNG)", "png", OUTPUT_PIXELS, export_png},
    {"Postscript (PS)", "ps", OUTPUT_POINTS | OUTPUT_MULTIPAGE, export_ps},
    {"Encapsulated Postscript (EPS)", "eps", OUTPUT_POINTS, export_eps},
    {"Portable Document Format (PDF)", "pdf", OUTPUT_POINTS | OUTPUT_MULTIPAGE, export_pdf},
    {"Scalable Vector Graphics (SVG)", "svg", OUTPUT_POINTS, export_svg},
    {NULL, NULL, 0, NULL},
  };

static EdaRenderer *renderer = NULL;
static TOPLEVEL *toplevel = NULL;
static char *filename = NULL;
static double width, height;

#define bad_arg_msg _("ERROR: Bad argument '%s' to %s option.\n")
#define bad_arg_prefix_msg _("ERROR: Bad argument '%s' to %s option: %s\n")
#define see_help_msg _("\nRun `gaf export --help' for more information.\n")

void
cmd_export (int argc, char **argv)
{
  int c, i, status;
  gchar *format_name = NULL;
  gchar *paper_name = g_strdup (gtk_paper_size_get_default ());
  gdouble dpi = DEFAULT_DPI;
  gboolean color = FALSE;
  GError *err = NULL;
  gchar *tmp;
  const gchar *out_arg = NULL;
  const gchar *out_suffix;
  gint pixel_width = -1, pixel_height = -1;
  gboolean portrait = TRUE;
  GtkPaperSize *paper = NULL;
  struct OutputFormat *exporter = NULL;
  GArray *render_color_map = NULL;
  gchar *original_cwd = g_get_current_dir ();

  /* We *don't* call gtk_init(), because we're only using GTK for
   * GtkPaperSize, which doesn't require gtk_init().*/
  g_type_init ();
  scm_init_guile ();
  libgeda_init ();
  scm_dynwind_begin (0);
  toplevel = s_toplevel_new ();
  edascm_dynwind_toplevel (toplevel);

  /* Parse command-line arguments */
  while ((c = getopt_long (argc, argv, shell_short_options,
                           shell_long_options, NULL)) != -1) {
    switch (c) {
    case 0:
      /* This is a long-form-only flag option, and has already been
       * dealt with by getopt_long(). */
      break;

    case 'f':
      g_free (format_name);
      format_name = g_locale_to_utf8 (optarg, -1, NULL, NULL, &err);
      if (format_name == NULL) {
        fprintf (stderr, bad_arg_prefix_msg,
                 optarg, "-f,--format", err->message);
        exit (1);
      }
      break;

    case 'o':
      g_free (filename);
      /* We need an absolute path to the output file, because f_open()
       * obnoxiously changes directory. */
      if (g_path_is_absolute (optarg)) {
        filename = g_strdup (optarg);
      } else {
        tmp = g_get_current_dir ();
        filename = g_build_filename (tmp, optarg, NULL);
        g_free (tmp);
      }
      out_arg = optarg; /* Save this for error messages */
      break;

    case 'p':
      g_free (paper_name);
      paper_name = g_locale_to_utf8 (optarg, -1, NULL, NULL, &err);
      if (paper_name == NULL) {
        fprintf (stderr, bad_arg_prefix_msg,
                 optarg, "-p,--paper", err->message);
        exit (1);
      }
      break;

    case 's':
      status = sscanf (optarg, "%ix%i", &pixel_width, &pixel_height);
      if (status != 2 || pixel_width <= 0 || pixel_height <= 0) {
        fprintf (stderr, bad_arg_msg, optarg, "-s,--size");
        fprintf (stderr, see_help_msg);
        exit (1);
      }
      break;

    case 2: /* --dpi */
      dpi = g_ascii_strtod (optarg, NULL);
      if (dpi <= 0) {
        fprintf (stderr, bad_arg_msg, optarg, "--dpi");
        exit (1);
      }
      break;

    case 3: /* --portrait */
      portrait = TRUE;
      break;

    case 4: /* --landscape */
      portrait = FALSE;
      break;

    case 'c':
      color = TRUE;
      break;

    case 'h':
      export_usage ();
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

  /* Check that some schematic files and an output file  were specified */
  if (argc == optind) {
    fprintf (stderr,
             _("ERROR: You must specify at least one input filename.\n"));
    fprintf (stderr, see_help_msg);
    exit (1);
  }
  if (filename == NULL) {
    fprintf (stderr,
             _("ERROR: You must specify an output filename.\n"));
    fprintf (stderr, see_help_msg);
    exit (1);
  }

  /* Now load rc files, if necessary */
  if (getenv ("GAF_INHIBIT_RCFILES") == NULL) {
    g_rc_parse (toplevel, "gaf export", NULL, NULL);
  }
  i_vars_libgeda_set (toplevel); /* Ugh */

  /* Validate paper size, if given */
  tmp = g_utf8_strdown (paper_name, -1);
  paper = gtk_paper_size_new (tmp);
  if (paper == NULL) {
    fprintf (stderr, _("ERROR: Invalid paper size '%s'.\n"),
             paper_name);
    exit (1);
  }
  g_free (tmp);

  /* If no format was specified, try and guess from output
   * filename. */
  if (format_name == NULL) {
    out_suffix = strrchr (filename, '.');
    if (out_suffix != NULL) {
      out_suffix++; /* Skip '.' */
    } else {
      fprintf (stderr,
               _("ERROR: Cannot infer output format from filename '%s'.\n"),
               out_arg);
      exit (1);
    }
  }

  /* Try and find an exporter function */
  tmp = g_utf8_strdown ((format_name == NULL) ? out_suffix : format_name, -1);
  for (i = 0; formats[i].name != NULL; i++) {
    if (strcmp (tmp, formats[i].alias) == 0) {
      exporter = &formats[i];
      break;
    }
  }
  if (exporter == NULL) {
    if (format_name == NULL) {
      fprintf (stderr,
               _("ERROR: Cannot find supported format for filename '%s'.\n"),
               out_arg);
      exit (1);
    } else {
      fprintf (stderr,
               _("ERROR: Unsupported output format '%s'.\n"),
               format_name);
      fprintf (stderr, see_help_msg);
      exit (1);
    }
  }
  g_free (tmp);

  /* Try to figure out dimensions for the output */
  if (exporter->flags & OUTPUT_POINTS) {
    width = gtk_paper_size_get_width (paper, GTK_UNIT_POINTS);
    height = gtk_paper_size_get_height (paper, GTK_UNIT_POINTS);
  } else if (exporter->flags & OUTPUT_PIXELS) {
    if (pixel_width > 0) {
      width = pixel_width;
      height = pixel_height;
    } else {
      width = ceil (dpi * gtk_paper_size_get_width (paper, GTK_UNIT_INCH));
      height = ceil (dpi * gtk_paper_size_get_height (paper, GTK_UNIT_INCH));
    }
  } else g_assert_not_reached ();

  /* If portrait mode, swap width/height */
  if (portrait) {
    double dummy = width;
    width = height;
    height = dummy;
  }

  /* If more than one schematic/symbol file was specified, check that
   * exporter supports multipage output. */
  if ((argc - optind > 1) && !(exporter->flags & OUTPUT_MULTIPAGE)) {
    fprintf (stderr,
             _("ERROR: Selected output format does not support multipage output\n"));
    exit (1);
  }

  /* Load schematic files */
  while (optind < argc) {
    PAGE *page;
    tmp = argv[optind++];

    page = s_page_new (toplevel, tmp);
    if (!f_open (toplevel, page, tmp, &err)) {
      fprintf (stderr,
               _("ERROR: Failed to load '%s': %s\n"), tmp,
               err->message);
      exit (1);
    }
    if (g_chdir (original_cwd) != 0) {
      fprintf (stderr,
               _("ERROR: Failed to change directory to '%s': %s\n"),
               original_cwd, g_strerror (errno));
      exit (1);
    }
  }

  /* Create renderer */
  renderer = eda_renderer_new (NULL, NULL);

  /* Create color map */
  render_color_map =
    g_array_sized_new (FALSE, FALSE, sizeof(COLOR), MAX_COLORS);
  render_color_map =
    g_array_append_vals (render_color_map, print_colors, MAX_COLORS);
  if (!color) {
    /* Create a black and white color map.  All non-background colors
     * are black. */
    COLOR white = {~0, ~0, ~0, ~0, TRUE};
    COLOR black = {0, 0, 0, ~0, TRUE};
    for (i = 0; i < MAX_COLORS; i++) {
      COLOR *c = &g_array_index (render_color_map, COLOR, i);
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

  /* Render */
  exporter->func ();

  scm_dynwind_end ();
  exit (0);
}

static void
export_cairo_check_error (cairo_status_t status)
{
  if (status != CAIRO_STATUS_SUCCESS) {
    fprintf (stderr, _("ERROR: %s.\n"), cairo_status_to_string (status));
    exit (1);
  }
}

static void
export_page (PAGE *page)
{
  const GList *contents;
  GList *iter;
  cairo_matrix_t render_mtx;
  cairo_t *cr;
  int wx_min, wy_min, wx_max, wy_max;
  double w_width, w_height;
  double scale;

  cr = eda_renderer_get_cairo_context (renderer);

  if (page == NULL) {
    const GList *pages = geda_list_get_glist (toplevel->pages);
    g_assert (pages != NULL && pages->data != NULL);
    page = (PAGE *) pages->data;
  }

  /* Get objects to draw */
  contents = s_page_objects (page);

  /* Calculate and set transformation matrix */
  cairo_save (cr);
  cairo_matrix_init (&render_mtx, 1, 0, 0, -1, -1, -1); /* Very vague approximation */
  cairo_set_matrix (cr, &render_mtx);
  world_get_object_glist_bounds (toplevel, contents,
                                 &wx_min, &wy_min, &wx_max, &wy_max);
  cairo_restore (cr);

  w_width = wx_max - wx_min;
  w_height = wy_max - wy_min;
  scale = fmin (width / w_width, height / w_height);
  cairo_matrix_init (&render_mtx, scale, 0,
                     0, -scale,
                     - (wx_min + w_width / 2) * scale + width / 2,
                     (wy_min + w_height / 2) * scale + height / 2);

  /* Draw background */
  eda_cairo_set_source_color (cr, OUTPUT_BACKGROUND_COLOR,
                              eda_renderer_get_color_map (renderer));
  cairo_paint (cr);

  /* Draw objects & cues */
  cairo_save (cr);
  cairo_set_matrix (cr, &render_mtx);
  contents = s_page_objects (page);
  for (iter = (GList *) contents; iter != NULL; iter = g_list_next (iter))
    eda_renderer_draw (renderer, (OBJECT *) iter->data);
  for (iter = (GList *) contents; iter != NULL; iter = g_list_next (iter))
    eda_renderer_draw_cues (renderer, (OBJECT *) iter->data);
  cairo_restore (cr);
}

static void
export_simple_multipage (cairo_surface_t *surface)
{
  cairo_t *cr = cairo_create (surface);
  GList *iter;

  g_object_set (renderer, "cairo-context", cr, NULL);

  /* Output pages */
  for (iter = (GList *) geda_list_get_glist (toplevel->pages);
       iter != NULL;
       iter = g_list_next (iter)) {
    export_page ((PAGE *) iter->data);
    cairo_show_page (cr);
    export_cairo_check_error (cairo_surface_status (surface));
  }

  cairo_destroy (cr);
}

static void
export_png (void)
{
  cairo_surface_t *surface;
  cairo_t *cr;
  cairo_status_t status;

  /* Create a rendering surface */
  surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32,
                                        (int) ceil (width),
                                        (int) ceil (height));

  /* Create a cairo context */
  cr = cairo_create (surface);

  /* Set up renderer. We need to enable subpixel hinting. */
  g_object_set (renderer,
                "cairo-context", cr,
                "render-flags", EDA_RENDERER_FLAG_HINTING,
                NULL);

  /* Draw */
  export_page (NULL);

  /* Save to file */
  status = cairo_surface_write_to_png (surface, filename);
  export_cairo_check_error (status);
}

static void
export_ps (void)
{
  cairo_surface_t *surface;

  /* Create a rendering surface */
  surface = cairo_ps_surface_create (filename, width, height);

  /* Output pages */
  export_simple_multipage (surface);
  cairo_surface_finish (surface);
  export_cairo_check_error (cairo_surface_status (surface));
}

static void
export_eps (void)
{
  cairo_surface_t *surface;
  cairo_t *cr;

  /* Create a rendering surface */
  surface = cairo_ps_surface_create (filename, width, height);
  cairo_ps_surface_set_eps (surface, TRUE);

  /* Output pages */
  cr = cairo_create (surface);
  g_object_set (renderer, "cairo-context", cr, NULL);
  export_page (NULL);

  cairo_surface_finish (surface);
  export_cairo_check_error (cairo_surface_status (surface));
}

static void
export_pdf (void)
{
  cairo_surface_t *surface;

  surface = cairo_pdf_surface_create (filename, width, height);
  export_simple_multipage (surface);
  cairo_surface_finish (surface);
  export_cairo_check_error (cairo_surface_status (surface));
}

static void export_svg ()
{
  cairo_surface_t *surface;
  cairo_t *cr;

  surface = cairo_svg_surface_create (filename, width, height);
  cr = cairo_create (surface);
  g_object_set (renderer, "cairo-context", cr, NULL);
  export_page (NULL);
  cairo_surface_finish (surface);
  export_cairo_check_error (cairo_surface_status (surface));
}
