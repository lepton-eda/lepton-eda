/* gEDA - GPL Electronic Design Automation
 * gschlas - gEDA Load and Save
 * Copyright (C) 2002-2012 Ales Hvezda
 * Copyright (C) 2002-2012 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>

#include <stdio.h>
#include <sys/stat.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>
#include <libgedacairo/libgedacairo.h>

#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


#include <stdlib.h>
#include <math.h>


#include <pango/pangocairo.h>
#include <cairo-pdf.h>

#include "junction.h"
#include "print-settings.h"
#include "rc-config.h"



static void print_page(TOPLEVEL *current, EdaRenderer *renderer, PAGE *page);

TOPLEVEL *current;
static GArray* print_color_map = NULL;
static PrintSettings *print_settings = NULL;

typedef struct GSCH2PDF GSCH2PDF;

struct GSCH2PDF
{
    GPtrArray* input_filenames;
    gchar*     output_filename;
};



GSCH2PDF* gsch2pdf_new()
{
    GSCH2PDF* gsch2pdf = g_new0(GSCH2PDF, 1);

    gsch2pdf->input_filenames = g_ptr_array_new_with_free_func(g_free);
    gsch2pdf->output_filename = g_strdup("output.pdf");

    return gsch2pdf;
}



GSCH2PDF* gsch2pdf_new_with_args(int argc, gchar *argv[])
{
    GSCH2PDF* gsch2pdf = gsch2pdf_new();

    int index;
    int option;

    while ((option = getopt(argc, argv, "o:")) != -1)
    {
        switch (option)
        {
            case 'o':
                gsch2pdf->output_filename = g_strdup(optarg);
                break;

            default:
                break;
        }
    }

    for (index=optind; index<argc; index++)
    {
        g_ptr_array_add(gsch2pdf->input_filenames, g_strdup(argv[index]));
    }

    return gsch2pdf;
}



void gsch2pdf_free(GSCH2PDF* gsch2pdf)
{
    if (gsch2pdf != NULL)
    {
        g_ptr_array_free(gsch2pdf->input_filenames, TRUE);
        g_free(gsch2pdf->output_filename);

        g_free(gsch2pdf);
    }
}



void gsch2pdf_run(GSCH2PDF* gsch2pdf)
{
    int page_count = gsch2pdf->input_filenames->len;

    if (page_count > 0)
    {
        cairo_t* cairo = NULL;
        EdaRenderer *renderer = NULL;
        cairo_surface_t* surface;

        surface = cairo_pdf_surface_create(
            gsch2pdf->output_filename,
            72.0 * print_settings_get_page_width(print_settings),
            72.0 * print_settings_get_page_height(print_settings)
            );

        if (surface != NULL)
        {
            cairo = cairo_create(surface);
            cairo_surface_destroy(surface);
        }

        if (cairo != NULL)
        {
            renderer = g_object_new(
                EDA_TYPE_RENDERER,
                "cairo-context", cairo,
                "color-map",     print_color_map,
                NULL
                );
        }

        if (renderer != NULL)
        {
            int index;

            for (index=0; index<page_count; index++)
            {
                gchar* input_filename = g_ptr_array_index(gsch2pdf->input_filenames, index);

                PAGE *page = s_page_new(current, input_filename);

                int success = f_open(current, page, input_filename, NULL);

                if (success)
                {
                    print_page(current, renderer, page);
                    cairo_show_page(cairo);
                }

                s_page_delete(current, page);
            }

            g_object_unref(renderer);
         }

         if (cairo != NULL)
         {
             cairo_destroy(cairo);
         }
    }
}


GArray* create_color_map(void)
{
    GArray *color_map;
    int index;

    color_map = g_array_sized_new (FALSE, FALSE, sizeof(COLOR), MAX_COLORS);
    color_map = g_array_append_vals (color_map, print_colors, MAX_COLORS);

    for (index=0; index<MAX_COLORS; index++)
    {
        COLOR *color = &g_array_index(color_map, COLOR, index);

        if (!color->enabled)
        {
            continue;
        }

        if (color->a == 0 || index == BACKGROUND_COLOR)
        {
            color->enabled = FALSE;
            continue;
        }

        if (FALSE)  /* TODO: needs to be the print setting to enable B&W */
        {
            color->r = 0;
            color->g = 0;
            color->b = 0;
            color->a = ~0;
        }
    }

    return color_map;
}



static void print_junctions(EdaRenderer *renderer, const GArray *junctions)
{
    cairo_t *cairo = eda_renderer_get_cairo_context(renderer);
    GArray *colors = eda_renderer_get_color_map(renderer);
    int index;

    COLOR *color = &g_array_index(colors, COLOR, JUNCTION_COLOR);

    cairo_set_source_rgb(
        cairo,
        color->r / 255.0,
        color->g / 255.0,
        color->b / 255.0
        );

    for (index=0; index<junctions->len; index++)
    {
        sPOINT junction = g_array_index(junctions, sPOINT ,index);

        cairo_arc(
            cairo,
            junction.x,
            junction.y,
            print_settings_get_junction_size_net(print_settings),
            0,
            2 * M_PI
            );

        cairo_fill(cairo);
    }
}



static void print_object_list(EdaRenderer *renderer, const GList *objects)
{
    const GList *node = objects;

    while (node != NULL)
    {
        OBJECT *object = (OBJECT*) node->data;

        eda_renderer_draw(renderer, object);

        node = g_list_next(node);
    }
}



static int
text_rendered_bounds (void *user_data, OBJECT *object,
                      int *left, int *top, int *right, int *bottom)
{
  int result;
  double t, l, r, b;
  EdaRenderer *renderer = EDA_RENDERER (user_data);
  result = eda_renderer_get_user_bounds (renderer, object, &l, &t, &r, &b);
  *left = lrint (fmin (l,r));
  *top = lrint (fmin (t, b));
  *right = lrint (fmax (l, r));
  *bottom = lrint (fmax (t, b));
  return result;
}

static void print_page(TOPLEVEL *current, EdaRenderer *renderer, PAGE *page)
{
    cairo_matrix_t mtx;
    cairo_t *cairo = eda_renderer_get_cairo_context(renderer);

    const GList *list = s_page_objects(page);

    int wx_min = 0, wy_min = 0, wx_max = 0, wy_max = 0;
    cairo_save (cairo);
    cairo_matrix_init (&mtx, 1, 0, 0, -1, -1, -1); /* Very vague approximation */
    cairo_set_matrix (cairo, &mtx);

    world_get_object_glist_bounds (current, list, &wx_min, &wy_min, &wx_max, &wy_max);
    cairo_restore (cairo);

    cairo_rectangle_t rectangle;
    rectangle.x = wx_min;
    rectangle.y = wy_min;
    rectangle.width = wx_max - wx_min;
    rectangle.height = wy_max - wy_min;

    cairo_save (cairo);

    double sx = 72.0 * print_settings_get_print_width(print_settings) / rectangle.width;
    double sy = 72.0 * print_settings_get_print_height(print_settings) / rectangle.height;

    double s;

    if (sx < sy)
    {
        s = sx;
    }
    else
    {
        s = sy;
    }

    cairo_translate(
        cairo,
        72.0 * print_settings_get_page_margin_left(print_settings) + (72.0 * print_settings_get_print_width(print_settings) - s * rectangle.width) * print_settings_get_page_align_horizontal(print_settings),
        72.0 * (print_settings_get_page_margin_top(print_settings) + print_settings_get_print_height(print_settings)) - (72.0 * print_settings_get_print_height(print_settings) - s * rectangle.height) * print_settings_get_page_align_vertical(print_settings)
        );

    cairo_scale(
        cairo,
        s,
        -s
        );

    cairo_translate(
        cairo,
        -rectangle.x,
        -rectangle.y
        );

    print_object_list(renderer, list);

    GArray *junctions = g_array_new(FALSE, FALSE, sizeof(sPOINT));

    junction_locate(current, list, junctions, NULL);

    print_junctions(renderer, junctions);

    g_array_free(junctions, TRUE);

    cairo_restore(cairo);
}



static void main2(void *closure, int argc, char *argv[])
{
    GSCH2PDF *gsch2pdf;

    libgeda_init();

    s_log_init ("gsch2pdf");

    current = s_toplevel_new();

    print_settings = print_settings_new();

    rc_config_init();
    rc_config_set_print_settings(print_settings);

    g_rc_parse(current, argv[0], "gsch2pdfrc", NULL);

    i_vars_libgeda_set(current);

    print_color_map = create_color_map();

    gsch2pdf = gsch2pdf_new_with_args(argc, argv);

    gsch2pdf_run(gsch2pdf);
    gsch2pdf_free(gsch2pdf);

    s_page_delete_list(current);

    s_clib_free();
    s_slib_free();

    rc_config_set_print_settings(NULL);

    g_array_free(print_color_map, TRUE);

    exit(EXIT_SUCCESS);
}



int main(int argc, char *argv[])
{
    scm_boot_guile(argc, argv, main2, NULL);

    return EXIT_FAILURE;
}

