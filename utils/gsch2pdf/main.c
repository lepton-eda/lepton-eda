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



static PrintSettings *print_settings = NULL;

static void print_object_list(TOPLEVEL *current, cairo_t *cairo, const GList *objects);



static void print_arc(TOPLEVEL *current, cairo_t *cairo, OBJECT *object)
{
    cairo_set_line_width(
        cairo,
        object->line_width > 10.0 ? object->line_width : 10.0
        );

    cairo_set_source_rgb(
        cairo,
        0.0,
        0.0,
        0.0
        );

    cairo_new_sub_path(cairo);

    if (object->arc->end_angle > 0)
    {
        cairo_arc(
            cairo,
            object->arc->x,
            object->arc->y,
            object->arc->width / 2.0,
            M_PI * object->arc->start_angle / 180.0,
            M_PI * (object->arc->start_angle + object->arc->end_angle) / 180.0
            );
    }
    else
    {
         cairo_arc_negative(
            cairo,
            object->arc->x,
            object->arc->y,
            object->arc->width / 2.0,
            M_PI * object->arc->start_angle / 180.0,
            M_PI * (object->arc->start_angle + object->arc->end_angle) / 180.0
            );
    }

    cairo_stroke(cairo);
}



static void print_box(TOPLEVEL *current, cairo_t *cairo, OBJECT *object)
{
    cairo_set_source_rgb(
        cairo,
        0.0,
        0.0,
        0.0
        );

    if ((object->fill_type == FILLING_HATCH) || (object->fill_type == FILLING_MESH))
    {
        int index;
        GArray *lines = g_array_new (FALSE, FALSE, sizeof (LINE));

        m_hatch_box(object->box, object->fill_angle1, object->fill_pitch1, lines);

        if (object->fill_type == FILLING_MESH)
        {
            m_hatch_box(object->box, object->fill_angle2, object->fill_pitch2, lines);
        }

        cairo_set_line_width(
            cairo,
            object->fill_width > 5.0 ? object->fill_width : 5.0
            );

        for (index=0; index<lines->len; index++)
        {
            LINE *line = &g_array_index(lines, LINE, index);

            cairo_move_to(
                cairo,
                line->x[0],
                line->y[0]
                );

            cairo_line_to(
                cairo,
                line->x[1],
                line->y[1]
                );
        }
    }

    cairo_set_line_width(
        cairo,
        object->line_width > 10.0 ? object->line_width : 10.0
        );

    cairo_move_to(
        cairo,
        object->box->upper_x,
        object->box->upper_y
        );

    cairo_line_to(
        cairo,
        object->box->lower_x,
        object->box->upper_y
        );

    cairo_line_to(
        cairo,
        object->box->lower_x,
        object->box->lower_y
        );

    cairo_line_to(
        cairo,
        object->box->upper_x,
        object->box->lower_y
        );

    cairo_close_path(cairo);

    if (object->fill_type == FILLING_FILL)
    {
        cairo_fill_preserve(cairo);
    }

    cairo_stroke(cairo);
}



static void print_bus(TOPLEVEL *current, cairo_t *cairo, OBJECT *object)
{

    cairo_set_line_width(
        cairo,
        30.0
        );

    cairo_set_source_rgb(
        cairo,
        0.0,
        0.0,
        0.0
        );

    cairo_move_to(
        cairo,
        object->line->x[0],
        object->line->y[0]
        );

    cairo_line_to(
        cairo,
        object->line->x[1],
        object->line->y[1]
        );

    cairo_stroke(cairo);
}



static void print_circle(TOPLEVEL *current, cairo_t *cairo, OBJECT *object)
{
    cairo_set_source_rgb(
        cairo,
        0.0,
        0.0,
        0.0
        );

    if ((object->fill_type == FILLING_HATCH) || (object->fill_type == FILLING_MESH))
    {
        int index;
        GArray *lines = g_array_new (FALSE, FALSE, sizeof (LINE));

        m_hatch_circle(object->circle, object->fill_angle1, object->fill_pitch1, lines);

        if (object->fill_type == FILLING_MESH)
        {
            m_hatch_circle(object->circle, object->fill_angle2, object->fill_pitch2, lines);
        }

        cairo_set_line_width(
            cairo,
            object->fill_width > 5.0 ? object->fill_width : 5.0
            );


        for (index=0; index<lines->len; index++)
        {
            LINE *line = &g_array_index(lines, LINE, index);

            cairo_move_to(
                cairo,
                line->x[0],
                line->y[0]
                );

            cairo_line_to(
                cairo,
                line->x[1],
                line->y[1]
                );
        }
    }

    cairo_set_line_width(
        cairo,
        object->line_width > 10.0 ? object->line_width : 10.0
        );

    cairo_new_sub_path(cairo);

    cairo_arc(
        cairo,
        object->circle->center_x,
        object->circle->center_y,
        object->circle->radius,
        0.0,
        2.0 * M_PI
        );

    if (object->fill_type == FILLING_FILL)
    {
        cairo_fill(cairo);
    }

    cairo_stroke(cairo);
}



static void print_complex(TOPLEVEL *current, cairo_t *cairo, OBJECT *object)
{
    print_object_list(current, cairo, object->complex->prim_objs);
}


static void print_junctions(TOPLEVEL *current, cairo_t *cairo, const GArray *junctions)
{
    int index;

    cairo_set_source_rgb(
        cairo,
        0.0,
        0.0,
        0.0
        );

  for (index=0; index<junctions->len; index++) {
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

static void print_line(TOPLEVEL *current, cairo_t *cairo, OBJECT *object)
{
    cairo_set_line_width(
        cairo,
        object->line_width > 10.0 ? object->line_width : 10.0
        );

    cairo_set_source_rgb(
        cairo,
        0.0,
        0.0,
        0.0
        );

    cairo_move_to(
        cairo,
        object->line->x[0],
        object->line->y[0]
        );

    cairo_line_to(
        cairo,
        object->line->x[1],
        object->line->y[1]
        );

    cairo_stroke(cairo);
}



static void print_net(TOPLEVEL *current, cairo_t *cairo, OBJECT *object)
{
    cairo_set_line_width(
        cairo,
        10.0
        );

    cairo_set_source_rgb(
        cairo,
        0.0,
        0.0,
        0.0
        );

    cairo_move_to(
        cairo,
        object->line->x[0],
        object->line->y[0]
        );

    cairo_line_to(
        cairo,
        object->line->x[1],
        object->line->y[1]
        );

    cairo_stroke(cairo);
}



static void print_path(TOPLEVEL *current, cairo_t *cairo, OBJECT *object)
{
    int index;

    cairo_set_source_rgb(
        cairo,
        0.0,
        0.0,
        0.0
        );

    if ((object->fill_type == FILLING_HATCH) || (object->fill_type == FILLING_MESH))
    {
        int index;
        GArray *lines = g_array_new (FALSE, FALSE, sizeof (LINE));

        m_hatch_path(object->path, object->fill_angle1, object->fill_pitch1, lines);

        if (object->fill_type == FILLING_MESH)
        {
            m_hatch_path(object->path, object->fill_angle2, object->fill_pitch2, lines);
        }

        cairo_set_line_width(
            cairo,
            object->fill_width > 5.0 ? object->fill_width : 5.0
            );

        for (index=0; index<lines->len; index++)
        {
            LINE *line = &g_array_index(lines, LINE, index);

            cairo_move_to(
                cairo,
                line->x[0],
                line->y[0]
                );

            cairo_line_to(
                cairo,
                line->x[1],
                line->y[1]
                );
        }
    }

    for (index=0; index<object->path->num_sections; index++)
    {
        PATH_SECTION *section = object->path->sections + index;

        switch (section->code)
        {
            case PATH_MOVETO:
                cairo_close_path(cairo);

            case PATH_MOVETO_OPEN:
                cairo_move_to(
                    cairo,
                    section->x3,
                    section->y3
                    );
                break;

            case PATH_CURVETO:
                cairo_curve_to(
                    cairo,
                    section->x1,
                    section->y1,
                    section->x2,
                    section->y2,
                    section->x3,
                    section->y3
                    );
                break;

            case PATH_LINETO:
                cairo_line_to(
                    cairo,
                    section->x3,
                    section->y3
                    );
                break;

            case PATH_END:
                cairo_close_path(cairo);
                break;
        }
    }

    cairo_set_line_width(
        cairo,
        object->line_width > 10.0 ? object->line_width : 10.0
        );

    if (object->fill_type == FILLING_FILL)
    {
        cairo_fill_preserve(cairo);
    }

    cairo_stroke(cairo);
}



static void print_pin(TOPLEVEL *current, cairo_t *cairo, OBJECT *object)
{
    cairo_set_line_width(
        cairo,
        object->line_width > 10.0 ? object->line_width : 10.0
        );

    cairo_set_source_rgb(
        cairo,
        0.0,
        0.0,
        0.0
        );

    cairo_move_to(
        cairo,
        object->line->x[0],
        object->line->y[0]
        );

    cairo_line_to(
        cairo,
        object->line->x[1],
        object->line->y[1]
        );

    cairo_stroke(cairo);
}



static void print_text(TOPLEVEL *current, cairo_t *cairo, OBJECT *object)
{
    if (object->text->disp_string != NULL)
    {
        cairo_save(cairo);

        cairo_move_to(
            cairo,
            object->text->x,
            object->text->y
            );

        cairo_scale(
            cairo,
            1.0,
            -1.0
            );

        int flip = (object->text->angle == 180);

        if (!flip)
        {
            cairo_rotate(
                cairo,
                M_PI * object->text->angle / -180.0
                );
        }

        PangoContext *context = pango_cairo_create_context(cairo);

        pango_cairo_context_set_resolution(
            context,
            1600.0
            );

        PangoLayout *layout = pango_layout_new(context);

        PangoAlignment halign = PANGO_ALIGN_LEFT;

        switch (object->text->alignment)
        {
            case LOWER_LEFT:
            case MIDDLE_LEFT:
            case UPPER_LEFT:
                halign = flip ? PANGO_ALIGN_RIGHT : PANGO_ALIGN_LEFT;
               break;

            case LOWER_MIDDLE:
            case MIDDLE_MIDDLE:
            case UPPER_MIDDLE:
                halign = PANGO_ALIGN_CENTER;
                break;

            case LOWER_RIGHT:
            case MIDDLE_RIGHT:
            case UPPER_RIGHT:
                halign = flip ? PANGO_ALIGN_LEFT : PANGO_ALIGN_RIGHT;
                break;
        }

        pango_layout_set_alignment(layout, halign);

        PangoFontDescription *desc;
        desc = pango_font_description_from_string(print_settings_get_font(print_settings));

        pango_font_description_set_size(desc, PANGO_SCALE * object->text->size);

        pango_layout_set_font_description(layout, desc);
        pango_font_description_free(desc);

        /* Begin cap height computation */

        pango_layout_set_text(layout, "I", -1);
        pango_cairo_update_layout(cairo, layout);

        PangoRectangle extents_ink;
        PangoRectangle extents_logical;

        pango_layout_get_extents(
            layout,
            &extents_ink,
            &extents_logical
            );

        int coffset = extents_ink.y;

        /* End cap height computation */

        pango_layout_set_text(layout, object->text->disp_string, -1);
        pango_cairo_update_layout(cairo, layout);

        int baseline = pango_layout_get_baseline(layout);

        PangoLayoutIter *iter = pango_layout_get_iter(layout);

        while (pango_layout_iter_next_line(iter))
        {
            baseline = pango_layout_iter_get_baseline(iter);
        }

        pango_layout_iter_free(iter);


        pango_layout_get_extents(
            layout,
            &extents_ink,
            &extents_logical
            );

        double xalign = 0.0;
        double xoffset;
        double yalign = 0.0;
        double yoffset;

        switch (object->text->alignment)
        {
            case LOWER_LEFT:
            case MIDDLE_LEFT:
            case UPPER_LEFT:
                xalign = 0.0;
                break;

            case LOWER_MIDDLE:
            case MIDDLE_MIDDLE:
            case UPPER_MIDDLE:
                xalign = 0.5;
                break;

            case LOWER_RIGHT:
            case MIDDLE_RIGHT:
            case UPPER_RIGHT:
                xalign = 1.0;
                break;
        }

        if (flip)
        {
            xalign = 1.0 - xalign;
        }

        xoffset = extents_ink.x + xalign * extents_ink.width;

        switch (object->text->alignment)
        {
            case LOWER_LEFT:
            case LOWER_MIDDLE:
            case LOWER_RIGHT:
                yalign = 0.0;
                break;

            case MIDDLE_LEFT:
            case MIDDLE_MIDDLE:
            case MIDDLE_RIGHT:
                yalign = 0.5;
                break;

            case UPPER_LEFT:
            case UPPER_MIDDLE:
            case UPPER_RIGHT:
                yalign = 1.0;
                break;
        }

        if (flip)
        {
            yalign = 1.0 - yalign;
        }

        yoffset = baseline + yalign * (coffset - baseline);

        cairo_rel_move_to(
            cairo,
            xoffset / -1024.0,
            yoffset / -1024.0
            );

        pango_cairo_show_layout(cairo, layout);

        g_object_unref(layout);
        g_object_unref(context);

        cairo_restore(cairo);
    }
}



static void print_object(TOPLEVEL *current, cairo_t *cairo, OBJECT *object)
{
    if (o_is_visible(current, object))
    {
        switch (object->type)
        {
            case OBJ_ARC:
                print_arc(current, cairo, object);
                break;

            case OBJ_BOX:
                print_box(current, cairo, object);
                break;

            case OBJ_BUS:
                print_bus(current, cairo, object);
                break;

            case OBJ_CIRCLE:
                print_circle(current, cairo, object);
                break;

            case OBJ_COMPLEX:
            case OBJ_PLACEHOLDER:
                print_complex(current, cairo, object);
                break;

            case OBJ_LINE:
                print_line(current, cairo, object);
                break;

            case OBJ_NET:
                print_net(current, cairo, object);
                break;

            case OBJ_PATH:
                print_path(current, cairo, object);
                break;

            case OBJ_PIN:
                print_pin(current, cairo, object);
                break;

            case OBJ_TEXT:
                print_text(current, cairo, object);
                break;

            default:
                printf("default\n");
        }
    }
}



static void print_object_list(TOPLEVEL *current, cairo_t *cairo, const GList *objects)
{
    const GList *node = objects;

    while (node != NULL)
    {
        OBJECT *object = (OBJECT*) node->data;

        print_object(current, cairo, object);

        node = g_list_next(node);
    }
}



static void print_page(TOPLEVEL *current, cairo_t *cairo, PAGE *page)
{
    cairo_save(cairo);

    const GList *list = s_page_objects(page);

    cairo_surface_t *surface = cairo_recording_surface_create(CAIRO_CONTENT_COLOR_ALPHA, NULL);

    cairo_t *cairo2 = cairo_create(surface);

    cairo_rectangle_t rectangle;

    print_object_list(current, cairo2, list);

    cairo_recording_surface_ink_extents(
        surface,
        &rectangle.x,
        &rectangle.y,
        &rectangle.width,
        &rectangle.height
        );

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

    print_object_list(current, cairo, list);

    GArray *junctions = g_array_new(FALSE, FALSE, sizeof(sPOINT));
    
    junction_locate(current, list, junctions, NULL);

    print_junctions(current, cairo, junctions);

    g_array_free(junctions, TRUE);

    cairo_restore(cairo);
}



static void main2(void *closure, int argc, char *argv[])
{
    TOPLEVEL *current;
    int i;

    print_settings = print_settings_new();

    libgeda_init();

    s_log_init ("gsch2pdf");

    current = s_toplevel_new();

    rc_config_init();
    rc_config_set_print_settings(print_settings);

    g_rc_parse(current, argv[0], "gsch2pdfrc", NULL);

    i_vars_libgeda_set(current);

    cairo_surface_t *surface = cairo_pdf_surface_create(
        "output.pdf",
        72.0 * print_settings_get_page_width(print_settings),
        72.0 * print_settings_get_page_height(print_settings)
        );

    cairo_t *cairo = cairo_create(surface);

    for (i=1; i<argc; i++)
    {
        PAGE *page = s_page_new(current, argv[i]);

        int success = f_open(current, page, argv[i], NULL);

        if (success)
        {
            print_page(current, cairo, page);

            cairo_show_page(cairo);
        }

        s_page_delete(current, page);
    }

    s_page_delete_list(current);

    s_clib_free();
    s_slib_free();

    cairo_destroy(cairo);

    cairo_surface_flush(surface);
    cairo_surface_destroy(surface);

    rc_config_set_print_settings(NULL);

    exit(EXIT_SUCCESS);
}



int main(int argc, char *argv[])
{
    scm_boot_guile(argc, argv, main2, NULL);

    return EXIT_FAILURE;
}

