/* Copyright (C) 2013-2015 Roland Lutz

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  */

#include <stdio.h>
#include <string.h>
#include <xornstorage.h>


int main()
{
    xorn_revision_t rev0, rev1, rev2, rev3;
    xorn_object_t line, box, circle;

    struct xornsch_line line_data;
    struct xornsch_box box_data;
    struct xornsch_circle circle_data;
    struct xornsch_net net_data;


    /* Create an empty revision and make it read-only. */

    rev0 = xorn_new_revision(NULL);

    if (rev0 == NULL)
        goto error0;

    xorn_finalize_revision(rev0);


    /* Copy the revision */

    /* You could omit the previous step and just create an empty
       revision if you don't need to refer to the empty state later. */

    rev1 = xorn_new_revision(rev0);

    if (rev1 == NULL)
        goto error1;

    /* Prepare a xornsch_line structure */

    memset(&line_data, 0, sizeof line_data);
    line_data.pos.x = 0;
    line_data.pos.y = 1;
    line_data.size.x = 3;
    line_data.size.y = 2;
    line_data.color = 3;
    line_data.line.width = 1;

    /* Add a line object with this data to the revision */

    line = xornsch_add_line(rev1, &line_data);

    if (line == NULL)
        goto error2;

    /* Finalize the revision */

    xorn_finalize_revision(rev1);


    /* Create a copy of the last revision, add a box and a circle
       to it, and finalize it */

    rev2 = xorn_new_revision(rev1);
    if (rev2 == NULL)
        goto error2;

    memset(&box_data, 0, sizeof box_data);
    box_data.pos.x = 1;
    box_data.pos.y = 1;
    box_data.size.x = 2;
    box_data.size.y = 2;
    box_data.color = 3;
    box_data.line.width = 1;

    box = xornsch_add_box(rev2, &box_data);
    if (box == NULL)
        goto error3;

    memset(&circle_data, 0, sizeof circle_data);
    circle_data.pos.x = -1;
    circle_data.pos.y = -1;
    circle_data.radius = 2;
    circle_data.color = 3;
    circle_data.line.width = 1;
    circle_data.fill.type = 1;

    circle = xornsch_add_circle(rev2, &circle_data);
    if (circle == NULL)
        goto error3;

    xorn_finalize_revision(rev2);


    /* Create a copy of the last revision, set the line's object
       type to net and change its color, delete the box, and
       finalize the revision */

    rev3 = xorn_new_revision(rev2);
    if (rev3 == NULL)
        goto error3;

    memset(&net_data, 0, sizeof net_data);
    net_data.pos.x = 0;
    net_data.pos.y = 1;
    net_data.size.x = 3;
    net_data.size.y = 2;
    net_data.color = 4;

    if (xornsch_set_net_data(rev3, line, &net_data) == -1)
        goto error4;

    xorn_delete_object(rev3, box);

    xorn_finalize_revision(rev3);


    /* ... you could do something with these revisions now ... */


    /* Free the revisions (the order doesn't matter) */

    xorn_free_revision(rev3);
    xorn_free_revision(rev2);
    xorn_free_revision(rev1);
    xorn_free_revision(rev0);

    /* No need to free objects */

    return 0;

error4:
    xorn_free_revision(rev3);
error3:
    xorn_free_revision(rev2);
error2:
    xorn_free_revision(rev1);
error1:
    xorn_free_revision(rev0);
error0:
    fprintf(stderr, "Out of memory\n");
    return 1;
}
