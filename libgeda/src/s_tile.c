/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>
#include <math.h>

#ifndef HAVE_VSNPRINTF
#include <stdarg.h>
#endif

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>
#include <guile/gh.h>

#include "defines.h"
#include "struct.h"
#include "defines.h"
#include "globals.h"

#include "o_types.h"

#include "../include/colors.h"
#include "../include/prototype.h"


void s_tile_init(TOPLEVEL * w_current, PAGE * p_current)
{
    int i, j;
    TILE *t_current;
    int x_size = w_current->init_right / MAX_TILES_X;
    int y_size = w_current->init_bottom / MAX_TILES_Y;
    int x_sum = 0;
    int y_sum = 0;

#if DEBUG 
    printf("X, Y: %d %d\n", x_size, y_size);
#endif
    
    for (j = 0; j < MAX_TILES_Y; j++) {
	for (i = 0; i < MAX_TILES_X; i++) {
	    t_current = &p_current->world_tiles[i][j];

	    t_current->objects = NULL;

	    t_current->left = x_sum;
	    t_current->right = x_sum + x_size;

	    t_current->top = y_sum;
	    t_current->bottom = y_sum + y_size;

	    x_sum = x_sum + x_size;
	}
	x_sum = 0;
	y_sum = y_sum + y_size;
    }

#if DEBUG
    for (j = 0; j < MAX_TILES_Y; j++) {
	for (i = 0; i < MAX_TILES_X; i++) {
	    t_current = &p_current->world_tiles[i][j];
	    printf("\n%d %d\n", i, j);
	    printf("----------------\n");
	    printf("left %d top %d\n", t_current->left, t_current->top);
	    printf("right %d bottom %d\n", t_current->right,
		   t_current->bottom);
	}
    }
#endif
}

TILE_LOC *s_tile_new_loc(int i, int j)
{
    TILE_LOC *tile_loc;

    tile_loc = (TILE_LOC *) malloc(sizeof(TILE_LOC));

    tile_loc->i = i;
    tile_loc->j = j;

    return (tile_loc);
}

void s_tile_add_object(TOPLEVEL * w_current, OBJECT * object, int world_x1,
		       int world_y1, int world_x2, int world_y2)
{
  TILE *t_current;
  PAGE *p_current;
  GList *found;
  TILE_LOC *tile_loc;
  int i, j;
  int v, w;
  double x1, y1, x2, y2;
  double bottom;
  double m, b;
  double x_size, y_size;
  double x, y;
  int start, end;

#if DEBUG  
  printf("name: %s\n", object->name);
#endif

  if (w_current->ADDING_SEL) {
#if DEBUG    
    printf("s_tile_add_object, adding sel TRUE\n");
#endif
    return;
  }
  
  x_size = (double) w_current->init_right / (double) MAX_TILES_X;
  y_size = (double) w_current->init_bottom / (double) MAX_TILES_Y;

  x1 = (int) (world_x1 / x_size);
  x2 = (int) (world_x2 / x_size);
  y1 = (int) (world_y1 / y_size);
  y2 = (int) (world_y2 / y_size);

  bottom = x2 - x1;
  p_current = w_current->page_current;

  if (bottom != 0.0) {
    m = (double) (y2 - y1) / bottom;
    b = y1 - m * x1;

    start = min((int) x1, (int) x2);
    end = max((int) x1, (int) x2);
    for (i = start; i <= end; i++) {
      x = i;
      y = m * x + b;
      if (floor(y) != ceil(y)) {

        v = (int) x;
        w = (int) floor(y);
        if (v < 0 || w < 0 || v > MAX_TILES_X-1 || w > MAX_TILES_Y-1) {
          return;
        }
        /* assert(v < MAX_TILES_X && w < MAX_TILES_Y && v >= 0 && w >= 0); */
        t_current = &p_current->world_tiles[v][w];
        found = g_list_find(t_current->objects, object);

        if (!found) {
          /*printf("%d %d\n", v, w);*/
          t_current->objects = g_list_append(t_current->objects,
                                             object);

          tile_loc = s_tile_new_loc(v, w);
          object->tile_locs = g_list_append(object->tile_locs,
                                            tile_loc);
        }

        v = (int) x;
        w = (int) ceil(y);
        if (v < 0 || w < 0 || v > MAX_TILES_X-1 || w > MAX_TILES_Y-1) {
          return;
        }
        /*assert(v < MAX_TILES_X && w < MAX_TILES_Y && v >= 0 && w >= 0);*/
        t_current = &p_current->world_tiles[v][w];
        found = g_list_find(t_current->objects, object);

        if (!found) {
          /*printf("%d %d\n", v, w);*/
          t_current->objects = g_list_append(t_current->objects,
                                             object);
          tile_loc = s_tile_new_loc(v, w);
          object->tile_locs = g_list_append(object->tile_locs,
                                            tile_loc);
        }

      } else {
        v = (int) x;
        w = (int) floor(y);
        if (v < 0 || w < 0 || v > MAX_TILES_X-1 || w > MAX_TILES_Y-1) {
          return;
        }
        /*assert(v < MAX_TILES_X && w < MAX_TILES_Y && v >= 0 && w >= 0);*/
        t_current = &p_current->world_tiles[v][w];
        found = g_list_find(t_current->objects, object);

        if (!found) {
          /*printf("%d %d\n", v, w);*/
          t_current->objects = g_list_append(t_current->objects,
                                             object);
          tile_loc = s_tile_new_loc(v, w);
          object->tile_locs = g_list_append(object->tile_locs,
                                            tile_loc);
        }
      }
    }

    if (m != 0.0) {
      start = min((int) y1, (int) y2);
      end = max((int) y1, (int) y2);
      for (j = start; j <= end; j++) {
        y = j;
        x = (y - b) / m;
        if (floor(x) != ceil(x)) {
          w = (int) y;
          v = (int) floor(x);
          if (v < 0 || w < 0 || v > MAX_TILES_X-1 || w > MAX_TILES_Y-1) {
            return;
          }
          /*assert(v < MAX_TILES_X && w < MAX_TILES_Y &&
            v >= 0 && w >= 0);*/
          t_current = &p_current->world_tiles[v][w];
          found = g_list_find(t_current->objects, object);

          if (!found) {
            /*printf("%d %d\n", v, w);*/
            t_current->objects =
              g_list_append(t_current->objects, object);
            tile_loc = s_tile_new_loc(v, w);
            object->tile_locs =
              g_list_append(object->tile_locs, tile_loc);
          }

          w = (int) y;
          v = (int) ceil(x);
          if (v < 0 || w < 0 || v > MAX_TILES_X-1 || w > MAX_TILES_Y-1) {
            return;
          }
          /* assert(v < MAX_TILES_X && w < MAX_TILES_Y &&
             v >= 0 && w >= 0);*/
          t_current = &p_current->world_tiles[v][w];
          found = g_list_find(t_current->objects, object);

          if (!found) {
            /*printf("%d %d\n", v, w);*/
            t_current->objects =
              g_list_append(t_current->objects, object);
            tile_loc = s_tile_new_loc(v, w);
            object->tile_locs =
              g_list_append(object->tile_locs, tile_loc);
          }

        } else {
          w = (int) y;
          v = (int) floor(x);
          if (v < 0 || w < 0 || v > MAX_TILES_X-1 || w > MAX_TILES_Y-1) {
            return;
          }
          /*assert(v < MAX_TILES_X && w < MAX_TILES_Y &&
            v >= 0 && w >= 0);*/
          t_current = &p_current->world_tiles[v][w];
          found = g_list_find(t_current->objects, object);

          if (!found) {
            /*printf("%d %d\n", v, w);*/
            t_current->objects =
              g_list_append(t_current->objects, object);
            tile_loc = s_tile_new_loc(v, w);
            object->tile_locs =
              g_list_append(object->tile_locs, tile_loc);
          }
        }
      }
    }
  } else {
    start = min((int) y1, (int) y2);
    end = max((int) y1, (int) y2);
    for (j = start; j <= end; j++) {

      y = j;
      x = x1;
      v = (int) x;
      w = (int) y;
      if (v < 0 || w < 0 || v > MAX_TILES_X-1 || w > MAX_TILES_Y-1) {
        return;
      }
      /*assert(v < MAX_TILES_X && w < MAX_TILES_Y &&
        v >= 0 && w >= 0);*/
      t_current = &p_current->world_tiles[v][w];
      found = g_list_find(t_current->objects, object);

      if (!found) {
        /*printf("%d %d\n", v, w);*/
        t_current->objects = g_list_append(t_current->objects,
                                           object);
        tile_loc = s_tile_new_loc(v, w);
        object->tile_locs = g_list_append(object->tile_locs,
                                          tile_loc);
      }

    }
  }
}

void s_tile_remove_object_all_crude(TOPLEVEL * w_current, OBJECT * object)
{
  TILE *t_current;

  int i, j;

  for (j = 0; j < MAX_TILES_Y; j++) {
    for (i = 0; i < MAX_TILES_X; i++) {
#if 0				/* this checks for existance */
      t_current = &w_current->page_current->world_tiles[i][j];
      found = g_list_find(t_current->objects, object);

      if (found) {
        printf("found object %s in %d, %d\n", object->name, i, j);
        t_current->objects = g_list_remove(t_current->objects,
                                           object);
      }
#endif

      /* this just does the remove if the object is found */
#if 1
      t_current = &w_current->page_current->world_tiles[i][j];
      t_current->objects = g_list_remove(t_current->objects, object);
#endif
    }
  }
}

/* this is still wrong, p_current needs to be the page the object is on */
/* doesn't work if the current page isn't where the object lives */
void s_tile_remove_object_all(TOPLEVEL * w_current, PAGE *p_current,
                              OBJECT * object)
{
  TILE *t_current;
  TILE_LOC *tl_current;
  GList *tloc_list;
  GList *found;
  int i, j;

  tloc_list = object->tile_locs;
  while (tloc_list != NULL) {

    tl_current = (TILE_LOC *) tloc_list->data;
        
    i = tl_current->i;
    j = tl_current->j;

    free(tl_current);

    t_current = &p_current->world_tiles[i][j];
    t_current->objects = g_list_remove(t_current->objects, object);

#if 1 
    found = g_list_find(t_current->objects, object);
    if (found) {
      printf("found an object left over %s in %d, %d\nPlease e-mail ahvezda@geda.seul.org with this error message and .sch\n", object->name, i, j);
    }
#endif
    
    tloc_list = tloc_list->next;	
  }

  g_list_free(tloc_list);
  object->tile_locs = NULL;
}

void s_tile_update_object(TOPLEVEL * w_current, OBJECT * object)
{
    s_tile_remove_object_all(w_current, w_current->page_current, object);
    s_tile_add_object(w_current, object,
		      object->line->x[0], object->line->y[0],
		      object->line->x[1], object->line->y[1]);
}

void s_tile_print(TOPLEVEL * w_current)
{
  TILE *t_current;
  GList *temp;
  GList *temp2;
  OBJECT *o_current;
  TILE_LOC *tloc;
  int i, j;

  for (j = 0; j < MAX_TILES_Y; j++) {
    for (i = 0; i < MAX_TILES_X; i++) {
      printf("\nTile %d %d\n", i, j);

      t_current = &w_current->page_current->world_tiles[i][j];

      temp = t_current->objects;
      while (temp) {
        o_current = (OBJECT *) temp->data;

        printf("%s\n", o_current->name);
        temp2 = o_current->tile_locs;
        while (temp2 != NULL) {
          tloc = (TILE_LOC *) temp2->data;
          printf("	%d %d\n", tloc->i, tloc->j);
          temp2 = temp2->next;
        }

        temp = temp->next;
      }

      printf("------------------\n");
    }
  }

}

void s_tile_free_all(PAGE * p_current)
{
  int i, j;
  TILE *t_current;
  
  for (j = 0; j < MAX_TILES_Y; j++) {
    for (i = 0; i < MAX_TILES_X; i++) {
      t_current = &p_current->world_tiles[i][j];
      if (g_list_length(t_current->objects) != 0) {
        fprintf(stderr,
                "OOPS! t_current->objects had something in it when it was freed!\n");
        fprintf(stderr, "Length: %d\n", g_list_length(t_current->objects));


#if DEBUG          
        printf("%s %d %d\n", p_current->page_filename, i, j);
        objects = t_current->objects;
        while(objects != NULL) {
          real_object = (OBJECT *) objects->data;
          printf("yes %s\n", real_object->name);
          objects = objects->next;
        }
                        
        if (t_current->objects == NULL) {
          printf("yes null\n");
        }
#endif
        
      }
      g_list_free(t_current->objects);
    }
  }
}




