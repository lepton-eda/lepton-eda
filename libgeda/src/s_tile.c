/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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
#include <ctype.h>
#include <math.h>

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \file s_tile.c
 *  \brief Splits a page into tiles
 *
 *  With the <b>tiles</b> a page (st_page) is splitted into several smaller areas.
 *  The number of tiles is defined by <b>MAX_TILES_X</b> and <b>MAX_TILES_Y</b>.
 *  
 *  Each <b>TILE</b> (st_tile) can contain zero to many <b>OBJECTS</b> 
 *  (st_object) and each OBJECT can be in one or more TILES.
 * 
 *  The usage of tiles makes it easier to find geometrical connections between
 *  the line objects (OBJ_NET, OBJ_PIN, OBJ_BUS).
 *  
 *  \image html s_tile_overview.png
 *  \image latex s_tile_overview.pdf "Tile overview" width=14cm
 */

/*! \brief initialize the array of tiles
 *  \par Function Description
 *  This function splits the page size into tiles and initializes
 *  every tile.
 *  \param toplevel TOPLEVEL structure
 *  \param p_current The page that gets the tiles.
 */
void s_tile_init(TOPLEVEL * toplevel, PAGE * p_current)
{
  int i, j;
  TILE *t_current;
  int x_size = toplevel->init_right / MAX_TILES_X;
  int y_size = toplevel->init_bottom / MAX_TILES_Y;
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

/*! \brief add a line object to the tiles
 *  \par Function Description
 *  This function takes a single line object and adds it to
 *  every tile that is touched by the line. 
 *  It also adds all tiles that are touched by the object to 
 *  the objects tile list.
 *  \param toplevel The TOPLEVEL structure
 *  \param object The line OBJECT to add
 */
static void s_tile_add_line_object (TOPLEVEL *toplevel, OBJECT *object)
{
  TILE *t_current;
  PAGE *p_current;
  GList *found;
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

  g_return_if_fail (object != NULL);
  g_return_if_fail (object->line != NULL);

  p_current = o_get_page (toplevel, object);

  if (p_current == NULL) {
    return;
  }
  
  x_size = (double) toplevel->init_right / (double) MAX_TILES_X;
  y_size = (double) toplevel->init_bottom / (double) MAX_TILES_Y;

  x1 = (int) (object->line->x[0] / x_size);
  x2 = (int) (object->line->x[1] / x_size);
  y1 = (int) (object->line->y[0] / y_size);
  y2 = (int) (object->line->y[1] / y_size);

  bottom = x2 - x1;

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
        /* g_assert(v < MAX_TILES_X && w < MAX_TILES_Y && v >= 0 && w >= 0); */
        t_current = &p_current->world_tiles[v][w];
        found = g_list_find(t_current->objects, object);

        if (!found) {
          /*printf("%d %d\n", v, w);*/
          t_current->objects = g_list_append(t_current->objects,
                                             object);
          object->tiles = g_list_append(object->tiles, t_current);
        }

        v = (int) x;
        w = (int) ceil(y);
        if (v < 0 || w < 0 || v > MAX_TILES_X-1 || w > MAX_TILES_Y-1) {
          return;
        }
        /*g_assert(v < MAX_TILES_X && w < MAX_TILES_Y && v >= 0 && w >= 0);*/
        t_current = &p_current->world_tiles[v][w];
        found = g_list_find(t_current->objects, object);

        if (!found) {
          /*printf("%d %d\n", v, w);*/
          t_current->objects = g_list_append(t_current->objects,
                                             object);
          object->tiles = g_list_append(object->tiles, t_current);
        }

      } else {
        v = (int) x;
        w = (int) floor(y);
        if (v < 0 || w < 0 || v > MAX_TILES_X-1 || w > MAX_TILES_Y-1) {
          return;
        }
        /*g_assert(v < MAX_TILES_X && w < MAX_TILES_Y && v >= 0 && w >= 0);*/
        t_current = &p_current->world_tiles[v][w];
        found = g_list_find(t_current->objects, object);

        if (!found) {
          /*printf("%d %d\n", v, w);*/
          t_current->objects = g_list_append(t_current->objects,
                                             object);
          object->tiles = g_list_append(object->tiles, t_current);
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
          /*g_assert(v < MAX_TILES_X && w < MAX_TILES_Y &&
            v >= 0 && w >= 0);*/
          t_current = &p_current->world_tiles[v][w];
          found = g_list_find(t_current->objects, object);

          if (!found) {
            /*printf("%d %d\n", v, w);*/
            t_current->objects =
              g_list_append(t_current->objects, object);
            object->tiles = g_list_append(object->tiles, t_current);
          }

          w = (int) y;
          v = (int) ceil(x);
          if (v < 0 || w < 0 || v > MAX_TILES_X-1 || w > MAX_TILES_Y-1) {
            return;
          }
          /* g_assert(v < MAX_TILES_X && w < MAX_TILES_Y &&
             v >= 0 && w >= 0);*/
          t_current = &p_current->world_tiles[v][w];
          found = g_list_find(t_current->objects, object);

          if (!found) {
            /*printf("%d %d\n", v, w);*/
            t_current->objects =
              g_list_append(t_current->objects, object);
            object->tiles = g_list_append(object->tiles, t_current);
          }

        } else {
          w = (int) y;
          v = (int) floor(x);
          if (v < 0 || w < 0 || v > MAX_TILES_X-1 || w > MAX_TILES_Y-1) {
            return;
          }
          /*g_assert(v < MAX_TILES_X && w < MAX_TILES_Y &&
            v >= 0 && w >= 0);*/
          t_current = &p_current->world_tiles[v][w];
          found = g_list_find(t_current->objects, object);

          if (!found) {
            /*printf("%d %d\n", v, w);*/
            t_current->objects =
              g_list_append(t_current->objects, object);
            object->tiles = g_list_append(object->tiles, t_current);
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
      /*g_assert(v < MAX_TILES_X && w < MAX_TILES_Y &&
        v >= 0 && w >= 0);*/
      t_current = &p_current->world_tiles[v][w];
      found = g_list_find(t_current->objects, object);

      if (!found) {
        /*printf("%d %d\n", v, w);*/
        t_current->objects = g_list_append(t_current->objects,
                                           object);
        object->tiles = g_list_append(object->tiles, t_current);
      }

    }
  }
}

/*! \brief add an object to the tile ssytem
 *  \par Function Description
 *  This function takes dispatches the object to the correct
 *  function, depending on its type.
 *
 *  \param toplevel The TOPLEVEL structure
 *  \param object The line OBJECT to add
 */
void s_tile_add_object (TOPLEVEL *toplevel, OBJECT *object)
{
  GList *iter;

  switch (object->type) {
    case OBJ_NET:
    case OBJ_PIN:
    case OBJ_BUS:
      s_tile_add_line_object (toplevel, object);
      break;

  case OBJ_COMPLEX:
  case OBJ_PLACEHOLDER:
    for (iter = object->complex->prim_objs;
         iter != NULL;
         iter = g_list_next (iter)) {
      s_tile_add_object (toplevel, iter->data);
    }
  }
}

/*! \brief remove an object from the tiles
 *  \par Function Description
 *  This function remose an object from all tiles that are refered by the object.
 *  It also removes the object from each tile that contained the object.
 *  \param object The object to remove
 */
void s_tile_remove_object(OBJECT *object)
{
  GList *iter;
  GList *tl_current;

  /* Correctly deal with compound objects */
  if (object->type == OBJ_COMPLEX || object->type == OBJ_PLACEHOLDER) {
    for (iter = object->complex->prim_objs;
         iter != NULL;
         iter = g_list_next (iter)) {
      s_tile_remove_object (iter->data);
    }
  }

  for (tl_current = object->tiles;
       tl_current != NULL;
       tl_current = g_list_next (tl_current)) {
    TILE *t_current = (TILE*)tl_current->data;
    
    /* remove object from the list of objects for this tile */
    t_current->objects = g_list_remove(t_current->objects, object);
  }

  /* reset the list of tiles for this object appears in */
  g_list_free(object->tiles);
  object->tiles = NULL;
}

/*! \brief update the tile informations of an object
 *  \par Function Description
 *  This function updates the tile informations of an object.
 *  This function can be used if an object has been moved on the page
 *  \param toplevel The TOPLEVEL structure
 *  \param object The OBJECT to update
 */
void s_tile_update_object(TOPLEVEL * toplevel, OBJECT * object)
{
  s_tile_remove_object (object);
  s_tile_add_object (toplevel, object);
}


/*! \brief get a list of object lists of all tiles inside a region
 *  \par Function Description
 *  This functions collects all object lists of the tiles that are touched
 *  by the given rectangle (x1,y1), (x2,y2).
 *  \note The caller has to g_list_free() the returned list.
 */
GList* s_tile_get_objectlists(TOPLEVEL *toplevel, PAGE *p_current,
                              int world_x1, int world_y1,
                              int world_x2, int world_y2)
{
  TILE *t_current;
  int x1, x2, y1, y2, x, y;
  double x_size, y_size;
  GList *objectlists = NULL;

  x_size = (double) toplevel->init_right / (double) MAX_TILES_X;
  y_size = (double) toplevel->init_bottom / (double) MAX_TILES_Y;

  x1 = (int) (world_x1 / x_size);
  x2 = (int) (world_x2 / x_size);
  y1 = (int) (world_y1 / y_size);
  y2 = (int) (world_y2 / y_size);

  /* limit all tile ranges to [0, MAX_TILES-1]
     (paranoid error checking) */
  x1= max(x1, 0);  x1 = min(x1, MAX_TILES_X-1);
  x2= max(x2, 0);  x2 = min(x2, MAX_TILES_X-1);
  y1= max(y1, 0);  y1 = min(y1, MAX_TILES_Y-1);
  y2= max(y2, 0);  y2 = min(y2, MAX_TILES_Y-1);

  /* Check and correct the order of the coordinates */
  if (x1 > x2) {
    x = x1;  x1 = x2;  x2 = x;
  }
  if (y1 > y2) {
    y = y1;  y1 = y2;  y2 = y;
  }

  /* finally, collect all object lists from the tiles */
  for (x = x1; x <= x2; x++) {
    for (y = y1; y <= y2; y++) {
      t_current = &p_current->world_tiles[x][y];
      objectlists = g_list_append(objectlists, t_current->objects);
    }
  }

  return objectlists;
}


/*! \brief print all objects for each tile
 *  \par Function Description
 *  Debugging function to print all object names that are inside
 *  the tiles.
 */
void s_tile_print(TOPLEVEL * toplevel, PAGE *page)
{
  TILE *t_current;
  GList *temp;
  OBJECT *o_current;
  int i, j;

  for (j = 0; j < MAX_TILES_Y; j++) {
    for (i = 0; i < MAX_TILES_X; i++) {
      printf("\nTile %d %d\n", i, j);

      t_current = &page->world_tiles[i][j];

      temp = t_current->objects;
      while (temp) {
        o_current = (OBJECT *) temp->data;

        printf("%s\n", o_current->name);

        temp = g_list_next(temp);
      }

      printf("------------------\n");
    }
  }

}

/*! \brief free all object links from the tiles
 *  \par Function Description
 *  This function removes all objects from the tiles of the given \a page.
 *
 *  \param [in] p_current The PAGE to clean up the tiles
 *  \note In theory, calling this function is not required. If all objects
 *  have been removed from a page, all object lists of the tiles should be empty.
 */
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
      }
      g_list_free(t_current->objects);
    }
  }
}



