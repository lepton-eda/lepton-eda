/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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
#include <ctype.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifndef HAVE_VSNPRINTF
#include <stdarg.h>
#endif

#include <gtk/gtk.h>
#include <libguile.h>

#include "defines.h"
#include "struct.h"
#include "defines.h"
#include "globals.h"
#include "colors.h"
#include "o_types.h"

#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
CONN *s_conn_return_new(OBJECT * other_object, int type, int x, int y,
			int whichone, int other_whichone)
{
  CONN *new_conn;

  new_conn = (CONN *) g_malloc(sizeof(CONN));

#if DEBUG
  printf("** creating: %s %d %d\n", other_object->name, x, y);
#endif

  new_conn->other_object = other_object;
  new_conn->type = type;
  new_conn->x = x;
  new_conn->y = y;
  new_conn->whichone = whichone;
  new_conn->other_whichone = other_whichone;

  return (new_conn);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \return TRUE if the CONN structure is unique, FALSE otherwise.
 */
int s_conn_uniq(GList * conn_list, CONN * input_conn)
{
  GList *c_current;
  CONN *conn;

  c_current = conn_list;
  while (c_current != NULL) {
    conn = (CONN *) c_current->data;

    if (conn->other_object == input_conn->other_object &&
        conn->x == input_conn->x && conn->y == input_conn->y &&
        conn->type == input_conn->type) {
      return (FALSE);
    }

    c_current = g_list_next(c_current);
  }

  return (TRUE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int s_conn_remove_other(TOPLEVEL * toplevel, OBJECT * other_object,
			OBJECT * to_remove)
{
    GList *c_current = NULL;
    CONN *conn = NULL;

    c_current = other_object->conn_list;
    while (c_current != NULL) {
	conn = (CONN *) c_current->data;

	if (conn->other_object == to_remove) {
	    other_object->conn_list =
		g_list_remove(other_object->conn_list, conn);

#if DEBUG
	    printf("Found other_object in remove_other\n");
	    printf("Freeing other: %s %d %d\n", conn->other_object->name,
		   conn->x, conn->y);
#endif

	    /* Do not write modify c_current like this, since this will cause */
	    /* very nasty data corruption and upset glib's memory slice */
	    /* allocator. */
	    /* c_current->data = NULL;   Do not comment in */

	    g_free(conn);

#if 0 /* this does not work right */
            if (other_object->type == OBJ_BUS &&
                other_object->conn_list == NULL) {
              other_object->bus_ripper_direction = 0;
            }
#endif
            
	    return (TRUE);
	}

	c_current = g_list_next(c_current);
    }

    return (FALSE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_conn_remove(TOPLEVEL * toplevel, OBJECT * to_remove)
{
  GList *c_current;
  CONN *conn;

  if (to_remove->type != OBJ_PIN && to_remove->type != OBJ_NET &&
      to_remove->type != OBJ_BUS) {
    return;
  }

  c_current = to_remove->conn_list;
  while (c_current != NULL) {
    conn = (CONN *) c_current->data;

#if DEBUG
    printf("Removing: %s\n", conn->other_object->name);
#endif

    /* keep calling this till it returns false (all refs removed) */
    /* there is NO body to this while loop */
    while (s_conn_remove_other
           (toplevel, conn->other_object, to_remove));

#if DEBUG
    printf("returned from remove_other\n");
    printf("Freeing: %s %d %d\n", conn->other_object->name, conn->x,
           conn->y);
#endif
    c_current->data = NULL;
    g_free(conn);
    c_current = g_list_next(c_current);
  }

#if DEBUG
  printf("length: %d\n", g_list_length(to_remove->conn_list));
#endif

  g_list_free(to_remove->conn_list);
  to_remove->conn_list = NULL; /*! \todo Memory leak? TODO hack */

#if 0 /* this does not work right either */
  if (to_remove->type == OBJ_BUS)
  {
    to_remove->bus_ripper_direction = 0;
  }
#endif
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_conn_remove_complex(TOPLEVEL * toplevel, OBJECT * to_remove)
{
  OBJECT *o_current;
  
  if (to_remove->type != OBJ_COMPLEX && to_remove->type != OBJ_PLACEHOLDER) {
    return;
  }

  o_current = to_remove->complex->prim_objs;
  while (o_current != NULL) {
    switch (o_current->type) {
      case (OBJ_NET):
      case (OBJ_PIN):
      case (OBJ_BUS):
        s_conn_remove(toplevel, o_current);
        break;

    }
    o_current = o_current->next;
  }
  
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
OBJECT *s_conn_check_midpoint(OBJECT *o_current, int x, int y)
{
  int min_x, min_y, max_x, max_y;

  switch(o_current->type) {
    case(OBJ_NET):
    case(OBJ_PIN):
    case(OBJ_BUS):
      min_y = min(o_current->line->y[0], 
                  o_current->line->y[1]);
      max_y = max(o_current->line->y[0], 
                  o_current->line->y[1]);

				/* vertical */
      if ( (o_current->line->x[0] == x) &&
           (y > min_y) && (y < max_y) &&
           (o_current->line->x[0] ==
            o_current->line->x[1]) ) {
#if DEBUG
        printf("Found vertical point\n");
#endif
        return(o_current);
      }

      min_x = min(o_current->line->x[0], 
                  o_current->line->x[1]);
      max_x = max(o_current->line->x[0], 
                  o_current->line->x[1]);

				/* horizontal */
      if ( (o_current->line->y[0] == y) &&
           (x > min_x) && (x < max_x) &&
           (o_current->line->y[0] ==
            o_current->line->y[1]) ) {
#if DEBUG
        printf("Found horizontal point\n");
#endif
        return(o_current);
      }

      break;
  }
  return(NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_conn_update_object(TOPLEVEL * toplevel, OBJECT * object)
{
  TILE *t_current;
  TILE_LOC *tl_current;
  GList *tloc_list;
  GList *object_list;
  OBJECT *other_object;
  OBJECT *found;
  CONN *new_conn;
  int i, j;
  int k;
  int add_conn;

  /* loop over all tiles which object appears in */
  tloc_list = object->tile_locs;
  while (tloc_list != NULL) {
    tl_current = (TILE_LOC *) tloc_list->data;

    add_conn = FALSE;
    
    i = tl_current->i;
    j = tl_current->j;

#if DEBUG
    printf("\nInside tile: %d %d\n\n", i, j);
#endif
    
    t_current = &toplevel->page_current->world_tiles[i][j];
    object_list = t_current->objects;
    while (object_list != NULL) {
      other_object = (OBJECT *) object_list->data;
      
#if DEBUG
      printf("Tile has object: %s\n", other_object->name);
#endif

      /* here is where you check the end points */
      for (k = 0; k < 2; k++) {
        /* do first end point */

        if (object->line->x[0] == other_object->line->x[k] &&
            object->line->y[0] == other_object->line->y[k] &&
            object != other_object) {

          if (object->type == OBJ_PIN) {
            if (0 == object->whichend) {
              if (other_object->type == OBJ_PIN && /* new addition */
                  k != other_object->whichend ) { /* was 0 */
                add_conn = FALSE; 
              } else {
                add_conn = TRUE;
              }
            } else {
              add_conn = FALSE;
            }
          } else if (other_object->type == OBJ_PIN) {
            if (k == other_object->whichend) {
              if (object->type == OBJ_PIN && /* new addition */
                  k != object->whichend ) {
                add_conn = FALSE; 
              } else {
                add_conn = TRUE;
              }
            } else {
              add_conn = FALSE;
            }
          } else {
            add_conn = TRUE;
          }

          /* check for pin / bus compatibility */
          /* you cannot connect pins and buses at all */
          if (add_conn) {
            if ((object->type == OBJ_PIN && other_object->type == OBJ_BUS) ||
                (object->type == OBJ_BUS && other_object->type == OBJ_PIN)) {
              add_conn = FALSE;
            } else {
              add_conn = TRUE;
            }
          }

          /* check for net / bus compatibility */
          /* you cannot connect the endpoint of a bus to a net */
          /* and the reverse is true as well */
          if (add_conn) {
            if ((object->type == OBJ_NET && other_object->type == OBJ_BUS) ||
                (object->type == OBJ_BUS && other_object->type == OBJ_NET)) {
              add_conn = FALSE;
            } else {
              add_conn = TRUE;
            }
          }
          
#if DEBUG
          if (add_conn) {
            printf("0\n");
            printf("k: %d\n", k);
            printf("object: %d\n", object->whichend);
            printf("other: %d\n\n", other_object->whichend);
          }
#endif

          if (add_conn) {
            new_conn =
              s_conn_return_new(other_object, CONN_ENDPOINT,
                                other_object->line->x[k],
                                other_object->line->y[k], 0, k);

            /* do uniqness check */
            if (s_conn_uniq(object->conn_list, new_conn)) {
              object->conn_list =
                g_list_append(object->conn_list, new_conn);
            } else {
              g_free(new_conn);
            }

            new_conn = s_conn_return_new(object, CONN_ENDPOINT,
                                         object->line->x[0],
                                         object->line->y[0], k, 0);

            /* do uniqness check */
            if (s_conn_uniq(other_object->conn_list, new_conn)) {
              other_object->conn_list =
                g_list_append(other_object->conn_list,
                              new_conn);
            } else {
              g_free(new_conn);
            }

#if DEBUG
            printf("first end point -> %d\n", k);
#endif
          }
        }

        /* do second end point */
        if (object->line->x[1] == other_object->line->x[k] &&
            object->line->y[1] == other_object->line->y[k] &&
            object != other_object) {
          
          if (object->type == OBJ_PIN) {
            if (1 == object->whichend) {
              if (other_object->type == OBJ_PIN && /* new addition */
                  k != other_object->whichend ) { /* was 1 */
                add_conn = FALSE; 
              } else {
                add_conn = TRUE;
              }
            } else {
              add_conn = FALSE;
            }
          } else if (other_object->type == OBJ_PIN) {
            if (k == other_object->whichend) {
              if (object->type == OBJ_PIN && /* new addition */
                  k != object->whichend ) {
                add_conn = FALSE; 
              } else {
                add_conn = TRUE;
              }       
            } else {
              add_conn = FALSE;
            }
          } else {
            add_conn = TRUE;
          }

          /* check for pin / bus compatibility */
          /* you cannot connect pins and buses at all */
          if (add_conn) {
            if ((object->type == OBJ_PIN && other_object->type == OBJ_BUS) ||
                (object->type == OBJ_BUS && other_object->type == OBJ_PIN)) {
              add_conn = FALSE;
            } else {
              add_conn = TRUE;
            }
          }

          /* check for net / bus compatibility */
          /* you cannot connect the endpoint of a bus to a net */
          /* and the reverse is true as well */
          if (add_conn) {
            if ((object->type == OBJ_NET && other_object->type == OBJ_BUS) ||
                (object->type == OBJ_BUS && other_object->type == OBJ_NET)) {
              add_conn = FALSE;
            } else {
              add_conn = TRUE;
            }
          }

#if DEBUG
          if (add_conn) {
            printf("1\n");
            printf("k: %d\n", k);
            printf("object: %d\n", object->whichend);
            printf("other: %d\n\n", other_object->whichend);
          }
#endif
          
          if (add_conn) {
            new_conn =
              s_conn_return_new(other_object, CONN_ENDPOINT,
                                other_object->line->x[k],
                                other_object->line->y[k], 1, k);

            /* do uniqness check */
            if (s_conn_uniq(object->conn_list, new_conn)) {
              object->conn_list =
                g_list_append(object->conn_list, new_conn);
            } else {
              g_free(new_conn);
            }

            new_conn = s_conn_return_new(object, CONN_ENDPOINT,
                                         object->line->x[1],
                                         object->line->y[1], k, 1);

            /* do uniqness check */
            if (s_conn_uniq(other_object->conn_list, new_conn)) {
              other_object->conn_list =
                g_list_append(other_object->conn_list,
                              new_conn);
            } else {
              g_free(new_conn);
            }

#if DEBUG
            printf("second end point -> %d\n", k);
#endif
          }
        }
        

        /* check for midpoint of other object, k endpoint of current obj*/
        found = s_conn_check_midpoint(other_object,
                                      object->line->x[k],
                                      object->line->y[k]);

        /* pins are not allowed midpoint connections (on them) */
        /* pins can cause midpoint connections (via their endpoints) */
        if (found && other_object->type != OBJ_PIN) { /* found midpoint */
          /* have to add it to the current object and other object */
#if DEBUG         
          printf("%d endpoint to %s midpoint of: %s\n", k, object->name,
                 found->name);
#endif          

          if (object->type == OBJ_PIN) {
            if (k == object->whichend) {
              add_conn = TRUE;
            } else {
              add_conn = FALSE;
            }
          } else {
            add_conn = TRUE;
          }

          /* check for pin / bus compatibility */
          /* you cannot connect pins and buses at all */
          if (add_conn) {
            if ((object->type == OBJ_PIN && other_object->type == OBJ_BUS) ||
                (object->type == OBJ_BUS && other_object->type == OBJ_PIN)) {
              add_conn = FALSE;
            } else {
              add_conn = TRUE;
            }
          }

          /* check for bus / net compatibility */
          /* you cannot have the middle of bus connect to nets */
          if (add_conn) {
            if (object->type == OBJ_BUS && other_object->type == OBJ_NET) {
              add_conn = FALSE;
            } else {
              add_conn = TRUE;
            }
          }

          if (add_conn) {
            /* First do the other object and put it in the current */
            /* object list */
            new_conn =
              s_conn_return_new(other_object, CONN_MIDPOINT,
                                object->line->x[k],
                                object->line->y[k], k, -1);

            /* do uniqness check */
            if (s_conn_uniq(object->conn_list, new_conn)) {
              object->conn_list =
                g_list_append(object->conn_list, new_conn);
            } else {
              g_free(new_conn);
            }

            /* Next do the current object and put it into the other */
            /* object list */
            new_conn = s_conn_return_new(object, CONN_MIDPOINT,
                                         object->line->x[k],
                                         object->line->y[k], -1, k);

            /* do uniqness check */
            if (s_conn_uniq(other_object->conn_list, new_conn)) {
              other_object->conn_list =
                g_list_append(other_object->conn_list,
                              new_conn);
            } else {
              g_free(new_conn);
            }

          }
        }
        
        /****/

        /* do object's endpoints cross the middle of other_object? */
        /* check for midpoint of other object, k endpoint of current obj*/
        found = s_conn_check_midpoint(object,
                                      other_object->line->x[k],
                                      other_object->line->y[k]);

        /* pins are not allowed midpoint connections (on them) */
        /* pins can cause midpoint connections (via their endpoints) */
        if (found && object->type != OBJ_PIN) { /* found midpoint */
          /* have to add it to the current object and other object */
#if DEBUG          
          printf("SECOND! %d endpoint to %s midpoint of: %s\n", k,
                 other_object->name,
                 found->name);
#endif          

          if (other_object->type == OBJ_PIN) {
            if (k == other_object->whichend) {
              add_conn = TRUE;
            } else {
              add_conn = FALSE;
            }
          } else {
            add_conn = TRUE;
          }

          
          /* check for pin / bus compatibility */
          /* you cannot connect pins and buses at all */
          if (add_conn) {
            if ((object->type == OBJ_PIN && other_object->type == OBJ_BUS) ||
                (object->type == OBJ_BUS && other_object->type == OBJ_PIN)) {
              add_conn = FALSE;
            } else {
              add_conn = TRUE;
            }
          }

          /* check for bus / net compatibility */
          /* you cannot have the middle of bus connect to nets */
          if (add_conn) {
            if (object->type == OBJ_NET && other_object->type == OBJ_BUS) {
              add_conn = FALSE;
            } else {
              add_conn = TRUE;
            }
          }

          
          if (add_conn) {
            /* First do the other object and put it in the current */
            /* object list */
            new_conn =
              s_conn_return_new(other_object, CONN_MIDPOINT,
                                other_object->line->x[k],
                                other_object->line->y[k], -1, k);

            /* do uniqness check */
            if (s_conn_uniq(object->conn_list, new_conn)) {
              object->conn_list =
                g_list_append(object->conn_list, new_conn);
            } else {
              g_free(new_conn);
            }

            /* Next do the current object and put it into the other */
            /* object list */
            new_conn = s_conn_return_new(object, CONN_MIDPOINT,
                                         other_object->line->x[k],
                                         other_object->line->y[k], k, -1);

            /* do uniqness check */
            if (s_conn_uniq(other_object->conn_list, new_conn)) {
              other_object->conn_list =
                g_list_append(other_object->conn_list,
                              new_conn);
            } else {
              g_free(new_conn);
            }

          }
          /* **** */
          
        } /* found midpoint */

      } /* end of for over endpoints */

      object_list = g_list_next(object_list);
    } 

    tloc_list = g_list_next(tloc_list);
  }

#if DEBUG
  s_conn_print(object->conn_list);
#endif
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_conn_update_complex(TOPLEVEL * toplevel, OBJECT * complex)
{
  OBJECT *o_current;

  o_current = complex;
  while (o_current != NULL) {
    switch (o_current->type) {
      case (OBJ_PIN):
      case (OBJ_NET):
      case (OBJ_BUS):
        s_conn_update_object(toplevel, o_current);
        break;

    }
    o_current = o_current->next;
  }

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_conn_print(GList * conn_list)
{
  CONN *conn;
  GList *cl_current;

  printf("\nStarting s_conn_print\n");
  cl_current = conn_list;
  while (cl_current != NULL) {

    conn = (CONN *) cl_current->data;
    printf("-----------------------------------\n");
    printf("other object: %s\n", conn->other_object->name);
    printf("type: %d\n", conn->type);
    printf("x: %d y: %d\n", conn->x, conn->y);
    printf("whichone: %d\n", conn->whichone);
    printf("other_whichone: %d\n", conn->other_whichone);
    printf("-----------------------------------\n");

    cl_current = g_list_next(cl_current);
  }

}

/*! \brief Search for net in existing connections.
 *  \par Function Description
 *  This method searches the connection list for the first matching
 *  connection with the given x, y, and whichone endpoint.
 *
 *  \param [in] new_net    Net OBJECT to compare to.
 *  \param [in] whichone   The connection number to check.
 *  \param [in] conn_list  List of existing connections to compare
 *                         <B>new_net</B> to.
 *  \return TRUE if a matching connection is found, FALSE otherwise. 
 */
int s_conn_net_search(OBJECT* new_net, int whichone, GList * conn_list)
{
  CONN *conn;
  GList *cl_current;

  cl_current = conn_list;
  while (cl_current != NULL) {

    conn = (CONN *) cl_current->data;
    if (conn != NULL && conn->whichone == whichone && 
        conn->x == new_net->line->x[whichone] &&
	conn->y == new_net->line->y[whichone])
    {
       return TRUE;
    }

    cl_current = g_list_next(cl_current);
  }
 
  return FALSE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \warning
 *  Caller must g_list_free returned GList pointer.
 *  Do not free individual data items in list.
 */
GList *s_conn_return_others(GList *input_list, OBJECT *object)
{
  CONN *conn;
  GList *cl_current;
  GList *return_list=NULL;

  return_list = input_list;
  
  cl_current = object->conn_list;
  while (cl_current != NULL) {

    conn = (CONN *) cl_current->data;
    
    if (conn->other_object && conn->other_object != object) {
      return_list = g_list_append(return_list, conn->other_object);
    }
        
    cl_current = g_list_next(cl_current);
  }

  return(return_list);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \warning
 *  Caller must g_list_free returned GList pointer.
 *  Do not free individual data items in list.
 */
GList *s_conn_return_complex_others(GList *input_list, OBJECT *object)
{
  OBJECT *o_current;
  CONN *conn;
  GList *cl_current;
  GList *return_list=NULL;

  if (object->type != OBJ_COMPLEX && object->type != OBJ_PLACEHOLDER) {
    return(NULL);
  }

  return_list = input_list;
  
  o_current = object->complex->prim_objs;
  while(o_current != NULL) {
    cl_current = o_current->conn_list;
    while (cl_current != NULL) {

      conn = (CONN *) cl_current->data;
    
      if (conn->other_object && conn->other_object != o_current) {
        return_list = g_list_append(return_list, conn->other_object);
      }
        
      cl_current = g_list_next(cl_current);
    }

    o_current = o_current->next;
  }
  
  return(return_list);
}
