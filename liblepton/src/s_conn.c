/* Lepton EDA library
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
#include <ctype.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "liblepton_priv.h"

/*! \file s_conn.c
 *  \brief The connection system
 *
 *  The connection system stores and tracks the connections between
 *  connected <b>OBJECTS</b>. The connected OBJECTS are either
 *  <b>pins</b>, <b>nets</b> and <b>busses</b>.
 *
 *  Each connection object with the type <b>st_conn</b> represents a
 *  single unidirectional relation to another object.
 *
 *  The following figure with two nets and a pin shows the relations
 *  between connections and OBJECTS:
 *
 *  \image html s_conn_overview.png
 *  \image latex s_conn_overview.pdf "Connection overview" width=14cm
 */


/*! \brief create a new connection object
 *  \par Function Description
 *  create a single st_conn object and initialize it with the
 *  given parameters.
 *
 *  \return The new connection object
 */
LeptonConn*
s_conn_return_new (LeptonObject *other_object,
                   int type,
                   int x,
                   int y,
                   int whichone,
                   int other_whichone)
{
  LeptonConn *new_conn;

  new_conn = (LeptonConn *) g_malloc(sizeof(LeptonConn));

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

/*! \brief check if a connection is uniq in a list
 *  \par Function Description
 *  This function checks if there's no identical connection
 *  in the list of connections.
 *  \param conn_list list of connection objects
 *  \param input_conn single connection object.
 *  \return TRUE if the LeptonConn structure is unique, FALSE otherwise.
 */
int
s_conn_uniq (GList * conn_list,
             LeptonConn * input_conn)
{
  GList *c_current;
  LeptonConn *conn;

  c_current = conn_list;
  while (c_current != NULL) {
    conn = (LeptonConn *) c_current->data;

    if (conn->other_object == input_conn->other_object &&
        conn->x == input_conn->x && conn->y == input_conn->y &&
        conn->type == input_conn->type) {
      return (FALSE);
    }

    c_current = g_list_next(c_current);
  }

  return (TRUE);
}

/*! \brief remove a object from the connection list of another object
 *  \par Function Description
 *  This function removes the LeptonObject <b>to_remove</b> from the connection
 *  list of the LeptonObject <b>other_object</b>.
 *
 *  \param other_object LeptonObject from that the to_remove LeptonObject needs to be removed
 *  \param to_remove LeptonObject to remove
 *  \return TRUE if a connection has been deleted, FALSE otherwise
 */
int
s_conn_remove_other (LeptonObject *other_object,
                     LeptonObject *to_remove)
{
  GList *c_current = NULL;
  LeptonConn *conn = NULL;

  lepton_object_emit_pre_change_notify (other_object);

  c_current = other_object->conn_list;
  while (c_current != NULL) {
    conn = (LeptonConn *) c_current->data;

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
      if (lepton_object_is_bus (other_object) &&
          other_object->conn_list == NULL) {
        other_object->bus_ripper_direction = 0;
      }
#endif

      return (TRUE);
    }

    c_current = g_list_next(c_current);
  }

  lepton_object_emit_change_notify (other_object);

  return (FALSE);
}

/*! \brief remove an LeptonObject from the connection system
 *  \par Function Description
 *  This function removes all connections from and to the LeptonObject
 *  <b>to_remove</b>.
 *
 *  \param to_remove LeptonObject to unconnected from all other objects
 */
void
s_conn_remove_object_connections (LeptonObject *to_remove)
{
  GList *c_iter;
  LeptonConn *conn;
  GList *iter;
  LeptonObject *o_current;

  switch (lepton_object_get_type (to_remove)) {
    case OBJ_PIN:
    case OBJ_NET:
    case OBJ_BUS:
      for (c_iter = to_remove->conn_list;
           c_iter != NULL;
           c_iter = g_list_next (c_iter)) {
        conn = (LeptonConn *) c_iter->data;

        /* keep calling this till it returns false (all refs removed) */
        /* there is NO body to this while loop */
        while (s_conn_remove_other (conn->other_object, to_remove));

        c_iter->data = NULL;
        g_free (conn);
      }

      g_list_free (to_remove->conn_list);
      to_remove->conn_list = NULL;
      break;

    case OBJ_COMPONENT:
      for (iter = lepton_component_object_get_contents (to_remove);
           iter != NULL;
           iter = g_list_next (iter))
      {
        o_current = (LeptonObject*) iter->data;
        s_conn_remove_object_connections (o_current);
      }
      break;
  }
}

/*! \brief Checks if a point is a midpoint of an LeptonObject
 *  \par Function Description
 *  Checks if the point (<b>x</b>,<b>y</b>) is on the LeptonObject
 *  and between it's endpoints.
 *  \return TRUE if the point is a midpoint of the LeptonObject. FALSE
 *  if the point is not a midpoinit or if the LeptonObject is not a
 *  NET a PIN or a BUS or if the LeptonObject
 *  has neither horizontal nor vertical orientation.
 */
LeptonObject *s_conn_check_midpoint(LeptonObject *o_current, int x, int y)
{
  int min_x, min_y, max_x, max_y;

  switch (lepton_object_get_type (o_current)) {
  case(OBJ_NET):
  case(OBJ_PIN):
  case(OBJ_BUS):
    min_y = MIN(o_current->line->y[0],
                o_current->line->y[1]);
    max_y = MAX(o_current->line->y[0],
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

    min_x = MIN(o_current->line->x[0],
                o_current->line->x[1]);
    max_x = MAX(o_current->line->x[0],
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

/*! \brief adds a GList of LeptonObjects to the connection system
 *
 *  \par Function Description
 *  This function adds all connections from and to the OBJECTS
 *  of the given GList.
 *
 *  \param page      The LeptonPage structure
 *  \param obj_list  GList of LeptonObjects to add into the connection system
 */
void s_conn_update_glist (LeptonPage* page,
                          GList *obj_list)
{
  LeptonObject *o_current;
  GList *iter;

  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    o_current = (LeptonObject*) iter->data;
    s_conn_update_object (page, o_current);
  }
}


/*! \brief Checks if an object is bus, or a bus pin
 *
 *  \par Function Description
 *  Checks if an object is a bus or a bus pin
 *
 *  \param object  The LeptonObject to test
 *  \return TRUE if the objects is a bis, or bus pin
 */
static int is_bus_related (LeptonObject *object)
{
  return (lepton_object_is_bus (object) ||
          (lepton_object_is_pin (object) && object->pin_type == PIN_TYPE_BUS));
}


/*! \brief Checks if two objects are of compatible types to be connected
 *
 *  \par Function Description
 *  Checks if two objects are legal to be connected together
 *
 *  \param object1  First LeptonObject
 *  \param object2  Second LeptonObject
 *  \return TRUE if the objects are compatible, FALSE if not
 */
static int check_direct_compat (LeptonObject *object1, LeptonObject *object2)
{
  return (is_bus_related (object1) == is_bus_related (object2));
}


static void add_connection (LeptonObject *object, LeptonObject *other_object,
                            int type, int x, int y,
                            int whichone, int other_whichone)
{
  /* Describe the connection */
  LeptonConn *new_conn =
    s_conn_return_new (other_object, type, x, y, whichone, other_whichone);
  /* Do uniqness check */
  if (s_conn_uniq (object->conn_list, new_conn)) {
    object->conn_list = g_list_append (object->conn_list, new_conn);
  } else {
    g_free (new_conn);
  }
}

/*! \brief add a line LeptonObject to the connection system
 *  \par Function Description
 *  This function searches for all geometrical connections of the
 *  LeptonObject <b>object</b> to all other connectable
 *  objects. It adds connections to the object and from all other
 *  objects to this one.
 *  \param page   The LeptonPage structure
 *  \param object LeptonObject to add into the connection system
 */
static void
s_conn_update_line_object (LeptonPage* page,
                           LeptonObject *object)
{
  GList *object_list;
  LeptonObject *other_object;
  LeptonObject *found;
  int j, k;
  LeptonObject *component, *other_component;

  component = lepton_object_get_parent (object);

  /* loop over all connectible objects */
  for (object_list = page->connectible_list;
       object_list != NULL;
       object_list = g_list_next (object_list)) {
    other_object = (LeptonObject*) object_list->data;

    if (object == other_object)
      continue;

    other_component = lepton_object_get_parent (other_object);

    /* An object inside a symbol can only be connected up to another
     * object if they are (a) both inside the same object, or (b)
     * the object inside a symbol is a pin. */

    /* 1. Both objects are inside a symbol */
    if (component && other_component) {
      /* If inside different symbols, both must be pins to connect. */
      if (component != other_component
          && (!lepton_object_is_pin (object) ||
              !lepton_object_is_pin (other_object))) {
        continue;
      }

    /* 2. Updating object is inside a symbol, but other object is not. */
    }
    else if (component && !other_component)
    {
      if (!lepton_object_is_pin (object)) continue;
    /* 3. Updating object not inside symbol, but other object is. */
    }
    else if (!component && other_component)
    {
      if (!lepton_object_is_pin (other_object)) continue;
    }

    /* Here is where you check the end points */
    /* Check both end points of the other object */
    for (k = 0; k < 2; k++) {

      /* If the other object is a pin, only check the correct end */
      if (lepton_object_is_pin (other_object) &&
          other_object->whichend != k)
        continue;

      /* Check both end points of the object */
      for (j = 0; j < 2; j++) {

        /* If the object is a pin, only check the correct end */
        if (lepton_object_is_pin (object) &&
            object->whichend != j)
          continue;

        /* Check for coincidence and compatibility between
           the objects being tested. */
        if (object->line->x[j] == other_object->line->x[k] &&
            object->line->y[j] == other_object->line->y[k] &&
            check_direct_compat (object, other_object)) {

          lepton_object_emit_pre_change_notify (other_object);

          add_connection (object, other_object, CONN_ENDPOINT,
                          other_object->line->x[k],
                          other_object->line->y[k], j, k);

          add_connection (other_object, object, CONN_ENDPOINT,
                          object->line->x[j],
                          object->line->y[j], k, j);

          lepton_object_emit_change_notify (other_object);
        }
      }
    }

    /* Check both end points of the object against midpoints of the other */
    for (k = 0; k < 2; k++) {

      /* If the object is a pin, only check the correct end */
      if (lepton_object_is_pin (object) &&
          object->whichend != k)
        continue;

      /* check for midpoint of other object, k endpoint of current obj*/
      found = s_conn_check_midpoint (other_object, object->line->x[k],
                                                   object->line->y[k]);

      /* Pins are not allowed midpoint connections onto them. */
      /* Allow nets to connect to the middle of buses. */
      /* Allow compatible objects to connect. */
      if (found && !lepton_object_is_pin (other_object) &&
          ((lepton_object_is_net (object) &&
            lepton_object_is_bus (other_object)) ||
           check_direct_compat (object, other_object)))
      {

        add_connection (object, other_object, CONN_MIDPOINT,
                        object->line->x[k],
                        object->line->y[k], k, -1);

        add_connection (other_object, object, CONN_MIDPOINT,
                        object->line->x[k],
                        object->line->y[k], -1, k);
      }
    }

    /* Check both end points of the other object against midpoints of the first */
    for (k = 0; k < 2; k++) {

      /* If the other object is a pin, only check the correct end */
      if (lepton_object_is_pin (other_object) &&
          other_object->whichend != k)
        continue;

      /* do object's endpoints cross the middle of other_object? */
      /* check for midpoint of other object, k endpoint of current obj*/
      found = s_conn_check_midpoint (object, other_object->line->x[k],
                                             other_object->line->y[k]);

      /* Pins are not allowed midpoint connections onto them. */
      /* Allow nets to connect to the middle of buses. */
      /* Allow compatible objects to connect. */
      if (found && !lepton_object_is_pin (object) &&
          ((lepton_object_is_bus (object) &&
            lepton_object_is_net (other_object)) ||
           check_direct_compat (object, other_object))) {

        add_connection (object, other_object, CONN_MIDPOINT,
                        other_object->line->x[k],
                        other_object->line->y[k], -1, k);

        add_connection (other_object, object, CONN_MIDPOINT,
                        other_object->line->x[k],
                        other_object->line->y[k], k, -1);
      }
    }
  }

#if DEBUG
  s_conn_print(object->conn_list);
#endif
}

/*! \brief add an LeptonObject to the connection system
 *
 *  \par Function Description
 *  This function searches for all geometrical connections of the
 *  LeptonObject <b>object</b> to all other connectable
 *  objects. It adds connections to the object and from all other
 *  objects to this one.
 *
 *  \param page   The LeptonPage structure
 *  \param object LeptonObject to add into the connection system
 */
void
s_conn_update_object (LeptonPage* page,
                      LeptonObject *object)
{

  /* Add object to the list of connectible objects */
  s_conn_add_object (page, object);

  switch (lepton_object_get_type (object)) {
    case OBJ_PIN:
    case OBJ_NET:
    case OBJ_BUS:
      s_conn_update_line_object (page, object);
      break;

    case OBJ_COMPONENT:
      s_conn_update_glist (page, object->component->prim_objs);
      break;
  }
}

/*! \brief print all connections of a connection list
 *  \par Function Description
 *  This is a debugging function to print a List of connections.
 *  \param conn_list GList of connection objects
 */
void s_conn_print(GList * conn_list)
{
  LeptonConn *conn;
  GList *cl_current;

  printf("\nStarting s_conn_print\n");
  cl_current = conn_list;
  while (cl_current != NULL) {

    conn = (LeptonConn *) cl_current->data;
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
 *  \param [in] new_net    Net LeptonObject to compare to.
 *  \param [in] whichone   The connection number to check.
 *  \param [in] conn_list  List of existing connections to compare
 *                         <B>new_net</B> to.
 *  \return TRUE if a matching connection is found, FALSE otherwise.
 */
int s_conn_net_search(LeptonObject* new_net, int whichone, GList * conn_list)
{
  LeptonConn *conn;
  GList *cl_current;

  cl_current = conn_list;
  while (cl_current != NULL) {

    conn = (LeptonConn *) cl_current->data;
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

/*! \brief get a list of all objects connected to a list of LeptonObjects.
 *
 *  \par Function Description
 *  This function gets all other_object from the connection
 *  list of the LeptonObjects in the pased list.
 *
 *  \param [in] input_list GList of LeptonObject's or NULL
 *  \param [in] obj_list   The GList of LeptonObject to get connections from
 *  \return A GList of objects
 *
 *  \warning
 *  Caller must g_list_free returned GList pointer.
 *  Do not free individual data items in list.
 */
static GList *s_conn_return_glist_others (GList *input_list, GList *obj_list)
{
  GList *return_list;
  GList *iter;
  LeptonObject *o_current;

  return_list = input_list;

  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    o_current = (LeptonObject*) iter->data;
    return_list = s_conn_return_others (return_list, o_current);
  }

  return return_list;
}

/*! \brief get a list of all objects connected to this one
 *
 *  \par Function Description
 *  This function gets all other_object from the connection list of the current object.
 *  Component objects are entered, and their prim_objs processed. If an <b>input_list</b>
 *  is given, the other objects are appended to that list.
 *
 *  \param [in] input_list   GList of LeptonObject's
 *  \param [in] object       LeptonObject to get other LeptonObjects from
 *  \return A GList of LeptonObjects
 *
 *  \warning
 *  Caller must g_list_free returned GList pointer.
 *  Do not free individual data items in list.
 */
GList *s_conn_return_others(GList *input_list, LeptonObject *object)
{
  GList *c_iter;
  GList *return_list;

  return_list = input_list;

  switch (lepton_object_get_type (object)) {
    case OBJ_PIN:
    case OBJ_NET:
    case OBJ_BUS:
      for (c_iter = object->conn_list;
           c_iter != NULL; c_iter = g_list_next (c_iter)) {
        LeptonConn *conn = (LeptonConn *) c_iter->data;

        if (conn->other_object && conn->other_object != object) {
          return_list = g_list_append(return_list, conn->other_object);
        }
      }
      break;

    case OBJ_COMPONENT:
      return_list = s_conn_return_glist_others (return_list,
                                                object->component->prim_objs);
      break;
  }

  return return_list;
}

/*! \brief add a line object to the list of connectible objects
 *  \par Function Description
 *  \param page   The LeptonPage structure
 *  \param object The line LeptonObject to add
 */
static void
s_conn_add_line_object (LeptonPage *page,
                        LeptonObject *object)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->line != NULL);

#if DEBUG
  printf ("name: %s\n", object->name);
#endif

  if (page == NULL) {
    return;
  }

  if (!g_list_find (page->connectible_list, object)) {
    page->connectible_list = g_list_append (page->connectible_list, object);
  }
}

/*! \brief add an object to the list of connectible objects
 *  \par Function Description
 *  This function takes dispatches the object to the correct
 *  function, depending on its type.
 *
 *  \param page   The LeptonPage structure
 *  \param object The line LeptonObject to add
 */
void
s_conn_add_object (LeptonPage *page,
                   LeptonObject *object)
{
  GList *iter;

  switch (lepton_object_get_type (object)) {
    case OBJ_NET:
    case OBJ_PIN:
    case OBJ_BUS:
      s_conn_add_line_object (page, object);
      break;

  case OBJ_COMPONENT:
    for (iter = object->component->prim_objs;
         iter != NULL;
         iter = g_list_next (iter)) {
      s_conn_add_object (page, (LeptonObject*) iter->data);
    }
  }
}

/*! \brief remove an object from the list of connectible objects
 *  \par Function Description
 *  \param object The object to remove
 */
void
s_conn_remove_object (LeptonPage* page,
                      LeptonObject *object)
{
  GList *iter;

  if (page == NULL) {
    return;
  }

  /* Correctly deal with compound objects */
  if (lepton_object_is_component (object))
  {
    for (iter = object->component->prim_objs;
         iter != NULL;
         iter = g_list_next (iter)) {
      s_conn_remove_object (page, (LeptonObject*) iter->data);
    }
  }

  page->connectible_list = g_list_remove (page->connectible_list, object);
}
