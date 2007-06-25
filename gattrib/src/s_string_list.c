/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003-2007 Stuart D. Brorson.
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

/*------------------------------------------------------------------
 * This file holds fcns involved in manipulating the STRING_LIST
 * structure.  STRING_LIST is basically a linked list of strings
 * (text).
 *------------------------------------------------------------------*/

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <math.h>

/*------------------------------------------------------------------
 * Gattrib specific includes
 *------------------------------------------------------------------*/
#include <libgeda/libgeda.h>       /* geda library fcns  */
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif



/*------------------------------------------------------------------*/
/*! \brief This returns a pointer to a new STRING_LIST object.
 *
 *------------------------------------------------------------------*/
STRING_LIST *s_string_list_new() {
  STRING_LIST *local_string_list;
  
  local_string_list = g_malloc(sizeof(STRING_LIST));
  local_string_list->data = NULL;
  local_string_list->next = NULL;
  local_string_list->prev = NULL;
  local_string_list->pos = -1;   /* can look for this later . . .  */
  
  return local_string_list;
}


/*------------------------------------------------------------------*/
/*! \brief This takes an old string list, duplicates it and returns a pointer
 * to the new, duplicate list.
 *
 *------------------------------------------------------------------*/
STRING_LIST *s_string_list_duplicate_string_list(STRING_LIST *old_string_list) {
  STRING_LIST *new_string_list;
  STRING_LIST *local_string_list;
  char *data;
  gint count;

  new_string_list = s_string_list_new();

  if (old_string_list->data == NULL) 
    /* This is an empty string list */
    return new_string_list;

  local_string_list = old_string_list;
  while (local_string_list != NULL) {
    data = g_strdup(local_string_list->data);
    s_string_list_add_item(new_string_list, &count, data);
    g_free(data);
    local_string_list = local_string_list->next;
  }

  return new_string_list;
}


/*------------------------------------------------------------------*/
/*! \brief This fcn inserts the item into a char* list.  
 * It first cycles through the
 * list to make sure that there are no duplications. The list is assumed
 * to be a STRING_LIST:
 * struct STRING_LIST
 * {
 *   char *data;
 *   int pos;
 *   STRING_LIST *next;
 *   STRING_LIST *prev;
 * };
 *
 *------------------------------------------------------------------*/
void s_string_list_add_item(STRING_LIST *list, int *count, char *item) {

  gchar *trial_item = NULL;
  STRING_LIST *prev;
  STRING_LIST *local_list;
  
  if (list == NULL) {
    fprintf(stderr, "In s_string_list_add_item, tried to add to a NULL list.\n");
    return;
  }

  /* First check to see if list is empty.  Handle insertion of first item 
     into empty list separately.  (Is this necessary?) */
  if (list->data == NULL) {
#ifdef DEBUG
    printf("In s_string_list_add_item, about to place first item in list.\n");
#endif
    list->data = (gchar *) g_strdup(item);
    list->next = NULL;
    list->prev = NULL;  /* this may have already been initialized. . . . */
    list->pos = *count; /* This enumerates the pos on the list.  Value is reset later by sorting. */
    (*count)++;  /* increment count to 1 */
    return;
  }

  /* Otherwise, loop through list looking for duplicates */
  prev = list;
  while (list != NULL) {
    trial_item = (gchar *) g_strdup(list->data);        
    if (strcmp(trial_item, item) == 0) {
      /* Found item already in list.  Just return. */
      g_free(trial_item);
      return;
    }
    g_free(trial_item);
    prev = list;
    list = list->next;
  }

  /* If we are here, it's 'cause we didn't find the item pre-existing in the list. */
  /* In this case, we insert it. */

  local_list = (STRING_LIST *) g_malloc(sizeof(STRING_LIST));  /* allocate space for this list entry */
  local_list->data = (gchar *) g_strdup(item);   /* copy data into list */
  local_list->next = NULL;
  local_list->prev = prev;  /* point this item to last entry in old list */
  prev->next = local_list;  /* make last item in old list point to this one. */
  local_list->pos = *count; /* This enumerates the pos on the list.  Value is reset later by sorting. */
  (*count)++;  /* increment count */
  /*   list = local_list;  */
  return;

}


/*------------------------------------------------------------------*/
/*! \brief This fcn deletes an item in a STRING_LIST.
 * It takes args: list to to delete item, pointer to no of items in
 * list at end, and the item itself to remove.
 *
 *------------------------------------------------------------------*/
void s_string_list_delete_item(STRING_LIST **list, int *count, gchar *item) {

  gchar *trial_item = NULL;
  STRING_LIST *list_item;
  STRING_LIST *next_item = NULL;
  STRING_LIST *prev_item = NULL;

  /* First check to see if list is empty.  If empty, spew error and return */
  if ( (*list)->data == NULL) {
    fprintf(stderr, "In s_string_list_delete_item, tried to remove item from empty list\n");
    return;
  }

#ifdef DEBUG
    printf("In s_string_list_delete_item, about to delete item %s from list.\n", item);
#endif

  /* Now loop through list looking for item */
  list_item = (*list);
  while (list_item != NULL) {
    trial_item = (gchar *) g_strdup(list_item->data);        
#ifdef DEBUG
    printf("In s_string_list_delete_item, matching item against trial item = %s from list.\n", trial_item);
#endif
    if (strcmp(trial_item, item) == 0) {
      /* found item, now delete it. */
#ifdef DEBUG
    printf("In s_string_list_delete_item, found match . . . . . \n");
#endif
      prev_item = list_item->prev;
      next_item = list_item->next;

      /* Check position in list */
      if (next_item == NULL && prev_item == NULL) {
	/* pathological case of one item list. */
	(*list) = NULL;
      } else if (next_item == NULL && prev_item != NULL) {
	/* at list's end */
	prev_item->next = NULL;
      } else if (next_item != NULL && prev_item == NULL) {
	/* at list's beginning */
	next_item->prev = NULL;
	(*list) = next_item;         /* also need to fix pointer to list head */
	/*  g_free(list);  */
      } else {
	/* normal case of element in middle of list */
	prev_item->next = next_item;
	next_item->prev = prev_item;
      }
      
#ifdef DEBUG
    printf("In s_string_list_delete_item, now free list_item\n");
#endif
      g_free(list_item);  /* free current list item */
      (*count)--;       /* decrement count */
      /* Do we need to re-number the list? */

#ifdef DEBUG
    printf("In s_string_list_delete_item, now free trial_item\n");
#endif
      g_free(trial_item); /* free trial item before returning */
#ifdef DEBUG
    printf("In s_string_list_delete_item, returning . . . .\n");
#endif
      return;
    }
    g_free(trial_item);
    list_item = list_item->next;
  }

  /* If we are here, it's 'cause we didn't find the item.
   * Spew error and return.
   */
  fprintf(stderr, "In s_string_list_delete_item, couldn't delete item %s\n", item);
  return;

}


/*------------------------------------------------------------------*/
/*! \brief This fcn looks for item in the list.  It returns 1 if item is
 * present, 0 if absent.
 *
 *------------------------------------------------------------------*/
int s_string_list_in_list(STRING_LIST *list, char *item) {

  gchar *trial_item = NULL;
  
  /* First check to see if list is empty.  If empty, return
   * 0 automatically.  (I probably don't need to handle this 
   * separately.)  */
  if (list->data == NULL) {
    return 0;
  }

  /* Otherwise, loop through list looking for duplicates */
  while (list != NULL) {
    trial_item = (gchar *) g_strdup(list->data);        
    if (strcmp(trial_item, item) == 0) {
      /* Found item already in list.  return 1. */
      g_free(trial_item);
      return 1;
    }
    g_free(trial_item);
    list = list->next;
  }

  /* If we are here, it's 'cause we didn't find the item 
   * pre-existing in the list.  In this case, return 0 */
  return 0;

}


/*------------------------------------------------------------------*/
/*! \brief This fcn returns the index'th item in the string list.
 * It returns NULL if there is a problem
 *
 *------------------------------------------------------------------*/
gchar *s_string_list_get_data_at_index(STRING_LIST *list, gint index) 
{
  gint i;
  STRING_LIST *local_item;

  /* First check to see if list is empty.  If empty, return
   * NULL automatically.  */
  if (list->data == NULL) {
    return NULL;
  }

  local_item = list;
  for (i = 0 ; i < index ; i++) {
    if (local_item == NULL) {
      return NULL;
    } else {
      local_item = local_item->next;
    }
  }
  return local_item->data;
}


/*------------------------------------------------------------------*/
/*! \brief This fcn takes the master comp list 
 * sheet_head->master_comp_list_head
 * and sorts it in this order:
 * <all refdeses in alphabetical order>
 * Right now it does nothing other than fill in the "position"
 * and "length" variables.
 *
 *------------------------------------------------------------------*/
void s_string_list_sort_master_comp_list() {
  int i = 0;
  STRING_LIST *local_list, *p;

  /* Here's where we do the sort.  The sort is done using a fcn found on the web. */
  local_list = sheet_head->master_comp_list_head;
  for (p=local_list; p; p=p->next)
    p->pos = 0;
  local_list = listsort(local_list, 0, 1);

  /* Do this after sorting is done.  This resets the order of the individual items
   * in the list.  */
  while (local_list != NULL) {  /* make sure item is not null */
    local_list->pos = i;
    if (local_list->next != NULL) {  
      i++;
      local_list = local_list->next;
    } else {
      break;                    /* leave loop *before* iterating to NULL EOL marker */
    }
  }

  /* Now go to first item in local list and reassign list head to new first element */
  while (local_list->prev != NULL) {
    local_list = local_list->prev;
  }

  sheet_head->master_comp_list_head = local_list;

  return;
}

/*------------------------------------------------------------------*/
/*! \brief This fcn takes the master comp attrib list 
 * sheet_head->master_comp_attrib_list_head
 * and sorts it in this order:
 * <all refdeses in alphabetical order>
 * Right now it does nothing other than fill in the "position"
 * and "length" variables.
 *
 *------------------------------------------------------------------*/

/* This list overrides the alphanumeric sort.  Attribs not found in
   this list are sorted as if they had a value of DEFAULT_ATTRIB_POS
   within this list, but alphanumerically relative to each other.  */
static struct {
  const char *attrib;
  int pos;
} certain_attribs[] = {
  {"device", 1},
  {"footprint", 2},
  {"value", 3},
  {"symversion", 200}
};
#define NUM_CERTAINS (sizeof(certain_attribs)/sizeof(certain_attribs[0]))
#define DEFAULT_ATTRIB_POS 100

void s_string_list_sort_master_comp_attrib_list() {
  int i = 0;
  STRING_LIST *local_list, *p;

  /* Here's where we do the sort */
  local_list = sheet_head->master_comp_attrib_list_head;

  /*
   * Note that this sort is TBD -- it is more than just an alphabetic sort 'cause we want 
   * certain attribs to go first. 
   */
  for (p=local_list; p; p=p->next) {
    int i;
    p->pos = DEFAULT_ATTRIB_POS;
    for (i=0; i<NUM_CERTAINS; i++)
      if (p->data != NULL) {
        if (strcmp (certain_attribs[i].attrib, p->data) == 0)
  	{
	  p->pos = certain_attribs[i].pos;
	  break;
	}
      }
  }

  local_list = listsort(local_list, 0, 1);
  sheet_head->master_comp_attrib_list_head = local_list;

  /* Do this after sorting is done.  This resets the order of the individual items
   * in the list.  */
  while (local_list != NULL) {
    local_list->pos = i;
    i++;
    local_list = local_list->next;
  }

  return;
}

/*------------------------------------------------------------------*/
/*! \brief This fcn takes the master net list 
 * sheet_head->master_net_list_head
 * and sorts it in this order:
 * <all nets in alphabetical order>
 *
 *------------------------------------------------------------------*/
void s_string_list_sort_master_net_list() {
  int i = 0;
  STRING_LIST *local_list;


  /* Do this after sorting is done.  This resets the order of the individual items
   * in the list.  */
  local_list = sheet_head->master_net_list_head;
  while (local_list != NULL) {
    local_list->pos = i;
    i++;
    local_list = local_list->next;
  }

  return;
}

/*------------------------------------------------------------------*/
/*! \brief This fcn takes the master net attrib list 
 * sheet_head->master_net_attrib_list_head
 * and sorts it in this order:
 * value, footprint, model-name, file, 
 * <all other attribs in alphabetical order>
 *
 *------------------------------------------------------------------*/
void s_string_list_sort_master_net_attrib_list() {
  int i = 0;
  STRING_LIST *local_list;


  /* Do this after sorting is done.  This resets the order of the individual items
   * in the list.  */
  local_list = sheet_head->master_net_attrib_list_head;
  while (local_list != NULL) {
    local_list->pos = i;
    i++;
    local_list = local_list->next;
  }

  return;
}


/*------------------------------------------------------------------*/
/*! \brief This fcn takes the master pin list 
 * sheet_head->master_pin_list_head
 * and sorts it in this order:
 * <all refdeses in alphabetical order>
 * Right now it does nothing other than fill in the "position"
 * and "length" variables.
 *
 *------------------------------------------------------------------*/
void s_string_list_sort_master_pin_list() {
  int i = 0;
  STRING_LIST *local_list, *p;

  /* Here's where we do the sort.  The sort is done using a fcn found on the web. */
  local_list = sheet_head->master_pin_list_head;
  for (p=local_list; p; p=p->next)
    p->pos = 0;
  local_list = listsort(local_list, 0, 1);

  /* Do this after sorting is done.  This resets the order of the individual items
   * in the list.  */
  while (local_list != NULL) {  /* make sure item is not null */
    local_list->pos = i;
    if (local_list->next != NULL) {  
      i++;
      local_list = local_list->next;
    } else {
      break;                    /* leave loop *before* iterating to NULL EOL marker */
    }
  }

  /* Now go to first item in local list and reassign list head to new first element */
  while (local_list->prev != NULL) {
    local_list = local_list->prev;
  }

  sheet_head->master_pin_list_head = local_list;

  return;
}

/*------------------------------------------------------------------*/
/*! \brief This fcn takes the master pin attrib list 
 * sheet_head->master_pin_attrib_list_head
 * and sorts it in this order:
 * <all pin attribs in alphabetical order>
 * Right now it does nothing other than fill in the "position"
 * and "length" variables.
 *
 *------------------------------------------------------------------*/
void s_string_list_sort_master_pin_attrib_list() {
  int i = 0;
  STRING_LIST *local_list;

  /* Here's where we do the sort */

  /*
   * Note that this sort is TBD -- it is more than just an alphabetic sort 'cause we want 
   * certain attribs to go first. 
   */
  

  /* Do this after sorting is done.  This resets the order of the individual items
   * in the list.  */
  local_list = sheet_head->master_pin_attrib_list_head;
  while (local_list != NULL) {
    local_list->pos = i;
    i++;
    local_list = local_list->next;
  }

  return;
}

