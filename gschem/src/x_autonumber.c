/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */

#include <config.h>

#include <stdio.h>
#include <ctype.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "gschem.h"
#include <gdk/gdkkeysyms.h>

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define GLADE_HOOKUP_OBJECT(component,widget,name) \
  g_object_set_data_full (G_OBJECT (component), name, \
    gtk_widget_ref (widget), (GDestroyNotify) gtk_widget_unref)

/** @brief How many entries to keep in the "Search text" combo box. */
#define HISTORY_LENGTH		15

/* autonumber text structs and enums */
enum {
  AUTONUMBER_SORT_DIAGONAL,
  AUTONUMBER_SORT_YX, 
  AUTONUMBER_SORT_YX_REV, 
  AUTONUMBER_SORT_XY, 
  AUTONUMBER_SORT_XY_REV, 
  AUTONUMBER_SORT_FILE 
};

enum {
  AUTONUMBER_IGNORE, 
  AUTONUMBER_RENUMBER, 
  AUTONUMBER_RESPECT
};

enum {
  SCOPE_SELECTED, 
  SCOPE_PAGE, 
  SCOPE_HIERARCHY 
};

typedef struct autonumber_text_t AUTONUMBER_TEXT;

/** @brief Stored state of the autonumber text dialog */
struct autonumber_text_t {
  /** @brief Search text history */
  GList *scope_text;

  /** @brief Scope for searching existing numbers */
  gint scope_skip;

  /** @brief Scope for autonumbering text */
  gint scope_number;

  /** @brief Overwrite existing numbers in scope */
  gboolean scope_overwrite;

  /** @brief Sort order */
  gint order;

  /** @brief Starting number for automatic numbering */
  gint startnum;

  /** @brief Remove numbers instead of automatic numbering */
  gboolean removenum;

  /** @brief Automatic assignments of slots */
  gboolean slotting;

  /** @brief Pointer to the dialog */ 
  GtkWidget *dialog;

  /** @brief Pointer to the GSCHEM_TOPLEVEL struct */
  GSCHEM_TOPLEVEL *w_current;

  /* variables used while autonumbering */
  gchar * current_searchtext;
  gint root_page;      /* flag whether its the root page or not */
  GList *used_numbers; /* list of used numbers */ 
  GList *free_slots;   /* list of FREE_SLOT objects */
  GList *used_slots;   /* list of USED_SLOT objects */
};

typedef struct autonumber_slot_t AUTONUMBER_SLOT;

struct autonumber_slot_t {
  gchar *symbolname;     /* or should I use the device name? (Werner) */
  gint number;           /* usually this is the refdes number */
  gint slotnr;      /* just the number of the free slot */
};



/* ***** BACK-END CODE ***************************************************** */

/********** compare functions for g_list_sort, ... ***********************/
/*! \brief GCompareFunc function to sort a list with g_list_sort().
 *  \par Function Description
 *  Compares the integer values of the gconstpointers a and b.
 *  \return -1 if a<b, 1 if a>b, 0 if a==b
 */
gint autonumber_sort_numbers(gconstpointer a, gconstpointer b) {
  if (GPOINTER_TO_INT(a) < GPOINTER_TO_INT(b))
    return -1;
  if (GPOINTER_TO_INT(a) > GPOINTER_TO_INT(b))
    return 1;
  return 0;
}

/*! \brief GCompareFunc function to sort text objects by there location 
 *  \par Function Description 
 *  This Funcion takes two <B>OBJECT*</B> arguments and compares the  
 *  location of the two text objects. The first sort criteria is the x location,
 *  the second sort criteria is the y location.
 *  The Function is used as GCompareFunc by g_list_sort().
 */
gint autonumber_sort_xy(gconstpointer a, gconstpointer b) {
  OBJECT *aa, *bb;
  aa=(OBJECT *) a;  bb=(OBJECT *) b;
  if (aa->text->x < bb->text->x)
    return -1;
  if (aa->text->x > bb->text->x)
    return 1;
  /* x == x */
  if (aa->text->y > bb->text->y)
    return -1;
  if (aa->text->y < bb->text->y)
    return 1;
  return 0;
}

/*! \brief GCompareFunc function to sort text objects by there location 
 *  \par Function Description
 *  This funcion takes two <B>OBJECT*</B> arguments and compares the  
 *  location of the two text objects. The first sort criteria is the x location,
 *  the second sort criteria is the y location.
 *  This function sorts the objects in reverse order.
 *  The function is used as GCompareFunc by g_list_sort().
 */
gint autonumber_sort_xy_rev(gconstpointer a, gconstpointer b) {
  OBJECT *aa, *bb;
  aa=(OBJECT *) a;  bb=(OBJECT *) b;
  if (aa->text->x < bb->text->x)
    return 1;
  if (aa->text->x > bb->text->x)
    return -1;
  /* x == x */
  if (aa->text->y < bb->text->y)
    return 1;
  if (aa->text->y > bb->text->y)
    return -1;
  return 0;
}

/*! \brief GCompareFunc function to sort text objects by there location 
 *  \par Function Description
 *  This funcion takes two <B>OBJECT*</B> arguments and compares the 
 *  location of the two text objects. The first sort criteria is the y location,
 *  the second sort criteria is the x location.
 *  The function is used as GCompareFunc by g_list_sort().
 */
int autonumber_sort_yx(gconstpointer a, gconstpointer b) {
  OBJECT *aa, *bb;
  aa=(OBJECT *) a;  bb=(OBJECT *) b;
  if (aa->text->y > bb->text->y)
    return -1;
  if (aa->text->y < bb->text->y)
    return 1;
  /* y == y */
  if (aa->text->x < bb->text->x)
    return -1;
  if (aa->text->x > bb->text->x)
    return 1;
  return 0;
}

/*! \brief GCompareFunc function to sort text objects by there location 
 *  \par Function Description
 *  This Funcion takes two <B>OBJECT*</B> arguments and compares the 
 *  location of the two text objects. The first sort criteria is the y location,
 *  the second sort criteria is the x location.
 *  This function sorts the objects in reverse order.
 *  The function is used as GCompareFunc by g_list_sort().
 */
int autonumber_sort_yx_rev(gconstpointer a, gconstpointer b) {
  OBJECT *aa, *bb;
  aa=(OBJECT *) a;  bb=(OBJECT *) b;
  if (aa->text->y > bb->text->y)
    return 1;
  if (aa->text->y < bb->text->y)
    return -1;
  /* y == y */
  if (aa->text->x > bb->text->x)
    return 1;
  if (aa->text->x < bb->text->x)
    return -1;
  return 0;
}

/*! \brief GCompareFunc function to sort text objects by there location 
 *  \par Function Description
 *  This Funcion takes two <B>OBJECT*</B> arguments and compares the 
 *  location of the two text objects. The sort criteria is the combined x- and the 
 *  y-location. The function sorts from top left to bottom right.
 *  The function is used as GCompareFunc by g_list_sort().
 */
int autonumber_sort_diagonal(gconstpointer a, gconstpointer b) {
  OBJECT *aa, *bb;
  aa=(OBJECT *) a;  bb=(OBJECT *) b;
  if (aa->text->x - aa->text->y < bb->text->x - bb->text->y)
    return -1;
  if (aa->text->x - aa->text->y > bb->text->x - bb->text->y)
    return 1;
  return 0;
}

/*! \brief GCompareFunc function to acces <B>AUTONUMBER_SLOT</B> object in a GList
 *  \par Function Description
 *  This Funcion takes two <B>AUTONUMBER_SLOT*</B> arguments and compares them.
 *  Sorting criteria is are the AUTONUMBER_SLOT members: first the symbolname, than the 
 *  number and last the slotnr.
 *  If the number or the slotnr is set to zero it acts as a wildcard. 
 *  The function is used as GCompareFunc by GList functions.
 */
gint freeslot_compare(gconstpointer a, gconstpointer b) 
{
  AUTONUMBER_SLOT *aa, *bb;
  gint res;
  aa = (AUTONUMBER_SLOT *) a;  bb = (AUTONUMBER_SLOT *) b;
  
  if ((res = strcmp(aa->symbolname, bb->symbolname)) != 0)
    return res;

  /* aa->symbolname == bb->symbolname */
  if (aa->number == 0 || bb->number == 0)
    return 0;
  if (aa->number > bb->number)
    return 1;
  if (aa->number < bb->number)
    return -1;

  /* aa->number == bb->number */
  if (aa->slotnr == 0 || bb->slotnr == 0)
    return 0;
  if (aa->slotnr > bb->slotnr)
    return 1;
  if (aa->slotnr < bb->slotnr)
    return -1;

  return 0;
}

/*! \brief Prints a <B>GList</B> of <B>AUTONUMBER_SLOT</B> elements
 *  \par Function Description
 *  This funcion prints the elements of a GList that contains <B>AUTONUMBER_SLOT</B> elements
 *  It is only used for debugging purposes.
 */
void freeslot_print(GList *list) {
  GList *item;
  AUTONUMBER_SLOT *fs;
  
  printf("freeslot_print(): symname, number, slot\n");
  for (item = list; item != NULL; item = g_list_next(item)) {
    fs = item ->data;
    printf("  %s, %d, %d\n",fs->symbolname, fs->number, fs->slotnr);
  }
}


/*! \brief Function to clear the databases of used parts
 *  \par Function Descriptions
 *  Just remove the list of used numbers, used slots and free slots.
 */
void autonumber_clear_database (AUTONUMBER_TEXT *autotext)
{
  /* cleanup everything for the next searchtext */
  if (autotext->used_numbers != NULL) {
    g_list_free(autotext->used_numbers);
    autotext->used_numbers = NULL;
  }
  if (autotext->free_slots != NULL) {
    g_list_foreach(autotext->free_slots, (GFunc) g_free, NULL);
    g_list_free(autotext->free_slots);
    autotext->free_slots = NULL;
  }
  if (autotext->used_slots != NULL) {
    g_list_foreach(autotext->used_slots, (GFunc) g_free, NULL);
    g_list_free(autotext->used_slots);
    autotext->used_slots = NULL;
  }
}

/*! \brief Function to test, whether the OBJECT matches the autotext criterias
 *  \par Function Description
 *  The criterias are those of the autonumber text dialog. The function decides
 *  whether the <B>OBJECT</B> has to be renumberd, ignored or taken care of when
 *  renumbering all other objects.  
 *  \return one of these integer values: <B>AUTONUMBER_IGNORE</B>, 
 *  <B>AUTONUMBER_RESPECT</B> or <B>AUTONUMBER_RENUMBER</B> and the current number
 *  of the text object in <B>*number</B>.
 */
gint autonumber_match(AUTONUMBER_TEXT *autotext, OBJECT *o_current, gint *number)
{
  gint i, len, isnumbered=1; 
  const gchar *str = NULL;

  len = strlen(autotext->current_searchtext);
  /* first find out whether we can ignore that object */
  if (o_current->type != OBJ_TEXT)  /* text object */
    return AUTONUMBER_IGNORE;

  str = o_text_get_string (autotext->w_current->toplevel, o_current);

  if (!(strlen(str) - len > 0)
      || !g_str_has_prefix(str, autotext->current_searchtext))
    return AUTONUMBER_IGNORE;

  /* the string object matches with its leading characters to the searchtext */
  /* now look for the extension, either a number or the "?" */
  if (g_str_has_suffix (str,"?")) {
    isnumbered = 0;
    /* There must not be any character between the "?" and the searchtext */
    if (strlen(str) != len+1)
      return AUTONUMBER_IGNORE;
  }
  else {
    if (!isdigit( (int) (str[len]) )) /* has at least one digit */
      return AUTONUMBER_IGNORE;
    
    for (i=len+1; str[i]; i++) /* and only digits */
      if (!isdigit( (int) (str[i]) ))
	return AUTONUMBER_IGNORE;
  }
  
  /* we have six cases, 3 from focus multiplied by 2 selection cases */
  if ((autotext->root_page || autotext->scope_number == SCOPE_HIERARCHY)
      && (o_current->selected 
	  || autotext->scope_number == SCOPE_HIERARCHY || autotext->scope_number == SCOPE_PAGE)
      && (!isnumbered || (autotext->scope_overwrite)))
    return AUTONUMBER_RENUMBER;
  
  if (isnumbered
      && !(autotext->scope_skip == SCOPE_SELECTED 
	   && !(o_current->selected)  && autotext->root_page)) {
    sscanf(&(str[len])," %d", number);
    return AUTONUMBER_RESPECT; /* numbered objects which we don't renumber */
  }
  else
    return AUTONUMBER_IGNORE;  /* unnumbered objects outside the focus */
}


/*! \brief Creates a list of already numbered objects and slots
 *  \par Function Description
 *  This function collects the used numbers of a single schematic page.
 *  The used element numbers are stored in a GList container
 *  inside the <B>AUTONUMBER_TEXT</B> struct.
 *  The slotting container is a little bit different. It stores free slots of
 *  multislotted symbols, that were used only partially.
 *  The criterias are derivated from the autonumber dialog entries.
 */
void autonumber_get_used(GSCHEM_TOPLEVEL *w_current, AUTONUMBER_TEXT *autotext)
{
  gint number, numslots, slotnr, i;
  OBJECT *o_current, *o_parent;
  AUTONUMBER_SLOT *slot;
  GList *slot_item;
  char *numslot_str, *slot_str;
  const GList *iter;
  
  for (iter = s_page_objects (w_current->toplevel->page_current);
       iter != NULL;
       iter = g_list_next (iter)) {
    o_current = iter->data;
    if (autonumber_match(autotext, o_current, &number) == AUTONUMBER_RESPECT) {
      /* check slot and maybe add it to the lists */
      o_parent = o_current->attached_to;
      if (autotext->slotting && o_parent != NULL) {
	/* check for slotted symbol */
	numslot_str =
	  o_attrib_search_object_attribs_by_name (o_parent, "numslots", 0);
	if (numslot_str != NULL) {
	  sscanf(numslot_str," %d",&numslots);
	  g_free(numslot_str);

	  if (numslots > 0) { 
	    slot_str = o_attrib_search_object_attribs_by_name (o_parent, "slot", 0);
	    if (slot_str == NULL) {
	      s_log_message(_("slotted object without slot attribute may cause "
			      "problems when autonumbering slots\n"));
	    }
	    else {
	      sscanf(slot_str, " %d", &slotnr);
	      slot = g_new(AUTONUMBER_SLOT,1);
	      slot->number = number;
	      slot->slotnr = slotnr;
	      slot->symbolname = o_parent->complex_basename;
	

	      slot_item = g_list_find_custom(autotext->used_slots,
						 slot,
						 (GCompareFunc) freeslot_compare);
	      if (slot_item != NULL) { /* duplicate slot in used_slots */
		s_log_message(_("duplicate slot may cause problems: "
				"[symbolname=%s, number=%d, slot=%d]\n"),
				slot->symbolname, slot->number, slot->slotnr);
		g_free(slot);
	      }
	      else {
		autotext->used_slots = g_list_insert_sorted(autotext->used_slots,
							    slot,
							    (GCompareFunc) freeslot_compare);
		
		slot_item = g_list_find_custom(autotext->free_slots,
						   slot,
						   (GCompareFunc) freeslot_compare);
		if (slot_item == NULL) {
		  /* insert all slots to the list, except of the current one */
		  for (i=1; i <= numslots; i++) {
		    if (i != slotnr) {
		      slot = g_memdup(slot, sizeof(AUTONUMBER_SLOT));
		      slot->slotnr = i;
		      autotext->free_slots = g_list_insert_sorted(autotext->free_slots,
								  slot,
								  (GCompareFunc) freeslot_compare);
		    }
		  }
		}
		else {
		  g_free(slot_item->data);
		  autotext->free_slots = g_list_delete_link(autotext->free_slots, slot_item);
		}
	      }
	    }
	  }
	}
      }
      /* put number into the used list */
      autotext->used_numbers = g_list_insert_sorted(autotext->used_numbers,
						    GINT_TO_POINTER(number),
						    (GCompareFunc) autonumber_sort_numbers);
    }
  }
}


/*! \brief Gets or generates free numbers for the autonumbering process.
 *  \par Function Description
 *  This function gets or generates new numbers for the <B>OBJECT o_current</B>. 
 *  It uses the element numbers <B>used_numbers</B> and the list of the free slots
 *  <B>free_slots</B> of the <B>AUTONUMBER_TEXT</B> struct.
 *  \return 
 *  The new number is returned into the <B>number</B> parameter.
 *  <B>slot</B> is set if autoslotting is active, else it is set to zero.
 */
void autonumber_get_new_numbers(AUTONUMBER_TEXT *autotext, OBJECT *o_current, 
				gint *number, gint *slot)
{
  GList *item;
  gint new_number, numslots, i;
  AUTONUMBER_SLOT *freeslot;
  OBJECT *o_parent = NULL;
  GList *freeslot_item;
  gchar *numslot_str;

  new_number = autotext->startnum;
  
  /* Check for slots first */
  /* 1. are there any unused slots in the database? */
  o_parent = o_current->attached_to;
  if (autotext->slotting && o_parent != NULL) {
    freeslot = g_new(AUTONUMBER_SLOT,1);
    freeslot->symbolname = o_parent->complex_basename;
    freeslot->number = 0;
    freeslot->slotnr = 0;
    freeslot_item = g_list_find_custom(autotext->free_slots,
				       freeslot,
				       (GCompareFunc) freeslot_compare);
    g_free(freeslot);
    /* Yes! -> remove from database, apply it */
    if (freeslot_item != NULL) {
      freeslot = freeslot_item->data;
      *number = freeslot->number;
      *slot = freeslot->slotnr;
      g_free(freeslot);
      autotext->free_slots = g_list_delete_link(autotext->free_slots, freeslot_item);
      
      return;
    }
  }

  /* get a new number */
  item = autotext->used_numbers; 
  while (1) {
    while (item != NULL && GPOINTER_TO_INT(item->data) < new_number)
      item = g_list_next(item);

    if (item == NULL || GPOINTER_TO_INT(item->data) > new_number)
      break;
    else  /* new_number == item->data */
      new_number++;
  }
  *number = new_number;
  *slot = 0;
  
  /* insert the new number to the used list */
  autotext->used_numbers = g_list_insert_sorted(autotext->used_numbers,
						GINT_TO_POINTER(new_number),
						(GCompareFunc) autonumber_sort_numbers);

  /* 3. is o_current a slotted object ? */
  if ((autotext->slotting) && o_parent != NULL) {
    numslot_str =
      o_attrib_search_object_attribs_by_name (o_parent, "numslots", 0);
    if (numslot_str != NULL) {
      sscanf(numslot_str," %d",&numslots);
      g_free(numslot_str);
      if (numslots > 0) { 
	/* Yes! -> new number and slot=1; add the other slots to the database */
	*slot = 1;
	for (i=2; i <=numslots; i++) {
	  freeslot = g_new(AUTONUMBER_SLOT,1);
	  freeslot->symbolname = o_parent->complex_basename;
	  freeslot->number = new_number;
	  freeslot->slotnr = i;
	  autotext->free_slots = g_list_insert_sorted(autotext->free_slots,
						      freeslot,
						      (GCompareFunc) freeslot_compare);
	}
      }
    }
  }
}

/** @brief Removes the number from the element.
 *
 *  This function updates the text content of the \a o_current object.
 *
 *  @param autotext Pointer to the state structure
 *  @param o_current Pointer to the object from which to remove the number
 *
 */
void autonumber_remove_number(AUTONUMBER_TEXT * autotext, OBJECT *o_current)
{
  OBJECT *o_parent, *o_slot;
  gchar *slot_str;
  gchar *str = NULL;

  /* replace old text */
  str = g_strdup_printf("%s?", autotext->current_searchtext);
  o_text_set_string (autotext->w_current->toplevel, o_current, str);
  g_free (str);

  /* remove the slot attribute if slotting is active */
  if (autotext->slotting) {
    /* get the slot attribute */
    o_parent = o_current->attached_to;
    if (o_parent != NULL) {
      slot_str = s_slot_search_slot (o_parent, &o_slot);
      g_free (slot_str);
      /* Only attempt to remove non-inherited slot attributes */
      if (o_slot != NULL && !o_attrib_is_inherited (o_slot)) {
        /* delete the slot attribute */
        o_selection_remove (autotext->w_current->toplevel,
                            autotext->w_current->toplevel->
                              page_current->selection_list, o_slot);
        o_delete (autotext->w_current, o_slot);
      }
    }
  }
  autotext->w_current->toplevel->page_current->CHANGED = 1;
}

/*! \brief Changes the number <B>OBJECT</B> element. Changes the slot attribute.
 *  \par Function Description
 *  This function updates the text content of the <B>o_current</B> object.
 *  If the <B>slot</B> value is not zero. It updates the slot attribut of the
 *  complex element that is also the parent object of the o_current element.
 */
void autonumber_apply_new_text(AUTONUMBER_TEXT * autotext, OBJECT *o_current,
			       gint number, gint slot)
{
  char *str;

  /* update the slot on the owning object */
  str = g_strdup_printf ("slot=%d", slot);
  o_slot_end (autotext->w_current, o_current->attached_to, str);
  g_free (str);

  /* replace old text */
  str = g_strdup_printf("%s%d", autotext->current_searchtext, number);
  o_text_set_string (autotext->w_current->toplevel, o_current, str);
  g_free (str);

  autotext->w_current->toplevel->page_current->CHANGED = 1;
}


/*! \brief Handles all the options of the autonumber text dialog
 *  \par Function Description
 *  This function is the master of all autonumber code. It receives the options of
 *  the the autonumber text dialog in an <B>AUTONUMBER_TEXT</B> structure.
 *  First it collects all pages of a hierarchical schematic.
 *  Second it gets all matching text elements for the searchtext.
 *  Then it renumbers all text elements of all schematic pages. The renumbering 
 *  follows the rules of the parameters given in the autonumber text dialog.
 */
void autonumber_text_autonumber(AUTONUMBER_TEXT *autotext)
{
  GList *pages;
  GList *searchtext_list=NULL;
  GList *text_item, *obj_item, *page_item;
  OBJECT *o_current;
  GSCHEM_TOPLEVEL *w_current;
  gchar *searchtext;
  gchar *scope_text;
  gchar *new_searchtext;
  gint i, number, slot;
  GList *o_list = NULL;
  const GList *iter;
  
  w_current = autotext->w_current;
  autotext->current_searchtext = NULL;
  autotext->root_page = 1;
  autotext->used_numbers = NULL;
  autotext->free_slots = NULL;
  autotext->used_slots = NULL;

  scope_text = g_list_first(autotext->scope_text)->data;

  /* Step1: get all pages of the hierarchy */
  pages = s_hierarchy_traversepages(w_current->toplevel, HIERARCHY_NODUPS);

  /*  g_list_foreach(pages, (GFunc) s_hierarchy_print_page, NULL); */

  /* Step2: if searchtext has an asterisk at the end we have to find
     all matching searchtextes. 

     Example:  "refdes=*" will match each text that starts with "refdes="
     and has a trailing "?" or a trailing number if the "all"-option is set.
     We get a list of possible prefixes: refdes=R, refdes=C.

     If there is only one search pattern, it becomes a single item
     in the searchtext list */
  
  if (strlen(scope_text) == 0) {
    s_log_message(_("No searchstring given in autonumber text.\n"));
    return; /* error */
  }
  else if (g_str_has_suffix(scope_text,"?") == TRUE) {
    /* single searchtext, strip of the "?" */
    searchtext = g_strndup(scope_text, strlen(scope_text)-1);
    searchtext_list=g_list_append (searchtext_list, searchtext);
  }
  else if (g_str_has_suffix(scope_text,"*") == TRUE) {
    /* strip of the "*" */
    searchtext = g_strndup(scope_text, strlen(scope_text)-1);
    /* collect all the possible searchtexts in all pages of the hierarchy */
    for (page_item = pages; page_item != NULL; page_item = g_list_next(page_item)) {
      s_page_goto(w_current->toplevel, page_item->data);
      /* iterate over all objects an look for matching searchtext's */
      for (iter = s_page_objects (w_current->toplevel->page_current);
           iter != NULL;
           iter = g_list_next (iter)) {
	o_current = iter->data;
	if (o_current->type == OBJ_TEXT) {
	  if (autotext->scope_number == SCOPE_HIERARCHY
	      || autotext->scope_number == SCOPE_PAGE
	      || ((autotext->scope_number == SCOPE_SELECTED) && (o_current->selected))) {
            const gchar *str = o_text_get_string (w_current->toplevel, o_current);
	    if (g_str_has_prefix (str, searchtext)) {
	      /* the beginnig of the current text matches with the searchtext now */
	      /* strip of the trailing [0-9?] chars and add it too the searchtext */
	      for (i = strlen (str)-1;
		   (i >= strlen(searchtext))
		     && (str[i] == '?'
			 || isdigit( (int) (str[i]) ));
		   i--)
		; /* void */
		
	      new_searchtext = g_strndup (str, i+1);
	      if (g_list_find_custom(searchtext_list, new_searchtext,
				     (GCompareFunc) strcmp) == NULL ) {
		searchtext_list = g_list_append(searchtext_list, new_searchtext);
	      }
	      else {
		g_free(new_searchtext);
	      }
	    }
	  }
	}
      }
      if (autotext->scope_number == SCOPE_SELECTED || autotext->scope_number == SCOPE_PAGE)
	break; /* search only in the first page */
    }
    g_free(searchtext);
  }
  else {
    s_log_message(_("No '*' or '?' given at the end of the autonumber text.\n"));
    return;
  }

  /* Step3: iterate over the search items in the list */
  for (text_item=searchtext_list; text_item !=NULL; text_item=g_list_next(text_item)) {
    autotext->current_searchtext = text_item->data;
    /* printf("autonumber_text_autonumber: searchtext %s\n", autotext->current_searchtext); */
    /* decide whether to renumber page by page or get a global used-list */
    if (autotext->scope_skip == SCOPE_HIERARCHY) {  /* whole hierarchy database */
      /* renumbering all means that no db is required */
      if (!(autotext->scope_number == SCOPE_HIERARCHY
	    && autotext->scope_overwrite)) {
	for (page_item = pages; page_item != NULL; page_item = g_list_next(page_item)) {
	  autotext->root_page = (pages->data == page_item->data);
	  s_page_goto(w_current->toplevel, page_item->data);
	  autonumber_get_used(w_current, autotext);
	}
      }
    }
    
    /* renumber the elements */
    for (page_item = pages; page_item != NULL; page_item = g_list_next(page_item)) {
      s_page_goto(w_current->toplevel, page_item->data);
      autotext->root_page = (pages->data == page_item->data);
      /* build a page database if we're numbering pagebypage or selection only*/
      if (autotext->scope_skip == SCOPE_PAGE || autotext->scope_skip == SCOPE_SELECTED) {
	autonumber_get_used(w_current, autotext);
      }
      
      /* RENUMBER CODE FOR ONE PAGE AND ONE SEARCHTEXT*/
      /* 1. get objects to renumber */
      for (iter = s_page_objects (w_current->toplevel->page_current);
           iter != NULL;
           iter = g_list_next (iter)) {
        o_current = iter->data;
	if (autonumber_match(autotext, o_current, &number) == AUTONUMBER_RENUMBER) {
	  /* put number into the used list */
	  o_list = g_list_append(o_list, o_current);
	}
      }

      /* 2. sort object list */
      switch (autotext->order) {
      case AUTONUMBER_SORT_YX:
	o_list=g_list_sort(o_list, autonumber_sort_yx);
	break;
      case AUTONUMBER_SORT_YX_REV:
	o_list=g_list_sort(o_list, autonumber_sort_yx_rev);
	break;
      case AUTONUMBER_SORT_XY:
	o_list=g_list_sort(o_list, autonumber_sort_xy);
	break;
      case AUTONUMBER_SORT_XY_REV:
	o_list=g_list_sort(o_list, autonumber_sort_xy_rev);
	break;
      case AUTONUMBER_SORT_DIAGONAL:
	o_list=g_list_sort(o_list, autonumber_sort_diagonal);
	break;
      default:
	; /* unsorted file order */
      }
	 
      /* 3. renumber/reslot the objects */
      for(obj_item=o_list; obj_item != NULL; obj_item=g_list_next(obj_item)) {
	o_current= obj_item->data;
      	if(autotext->removenum) {
	  autonumber_remove_number(autotext, o_current);		
	} else {
	  /* get valid numbers from the database */
	  autonumber_get_new_numbers(autotext, o_current, &number, &slot);
	  /* and apply it. TODO: join these two functions */
	  autonumber_apply_new_text(autotext, o_current, number, slot);
	}
      }
      g_list_free(o_list);
      o_list = NULL;

      /* destroy the page database */
      if (autotext->scope_skip == SCOPE_PAGE 
	  || autotext->scope_skip == SCOPE_SELECTED) 
	autonumber_clear_database(autotext);

      if (autotext->scope_number == SCOPE_SELECTED 
	  || autotext->scope_number == SCOPE_PAGE)
	break; /* only renumber the parent page (the first page) */
    }
    autonumber_clear_database(autotext);   /* cleanup */
  }

  /* cleanup and redraw all*/
  g_list_foreach(searchtext_list, (GFunc) g_free, NULL);
  g_list_free(searchtext_list);
  s_page_goto(w_current->toplevel, pages->data); /* go back to the root page */
  o_invalidate_all (w_current);
  g_list_free(pages);
  o_undo_savestate(w_current, UNDO_ALL);
}

/* ***** UTILITY GUI FUNCTIONS (move to a separate file in the future?) **** */

/** @brief Finds a widget by its name given a pointer to its parent.
 *
 * @param widget Pointer to the parent widget.
 * @param widget_name Name of the widget.
 * @return Pointer to the widget or NULL if not found. */
GtkWidget* lookup_widget(GtkWidget *widget, const gchar *widget_name)
{
  GtkWidget *found_widget;

  found_widget = (GtkWidget*) g_object_get_data(G_OBJECT(widget), 
						widget_name);

  return found_widget;
}

/*! \brief Put the icons and the text into the sortorder combobox
 *  \par Function Description
 *  Load all bitmaps for the combobox and store them together with the label
 *  in a GtkListStore.
 */
void autonumber_sortorder_create(GSCHEM_TOPLEVEL *w_current, GtkWidget *sort_order)
{
  GtkListStore *store;
  GtkTreeIter iter;
  GtkCellRenderer *renderer;
  GdkPixbuf *pixbuf;
  gchar *path;
  GError *error=NULL;

  gchar *filenames[] = {"gschem-diagonal.png", 
			"gschem-top2bottom.png", "gschem-bottom2top.png",
			"gschem-left2right.png", "gschem-right2left.png",
			"gschem-fileorder.png",
			NULL};
  gchar *names[] = {N_("Diagonal"),
		    N_("Top to bottom"), N_("Bottom to top"),
		    N_("Left to right"), N_("Right to left"),
		    N_("File order"),
		    NULL};
  gint i;

  store = gtk_list_store_new(2, G_TYPE_STRING, GDK_TYPE_PIXBUF); 

  for (i=0; filenames[i] != NULL; i++) {
    path=g_build_filename(w_current->toplevel->bitmap_directory,
		     filenames[i], NULL);
    pixbuf = gdk_pixbuf_new_from_file(path, &error);
    g_free(path);
    gtk_list_store_append(store, &iter);
    gtk_list_store_set(store, &iter, 
		       0, _(names[i]),
		       1, pixbuf,
		       -1);
  }

  gtk_combo_box_set_model(GTK_COMBO_BOX(sort_order), GTK_TREE_MODEL(store));
  renderer = gtk_cell_renderer_text_new ();

  gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (sort_order),
			      renderer, TRUE);
  gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT (sort_order),
				  renderer, "text", 0, NULL);
  renderer = gtk_cell_renderer_pixbuf_new();
  g_object_set(G_OBJECT(renderer), "xpad", 5, "ypad", 5, NULL);

  gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (sort_order),
			      renderer, FALSE);
  gtk_cell_layout_set_attributes (GTK_CELL_LAYOUT (sort_order),
				  renderer, "pixbuf", 1, NULL);
}

/* ***** STATE STRUCT HANDLING (interface between GUI and backend code) **** */

/** @brief Adds a line to the search text history list
 *
 * Function makes sure that: 1) There are no duplicates in the list and 2) the
 * last search text is always at the top of the list.
 */
GList *autonumber_history_add(GList *history, gchar *text)
{
  /* Search for this text in history and delete it (so we don't have
   * duplicate entries) */

  GList *cur;

  cur=history;
  while(cur!=NULL) {
    if(!strcmp(text, cur->data)) {
      history=g_list_remove_link(history, cur);

      g_free(cur->data);
      g_list_free(cur);
      break;
    }
    cur=g_list_next(cur);
  }

  /* Add the new text at the beginning of the list */

  history=g_list_prepend(history, text);

  /* Truncate history */
  while(g_list_length(history) > HISTORY_LENGTH) {
    GList *last = g_list_last(history);

    history = g_list_remove_link(history, last);

    g_free(last->data);
    g_list_free(last);
  }

  return history;
}

/** @brief Allocate and initialize the state structure
 *
 * @return Pointer to the allocated structure or NULL on error. 
 */
AUTONUMBER_TEXT *autonumber_init_state()
{
  AUTONUMBER_TEXT *autotext;

  /* Default contents of the combo box history */
  gchar *default_text[] = {
    "refdes=*", 
    "refdes=C?", 
    "refdes=D?", 
    "refdes=I?",
    "refdes=L?", 
    "refdes=Q?", 
    "refdes=R?", 
    "refdes=T?",
    "refdes=U?", 
    "refdes=X?", 
    "netname=*", 
    "netname=A?",
    "netname=D?", 
    NULL
  };
  gchar **t;

  autotext = g_new(AUTONUMBER_TEXT, 1);

  if(autotext==NULL) return NULL;

  autotext->scope_text = NULL;
  t=default_text;
  while(*t!=NULL) {
    autotext->scope_text=g_list_append(autotext->scope_text, 
				       g_strdup(*t));
    t++;
  }

  autotext->scope_skip = SCOPE_PAGE;
  autotext->scope_number = SCOPE_SELECTED;

  autotext->scope_overwrite = 0;
  autotext->order = AUTONUMBER_SORT_DIAGONAL;

  autotext->startnum=1;

  autotext->removenum=0;
  autotext->slotting=0;

  autotext->dialog = NULL;

  return autotext;
}

/** @brief Restore the settings for the autonumber text dialog
 *
 * @param autotext Pointer to the state struct.
 */
void autonumber_set_state(AUTONUMBER_TEXT *autotext)
{
  GtkWidget *widget;
  GtkTreeModel *model;
  GList *el;
  /* Scope */

  /* Search text history */
  widget = lookup_widget(autotext->dialog, "scope_text");

  /* Simple way to clear the ComboBox. Owen from #gtk+ says: 
   *
   * Yeah, it's just slightly "shady" ... if you want to stick to fully 
   * advertised API, you need to remember how many rows you added and 
   * use gtk_combo_box_remove_text() */

  model = gtk_combo_box_get_model(GTK_COMBO_BOX(widget));
  gtk_list_store_clear(GTK_LIST_STORE(model));

  for (el= autotext->scope_text; el != NULL; el=g_list_next(el)) {
    gtk_combo_box_append_text(GTK_COMBO_BOX(widget), el->data);
  }

  widget = gtk_bin_get_child(GTK_BIN(widget));
  gtk_entry_set_text(GTK_ENTRY(widget), g_list_first(autotext->scope_text)->data);

  widget = lookup_widget(autotext->dialog, "scope_skip");
  gtk_combo_box_set_active(GTK_COMBO_BOX(widget),
			   autotext->scope_skip);

  widget = lookup_widget(autotext->dialog, "scope_number");
  gtk_combo_box_set_active(GTK_COMBO_BOX(widget), 
			   autotext->scope_number);

  widget = lookup_widget(autotext->dialog, "scope_overwrite");
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget), 
			       autotext->scope_overwrite);

  /* Options */
  widget = lookup_widget(autotext->dialog, "opt_startnum");
  gtk_spin_button_set_value(GTK_SPIN_BUTTON(widget), 
			    autotext->startnum);

  widget = lookup_widget(autotext->dialog, "sort_order");
  gtk_combo_box_set_active(GTK_COMBO_BOX(widget), autotext->order);

  widget = lookup_widget(autotext->dialog, "opt_removenum");
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget), 
			       autotext->removenum);

  widget = lookup_widget(autotext->dialog, "opt_slotting");
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(widget),
			       autotext->slotting);
}

/** @brief Get the settings from the autonumber text dialog
 *
 * Get the settings from the autonumber text dialog and store it in the
 * <B>AUTONUMBER_TEXT</B> structure.
 *
 * @param autotext Pointer to the state struct.
 */
void autonumber_get_state(AUTONUMBER_TEXT *autotext)
{
  GtkWidget *widget;
  gchar *text;

  /* Scope */

  /* Search text history */
  widget = lookup_widget(autotext->dialog, "scope_text");
  widget = gtk_bin_get_child(GTK_BIN(widget));
  text = g_strdup(gtk_entry_get_text( GTK_ENTRY(widget)));

  autotext->scope_text=autonumber_history_add(autotext->scope_text, text);

  widget = lookup_widget(autotext->dialog, "scope_skip");
  autotext->scope_skip = gtk_combo_box_get_active( GTK_COMBO_BOX(widget) );

  widget = lookup_widget(autotext->dialog, "scope_number");
  autotext->scope_number = gtk_combo_box_get_active(GTK_COMBO_BOX(widget) );

  widget = lookup_widget(autotext->dialog, "scope_overwrite");
  autotext->scope_overwrite = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget));

  /* Sort order */
  widget = lookup_widget(autotext->dialog, "sort_order");
  autotext->order= gtk_combo_box_get_active(GTK_COMBO_BOX(widget));

  /* Options */
  widget = lookup_widget(autotext->dialog, "opt_startnum");
  autotext->startnum=gtk_spin_button_get_value_as_int(GTK_SPIN_BUTTON(widget));

  widget = lookup_widget(autotext->dialog, "opt_removenum");
  autotext->removenum = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget));

  widget = lookup_widget(autotext->dialog, "opt_slotting");
  autotext->slotting = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget));
}

/* ***** CALLBACKS (functions that get called directly from the GTK) ******* */

/*! \brief response  callback for the autonumber text dialog
 *  \par Function Description
 *  The function just closes the dialog if the close button is pressed or the 
 *  user closes the dialog window.
 *  Triggering the apply button will call the autonumber action functions.
 */
void autonumber_text_response(GtkWidget * widget, gint response,
			      AUTONUMBER_TEXT *autotext)
{
  switch (response) {
  case GTK_RESPONSE_ACCEPT:
    autonumber_get_state(autotext);
    if (autotext->removenum == TRUE && autotext->scope_overwrite == FALSE) {
      /* temporarly set the overwrite flag */
      autotext->scope_overwrite = TRUE;
      autonumber_text_autonumber(autotext);
      autotext->scope_overwrite = FALSE;
    }
    else {
      autonumber_text_autonumber(autotext);
    }
    break;
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    gtk_widget_destroy(autotext->dialog);
    autotext->dialog = NULL;
    break;
  default:
    printf("ERROR: autonumber_text_response(): strange signal %d\n",response);
  }
}


/** @brief Callback that activates or deactivates "overwrite existing numbers" 
 * check box.
 *
 * This gets called each time "remove numbers" check box gets clicked.
 */
void autonumber_removenum_toggled(GtkWidget * opt_removenum, 
				  AUTONUMBER_TEXT *autotext)
{
  GtkWidget *scope_overwrite;

  scope_overwrite=lookup_widget(autotext->dialog, "scope_overwrite");

  /* toggle activity of scope overwrite with respect to removenum */
  if(gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(opt_removenum))) {
    gtk_widget_set_sensitive(scope_overwrite, 0);
  } else {
    gtk_widget_set_sensitive(scope_overwrite, 1);
  }
}


/* ***** DIALOG SET-UP ***************************************************** */

/** @brief Creates the autonumber text dialog.
 *
 * Dialog is not shown. No callbacks are registered. This is basically
 * unmodified code returned by Glade.
 *
 * Only modification was the following substitution:
 *
 * %s/create_pixmap (autonumber_text, "\(.*\)")/autonumber_create_pixmap("gschem-\1", w_current)/
 * 
 * and addition of the "w_current" parameter.
 *
 * @param w_current Pointer to the top level struct.
 * @return Pointer to the dialog window.
 */
GtkWidget* autonumber_create_dialog(GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *autonumber_text;
  GtkWidget *vbox1;
  GtkWidget *alignment1;
  GtkWidget *vbox3;
  GtkWidget *table1;
  GtkWidget *label4;
  GtkWidget *scope_text;
  GtkWidget *label8;
  GtkWidget *label6;
  GtkWidget *scope_number;
  GtkWidget *scope_skip;
  GtkWidget *scope_overwrite;
  GtkWidget *label1;
  GtkWidget *alignment3;
  GtkWidget *vbox4;
  GtkWidget *table3;
  GtkWidget *label12;
  GtkWidget *label13;
  GtkObject *opt_startnum_adj;
  GtkWidget *opt_startnum;
  GtkWidget *sort_order;
  GtkWidget *opt_removenum;
  GtkWidget *opt_slotting;
  GtkWidget *label3;


  autonumber_text = gschem_dialog_new_with_buttons(_("Autonumber text"),
                                                   GTK_WINDOW(w_current->main_window),
                                                   0, /* not modal */
                                                   "autonumber", w_current,
                                                   GTK_STOCK_CLOSE,
                                                   GTK_RESPONSE_REJECT,
                                                   GTK_STOCK_APPLY,
                                                   GTK_RESPONSE_ACCEPT,
                                                   NULL);
  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(autonumber_text),
					  GTK_RESPONSE_ACCEPT,
					  GTK_RESPONSE_REJECT,
					  -1);

  gtk_window_position (GTK_WINDOW (autonumber_text),
		       GTK_WIN_POS_MOUSE);
  
  gtk_container_border_width(GTK_CONTAINER(autonumber_text), 
			     DIALOG_BORDER_SPACING);
  vbox1 = GTK_DIALOG(autonumber_text)->vbox;
  gtk_box_set_spacing(GTK_BOX(vbox1), DIALOG_V_SPACING);

  /* scope section */
  label1 = gtk_label_new (_("<b>Scope</b>"));
  gtk_label_set_use_markup (GTK_LABEL (label1), TRUE);
  gtk_misc_set_alignment (GTK_MISC(label1), 0, 0);
  gtk_box_pack_start (GTK_BOX(vbox1), label1, TRUE, TRUE, 0);
  gtk_widget_show (label1);

  alignment1 = gtk_alignment_new (0, 0, 1, 1);
  gtk_widget_show (alignment1);
  gtk_box_pack_start (GTK_BOX (vbox1), alignment1, TRUE, TRUE, 0);
  gtk_alignment_set_padding (GTK_ALIGNMENT (alignment1), 
			     0, 0, DIALOG_INDENTATION, 0);

  vbox3 = gtk_vbox_new (FALSE, 0);
  gtk_widget_show (vbox3);
  gtk_container_add (GTK_CONTAINER (alignment1), vbox3);

  table1 = gtk_table_new (3, 2, FALSE);
  gtk_widget_show (table1);
  gtk_box_pack_start (GTK_BOX (vbox3), table1, TRUE, TRUE, 0);
  gtk_table_set_row_spacings (GTK_TABLE (table1), DIALOG_V_SPACING);
  gtk_table_set_col_spacings (GTK_TABLE (table1), DIALOG_H_SPACING);

  label4 = gtk_label_new (_("Search for:"));
  gtk_widget_show (label4);
  gtk_table_attach (GTK_TABLE (table1), label4, 0, 1, 0, 1,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  gtk_misc_set_alignment (GTK_MISC (label4), 0, 0.5);

  scope_text = gtk_combo_box_entry_new_text ();
  gtk_entry_set_activates_default(GTK_ENTRY(gtk_bin_get_child(GTK_BIN(scope_text))), TRUE);
  gtk_widget_show (scope_text);
  gtk_table_attach (GTK_TABLE (table1), scope_text, 1, 2, 0, 1,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (GTK_FILL), 0, 0);

  label8 = gtk_label_new (_("Autonumber text in:"));
  gtk_widget_show (label8);
  gtk_table_attach (GTK_TABLE (table1), label8, 0, 1, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  gtk_misc_set_alignment (GTK_MISC (label8), 0, 0.5);

  label6 = gtk_label_new (_("Skip numbers found in:"));
  gtk_widget_show (label6);
  gtk_table_attach (GTK_TABLE (table1), label6, 0, 1, 2, 3,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  gtk_misc_set_alignment (GTK_MISC (label6), 0, 0.5);

  scope_number = gtk_combo_box_new_text ();
  gtk_widget_show (scope_number);
  gtk_table_attach (GTK_TABLE (table1), scope_number, 1, 2, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (GTK_FILL), 0, 0);
  gtk_combo_box_append_text (GTK_COMBO_BOX (scope_number), _("Selected objects"));
  gtk_combo_box_append_text (GTK_COMBO_BOX (scope_number), _("Current page"));
  gtk_combo_box_append_text (GTK_COMBO_BOX (scope_number), _("Whole hierarchy"));

  scope_skip = gtk_combo_box_new_text ();
  gtk_widget_show (scope_skip);
  gtk_table_attach (GTK_TABLE (table1), scope_skip, 1, 2, 2, 3,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (GTK_FILL), 0, 0);
  gtk_combo_box_append_text (GTK_COMBO_BOX (scope_skip), _("Selected objects"));
  gtk_combo_box_append_text (GTK_COMBO_BOX (scope_skip), _("Current page"));
  gtk_combo_box_append_text (GTK_COMBO_BOX (scope_skip), _("Whole hierarchy"));

  scope_overwrite = gtk_check_button_new_with_mnemonic (_("Overwrite existing numbers"));
  gtk_widget_show (scope_overwrite);
  gtk_box_pack_start (GTK_BOX (vbox3), scope_overwrite, FALSE, FALSE, 6);

  /* Options section */
  label3 = gtk_label_new (_("<b>Options</b>"));
  gtk_label_set_use_markup (GTK_LABEL (label3), TRUE);
  gtk_misc_set_alignment(GTK_MISC(label3), 0, 0);
  gtk_widget_show (label3);
  gtk_box_pack_start(GTK_BOX(vbox1), label3, TRUE, TRUE, 0);

  alignment3 = gtk_alignment_new (0, 0, 1, 1);
  gtk_widget_show (alignment3);
  gtk_box_pack_start(GTK_BOX(vbox1), alignment3, TRUE, TRUE, 0);
  gtk_alignment_set_padding (GTK_ALIGNMENT (alignment3), 
			     0, 0, DIALOG_INDENTATION, 0);

  vbox4 = gtk_vbox_new (FALSE, 3);
  gtk_widget_show (vbox4);
  gtk_container_add (GTK_CONTAINER (alignment3), vbox4);

  table3 = gtk_table_new (2, 2, FALSE);
  gtk_widget_show (table3);
  gtk_box_pack_start (GTK_BOX (vbox4), table3, TRUE, TRUE, 0);
  gtk_table_set_row_spacings (GTK_TABLE (table3), DIALOG_V_SPACING);
  gtk_table_set_col_spacings (GTK_TABLE (table3), DIALOG_H_SPACING);

  label12 = gtk_label_new (_("Starting number:"));
  gtk_widget_show (label12);
  gtk_table_attach (GTK_TABLE (table3), label12, 0, 1, 0, 1,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  gtk_misc_set_alignment (GTK_MISC (label12), 0, 0.5);

  label13 = gtk_label_new (_("Sort order:"));
  gtk_widget_show (label13);
  gtk_table_attach (GTK_TABLE (table3), label13, 0, 1, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);
  gtk_misc_set_alignment (GTK_MISC (label13), 0, 0.5);

  opt_startnum_adj = gtk_adjustment_new (1, 0, 10000, 1, 10, 10);
  opt_startnum = gtk_spin_button_new (GTK_ADJUSTMENT (opt_startnum_adj), 1, 0);
  gtk_entry_set_activates_default(GTK_ENTRY(opt_startnum), TRUE);
  gtk_widget_show (opt_startnum);
  gtk_table_attach (GTK_TABLE (table3), opt_startnum, 1, 2, 0, 1,
                    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
                    (GtkAttachOptions) (0), 0, 0);

  sort_order = gtk_combo_box_new();
  gtk_widget_show (sort_order);
  gtk_table_attach (GTK_TABLE (table3), sort_order, 1, 2, 1, 2,
                    (GtkAttachOptions) (GTK_FILL),
                    (GtkAttachOptions) (GTK_FILL), 0, 0);

  opt_removenum = gtk_check_button_new_with_mnemonic (_("Remove numbers"));
  gtk_widget_show (opt_removenum);
  gtk_box_pack_start (GTK_BOX (vbox4), opt_removenum, FALSE, FALSE, 0);

  opt_slotting = gtk_check_button_new_with_mnemonic (_("Automatic slotting"));
  gtk_widget_show (opt_slotting);
  gtk_box_pack_start (GTK_BOX (vbox4), opt_slotting, FALSE, FALSE, 0);

  /* Store pointers to all widgets, for use by lookup_widget(). */
  GLADE_HOOKUP_OBJECT (autonumber_text, scope_text, "scope_text");
  GLADE_HOOKUP_OBJECT (autonumber_text, scope_number, "scope_number");
  GLADE_HOOKUP_OBJECT (autonumber_text, scope_skip, "scope_skip");
  GLADE_HOOKUP_OBJECT (autonumber_text, scope_overwrite, "scope_overwrite");
  GLADE_HOOKUP_OBJECT (autonumber_text, opt_startnum, "opt_startnum");
  GLADE_HOOKUP_OBJECT (autonumber_text, sort_order, "sort_order");
  GLADE_HOOKUP_OBJECT (autonumber_text, opt_removenum, "opt_removenum");
  GLADE_HOOKUP_OBJECT (autonumber_text, opt_slotting, "opt_slotting");

  return autonumber_text;
}

/*! \brief Create or restore the autonumber text dialog
 *
 *  If the function is called the first time the dialog is created.
 *  If the dialog is only in background it is moved to the foreground.
 *
 *  @param w_current Pointer to the top level struct
 */
void autonumber_text_dialog(GSCHEM_TOPLEVEL *w_current)
{
  static AUTONUMBER_TEXT *autotext = NULL;

  GtkWidget *opt_removenum = NULL;
  GtkWidget *sort_order = NULL;

  if(autotext == NULL) {
    /* first call of this function, init dialog structure */
    autotext=autonumber_init_state();
  }

  /* set the GSCHEM_TOPLEVEL always. Can it be changed between the calls??? */
  autotext->w_current = w_current;

  if(autotext->dialog == NULL) {
    /* Dialog is not currently displayed - create it */

    autotext->dialog = autonumber_create_dialog(w_current);

    opt_removenum = lookup_widget(autotext->dialog, "opt_removenum");
    sort_order = lookup_widget(autotext->dialog, "sort_order");

    autonumber_sortorder_create(w_current, sort_order);

    gtk_dialog_set_default_response (GTK_DIALOG (autotext->dialog), 
                                     GTK_RESPONSE_ACCEPT);

    gtk_signal_connect(GTK_OBJECT(autotext->dialog), "response",
		       GTK_SIGNAL_FUNC(autonumber_text_response),
		       autotext);

    gtk_signal_connect(GTK_OBJECT(opt_removenum),
		       "clicked",
		       GTK_SIGNAL_FUNC(autonumber_removenum_toggled),
		       autotext);

    autonumber_set_state(autotext);

    gtk_widget_show_all(autotext->dialog);
  }

  /* if the dialog is in the background or minimized: show it */
  gtk_window_present(GTK_WINDOW(autotext->dialog));
}
