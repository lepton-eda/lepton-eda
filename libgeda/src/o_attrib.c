/* gEDA - GNU Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998 Ales V. Hvezda
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
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>
#include <stdio.h>
#include <strings.h>
#include <math.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include "struct.h"
#include "defines.h"
#include "globals.h"
#include "s_passing.h"
#include "o_types.h"

#include "colors.h"

#include "../include/prototype.h"

#define DELIMITERS ",; "

/* No special type for attributes */
/* You can only edit text attributes */

/* be sure in o_copy o_move o_delete you maintain the attributes */
/* delete is a bare, because you will have to unattach the other end */
/* and in o_save o_read as well */
/* and in o_select when selecting objects, select the attributes */

/* there needs to be a modifier (in struct.h, such as a flag) which */
/* signifies that this is an attribute */

/* return pointer from attrib_list */
ATTRIB *
o_attrib_search(ATTRIB *list, OBJECT *item)
{
	ATTRIB *a_current;

	/* hack do this for all relavent routines! */	
	if (item == NULL) {
		return(NULL);
	}

	a_current = list;

	while(a_current != NULL) {
		if (a_current->object != NULL) {
			if (item->sid == a_current->object->sid) {	
				return(a_current);	
			}
		}

		a_current = a_current->next;
	}

	return(NULL);
}

ATTRIB *
o_attrib_return_tail(ATTRIB *head) 
{
	ATTRIB *a_current=NULL;
        ATTRIB *current=NULL;

        a_current = head;
        while ( a_current != NULL ) { /* goto end of list */
                current = a_current;
                a_current = a_current->next;
        }
        return(current); 
}

/* rename to be consistant */
ATTRIB *
add_attrib_head(OBJECT *parent)
{
	ATTRIB *head = NULL;

	head = (ATTRIB *) malloc(sizeof(ATTRIB));
	head->next = NULL;

	/* highly experimental hack */
	head->object = parent; 
	head->copied_to = NULL;
	head->prev = NULL;

	/* why the grief? well everywhere a attribute is refered to */
	/* you have to skip over the head, you really ought to robustify */
	/* all references to this object pointer when talking to attributes */
	/* hack of course I think this is okay now though */

	return(head);
}

/* attrib_list is the list where you want to add item to */
/* item is the item you want to add as an attribute */
ATTRIB *
o_attrib_add(TOPLEVEL *w_current, ATTRIB *list_head, OBJECT *item)
{
	ATTRIB *end = NULL;
	ATTRIB *new = NULL;

	/* get tail of list_head */
	end = o_attrib_return_tail(list_head);

	/* create an new st_attrib object */
	new = (ATTRIB *) malloc(sizeof(ATTRIB));

	/* fill item with correct data (mainly item) */
	new->next = NULL;
	new->prev = end;
	new->object = item;
	new->copied_to = NULL;
	new->object->attribute = 1; /* Set the attribute to true, hack define */
	/* Show that that item is an attribute */
	new->object->color = w_current->attribute_color; 
	/* this is assuming item is text */
	o_complex_set_color(w_current, new->object->color, new->object->complex);

	/* Add link from item to attrib listing */
	new->object->attached_to = new;

	/* set next of tail of end->attrib to item */
	if (end) {
		end->next = new;
		return(new);
	} else {
		return(new);
	}
}

/* this routine is not nice to next and prev */
/* this routine is only called from free_all */
void
o_attrib_free(TOPLEVEL *w_current, ATTRIB *current)
{
	if (current != NULL) {

		/* this makes me nervous... very nervous */
		if (current->object != NULL && current->prev != NULL) {
			current->object->attribute = 0;	
			current->object->color = w_current->detachedattr_color;	
			o_complex_set_color(w_current, current->object->color, 
					    current->object->complex);
			current->object->attached_to=NULL;
			o_redraw_single(w_current, current->object);
		}

		/* were do we detach the object->attached_to? above */
		current->object=NULL;

		free(current);

	}
}


/* IMPORTANT, lists first then specific single item */
/* object is the place where you want to add item as an attribute */
/* list are the actual attributes to be added */
/* parent_list is the list where the actual attribute objects live */
/* typically parent_list is object_parent (object_head), but it is */
/* overridden in o_complex_add so that it points to head node of the complex */
void
o_attrib_attach(TOPLEVEL *w_current, OBJECT *parent_list, OBJECT *list, OBJECT *object)
{
	OBJECT *real = NULL;
	OBJECT *o_current = NULL;

	/* unused in this code? yes... 
	OBJECT *temp2 = NULL;
	*/

	ATTRIB *found = NULL;
	OBJECT *found2 = NULL; /* object in main list */

        o_current = list; /* hack get consistant names */

	if (object->type == OBJ_NTEXT) {
	/* printf("Make sure first is NOT a text item"); */
	/* messages like this need to to a error location, like a log! */
	/* or maybe it's okay.. don't do anything if this happens */
		return;
		return;
	}

#if 0	
/* for now this is out... */
/*  I suppose you can attach anything to anything */
	while(temp2 != NULL) {
		if (temp->type != OBJ_NTEXT) {
			printf("Cannot attach non-text object as an attribute\n");
			return;
		}
		temp2 = temp2->next;
	}
#endif
	
	/* find the real object in the true object_list */
	real = (OBJECT *) o_list_search(parent_list, object);	
	/* check to make sure this is not null hack */

	if (real == NULL) {
		printf("ah.. object was not found in the parent list!\n");
		return;
	}

	while (o_current != NULL) {
		/* is the object already part of the list ? */
		found = o_attrib_search(real->attribs, o_current);
		if (!found) { /* no it's not, add it to the list */
		
			found2 = (OBJECT *) o_list_search(parent_list, o_current);	

			/* check to see if found2 is not null hack */

			if (real->attribs == NULL) 
				real->attribs = add_attrib_head(real);

			o_attrib_add(w_current, real->attribs, found2);

			/* can't do this here since just selecting something */
			/* will cause this to be set */
			/* w_current->page_current->CHANGED=1;*/
		
			/* Also set the selection's color as well */
			o_current->color = w_current->attribute_color; 
			o_complex_set_color(w_current, o_current->color, 
					    o_current->complex);

#if 0
if (real->attribs == NULL) 
real->attribs = o_attrib_add(real->attribs,found2);
else
o_attrib_add(real->attribs, found2);
#endif

		} else {
			printf("attribute already in list\n");
		}
		o_current = o_current->next;
	}
	
}

void
o_attrib_detach_test(TOPLEVEL *w_current, OBJECT *list, OBJECT *items) 
{

/* this all needs to be rethought out */
	/* loop over items till NULL */
		/* Search for item in object->attrib */
		/* o_attrib_search(list->attribs, current_item) */
			/* if found */
				/*call o_attrib_remove(object->attributes, current_item->attrib_struct);*/
			/* if not found */
				/* do nothing */

}

/* only can edit a text, special case of edit text */
void
o_attrib_edit(OBJECT *list, OBJECT *item)
{

}


/* should this be st_attrib or st_object? */
void
o_attrib_select_draw(ATTRIB *list)
{
	/* draw list */
		/* either white */
		/* or a white bounding box? */

}

/* should this be st_attrib or st_object? */
void
o_attrib_unselect_draw(ATTRIB *list)
{
	/* draw list */
		/* either white */
		/* or a white bounding box? */

}

/* this routine uses o_attrib_free (which isn't nice to next, prev) */
/* so it should only be used when an object is being destroyed */
/* goes backwards */
void
o_attrib_free_all(TOPLEVEL *w_current, ATTRIB *list)
{
        ATTRIB *a_current; 
	ATTRIB *a_next;

	a_current = list;

	while (a_current != NULL) {
		a_next = a_current->next;
		o_attrib_free(w_current, a_current);
                a_current = a_next;
       	}
}

void
o_attrib_print(ATTRIB *attributes) 
{
	ATTRIB *a_current;

	a_current = attributes;


	while (a_current != NULL) {
		printf("Attribute points to: %s\n", a_current->object->name);
		a_current = a_current->next;
	}
}

/* very hard */
/* I don't think we need it though */
ATTRIB * 
o_attrib_copy(ATTRIB *list)
{
	return(NULL);
}

/* this routine goes out and removes the current attribute, while */
/* preserving the next, prev pointers */
/* this routine should be used when detaching */
void
o_attrib_delete(ATTRIB *a_current)
{
	if (a_current != NULL) {

		if (a_current->next)
                        a_current->next->prev = a_current->prev;
                else
                        a_current->next = NULL;

                if (a_current->prev)
                        a_current->prev->next = a_current->next;
                else
                        a_current->prev = NULL;

		if (a_current->object) {
			a_current->object->attribute=0;
			a_current->object->attached_to=NULL;
		}
		a_current->object = NULL;

		free(a_current);
	}
}

/* this routine goes out and removes and attribute from a list */
/* it searches for the attribute and then removes it using the good */
/* o_attrib_delete() routine */
/* this routine is the detach_all routine */
/* it's not currently being used */
/* it's not even done */
void
o_attrib_remove(ATTRIB *list, ATTRIB *remove) 
{
	ATTRIB *a_current;

	a_current = list;

	while (a_current != NULL) {
	
		if (a_current == remove) {
			
		}	
		a_current = a_current->next;
	}
}

/* is this used? */
void
o_attrib_detach_all(TOPLEVEL *w_current, OBJECT *object_list, OBJECT *main_head)
{
	OBJECT *o_current=NULL;
        OBJECT *real=NULL;

        o_current = object_list;

        while(o_current != NULL) {

                real = (OBJECT *) o_list_search(main_head, o_current);

                if (real) {
                        if (real->attribs != NULL) {
                                o_attrib_free_all(w_current, real->attribs);
                                real->attribs = NULL; /* leak possible? */
				w_current->page_current->CHANGED=1;
                        }
                }
                o_current = o_current->next;
        }
}

OBJECT *
o_read_attribs(TOPLEVEL *w_current, FILE *fp, OBJECT *object_to_get_attribs, char *version)
{
	OBJECT *object_list=NULL;
	char buf[1024];
	char string[1024];
	char objtype;

	object_list = object_to_get_attribs;

	while ( fgets(buf, 1024, fp) != NULL) {

		sscanf(buf, "%c", &objtype);
		switch (objtype) {

			case(OBJ_LINE):
				object_list = (OBJECT *) o_line_read(w_current, object_list, buf, version);
			break;


			case(OBJ_NET):
				object_list = (OBJECT *) o_net_read(w_current, object_list, buf, version);
			break;

			case(OBJ_BOX):
				object_list = (OBJECT *) o_box_read(w_current, object_list, buf, version);
			break;
		
			case(OBJ_CIRCLE):
				object_list = (OBJECT *) o_circle_read(w_current, object_list, buf, version);
			break;

			case(OBJ_COMPLEX):
			
				object_list = (OBJECT *) o_complex_read(w_current, object_list, buf, version);

				/* this is necessary because complex may add
				   attributes which float */
				/* still needed? */
				object_list = (OBJECT *) return_tail(object_list);
			break;

			case(OBJ_NTEXT):
				fgets(string, 1024, fp); /* check if invalid */
				object_list = (OBJECT *) o_ntext_read(w_current, object_list, buf, string, version);
			break;

			case(OBJ_PIN):
				object_list = (OBJECT *) o_pin_read(w_current, object_list, buf, version);
			break;

			case(OBJ_ARC):
				object_list = (OBJECT *) o_arc_read(w_current, object_list, buf, version);
			break;

			case(ENDATTACH_ATTR): 
				return(object_list);
			break;	

		}

		o_attrib_attach(w_current, w_current->page_current->object_parent, 
			object_list, object_to_get_attribs);
	}
	return(object_list);
}

void
o_save_attribs(FILE *fp, ATTRIB *attribs)
{
	ATTRIB *a_current=NULL;
	OBJECT *o_current=NULL;
	char buf[200];
	char *out;

	a_current = attribs;

	fprintf(fp, "{\n");
	
	while ( a_current != NULL ) {

		o_current = a_current->object;	

		if (o_current->type != OBJ_HEAD) {

			switch (o_current->type) {

				case(OBJ_LINE):
					out = (char *) o_line_save(buf, o_current);
				break;

				case(OBJ_NET):
					out = (char *) o_net_save(buf, o_current);
				break;

				case(OBJ_BOX):
					out = (char *) o_box_save(buf, o_current);
				break;
		
				case(OBJ_CIRCLE):
					out = (char *) o_circle_save(buf, o_current);
				break;

				case(OBJ_COMPLEX):
					out = (char *) o_complex_save(buf, o_current);
				break;

				case(OBJ_NTEXT):
					out = (char *) o_ntext_save(buf, o_current);
				break;

				case(OBJ_PIN):
					out = (char *) o_pin_save(buf, o_current);
				break;

				case(OBJ_ARC):
					out = (char *) o_arc_save(buf, o_current);
				break;

				default:
					fprintf(stderr, "Error type!\n");
					exit(-1);
				break;
			}
			/* output the line */
			fprintf(fp, "%s\n", out);
		}
	a_current = a_current->next;
	} 

	fprintf(fp, "}\n");
}

/* non-zero if has an equals in it, hence it's a name=value attribute */
/* both name and value must be pre allocated */
int
o_attrib_get_name_value(char *string, char *name, char *value )
{
	int i=0;
	int j=0;
	int attribute_found=0;

	name[0] = '\0';
	value[0] = '\0';

	/* get the name */
	while(string[i] != '\0' && !attribute_found) {
		if (string[i] == '=') {
			attribute_found = 1;	
		} else {
			name[i] = string[i];
			i++;
		}
	}

	if (!attribute_found) {
		return(0);
	}
	
	name[i] = '\0';
	i++; /* skip over the ='s */

	while(string[i] != '\0') {
			value[j] = string[i];
			i++;
			j++;
	}

	value[j] = '\0';

	return(attribute_found);
}

void
o_attrib_free_current(TOPLEVEL *w_current)
{
	if (w_current->current_attribute) {
		free(w_current->current_attribute);
	}
	w_current->current_attribute=NULL;
}

/* flag will take on any value which show_name_value takes */
/* SHOW_NAME_VALUE, SHOW_VALUE, SHOW_NAME */
void
o_attrib_set_show(TOPLEVEL *w_current, int flag)
{
	w_current->current_show = flag;
}

/* flag will be VISIBLE or INVISIBLE */
void
o_attrib_set_visible(TOPLEVEL *w_current, int flag)
{
	w_current->current_visible = flag;
}

void
o_attrib_set_string(TOPLEVEL *w_current, char *string)
{
	int len;

	/* need to put an error messages here */
	if (string == NULL)  {
		fprintf(stderr, "error! string in set_string was NULL\n");
		return;
	}

	if (w_current->current_attribute != NULL) {
		free(w_current->current_attribute);
		w_current->current_attribute=NULL;
	}

	len = strlen(string);

	w_current->current_attribute = (char *) 
			malloc(sizeof(char)*len+1);

	strcpy(w_current->current_attribute,string);
	
	/* be sure to free this string somewhere and free the input string */
}

/* it is the responsibility of the caller to free the returned string */
/* returned string is the value */
/* counter is the nth occurance of the attribute, starts from ZERO! */
/* zero being the first occurance */
/* the list is the top level list... don't pass it an object_head list */
/* be sure caller free's return value */
char *
o_attrib_search_name(OBJECT *list, char *name, int counter) 
{
	OBJECT *o_current;
	ATTRIB *a_current;
	OBJECT *found;
	int val;
	int internal_counter=0;
	char found_name[128]; /* limit hack */
	char found_value[128];
	char *return_string;

	o_current = list;

	while(o_current != NULL) {
	   if (o_current->attribs != NULL) {
		a_current = o_current->attribs;

		while(a_current != NULL) {
			found = a_current->object;
			if (found != NULL && found->text_string) {
				val = o_attrib_get_name_value(
					found->text_string, 
					found_name, found_value);

				if (val) {
				   if (strcmp(name, found_name) == 0) {
					if (counter != internal_counter) {
						internal_counter++;	
					} else {
					   return_string = (char *) 
							malloc(
			     				 sizeof(char)*
							 strlen(found_value)+1);
					  strcpy(return_string, found_value);
					  return(return_string);
					}
				   }
				}	

#if DEBUG 
				printf("0 _%s_\n", found->text_string);
				printf("1 _%s_\n", found_name);
				printf("2 _%s_\n", found_value);
#endif
			}
			a_current=a_current->next;
		}	
	  }

	/* search for attributes outside */

		if (o_current->type == OBJ_NTEXT) {
		 	val = o_attrib_get_name_value(
					o_current->text_string, 
					found_name, found_value);
			if (val) {
			   if (strcmp(name, found_name) == 0) {
				if (counter != internal_counter) {
					internal_counter++;	
				} else {
				   return_string = (char *) 
						malloc(
			     			 sizeof(char)*
						 strlen(found_value)+1);
				  strcpy(return_string, found_value);
				  return(return_string);
				}
			   }
			}	
		}

	  o_current=o_current->next;
	}
	
	return (NULL);
} 

/* it is the responsibility of the caller to free the returned string */
/* returned string is the value */
/* counter is the nth occurance of the attribute, starts from ZERO! */
/* zero being the first occurance */
/* the list is the top level list... don't pass it an object_head list */
/* be sure caller free's return value */
/* this needs a better name, but it's a special function of above which */
/* returns the object where the attribute lives */
char *
o_attrib_search_name2(OBJECT *list, char *name, OBJECT **return_found) 
{
	OBJECT *o_current;
	ATTRIB *a_current;
	OBJECT *found;
	int val;
	char found_name[128]; /* limit hack */
	char found_value[128];
	char *return_string;

	o_current = list;

	while(o_current != NULL) {
	   if (o_current->attribs != NULL) {
		a_current = o_current->attribs;

		while(a_current != NULL) {
			found = a_current->object;
			if (found != NULL && found->text_string) {
				val = o_attrib_get_name_value(
					found->text_string, 
					found_name, found_value);

				if (val) {
				   if (strcmp(name, found_name) == 0) {
					   return_string = (char *) 
							malloc(
			     				 sizeof(char)*
							 strlen(found_value)+1);
					  strcpy(return_string, found_value);
					  if (return_found) {
					  	*return_found = found;
					  }
					  return(return_string);
				   }
				}	

#if DEBUG 
				printf("0 _%s_\n", found->text_string);
				printf("1 _%s_\n", found_name);
				printf("2 _%s_\n", found_value);
#endif
			}
			a_current=a_current->next;
		}	
	  }

	/* search for attributes outside */

		if (o_current->type == OBJ_NTEXT) {
		 	val = o_attrib_get_name_value(
					o_current->text_string, 
					found_name, found_value);
			if (val) {
			   if (strcmp(name, found_name) == 0) {
				   return_string = (char *) 
						malloc(
			     			 sizeof(char)*
						 strlen(found_value)+1);
				  strcpy(return_string, found_value);
				  if (return_found) {
					*return_found = o_current;
				  return(return_string);
				}
			   }
			}	
		}

	  o_current=o_current->next;
	}
	
	return (NULL);
} 

/* it is the responsibility of the caller to free the returned string */
/* returned string is the value */
/* counter is the nth occurance of the attribute, starts from ZERO! */
/* zero being the first occurance */
/* be sure caller free's return value */
char *
o_attrib_search_name_partial(OBJECT *object, char *name, int counter) 
{
	OBJECT *o_current;
	ATTRIB *a_current;
	OBJECT *found;
	int val;
	int internal_counter=0;
	char found_name[128]; /* limit hack */
	char found_value[128];
	char *return_string;

	o_current = object;

	if (o_current->attribs != NULL) {
		a_current = o_current->attribs;

		while(a_current != NULL) {
			found = a_current->object;
			if (found != NULL && found->text_string) {
				val = o_attrib_get_name_value(
					found->text_string, 
					found_name, found_value);

				if (val) {
				   if (strstr(found_name, name)) {
					if (counter != internal_counter) {
						internal_counter++;	
					} else {
					   return_string = (char *) 
							malloc(
			     				 sizeof(char)*
							 strlen(found_value)+1);
					  strcpy(return_string, found_value);
					  return(return_string);
					}
				   }
				}	

#if DEBUG 
				printf("0 _%s_\n", found->text_string);
				printf("1 _%s_\n", found_name);
				printf("2 _%s_\n", found_value);
#endif
			}
			a_current=a_current->next;
		}	

	/* search for attributes outside */

		if (o_current->type == OBJ_NTEXT) {
		 	val = o_attrib_get_name_value(
					o_current->text_string, 
					found_name, found_value);
			if (val) {
			   if (strstr(found_name, name)) {
				if (counter != internal_counter) {
					internal_counter++;	
				} else {
				   return_string = (char *) 
						malloc(
			     			 sizeof(char)*
						 strlen(found_value)+1);
				  strcpy(return_string, found_value);
				  return(return_string);
				}
			   }
			}	
		}
	  }
	
	return (NULL);
} 

/* this function search for the counter'th occurance of the string attribute */
/* be sure caller free's return value */
/* this routine should NOT be used anywhere */
OBJECT *
o_attrib_search_attrib(OBJECT *list, char *attribute, int counter) 
{
	OBJECT *o_current;
	ATTRIB *a_current;
	OBJECT *found;
	int internal_counter=0;

	o_current = list;

	while(o_current != NULL) {
	   if (o_current->attribs != NULL) {
		a_current = o_current->attribs;

		while(a_current != NULL) {
			found = a_current->object;
			if (found != NULL && found->text_string) {

			       if (strcmp(attribute, found->text_string) == 0) {
					if (counter != internal_counter) {
						internal_counter++;	
					} else {
					  return(found);
					}
			   	}

#if DEBUG
				printf("0 _%s_\n", found->text_string);
				printf("1 _%s_\n", found_name);
				printf("2 _%s_\n", found_value);
#endif
			}
			a_current=a_current->next;
		}	
	  }

		/* search for the attribute outside here... */
	/* I don't think I have to do this here,  since this routine isn't
	   used */

	  o_current=o_current->next;
	}
	
	return (NULL);
} 


OBJECT *
o_attrib_return_parent(ATTRIB *attribute) 
{
	ATTRIB *a_current;

	a_current = attribute;

	while (a_current->prev != NULL) {
		a_current = a_current->prev;	
	}	

	/* should be pointing to the parent */
	
	return(a_current->object);	
}

ATTRIB *
o_attrib_copy_all(TOPLEVEL *w_current, OBJECT *attached_to, ATTRIB *attributes) 
{
	ATTRIB *a_current=NULL;
	ATTRIB *a_head=NULL;
	ATTRIB *a_new=NULL;
	ATTRIB *a_prev=NULL;

	a_current = attributes;

	while (a_current != NULL) {
	
		a_new = (ATTRIB *) malloc(sizeof(ATTRIB));
	
		/* in the case of the head attrib node, object points to 
		 * the parent which the attributes are attached to */	
		if (a_head == NULL) {
			a_new->object = attached_to;
		} else {
			a_new->object = a_current->object;  
		}
		

		
		/* object is not null and a_start is not null (ie we are not 
		 * messing with the head attrib node) 
		 */
		if (a_new->object && a_head != NULL)  {
			a_new->object->attached_to = a_new;
		}

		a_new->copied_to = a_current->copied_to;  

		a_new->prev = a_prev; 
	
		/* set previous's next pointer */
		/* if it's null that means we are at the first attrib */
		if (a_prev) {
			a_prev->next = a_new;
		} else {
			a_head = a_new;
		}
	
		a_new->next = NULL;
		a_prev = a_new;
		a_current = a_current->next;	
	}	

	/* should be pointing to the head node */
	return(a_head);	
}

void
o_attrib_reattach(ATTRIB *attributes) 
{
	ATTRIB *a_current=NULL;

	a_current = attributes;
	
	/* skip over head node */
	if (a_current)
		a_current = a_current->next;

	while (a_current != NULL) {
		if (a_current->object)  {
			a_current->object->attached_to = a_current;
			a_current->object->attribute = 1;
		}
		a_current = a_current->next;	
	}
}


/* sets all attribute objects to the right color (attribute_color) */
void
o_attrib_set_color(TOPLEVEL *w_current, ATTRIB *attributes) 
{
	ATTRIB *a_current;

	a_current = attributes;

	/* skip over head */
	if (a_current) 
		a_current = a_current->next;

	while (a_current != NULL) {

		if (a_current->object) {	
			
			if (a_current->object->type == OBJ_NTEXT &&
			     a_current->object->complex) {
				o_complex_set_color(w_current, 
					w_current->attribute_color, 
					a_current->object->complex);	
			        a_current->object->color = 
					 w_current->attribute_color;
			}	

		a_current = a_current->next;	
		}
	}
}

/* be sure caller free's return value */
char *
o_attrib_search_special(OBJECT *o_current) 
{
	char *return_value;

	return_value = o_attrib_search_name(o_current->complex, "gnd", 0);

	if (return_value) {
		return(return_value);
	}

	return_value = o_attrib_search_name(o_current->complex, "vdd", 0);

	if (return_value) {
		return(return_value);
	}

	return(NULL);
}


/* be sure caller free's return value */
char *
o_attrib_search_name_single(OBJECT *object, char *name, OBJECT **return_found) 
{
	OBJECT *o_current;
	ATTRIB *a_current;
	OBJECT *found;
	int val;
	char found_name[128]; /* limit hack */
	char found_value[128];
	char *return_string;

	o_current = object;

	if (o_current->attribs != NULL) {
		a_current = o_current->attribs;

		while(a_current != NULL) {
			found = a_current->object;
			if (found != NULL && found->text_string) {
				val = o_attrib_get_name_value(
					found->text_string, 
					found_name, found_value);

				if (val) {
				   if (strcmp(name, found_name) == 0) {
					   return_string = (char *) 
							malloc(
			     				 sizeof(char)*
							 strlen(found_value)+1);
					  strcpy(return_string, found_value);
					  if (return_found) {
					  	*return_found = found;
					  }
					  return(return_string);
				   }
				}	

#if DEBUG 
				printf("0 _%s_\n", found->text_string);
				printf("1 _%s_\n", found_name);
				printf("2 _%s_\n", found_value);
#endif
			}
			a_current=a_current->next;
		}	

	}
	/* search for attributes outside */

	if (o_current->type == OBJ_NTEXT) {
	 	val = o_attrib_get_name_value(o_current->text_string, 
					found_name, found_value);

		if (val) {
		   if (strcmp(name, found_name) == 0) {
			   return_string = (char *) 
					malloc(
		     			 sizeof(char)*
					 strlen(found_value)+1);
			  strcpy(return_string, found_value);
			  if (return_found) {
			  	*return_found = found;
			  }
			  return(return_string);
			}
		}	
	}

	return (NULL);
} 

/* be sure caller frees string */
char *
o_attrib_search_slot(OBJECT *object, OBJECT **return_found)
{
	char *return_value;

	/* search for default value attribute buried inside the complex */
	return_value = o_attrib_search_name_single(object, "slot", return_found);

	/* I'm confused here does the next if get ever called? */
	if (return_value) {
		return(return_value);
	}

	if (return_found) {
		*return_found = NULL;
	}
	return(NULL);
}

/* be sure caller frees string */
char *
o_attrib_search_numslots(OBJECT *object, OBJECT **return_found)
{
	char *return_value;

	/* search for numslots attribute buried inside the complex */
	return_value = o_attrib_search_name(object->complex, "numslots", 0);

	if (return_value) {
		return(return_value);
	}

	if (return_found) {
		*return_found = NULL;
	}
	return(NULL);
}

/* be sure caller frees string */
char *
o_attrib_search_default_slot(OBJECT *object)
{
	char *return_value;

	/* search for default value attribute buried inside the complex */
	return_value = o_attrib_search_name(object->complex, "slot", 0);

	if (return_value) {
		return(return_value);
	}

	return(NULL);
}

char *
o_attrib_search_pin_number(OBJECT *object, int pin_number, 
	OBJECT **return_found)
{
	char *return_value;
	char *search_for;

	/* The 9 is the number of digits plus null */
	search_for = (char *) malloc(sizeof(char)*(strlen("slot")+9));

	sprintf(search_for, "pin%d", pin_number);

#if DEBUG
	printf("searching for %s\n", search_for);
#endif

	/* search for default value attribute buried inside the complex */
	/* using special name2 function which returns return_found */
	return_value = o_attrib_search_name2(object, search_for, return_found);
	free(search_for);

	if (return_value) {
		return(return_value);
	}

	if (return_found) {
		*return_found = NULL;
	}

	return(NULL);
}

/* be sure caller frees string */
char *
o_attrib_search_slot_number(OBJECT *object, int slotnumber)
{
	char *return_value;
	char *search_for;

	/* The 9 is the number of digits plus null */
	search_for = (char *) malloc(sizeof(char)*(strlen("slot")+9));

	sprintf(search_for, "slot%d", slotnumber);

#if DEBUG
	printf("slotnumber: _%s_\n", search_for); 
#endif
	/* search for default value attribute buried inside the complex */
	return_value = o_attrib_search_name(object->complex, search_for, 0);
	free(search_for);

	if (return_value) {
		return(return_value);
	}

	return(NULL);
}



void
o_attrib_slot_update(TOPLEVEL *w_current, OBJECT *object)
{
	OBJECT *o_current;
	OBJECT *o_slot_attrib;
	OBJECT *o_pin_attrib;
	char *string;
	char *slot_num;
	int slot;
	int pin_counter;
	char* current_pin;
	int current_pin_int;

	o_current = object;
	
	string = o_attrib_search_slot(o_current, &o_slot_attrib);
	
	if (!string) {
#if DEBUG 
		printf("didn't find slot=\n");
#endif
		return;
	} 
	

#if DEBUG 
	printf("SLOT attrib found!\n");
#endif

	slot = atoi(string);
	free(string);
#if DEBUG
	printf("slot is %d\n", slot);
#endif

	slot_num = o_attrib_search_slot_number(o_current, slot);

	if (!slot_num) {
#if DEBUG
		printf("didn't find slot#=\n");
#endif
		return;
	}

#if DEBUG
	printf("slot%d = _%s_\n", slot, string);	
#endif
	
	pin_counter = 1;
	current_pin = strtok(slot_num, DELIMITERS);
	while(current_pin != NULL) {

		string = o_attrib_search_pin_number(o_current->complex, pin_counter, &o_pin_attrib);

		if (string) {

			/* don't care about the value */
			free(string);

			if (o_pin_attrib->text_string) {
				free(o_pin_attrib->text_string);	
			}

			/* current_pin_int is the new pin value assigned */	
			current_pin_int = atoi(current_pin);	

			/* 18 is the size of two numbers plus null character */
			o_pin_attrib->text_string = (char *) malloc(
				sizeof(char)*(strlen("pin=")+18));

			sprintf(o_pin_attrib->text_string, "pin%d=%d", 
					pin_counter, current_pin_int); 

			o_ntext_recreate(w_current, o_pin_attrib);

#if DEBUG
			printf("full object string %s\n", o_pin_attrib->text_string);
#endif
			pin_counter++;
		} else {
			printf("component missing pin# attribute\n");
		}

		current_pin = strtok(NULL, DELIMITERS);
	} 

	free(slot_num);
}

void
o_attrib_slot_copy(TOPLEVEL *w_current, OBJECT *original, OBJECT *target)
{
	OBJECT *o_slot_attrib;
	OBJECT *o_pin_attrib;
	char *string;
	char *slot_num;
	int slot;
	int pin_counter;
	char* current_pin;
	int current_pin_int;

	
	string = o_attrib_search_slot(original, &o_slot_attrib);
	
	if (!string) {
#if DEBUG
		printf("didn't find slot=\n");
#endif
		return;
	} 
	
	slot = atoi(string);
	free(string);

#if DEBUG	
	printf("SLOT attrib found!\n");
#endif
#if DEBUG
	printf("slot is %d\n", slot);
#endif

	slot_num = o_attrib_search_slot_number(original, slot);

	if (!slot_num) {
#if DEBUG
		printf("didn't find slot#=\n");
#endif
		return;
	}

#if DEBUG
	printf("slot%d = _%s_\n", slot, string);	
#endif
	
	pin_counter = 1;
	current_pin = strtok(slot_num, DELIMITERS);
	while(current_pin != NULL) {

		string = o_attrib_search_pin_number(target->complex, pin_counter, &o_pin_attrib);

		if (string) {

			/* don't care about the value */
			free(string);

			if (o_pin_attrib->text_string) {
				free(o_pin_attrib->text_string);	
			}

			/* current_pin_int is the new pin value assigned */	
			current_pin_int = atoi(current_pin);	

			/* 18 is the size of two numbers plus null character */
			o_pin_attrib->text_string = (char *) malloc(
				sizeof(char)*(strlen("pin=")+18));

			sprintf(o_pin_attrib->text_string, "pin%d=%d", 
					pin_counter, current_pin_int); 

			o_ntext_recreate(w_current, o_pin_attrib);

#if DEBUG 
			printf("full object string %s\n", o_pin_attrib->text_string);
#endif
			pin_counter++;
		} else {
			printf("component missing pin# attribute\n");
		}

		current_pin = strtok(NULL, DELIMITERS);
	} 
	free(slot_num);
}
