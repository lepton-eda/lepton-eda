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
#include <string.h>
#include <math.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include "defines.h"
#include "struct.h"
#include "globals.h"
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
/* signifies that this is an attribute (really? why?) */

/* return pointer from attrib_list */

/* Martin Benes' auto uref renumber code */
void o_attrib_update_urefBM (TOPLEVEL *w_current, OBJECT *o_current) {
  OBJECT *list_head,*obj;
  char *uref;
  int i=-1,name_conflict,len;
  char *index_list;
  int index_list_len=1;

  if (strncmp(o_current->text->string,"uref=",5))
    return;

  len=strlen(o_current->text->string);
  uref=malloc(len+10);
  strcpy(uref,o_current->text->string);

  while (o_current->text->string[len-1]<='9' && 
	 o_current->text->string[len-1]>='0')
    --len;

  list_head=return_head(o_current);
  for (obj=list_head->next;obj;obj=obj->next) {
    if (obj->type==OBJ_TEXT && obj->attribute)
      ++index_list_len;
  }

  index_list=calloc(index_list_len,1);
  name_conflict=0;

  for (obj=list_head->next;obj;obj=obj->next) {
    if (obj->type==OBJ_TEXT && obj->attribute && obj!=o_current) {
      if (strncmp(uref,obj->text->string,len)==0) {
	if (strcmp(uref+len,obj->text->string+len)==0) {
	  name_conflict=1;
	}
	i=atoi(obj->text->string+len);
	if (i<index_list_len)
	  index_list[i]=1;
      }
    }
  }

  if (name_conflict) {
    for (i=0;index_list[i];++i);
    sprintf(uref+len,"%d", i);
    free(o_current->text->string);
    o_current->text->string=uref;
    o_text_recreate(w_current, o_current);
  }

  free(index_list);
}

ATTRIB *
o_attrib_search(ATTRIB *list, OBJECT *item)
{
	ATTRIB *a_current;

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

	if (new->object->type == OBJ_TEXT) {
		o_complex_set_color(new->object->text->prim_objs,
		                    new->object->color);
	} else if (new->object->type == OBJ_COMPLEX) {
		o_complex_set_color(new->object->complex->prim_objs,
		                    new->object->color);
	}

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
			current->object->attached_to=NULL;
			current->object->color = w_current->detachedattr_color;	

			if (current->object->type == OBJ_TEXT) {
				o_complex_set_color(current->object->text->prim_objs, 
						    current->object->color);
			} else {
				printf("Tried to set the color on a complex!\nlibgeda/src/o_attrib_free 1\n");
			}

			/* not sure on this */
			if (current->object->saved_color != -1) {
				if (current->object->type == OBJ_TEXT) {
					o_complex_set_saved_color_only(
						current->object->text->prim_objs, 
						w_current->detachedattr_color);
				} else {
					printf("Tried to set the color on a complex!\nlibgeda/src/o_attrib_free 2\n");
				}
				current->object->saved_color = w_current->
							detachedattr_color;
			}
		}

		/* were do we detach the object->attached_to? above */
		current->object=NULL;

		free(current);

	}
}


/* IMPORTANT, lists first then specific single item */
/* object is the place where you want to add item as an attribute */
/* text_object is the actual attribute to be added */
/* parent_list is the list where the actual attribute objects live */
/* typically parent_list is object_parent (object_head), but it is */
/* overridden in o_complex_add so that it points to head node of the complex */
void
o_attrib_attach(TOPLEVEL *w_current, OBJECT *parent_list, 
		OBJECT *text_object, OBJECT *object)
{
	OBJECT *o_current = NULL;

	ATTRIB *found = NULL;
	OBJECT *found2 = NULL; /* object in main list */

        o_current = text_object; 

	if (object == NULL) {
		printf("ah.. object was not found in the parent list!\n");
		return;
	}

	/* is the object already part of the list ? */
	found = o_attrib_search(object->attribs, o_current);
	if (!found) { /* no it's not, add it to the list */
		
		found2 = (OBJECT *) o_list_search(parent_list, o_current);	

		/* check to see if found2 is not null hack */
		if (found2) {
			if (found2->type == OBJ_TEXT) {

				if (object->attribs == NULL) {
					object->attribs = 
						add_attrib_head(object);
				}


				if (found2->attached_to) {
					fprintf(stderr, "You cannot attach this attribute [%s] to more than one object\n", found2->text->string);
				} else {

					o_attrib_add(w_current, 
				                     object->attribs, 
						     found2);

					o_current->color = w_current->
							   attribute_color; 

					o_complex_set_color(
						     o_current->text->prim_objs,
						     o_current->color);

					if (o_current->saved_color != -1) {
						o_complex_set_saved_color_only(
						      o_current->text->prim_objs, 
						      o_current->color);
						o_current->saved_color = 
							o_current->color;
					}
/* can't do this here since just selecting something */
/* will cause this to be set */
/* w_current->page_current->CHANGED=1;*/

#ifdef MARTIN_BENES
					o_attrib_update_urefBM (w_current, o_current);
#endif
				}
			} else {
				fprintf(stderr, "You cannot attach non text items as attributes!\n");
			}	
		}
	} else {
		if (o_current->text->string) { 	
			printf("Attribute [%s] already attached\n", 
				o_current->text->string);
		}
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
#if 0 /* not used */
	OBJECT *o_current=NULL;

        o_current = object_list;

        while(o_current != NULL) {

                X = (OBJECT *) o_list_search(main_head, o_current);

                if (X) {
                        if (X->attribs != NULL) {
                                o_attrib_free_all(w_current, X->attribs);
                                X->attribs = NULL; /* leak possible? */
				w_current->page_current->CHANGED=1;
                        }
                }
                o_current = o_current->next;
        }
#endif
}

OBJECT *
o_read_attribs(TOPLEVEL *w_current, FILE *fp, OBJECT *object_to_get_attribs, char *version)
{
	OBJECT *object_list=NULL;
	char buf[1024];
	char string[1024];
	char objtype;
	int ATTACH=FALSE;
	int saved_color=-1;

	object_list = object_to_get_attribs;

	while ( fgets(buf, 1024, fp) != NULL) {

		sscanf(buf, "%c", &objtype);
		switch (objtype) {

			case(OBJ_LINE):
				object_list = (OBJECT *) o_line_read(w_current, 
								object_list,
								buf, 
								version);
			break;


			case(OBJ_NET):
				object_list = (OBJECT *) o_net_read(w_current, 
								object_list, 
								buf, 
								version);
			break;

			case(OBJ_BUS):
				object_list = (OBJECT *) o_bus_read(w_current, 
								object_list, 
								buf, 
								version);
			break;

			case(OBJ_BOX):
				object_list = (OBJECT *) o_box_read(w_current, 
								object_list, 
								buf, 
								version);
			break;
		
			case(OBJ_CIRCLE):
				object_list = (OBJECT *) o_circle_read(
								w_current, 
								object_list, 
								buf, 
								version);
			break;

			case(OBJ_COMPLEX):
			
				object_list = (OBJECT *) o_complex_read(
								w_current, 
								object_list, 
								buf, 
								version);

				/* this is necessary because complex may add
				   attributes which float */
				/* still needed? */
				object_list = (OBJECT *) return_tail(
								  object_list);
			break;

			case(OBJ_PIN):
				object_list = (OBJECT *) o_pin_read(w_current, 
								object_list, 
								buf, 
								version);
			break;

			case(OBJ_ARC):
				object_list = (OBJECT *) o_arc_read(w_current, 
								object_list, 
								buf, 
								version);
			break;

			case(OBJ_TEXT):
				fgets(string, 1024, fp); /* check if invalid */
				object_list = (OBJECT *) o_text_read(w_current,
					                        object_list, 
								buf, 
								string, 
								version);
				saved_color = object_list->color;
				ATTACH=TRUE;
		
			break;

			case(ENDATTACH_ATTR): 
				return(object_list);
			break;	

		}

		if (ATTACH) {
			o_attrib_attach(w_current, 
					w_current->page_current->object_parent, 
					object_list, object_to_get_attribs);
			/* check color to set it to the right value */
			if (object_list->color != saved_color) {
				object_list->color = saved_color;

				if (object_list->type == OBJ_TEXT) {	
					o_complex_set_color(
						object_list->text->prim_objs,
                                                object_list->color);
				} else {
					printf("Tried to set the color on a complex in libgeda/src/o_read_attribs\n");
				}
			}
			ATTACH=FALSE;
		} else {
			fprintf(stderr, "Tried to attach a non-text item as an attribute\n");
		}
	}
	return(object_list);
}

/* this should be trimmed down to only save attributes which are text items */
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

				case(OBJ_BUS):
					out = (char *) o_bus_save(buf, o_current);
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

				case(OBJ_TEXT):
					out = (char *) o_text_save(buf, o_current);
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
/* And if you get an invalid attribute (improper) with a name and no */
/* value, then it is NOT an attribute */
/* Also, there cannot be any spaces beside the equals sign */
int
o_attrib_get_name_value(char *string, char *name, char *value )
{
	int i=0;
	int j=0;
	int attribute_found=FALSE;
	char *equal_ptr;

	name[0] = '\0';
	value[0] = '\0';

	/* get the name */
	while(string[i] != '\0' && !attribute_found) {
		if (string[i] == '=') {
			attribute_found = TRUE;	
		} else {
			name[i] = string[i];
			i++;
		}
	}

	if (!attribute_found) {
		return(FALSE);
	}

	/* make sure there are no spaces in between equals */
 	equal_ptr = strchr(string, '=');
	if ( (*(equal_ptr + 1) == ' ') || (*(equal_ptr - 1) == ' ') ) {
#if DEBUG /* sometimes you have text with an ='s in it, it shouldn't be */
	  /* treated like an attribute */
		s_log_message("Found attrib/text with spaces beside the ='s [%s]\n", string);
		s_log_message("You can ignore the above message if the text is not intended to be an attribute\n");
		fprintf(stderr, "Found an attribute with spaces beside the ='s [%s]\n", string);
#endif
		return(FALSE);
	}

	name[i] = '\0';
	i++; /* skip over the ='s */

	while(string[i] != '\0') {
			value[j] = string[i];
			i++;
			j++;
	}

	value[j] = '\0';

	if (value[0] == '\0') {
		fprintf(stderr, "Found an improper attribute: _%s_\n", 
			string);
		return(FALSE);
	} else {
		return(attribute_found);
	}
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
/* unless you know what you are doing... */
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
			if (found != NULL) {
                           if (found->type == OBJ_TEXT) {
				val = o_attrib_get_name_value(
					found->text->string, 
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
				printf("0 _%s_\n", found->text->string);
				printf("1 _%s_\n", found_name);
				printf("2 _%s_\n", found_value);
#endif
                           }
			}
			a_current=a_current->next;
		}	
	  }

	/* search for attributes outside */

		if (o_current->type == OBJ_TEXT) {
		 	val = o_attrib_get_name_value(
					o_current->text->string, 
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
/* unless you know what you are doing... */
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
			if (found != NULL) {
			   if (found->type == OBJ_TEXT) {
				val = o_attrib_get_name_value(
					found->text->string, 
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
				printf("0 _%s_\n", found->text->string);
				printf("1 _%s_\n", found_name);
				printf("2 _%s_\n", found_value);
#endif
                           }
			}
			a_current=a_current->next;
		}	
	  }

	/* search for attributes outside */

		if (o_current->type == OBJ_TEXT) {
		 	val = o_attrib_get_name_value(
					o_current->text->string, 
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

	if (o_current == NULL) {
		return(NULL);
	}

	if (o_current->attribs != NULL) {
		a_current = o_current->attribs;

		while(a_current != NULL) {
			found = a_current->object;
			if (found != NULL) {
                           if (found->type == OBJ_TEXT) {
				val = o_attrib_get_name_value(
					found->text->string, 
					found_name, found_value);

				if (val) {
				   if (strstr(found_name, name)) {
					if (counter != internal_counter) {
						internal_counter++;	
					} else {
/* ************************************************************* */
/* THIS IS A TOTAL HACK.  Ales should be *totally* ashamed of this */
/* This prevents gnetlist from picking up pinlabel when it wants pin */
/* THIS BETTER BE FIXED when pin#=# goes away */
				         if (!strstr(found_name, "label")) {
/* ************************************************************* */
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

#if DEBUG 
				printf("0 _%s_\n", found->text->string);
				printf("1 _%s_\n", found_name);
				printf("2 _%s_\n", found_value);
#endif
                           }
			}
			a_current=a_current->next;
		}	

	/* search for attributes outside */

		if (o_current->type == OBJ_TEXT) {
		 	val = o_attrib_get_name_value(
					o_current->text->string, 
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

/* return is the OBJECT */
/* counter is the nth occurance of the attribute, starts from ZERO! */
/* zero being the first occurance */
/* The value is what is search for, however name is also partially */
/* compared to make sure you get the attribute you really want */
OBJECT *
o_attrib_search_attrib_value(ATTRIB *list, char *value, char *name, 
			     int counter) 
{
	OBJECT *found;
	ATTRIB *a_current;
	int val;
	int internal_counter=0;
	char found_name[128]; /* limit hack */
	char found_value[128];

	a_current = list;
	
	if (!value) 
		return(NULL);

	if (!name) 
		return(NULL);

	while(a_current != NULL) {
		found = a_current->object;
		if (found != NULL) {
		   if (found->type == OBJ_TEXT) {
			val = o_attrib_get_name_value(found->text->string, 
				                      found_name, found_value);

			if (val) {
#if DEBUG
				printf("found value: %s\n", found_value);
				printf("looking for: %s\n", value);
#endif
				if (strcmp(value, found_value) == 0) {
					if (counter != internal_counter) {
						internal_counter++;	
					} else {
				   		if (strstr(found_name, name)) {
							return(found);
						}
					}
				}
			}	

                   }
		}
		a_current=a_current->next;
	 }

	return (NULL);
} 

/* it is the responsibility of the caller to free the returned string */
/* returned string is the value of the attribute */
/* counter is the nth occurance of the attribute, starts from ZERO! */
/* zero being the first occurance */
char *
o_attrib_search_attrib_name(ATTRIB *list, char *name, int counter) 
{
	OBJECT *found;
	ATTRIB *a_current;
	int val;
	int internal_counter=0;
	char found_name[128]; /* limit hack */
	char found_value[128];
	char *return_string;

	a_current = list;

	while(a_current != NULL) {
		found = a_current->object;
		if (found != NULL) {
                   if (found->type == OBJ_TEXT) {
			val = o_attrib_get_name_value(found->text->string, 
				                      found_name, found_value);

			if (val) {
#if DEBUG
				printf("found name: %s\n", found_name);
				printf("looking for: %s\n", name);
#endif
				if (strcmp(name, found_name) == 0) {
					if (counter != internal_counter) {
						internal_counter++;	
					} else {
						return_string = (char *) 
								malloc(
							sizeof(char)*
							strlen(found_value)+1);
							strcpy(return_string,
							       found_value);
						return(return_string);
					}
				}
			}	
                    }
		}
		a_current=a_current->next;
	 }

	return (NULL);
} 

/* it is the responsibility of the caller to free the returned string */
/* returned string is the value */
/* counter is the nth occurance of the attribute, starts from ZERO! */
/* zero being the first occurance */
/* the list is the top level list... don't pass it an object_head list */
/* unless you know what you are doing... */
/* be sure caller free's return value */
/* this function only search for toplevel attributes */
char *
o_attrib_search_toplevel(OBJECT *list, char *name, int counter) 
{
	OBJECT *o_current;
	int val;
	int internal_counter=0;
	char found_name[128]; /* limit hack */
	char found_value[128];
	char *return_string;

	o_current = list;

	while(o_current != NULL) {

	/* search for attributes outside */

		if (o_current->type == OBJ_TEXT) {
		 	val = o_attrib_get_name_value(
					o_current->text->string, 
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
			
			if (a_current->object->type == OBJ_TEXT &&
			     a_current->object->text->prim_objs) {

				/* I'm not terribly happy with this */
		
				if (a_current->object->saved_color != -1) {

					/* if the object is selected, make */
					/* sure it it say selected */
					o_complex_set_color(
						a_current->object->text->prim_objs,
						SELECT_COLOR);
			        	a_current->object->color = 
						SELECT_COLOR;

					o_complex_set_saved_color_only(
						a_current->object->text->prim_objs,
						w_current->attribute_color); 
			        	a_current->object->saved_color = w_current->
							attribute_color;

				} else {
					o_complex_set_color(
						a_current->object->text->prim_objs,
						w_current->attribute_color); 
			        	a_current->object->color = 
						 w_current->attribute_color;
				}
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

	return_value = o_attrib_search_name(o_current->complex->prim_objs, 
					    "gnd", 0);

	if (return_value) {
		return(return_value);
	}

	return_value = o_attrib_search_name(o_current->complex->prim_objs, 
					    "vdd", 0);

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
	OBJECT *found=NULL;
	int val;
	char found_name[128]; /* limit hack */
	char found_value[128];
	char *return_string;

	o_current = object;

	if (o_current == NULL) {
		return(NULL);
	}

	if (o_current->attribs != NULL) {
		a_current = o_current->attribs;

		while(a_current != NULL) {
			found = a_current->object;
			if (found != NULL) {
                           if (found->type == OBJ_TEXT) {
				val = o_attrib_get_name_value(
					found->text->string, 
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
				printf("0 _%s_\n", found->text->string);
				printf("1 _%s_\n", found_name);
				printf("2 _%s_\n", found_value);
#endif
                            }
			}
			a_current=a_current->next;
		}	
	}
	/* search for attributes outside */

	if (o_current->type == OBJ_TEXT) {
	 	val = o_attrib_get_name_value(o_current->text->string, 
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

/* be sure caller free's return value */
/* this function is like above, except that it returns the n'th occurance */
/* of the attribute.  counter starts counting at zero */
char *
o_attrib_search_name_single_count(OBJECT *object, char *name, 
                                  int counter) 
{
	OBJECT *o_current;
	ATTRIB *a_current;
	OBJECT *found=NULL;
	int val;
	char found_name[128]; /* limit hack */
	char found_value[128];
	char *return_string;
	int internal_counter=0;
	

	o_current = object;

	if (o_current == NULL) {
		return(NULL);
	}

	if (o_current->attribs != NULL) {
		a_current = o_current->attribs;

		while(a_current != NULL) {
			found = a_current->object;
			if (found != NULL) {
                           if (found->type == OBJ_TEXT) {
				val = o_attrib_get_name_value(
					found->text->string, 
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
				printf("0 _%s_\n", found->text->string);
				printf("1 _%s_\n", found_name);
				printf("2 _%s_\n", found_value);
#endif
			   }
			}
			a_current=a_current->next;
		}	

	}
	/* search for attributes outside */

	if (o_current->type == OBJ_TEXT) {
	 	val = o_attrib_get_name_value(o_current->text->string, 
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
	return_value = o_attrib_search_name(object->complex->prim_objs, 
					    "numslots", 0);

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
	return_value = o_attrib_search_name(object->complex->prim_objs, 
					    "slot", 0);

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
	search_for = (char *) malloc(sizeof(char)*(strlen("pin")+9));

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
	return_value = o_attrib_search_name(object->complex->prim_objs, 
					    search_for, 0);
	free(search_for);

	if (return_value) {
		return(return_value);
	}

	return(NULL);
}


char *
o_attrib_search_component(OBJECT *object, char *name)
{
	char *return_value = NULL;

	if (!name) {
		return(NULL);
	}

	if (object->type != OBJ_COMPLEX) {
		return(NULL);
	}

	/* first look inside the complex object */
	return_value = o_attrib_search_name(object->complex->prim_objs, 
					    name, 0);

	if (return_value) {
		return(return_value);
	}

	/* now look outside to see if it was attached externally */
	return_value = o_attrib_search_name_single(object, name, NULL);

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

		string = o_attrib_search_pin_number(
			o_current->complex->prim_objs, 
			pin_counter, &o_pin_attrib);

		if (string) {

			/* don't care about the value */
			free(string);

			if (o_pin_attrib->text->string) {
				free(o_pin_attrib->text->string);	
			}

			/* current_pin_int is the new pin value assigned */	
			/* however this is not valid for alphanumeric pins */
			/* current_pin_int = atoi(current_pin);	*/

			/* 9 is the size of one number plus null character */
			o_pin_attrib->text->string = (char *) malloc(
				sizeof(char)*(strlen("pin=")+
					      strlen(current_pin)+9));

			/* removed _int from current_pin */
			sprintf(o_pin_attrib->text->string, "pin%d=%s", 
					pin_counter, current_pin); 

			o_text_recreate(w_current, o_pin_attrib);

#if DEBUG 
			printf("full object string %s\n", o_pin_attrib->text->string);
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

		string = o_attrib_search_pin_number(target->complex->prim_objs,
						pin_counter, &o_pin_attrib);

		if (string) {

			/* don't care about the value */
			free(string);

			if (o_pin_attrib->text->string) {
				free(o_pin_attrib->text->string);	
			}

			/* current_pin_int is the new pin value assigned */	
			/* however this is not valid for alphanumeric pins */
			/* current_pin_int = atoi(current_pin);	*/

			/* 9 is the size of one number plus null character */
			o_pin_attrib->text->string = (char *) malloc(
				sizeof(char)*(strlen("pin=")+
					      strlen(current_pin)+9));

			/* changed current_pin_int to current_pin (a string) */
			sprintf(o_pin_attrib->text->string, "pin%d=%s", 
					pin_counter, current_pin); 

			o_text_recreate(w_current, o_pin_attrib);

#if DEBUG 
			printf("full object string %s\n", o_pin_attrib->text->string);
#endif
			pin_counter++;
		} else {
			printf("component missing pin# attribute\n");
		}

		current_pin = strtok(NULL, DELIMITERS);
	} 
	free(slot_num);
}

/* returns the number of toplevel attributes in all loaded pages */
int
o_attrib_count_toplevel(TOPLEVEL *w_current, char *name)
{
	int ret_value=0;
	int counter=0;
	PAGE *p_current;
	char *string;

	p_current = w_current->page_head;

	while(p_current != NULL) {

		counter = 0;
		string = o_attrib_search_name(p_current->object_head, 
				name, counter); 
	printf("%s %d\n", name, counter);
		while(string) {
			printf("inside\n");
			ret_value++;
			free(string);
			string=NULL;
			counter++;

			string = o_attrib_search_name(p_current->object_head, 
				name, counter); 
		}

		p_current=p_current->next;
	}
	return(ret_value);
}

/* search all loaded pages for first found toplevel attribute */
/* caller is responsible to freeing returned value */
/* see o_attrib_search_toplevel for other comments */
char *
o_attrib_search_toplevel_all(PAGE *page_head, char *name)
{
	PAGE *p_current;
	char *ret_value=NULL;

	p_current = page_head;

	while (p_current != NULL) {


		/* don't look into the head of page_head */
		if (p_current->pid != -1) {

			/* only look for first occurrance of the attribute */
			ret_value = o_attrib_search_toplevel(
						p_current->object_head, 
						name, 0);
		}

		if (ret_value != NULL) {
			return(ret_value);
		}
		
		p_current = p_current->next;
	}

	return(NULL);
}

/* This function returns all attached attribute objects to the specified */
/* object */
/* The returned list is an array of objects and should be freed using the */
/* below function */
/* This routine will only look for attached attributes and not unattached */
/* free floating attribs */
OBJECT **
o_attrib_return_attribs(OBJECT *object_list, OBJECT *sel_object) 
{
	OBJECT **found_objects;
	int num_attribs=0;
	int i=0;
	ATTRIB *a_current;	
	OBJECT *o_current;
	OBJECT *object;

	object = (OBJECT *) o_list_search(object_list, sel_object);

	if (!object) {
		return(NULL);	
	}

	if (!object->attribs) {
		return(NULL);
	}

	if (!object->attribs->next) {
		return(NULL);
	}


	/* first go through and count the number of attribs */
	a_current = object->attribs->next;	
	while(a_current != NULL) {
		num_attribs++;
		a_current = a_current->next;
	}

	found_objects = (OBJECT **) malloc(sizeof(OBJECT *)*(num_attribs+1));

	/* now actually fill the array of objects */
	a_current = object->attribs->next;	
	while(a_current != NULL) {
		if (a_current->object != NULL) {
			o_current = a_current->object;
			if (o_current->type == OBJ_TEXT && 
			    o_current->text->string) {
				found_objects[i] = o_current;
				i++;
			}
		}
		a_current = a_current->next;
	}

	found_objects[i] = NULL;

#if DEBUG 
	i=0;
	while(found_objects[i] != NULL) {
		/*for (i = 0 ; i < num_attribs; i++) {*/
		printf("%d : %s\n", i, found_objects[i]->text->string);
		i++;
	}
#endif

	return(found_objects);
}

void
o_attrib_free_returned(OBJECT **found_objects)
{
	int i=0;

	if (!found_objects) {
		return;
	}

	/* don't free the insides of found_objects, since the contents are */
	/* just pointers into the real object list */
	while(found_objects[i] != NULL) {
		found_objects[i] = NULL;
		i++;	
	}

	free(found_objects);
}

