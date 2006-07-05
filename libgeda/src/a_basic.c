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
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <gtk/gtk.h>
#include <libguile.h>

#include "defines.h"
#include "struct.h"
#include "globals.h"
#include "o_types.h"
#include "colors.h"

#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \brief Current version string */
#define VERSION_20020825 20020825

/*! \brief Save embedded attributes to current file
 *  \par Function Description
 *  This function will save all embedded attributes to a file.
 *
 *  \param [in,out] w_current
 *  \param [in]     object_list  The list of attributes to write to file
 *  \param [in]     fp           The file to write to.
 */
void o_save_embedded(TOPLEVEL *w_current, OBJECT *object_list, FILE *fp)
{
  OBJECT *o_current=NULL;
  char *out;

  /* make sure you init net_consolide to false (default) in all */
  /* programs */
  if (w_current->net_consolidate == TRUE) {
    o_net_consolidate(w_current);
  }
	
  o_current = object_list;

  while ( o_current != NULL ) {

    if (o_current->type != OBJ_HEAD) {

      if (o_current->attribute == 0) {

        switch (o_current->type) {

          case(OBJ_LINE):
            out = (char *) o_line_save(o_current);
            break;
	
          case(OBJ_NET):
            out = (char *) o_net_save(o_current);
            break;

          case(OBJ_BUS):
            out = (char *) o_bus_save(o_current);
            break;
	
          case(OBJ_BOX):
            out = (char *) o_box_save(o_current);
            break;
			
          case(OBJ_CIRCLE):
            out = (char *) o_circle_save(o_current);
            break;

          case(OBJ_COMPLEX):
            out = (char *) o_complex_save(o_current);
            if (strncmp(o_current->complex_clib, "EMBEDDED", 8) == 0) {
              fprintf(fp, "[\n");
								
              o_save_embedded(
                              w_current,
                              o_current->
                              complex->prim_objs,
                              fp);

              fprintf(fp, "]\n");
            }
            break;

          case(OBJ_PLACEHOLDER):  /* new type by SDB 1.20.2005 */
            out = (char *) o_complex_save(o_current);
            break;

          case(OBJ_TEXT):
            out = (char *) o_text_save(o_current);
            break;

          case(OBJ_PIN):
            out = (char *) o_pin_save(o_current);
            break;
	
          case(OBJ_ARC):
            out = (char *) o_arc_save(o_current);
            break;

  	  case(OBJ_PICTURE):
	    out = (char *) o_picture_save(o_current); 
	    break;

          default:
            fprintf(stderr, "Error type!\n");
            exit(-1);
            break;
        }

	/* output the line */
        fprintf(fp, "%s\n", out);
	free(out);

        /* save those attributes */
        if (o_current->attribs != NULL) {
          if (o_current->attribs->next != NULL) {
            o_save_attribs(fp, o_current->attribs->next);
          }
        }

      }
    } 
    o_current = o_current->next;
  }
}

/*! \brief Write libgeda file header
 *  \par Function Description
 *  This function simply prints the VERSION and FILEFORMAT_VERSION
 *  definitions to the file.
 *  
 *  \param [in] fp  The file to write the header to.
 */
void o_save_write_header(FILE *fp)
{
  fprintf(fp, "v %s %u\n", VERSION, FILEFORMAT_VERSION);
}

/*! \brief Save a file
 *  \par Function Description
 *  This function saves the data in a libgeda format to a file
 *  \param [in] w_current  The data to save to file.
 *  \param [in] filename   The filename to save the data to.
 *  \return 1 on success, 0 on failure.
 */
int o_save(TOPLEVEL *w_current, const char *filename)
{
  OBJECT *o_current=NULL;
  FILE *fp;
  char *out;
  int already_wrote=0;
	
  fp = fopen(filename, "w");
	
  if (fp == NULL) {
    s_log_message("o_save: Could not open [%s]\n", filename);
    return 0;
  }


  o_current = w_current->page_current->object_head;

  /* make sure you init net_consolide to false (default) in all */
  /* programs */
  if (w_current->net_consolidate == TRUE) {
    o_net_consolidate(w_current);
  }

  o_save_write_header(fp);

  while ( o_current != NULL ) {

    if (o_current->type != OBJ_HEAD) {

      if (o_current->attribute == 0) {

        switch (o_current->type) {

          case(OBJ_LINE):
            out = (char *) o_line_save(o_current);
            break;
	
          case(OBJ_NET):
            out = (char *) o_net_save(o_current);
            break;

          case(OBJ_BUS):
            out = (char *) o_bus_save(o_current);
            break;
	
          case(OBJ_BOX):
            out = (char *) o_box_save(o_current);
            break;
			
          case(OBJ_CIRCLE):
            out = (char *) o_circle_save(o_current);
            break;

          case(OBJ_COMPLEX):
            out = (char *) o_complex_save(o_current);
            fprintf(fp, "%s\n", out);
            already_wrote=1;
            if (strncmp(o_current->complex_clib, "EMBEDDED", 8) == 0) {
              fprintf(fp, "[\n");
								
              o_save_embedded(
                              w_current,
                              o_current->
                              complex->prim_objs,
                              fp);

              fprintf(fp, "]\n");
            }
            break;

          case(OBJ_PLACEHOLDER):  /* new type by SDB 1.20.2005 */
            out = (char *) o_complex_save(o_current);
            break;

          case(OBJ_TEXT):
            out = (char *) o_text_save(o_current);
            break;

          case(OBJ_PIN):
            out = (char *) o_pin_save(o_current);
            break;
	
          case(OBJ_ARC):
            out = (char *) o_arc_save(o_current);
            break;

  	  case(OBJ_PICTURE):
	    out = (char *) o_picture_save(o_current); 
	    break;

          default:
            fprintf(stderr, "Error type!\n");
            exit(-1);
            break;
        }

				/* output the line */
        if (!already_wrote) {
          fprintf(fp, "%s\n", out);
	  free(out);
        } else {
          already_wrote=0;
        }

				/* save those attributes */
        if (o_current->attribs != NULL) {
          if (o_current->attribs->next != NULL) {
            o_save_attribs(fp, o_current->attribs->next);
          }
        }

      }
    } 
    o_current = o_current->next;
  }

  fclose(fp);
  return 1;
}

/*! \brief Read a file
 *  \par Function Description
 *  This function reads a file in libgead format.
 *
 *  \param [in,out] w_current    The current TOPLEVEL structure.
 *  \param [in]     object_list  The object_list to read data to.
 *  \param [in]     filename     The filename to read from.
 *  \return object_list if successful read, or NULL on error.
 */
OBJECT *o_read(TOPLEVEL *w_current, OBJECT *object_list, char *filename)
{
  FILE *fp;
  char buf[1024];
  char objtype;
  OBJECT *object_list_save=NULL;
  OBJECT *temp_tail=NULL;
  OBJECT *temp_parent=NULL;
  OBJECT *object_before_attr=NULL;
  unsigned int release_ver;
  unsigned int fileformat_ver;
  unsigned int current_fileformat_ver;
  int found_pin = 0;
  OBJECT* last_complex = NULL;
  int itemsread = 0;

  /* fill version with default file format version (the current one) */
  current_fileformat_ver = FILEFORMAT_VERSION;

  fp = fopen(filename, "r");

  if (fp == NULL) {
    s_log_message("o_read: Could not open [%s]\n", filename);
    return(NULL);
  }

  while ( fgets(buf, 1024, fp) != NULL) {

    sscanf(buf, "%c", &objtype);

    /* Do we need to check the symbol version?  Yes, but only if */
    /* 1) the last object read was a complex and */
    /* 2) the next object isn't the start of attributes.  */
    /* If the next object is the start of attributes, then check the */
    /* symbol version after the attributes have been read in, see the */
    /* STARTATTACH_ATTR case */
    if (last_complex && objtype != STARTATTACH_ATTR)
    {
        /* yes */
        /* verify symbol version (not file format but rather contents) */
        o_complex_check_symversion(w_current, last_complex);
        last_complex = NULL;  /* no longer need to check */
    }

    switch (objtype) {

      case(OBJ_LINE):
        object_list = (OBJECT *) o_line_read(w_current, object_list, buf, 
	                                     release_ver, fileformat_ver);
        break;


      case(OBJ_NET):
        object_list = (OBJECT *) o_net_read(w_current, object_list, buf,
                                            release_ver, fileformat_ver);
        break;

      case(OBJ_BUS):
        object_list = (OBJECT *) o_bus_read(w_current, object_list, buf,
                                            release_ver, fileformat_ver);
        break;

      case(OBJ_BOX):
        object_list = (OBJECT *) o_box_read(w_current, object_list, buf,
                                            release_ver, fileformat_ver);
        break;
		
      case(OBJ_PICTURE):
        object_list = (OBJECT *) o_picture_read(w_current, object_list, buf,
						fp,
						release_ver, fileformat_ver);
        break;
		
      case(OBJ_CIRCLE):
        object_list = (OBJECT *) o_circle_read(w_current, object_list, buf,
                                               release_ver, fileformat_ver);
        break;

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
        object_list = (OBJECT *) o_complex_read(w_current, object_list, buf,
                                                release_ver, fileformat_ver);

        /* this is necessary because complex may add attributes which float */
	/* needed now? */
        object_list = (OBJECT *) return_tail(object_list);

        /* last_complex is used for verifying symversion attribute */
        last_complex = object_list;
        break;

      case(OBJ_TEXT):
        /* fgets(string, 1024, fp); string lines are now read inside: */
        object_list = (OBJECT *) o_text_read(w_current, object_list, buf,
                                             fp,
                                             release_ver, fileformat_ver);
        break;

      case(OBJ_PIN):
        object_list = (OBJECT *) o_pin_read(w_current, object_list, buf,
                                            release_ver, fileformat_ver);
        found_pin++;
        break;

      case(OBJ_ARC):
        object_list = (OBJECT *) o_arc_read(w_current, object_list, buf,
                                            release_ver, fileformat_ver);
        break;

      case(STARTATTACH_ATTR): 
        object_before_attr = object_list;
	/* first is the fp */
	/* 2nd is the object to get the attributes */ 
        object_list = (OBJECT *) o_read_attribs(w_current, fp, object_list,
                                                release_ver, fileformat_ver);

        /* by now we have finished reading all the attributes */
        /* did we just finish attaching to a complex object? */
        if (last_complex)
        {
          /* yes */
          /* verify symbol version (not file format but rather contents) */
          o_complex_check_symversion(w_current, last_complex);
          last_complex = NULL;
        }
        
	/* slots only apply to complex objects */
        if (object_before_attr->type == OBJ_COMPLEX || 
	    object_before_attr->type == OBJ_PLACEHOLDER) {
          o_attrib_slot_update(w_current, object_before_attr);	
        }

	/* need this? nope */
	/*object_list = return_tail(object_list);*/
        object_before_attr = NULL;
        break;

      case(START_EMBEDDED): 
        object_list_save = object_list;
        object_list = object_list_save->complex->
          prim_objs;
				
        temp_tail =
          w_current->page_current->object_tail;
        temp_parent =
          w_current->page_current->object_parent;
        w_current->page_current->object_parent = 
          object_list;
        break;

      case(END_EMBEDDED): 
        object_list = object_list_save;
	/* don't do this since objects are already
	 * stored/read translated 
         * o_complex_world_translate(w_current, object_list->x,
         *                           object_list->y, object_list->complex);
	 */
        w_current->page_current->object_tail = temp_tail;
        w_current->page_current->object_parent = temp_parent;
        break;

      case(ENDATTACH_ATTR):
        /* this case is never hit, since the } is consumed by o_read_attribs */
        break;	

      case(INFO_FONT): 
        o_text_set_info_font(buf);
        break;	

      case(COMMENT):
	/* do nothing */
        break;

      case(VERSION_CHAR):
        itemsread = sscanf(buf, "v %u %u\n", &release_ver, &fileformat_ver);

	/* 20030921 was the last version which did not have a fileformat */
	/* version.  The below latter test should not happen, but it is here */
	/* just in in case. */
	if (release_ver <= VERSION_20030921 || itemsread == 1)
        { 
          fileformat_ver = 0;
	}
        
	if (fileformat_ver < current_fileformat_ver)
        {
       	  s_log_message("Read an old format sym/sch file!\n"
                        "Please run g[sym|sch]update on:\n[%s]\n", filename);
	}
        break;

      default:
        fprintf(stderr, "Read garbage in [%s] :\n>>\n%s<<\n",
                filename, buf);
        break;
    }

  }
  fclose(fp);

  /* Was the very last thing we read a complex and has it not been checked */
  /* yet?  This would happen if the complex is at the very end of the file  */
  /* and had no attached attributes */
  if (last_complex)
  {
        o_complex_check_symversion(w_current, last_complex);
        last_complex = NULL;  /* no longer need to check */
  }

  if (found_pin) {
    if (release_ver <= VERSION_20020825) {
      o_pin_update_whichend(w_current, return_head(object_list), found_pin);
    }
  }
  
  return(object_list);
}

/*! \brief Scale a set of lines.
 *  \par Function Description
 *  This function takes a list of lines and scales them
 *  by the values of x_scale and y_scale.
 *
 *  \param [in] w_current  The current TOPLEVEL object.
 *  \param [in,out]  list  The list with lines to scale.
 *  \param [in]   x_scale  The x scale value for the lines.
 *  \param [in]   y_scale  The y scale value for the lines.
 *
 *  \todo this really doesn't belong here. you need more of a core routine
 *        first. yes.. this is the core routine, just strip out the drawing
 *        stuff
 *        move it to o_complex_scale
 */
void o_scale(TOPLEVEL *w_current, OBJECT *list, int x_scale, int y_scale)
{
  OBJECT *o_current;

  /* this is okay if you just hit scale and have nothing selected */
  if (list == NULL) { 
    /* w_current->event_state = SELECT;*/
    /* i_update_status(w_current, "Select Mode"); not here */
    /*		w_current->inside_action = 0;*/
    return;
  }


  o_current = list;


  while (o_current != NULL) {

    switch(o_current->type) {

      case(OBJ_LINE):
				/* erase the current selection */
        w_current->override_color =
          w_current->background_color;
        o_redraw_single(w_current, o_current);
                                /* o_line_draw(w_current, o_current);*/
        w_current->override_color = -1;

        o_line_scale_world(w_current, 
                           x_scale, y_scale, o_current);
        break;
    }

    o_current = o_current->next;
  }

  /* don't do this at this level */
  /* w_current->page_current->CHANGED=1;*/
}


