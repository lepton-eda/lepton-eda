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

/*! \brief Get the file header string.
 *  \par Function Description
 *  This function simply returns the DATE_VERSION and
 *  FILEFORMAT_VERSION formatted as a gEDA file header.
 *
 *  \warning <em>Do not</em> free the returned string.
 */
const gchar *o_file_format_header()
{
  static gchar *header = NULL;

  if (header == NULL)
    header = g_strdup_printf("v %s %u\n", DATE_VERSION, FILEFORMAT_VERSION);

  return header;
}

/*! \brief "Save" a file into a string buffer
 *  \par Function Description
 *  This function saves a whole schematic into a buffer in libgeda
 *  format. The buffer should be freed when no longer needed.
 *
 *  \param [in] toplevel  The data to save.
 *  \returns a buffer containing schematic data or NULL on failure.
 */
gchar *o_save_buffer (TOPLEVEL *toplevel)
{
  GString *acc;
  gchar *buffer;

  if (toplevel == NULL) return NULL;

  /* make sure you init net_consolide to false (default) in all */
  /* programs */
  if (toplevel->net_consolidate == TRUE) {
    o_net_consolidate(toplevel);
  }

  acc = g_string_new (o_file_format_header());

  buffer = o_save_objects (toplevel->page_current->object_head);
  g_string_append (acc, buffer);
  g_free (buffer);

  return g_string_free (acc, FALSE);
}

/*! \brief Save a series of objects into a string buffer
 *  \par Function Description
 *  This function recursively saves a set of objects into a buffer in
 *  libgeda format.  User code should not normally call this function;
 *  they should call o_save_buffer() instead.
 *
 *  \param [in] object_list  Head of list of objects to save.
 *  \returns a buffer containing schematic data or NULL on failure.
 */
gchar *o_save_objects (OBJECT *object_list)
{
  OBJECT *o_current = object_list;
  gchar *out;
  GString *acc;
  gboolean already_wrote = FALSE;

  g_assert (object_list != NULL);

  acc = g_string_new("");

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
            g_string_append_printf(acc, "%s\n", out);
            already_wrote = TRUE;
	    g_free(out); /* need to free here because of the above flag */

            if (o_complex_is_embedded(o_current)) {
              g_string_append(acc, "[\n");
	      
              out = o_save_objects(o_current->complex->prim_objs);
              g_string_append (acc, out);
              g_free(out);

              g_string_append(acc, "]\n");
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
            g_assert_not_reached();
            fprintf(stderr, "Error type!\n");
            exit(-1);
            break;
        }

        /* output the line */
        if (!already_wrote) {
          g_string_append_printf(acc, "%s\n", out);
	  g_free(out);
        } else {
          already_wrote = FALSE;
        }

        /* save any attributes */
        if (o_current->attribs != NULL) {
          out = o_save_attribs(o_current->attribs);
          g_string_append(acc, out);
          g_free (out);
        }

      }
    } 
    o_current = o_current->next;
  }

  return g_string_free (acc, FALSE);
}

/*! \brief Save a file
 *  \par Function Description
 *  This function saves the data in a libgeda format to a file
 *  \param [in] toplevel  The data to save to file.
 *  \param [in] filename   The filename to save the data to.
 *  \return 1 on success, 0 on failure.
 */
int o_save(TOPLEVEL *toplevel, const char *filename)
{
  FILE *fp;
  char *buffer;
	
  fp = fopen(filename, "wb");
	
  if (fp == NULL) {
    s_log_message("o_save: Could not open [%s]\n", filename);
    return 0;
  }

  buffer = o_save_buffer (toplevel);
  fwrite (buffer, strlen(buffer), 1, fp);
  g_free (buffer);
  fclose (fp);

  return 1;
}

/*! \brief Read a memory buffer
 *  \par Function Description
 *  This function reads data in libgeda format from a memory buffer.
 *
 *  If the size argument is negative, the buffer is assumed to be
 *  null-terminated.
 *
 *  The name argument is used for debugging, and should be set to a
 *  meaningful string (e.g. the name of the file the data is from).
 *
 *  \param [in,out] toplevel    The current TOPLEVEL structure.
 *  \param [in]     object_list  The object_list to read data to.
 *  \param [in]     buffer       The memory buffer to read from.
 *  \param [in]     size         The size of the buffer.
 *  \param [in]     name         The name to describe the data with.
 *  \return object_list if successful read, or NULL on error.
 */
OBJECT *o_read_buffer(TOPLEVEL *toplevel, OBJECT *object_list,
		      char *buffer, const int size, 
		      const char *name)
{
  char *line = NULL;
  TextBuffer *tb = NULL;

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

  int embedded_level = 0;


  /* fill version with default file format version (the current one) */
  current_fileformat_ver = FILEFORMAT_VERSION;

  if (buffer == NULL) {
    s_log_message("o_read_buffer: Received NULL buffer\n");
    return(NULL);
  }

  tb = s_textbuffer_new (buffer, size);
  g_assert (tb != NULL);

  while (1) {

    line = s_textbuffer_next_line(tb);
    if (line == NULL) break;

    sscanf(line, "%c", &objtype);

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
        o_complex_check_symversion(toplevel, last_complex);
        last_complex = NULL;  /* no longer need to check */
    }

    switch (objtype) {

      case(OBJ_LINE):
        object_list = (OBJECT *) o_line_read(toplevel, object_list, line,
	                                     release_ver, fileformat_ver);
        break;


      case(OBJ_NET):
        object_list = (OBJECT *) o_net_read(toplevel, object_list, line,
                                            release_ver, fileformat_ver);
        break;

      case(OBJ_BUS):
        object_list = (OBJECT *) o_bus_read(toplevel, object_list, line,
                                            release_ver, fileformat_ver);
        break;

      case(OBJ_BOX):
        object_list = (OBJECT *) o_box_read(toplevel, object_list, line,
                                            release_ver, fileformat_ver);
        break;
		
      case(OBJ_PICTURE):
	line = g_strdup(line);
        object_list = (OBJECT *) o_picture_read(toplevel, object_list,
						line, tb,
						release_ver, fileformat_ver);
	g_free (line);
        break;
		
      case(OBJ_CIRCLE):
        object_list = (OBJECT *) o_circle_read(toplevel, object_list, line,
                                               release_ver, fileformat_ver);
        break;

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
        object_list = (OBJECT *) o_complex_read(toplevel, object_list, line,
                                                release_ver, fileformat_ver);

        /* this is necessary because complex may add attributes which float */
	/* needed now? */
        object_list = (OBJECT *) return_tail(object_list);

        /* last_complex is used for verifying symversion attribute */
        last_complex = object_list;
        break;

      case(OBJ_TEXT):
	line = g_strdup(line);
        object_list = (OBJECT *) o_text_read(toplevel, object_list,
					     line, tb,
                                             release_ver, fileformat_ver);
	g_free(line);
        break;

      case(OBJ_PIN):
        object_list = (OBJECT *) o_pin_read(toplevel, object_list, line,
                                            release_ver, fileformat_ver);
        found_pin++;
        break;

      case(OBJ_ARC):
        object_list = (OBJECT *) o_arc_read(toplevel, object_list, line,
                                            release_ver, fileformat_ver);
        break;

      case(STARTATTACH_ATTR): 
        object_before_attr = object_list;
	/* first is the fp */
	/* 2nd is the object to get the attributes */ 
        object_list = (OBJECT *) o_read_attribs(toplevel, object_list,
						tb,
                                                release_ver, fileformat_ver);

        /* by now we have finished reading all the attributes */
        /* did we just finish attaching to a complex object? */
        if (last_complex)
        {
          /* yes */
          /* verify symbol version (not file format but rather contents) */
          o_complex_check_symversion(toplevel, last_complex);
          last_complex = NULL;
        }
        
	/* slots only apply to complex objects */
        if (object_before_attr->type == OBJ_COMPLEX || 
	    object_before_attr->type == OBJ_PLACEHOLDER) {
          o_attrib_slot_update(toplevel, object_before_attr);
        }

	/* need this? nope */
	/*object_list = return_tail(object_list);*/
        object_before_attr = NULL;
        break;

      case(START_EMBEDDED): 
        
	if(object_list->type == OBJ_COMPLEX || 
	   object_list->type == OBJ_PLACEHOLDER) {

        	object_list_save = object_list;
        	object_list = object_list_save->complex->prim_objs;
				
        	temp_tail = toplevel->page_current->object_tail;
        	temp_parent = toplevel->page_current->object_parent;
        	toplevel->page_current->object_parent = object_list;

		embedded_level++;
	} else {
        	fprintf(stderr, "Read unexpected embedded "
				"symbol start marker in [%s] :\n>>\n%s<<\n", 
				name, line);
	}
       	break;

      case(END_EMBEDDED): 
      	if(embedded_level>0) {
        	object_list = object_list_save;
		/* don't do this since objects are already
		 * stored/read translated 
	         * o_complex_translate_world(toplevel, object_list->x,
                 *                   object_list->y, object_list->complex);
		 */
	        toplevel->page_current->object_tail = temp_tail;
	        toplevel->page_current->object_parent = temp_parent;

          o_complex_recalc( toplevel, object_list );
		embedded_level--;
	} else {
        	fprintf(stderr, "Read unexpected embedded "
				"symbol end marker in [%s] :\n>>\n%s<<\n", 
				name, line);
	}
	
        break;

      case(ENDATTACH_ATTR):
        /* this case is never hit, since the } is consumed by o_read_attribs */
        break;	

      case(INFO_FONT): 
        o_text_set_info_font(line);
        break;	

      case(COMMENT):
	/* do nothing */
        break;

      case(VERSION_CHAR):
        itemsread = sscanf(line, "v %u %u\n", &release_ver, &fileformat_ver);

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
                        "Please run g[sym|sch]update on:\n[%s]\n", name);
	}
        break;

      default:
        fprintf(stderr, "Read garbage in [%s] :\n>>\n%s<<\n",
                name, line);
        break;
    }

  }

  /* Was the very last thing we read a complex and has it not been checked */
  /* yet?  This would happen if the complex is at the very end of the file  */
  /* and had no attached attributes */
  if (last_complex)
  {
        o_complex_check_symversion(toplevel, last_complex);
        last_complex = NULL;  /* no longer need to check */
  }

  if (found_pin) {
    if (release_ver <= VERSION_20020825) {
      o_pin_update_whichend(toplevel, return_head(object_list), found_pin);
    }
  }

  tb = s_textbuffer_free(tb);
  
  return(object_list);

}

/*! \brief Read a file
 *  \par Function Description
 *  This function reads a file in libgeda format.
 *
 *  \param [in,out] toplevel    The current TOPLEVEL structure.
 *  \param [in]     object_list  The object_list to read data to.
 *  \param [in]     filename     The filename to read from.
 *  \return object_list if successful read, or NULL on error.
 */
OBJECT *o_read(TOPLEVEL *toplevel, OBJECT *object_list, char *filename)
{
  GError *err = NULL;
  char *buffer = NULL;
  size_t size;
  OBJECT *result = NULL;

  g_file_get_contents(filename, &buffer, &size, &err);

  g_assert ((buffer == NULL && err != NULL) 
	    || (buffer != NULL && err == NULL));

  if (err != NULL)
    {
      /* Report error to user, and free error */
      g_assert (buffer == NULL);
      fprintf (stderr, "o_read: Unable to read file: [%s]\n", err->message);
      g_error_free (err);
      return NULL;
    } 

  /* Parse file contents */
  g_assert (buffer != NULL);
  result = o_read_buffer (toplevel, object_list, buffer, size, filename);
  g_free (buffer);
  return result;
}

/*! \brief Scale a set of lines.
 *  \par Function Description
 *  This function takes a list of lines and scales them
 *  by the values of x_scale and y_scale.
 *
 *  \param [in] toplevel  The current TOPLEVEL object.
 *  \param [in,out]  list  The list with lines to scale.
 *  \param [in]   x_scale  The x scale value for the lines.
 *  \param [in]   y_scale  The y scale value for the lines.
 *
 *  \todo this really doesn't belong here. you need more of a core routine
 *        first. yes.. this is the core routine, just strip out the drawing
 *        stuff
 *        move it to o_complex_scale
 */
void o_scale(TOPLEVEL *toplevel, OBJECT *list, int x_scale, int y_scale)
{
  OBJECT *o_current;

  /* this is okay if you just hit scale and have nothing selected */
  if (list == NULL) {
    return;
  }

  o_current = list;
  while (o_current != NULL) {
    switch(o_current->type) {
      case(OBJ_LINE):
        o_line_scale_world(toplevel, x_scale, y_scale, o_current);
        break;
    }
    o_current = o_current->next;
  }
}


