/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2008 Ales Hvezda
 * Copyright (C) 1998-2008 gEDA Contributors (see ChangeLog for details)
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

/*! \file a_basic.c
 *  \brief basic libgeda read and write functions
 */
#include <config.h>

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

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
 *  \param [in] toplevel    The current TOPLEVEL.
 *  \param [in] object_list The head of a GList of OBJECTs to save.
 *  \returns a buffer containing schematic data or NULL on failure.
 */
gchar *o_save_buffer (TOPLEVEL *toplevel, const GList *object_list)
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

  buffer = o_save_objects (object_list, FALSE);
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
 *  With save_attribs passed as FALSE, attribute objects are skipped over,
 *  and saved separately - after the objects they are attached to. When
 *  we recurse for saving out those attributes, the function must be called
 *  with save_attribs passed as TRUE.
 *
 *  \param [in] object_list   The head of a GList of objects to save.
 *  \param [in] save_attribs  Should attribute objects encounterd be saved?
 *  \returns a buffer containing schematic data or NULL on failure.
 */
gchar *o_save_objects (const GList *object_list, gboolean save_attribs)
{
  OBJECT *o_current;
  const GList *iter;
  gchar *out;
  GString *acc;
  gboolean already_wrote = FALSE;

  acc = g_string_new("");

  iter = object_list;

  while ( iter != NULL ) {
    o_current = (OBJECT *)iter->data;

    if (save_attribs || o_current->attached_to == NULL) {

      switch (o_current->type) {

        case(OBJ_LINE):
          out = o_line_save(o_current);
          break;

        case(OBJ_NET):
          out = o_net_save(o_current);
          break;

        case(OBJ_BUS):
          out = o_bus_save(o_current);
          break;

        case(OBJ_BOX):
          out = o_box_save(o_current);
          break;

        case(OBJ_CIRCLE):
          out = o_circle_save(o_current);
          break;

        case(OBJ_COMPLEX):
          out = o_complex_save(o_current);
          g_string_append_printf(acc, "%s\n", out);
          already_wrote = TRUE;
          g_free(out); /* need to free here because of the above flag */

          if (o_complex_is_embedded(o_current)) {
            g_string_append(acc, "[\n");

            out = o_save_objects(o_current->complex->prim_objs, FALSE);
            g_string_append (acc, out);
            g_free(out);

            g_string_append(acc, "]\n");
          }
          break;

        case(OBJ_PLACEHOLDER):  /* new type by SDB 1.20.2005 */
          out = o_complex_save(o_current);
          break;

        case(OBJ_TEXT):
          out = o_text_save(o_current);
          break;

        case(OBJ_PATH):
          out = o_path_save(o_current);
          break;

        case(OBJ_PIN):
          out = o_pin_save(o_current);
          break;

        case(OBJ_ARC):
          out = o_arc_save(o_current);
          break;

        case(OBJ_PICTURE):
          out = o_picture_save(o_current);
          break;

        default:
          /*! \todo Maybe we can continue instead of just failing
           *  completely? In any case, failing gracefully is better
           *  than killing the program, which is what this used to
           *  do... */
          g_critical (_("o_save_objects: object %p has unknown type '%c'\n"),
                      o_current, o_current->type);
          /* Dump string built so far */
          g_string_free (acc, TRUE);
          return NULL;
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
        g_string_append (acc, "{\n");

        out = o_save_objects (o_current->attribs, TRUE);
        g_string_append (acc, out);
        g_free(out);

        g_string_append (acc, "}\n");
      }
    }

    iter = g_list_next (iter);
  }

  return g_string_free (acc, FALSE);
}

/*! \brief Save a file
 *  \par Function Description
 *  This function saves the data in a libgeda format to a file
 *  \param [in] toplevel    The current TOPLEVEL.
 *  \param [in] object_list The head of a GList of OBJECTs to save.
 *  \param [in] filename    The filename to save the data to.
 *  \return 1 on success, 0 on failure.
 */
int o_save (TOPLEVEL *toplevel, const GList *object_list,
            const char *filename)
{
  FILE *fp;
  char *buffer;

  fp = fopen(filename, "wb");

  if (fp == NULL) {
    s_log_message(_("o_save: Could not open [%s]\n"), filename);
    return 0;
  }

  buffer = o_save_buffer (toplevel, object_list);
  if (fwrite (buffer, strlen(buffer), 1, fp) != 1) {
    /* An error occurred with fwrite */
#warning FIXME: What do we do?
  }
  g_free (buffer);
  fclose (fp);

  return 1;
}

/*! \brief Save a file
 *  \par Function Description
 *  This function saves the current page in a libgeda format to a file
 *  \param [in] toplevel    The current TOPLEVEL.
 *  \param [in] filename    The filename to save the data to.
 *  \return 1 on success, 0 on failure.
 */
int o_save_curr_page (TOPLEVEL *toplevel, const char *filename)
{
  return o_save (toplevel, s_page_objects (toplevel->page_current),
                 filename);
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
 *  \return GList of objects if successful read, or NULL on error.
 */
GList *o_read_buffer (TOPLEVEL *toplevel, GList *object_list,
                      char *buffer, const int size,
                      const char *name)
{
  char *line = NULL;
  TextBuffer *tb = NULL;

  char objtype;
  GList *object_list_save=NULL;
  OBJECT *new_obj=NULL;
  GList *new_obj_list;
  GList *iter;
  unsigned int release_ver;
  unsigned int fileformat_ver;
  unsigned int current_fileformat_ver;
  int found_pin = 0;
  OBJECT* last_complex = NULL;
  int itemsread = 0;

  int embedded_level = 0;


  /* fill version with default file format version (the current one) */
  current_fileformat_ver = FILEFORMAT_VERSION;

  g_return_val_if_fail ((buffer != NULL), NULL);

  tb = s_textbuffer_new (buffer, size);

  object_list = g_list_reverse (object_list);

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
        new_obj = o_line_read (toplevel, line, release_ver, fileformat_ver);
        object_list = g_list_prepend (object_list, new_obj);
        break;


      case(OBJ_NET):
        new_obj = o_net_read (toplevel, line, release_ver, fileformat_ver);
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_BUS):
        new_obj = o_bus_read (toplevel, line, release_ver, fileformat_ver);
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_BOX):
        new_obj = o_box_read (toplevel, line, release_ver, fileformat_ver);
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_PICTURE):
        line = g_strdup (line);
        new_obj = o_picture_read (toplevel, line, tb, release_ver, fileformat_ver);
        g_free (line);
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_CIRCLE):
        new_obj = o_circle_read (toplevel, line, release_ver, fileformat_ver);
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
        new_obj = o_complex_read (toplevel, line, release_ver, fileformat_ver);
        object_list = g_list_prepend (object_list, new_obj);

        /* last_complex is used for verifying symversion attribute */
        last_complex = new_obj;
        break;

      case(OBJ_TEXT):
        line = g_strdup (line);
        new_obj = o_text_read (toplevel, line, tb, release_ver, fileformat_ver);
        g_free (line);
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_PATH):
        line = g_strdup(line);
        new_obj = o_path_read (toplevel, line, tb, release_ver, fileformat_ver);
        g_free (line);
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_PIN):
        new_obj = o_pin_read (toplevel, line, release_ver, fileformat_ver);
        object_list = g_list_prepend (object_list, new_obj);
        found_pin++;
        break;

      case(OBJ_ARC):
        new_obj = o_arc_read (toplevel, line, release_ver, fileformat_ver);
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(STARTATTACH_ATTR): 
        /* first is the fp */
        /* 2nd is the object to get the attributes */
        new_obj_list = o_read_attribs (toplevel, NULL, new_obj, tb, release_ver, fileformat_ver);
        new_obj_list = g_list_reverse (new_obj_list);
        object_list = g_list_concat (new_obj_list, object_list);

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
        if (new_obj != NULL &&
            (new_obj->type == OBJ_COMPLEX ||
             new_obj->type == OBJ_PLACEHOLDER)) {
          o_attrib_slot_update (toplevel, new_obj);
        }

        new_obj = NULL;
        break;

      case(START_EMBEDDED):
        new_obj = object_list->data;

        if (new_obj != NULL &&
            (new_obj->type == OBJ_COMPLEX ||
             new_obj->type == OBJ_PLACEHOLDER)) {

          object_list_save = object_list;
          object_list = new_obj->complex->prim_objs;

          embedded_level++;
        } else {
          fprintf(stderr, _("Read unexpected embedded "
                            "symbol start marker in [%s] :\n>>\n%s<<\n"),
                          name, line);
        }
        break;

      case(END_EMBEDDED):
        if (embedded_level>0) {
          /* don't do this since objects are already
           * stored/read translated
           * o_complex_translate_world (toplevel, object_list->x,
           *                            object_list->y, object_list->complex);
           */

          new_obj = object_list_save->data;
          new_obj->complex->prim_objs = object_list;
          object_list = object_list_save;

          /* set the parent field now */
          for (iter = new_obj->complex->prim_objs;
               iter != NULL; iter = g_list_next (iter)) {
            OBJECT *tmp = iter->data;
            tmp->parent = new_obj;
          }

          o_recalc_single_object (toplevel, new_obj);

          embedded_level--;
        } else {
          fprintf(stderr, _("Read unexpected embedded "
                            "symbol end marker in [%s] :\n>>\n%s<<\n"),
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
        if (release_ver <= VERSION_20030921 || itemsread == 1) {
          fileformat_ver = 0;
        }

        if (fileformat_ver == 0) {
          s_log_message(_("Read an old format sym/sch file!\n"
                          "Please run g[sym|sch]update on:\n[%s]\n"), name);
        }
        break;

      default:
        fprintf(stderr, _("Read garbage in [%s] :\n>>\n%s<<\n"),
                name, line);
        new_obj = NULL;
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
      o_pin_update_whichend (toplevel, object_list, found_pin);
    }
  }

  tb = s_textbuffer_free(tb);

  object_list = g_list_reverse (object_list);

  return(object_list);

}

/*! \brief Read a file
 *  \par Function Description
 *  This function reads a file in libgeda format.
 *
 *  \param [in,out] toplevel    The current TOPLEVEL structure.
 *  \param [in]     object_list  The object_list to read data to.
 *  \param [in]     filename     The filename to read from.
 *  \param [in,out] err          #GError structure for error reporting, or
 *                               NULL to disable error reporting
 *  \return object_list if successful read, or NULL on error.
 */
GList *o_read (TOPLEVEL *toplevel, GList *object_list, char *filename,
               GError **err)
{
  char *buffer = NULL;
  size_t size;
  GList *result;

  /* Return NULL if error reporting is enabled and the return location
   * for an error isn't NULL. */
  g_return_val_if_fail (err == NULL || *err == NULL, NULL);

  if (!g_file_get_contents(filename, &buffer, &size, err)) {
    return NULL;
  } 

  /* Parse file contents */
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
void o_scale (TOPLEVEL *toplevel, GList *list, int x_scale, int y_scale)
{
  OBJECT *o_current;
  GList *iter;

  /* this is okay if you just hit scale and have nothing selected */
  if (list == NULL) {
    return;
  }

  iter = list;
  while (iter != NULL) {
    o_current = (OBJECT *)iter->data;
    switch(o_current->type) {
      case(OBJ_LINE):
        o_line_scale_world(toplevel, x_scale, y_scale, o_current);
        break;
    }
    iter = g_list_next (iter);
  }
}


