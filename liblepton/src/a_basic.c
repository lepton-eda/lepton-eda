/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2020 Lepton EDA Contributors
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

/*! \file a_basic.c
 *  \brief basic libgeda read and write functions
 */
#include <config.h>
#include <version.h>

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "libgeda_priv.h"

/*! \brief Save a file
 *  \par Function Description
 *  This function saves the data in a libgeda format to a file
 *
 *  \bug g_access introduces a race condition in certain cases, but
 *  solves bug #698565 in the normal use-case
 *
 *  \param [in] toplevel    The current TOPLEVEL.
 *  \param [in] object_list The head of a GList of OBJECTs to save.
 *  \param [in] filename    The filename to save the data to.
 *  \param [in,out] err     #GError structure for error reporting.
 *  \return 1 on success, 0 on failure.
 */
int o_save (TOPLEVEL *toplevel, const GList *object_list,
            const char *filename, GError **err)
{
  char *buffer;

  /* Check to see if real filename is writable; if file doesn't exists
     we assume all is well */
  if (g_file_test(filename, G_FILE_TEST_EXISTS) &&
      g_access(filename, W_OK) != 0) {
    g_set_error (err, G_FILE_ERROR, G_FILE_ERROR_PERM,
                 _("File %1$s is read-only"), filename);
    return 0;
  }

  buffer = geda_object_list_to_buffer (object_list);
  if (!g_file_set_contents (filename, buffer, strlen(buffer), err)) {
    g_free (buffer);
    return 0;
  }
  g_free (buffer);

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
 *  \return GList of objects if successful read, or NULL on error.
 */
GList *o_read_buffer (TOPLEVEL *toplevel, GList *object_list,
                      char *buffer, const int size,
                      const char *name, GError **err)
{
  const char *line = NULL;
  TextBuffer *tb = NULL;

  char objtype;
  GList *object_list_save=NULL;
  OBJECT *new_obj=NULL;
  GList *new_attrs_list;
  GList *new_object_list = NULL;
  GList *iter;
  unsigned int release_ver = 0;
  unsigned int fileformat_ver = 0;
  int found_pin = 0;
  OBJECT* last_component = NULL;
  int itemsread = 0;

  int embedded_level = 0;

  g_return_val_if_fail ((buffer != NULL), NULL);

  /* Check the buffer is valid UTF-8 */
  if (!g_utf8_validate (buffer, (size < 0) ? -1 : size, NULL)) {
    g_set_error (err, EDA_ERROR, EDA_ERROR_UNKNOWN_ENCODING,
                 _("Schematic data was not valid UTF-8"));
    return NULL;
  }

  tb = s_textbuffer_new (buffer, size, name);

  while (1) {

    line = s_textbuffer_next_line(tb);
    if (line == NULL) break;

    sscanf(line, "%c", &objtype);

    /* Do we need to check the symbol version?  Yes, but only if */
    /* 1) the last object read was a component and */
    /* 2) the next object isn't the start of attributes.  */
    /* If the next object is the start of attributes, then check the */
    /* symbol version after the attributes have been read in, see the */
    /* STARTATTACH_ATTR case */
    if (last_component && objtype != STARTATTACH_ATTR)
    {
        /* yes */
        /* verify symbol version (not file format but rather contents) */
        o_component_check_symversion(toplevel, last_component);
        last_component = NULL;  /* no longer need to check */
    }

    switch (objtype) {

      case(OBJ_LINE):
        if ((new_obj = o_line_read (toplevel, line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;


      case(OBJ_NET):
        if ((new_obj = o_net_read (toplevel, line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(OBJ_BUS):
        if ((new_obj = o_bus_read (line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(OBJ_BOX):
        if ((new_obj = o_box_read (line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(OBJ_PICTURE):
        new_obj = o_picture_read (toplevel, line, tb, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(OBJ_CIRCLE):
        if ((new_obj = o_circle_read (line, release_ver, fileformat_ver, err)) == NULL)
	  goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(OBJ_COMPONENT):
      case(OBJ_PLACEHOLDER):
        if ((new_obj = o_component_read (toplevel, line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);

        /* last_component is used for verifying symversion attribute */
        last_component = new_obj;
        break;

      case(OBJ_TEXT):
        new_obj = o_text_read (toplevel, line, tb, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(OBJ_PATH):
        new_obj = o_path_read (toplevel, line, tb, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(OBJ_PIN):
        if ((new_obj = o_pin_read (toplevel, line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        found_pin++;
        break;

      case(OBJ_ARC):
        if ((new_obj = o_arc_read (line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        new_object_list = g_list_prepend (new_object_list, new_obj);
        break;

      case(STARTATTACH_ATTR):
        /* first is the fp */
        /* 2nd is the object to get the attributes */
        if (new_obj != NULL) {
          new_attrs_list = o_read_attribs (toplevel, new_obj, tb, release_ver, fileformat_ver, err);
          if (new_attrs_list == NULL)
            goto error;
          new_object_list = g_list_concat (new_attrs_list, new_object_list);

          /* by now we have finished reading all the attributes */
          /* did we just finish attaching to a component object? */
          if (last_component)
          {
            /* yes */
            /* verify symbol version (not file format but rather contents) */
            o_component_check_symversion(toplevel, last_component);
            last_component = NULL;
          }

          /* slots only apply to component objects */
          if (new_obj != NULL &&
              (new_obj->type == OBJ_COMPONENT ||
               new_obj->type == OBJ_PLACEHOLDER)) {
            s_slot_update_object (new_obj);
          }
          new_obj = NULL;
        }
        else {
          g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE, _("Read unexpected attach "
                                                                 "symbol start marker in [%1$s] :\n>>\n%2$s<<\n"),
                       name, line);
          goto error;
        }
        break;

      case(START_EMBEDDED):
        if (new_obj != NULL &&
            (new_obj->type == OBJ_COMPONENT ||
             new_obj->type == OBJ_PLACEHOLDER)) {

          object_list_save = new_object_list;
          new_object_list = new_obj->component->prim_objs;

          embedded_level++;
        } else {
          g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE, _("Read unexpected embedded "
                                                                 "symbol start marker in [%1$s] :\n>>\n%2$s<<\n"),
                       name, line);
          goto error;
        }
        break;

      case(END_EMBEDDED):
        if (embedded_level>0) {
          /* don't do this since objects are already
           * stored/read translated
           * geda_component_object_translate (toplevel, new_object_list->x,
           *                                  new_object_list->y, new_object_list->component);
           */
          new_object_list = g_list_reverse (new_object_list);

          new_obj = (OBJECT*) object_list_save->data;
          new_obj->component->prim_objs = new_object_list;
          new_object_list = object_list_save;

          /* set the parent field now */
          for (iter = new_obj->component->prim_objs;
               iter != NULL; iter = g_list_next (iter)) {
            OBJECT *tmp = (OBJECT*) iter->data;
            tmp->parent = new_obj;
          }

          embedded_level--;
        } else {
          g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE, _("Read unexpected embedded "
                                                                 "symbol end marker in [%s] :\n>>\n%s<<\n"),
                       name, line);
          goto error;
        }
        break;

      case(ENDATTACH_ATTR):
        /* this case is never hit, since the } is consumed by o_read_attribs */
        break;

      case(INFO_FONT):
        /* NOP */
        break;

      case(COMMENT):
        /* do nothing */
        break;

      case(VERSION_CHAR):
        itemsread = sscanf(line, "v %u %u\n", &release_ver, &fileformat_ver);

        if (itemsread == 0) {
          g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE, "Failed to parse version from buffer.");
          goto error;
        }

        /* 20030921 was the last version which did not have a fileformat */
        /* version.  The below latter test should not happen, but it is here */
        /* just in in case. */
        if (release_ver <= VERSION_20030921 || itemsread == 1) {
          fileformat_ver = 0;
        }

        if (fileformat_ver == 0) {
          s_log_message(_("Read an old format sym/sch file!\n"
                          "Please run g[sym|sch]update on:\n[%1$s]"), name);
        }
        break;

      default:
        g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE, _("Read garbage in [%1$s] :\n>>\n%2$s<<\n"), name, line);
        new_obj = NULL;
        goto error;
    }

  }

  /* Was the very last thing we read a component and has it not been checked */
  /* yet?  This would happen if the component is at the very end of the file  */
  /* and had no attached attributes */
  if (last_component)
  {
        o_component_check_symversion(toplevel, last_component);
        last_component = NULL;  /* no longer need to check */
  }

  if (found_pin) {
    if (release_ver <= VERSION_20020825) {
      geda_pin_object_update_whichend (toplevel, new_object_list, found_pin);
    }
  }

  s_textbuffer_free(tb);

  new_object_list = g_list_reverse(new_object_list);
  object_list = g_list_concat (object_list, new_object_list);

  return(object_list);

error:
  geda_object_list_delete (new_object_list);

  unsigned long linenum = s_textbuffer_linenum (tb);
  g_prefix_error (err, "Parsing stopped at line %lu:\n", linenum);

  return NULL;
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
  result = o_read_buffer (toplevel, object_list, buffer, size, filename, err);
  g_free (buffer);
  return result;
}
