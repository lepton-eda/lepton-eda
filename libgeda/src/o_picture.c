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
#include <math.h>

/* for basename(3) */
#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif

#include <gtk/gtk.h>
#include <guile/gh.h>

#include <gdk/gdk.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gdk-pixbuf/gdk-pixdata.h>


#include "libgeda_priv.h"

/*! \brief Create picture OBJECT from character string.
 *  \par Function Description
 *  This function will get the description of a picture from the
 *  character string <B>*first_line</B>. The new picture is then added
 *  to the list of object of which <B>*object_list</B> is the last
 *  element before the call.  The function returns the new last
 *  element, that is the added picture object.
 *
 *  \param [in]  toplevel       The TOPLEVEL object.
 *  \param [out] object_list     OBJECT list to create picture in.
 *  \param [in]  first_line      Character string with picture description.
 *  \param [in]  tb              Text buffer to load embedded data from.
 *  \param [in]  release_ver     libgeda release version number.
 *  \param [in]  fileformat_ver  libgeda file format version number.
 *  \return A pointer to the new picture object.
 */
OBJECT *o_picture_read(TOPLEVEL *toplevel, OBJECT *object_list,
		       const char *first_line,
		       TextBuffer *tb,
		       unsigned int release_ver,
		       unsigned int fileformat_ver)
{
  int x1, y1;
  int width, height, angle;
  gchar mirrored, embedded;
  int num_conv;
  gchar type;
  gchar *line = NULL;
  gchar *filename;
  GdkPixbuf *pixbuf = NULL;
  gchar *file_content = NULL;
  guint file_length = 0;
  GError *err = NULL;
  static char gdk_initialized=0;

  /* Initialize GDK first in case this isn't a graphic app */
  if (gdk_initialized == 0) {
    gdk_init(NULL, NULL);
    gdk_initialized = 1;
  }

  num_conv = sscanf(first_line, "%c %d %d %d %d %d %c %c\n",
	 &type, &x1, &y1, &width, &height, &angle, &mirrored, &embedded);
  
  if (num_conv != 8) {
    s_log_message (_("Error reading picture definition line: %s.\n"),
                   first_line);
  }

  /* Convert from ascii character to number */
  if (g_ascii_isdigit(mirrored)) {
    mirrored -= 0x30;
  }

  if (g_ascii_isdigit(embedded)) {
    embedded -= 0x30;
  }

  if (width == 0 || height == 0) {
    s_log_message(_("Found a zero width/height picture [ %c %d %d %d %d ]\n"),
                  type, x1, y1, width, height);
  }

  if ( (mirrored > 1) || (mirrored < 0)) {
    s_log_message(_("Found a picture with a wrong 'mirrored' parameter: %c.\n"),
	    mirrored);
    s_log_message(_("Setting mirrored to 0\n"));
    mirrored = 0;
  }

  if ( (embedded > 1) || (embedded < 0)) {
    s_log_message(_("Found a picture with a wrong 'embedded' parameter: %c.\n"),
	    embedded);
    s_log_message(_("Setting embedded to 0\n"));
    embedded = 0;
  }
  switch(angle) {
	
    case(0):
    case(90):
    case(180):
    case(270):
    break;

    default:
      s_log_message(_("Found an unsupported picture angle [ %d ]\n"), angle);
      s_log_message(_("Setting angle to 0\n"));
      angle=0;
      break;

  }

  filename = g_strdup(s_textbuffer_next_line(tb));
  filename = remove_last_nl(filename);	

  if (embedded == 1) {
    GString *encoded_picture=g_string_new("");
    char finished = 0;

    /* Read the encoded picture */
    do {

      line = s_textbuffer_next_line(tb);
      if (line == NULL) break;

      if (g_strcasecmp(line, ".\n") != 0) {
        encoded_picture = g_string_append (encoded_picture, line);
      } else {
        finished = 1;
      }
    } while (finished == 0);

    /* Decode the picture */
    file_content = s_encoding_base64_decode(encoded_picture->str,
                                            encoded_picture->len,
                                            &file_length);
    if (encoded_picture != NULL) {
      g_string_free (encoded_picture, TRUE);
    }

    if (file_content == NULL) {
      s_log_message (_("Failed to load image from embedded data [%s]: %s\n"),
                     filename, _("Base64 decoding failed."));
      s_log_message (_("Falling back to file loading. Picture unembedded.\n"));
      embedded = 0;
    }
  }

  /* If we have embedded data, try loading from the decoded buffer */
  if (file_content != NULL) {
    pixbuf = o_picture_pixbuf_from_buffer (file_content, file_length, &err);
    if (err != NULL) {
      s_log_message (_("Failed to load image from embedded data [%s]: %s\n"),
                     filename, err->message);
      s_log_message (_("Falling back to file loading. Picture unembedded.\n"));
      g_error_free (err);
      err = NULL;
      embedded = 0;
    }
  }

  /* If we haven't loaded the pixbuf above, try loading from file */
  if (pixbuf == NULL) {
    pixbuf = gdk_pixbuf_new_from_file (filename, &err);
    if (err != NULL) {
      s_log_message (_("Failed to load image from file [%s]: %s\n"),
                     filename, err->message);
      g_error_free (err);
      err = NULL;
    }
  }

  /* If the pixbuf couldn't be loaded, then try to load a warning picture */
  if (pixbuf == NULL) {
    char *temp_filename;

    s_log_message (_("Loading warning picture.\n"));
    
    temp_filename = g_build_filename (toplevel->bitmap_directory,
                                      "gschem-warning.png", NULL);
    pixbuf = gdk_pixbuf_new_from_file (temp_filename, NULL);
    if (pixbuf == NULL) {
      s_log_message( _("Error loading picture from file: %s.\n"),
                     temp_filename);
    }      
    g_free (temp_filename);
  }
  
  /* create and add the picture to the list */
  /* The picture is described by its upper left and lower right corner */
  object_list = o_picture_add(toplevel, object_list, pixbuf,
                              file_content, file_length, filename,
                              (double)height/ width,
                              type,
                              x1, y1+height, x1+width, y1,
                              angle, mirrored, embedded);

  /* Don't free file_content, it is now owned by the picture object */

  return(object_list);
}


/*! \brief Create a character string representation of a picture OBJECT.
 *  \par Function Description
 *  This function formats a string in the buffer <B>*buff</B> to describe
 *  the picture object <B>*object</B>.
 *
 *  \param [in] object  Picture OBJECT to create string from.
 *  \return A pointer to the picture OBJECT character string.
 *
 *  \note
 *  Caller must g_free returned character string.
 *
 */
char *o_picture_save(OBJECT *object)
{
  int width, height, x1, y1;
  gchar *encoded_picture=NULL;
  gchar *out=NULL;
  guint encoded_picture_length;

  /* calculate the width and height of the box */
  width  = abs(object->picture->lower_x - object->picture->upper_x); 
  height = abs(object->picture->upper_y - object->picture->lower_y);

  /* calculate the lower left corner of the box */
  x1 = object->picture->upper_x;
  y1 = object->picture->upper_y - height; /* move the origin to 0, 0*/

#if DEBUG
  printf("picture: %d %d %d %d\n", x1, y1, width, height);
#endif

  /* Encode the picture if it's embedded */
  if (object->picture->embedded == 1) {
    encoded_picture =
      s_encoding_base64_encode( (char *)object->picture->file_content,
                                object->picture->file_length,
                                &encoded_picture_length,
                                TRUE);
    if (encoded_picture == NULL) {
      s_log_message(_("ERROR: o_picture_save: unable to encode the picture.\n"));
    }
  }

  if (object->picture->embedded==1 &&
      encoded_picture != NULL) {
    out = g_strdup_printf("%c %d %d %d %d %d %c %c\n%s\n%s\n%s", 
			  object->type,
			  x1, y1, width, height,
			  object->picture->angle,
			  /* Convert the (0,1) chars to ASCII */
			  (object->picture->mirrored)+0x30, 
			  object->picture->embedded+0x30, 
			  object->picture->filename,
			  encoded_picture,
			  ".");
  }
  else {
    out = g_strdup_printf("%c %d %d %d %d %d %c %c\n%s", 
			  object->type,
			  x1, y1, width, height,
			  object->picture->angle,
			  /* Convert the (0,1) chars to ASCII */
			  (object->picture->mirrored)+0x30, 
			  object->picture->embedded+0x30, 
			  object->picture->filename);
  }
  g_free(encoded_picture);

  return(out);
}


/*! \brief Create and add picture OBJECT to list.
 *  \par Function Description
 *  This function creates a new object representing a picture.
 *  This object is added to the end of the list <B>list_tail</B> pointed
 *  object belongs to.
 *  The picture is described by its upper left corner - <B>x1</B>, <B>y1</B> -
 *  and its lower right corner - <B>x2</B>, <B>y2</B>.
 *  The <B>type</B> parameter must be equal to #OBJ_PICTURE. 
 *
 *  The #OBJECT structure is allocated with the
 *  #s_basic_init_object() function. The structure describing the
 *  picture is allocated and initialized with the parameters given to the
 *  function.
 *
 *  The object is added to the end of the list described by the
 *  <B>object_list</B> parameter by the #s_basic_link_object().
 *
 *  \param [in]     toplevel      The TOPLEVEL object.
 *  \param [in,out] list_tail     OBJECT list to add line to.
 *  \param [in]     pixbuf        The GdkPixbuf picture to add.
 *                                A copy of this pixbuf is made.
 *  \param [in]     file_content  Raw data of the image file.
 *                                NULL for non embedded loading. The object
 *                                object takes ownership of this buffer, and it
 *                                should not be free'd by the caller.
 *  \param [in]     file_length   Length of raw data buffer
 *  \param [in]     filename      File name backing this picture.
 *                                A copy of this string is made.
 *  \param [in]     ratio         Picture height to width ratio.
 *  \param [in]     type          Must be OBJ_PICTURE.
 *  \param [in]     x1            Upper x coordinate.
 *  \param [in]     y1            Upper y coordinate.
 *  \param [in]     x2            Lower x coordinate.
 *  \param [in]     y2            Lower y coordinate.
 *  \param [in]     angle         Picture rotation angle.
 *  \param [in]     mirrored      Whether the image should be mirrored or not.
 *  \param [in]     embedded      Whether the embedded flag should be set or not.
 *  \return A pointer to the new end of the object list.
 */
OBJECT *o_picture_add(TOPLEVEL *toplevel, OBJECT *list_tail, GdkPixbuf *pixbuf,
                      gchar *file_content, gsize file_length, char *filename,
                      double ratio, char type,
                      int x1, int y1, int x2, int y2, int angle, char mirrored,
                      char embedded)
{
  OBJECT *new_node;
  PICTURE *picture;

  /* create the object */
  new_node        = s_basic_init_object(type, "picture");

  picture = (PICTURE *) g_malloc(sizeof(PICTURE));
  new_node->picture = picture;

  /* describe the picture with its upper left and lower right corner */
  picture->upper_x = x1;
  picture->upper_y = y1;
  picture->lower_x = x2;
  picture->lower_y = y2;

  picture->file_content = file_content;
  picture->file_length  = file_length;
  picture->filename = g_strdup (filename);
  picture->ratio = ratio;
  picture->original_picture = gdk_pixbuf_copy(pixbuf);
  picture->displayed_picture = NULL;
  picture->angle = angle;
  picture->mirrored = mirrored;
  picture->embedded = embedded;

  new_node->draw_func = picture_draw_func;
  new_node->sel_func  = select_func;

  /* compute the bounding picture */
  o_picture_recalc(toplevel, new_node);

  /* add the object to the list */
  list_tail = (OBJECT *) s_basic_link_object(new_node, list_tail);

  return(list_tail);
}

/*! \brief Recalculate picture bounding box.
 *  \par Function Description
 *  This function recalculates the bounding box of the <B>o_current</B>
 *  parameter picture object.
 *
 *  \param [in] toplevel      The TOPLEVEL object.
 *  \param [in,out] o_current  Picture OBJECT to be recalculated.
 */
void o_picture_recalc(TOPLEVEL *toplevel, OBJECT *o_current)
{
  int left, top, right, bottom;

  if (o_current->picture == NULL) {
    return;
  }

  /* update the bounding picture - world units */
  world_get_picture_bounds(toplevel, o_current,
		     &left, &top, &right, &bottom);
  o_current->w_left   = left;
  o_current->w_top    = top;
  o_current->w_right  = right;
  o_current->w_bottom = bottom;
  
}

/*! \brief Get picture bounding rectangle in WORLD coordinates.
 *  \par Function Description
 *  This function sets the <B>left</B>, <B>top</B>, <B>right</B> and
 *  <B>bottom</B> parameters to the boundings of the picture object
 *  described in <B>*picture</B> in WORLD units.
 *
 *  \param [in]  toplevel  The TOPLEVEL object.
 *  \param [in]  object     Picture OBJECT to read coordinates from.
 *  \param [out] left       Left picture coordinate in WORLD units.
 *  \param [out] top        Top picture coordinate in WORLD units.
 *  \param [out] right      Right picture coordinate in WORLD units.
 *  \param [out] bottom     Bottom picture coordinate in WORLD units.
 */
void world_get_picture_bounds(TOPLEVEL *toplevel, OBJECT *object,
                              int *left, int *top, int *right, int *bottom)
{
  *left   = min(object->picture->upper_x, object->picture->lower_x);
  *top    = min(object->picture->upper_y, object->picture->lower_y);
  *right  = max(object->picture->upper_x, object->picture->lower_x);
  *bottom = max(object->picture->upper_y, object->picture->lower_y);

}
                 
/*! \brief Modify the description of a picture OBJECT.
 *  \par Function Description
 *  This function modifies the coordinates of one of the four corner of
 *  the picture. The new coordinates of the corner identified by
 *  <B>whichone</B> are given by <B>x</B> and <B>y</B> in world unit.
 *
 *  The coordinates of the corner is modified in the world coordinate system.
 *  Screen coordinates and boundings are then updated.
 *
 *  \param [in]     toplevel  The TOPLEVEL object.
 *  \param [in,out] object     Picture OBJECT to modify.
 *  \param [in]     x          New x coordinate.
 *  \param [in]     y          New y coordinate.
 *  \param [in]     whichone   Which picture parameter to modify.
 *
 *  <B>whichone</B> can have the following values:
 *  <DL>
 *    <DT>*</DT><DD>PICTURE_UPPER_LEFT
 *    <DT>*</DT><DD>PICTURE_LOWER_LEFT
 *    <DT>*</DT><DD>PICTURE_UPPER_RIGHT
 *    <DT>*</DT><DD>PICTURE_LOWER_RIGHT
 *  </DL>
 *
 *  \par Author's note
 *  pb20011002 - rewritten : old one did not used x, y and whichone
 */
void o_picture_modify(TOPLEVEL *toplevel, OBJECT *object,
		      int x, int y, int whichone)
{
  int tmp;
  
  /* change the position of the selected corner */
  switch(whichone) {
    case PICTURE_UPPER_LEFT:
      object->picture->upper_x = x;
      tmp = abs(object->picture->upper_x - object->picture->lower_x) / 
	object->picture->ratio;
      if (y < object->picture->lower_y) {
	tmp = -tmp;
      }
      object->picture->upper_y = object->picture->lower_y + tmp;
      break;
			
    case PICTURE_LOWER_LEFT:
      object->picture->upper_x = x;
      tmp = abs(object->picture->upper_x - object->picture->lower_x) / 
	object->picture->ratio;
      if (y > object->picture->upper_y) {
	tmp = -tmp;
      }
      object->picture->lower_y = object->picture->upper_y - tmp;
      break;
      
    case PICTURE_UPPER_RIGHT:
      object->picture->lower_x = x;
      tmp = abs(object->picture->upper_x - object->picture->lower_x) / 
	object->picture->ratio;
      if (y < object->picture->lower_y) {
	tmp = -tmp;
      }
      object->picture->upper_y = object->picture->lower_y + tmp;
      break;
      
    case PICTURE_LOWER_RIGHT:
      object->picture->lower_x = x;
      tmp = abs(object->picture->upper_x - object->picture->lower_x) / 
	object->picture->ratio;
      if (y > object->picture->upper_y) {
	tmp = -tmp;
      }
      object->picture->lower_y = object->picture->upper_y - tmp;
      break;
      
    default:
      return;
  }
  
  /* need to update the upper left and lower right corners */
  if(object->picture->upper_x > object->picture->lower_x) {
    tmp                      = object->picture->upper_x;
    object->picture->upper_x = object->picture->lower_x;
    object->picture->lower_x = tmp;
  }
  
  if(object->picture->upper_y < object->picture->lower_y) {
    tmp                      = object->picture->upper_y;
    object->picture->upper_y = object->picture->lower_y;
    object->picture->lower_y = tmp;
  }
	
  /* recalculate the screen coords and the boundings */
  o_picture_recalc(toplevel, object);
}

/*! \brief Rotate picture OBJECT using WORLD coordinates.
 *  \par Function Description 
 *  This function rotates the picture described by <B>*object</B> around
 *  the (<B>world_centerx</B>, <B>world_centery</B>) point by <B>angle</B>
 *  degrees.
 *  The center of rotation is in world units.
 *
 *  \param [in]      toplevel      The TOPLEVEL object.
 *  \param [in]      world_centerx  Rotation center x coordinate in
 *                                  WORLD units.
 *  \param [in]      world_centery  Rotation center y coordinate in
 *                                  WORLD units.
 *  \param [in]      angle          Rotation angle in degrees (See note below).
 *  \param [in,out]  object         Picture OBJECT to rotate.
 */
void o_picture_rotate_world(TOPLEVEL *toplevel,
			    int world_centerx, int world_centery, int angle,
			    OBJECT *object)
{
  int newx1, newy1;
  int newx2, newy2;
  
  /* Only 90 degree multiple and positive angles are allowed. */
  /* angle must be positive */
  if(angle < 0) angle = -angle;
  /* angle must be a 90 multiple or no rotation performed */
  if((angle % 90) != 0) return;
  
  object->picture->angle = ( object->picture->angle + angle ) % 360;
	
  /* The center of rotation (<B>world_centerx</B>, <B>world_centery</B>) is
   * translated to the origin. The rotation of the upper left and lower
   * right corner are then performed. Finally, the rotated picture is
   * translated back to its previous location.
   */
  /* translate object to origin */
  object->picture->upper_x -= world_centerx;
  object->picture->upper_y -= world_centery;
  object->picture->lower_x -= world_centerx;
  object->picture->lower_y -= world_centery;
  
  /* rotate the upper left corner of the picture */
  rotate_point_90(object->picture->upper_x, object->picture->upper_y, angle,
		  &newx1, &newy1);
  
  /* rotate the lower left corner of the picture */
  rotate_point_90(object->picture->lower_x, object->picture->lower_y, angle,
		  &newx2, &newy2);
  
  /* reorder the corners after rotation */
  object->picture->upper_x = min(newx1,newx2);
  object->picture->upper_y = max(newy1,newy2);
  object->picture->lower_x = max(newx1,newx2);
  object->picture->lower_y = min(newy1,newy2);
  
  /* translate object back to normal position */
  object->picture->upper_x += world_centerx;
  object->picture->upper_y += world_centery;
  object->picture->lower_x += world_centerx;
  object->picture->lower_y += world_centery;
  
  /* recalc boundings and screen coords */
  o_picture_recalc(toplevel, object);
	
}

/*! \brief Mirror a picture using WORLD coordinates.
 *  \par Function Description
 *  This function mirrors the picture from the point
 *  (<B>world_centerx</B>,<B>world_centery</B>) in world unit.
 *
 *  The picture is first translated to the origin, then mirrored and
 *  finally translated back at its previous position.
 *
 *  \param [in]     toplevel      The TOPLEVEL object.
 *  \param [in]     world_centerx  Origin x coordinate in WORLD units.
 *  \param [in]     world_centery  Origin y coordinate in WORLD units.
 *  \param [in,out] object         Picture OBJECT to mirror.
 */
void o_picture_mirror_world(TOPLEVEL *toplevel,
			    int world_centerx, int world_centery,
			    OBJECT *object)
{
  int newx1, newy1;
  int newx2, newy2;

  
  /* Set info in object */
  object->picture->mirrored = (object->picture->mirrored ^ 1) & 1;

  /* translate object to origin */
  object->picture->upper_x -= world_centerx;
  object->picture->upper_y -= world_centery;
  object->picture->lower_x -= world_centerx;
  object->picture->lower_y -= world_centery;

  /* mirror the corners */
  newx1 = -object->picture->upper_x;
  newy1 = object->picture->upper_y;
  newx2 = -object->picture->lower_x;
  newy2 = object->picture->lower_y;

  /* reorder the corners */
  object->picture->upper_x = min(newx1,newx2);
  object->picture->upper_y = max(newy1,newy2);
  object->picture->lower_x = max(newx1,newx2);
  object->picture->lower_y = min(newy1,newy2);

  /* translate back in position */
  object->picture->upper_x += world_centerx;
  object->picture->upper_y += world_centery;
  object->picture->lower_x += world_centerx;
  object->picture->lower_y += world_centery;

  /* recalc boundings and screen coords */
  o_picture_recalc(toplevel, object);
  
}

/*! \brief Translate a picture position in WORLD coordinates by a delta.
 *  \par Function Description
 *  This function applies a translation of (<B>x1</B>,<B>y1</B>) to the picture
 *  described by <B>*object</B>. <B>x1</B> and <B>y1</B> are in world units.
 *
 *  \param [in]     toplevel  The TOPLEVEL object.
 *  \param [in]     x1         x distance to move.
 *  \param [in]     y1         y distance to move.
 *  \param [in,out] object     Picture OBJECT to translate.
 */
void o_picture_translate_world(TOPLEVEL *toplevel,
			       int x1, int y1, OBJECT *object)
{
  if (object == NULL) printf("btw NO!\n");

  /* Do world coords */
  object->picture->upper_x = object->picture->upper_x + x1;
  object->picture->upper_y = object->picture->upper_y + y1;
  object->picture->lower_x = object->picture->lower_x + x1;
  object->picture->lower_y = object->picture->lower_y + y1;     
  
  /* recalc the screen coords and the bounding picture */
  o_picture_recalc(toplevel, object);
}

/*! \brief Create a copy of a picture.
 *  \par Function Description
 *  This function creates a verbatim copy of the object pointed by
 *  <B>o_current</B> describing a picture. The new object is added at the
 *  end of the list, following the <B>list_tail</B> pointed object.
 *
 *  \param [in]  toplevel   The TOPLEVEL object.
 *  \param [out] list_tail  OBJECT list to copy to.
 *  \param [in]  objcet     Picture OBJECT to copy.
 *  \return A new pointer to the end of the object list.
 */
OBJECT *o_picture_copy(TOPLEVEL *toplevel, OBJECT *list_tail,
		       OBJECT *object)
{
  OBJECT *new_node;
  PICTURE *picture;

  /* create the object */
  new_node = s_basic_init_object(object->type, "picture");

  picture = g_malloc(sizeof(PICTURE));
  new_node->picture = picture;

  if (object->saved_color == -1) {
    new_node->color = object->color;
  } else {
    new_node->color = object->saved_color;
  }

  /* describe the picture with its upper left and lower right corner */
  picture->upper_x = object->picture->upper_x;
  picture->upper_y = object->picture->upper_y;
  picture->lower_x = object->picture->lower_x;
  picture->lower_y = object->picture->lower_y;

  if (object->picture->file_content != NULL) {
    picture->file_content = g_malloc (object->picture->file_length);
    memcpy (picture->file_content, object->picture->file_content,
                                   object->picture->file_length);
  } else {
    picture->file_content = NULL;
  }

  picture->file_length = object->picture->file_length;
  picture->filename    = g_strdup (object->picture->filename);
  picture->ratio       = object->picture->ratio;
  picture->angle       = object->picture->angle;
  picture->mirrored    = object->picture->mirrored;
  picture->embedded    = object->picture->embedded;

  /* Copy the picture data */
  picture->original_picture =
    gdk_pixbuf_copy(object->picture->original_picture);

  picture->displayed_picture =
    gdk_pixbuf_copy(object->picture->displayed_picture);

  new_node->draw_func = object->draw_func;
  new_node->sel_func  = object->sel_func;

  /* compute the bounding picture */
  o_picture_recalc(toplevel, new_node);

  /* add the object to the list */
  list_tail = (OBJECT *) s_basic_link_object(new_node, list_tail);

  /* return the new tail of the object list */
  return(list_tail);
}


/*! \brief Get RGB data from image.
 *  \par Function Description
 *  This function returns the RGB data of the given image.
 *  Function taken from the DIA source code (http://www.gnome.org/projects/dia)
 *  and licensed under the GNU GPL version 2.
 *
 *  \param [in] image  GdkPixbuf image to read RGB data from.
 *  \return Array of rgb data from image.
 *
 *  \note
 *  Caller must g_free returned guint8 array.
 */
guint8 *o_picture_rgb_data(GdkPixbuf *image)
{
  int width = gdk_pixbuf_get_width(image);
  int height = gdk_pixbuf_get_height(image);
  int rowstride = gdk_pixbuf_get_rowstride(image);
  int size = height*rowstride;
  guint8 *rgb_pixels = g_malloc(size);

  if (gdk_pixbuf_get_has_alpha(image)) {
    guint8 *pixels = gdk_pixbuf_get_pixels(image);
    int i, j;
    for (i = 0; i < height; i++) {
      for (j = 0; j < width; j++) {
	rgb_pixels[i*rowstride+j*3] = pixels[i*rowstride+j*4];
	rgb_pixels[i*rowstride+j*3+1] = pixels[i*rowstride+j*4+1];
	rgb_pixels[i*rowstride+j*3+2] = pixels[i*rowstride+j*4+2];
      }
    }
    return rgb_pixels;
  } else {
    guint8 *pixels = gdk_pixbuf_get_pixels(image);

    g_memmove(rgb_pixels, pixels, height*rowstride);
    return rgb_pixels;
  }
}

/*! \brief Get mask data from image.
 *  \par Function Description
 *  This function returns the mask data of the given image.
 *  Function taken from the DIA source code (http://www.gnome.org/projects/dia)
 *  and licensed under the GNU GPL version 2.
 *
 *  \param [in] image  GdkPixbuf image to get mask data from.
 *  \return Array of mask data from image.
 *
 *  \note
 *  Caller must g_free returned guint8 array.
 */
guint8 *o_picture_mask_data(GdkPixbuf *image)
{
  guint8 *pixels;
  guint8 *mask;
  int i, size;

  if (!gdk_pixbuf_get_has_alpha(image)) {
    return NULL;
  }
  
  pixels = gdk_pixbuf_get_pixels(image);

  size = gdk_pixbuf_get_width(image)*
    gdk_pixbuf_get_height(image);

  mask = g_malloc(size);

  /* Pick every fourth byte (the alpha channel) into mask */
  for (i = 0; i < size; i++)
    mask[i] = pixels[i*4+3];

  return mask;
}

/*! \brief Print picture to Postscript document.
 *  \par Function Description
 *  This function prints a picture object. The picture is defined by the
 *  coordinates of its upper left corner in (<B>x</B>,<B>y</B>) and its width
 *  and height given by the <B>width</B> and <B>height</B> parameters. 
 *  The Postscript document is defined by the file pointer <B>fp</B>.
 *  Function based on the DIA source code (http://www.gnome.org/projects/dia)
 *  and licensed under the GNU GPL version 2.
 *
 *  All dimensions are in mils.
 *  
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] fp         FILE pointer to Postscript document.
 *  \param [in] o_current  Picture OBJECT to write to document.
 *  \param [in] origin_x   Page x coordinate to place picture OBJECT.
 *  \param [in] origin_y   Page y coordinate to place picture OBJECT.
 */
void o_picture_print(TOPLEVEL *toplevel, FILE *fp, OBJECT *o_current,
		     int origin_x, int origin_y)
{
  int x1, y1, x, y;
  int height, width;
  GdkPixbuf* image = o_current->picture->original_picture;
  int img_width, img_height, img_rowstride;
  double ratio;
  guint8 *rgb_data;
  guint8 *mask_data;

  /* calculate the width and height of the box */
  width  = abs(o_current->picture->lower_x - o_current->picture->upper_x); 
  height = abs(o_current->picture->upper_y - o_current->picture->lower_y);

  /* calculate the origin of the box */
  x1 = o_current->picture->upper_x;
  y1 = o_current->picture->upper_y;

  img_width = gdk_pixbuf_get_width(image);
  img_rowstride = gdk_pixbuf_get_rowstride(image);
  img_height = gdk_pixbuf_get_height(image);

  rgb_data = o_picture_rgb_data(image);
  mask_data = o_picture_mask_data(image);

  ratio = height/width;

  fprintf(fp, "gsave\n");

  /* color output only */
  fprintf(fp, "/pix %i string def\n", img_width * 3);
  fprintf(fp, "%i %i 8\n", img_width, img_height);
  fprintf(fp, "%i %i translate\n", x1, y1);
  fprintf(fp, "%i %i scale\n", width, height);
  fprintf(fp, "[%i 0 0 -%i 0 0]\n", img_width, img_height);

  fprintf(fp, "{currentfile pix readhexstring pop}\n");
  fprintf(fp, "false 3 colorimage\n");
  fprintf(fp, "\n");

  if (mask_data) {
    for (y = 0; y < img_height; y++) {
      for (x = 0; x < img_width; x++) {
	int i = y*img_rowstride+x*3;
	int m = y*img_width+x;
        fprintf(fp, "%02x", 255-(mask_data[m]*(255-rgb_data[i])/255));
        fprintf(fp, "%02x", 255-(mask_data[m]*(255-rgb_data[i+1])/255));
        fprintf(fp, "%02x", 255-(mask_data[m]*(255-rgb_data[i+2])/255));
      }
      fprintf(fp, "\n");
    }
  } else {
    for (y = 0; y < img_height; y++) {
      for (x = 0; x < img_width; x++) {
	int i = y*img_rowstride+x*3;
        fprintf(fp, "%02x", (int)(rgb_data[i]));
        fprintf(fp, "%02x", (int)(rgb_data[i+1]));
        fprintf(fp, "%02x", (int)(rgb_data[i+2]));
      }
      fprintf(fp, "\n");
    }
  }
  fprintf(fp, "grestore\n");
  fprintf(fp, "\n");
   
  g_free(rgb_data);
  g_free(mask_data);

	
}


/*! \brief Embed the image file associated with a picture
 *
 *  \par Function Description
 *  This function reads and embeds image file associated with the picture.
 *
 *  \param [in]     toplevel     The TOPLEVEL object.
 *  \param [in]     object       The picture OBJECT to embed
 */
void o_picture_embed (TOPLEVEL *toplevel, OBJECT *object)
{
  GError *err = NULL;
  GdkPixbuf *pixbuf;
  gchar *filename;

  /* Free any existing embedded data */
  g_free (object->picture->file_content);
  object->picture->file_content = NULL;

  g_file_get_contents (object->picture->filename,
                       &object->picture->file_content,
                       &object->picture->file_length,
                       &err);
  if (err != NULL) {
    s_log_message (_("Failed to load image from file [%s]: %s\n"),
                   object->picture->filename, err->message);
    g_error_free (err);
    return;
  }

  object->picture->embedded = 1;

  pixbuf = o_picture_pixbuf_from_buffer (object->picture->file_content,
                                         object->picture->file_length,
                                         &err);
  if (err != NULL) {
    s_log_message (_("Failed to load image from embedded data [%s]: %s\n"),
                   object->picture->filename, err->message);
    s_log_message (_("Falling back to file loading. Picture unembedded.\n"));
    g_error_free (err);
    object->picture->embedded = 0;
    return;
  }

  /* Change to the new pixbuf loaded before we embedded. */
  if (object->picture->original_picture != NULL)
    g_object_unref(object->picture->original_picture);

  object->picture->original_picture = pixbuf;

  filename = g_path_get_basename(object->picture->filename);
  s_log_message (_("Picture [%s] has been embedded\n"), filename);
  g_free(filename);
}


/*! \brief Unembed a picture, reloading the image from disk
 *
 *  \par Function Description
 *  This function re-reads the image file associated with the picture, and
 *  discards the embeded copy of the file.
 *
 *  \param [in]     toplevel     The TOPLEVEL object.
 *  \param [in]     object       The picture OBJECT to unembed
 */
void o_picture_unembed (TOPLEVEL *toplevel, OBJECT *object)
{
  GError *err = NULL;
  GdkPixbuf *pixbuf;
  gchar *filename;

  pixbuf = gdk_pixbuf_new_from_file (object->picture->filename, &err);
  if (err != NULL) {
    s_log_message (_("Failed to load image from file [%s]: %s\n"),
                   object->picture->filename, err->message);
    g_error_free (err);
    return;
  }

  /* Change to the new pixbuf loaded from the file. */
  if (object->picture->original_picture != NULL)
    g_object_unref(object->picture->original_picture);

  object->picture->original_picture = pixbuf;

  g_free (object->picture->file_content);
  object->picture->file_content = NULL;
  object->picture->file_length = 0;
  object->picture->embedded = 0;

  filename = g_path_get_basename(object->picture->filename);
  s_log_message (_("Picture [%s] has been unembedded\n"), filename);
  g_free(filename);
}


/*! \brief Load a GdkPixbuf from a memory buffer
 *
 *  \par Function Description
 *  This function loads a GdkPixbuf from a memory buffer. The pixbuf
 *  returned already has a reference taken out on the callers behalf.
 *
 *  \param [in]   file_content  The memory buffer containing the image data.
 *  \param [in]   file_length   The size of the image data
 *  \param [out]  err           GError** pointer to return any error messages.
 *  \return  A GdkPixbuf loaded from the image data, or NULL on error.
 */

GdkPixbuf *o_picture_pixbuf_from_buffer (gchar *file_content,
                                         gsize file_length,
                                         GError **err)
{
  GdkPixbufLoader *loader;
  GdkPixbuf *pixbuf;

  loader = gdk_pixbuf_loader_new();

  gdk_pixbuf_loader_write (loader, (guchar *)file_content,
                                             file_length, err);
  if (err != NULL && *err != NULL)
    return NULL;

  gdk_pixbuf_loader_close (loader, err);
  if (err != NULL && *err != NULL)
    return NULL;

  pixbuf = gdk_pixbuf_loader_get_pixbuf (loader);

  if (pixbuf != NULL)
    g_object_ref (pixbuf);

  g_object_unref (loader);

  return pixbuf;
}

/*! \brief Calculates the distance between the given point and the closest
 * point in the picture.
 *
 *  Interrior points within the picture return a distance of zero.
 *
 *  \param [in] object The object, where object->picture != NULL.
 *  \param [in] x The x coordinate of the given point.
 *  \param [in] y The y coordinate of the given point.
 *  \return The shortest distance from the object to the point.  With an
 *  invalid parameter, this function returns G_MAXDOUBLE.
 */
gdouble o_picture_shortest_distance(PICTURE *picture, gint x, gint y)
{
  gdouble dx;
  gdouble dy;
  gdouble shortest_distance;
  gdouble x0;
  gdouble x1;
  gdouble y0;
  gdouble y1;

  if (picture == NULL) {
    g_critical("o_picture_shortest_distance(): picture == NULL\n");
    return G_MAXDOUBLE;
  }

  x0 = (gdouble) min(picture->upper_x, picture->lower_x);
  x1 = (gdouble) max(picture->upper_x, picture->lower_x);
  y0 = (gdouble) min(picture->upper_y, picture->lower_y);
  y1 = (gdouble) max(picture->upper_y, picture->lower_y);

  dx = min(((gdouble) x)-x0, x1-((gdouble) x));
  dy = min(((gdouble) y)-y0, y1-((gdouble) y));

  dx = min(dx, 0);
  dy = min(dy, 0);

  shortest_distance = sqrt((dx*dx) + (dy*dy));

  return shortest_distance;
}

