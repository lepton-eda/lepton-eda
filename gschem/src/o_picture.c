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
#include <math.h>
#include <stdio.h>

#include "gschem.h"

/* This works, but using one macro inside of other doesn't */
#define GET_PICTURE_WIDTH(w)			\
  abs((w)->second_wx - (w)->first_wx) 
#define GET_PICTURE_HEIGHT(w)						\
  (w)->pixbuf_wh_ratio == 0 ? 0 : abs((w)->second_wx - (w)->first_wx)/(w)->pixbuf_wh_ratio
#define GET_PICTURE_LEFT(w)			\
  min((w)->first_wx, (w)->second_wx)
#define GET_PICTURE_TOP(w)						\
  (w)->first_wy > (w)->second_wy ? (w)->first_wy  :			\
  (w)->first_wy+abs((w)->second_wx - (w)->first_wx)/(w)->pixbuf_wh_ratio

/*! \brief Start process to input a new picture.
 *  \par Function Description
 *  This function starts the process to input a new picture. Parameters
 *  for this picture are put into/extracted from the <B>w_current</B> toplevel
 *  structure.
 *  <B>w_x</B> and <B>w_y</B> are current coordinates of the pointer in world
 *  coordinates.
 *
 *  The first step is to input one corner of the picture. This corner is
 *  (<B>w_x</B>,<B>w_y</B>) snapped to the grid and saved in
 *  <B>w_current->first_wx</B> and <B>w_current->first_wy</B>.
 *
 *  The other corner will be saved in (<B>w_current->second_wx</B>,
 *  <B>w_current->second_wy</B>).
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.    
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_picture_start(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  /* init first_w[x|y], second_w[x|y] to describe box */
  w_current->first_wx = w_current->second_wx = w_x;
  w_current->first_wy = w_current->second_wy = w_y;

  /* start to draw the box */
  o_picture_invalidate_rubber (w_current);
  w_current->rubber_visible = 1;
}

/*! \brief End the input of a circle.
 *  \par Function Description
 *  This function ends the input of the second corner of a picture.
 *  The picture is defined by (<B>w_current->first_wx</B>,<B>w_current->first_wy</B>
 *  and (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>.
 *
 *  The temporary picture frame is erased ; a new picture object is allocated,
 *  initialized and linked to the object list ; The object is finally
 *  drawn on the current sheet.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        (unused)
 *  \param [in] w_y        (unused)
 */
void o_picture_end(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *new_obj;
  int picture_width, picture_height;
  int picture_left, picture_top;

  g_assert( w_current->inside_action != 0 );

  /* erase the temporary picture */
  /* o_picture_draw_rubber(w_current); */
  w_current->rubber_visible = 0;
  
  picture_width  = GET_PICTURE_WIDTH (w_current);
  picture_height = GET_PICTURE_HEIGHT(w_current);
  picture_left   = GET_PICTURE_LEFT  (w_current);
  picture_top    = GET_PICTURE_TOP   (w_current);

  /* pictures with null width and height are not allowed */
  if ((picture_width == 0) && (picture_height == 0)) {
    /* cancel the object creation */
    return;
  }

  /* create the object */
  new_obj = o_picture_new(toplevel, w_current->current_pixbuf,
                          NULL, 0, w_current->pixbuf_filename,
                          w_current->pixbuf_wh_ratio, OBJ_PICTURE,
                          picture_left, picture_top,
                          picture_left + picture_width,
                          picture_top - picture_height,
                          0, FALSE, FALSE);
  s_page_append (toplevel, toplevel->page_current, new_obj);

  toplevel->page_current->CHANGED = 1;
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \brief Creates the add image dialog
 *  \par Function Description
 *  This function creates the add image dialog and loads the selected picture.
 */
void picture_selection_dialog (GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  gchar *filename;
  GdkPixbuf *pixbuf;
  GError *error = NULL;
  
  w_current->pfswindow = gtk_file_chooser_dialog_new ("Select a picture file...",
						      GTK_WINDOW(w_current->main_window),
						      GTK_FILE_CHOOSER_ACTION_OPEN,
						      GTK_STOCK_CANCEL, 
						      GTK_RESPONSE_CANCEL,
						      GTK_STOCK_OPEN, 
						      GTK_RESPONSE_ACCEPT,
						      NULL);
  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->pfswindow),
					  GTK_RESPONSE_ACCEPT,
					  GTK_RESPONSE_CANCEL,
					  -1);

  if (w_current->pixbuf_filename)
    gtk_file_chooser_set_filename(GTK_FILE_CHOOSER(w_current->pfswindow), 
				  w_current->pixbuf_filename);
    
  if (gtk_dialog_run (GTK_DIALOG (w_current->pfswindow)) == GTK_RESPONSE_ACCEPT) {

    filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (w_current->pfswindow));
    gtk_widget_destroy(w_current->pfswindow);
    w_current->pfswindow=NULL;

    pixbuf = gdk_pixbuf_new_from_file (filename, &error);
    
    if (!pixbuf) {
      GtkWidget *dialog;
      
      dialog = gtk_message_dialog_new (GTK_WINDOW (w_current->main_window),
				       GTK_DIALOG_DESTROY_WITH_PARENT,
				       GTK_MESSAGE_ERROR,
				       GTK_BUTTONS_CLOSE,
				       _("Failed to load picture: %s"),
				       error->message);
      /* Wait for any user response */
      gtk_dialog_run (GTK_DIALOG (dialog));
      
      g_error_free (error);
      gtk_widget_destroy(dialog);
    }
    else {
#if DEBUG
      printf("Picture loaded succesfully.\n");
#endif
      
      o_invalidate_rubber(w_current);
      i_update_middle_button(w_current, i_callback_add_picture, _("Picture"));
      w_current->inside_action = 0;
      
      o_picture_set_pixbuf(w_current, pixbuf, filename);
    
      toplevel->page_current->CHANGED=1;
      i_set_state(w_current, DRAWPICTURE);
    }
    g_free (filename);
  }

  i_update_toolbar(w_current);
  if (w_current->pfswindow) {
    gtk_widget_destroy(w_current->pfswindow);
    w_current->pfswindow=NULL;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 * used in button cancel code in x_events.c
 */
void o_picture_invalidate_rubber (GSCHEM_TOPLEVEL *w_current)
{
  int left, top, width, height;

  WORLDtoSCREEN (w_current,
                 GET_PICTURE_LEFT(w_current), GET_PICTURE_TOP(w_current),
                 &left, &top);
  width = SCREENabs (w_current, GET_PICTURE_WIDTH (w_current));
  height = SCREENabs (w_current, GET_PICTURE_HEIGHT(w_current));

  o_invalidate_rect (w_current, left, top, left + width, top);
  o_invalidate_rect (w_current, left, top, left, top + height);
  o_invalidate_rect (w_current, left + width, top, left + width, top + height);
  o_invalidate_rect (w_current, left, top + height, left + width, top + height);
}

/*! \brief Draw temporary picture while dragging edge.
 *  \par Function Description
 *  This function is used to draw the box while dragging one of its edge or
 *  angle. It erases the previous temporary box drawn before, and draws
 *  a new updated one. <B>w_x</B> and <B>w_y</B> are the new position of the mobile
 *  point, ie the mouse.
 *
 *  The old values are inside the <B>w_current</B> pointed structure. Old
 *  width, height and left and top values are recomputed by the corresponding
 *  macros.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_picture_motion (GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
#if DEBUG
  printf("o_picture_rubberbox called\n");
#endif
  g_assert( w_current->inside_action != 0 );

  /* erase the previous temporary box */
  if (w_current->rubber_visible)
    o_picture_invalidate_rubber (w_current);

  /*
   * New values are fixed according to the <B>w_x</B> and <B>w_y</B> parameters. 
   * These are saved in <B>w_current</B> pointed structure as new temporary values. 
   * The new box is then drawn.
   */

  /* update the coords of the corner */
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  /* draw the new temporary box */
  o_picture_invalidate_rubber (w_current);
  w_current->rubber_visible = 1;
}

/*! \brief Draw picture from GSCHEM_TOPLEVEL object.
 *  \par Function Description
 *  This function draws the box from the variables in the GSCHEM_TOPLEVEL
 *  structure <B>*w_current</B>.
 *  One corner of the box is at (<B>w_current->first_wx</B>,
 *  <B>w_current->first_wy</B>) and the second corner is at
 *  (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 */
void o_picture_draw_rubber (GSCHEM_TOPLEVEL *w_current)
{
  int left, top, width, height;

  /* get the width/height and the upper left corner of the picture */
  left =   GET_PICTURE_LEFT (w_current);
  top =    GET_PICTURE_TOP (w_current);
  width =  GET_PICTURE_WIDTH (w_current);
  height = GET_PICTURE_HEIGHT (w_current);

  gschem_cairo_box (w_current, 0, left, top - height, left + width, top);

  gschem_cairo_set_source_color (w_current,
                                 x_color_lookup_dark (SELECT_COLOR));
  gschem_cairo_stroke (w_current, TYPE_SOLID, END_NONE, 0, -1, -1);
}

/*! \brief Draw a picture on the screen.
 *  \par Function Description
 *  This function is used to draw a picture on screen. The picture is
 *  described in the OBJECT which is referred by <B>o_current</B>. The picture
 *  is displayed according to the current state, described in the
 *  GSCHEM_TOPLEVEL object pointed by <B>w_current</B>.
 *
 *  It first checks if the OBJECT pointed is valid or not. If not it
 *  returns and do not output anything. That should never happen though.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  Picture OBJECT to draw.
 */
void o_picture_draw (GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int s_upper_x, s_upper_y, s_lower_x, s_lower_y;

  if (o_current->picture == NULL) {
    return;
  }

  WORLDtoSCREEN (w_current, o_current->picture->upper_x,
                            o_current->picture->upper_y, &s_upper_x, &s_upper_y);
  WORLDtoSCREEN (w_current, o_current->picture->lower_x,
                            o_current->picture->lower_y, &s_lower_x, &s_lower_y);

  cairo_save (w_current->cr);

  int swap_wh = (o_current->picture->angle == 90 || o_current->picture->angle == 270);
  float orig_width  = swap_wh ? gdk_pixbuf_get_height (o_current->picture->pixbuf) :
                                gdk_pixbuf_get_width  (o_current->picture->pixbuf);
  float orig_height = swap_wh ? gdk_pixbuf_get_width  (o_current->picture->pixbuf) :
                                gdk_pixbuf_get_height (o_current->picture->pixbuf);

  cairo_translate (w_current->cr, s_upper_x, s_upper_y);
  cairo_scale (w_current->cr,
    (float)SCREENabs (w_current, abs (o_current->picture->upper_x -
                                      o_current->picture->lower_x)) / orig_width,
    (float)SCREENabs (w_current, abs (o_current->picture->upper_y -
                                      o_current->picture->lower_y)) / orig_height);

  /* Evil magic translates picture origin to the right position for a given rotation */
  switch (o_current->picture->angle) {
    case 0:                                                               break;
    case 90:   cairo_translate (w_current->cr, 0,          orig_height);  break;
    case 180:  cairo_translate (w_current->cr, orig_width, orig_height);  break;
    case 270:  cairo_translate (w_current->cr, orig_width, 0          );  break;
  }
  cairo_rotate (w_current->cr, -o_current->picture->angle * M_PI / 180.);
  if (o_current->picture->mirrored)
    cairo_scale (w_current->cr, -1, 1);

  gdk_cairo_set_source_pixbuf (w_current->cr,
                               o_current->picture->pixbuf, 0,0);
  cairo_rectangle (w_current->cr, 0, 0,
                   gdk_pixbuf_get_width (o_current->picture->pixbuf),
                   gdk_pixbuf_get_height (o_current->picture->pixbuf));

  cairo_clip (w_current->cr);
  cairo_paint (w_current->cr);
  cairo_restore (w_current->cr);

  /* Grip specific stuff */
  if (o_current->selected && w_current->draw_grips) {
    o_picture_draw_grips (w_current, o_current);
  }
}

/*! \brief Draw grip marks on picture.
 *  \par Function Description
 *  This function draws four grips on the corners of the picture described
 *  by <B>*o_current</B>.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  Picture OBJECT to draw grip points on.
 */
void o_picture_draw_grips(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  if (w_current->draw_grips == FALSE)
    return;

  /* grip on upper left corner (whichone = PICTURE_UPPER_LEFT) */
  o_grips_draw (w_current, o_current->picture->upper_x,
                           o_current->picture->upper_y);

  /* grip on upper right corner (whichone = PICTURE_UPPER_RIGHT) */
  o_grips_draw (w_current, o_current->picture->lower_x,
                           o_current->picture->upper_y);

  /* grip on lower left corner (whichone = PICTURE_LOWER_LEFT) */
  o_grips_draw (w_current, o_current->picture->upper_x,
                           o_current->picture->lower_y);

  /* grip on lower right corner (whichone = PICTURE_LOWER_RIGHT) */
  o_grips_draw (w_current, o_current->picture->lower_x,
                           o_current->picture->lower_y);

  /* Box surrounding the picture */
  gschem_cairo_box (w_current, 0,
                    o_current->picture->upper_x, o_current->picture->upper_y,
                    o_current->picture->lower_x, o_current->picture->lower_y);

  gschem_cairo_set_source_color (w_current,
                                 x_color_lookup_dark (SELECT_COLOR));
  gschem_cairo_stroke (w_current, TYPE_SOLID, END_NONE, 0, -1, -1);
}


/*! \brief Draw a picture described by OBJECT with translation
 *  \par Function Description
 *  This function daws the picture object described by <B>*o_current</B>
 *  translated by the vector (<B>dx</B>,<B>dy</B>).
 *  The translation vector is in world unit.
 *
 *  The picture is displayed with the color of the object.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] dx         Delta x coordinate for picture.
 *  \param [in] dy         Delta y coordinate for picture.
 *  \param [in] o_current  Picture OBJECT to draw.
 */
void o_picture_draw_place (GSCHEM_TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
  if (o_current->picture == NULL) {
    return;
  }

  gschem_cairo_box (w_current, 0, o_current->picture->upper_x + dx,
                                  o_current->picture->upper_y + dy,
                                  o_current->picture->lower_x + dx,
                                  o_current->picture->lower_y + dy);

  gschem_cairo_set_source_color (w_current,
                                 x_color_lookup_dark (o_current->color));
  gschem_cairo_stroke (w_current, TYPE_SOLID, END_NONE, 0, -1, -1);
}

/*! \brief Replace all selected pictures with a new picture
 *  \par Function Description
 *  This function replaces all pictures in the current selection with a 
 *  new image.
 *   
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object
 *  \param [in] pixbuf     New GdkPixbuf object
 *  \param [in] filename   The filename of the new picture
 *  
 */
void o_picture_exchange (GSCHEM_TOPLEVEL *w_current, GdkPixbuf *pixbuf,
			 const gchar *filename)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *list;  

  list = geda_list_get_glist( toplevel->page_current->selection_list );
  while (list != NULL) {
    OBJECT *object;

    object = (OBJECT *) list->data;
    if (object == NULL) {
      fprintf(stderr, _("ERROR: NULL object!\n"));
      exit(-1);
    }
    if (!object->attached_to) {
      /* It's selected. Then change picture if it's a picture */
      if (object->type == OBJ_PICTURE) {

        /* Erase previous picture */
        o_invalidate (w_current, object);

        g_free(object->picture->filename);

        object->picture->filename = (char *) g_strdup(filename);

        /* Unref the old pixmap */
        if (object->picture->pixbuf != NULL) {
          g_object_unref (object->picture->pixbuf);
          object->picture->pixbuf = NULL;
        }

        if (object->picture->embedded) {
          /* For embedded pictures, call o_picture_embed() to update the
           * embedded picture data from the new file and reload the pixmap */
          o_picture_embed(toplevel, object);
        } else {
          /* For non-embedded pictures, create a copy of the passed pixbuf
           * and insert it manually */
          object->picture->pixbuf = gdk_pixbuf_copy (pixbuf);
          if (object->picture->pixbuf == NULL) {
            fprintf(stderr, "change picture: Couldn't get enough memory for the new picture\n");
            return;
          }
        }

        object->picture->ratio = (double)gdk_pixbuf_get_width(pixbuf) /
                                         gdk_pixbuf_get_height(pixbuf);
        /* Draw new picture */
        o_invalidate (w_current, object);
      }
    }
    list = g_list_next(list);
  }
}

/*! \brief Create dialog to exchange picture objects
 *  \par Function Description
 *  This function opens a file chooser and replaces all pictures of the selections
 *  with the new picture.
 *
 *  \todo Maybe merge this dialog function with picture_selection_dialog()
 */
void picture_change_filename_dialog (GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  gchar *filename;
  GdkPixbuf *pixbuf;
  GError *error = NULL;
  
  w_current->pfswindow = gtk_file_chooser_dialog_new ("Select a picture file...",
						      GTK_WINDOW(w_current->main_window),
						      GTK_FILE_CHOOSER_ACTION_OPEN,
						      GTK_STOCK_CANCEL, 
						      GTK_RESPONSE_CANCEL,
						      GTK_STOCK_OPEN, 
						      GTK_RESPONSE_ACCEPT,
						      NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->pfswindow),
					  GTK_RESPONSE_ACCEPT,
					  GTK_RESPONSE_CANCEL,
					  -1);

  if (w_current->pixbuf_filename)
    gtk_file_chooser_set_filename(GTK_FILE_CHOOSER(w_current->pfswindow), 
				  w_current->pixbuf_filename);
    
  if (gtk_dialog_run (GTK_DIALOG (w_current->pfswindow)) == GTK_RESPONSE_ACCEPT) {

    filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (w_current->pfswindow));
    gtk_widget_destroy(w_current->pfswindow);
    w_current->pfswindow=NULL;

    pixbuf = gdk_pixbuf_new_from_file (filename, &error);
    
    if (!pixbuf) {
      GtkWidget *dialog;
      
      dialog = gtk_message_dialog_new (GTK_WINDOW (w_current->main_window),
				       GTK_DIALOG_DESTROY_WITH_PARENT,
				       GTK_MESSAGE_ERROR,
				       GTK_BUTTONS_CLOSE,
				       _("Failed to load picture: %s"),
				       error->message);
      /* Wait for any user response */
      gtk_dialog_run (GTK_DIALOG (dialog));
      
      g_error_free (error);
      gtk_widget_destroy(dialog);
    }
    else {
#if DEBUG
      printf("Picture loaded succesfully.\n");
#endif

      o_invalidate_rubber(w_current);
      w_current->inside_action = 0;

      /* \FIXME Should we set the pixbuf buffer in GSCHEM_TOPLEVEL to store
	 the current pixbuf? (Werner)
	 o_picture_set_pixbuf(w_current, pixbuf, filename); */

      o_picture_exchange(w_current, pixbuf, filename);

      g_object_unref(pixbuf);
      toplevel->page_current->CHANGED=1;
    }
    g_free (filename);
  }

  i_update_toolbar(w_current);
  if (w_current->pfswindow) {
    gtk_widget_destroy(w_current->pfswindow);
    w_current->pfswindow=NULL;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] pixbuf
 *  \param [in] filename
 */
void o_picture_set_pixbuf(GSCHEM_TOPLEVEL *w_current,
                          GdkPixbuf *pixbuf, char *filename)
{

  /* need to put an error messages here */
  if (pixbuf == NULL)  {
    fprintf(stderr, "error! picture in set pixbuf was NULL\n");
    return;
  }

  if (w_current->current_pixbuf != NULL) {
    g_object_unref(w_current->current_pixbuf);
    w_current->current_pixbuf=NULL;
  }

  if (w_current->pixbuf_filename != NULL) {
    g_free(w_current->pixbuf_filename);
    w_current->pixbuf_filename=NULL;
  }

  w_current->current_pixbuf = pixbuf;
  w_current->pixbuf_filename = (char *) g_strdup(filename);

  w_current->pixbuf_wh_ratio = (double) gdk_pixbuf_get_width(pixbuf) /
                                        gdk_pixbuf_get_height(pixbuf);

  /* be sure to free this pixbuf somewhere */
}
