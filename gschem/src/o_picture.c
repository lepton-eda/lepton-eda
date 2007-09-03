/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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
#include <math.h>
#include <stdio.h>

#include <libgeda/libgeda.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include "../include/globals.h"
#include "../include/prototype.h"

/* This works, but using one macro inside of other doesn't */
#define GET_PICTURE_WIDTH(w)			\
	abs((w)->last_x - (w)->start_x) 
#define GET_PICTURE_HEIGHT(w)			\
	(w)->pixbuf_wh_ratio == 0 ? 0 : abs((w)->last_x - (w)->start_x)/(w)->pixbuf_wh_ratio
#define GET_PICTURE_LEFT(w)				\
	min((w)->start_x, (w)->last_x);
#define GET_PICTURE_TOP(w)				\
	(w)->start_y < (w)->last_y ? (w)->start_y  : \
                                     (w)->start_y-abs((w)->last_x - (w)->start_x)/(w)->pixbuf_wh_ratio;

/*! \brief Start process to input a new picture.
 *  \par Function Description
 *  This function starts the process to input a new picture. Parameters
 *  for this picture are put into/extracted from the <B>w_current</B> toplevel
 *  structure.
 *  <B>x</B> and <B>y</B> are current coordinates of the pointer in screen
 *  coordinates.
 *
 *  The first step is to input one corner of the picture. This corner is
 *  (<B>x</B>,<B>y</B>) snapped to the grid and saved in
 *  <B>w_current->start_x</B> and <B>w_current->start_y</B>.
 *
 *  The other corner will be saved in (<B>w_current->last_x</B>,
 *  <B>w_current->last_y</B>).
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] x          Current x coordinate of pointer in screen units.    
 *  \param [in] y          Current y coordinate of pointer in screen units.
 */
void o_picture_start(TOPLEVEL *w_current, int x, int y)
{
#if DEBUG
  printf("o_picture_start called\n");
#endif
  /* init start_[x|y], last_[x|y] to describe box */
  w_current->last_x = w_current->start_x = fix_x(w_current, x);
  w_current->last_y = w_current->start_y = fix_y(w_current, y);

  /* start to draw the box */
  o_picture_rubberbox_xor(w_current);

}

/*! \brief End the input of a circle.
 *  \par Function Description
 *  This function ends the input of the second corner of a picture.
 *  The (<B>x</B>,<B>y</B>) point is set to be this second corner. The picture
 *  is then defined by (<B>w_current->start_x</B>,<B>w_current->start_y</B>
 *  and (<B>w_current->last_x</B>,<B>w_current->last_y</B> that is a snapped
 *  version of (<B>x</B>,<B>y</B>).
 *  <B>x</B> and <B>y</B> are in screen unit.
 *
 *  The temporary picture is erased ; a new picture object is allocated,
 *  initialized and linked to the object list ; The object is finally
 *  drawn on the current sheet.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] x          Current x coordinate of pointer in screen units.
 *  \param [in] y          Current y coordinate of pointer in screen units.
 */
void o_picture_end(TOPLEVEL *w_current, int x, int y)
{
  int x1, y1;
  int x2, y2;
  int picture_width, picture_height;
  int picture_left, picture_top;

  if (w_current->inside_action == 0) {
    o_redraw(w_current, w_current->page_current->object_head, TRUE);
    return;
  }

  /* get the last coords of the pointer */
  w_current->last_x = fix_x(w_current, x);
  w_current->last_y = fix_y(w_current, y);

  /* erase the temporary picture */
  o_picture_rubberbox_xor(w_current);
  
  picture_width  = GET_PICTURE_WIDTH (w_current);
  picture_height = GET_PICTURE_HEIGHT(w_current);
  picture_left   = GET_PICTURE_LEFT  (w_current);
  picture_top    = GET_PICTURE_TOP   (w_current);

  /* pictures with null width and height are not allowed */
  if ((picture_width == 0) && (picture_height == 0)) {
    /* cancel the object creation */
    w_current->start_x = (-1);
    w_current->start_y = (-1);
    w_current->last_x  = (-1);
    w_current->last_y  = (-1);
    return;
  }

  /* calculate the world coords of the upper left and lower right corners */
  SCREENtoWORLD(w_current, picture_left, picture_top, &x1, &y1);
  SCREENtoWORLD(w_current,
                picture_left + picture_width,
		picture_top + picture_height, &x2, &y2);
  x1 = snap_grid(w_current, x1);
  y1 = snap_grid(w_current, y1);
  x2 = snap_grid(w_current, x2);
  y2 = snap_grid(w_current, y2);

  /* create the object */
  w_current->page_current->object_tail = 
  o_picture_add(w_current,
                w_current->page_current->object_tail,
		w_current->current_pixbuf,
		w_current->pixbuf_filename,
		w_current->pixbuf_wh_ratio,
                OBJ_PICTURE, x1, y1, x2, y2, 0, FALSE, FALSE);

  /* draw it */
  o_redraw_single(w_current, w_current->page_current->object_tail);
  
#if DEBUG
  printf("coords: %d %d %d %d\n", x1, y2, x2, y2);
#endif
	
  w_current->start_x = (-1);
  w_current->start_y = (-1);
  w_current->last_x  = (-1);
  w_current->last_y  = (-1);
	
  w_current->page_current->CHANGED = 1;

  o_undo_savestate(w_current, UNDO_ALL);

}

/*! \brief Creates the add image dialog
 *  \par Function Description
 *  This function creates the add image dialog and loads the selected picture.
 */
void picture_selection_dialog (TOPLEVEL *w_current)
{
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
#if GTK_CHECK_VERSION (2,6,0)
  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->pfswindow),
					  GTK_RESPONSE_ACCEPT,
					  GTK_RESPONSE_CANCEL,
					  -1);
#endif

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
      
      o_erase_rubber(w_current);
      i_update_middle_button(w_current, i_callback_add_picture, _("Picture"));
      w_current->inside_action = 0;
      
      o_picture_set_pixbuf(w_current, pixbuf, filename);
    
      w_current->page_current->CHANGED=1;
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
void o_picture_eraserubber(TOPLEVEL *w_current)
{
#if DEBUG
  printf("o_picture_eraserubber called\n");
#endif
  o_picture_rubberbox_xor(w_current);
}

/*! \brief Draw temporary picture while dragging edge.
 *  \par Function Description
 *  This function is used to draw the box while dragging one of its edge or
 *  angle. It erases the previous temporary box drawn before, and draws
 *  a new updated one. <B>x</B> and <B>y</B> are the new position of the mobile
 *  point, ie the mouse.
 *
 *  The old values are inside the <B>w_current</B> pointed structure. Old
 *  width, height and left and top values are recomputed by the corresponding
 *  macros. The box is then erased by performing a xor-drawing over the box.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] x          Current x coordinate of pointer in screen units.
 *  \param [in] y          Current y coordinate of pointer in screen units.
 */
void o_picture_rubberbox(TOPLEVEL *w_current, int x, int y)
{
#if DEBUG
  printf("o_picture_rubberbox called\n");
#endif
  if (w_current->inside_action == 0) {
    o_redraw(w_current, w_current->page_current->object_head, TRUE);
    return;
  }

  /* erase the previous temporary box */
  o_picture_rubberbox_xor(w_current);

  /*
   * New values are fixed according to the <B>x</B> and <B>y</B> parameters. These are saved in <B>w_current</B> pointed structure as new temporary values. The new box is then drawn.

   */

  /* update the coords of the corner */
  w_current->last_x = fix_x(w_current, x);
  w_current->last_y = fix_y(w_current, y);

  /* draw the new temporary box */
  o_picture_rubberbox_xor(w_current);
  
}

/*! \brief Draw picture from TOPLEVEL object.
 *  \par Function Description
 *  This function draws the box from the variables in the toplevel
 *  structure <B>*w_current</B>.
 *  One corner of the box is at (<B>w_current->start_x</B>,
 *  <B>w_current->start_y</B>) and the second corner is at
 *  (<B>w_current->last_x</B>,<B>w_current->last_y</B>.
 *
 *  The box is drawn with a xor-function over the current sheet with the
 *  selection color.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 */
void o_picture_rubberbox_xor(TOPLEVEL *w_current)
{
  int picture_width, picture_height, picture_left, picture_top;
  
  /* get the width/height and the upper left corner of the picture */
  picture_width  = GET_PICTURE_WIDTH (w_current);
  picture_height = GET_PICTURE_HEIGHT(w_current);
  picture_left   = GET_PICTURE_LEFT  (w_current);
  picture_top    = GET_PICTURE_TOP   (w_current);
  
#if DEBUG
  printf("o_picture_rubberbox_xor called:\n");
  printf("pixbuf wh ratio: %i\n", w_current->pixbuf_wh_ratio);
  printf("start: %i, %i\n", w_current->start_x, w_current->start_y);
  printf("last: %i, %i\n", w_current->last_x, w_current->last_y);
  printf("Left: %i\nTop: %i\nWidth: %i\nHeight: %i\n",
	 picture_left, picture_top, picture_width, picture_height);
#endif
  /* draw the picture from the previous variables */
  gdk_gc_set_foreground(w_current->xor_gc, 
			x_get_darkcolor(w_current->select_color)); 
  gdk_gc_set_line_attributes(w_current->xor_gc, 0, 
			     GDK_LINE_SOLID, GDK_CAP_NOT_LAST, 
			     GDK_JOIN_MITER);
  gdk_draw_rectangle(w_current->window, w_current->xor_gc,
		     FALSE, picture_left, picture_top,
		     picture_width, picture_height);
}

/*! \brief Draw a picture on the screen.
 *  \par Function Description
 *  This function is used to draw a picture on screen. The picture is
 *  described in the OBJECT which is referred by <B>o_current</B>. The picture
 *  is displayed according to the current state, described in the
 *  TOPLEVEL object pointed by <B>w_current</B>.
 *
 *  It first checks if the OBJECT pointed is valid or not. If not it
 *  returns and do not output anything. That should never happen though.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] o_current  Picture OBJECT to draw.
 */
void o_picture_draw(TOPLEVEL *w_current, OBJECT *o_current)
{
  int wleft, wright, wtop, wbottom; /* world bounds */
  int s_upper_x, s_upper_y, s_lower_x, s_lower_y;

  if (o_current->picture == NULL) {
    return;
  }

  /* Get read to check for visibility of this line by using it's
   * bounding picture
   */
  world_get_picture_bounds(w_current, o_current,
                           &wleft, &wtop, &wright, &wbottom);
	
  if (!visible(w_current, wleft, wtop, wright, wbottom)) {
    return;
  }

  WORLDtoSCREEN( w_current, o_current->picture->upper_x, o_current->picture->upper_y,
                 &s_upper_x, &s_upper_y );
  WORLDtoSCREEN( w_current, o_current->picture->lower_x, o_current->picture->lower_y,
                 &s_lower_x, &s_lower_y );

#if  DEBUG 
  printf("drawing picture\n\n");
  
  printf("drawing picture: %d %d %d %d\n",
         s_upper_x, s_upper_y,
         s_upper_x + abs(s_lower_x - s_upper_x),
         s_upper_y + abs(s_lower_y - s_upper_y));
#endif

  /*
   * First, the picture is drawn.
   * Finally the function takes care of the grips.
   */

  if (o_current->picture->displayed_picture != NULL) {
    g_object_unref(o_current->picture->displayed_picture);
    o_current->picture->displayed_picture = NULL;
  }
  /* If it's not drawing using the background color then draw the image */
  if (w_current->override_color != w_current->background_color) { 
    GdkPixbuf *temp_pixbuf1, *temp_pixbuf2;

    /* Create a copy of the pixbuf rotated */
    temp_pixbuf1 = gdk_pixbuf_rotate(o_current->picture->original_picture, 
				    o_current->picture->angle);

    if (temp_pixbuf1 == NULL) {
      fprintf(stderr, "Couldn't get enough memory for rotating the picture\n");
      return;
    }

    temp_pixbuf2 = gdk_pixbuf_mirror_flip(temp_pixbuf1,
					  o_current->picture->mirrored, FALSE);
    g_object_unref(temp_pixbuf1);

    if (temp_pixbuf2 == NULL) {
      fprintf(stderr, "Couldn't get enough memory for mirroring the picture\n");
      return;
    }

    o_current->picture->displayed_picture = 
    gdk_pixbuf_scale_simple(temp_pixbuf2, 
                            abs(s_lower_x - s_upper_x), 
                            abs(s_lower_y - s_upper_y), 
                            GDK_INTERP_BILINEAR);
    g_object_unref(temp_pixbuf2);

    if (o_current->picture->displayed_picture == NULL) {
      fprintf(stderr, "Couldn't get enough memory for scaling the picture\n");
      return;
    }

    if (w_current->DONT_REDRAW == 0) {
      gdk_draw_pixbuf(w_current->window, w_current->gc,
		      o_current->picture->displayed_picture, 
		      0, 0, s_upper_x, s_upper_y, 
		      -1, -1, GDK_RGB_DITHER_NONE, 0, 0);
      gdk_draw_pixbuf(w_current->backingstore, w_current->gc,
		      o_current->picture->displayed_picture, 
		      0, 0, s_upper_x, s_upper_y, 
		      -1, -1, GDK_RGB_DITHER_NONE, 0, 0);
    }
  }
  else {
    if (w_current->DONT_REDRAW == 0) {
      /* Erase the picture, drawing a rectangle with the background color */
      gdk_gc_set_foreground(w_current->gc, 
			    x_get_color(w_current->background_color));
      gdk_draw_rectangle(w_current->window, w_current->gc, TRUE, 
			 s_upper_x, s_upper_y,
			 abs(s_lower_x - s_upper_x), 
			 abs(s_lower_y - s_upper_y));
      gdk_draw_rectangle(w_current->backingstore, w_current->gc, TRUE, 
			 s_upper_x, s_upper_y,
			 abs(s_lower_x -s_upper_x),
			 abs(s_lower_y - s_upper_y));
    }
  }

  /* Grip specific stuff */
  if ((o_current->draw_grips == TRUE) && (w_current->draw_grips == TRUE)) {
      if (!o_current->selected) {
	/* object is no more selected, erase the grips */
	o_current->draw_grips = FALSE;
	if (w_current->DONT_REDRAW == 0) {
	  o_picture_erase_grips(w_current, o_current); 
	}
      } else {
	/* object is selected, draw the grips on the picture */
	if (w_current->DONT_REDRAW == 0) {
	  o_picture_draw_grips(w_current, o_current); 
	}
      }
  }
}

/*! \brief Draw grip marks on picture.
 *  \par Function Description
 *  This function draws four grips on the corners of the picture described
 *  by <B>*o_current</B>.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] o_current  Picture OBJECT to draw grip points on.
 */
void o_picture_draw_grips(TOPLEVEL *w_current, OBJECT *o_current) 
{
  int s_upper_x, s_upper_y, s_lower_x, s_lower_y;

#if DEBUG
  printf("o_picture_draw_grips called\n");
#endif
  if (w_current->draw_grips == FALSE)
	  return;

  WORLDtoSCREEN( w_current, o_current->picture->upper_x, o_current->picture->upper_y,
                 &s_upper_x, &s_upper_y );
  WORLDtoSCREEN( w_current, o_current->picture->lower_x, o_current->picture->lower_y,
                 &s_lower_x, &s_lower_y );
  

  /* grip on upper left corner (whichone = PICTURE_UPPER_LEFT) */
  o_grips_draw(w_current, s_upper_x, s_upper_y);
  
  /* grip on upper right corner (whichone = PICTURE_UPPER_RIGHT) */
  o_grips_draw(w_current, s_lower_x, s_upper_y);
  
  /* grip on lower left corner (whichone = PICTURE_LOWER_LEFT) */
  o_grips_draw(w_current, s_upper_x, s_lower_y);
  
  /* grip on lower right corner (whichone = PICTURE_LOWER_RIGHT) */
  o_grips_draw(w_current, s_lower_x, s_lower_y);
  
  /* Box surrounding the picture */
  gdk_draw_rectangle(w_current->window, w_current->gc, FALSE, 
		     s_upper_x, s_upper_y,
		     abs(s_upper_x - s_lower_x),
		     abs(s_upper_y - s_lower_y));
  gdk_draw_rectangle(w_current->backingstore, w_current->gc, FALSE, 
		     s_upper_x, s_upper_y,
		     abs(s_upper_x - s_lower_x),
		     abs(s_upper_y - s_lower_y));
}

/*! \brief Erase grip marks from box.
 *  \par Function Description
 *  This function erases the four grips displayed on the <B>*o_current</B>
 *  picture object. These grips are on each of the corner.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] o_current  Picture OBJECT to erase grip marks from.
 */
void o_picture_erase_grips(TOPLEVEL *w_current, OBJECT *o_current) 
{
  int s_upper_x, s_upper_y, s_lower_x, s_lower_y;

#if DEBUG
  printf("o_picture_erase_grips called\n");
#endif
  if (w_current->draw_grips == FALSE)
	  return;

  WORLDtoSCREEN( w_current, o_current->picture->upper_x, o_current->picture->upper_y,
                 &s_upper_x, &s_upper_y );
  WORLDtoSCREEN( w_current, o_current->picture->lower_x, o_current->picture->lower_y,
                 &s_lower_x, &s_lower_y );
  
  /* grip on upper left corner (whichone = PICTURE_UPPER_LEFT) */
  o_grips_erase(w_current, s_upper_x, s_upper_y);
  
  /* grip on upper right corner (whichone = PICTURE_UPPER_RIGHT) */
  o_grips_erase(w_current, s_lower_x, s_upper_y);
  
  /* grip on lower left corner (whichone = PICTURE_LOWER_LEFT) */
  o_grips_erase(w_current, s_upper_x, s_lower_y);
  
  /* grip on lower right corner (whichone = PICTURE_LOWER_RIGHT) */
  o_grips_erase(w_current, s_lower_x, s_lower_y);
  
  /* Box surrounding the picture */
  gdk_draw_rectangle(w_current->window, w_current->gc, FALSE, 
		     s_upper_x, s_upper_y,
		     abs(s_upper_x - s_lower_x),
		     abs(s_upper_y - s_lower_y));
  gdk_draw_rectangle(w_current->backingstore, w_current->gc, FALSE, 
		     s_upper_x, s_upper_y,
		     abs(s_upper_x - s_lower_x),
		     abs(s_upper_y - s_lower_y));
  
}

/*! \brief Erase a picture described by OBJECT.
 *  \par Function Description
 *  This function erases a picture, described in a <B>OBJECT</B> structure
 *  pointed by <B>o_current</B>.
 *
 *  It makes a call to the function #o_picture_draw() after setting
 *  the special color. Therefore a picture is drawn with background color
 *  over the previous one.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] o_current  Picture OBJECT to erase.
 */
void o_picture_erase(TOPLEVEL *w_current, OBJECT *o_current)
{
#if DEBUG
  printf("o_picture_erase called\n");
#endif
    gdk_gc_set_foreground(w_current->gc,
                          x_get_color(w_current->background_color));
    w_current->override_color = w_current->background_color;
    o_picture_draw(w_current, o_current);
    w_current->override_color = -1;
}

/*! \brief Draw a picture described by OBJECT with translation
 *  \par Function Description
 *  This function daws the picture object described by <B>*o_current</B>
 *  translated by the vector (<B>dx</B>,<B>dy</B>) with an xor-function over
 *  the current sheet.
 *  The translation vector is in screen unit.
 *
 *  The picture is displayed with the color of the object.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] dx         Delta x coordinate for picture.
 *  \param [in] dy         Delta y coordinate for picture.
 *  \param [in] o_current  Picture OBJECT to draw.
 */
void o_picture_draw_xor(TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
  int screen_x1, screen_y1;
  int screen_x2, screen_y2;
  int color;

#if DEBUG
  printf("o_picture_draw_xor called.\n");
#endif
  if (o_current->picture == NULL) {
    return;
  }

  WORLDtoSCREEN( w_current, o_current->picture->upper_x, o_current->picture->upper_y,
                 &screen_x1, &screen_y1 );
  WORLDtoSCREEN( w_current, o_current->picture->lower_x, o_current->picture->lower_y,
                 &screen_x2, &screen_y2 );
  
  if (o_current->saved_color != -1) {
    color = o_current->saved_color;
  } else {
    color = o_current->color;
  }
  
  gdk_gc_set_foreground(w_current->outline_xor_gc,
                        x_get_darkcolor(color));
  
  gdk_draw_rectangle(w_current->window,
                     w_current->outline_xor_gc, FALSE,
                     screen_x1 + dx,
                     screen_y1 + dy,
                     abs(screen_x2 - screen_x1),
                     abs(screen_y2 - screen_y1));
}

/*! \brief Replace all selected pictures with a new picture
 *  \par Function Description
 *  This function replaces all pictures in the current selection with a 
 *  new image.
 *   
 *  \param [in] w_current  The TOPLEVEL object
 *  \param [in] pixbuf     New GdkPixbuf object
 *  \param [in] filename   The filename of the new picture
 *  
 */
void o_picture_exchange (TOPLEVEL *w_current, GdkPixbuf *pixbuf, 
			 const gchar *filename)
{
  GList *list;  

  list = geda_list_get_glist( w_current->page_current->selection_list );
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
	o_picture_erase(w_current, object);

	/* Change picture attributes */
	if (object->picture->original_picture != NULL) {
	  g_object_unref(object->picture->original_picture);
	  object->picture->original_picture=NULL;
	}
	
	if (object->picture->filename != NULL) {
	  g_free(object->picture->filename);
	  object->picture->filename=NULL;
	}
	/* Create a copy of the pixbuf rotated */
	object->picture->original_picture = gdk_pixbuf_rotate(pixbuf, 0);
	
	if (object->picture->original_picture == NULL) {
	  fprintf(stderr, "change picture: Couldn't get enough memory for the new picture\n");
	  return;
	}

	object->picture->filename = (char *) g_strdup(filename);
  
	object->picture->ratio = gdk_pixbuf_get_width(pixbuf) / 
	  gdk_pixbuf_get_height(pixbuf);
	/* Draw new picture */
	o_picture_draw(w_current, object);
      }
    }
    list = list->next;
  }
}

/*! \brief Create dialog to exchange picture objects
 *  \par Function Description
 *  This function opens a file chooser and replaces all pictures of the selections
 *  with the new picture.
 *
 *  \todo Maybe merge this dialog function with picture_selection_dialog()
 */
void picture_change_filename_dialog (TOPLEVEL *w_current)
{
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

#if GTK_CHECK_VERSION (2,6,0)
  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->pfswindow),
					  GTK_RESPONSE_ACCEPT,
					  GTK_RESPONSE_CANCEL,
					  -1);
#endif

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

      o_erase_rubber(w_current);
      w_current->inside_action = 0;

      /* \FIXME Should we set the pixbuf buffer in TOPLEVEL to store 
	 the current pixbuf? (Werner)
	 o_picture_set_pixbuf(w_current, pixbuf, filename); */

      o_picture_exchange(w_current, pixbuf, filename);

      g_object_unref(pixbuf);
      w_current->page_current->CHANGED=1;
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
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] pixbuf
 *  \param [in] filename
 */
void o_picture_set_pixbuf(TOPLEVEL *toplevel,
                          GdkPixbuf *pixbuf, char *filename)
{

  /* need to put an error messages here */
  if (pixbuf == NULL)  {
    fprintf(stderr, "error! picture in set pixbuf was NULL\n");
    return;
  }

  if (toplevel->current_pixbuf != NULL) {
    g_object_unref(toplevel->current_pixbuf);
    toplevel->current_pixbuf=NULL;
  }

  if (toplevel->pixbuf_filename != NULL) {
    g_free(toplevel->pixbuf_filename);
    toplevel->pixbuf_filename=NULL;
  }

  toplevel->current_pixbuf = pixbuf;
  toplevel->pixbuf_filename = (char *) g_strdup(filename);

  toplevel->pixbuf_wh_ratio = (double) gdk_pixbuf_get_width(pixbuf) /
                                        gdk_pixbuf_get_height(pixbuf);

  /* be sure to free this pixbuf somewhere */
}
