/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void picture_selection_dialog (TOPLEVEL *w_current)
{
  GtkWidget *file_selector;
  
  /* Create the selector */
  if (!w_current->pfswindow) {
#if DEBUG
    printf("Creating new picture file selection dialog\n");
#endif
    w_current->pfswindow = gtk_file_selection_new (_("Please select a picture file."));
    file_selector = w_current->pfswindow;
    if (w_current->pixbuf_filename)
      gtk_file_selection_set_filename(GTK_FILE_SELECTION(file_selector), w_current->pixbuf_filename);
    gtk_window_position(GTK_WINDOW (w_current->pfswindow),
			GTK_WIN_POS_NONE);
    
    g_signal_connect (G_OBJECT (file_selector), "destroy",
		      G_CALLBACK (picture_selection_cancel), w_current);
    
    
    g_signal_connect (GTK_OBJECT (GTK_FILE_SELECTION (file_selector)->ok_button),
		      "clicked",
		      G_CALLBACK (picture_selection_ok),
		      w_current);
    
    /* 
     * Ensure that the dialog box is destroyed when the user clicks the
     * cancel button.
     */
    g_signal_connect (GTK_OBJECT (GTK_FILE_SELECTION (file_selector)->cancel_button),
		      "clicked",
		      G_CALLBACK (picture_selection_cancel),
		      w_current); 
    
  }

  /* Display that dialog */
  if (!GTK_WIDGET_VISIBLE (w_current->pfswindow)) {
    gtk_widget_show (w_current->pfswindow);
#if 0
    gtk_grab_add (w_current->pfswindow);
#endif
  } else {
    gdk_window_raise(w_current->pfswindow->window);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void picture_selection_ok (GtkWidget *widget, TOPLEVEL *w_current)
{
  GtkWidget *file_selector = (GtkWidget *)w_current->pfswindow;
  const gchar *selected_filename;
  GdkPixbuf *pixbuf;
  GError *error;
  

  selected_filename = (char *) g_strdup(gtk_file_selection_get_filename (GTK_FILE_SELECTION (file_selector)));
#if DEBUG
  g_print ("Selected picture: %s\n", selected_filename);
#endif   
  picture_selection_cancel(widget, w_current);
  
  error = NULL;
  pixbuf = gdk_pixbuf_new_from_file (selected_filename, &error);

  if (!pixbuf) {
    GtkWidget *dialog;
    
    dialog = gtk_message_dialog_new (GTK_WINDOW (w_current->main_window),
                                     GTK_DIALOG_DESTROY_WITH_PARENT,
                                     GTK_MESSAGE_ERROR,
                                     GTK_BUTTONS_CLOSE,
                                     _("Failed to load picture: %s"),
                                     error->message);
    g_error_free (error);
     
    g_signal_connect (dialog, "response",
                      G_CALLBACK (gtk_widget_destroy), NULL);

    gtk_widget_show (dialog);
    return;
  }

#if DEBUG
  printf("Picture loaded succesfully.\n");
#endif

  exit_if_null(w_current);

  o_erase_rubber(w_current);

  i_update_middle_button(w_current, i_callback_add_picture, _("Picture"));
  w_current->inside_action = 0;

  o_picture_set_pixbuf(w_current, pixbuf, (char *) selected_filename);
  /* o_picture_set_pixbuf allocates memory for filename, so free the pointer */
  g_free((char *)selected_filename);

  w_current->page_current->CHANGED=1;

  i_allow_expose();
  i_set_state(w_current, DRAWPICTURE);
   
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void picture_selection_cancel (GtkWidget *widget, TOPLEVEL *w_current)
{
  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);
  gtk_widget_destroy(w_current->pfswindow);
  w_current->pfswindow=NULL;
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
  if (o_current->picture == NULL) {
    return;
  }

  /*
   * The function now recalculates the OBJECT as a picture. It involves
   * calculating every single dimensions according to the zoom factor the
   * position, @dots{}.
   * It also recalculates the bounding picture of the object and check
   * whether this object is visible or not. If not there is no reason to
   * draw it !
   */
  o_picture_recalc(w_current, o_current);

  /* Get read to check for visibility of this line by using it's
   * bounding picture
   */
  world_get_picture_bounds(w_current, o_current->picture,
                           &wleft, &wtop, &wright, &wbottom);
	
  if (!visible(w_current, wleft, wtop, wright, wbottom)) {
    return;
  }
	
#if  DEBUG 
  printf("drawing picture\n\n");
  
  printf("drawing picture: %d %d %d %d\n",
         o_current->picture->screen_upper_x,
         o_current->picture->screen_upper_y,
         o_current->picture->screen_upper_x +
         abs(o_current->picture->screen_lower_x -
             o_current->picture->screen_upper_x),
         o_current->picture->screen_upper_y +
         abs(o_current->picture->screen_lower_y -
             o_current->picture->screen_upper_y));
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
                            abs(o_current->picture->screen_lower_x -
                                o_current->picture->screen_upper_x), 
                            abs(o_current->picture->screen_lower_y - 
                                o_current->picture->screen_upper_y), 
                            GDK_INTERP_BILINEAR);
    g_object_unref(temp_pixbuf2);

    if (o_current->picture->displayed_picture == NULL) {
      fprintf(stderr, "Couldn't get enough memory for scaling the picture\n");
      return;
    }

    if (w_current->DONT_REDRAW == 0) {
      gdk_draw_pixbuf(w_current->window, w_current->gc,
		      o_current->picture->displayed_picture, 
		      0, 0, o_current->picture->screen_upper_x,
		      o_current->picture->screen_upper_y, 
		      -1, -1, GDK_RGB_DITHER_NONE, 0, 0);
      gdk_draw_pixbuf(w_current->backingstore, w_current->gc,
		      o_current->picture->displayed_picture, 
		      0, 0, o_current->picture->screen_upper_x,
		      o_current->picture->screen_upper_y, 
		      -1, -1, GDK_RGB_DITHER_NONE, 0, 0);
    }
  }
  else {
    if (w_current->DONT_REDRAW == 0) {
      /* Erase the picture, drawing a rectangle with the background color */
      gdk_gc_set_foreground(w_current->gc, 
			    x_get_color(w_current->background_color));
      gdk_draw_rectangle(w_current->window, w_current->gc, TRUE, 
			 o_current->picture->screen_upper_x,
			 o_current->picture->screen_upper_y,
			 abs(o_current->picture->screen_lower_x -
			     o_current->picture->screen_upper_x), 
			 abs(o_current->picture->screen_lower_y - 
			     o_current->picture->screen_upper_y));
      gdk_draw_rectangle(w_current->backingstore, w_current->gc, TRUE, 
			 o_current->picture->screen_upper_x,
			 o_current->picture->screen_upper_y,
			 abs(o_current->picture->screen_lower_x -
			     o_current->picture->screen_upper_x), 
			 abs(o_current->picture->screen_lower_y - 
			     o_current->picture->screen_upper_y));
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
#if DEBUG
  printf("o_picture_draw_grips called\n");
#endif
  if (w_current->draw_grips == FALSE)
	  return;

  /* grip on upper left corner (whichone = PICTURE_UPPER_LEFT) */
  o_grips_draw(w_current,
	       o_current->picture->screen_upper_x,
	       o_current->picture->screen_upper_y);
  
  /* grip on upper right corner (whichone = PICTURE_UPPER_RIGHT) */
  o_grips_draw(w_current,
	       o_current->picture->screen_lower_x,
	       o_current->picture->screen_upper_y);
  
  /* grip on lower left corner (whichone = PICTURE_LOWER_LEFT) */
  o_grips_draw(w_current,
	       o_current->picture->screen_upper_x,
	       o_current->picture->screen_lower_y);
  
  /* grip on lower right corner (whichone = PICTURE_LOWER_RIGHT) */
  o_grips_draw(w_current,
	       o_current->picture->screen_lower_x,
	       o_current->picture->screen_lower_y);
  
  /* Box surrounding the picture */
  gdk_draw_rectangle(w_current->window, w_current->gc, FALSE, 
		     o_current->picture->screen_upper_x,
		     o_current->picture->screen_upper_y,
		     abs(o_current->picture->screen_upper_x -
			 o_current->picture->screen_lower_x),
		     abs(o_current->picture->screen_upper_y -
			 o_current->picture->screen_lower_y));
  gdk_draw_rectangle(w_current->backingstore, w_current->gc, FALSE, 
		     o_current->picture->screen_upper_x,
		     o_current->picture->screen_upper_y,
		     abs(o_current->picture->screen_upper_x -
			 o_current->picture->screen_lower_x),
		     abs(o_current->picture->screen_upper_y -
			 o_current->picture->screen_lower_y));
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
#if DEBUG
  printf("o_picture_erase_grips called\n");
#endif
  if (w_current->draw_grips == FALSE)
	  return;

  /* grip on upper left corner (whichone = PICTURE_UPPER_LEFT) */
  o_grips_erase(w_current,
		o_current->picture->screen_upper_x,
		o_current->picture->screen_upper_y);
  
  /* grip on upper right corner (whichone = PICTURE_UPPER_RIGHT) */
  o_grips_erase(w_current,
		o_current->picture->screen_lower_x,
		o_current->picture->screen_upper_y);
  
  /* grip on lower left corner (whichone = PICTURE_LOWER_LEFT) */
  o_grips_erase(w_current,
		o_current->picture->screen_upper_x,
		o_current->picture->screen_lower_y);
  
  /* grip on lower right corner (whichone = PICTURE_LOWER_RIGHT) */
  o_grips_erase(w_current,
		o_current->picture->screen_lower_x,
		o_current->picture->screen_lower_y);
  
  /* Box surrounding the picture */
  gdk_draw_rectangle(w_current->window, w_current->gc, FALSE, 
		     o_current->picture->screen_upper_x,
		     o_current->picture->screen_upper_y,
		     abs(o_current->picture->screen_upper_x -
			 o_current->picture->screen_lower_x),
		     abs(o_current->picture->screen_upper_y -
			 o_current->picture->screen_lower_y));
  gdk_draw_rectangle(w_current->backingstore, w_current->gc, FALSE, 
		     o_current->picture->screen_upper_x,
		     o_current->picture->screen_upper_y,
		     abs(o_current->picture->screen_upper_x -
			 o_current->picture->screen_lower_x),
		     abs(o_current->picture->screen_upper_y -
			 o_current->picture->screen_lower_y));
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

  screen_x1 = o_current->picture->screen_upper_x;
  screen_y1 = o_current->picture->screen_upper_y;
  screen_x2 = o_current->picture->screen_lower_x;
  screen_y2 = o_current->picture->screen_lower_y;

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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void picture_change_selection_cancel (GtkWidget *widget, TOPLEVEL *w_current)
{
  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);
  gtk_widget_destroy(w_current->pcfswindow);
  w_current->pcfswindow=NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void picture_change_selection_ok (GtkWidget *widget, TOPLEVEL *w_current)
{
  GtkWidget *file_selector = (GtkWidget *)w_current->pcfswindow;
  const gchar *selected_filename;
  GdkPixbuf *pixbuf;
  GError *error;
  SELECTION *list;  

  selected_filename = (char *) g_strdup(gtk_file_selection_get_filename (GTK_FILE_SELECTION (file_selector)));
#if DEBUG
  g_print ("Selected picture: %s\n", selected_filename);
#endif   
  picture_change_selection_cancel(widget, w_current);
  
  error = NULL;
  pixbuf = gdk_pixbuf_new_from_file (selected_filename, &error);

  if (!pixbuf) {
    GtkWidget *dialog;
    
    dialog = gtk_message_dialog_new (GTK_WINDOW (w_current->main_window),
                                     GTK_DIALOG_DESTROY_WITH_PARENT,
                                     GTK_MESSAGE_ERROR,
                                     GTK_BUTTONS_CLOSE,
                                     _("Failed to load picture: %s"),
                                     error->message);
    g_error_free (error);
     
    g_signal_connect (dialog, "response",
                      G_CALLBACK (gtk_widget_destroy), NULL);

    gtk_widget_show (dialog);
    return;
  }
#if DEBUG
  printf("Picture loaded succesfully.\n");
#endif

  exit_if_null(w_current);

  o_erase_rubber(w_current);

  w_current->inside_action = 0;

  list = w_current->page_current->selection2_head->next;
  while (list != NULL) {
    OBJECT *object;
    
    object = list->selected_object;
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

	object->picture->filename = (char *) g_strdup(selected_filename);
  
	object->picture->ratio = gdk_pixbuf_get_width(pixbuf) / 
	  gdk_pixbuf_get_height(pixbuf);
	/* Draw new picture */
	o_picture_draw(w_current, object);

      }
    }
    list = list->next;
  }
  
  g_free ((char *) selected_filename);
  g_object_unref(pixbuf);
  w_current->page_current->CHANGED=1;

  i_allow_expose();
   
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void picture_change_filename_dialog (TOPLEVEL *w_current)
{

   GtkWidget *file_selector;

   /* Create the selector */
   if (!w_current->pcfswindow) {
#if DEBUG
     printf("Creating change picture file selection dialog\n");
#endif
     w_current->pcfswindow = gtk_file_selection_new (_("Please select a picture file."));
     file_selector = w_current->pcfswindow;
     if (w_current->pixbuf_filename)
       gtk_file_selection_set_filename(GTK_FILE_SELECTION(file_selector),
				       w_current->pixbuf_filename);
     gtk_window_position(GTK_WINDOW(w_current->pcfswindow),
                         GTK_WIN_POS_NONE);
     
     g_signal_connect (G_OBJECT(file_selector), "destroy",
		       G_CALLBACK(picture_change_selection_cancel),
		       w_current);
     
     
     g_signal_connect (GTK_OBJECT(GTK_FILE_SELECTION(file_selector)->ok_button),
                       "clicked",
                       G_CALLBACK(picture_change_selection_ok),
                       w_current);
   			   
     /* 
      * Ensure that the dialog box is destroyed when the user clicks the
      * cancel button.
      */
     g_signal_connect (GTK_OBJECT(GTK_FILE_SELECTION(file_selector)->cancel_button),
		       "clicked",
		       G_CALLBACK (picture_change_selection_cancel),
		       w_current); 
   		       
   }

   /* Display that dialog */
   if (!GTK_WIDGET_VISIBLE (w_current->pcfswindow)) {
     gtk_widget_show (w_current->pcfswindow);
     #if 0
     gtk_grab_add (w_current->pcfswindow);
     #endif
   } else {
     gdk_window_raise(w_current->pcfswindow->window);
   }
}
