/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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
#include <config.h>
#include <math.h>
#include <stdio.h>

#include "gschem.h"

/* This works, but using one macro inside of other doesn't */
#define GET_PICTURE_WIDTH(w) \
  abs((w)->second_wx - (w)->first_wx)
#define GET_PICTURE_HEIGHT(w) \
  (w)->pixbuf_wh_ratio == 0 ? 0 : abs((w)->second_wx - (w)->first_wx)/(w)->pixbuf_wh_ratio
#define GET_PICTURE_LEFT(w) \
  MIN((w)->first_wx, (w)->second_wx)
#define GET_PICTURE_TOP(w) \
  (w)->first_wy > (w)->second_wy ? (w)->first_wy  : \
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
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_picture_start(GschemToplevel *w_current, int w_x, int w_y)
{
  i_action_start (w_current);

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
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        (unused)
 *  \param [in] w_y        (unused)
 */
void o_picture_end(GschemToplevel *w_current, int w_x, int w_y)
{
  LeptonObject *new_obj;
  int picture_width, picture_height;
  int picture_left, picture_top;

  g_assert (schematic_window_get_inside_action (w_current) != 0);

  LeptonPage *active_page = schematic_window_get_active_page (w_current);

  /* erase the temporary picture */
  /* o_picture_draw_rubber(w_current); */
  w_current->rubber_visible = 0;

  picture_width  = GET_PICTURE_WIDTH (w_current);
  picture_height = GET_PICTURE_HEIGHT(w_current);
  picture_left   = GET_PICTURE_LEFT  (w_current);
  picture_top    = GET_PICTURE_TOP   (w_current);

  /* pictures with null width or height are not allowed */
  if ((picture_width != 0) && (picture_height != 0)) {

    /* create the object */
    new_obj = lepton_picture_object_new (NULL,
                                         0,
                                         w_current->pixbuf_filename,
                                         picture_left,
                                         picture_top,
                                         picture_left + picture_width,
                                         picture_top - picture_height,
                                         0,
                                         FALSE,
                                         FALSE);
    lepton_page_append (active_page, new_obj);

    /* Run add-objects-hook */
    g_run_hook_object (w_current, "add-objects-hook", new_obj);

    schematic_window_active_page_changed (w_current);
    o_undo_savestate_old(w_current, UNDO_ALL);
  }
  i_action_stop (w_current);
}



/*! \brief Setup file filters for the "Add Picture" dialog.
 *
 *  \param filechooser  Picture file chooser.
 */
static void
setup_filechooser_filters (GtkFileChooser* filechooser)
{
  GSList* formats = gdk_pixbuf_get_formats();
  g_return_if_fail (formats != NULL);

  /* Filter for all file formats supported by gdk-pixbuf:
  */
  GtkFileFilter* filter = gtk_file_filter_new();
  gtk_file_filter_set_name (filter, _("All supported formats"));
  gtk_file_chooser_add_filter (filechooser, filter);

  for (GSList* p = formats; p != NULL; p = g_slist_next (p))
  {
    GdkPixbufFormat* fmt = (GdkPixbufFormat*) p->data;
    gchar** extensions = gdk_pixbuf_format_get_extensions (fmt);

    for (gchar** ext = extensions; *ext != NULL; ++ext)
    {
      gchar* str = g_strdup_printf ("*.%s", *ext);
      gtk_file_filter_add_pattern (filter, str);
      g_free (str);
    }

    g_strfreev (extensions);
  }

  g_slist_free (formats);

  /* Filter for all files:
  */
  GtkFileFilter* filter_all = gtk_file_filter_new();
  gtk_file_filter_set_name (filter_all, _("All files"));
  gtk_file_filter_add_pattern (filter_all, "*");
  gtk_file_chooser_add_filter (filechooser, filter_all);

} /* setup_filechooser_filters() */



/*! \brief Creates and shows the "Add Picture" dialog.
 */
void picture_selection_dialog (GschemToplevel *w_current)
{
  gchar *filename;
  GdkPixbuf *pixbuf;
  GError *error = NULL;

  GtkWidget* pfswindow = gtk_file_chooser_dialog_new (_("Add Picture"),
                                                      GTK_WINDOW(w_current->main_window),
                                                      GTK_FILE_CHOOSER_ACTION_OPEN,
                                                      _("_Cancel"),
                                                      GTK_RESPONSE_CANCEL,
                                                      _("_Open"),
                                                      GTK_RESPONSE_ACCEPT,
                                                      NULL);

  setup_filechooser_filters (GTK_FILE_CHOOSER (pfswindow));

#ifndef ENABLE_GTK3
  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(pfswindow),
                                          GTK_RESPONSE_ACCEPT,
                                          GTK_RESPONSE_CANCEL,
                                          -1);
#endif

  if (w_current->pixbuf_filename)
    gtk_file_chooser_set_filename(GTK_FILE_CHOOSER(pfswindow),
                                  w_current->pixbuf_filename);

  if (gtk_dialog_run (GTK_DIALOG (pfswindow)) == GTK_RESPONSE_ACCEPT) {

    filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (pfswindow));
    pixbuf = gdk_pixbuf_new_from_file (filename, &error);

    if (!pixbuf) {
      GtkWidget *dialog;

      dialog = gtk_message_dialog_new (GTK_WINDOW (w_current->main_window),
                                       GTK_DIALOG_DESTROY_WITH_PARENT,
                                       GTK_MESSAGE_ERROR,
                                       GTK_BUTTONS_CLOSE,
                                       _("Failed to load picture: %1$s"),
                                       error->message);
      gtk_window_set_title (GTK_WINDOW (dialog), "lepton-schematic");
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

      i_action_stop (w_current);

      o_picture_set_pixbuf(w_current, pixbuf, filename);

      schematic_window_active_page_changed (w_current);
      i_set_state(w_current, PICTUREMODE);
    }
    g_free (filename);
  }

  gtk_widget_destroy(pfswindow);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 * used in button cancel code in x_events.c
 */
void o_picture_invalidate_rubber (GschemToplevel *w_current)
{
  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  gschem_page_view_invalidate_world_rect (page_view,
                                          GET_PICTURE_LEFT (w_current),
                                          GET_PICTURE_TOP (w_current),
                                          GET_PICTURE_LEFT (w_current) + GET_PICTURE_WIDTH (w_current),
                                          GET_PICTURE_TOP (w_current) + GET_PICTURE_HEIGHT (w_current));
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
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_picture_motion (GschemToplevel *w_current, int w_x, int w_y)
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

/*! \brief Draw picture from GschemToplevel object.
 *  \par Function Description
 *  This function draws the box from the variables in the
 *  #GschemToplevel structure <B>*w_current</B> using \a renderer.
 *  One corner of the box is at (<B>w_current->first_wx</B>,
 *  <B>w_current->first_wy</B>) and the second corner is at
 *  (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>.
 *
 *  \param [in] w_current  The #GschemToplevel object.
 *  \param [in] renderer   The \c EdaRenderer object.
 */
void o_picture_draw_rubber (GschemToplevel *w_current, EdaRenderer *renderer)
{
  int left, top, width, height;
  double wwidth = 0;
  cairo_t *cr = eda_renderer_get_cairo_context (renderer);
  GArray *color_map = eda_renderer_get_color_map (renderer);
  int flags = eda_renderer_get_cairo_flags (renderer);

  /* get the width/height and the upper left corner of the picture */
  left =   GET_PICTURE_LEFT (w_current);
  top =    GET_PICTURE_TOP (w_current);
  width =  GET_PICTURE_WIDTH (w_current);
  height = GET_PICTURE_HEIGHT (w_current);

  eda_cairo_box (cr, flags, wwidth, left, top - height, left + width, top);
  eda_cairo_set_source_color (cr, SELECT_COLOR, color_map);
  eda_cairo_stroke (cr, flags, TYPE_SOLID, END_NONE, wwidth, -1, -1);
}

/*! \brief Replace all selected pictures with a new picture
 * \par Function Description
 * Replaces all pictures in the current selection with a new image.
 *
 * \param [in] w_current  The GschemToplevel object
 * \param [in] filename   The filename of the new picture
 * \param [out] error     The location to return error information.
 * \return TRUE on success, FALSE on failure.
 */
gboolean
o_picture_exchange (GschemToplevel *w_current,
                    const gchar *filename, GError **error)
{
  GList *iter;
  LeptonPage *active_page = schematic_window_get_active_page (w_current);

  for (iter = lepton_list_get_glist (active_page->selection_list);
       iter != NULL;
       iter = g_list_next (iter)) {

    LeptonObject *object = (LeptonObject *) iter->data;
    g_assert (object != NULL);

    if (lepton_object_is_picture (object))
    {
      gboolean status;

      /* Erase previous picture */
      o_invalidate (w_current, object);

      status = lepton_picture_object_set_from_file (object, filename, error);
      if (!status) return FALSE;

      /* Draw new picture */
      o_invalidate (w_current, object);
    }
  }
  return TRUE;
}

/*! \brief Create dialog to exchange picture objects
 *  \par Function Description
 *  This function opens a file chooser and replaces all pictures of the selections
 *  with the new picture.
 *
 *  \todo Maybe merge this dialog function with picture_selection_dialog()
 */
void picture_change_filename_dialog (GschemToplevel *w_current)
{
  gchar *filename;
  gboolean result;
  GError *error = NULL;

  GtkWidget* pfswindow = gtk_file_chooser_dialog_new (_("Select Picture"),
                                                      GTK_WINDOW(w_current->main_window),
                                                      GTK_FILE_CHOOSER_ACTION_OPEN,
                                                      _("_Cancel"),
                                                      GTK_RESPONSE_CANCEL,
                                                      _("_Open"),
                                                      GTK_RESPONSE_ACCEPT,
                                                      NULL);

  setup_filechooser_filters (GTK_FILE_CHOOSER (pfswindow));

#ifndef ENABLE_GTK3
  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(pfswindow),
                                          GTK_RESPONSE_ACCEPT,
                                          GTK_RESPONSE_CANCEL,
                                          -1);
#endif

  if (w_current->pixbuf_filename)
    gtk_file_chooser_set_filename(GTK_FILE_CHOOSER(pfswindow),
                                  w_current->pixbuf_filename);

  if (gtk_dialog_run (GTK_DIALOG (pfswindow)) == GTK_RESPONSE_ACCEPT) {

    filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (pfswindow));
    result = o_picture_exchange (w_current, filename, &error);

    if (!result) {
      GtkWidget *dialog;

      dialog = gtk_message_dialog_new (GTK_WINDOW (w_current->main_window),
                                       GTK_DIALOG_DESTROY_WITH_PARENT,
                                       GTK_MESSAGE_ERROR,
                                       GTK_BUTTONS_CLOSE,
                                       _("Failed to replace pictures: %s"),
                                       error->message);
      gtk_window_set_title (GTK_WINDOW (dialog), "lepton-schematic");
      /* Wait for any user response */
      gtk_dialog_run (GTK_DIALOG (dialog));

      g_error_free (error);
      gtk_widget_destroy(dialog);
    } else {
      schematic_window_active_page_changed (w_current);
    }
    g_free (filename);
  }

  gtk_widget_destroy(pfswindow);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] pixbuf
 *  \param [in] filename
 */
void o_picture_set_pixbuf(GschemToplevel *w_current,
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
