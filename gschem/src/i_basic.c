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

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void i_update_status(TOPLEVEL *w_current, const char *string)
{
  if (!w_current->status_label) {
    return;
  }

  if (string) {
    /* NOTE: consider optimizing this if same label */
    w_current->DONT_RESIZE=1;
    gtk_label_set(GTK_LABEL(w_current->status_label),
                  (char *) string);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* Prefix to i_show_state(), i_set_state() and i_set_state_msg()
 , Allows expose events to happen
 * *EK* Egil Kvaleberg
 */
void i_allow_expose(void)
{
  /* This function is probably not needed and should be deleted eventually */
#if 0
   /* HACK: This one allows the expose events to happen? */
    w_current->DONT_EXPOSE = 1;
#endif
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* Get string corresponding to the currently selected mode
 * The returned string will only last until the next time
 * the function is called (which is probably just fine, really)
 * *EK* Egil Kvaleberg
 */
static const char *i_status_string(TOPLEVEL *w_current)
{
  static char *buf = 0;

  switch ( w_current->event_state ) {
    case NONE:
    case STARTROUTENET: /*! \todo */
    case ENDROUTENET: /*! \todo */
      return "";
    case STARTSELECT:
    case SELECT:
    case SBOX:
    case GRIPS:
      return _("Select Mode");
    case DRAWATTRIB:
    case ENDATTRIB:
      return _("Attribute Mode"); /*EK* new */
    case DRAWCOMP:
    case ENDCOMP:
      return _("Component Mode"); /*EK* new */
    case TEXTENTRY:
    case ENDTEXT:
    case DRAWTEXT:
      return _("Text Mode"); /*EK* new */
    case STARTCOPY:
    case ENDCOPY:
      return _("Copy Mode");
    case STARTMOVE:
    case ENDMOVE:
      return _("Move Mode");
    case ENDROTATEP:
      return _("Rotate Mode");
    case ENDMIRROR:
      return _("Mirror Mode");
    case ZOOM:
    case ZOOMBOXEND:
    case ZOOMBOXSTART:
      return _("Zoom Box");
    case STARTPAN:
    case PAN:
    case MOUSEPAN:
      return _("Pan Mode");
    case STARTPASTE:
    case ENDPASTE:
      if (buf) g_free(buf);
      buf = g_strdup_printf(_("Paste %d Mode"), w_current->buffer_number+1);
      return buf;
    case STARTDRAWNET:
    case DRAWNET:
    case NETCONT:
      return _("Net Mode");
    case STARTDRAWBUS:
    case DRAWBUS:
    case BUSCONT:
      return _("Bus Mode");
    case DRAWLINE:
    case ENDLINE:
      return _("Line Mode");
    case DRAWBOX:
    case ENDBOX:
      return _("Box Mode");
    case DRAWPICTURE:
    case ENDPICTURE:
      return _("Picture Mode");
    case DRAWCIRCLE:
    case ENDCIRCLE:
      return _("Circle Mode");
    case DRAWARC:
    case ENDARC:
      return _("Arc Mode");
    case DRAWPIN:
    case ENDPIN:
      return _("Pin Mode");
    case COPY:
      return _("Copy");
    case MOVE:
      return _("Move");
    case MCOPY:
      return _("Multiple Copy");
    case STARTMCOPY:
    case ENDMCOPY:
      return _("Multiple Copy Mode");
  }
  return ""; /* should not happen */
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* Show state field on screen,
 * possibly with the addition of an extra message
 * *EK* Egil Kvaleberg
 */
void i_show_state(TOPLEVEL *w_current, const char *message)
{
  static char *buf = 0;
  char *aux = 0;
  const char *state_string = i_status_string(w_current);
  const char *what_to_say;

  /* This may be a memory leak... either way it's weird and -Wall is */
  /* complaining... */
  buf = g_strdup("");
  aux = buf;

  if ( message != NULL) {
    if ( message && message[0] ) {
      buf = g_strdup_printf("%s - ", message);
      if (aux) g_free(aux);
      aux = buf;
    }
  }

  if ( !w_current->snap ) {
    buf = g_strdup_printf("%s%s - ", buf, _("Snap Off"));
    if (aux) g_free(aux);
    aux = buf;
  }

  if ( w_current->show_hidden_text ) {
    buf = g_strdup_printf("%s%s - ", buf, _("Show Hidden"));
    if (aux) g_free(aux);
    aux = buf;
  }

  if (buf != NULL) {
    if (strlen(buf) > 0) {
      buf = g_strdup_printf("%s%s", buf, state_string);
      if (aux) g_free(aux);
      aux = buf;
      what_to_say = buf;
    } else {
      what_to_say = state_string;
    }
  }
  else {
    what_to_say = state_string;
  }

  i_update_status(w_current, what_to_say);
  if (buf != NULL) g_free(buf);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* Set new state, then show state field
 * *EK* Egil Kvaleberg
 */
void i_set_state(TOPLEVEL *w_current, enum x_states newstate)
{
  i_set_state_msg(w_current, newstate, NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* Set new state, then show state field including some message
 * *EK* Egil Kvaleberg
 */
void i_set_state_msg(TOPLEVEL *w_current, enum x_states newstate,
		     const char *message)
{
  w_current->event_state = newstate;
  i_show_state(w_current, message);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void i_update_left_button(const char *string)
{
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void i_update_middle_button(TOPLEVEL *w_current,
			    void (*func_ptr)(gpointer, guint, GtkWidget*), const char *string)
{
  char *temp_string;

  if (func_ptr == NULL) {
    return;
  }

  if (string == NULL) {
    return;
  }	

  if (!w_current->middle_label) {
    return;
  }

  switch(w_current->middle_button) {

    /* remove this case eventually and make it a null case */
    case(ACTION):
    w_current->DONT_RESIZE = 1;
    gtk_label_set(GTK_LABEL(w_current->middle_label),
                  _("Action"));
    break;

#ifdef HAS_LIBSTROKE
    case(STROKE):
    w_current->DONT_RESIZE = 1;

    gtk_label_set(GTK_LABEL(w_current->middle_label),
                  _("Stroke"));
    break;
#else 
    /* remove this case eventually and make it a null case */
    case(STROKE):
    w_current->DONT_RESIZE = 1;
    gtk_label_set(GTK_LABEL(w_current->middle_label),
                  _("none"));
    break;
#endif
		
    case(REPEAT):
    w_current->DONT_RESIZE = 1;

    temp_string = g_strconcat (_("Repeat/"), string, NULL);

    gtk_label_set(GTK_LABEL(w_current->middle_label),
                  temp_string);
    w_current->last_callback = func_ptr;
    g_free(temp_string);
    break;

  }

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void i_update_right_button(const char *string)
{
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void i_update_toolbar(TOPLEVEL *w_current)
{
  if (!w_current->toolbars) 
	return;

  switch(w_current->event_state) {
    case(NONE):
    case(SELECT):
    case(STARTSELECT): 
    case(TEXTENTRY): 
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(
				   w_current->toolbar_select), TRUE);
      break;
      
    case(DRAWNET): 
    case(STARTDRAWNET): 
    case(NETCONT): 
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(
				   w_current->toolbar_net), TRUE);
      break;
      
    case(DRAWBUS): 
    case(STARTDRAWBUS): 
    case(BUSCONT): 
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(
				   w_current->toolbar_bus), TRUE);
      break;
      
    case(DRAWCOMP): /*! \todo */
    case(DRAWLINE): /*! \todo */
    case(DRAWBOX): /*! \todo */
    case(DRAWPICTURE): /*! \todo */
    case(DRAWPIN): /*! \todo */
    case(DRAWCIRCLE): /*! \todo */
    case(DRAWARC): /*! \todo */
    case(MOVE): /*! \todo */
    case(COPY): /*! \todo */
    case(ZOOM): /*! \todo */
    case(PAN): /*! \todo */
    case(STARTPAN): /*! \todo */
    case(STARTCOPY): /*! \todo */
    case(STARTMOVE): /*! \todo */
    case(ENDCOPY): /*! \todo */
    case(ENDMOVE): /*! \todo */
    case(ENDLINE): /*! \todo */
    case(ENDBOX): /*! \todo */
    case(ENDPICTURE): /*! \todo */
    case(ENDCIRCLE): /*! \todo */
    case(ENDARC): /*! \todo */
    case(ENDPIN): /*! \todo */
    case(ENDCOMP): /*! \todo */
    case(DRAWATTRIB): /*! \todo */
    case(ENDATTRIB): /*! \todo */
    case(DRAWTEXT): /*! \todo */
    case(ENDTEXT): /*! \todo */
    case(ENDROTATEP): /*! \todo */
    case(ENDMIRROR): /*! \todo */
    case(ZOOMBOXSTART): /*! \todo */
    case(ZOOMBOXEND): /*! \todo */
    case(STARTROUTENET): /*! \todo */
    case(ENDROUTENET): /*! \todo */
    case(MOUSEPAN): /*! \todo */
    case(STARTPASTE): /*! \todo */
    case(ENDPASTE): /*! \todo */
    case(GRIPS): /*! \todo */
    case(MCOPY): /*! \todo */
    case(STARTMCOPY): /*! \todo */
    case(ENDMCOPY): /*! \todo */
    default:
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(
				   w_current->toolbar_select), TRUE);
      break;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void i_update_menus(TOPLEVEL *w_current)
{
  /* 
   * This is very simplistic.  Right now it just disables all menu
   * items which get greyed out when a component is not selected.
   * Eventually what gets enabled/disabled
   * should be based on what is in the selection list 
   */
  
  if (o_selection_return_num(w_current->page_current->selection2_head)) {
    /* since one or more things are selected, we set these TRUE */
    /* These strings should NOT be internationalized */
    x_menus_sensitivity(w_current, "Edit/Cut Buffer", TRUE);
    x_menus_sensitivity(w_current, "Edit/Copy Buffer", TRUE);
    x_menus_sensitivity(w_current, "Edit/Edit...", TRUE);
    x_menus_sensitivity(w_current, "Edit/Edit Text...", TRUE);
    x_menus_sensitivity(w_current, "Edit/Copy Mode", TRUE);
    x_menus_sensitivity(w_current, "Edit/Multiple Copy Mode", TRUE);
    x_menus_sensitivity(w_current, "Edit/Move Mode", TRUE);
    x_menus_sensitivity(w_current, "Edit/Delete", TRUE);
    x_menus_sensitivity(w_current, "Edit/Rotate 90 Mode", TRUE);
    x_menus_sensitivity(w_current, "Edit/Mirror Mode", TRUE);
    x_menus_sensitivity(w_current, "Edit/Slot...", TRUE);
    x_menus_sensitivity(w_current, "Edit/Color...", TRUE);
    x_menus_sensitivity(w_current, "Edit/Lock", TRUE);
    x_menus_sensitivity(w_current, "Edit/Unlock", TRUE);
    x_menus_sensitivity(w_current, "Edit/Line Width & Type...", TRUE);
    x_menus_sensitivity(w_current, "Edit/Fill Type...", TRUE);
    x_menus_sensitivity(w_current, "Edit/Embed Component/Picture", TRUE);
    x_menus_sensitivity(w_current, "Edit/Unembed Component/Picture", TRUE);
    x_menus_sensitivity(w_current, "Edit/Update Component", TRUE);
    x_menus_sensitivity(w_current, "Buffer/Copy into 1", TRUE);
    x_menus_sensitivity(w_current, "Buffer/Copy into 2", TRUE);
    x_menus_sensitivity(w_current, "Buffer/Copy into 3", TRUE);
    x_menus_sensitivity(w_current, "Buffer/Copy into 4", TRUE);
    x_menus_sensitivity(w_current, "Buffer/Copy into 5", TRUE);
    x_menus_sensitivity(w_current, "Buffer/Cut into 1", TRUE);
    x_menus_sensitivity(w_current, "Buffer/Cut into 2", TRUE);
    x_menus_sensitivity(w_current, "Buffer/Cut into 3", TRUE);
    x_menus_sensitivity(w_current, "Buffer/Cut into 4", TRUE);
    x_menus_sensitivity(w_current, "Buffer/Cut into 5", TRUE);
    x_menus_sensitivity(w_current, "Hierarchy/Down Schematic", TRUE);
    x_menus_sensitivity(w_current, "Hierarchy/Down Symbol", TRUE);
    x_menus_sensitivity(w_current, "Hierarchy/Documentation", TRUE);
    x_menus_sensitivity(w_current, "Attributes/Attach", TRUE);
    x_menus_sensitivity(w_current, "Attributes/Detach", TRUE);
    x_menus_sensitivity(w_current, "Attributes/Show Value", TRUE);
    x_menus_sensitivity(w_current, "Attributes/Show Name", TRUE);
    x_menus_sensitivity(w_current, "Attributes/Show Both", TRUE);
    x_menus_sensitivity(w_current, "Attributes/Toggle Visibility", TRUE);

    /*  Menu items for hierarchy added by SDB 1.9.2005.  */
    x_menus_popup_sensitivity(w_current, "/Down Schematic", TRUE);
    x_menus_popup_sensitivity(w_current, "/Down Symbol", TRUE);
    /* x_menus_popup_sensitivity(w_current, "/Up", TRUE); */

  } else {
    /* Nothing is slected.  grey these out */
    /* These strings should NOT be internationalized */
    x_menus_sensitivity(w_current, "Edit/Cut Buffer", FALSE);
    x_menus_sensitivity(w_current, "Edit/Copy Buffer", FALSE);
    x_menus_sensitivity(w_current, "Edit/Edit...", FALSE);
    x_menus_sensitivity(w_current, "Edit/Edit Text...", FALSE);
    x_menus_sensitivity(w_current, "Edit/Copy Mode", FALSE);
    x_menus_sensitivity(w_current, "Edit/Multiple Copy Mode", FALSE);
    x_menus_sensitivity(w_current, "Edit/Move Mode", FALSE);
    x_menus_sensitivity(w_current, "Edit/Delete", FALSE);
    x_menus_sensitivity(w_current, "Edit/Rotate 90 Mode", FALSE);
    x_menus_sensitivity(w_current, "Edit/Mirror Mode", FALSE);
    x_menus_sensitivity(w_current, "Edit/Slot...", FALSE);
    x_menus_sensitivity(w_current, "Edit/Color...", FALSE);
    x_menus_sensitivity(w_current, "Edit/Lock", FALSE);
    x_menus_sensitivity(w_current, "Edit/Unlock", FALSE);
    x_menus_sensitivity(w_current, "Edit/Line Width & Type...", FALSE);
    x_menus_sensitivity(w_current, "Edit/Fill Type...", FALSE);
    x_menus_sensitivity(w_current, "Edit/Embed Component/Picture", FALSE);
    x_menus_sensitivity(w_current, "Edit/Unembed Component/Picture", FALSE);
    x_menus_sensitivity(w_current, "Edit/Update Component", FALSE);
    x_menus_sensitivity(w_current, "Buffer/Copy into 1", FALSE);
    x_menus_sensitivity(w_current, "Buffer/Copy into 2", FALSE);
    x_menus_sensitivity(w_current, "Buffer/Copy into 3", FALSE);
    x_menus_sensitivity(w_current, "Buffer/Copy into 4", FALSE);
    x_menus_sensitivity(w_current, "Buffer/Copy into 5", FALSE);
    x_menus_sensitivity(w_current, "Buffer/Cut into 1", FALSE);
    x_menus_sensitivity(w_current, "Buffer/Cut into 2", FALSE);
    x_menus_sensitivity(w_current, "Buffer/Cut into 3", FALSE);
    x_menus_sensitivity(w_current, "Buffer/Cut into 4", FALSE);
    x_menus_sensitivity(w_current, "Buffer/Cut into 5", FALSE);
    x_menus_sensitivity(w_current, "Hierarchy/Down Schematic", FALSE);
    x_menus_sensitivity(w_current, "Hierarchy/Down Symbol", FALSE);
    x_menus_sensitivity(w_current, "Hierarchy/Documentation", FALSE);
    x_menus_sensitivity(w_current, "Attributes/Attach", FALSE);
    x_menus_sensitivity(w_current, "Attributes/Detach", FALSE);
    x_menus_sensitivity(w_current, "Attributes/Show Value", FALSE);
    x_menus_sensitivity(w_current, "Attributes/Show Name", FALSE);
    x_menus_sensitivity(w_current, "Attributes/Show Both", FALSE);
    x_menus_sensitivity(w_current, "Attributes/Toggle Visibility", FALSE);

    /*  Menu items for hierarchy added by SDB 1.9.2005.  */
    x_menus_popup_sensitivity(w_current, "/Down Schematic", FALSE);
    x_menus_popup_sensitivity(w_current, "/Down Symbol", FALSE);
    /* x_menus_popup_sensitivity(w_current, "/Up", FALSE);	*/
  }

  if ((object_buffer[0] != NULL) && (object_buffer[0]->next != NULL)) {
    x_menus_sensitivity(w_current, "Edit/Paste Buffer", TRUE);
    x_menus_sensitivity(w_current, "Buffer/Paste from 1", TRUE);
  } else {
    x_menus_sensitivity(w_current, "Edit/Paste Buffer", FALSE);
    x_menus_sensitivity(w_current, "Buffer/Paste from 1", FALSE);
  }

  if ((object_buffer[1] != NULL) && (object_buffer[1]->next != NULL)) {
    x_menus_sensitivity(w_current, "Buffer/Paste from 2", TRUE);
  } else {
    x_menus_sensitivity(w_current, "Buffer/Paste from 2", FALSE);
  }

  if ((object_buffer[2] != NULL) && (object_buffer[2]->next != NULL)) {
    x_menus_sensitivity(w_current, "Buffer/Paste from 3", TRUE);
  } else {
    x_menus_sensitivity(w_current, "Buffer/Paste from 3", FALSE);
  }

  if ((object_buffer[3] != NULL) && (object_buffer[3]->next != NULL)) {
    x_menus_sensitivity(w_current, "Buffer/Paste from 4", TRUE);
  } else {
    x_menus_sensitivity(w_current, "Buffer/Paste from 4", FALSE);
  }

  if ((object_buffer[4] != NULL) && (object_buffer[4]->next != NULL)) {
    x_menus_sensitivity(w_current, "Buffer/Paste from 5", TRUE);
  } else {
    x_menus_sensitivity(w_current, "Buffer/Paste from 5", FALSE);
  }
 

}
 
#if 0
/* This data is in X bitmap format, and can be created with the 'bitmap'
     utility. */
  #define cursor1_width 16
  #define cursor1_height 16
  static unsigned char cursor1_bits[] = {
   0x00, 0x00, 0x08, 0x38, 0x18, 0x08, 0x38, 0x18, 0x78, 0x08, 0xf8, 0x38,
   0xf8, 0x01, 0xf8, 0x03, 0xf8, 0x07, 0xf8, 0x00, 0xd8, 0x00, 0x88, 0x01,
   0x80, 0x01, 0x00, 0x03, 0x00, 0x03, 0x00, 0x00};

  static unsigned char cursor1mask_bits[] = {
   0x0c, 0x7c, 0x1c, 0x7c, 0x3c, 0x7c, 0x7c, 0x3c, 0xfc, 0x7c, 0xfc, 0x7d,
   0xfc, 0x7f, 0xfc, 0x07, 0xfc, 0x0f, 0xfc, 0x0f, 0xfc, 0x01, 0xdc, 0x03,
   0xcc, 0x03, 0x80, 0x07, 0x80, 0x07, 0x00, 0x03};
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void i_update_cursor(TOPLEVEL *w_current)
{
#if 0
  if (!w_current->cursors) 
	return;
#endif

#if 0
  GdkCursor *cursor;

  GdkPixmap *source, *mask;
  GdkColor fg = { 0, 0, 0, 0 }; /* Red. */
  GdkColor bg = { 0, 65535, 65535, 65535 }; /* Blue. */
    
  source = gdk_bitmap_create_from_data (NULL, cursor1_bits,
					cursor1_width, cursor1_height);
  mask = gdk_bitmap_create_from_data (NULL, cursor1mask_bits,
				      cursor1_width, cursor1_height);
  cursor = gdk_cursor_new_from_pixmap (source, mask, &fg, &bg, 3, 1);
  gdk_pixmap_unref (source);
  gdk_pixmap_unref (mask);
  
  gdk_window_set_cursor (w_current->window, cursor);
#endif


  GdkCursor* cursor;
  cursor = gdk_cursor_new(GDK_ARROW);
  gdk_window_set_cursor(w_current->window, cursor);
  gdk_cursor_destroy(cursor);
  
#if 0
  switch(w_current->event_state) {
    case(NONE):
    case(SELECT):
    case(STARTSELECT): 
    case(TEXTENTRY): 
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(
				   w_current->toolbar_select), TRUE);
      break;

    case(DRAWNET): 
    case(STARTDRAWNET): 
    case(NETCONT): 
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(
				   w_current->toolbar_net), TRUE);
      break;

    case(DRAWBUS): 
    case(STARTDRAWBUS): 
    case(BUSCONT): 
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(
				   w_current->toolbar_bus), TRUE);
      break;

    case(DRAWCOMP): /*! \todo */
    case(DRAWLINE): /*! \todo */
    case(DRAWBOX): /*! \todo */
    case(DRAWPICTURE): /*! \todo */
    case(DRAWPIN): /*! \todo */
    case(DRAWCIRCLE): /*! \todo */
    case(DRAWARC): /*! \todo */
    case(MOVE): /*! \todo */
    case(COPY): /*! \todo */
    case(ZOOM): /*! \todo */
    case(PAN): /*! \todo */
    case(STARTPAN): /*! \todo */
    case(STARTCOPY): /*! \todo */
    case(STARTMOVE): /*! \todo */
    case(ENDCOPY): /*! \todo */
    case(ENDMOVE): /*! \todo */
    case(ENDLINE): /*! \todo */
    case(ENDBOX): /*! \todo */
    case(ENDPICTURE): /*! \todo */
    case(ENDCIRCLE): /*! \todo */
    case(ENDARC): /*! \todo */
    case(ENDPIN): /*! \todo */
    case(ENDCOMP): /*! \todo */
    case(DRAWATTRIB): /*! \todo */
    case(ENDATTRIB): /*! \todo */
    case(DRAWTEXT): /*! \todo */
    case(ENDTEXT): /*! \todo */
    case(ENDROTATEP): /*! \todo */
    case(ENDMIRROR): /*! \todo */
    case(ZOOMBOXSTART): /*! \todo */
    case(ZOOMBOXEND): /*! \todo */
    case(STARTROUTENET): /*! \todo */
    case(ENDROUTENET): /*! \todo */
    case(MOUSEPAN): /*! \todo */
    case(STARTPASTE): /*! \todo */
    case(ENDPASTE): /*! \todo */
    case(GRIPS): /*! \todo */
    case(MCOPY): /*! \todo */
    case(MCOPYSTART): /*! \todo */
    case(MCOPYEND): /*! \todo */
    default:
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(
				   w_current->toolbar_select), TRUE);
      break;
  }
#endif
}

/*! \brief Set filename as gschem  window title
 *  \par Function Description
 *  Set the window title using the gnome HID format style.
 */
void i_set_filename(TOPLEVEL *w_current, const gchar *string)
{
  gchar *print_string=NULL;
  gchar *filename=NULL;

  if (!w_current->main_window)
    return;
  if (string == NULL)
    return;

  filename = g_path_get_basename(string);
  
  print_string = g_strdup_printf("%s - gschem", filename);
  
  /* alternativ code with length limited pathname */
/*  int max_len = 70;
    if (strlen(string) > max_len) {
    print_string = g_strdup_printf("gschem: ...%s",
				   &(string[strlen(string)-max_len]));
  }
  else {
    print_string = g_strdup_printf("gschem: %s",string);
  }
*/

  gtk_window_set_title(GTK_WINDOW(w_current->main_window),
		       print_string);
  
  g_free(print_string);
  g_free(filename);
}

/*! \brief Write the grid settings to the gschem status bar
 *  \par Function Description
 *  The function takes the current grid paramters of gschem and
 *  prints it to the status bar.
 *  The format is "Grid([SnapGridSize],[CurrentGridSize])"
 */
void i_set_grid(TOPLEVEL *w_current, int visible_grid) 
{
  gchar *print_string=NULL;
  gchar *snap=NULL;
  gchar *grid=NULL;

  if (!w_current->grid_label) {
    return;
  }
  
  if (!w_current->snap)
    snap = g_strdup(_("OFF"));
  else
    snap = g_strdup_printf("%d", w_current->snap_size);

  if (!w_current->grid)
    grid = g_strdup(_("OFF"));
  else
    grid = g_strdup_printf("%d", visible_grid);

  print_string = g_strdup_printf(_("Grid(%s, %s)"), snap, grid);
  gtk_label_set(GTK_LABEL(w_current->grid_label), print_string);
  
  g_free(print_string);
  g_free(grid);
  g_free(snap);
}
