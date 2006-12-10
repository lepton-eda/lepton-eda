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
/*! \todo STILL NEED to clean up line lengths in aa and tr */
#include <config.h>

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/i_vars.h"
#include "../include/globals.h"
#include "../include/prototype.h"
#include "../include/x_dialog.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define GLADE_HOOKUP_OBJECT(component,widget,name) \
  g_object_set_data_full (G_OBJECT (component), name, \
    gtk_widget_ref (widget), (GDestroyNotify) gtk_widget_unref)

#define DIALOG_BORDER_SPACING 10
#define DIALOG_ELEMENT_SPACING 10


static GtkWidget* create_menu_linetype (TOPLEVEL *w_current);
static gint line_type_dialog_linetype_change (GtkWidget *w, gpointer data);
static void line_type_dialog_ok (GtkWidget *w, gpointer data);

static GtkWidget* create_menu_filltype (TOPLEVEL *w_current);
static gint fill_type_dialog_filltype_change(GtkWidget *w, gpointer data);
static void fill_type_dialog_ok(GtkWidget *w, gpointer data);


struct line_type_data {
  GtkWidget *dialog;
  GtkWidget *width_entry;
  GtkWidget *line_type;
  GtkWidget *length_entry;
  GtkWidget *space_entry;

  TOPLEVEL *toplevel;
  GList *objects;
};

struct fill_type_data {
  GtkWidget *dialog;
  GtkWidget *fill_type;
  GtkWidget *width_entry;
  GtkWidget *angle1_entry;
  GtkWidget *pitch1_entry;
  GtkWidget *angle2_entry;
  GtkWidget *pitch2_entry;

  TOPLEVEL *toplevel;
  GList *objects;
};

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void destroy_window(GtkWidget *widget, GtkWidget **window)
{
  *window = NULL;
}

/***************** Start of Text Input dialog box *********************/
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int text_input_dialog_keypress(GtkWidget * widget, GdkEventKey * event, 
			       TOPLEVEL * w_current)
{
   if (strcmp(gdk_keyval_name(event->keyval), "Escape") == 0) {
	text_input_dialog_close(NULL, w_current);	
 	return TRUE;
   }

   return FALSE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void text_input_dialog_apply(GtkWidget *w, TOPLEVEL *w_current)
{
  int len;
  char *string = NULL;
  GtkWidget *tientry;
  GtkTextBuffer *textbuffer;
  GtkTextIter start, end;

  tientry = gtk_object_get_data(GTK_OBJECT(w_current->tiwindow),"tientry");

  textbuffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(tientry));
  gtk_text_buffer_get_bounds (textbuffer, &start, &end);
  string =  gtk_text_iter_get_text (&start, &end);

  if (string[0] != '\0' ) {
    len = strlen(string);
#if DEBUG
    printf("text was: _%s_ %d\n", string, len);
#endif
    switch(w_current->text_caps) {
      case(LOWER):
        string_tolower(string, string);
        break;

      case(UPPER):
        string_toupper(string, string);
        break;

      case(BOTH):
      default:
				/* do nothing */
        break;
    }

    o_attrib_set_string(w_current, string);
    w_current->page_current->CHANGED=1;
    gtk_text_buffer_set_text(textbuffer, w_current->current_attribute, -1);
    select_all_text_in_textview(GTK_TEXT_VIEW(tientry));
    gtk_widget_grab_focus(tientry);

    w_current->event_state = DRAWTEXT;
    w_current->inside_action = 1;
  }

#if 0
  gtk_grab_remove(w_current->tiwindow);
  gtk_widget_destroy(w_current->tiwindow);
  w_current->tiwindow = NULL;
#endif
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void text_input_dialog_close(GtkWidget *w, TOPLEVEL *w_current)
{
  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);
#if 0
  gtk_grab_remove(w_current->tiwindow);
#endif
  gtk_widget_destroy(w_current->tiwindow);
  w_current->tiwindow=NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void text_input_dialog (TOPLEVEL *w_current)
{
  GtkWidget *label = NULL;
  GtkWidget *tientry = NULL;
  GtkWidget *buttonok     = NULL;
  GtkWidget *buttoncancel = NULL;
  GtkWidget *vbox, *action_area;
  GtkWidget *viewport1 = NULL;
  GtkWidget *scrolled_window = NULL;
  PangoTabArray *tab_array;
  int real_tab_width;

  if (!w_current->tiwindow) {
    w_current->tiwindow = x_create_dialog_box(&vbox, &action_area);

#if 0
    gtk_window_position(GTK_WINDOW (w_current->tiwindow),
                        GTK_WIN_POS_MOUSE);
#endif

    /*gtk_widget_set_usize(w_current->tiwindow, 400,130); no longer fixed size*/

    gtk_window_position(GTK_WINDOW (w_current->tiwindow),
                        GTK_WIN_POS_NONE);

    gtk_signal_connect(GTK_OBJECT (w_current->tiwindow),
                       "destroy", GTK_SIGNAL_FUNC(destroy_window),
                       &w_current->tiwindow);

    gtk_signal_connect(GTK_OBJECT(w_current->tiwindow),
                     "key_press_event",
                     (GtkSignalFunc) text_input_dialog_keypress, w_current);

#if 0 /* removed because it was causing the dialog box to not close */
    gtk_signal_connect(GTK_OBJECT (w_current->tiwindow),
                       "delete_event",
                       GTK_SIGNAL_FUNC(destroy_window),
                       &w_current->tiwindow);
#endif

    gtk_window_set_title(GTK_WINDOW (w_current->tiwindow),
                         _("Text Entry..."));
    gtk_container_border_width(
                               GTK_CONTAINER (w_current->tiwindow), 5);

    label = gtk_label_new (_("Enter text, click apply,\nmove cursor into window, click to place text.\nMiddle button to rotate while placing."));
    gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, TRUE, 5);
    gtk_widget_show (label);

    viewport1 = gtk_viewport_new (NULL, NULL);
    gtk_widget_show (viewport1);
    
    scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
				   GTK_POLICY_AUTOMATIC, 
				   GTK_POLICY_AUTOMATIC);
    gtk_container_add (GTK_CONTAINER (viewport1), scrolled_window);
    gtk_box_pack_start( GTK_BOX(vbox), viewport1, TRUE, TRUE, 10);
    gtk_widget_show(scrolled_window);

    tientry = gtk_text_view_new();
    gtk_text_view_set_editable(GTK_TEXT_VIEW(tientry), TRUE);
    select_all_text_in_textview(GTK_TEXT_VIEW(tientry));

    /* Set the tab width, using pango tab array */
    /*! \bug FIXME: This doesn't work. Why? */
    tab_array = pango_tab_array_new (1, TRUE);
    real_tab_width = text_view_calculate_real_tab_width(GTK_TEXT_VIEW(tientry),
							tab_in_chars);
    if (real_tab_width >= 0) {
      pango_tab_array_set_tab (tab_array, 0, PANGO_TAB_LEFT, real_tab_width);
      /* printf("Real tab width: %i\n", real_tab_width);*/
      gtk_text_view_set_tabs (GTK_TEXT_VIEW (tientry), 
			      tab_array);
    }
    else {
      g_warning ("text_input_dialog: Impossible to set tab width.\n");
    }
    pango_tab_array_free (tab_array);
    gtk_container_add(GTK_CONTAINER(scrolled_window), tientry);

    gtk_widget_show(tientry);
    gtk_widget_grab_focus(tientry);
    gtk_object_set_data(GTK_OBJECT(w_current->tiwindow),
                        "tientry",tientry);

    buttoncancel = gtk_button_new_from_stock (GTK_STOCK_CLOSE);
    GTK_WIDGET_SET_FLAGS (buttoncancel, GTK_CAN_DEFAULT);
    gtk_box_pack_start(
                       GTK_BOX(action_area),
                       buttoncancel, TRUE, TRUE, 0);
    gtk_signal_connect (GTK_OBJECT (buttoncancel), "clicked",
                        GTK_SIGNAL_FUNC(text_input_dialog_close),
                        w_current);
    gtk_widget_show (buttoncancel);

    buttonok = gtk_button_new_from_stock (GTK_STOCK_APPLY);
    GTK_WIDGET_SET_FLAGS (buttonok, GTK_CAN_DEFAULT);
    gtk_box_pack_start(
                       GTK_BOX(action_area),
                       buttonok, TRUE, TRUE, 0);
    gtk_signal_connect(GTK_OBJECT (buttonok), "clicked",
                       GTK_SIGNAL_FUNC(text_input_dialog_apply),
                       w_current);
    gtk_widget_show (buttonok);
    gtk_widget_grab_default (buttonok);
    
  }

  if (!GTK_WIDGET_VISIBLE (w_current->tiwindow)) {
    gtk_widget_show (w_current->tiwindow);
#if 0
    gtk_grab_add (w_current->tiwindow);
#endif
  } else {
    gdk_window_raise(w_current->tiwindow->window);
  }
}

/***************** End of Text Input dialog box ***********************/

/***************** Start of Text Edit dialog box **********************/
/*! \brief CAllback for a text aligment change
 *  \par Function Description
 *  This function stores a change of the text alignment in the
 *  <b>TOPLEVEL</b> struct.
 *  \todo Remove that function. Only the OK-Button should set any
 *  properties in the TOPLEVEL struct.
 */
gint change_alignment(GtkWidget *w, TOPLEVEL *w_current)
{
  char *alignment;
  alignment = gtk_object_get_data(GTK_OBJECT(w),"alignment");
  w_current->text_alignment = atoi(alignment);

  /*w_current->page_current->CHANGED=1; I don't think this belongs */
  /* o_undo_savestate(w_current, UNDO_ALL); I don't think this belongs */
	
  return 0;
}

/*! \brief Create the alignment menu for the text property dialog
 *  \par Function Description
 *  This function creates a GtkMenu with nine different alignment 
 *  entries.
 */
static GtkWidget *create_menu_alignment (TOPLEVEL *w_current)
{
  GtkWidget *menu;
  GtkWidget *menuitem;
  GSList *group;
  char *buf;

  menu = gtk_menu_new ();
  group = NULL;

  buf = g_strdup_printf( _("Lower Left"));
  menuitem = gtk_radio_menu_item_new_with_label (group, buf);
  g_free(buf);
  group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
  gtk_menu_append (GTK_MENU (menu), menuitem);
  gtk_object_set_data (GTK_OBJECT(menuitem), "alignment", "0");
  gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
                     (GtkSignalFunc) change_alignment,
                     w_current);
  gtk_widget_show (menuitem);

  buf = g_strdup_printf( _("Middle Left"));
  menuitem = gtk_radio_menu_item_new_with_label (group, buf);
  g_free(buf);
  group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
  gtk_menu_append (GTK_MENU (menu), menuitem);
  gtk_object_set_data (GTK_OBJECT(menuitem), "alignment", "1");
  gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
                     (GtkSignalFunc) change_alignment,
                     w_current);
  gtk_widget_show (menuitem);

  buf = g_strdup_printf( _("Upper Left"));
  menuitem = gtk_radio_menu_item_new_with_label (group, buf);
  g_free(buf);
  group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
  gtk_menu_append (GTK_MENU (menu), menuitem);
  gtk_object_set_data (GTK_OBJECT(menuitem), "alignment", "2");
  gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
                     (GtkSignalFunc) change_alignment,
                     w_current);
  gtk_widget_show (menuitem);

  buf = g_strdup_printf( _("Lower Middle"));
  menuitem = gtk_radio_menu_item_new_with_label (group, buf);
  g_free(buf);
  group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
  gtk_menu_append (GTK_MENU (menu), menuitem);
  gtk_object_set_data (GTK_OBJECT(menuitem), "alignment", "3");
  gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
                     (GtkSignalFunc) change_alignment,
                     w_current);
  gtk_widget_show (menuitem);

  buf = g_strdup_printf( _("Middle Middle"));
  menuitem = gtk_radio_menu_item_new_with_label (group, buf);
  g_free(buf);
  group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
  gtk_menu_append (GTK_MENU (menu), menuitem);
  gtk_object_set_data (GTK_OBJECT(menuitem), "alignment", "4");
  gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
                     (GtkSignalFunc) change_alignment,
                     w_current);
  gtk_widget_show (menuitem);

  buf = g_strdup_printf( _("Upper Middle"));
  menuitem = gtk_radio_menu_item_new_with_label (group, buf);
  g_free(buf);
  group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
  gtk_menu_append (GTK_MENU (menu), menuitem);
  gtk_object_set_data (GTK_OBJECT(menuitem), "alignment", "5");
  gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
                     (GtkSignalFunc) change_alignment,
                     w_current);
  gtk_widget_show (menuitem);

  buf = g_strdup_printf( _("Lower Right"));
  menuitem = gtk_radio_menu_item_new_with_label (group, buf);
  g_free(buf);
  group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
  gtk_menu_append (GTK_MENU (menu), menuitem);
  gtk_object_set_data (GTK_OBJECT(menuitem), "alignment", "6");
  gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
                     (GtkSignalFunc) change_alignment,
                     w_current);
  gtk_widget_show (menuitem);

  buf = g_strdup_printf( _("Middle Right"));
  menuitem = gtk_radio_menu_item_new_with_label (group, buf);
  g_free(buf);
  group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
  gtk_menu_append (GTK_MENU (menu), menuitem);
  gtk_object_set_data (GTK_OBJECT(menuitem), "alignment", "7");
  gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
                     (GtkSignalFunc) change_alignment,
                     w_current);
  gtk_widget_show (menuitem);

  buf = g_strdup_printf( _("Upper Right"));
  menuitem = gtk_radio_menu_item_new_with_label (group, buf);
  g_free(buf);
  group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
  gtk_menu_append (GTK_MENU (menu), menuitem);
  gtk_object_set_data (GTK_OBJECT(menuitem), "alignment", "8");
  gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
                     (GtkSignalFunc) change_alignment,
                     w_current);
  gtk_widget_show (menuitem);

  return menu;
}

/* we reuse the color menu so we need to declare it */
static GtkWidget *create_color_menu(TOPLEVEL * w_current, int * select_index);

/*! \brief Apply the settings from the text property dialog
 *  \par Function Description
 *  This function applies the user settings to the selected text objects
 *  and closes the dialog
 *  \todo Check why we have no color attribute in that dialog
 */
void text_edit_dialog_ok(GtkWidget *w, TOPLEVEL *w_current)
{
  int len=0;
  int text_size=8;
  char *text_string = NULL;
  char *text_size_string = NULL;
  int new_text_alignment;
  int num_selected;
  GtkTextBuffer *textbuffer;
  GtkTextIter start, end;
  GtkWidget *widget;

  num_selected = o_selection_return_num(w_current->page_current->selection2_head);

  /* text string entry will only show up if one object is selected */
  if (num_selected == 1) {
    widget = g_object_get_data (G_OBJECT (w_current->tewindow), "textentry");
    textbuffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(widget));
    gtk_text_buffer_get_bounds (textbuffer, &start, &end);
    text_string =  gtk_text_iter_get_text (&start, &end);
  } /* else the string will be null which is okay */
  
  widget = g_object_get_data (G_OBJECT (w_current->tewindow), "sizeentry");
  text_size_string = (char *) gtk_entry_get_text(GTK_ENTRY(widget));

  if (text_string) {
    len = strlen(text_string);
  }

  if (text_size_string) {
    text_size = atoi(text_size_string);
  }
  
  if (text_size == 0) {
    text_size = default_text_size;
  }

  new_text_alignment = w_current->text_alignment;

  o_text_edit_end(w_current, text_string, len, text_size, new_text_alignment);

  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);
  gtk_widget_destroy(w_current->tewindow);
  w_current->tewindow = NULL;
}

/*! 
 *  \brief Cancel function for the text property dialog
 *  \par Function Description
 *  Just close the dialog and clean up.
 *  \todo join the function into text_edit_dialog_response()
 */
void text_edit_dialog_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);
  gtk_widget_destroy(w_current->tewindow);
  w_current->tewindow = NULL;
}

/*! \brief Response function for the text property dialog
 *  \par Function Description
 *  This function receives the user response of the text property dialog.
 *  The response is either <b>OK</b> or <b>Cancel</b>
 *  
 */
void text_edit_dialog_response(GtkWidget * widget, gint response, TOPLEVEL *w_current)
{
  switch(response) {
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    text_edit_dialog_cancel(widget, w_current);
    break;
  case GTK_RESPONSE_ACCEPT:
    text_edit_dialog_ok(widget, w_current);
    break;
  default:
    printf("text_edit_dialog_response(): strange signal %d\n", response);
  }
}

/*! \brief Create the edit text properties dialog
 *  \par Function Description
 *  This Function creates the dialog to edit text properties. 
 *  \todo Check why there's no color in the calling parameters
 */
void text_edit_dialog (TOPLEVEL *w_current, char *string, int text_size,
		       int text_alignment)
{
  GtkWidget *label = NULL;
  GtkWidget *vbox;
  GtkWidget *optionmenu = NULL;
  GtkWidget *align_menu = NULL;
  GtkWidget *viewport1 = NULL;
  GtkWidget *textentry = NULL;
  GtkWidget *sizeentry = NULL;
  GtkWidget *scrolled_window = NULL;
  GtkTextBuffer *textbuffer;
  char *text_size_string;
  int num_selected=0;
  int select_index=0;

  if (!w_current->tewindow) {
    w_current->tewindow = gtk_dialog_new_with_buttons(_("Edit Text Properties"),
						      GTK_WINDOW(w_current->main_window),
						      GTK_DIALOG_MODAL,
						      GTK_STOCK_CANCEL,
						      GTK_RESPONSE_REJECT,
						      GTK_STOCK_OK,
						      GTK_RESPONSE_ACCEPT,
						      NULL);

    gtk_dialog_set_default_response(GTK_DIALOG(w_current->tewindow),
				    GTK_RESPONSE_ACCEPT);

    gtk_signal_connect(GTK_OBJECT(w_current->tewindow), "response",
		       GTK_SIGNAL_FUNC(text_edit_dialog_response), w_current);

    gtk_window_position(GTK_WINDOW (w_current->tewindow),
                        GTK_WIN_POS_MOUSE);

    
    vbox = GTK_DIALOG(w_current->tewindow)->vbox;
    gtk_container_set_border_width(GTK_CONTAINER(w_current->tewindow),5);
    gtk_box_set_spacing(GTK_BOX(vbox),5);

    /* add a text box if only one object is selected */
    num_selected = o_selection_return_num(w_current->page_current->selection2_head);
    if (num_selected == 1) {
      label = gtk_label_new (_("Text Content:"));
      gtk_misc_set_alignment(GTK_MISC(label),0,0);
      gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);
      gtk_widget_show (label);

      viewport1 = gtk_viewport_new (NULL, NULL);
      gtk_widget_show (viewport1);
      gtk_widget_set_size_request(GTK_WIDGET(viewport1),-1,75);

      scrolled_window = gtk_scrolled_window_new(NULL, NULL);
      gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
				     GTK_POLICY_AUTOMATIC, 
				     GTK_POLICY_AUTOMATIC);
      gtk_container_add (GTK_CONTAINER (viewport1), scrolled_window);
      gtk_box_pack_start( GTK_BOX(vbox), viewport1, TRUE, TRUE, 0);
      gtk_widget_show(scrolled_window);
      
      textentry = gtk_text_view_new();
      gtk_text_view_set_editable(GTK_TEXT_VIEW(textentry), TRUE);
      select_all_text_in_textview(GTK_TEXT_VIEW(textentry));

      /*! \bug FIXME: Set tab's width in the textview widget. */
      /* See first the code in text_input_dialog and get it working before adding it here. */

      gtk_container_add(GTK_CONTAINER(scrolled_window), textentry);
      gtk_widget_show (textentry);
      gtk_widget_grab_focus(textentry);
      GLADE_HOOKUP_OBJECT(w_current->tewindow, textentry,"textentry");
    }

    label = gtk_label_new(_("Text Color:"));
    gtk_misc_set_alignment(GTK_MISC(label),0,0);
    gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);
    gtk_widget_show(label);
    
    optionmenu = gtk_option_menu_new();
    gtk_option_menu_set_menu(GTK_OPTION_MENU(optionmenu), 
			     create_color_menu(w_current, &select_index));
    gtk_option_menu_set_history(GTK_OPTION_MENU(optionmenu), select_index);
	
    gtk_box_pack_start(GTK_BOX(vbox), optionmenu, FALSE, TRUE, 0);
    gtk_widget_show(optionmenu);

    label = gtk_label_new (_("Text Size:"));
    gtk_misc_set_alignment(GTK_MISC(label),0,0);
    gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);
    gtk_widget_show (label);

    sizeentry = gtk_entry_new_with_max_length (10);
    gtk_editable_select_region(GTK_EDITABLE (sizeentry), 0, -1);
    gtk_box_pack_start(GTK_BOX(vbox),
                       sizeentry, FALSE, FALSE, 5);
    gtk_widget_show (sizeentry);

    label = gtk_label_new (_("Text Alignment:"));
    gtk_misc_set_alignment(GTK_MISC(label),0,0);
    gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);
    gtk_widget_show (label);

    optionmenu = gtk_option_menu_new ();
    align_menu = create_menu_alignment (w_current);
    gtk_option_menu_set_menu(GTK_OPTION_MENU(optionmenu),
                             align_menu);
    gtk_option_menu_set_history(GTK_OPTION_MENU (optionmenu), 
                                text_alignment);
    w_current->text_alignment = text_alignment;
    gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(gtk_menu_get_active(GTK_MENU(align_menu))),
				   TRUE);

    GLADE_HOOKUP_OBJECT(w_current->tewindow, sizeentry,"sizeentry");

    gtk_box_pack_start(GTK_BOX(vbox), optionmenu, FALSE, TRUE, 0);
    gtk_widget_show(optionmenu);
  }

  if (!GTK_WIDGET_VISIBLE (w_current->tewindow)) {
    gtk_widget_show (w_current->tewindow);
    if (string != NULL) {
      if (num_selected == 1) { /* only if one thing is selected */
	textentry = g_object_get_data (G_OBJECT (w_current->tewindow), "textentry");
	textbuffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(textentry));
	gtk_text_buffer_set_text(GTK_TEXT_BUFFER(textbuffer), string, -1);
	select_all_text_in_textview(GTK_TEXT_VIEW(textentry));
      }
    }

    text_size_string = g_strdup_printf("%d", text_size);
    sizeentry = g_object_get_data (G_OBJECT (w_current->tewindow), "sizeentry");
    gtk_entry_set_text(GTK_ENTRY(sizeentry),
                       text_size_string);
    g_free(text_size_string);
  }
}

/***************** End of Text Edit dialog box ************************/

/***************** Start of Line Type/width dialog box ****************/

/*! \brief Create a line type menu for the line type dialog
 *  \par Function Description
 *  This function creates a GtkMenu with the different linetypes.
 */
static GtkWidget *create_menu_linetype (TOPLEVEL *w_current)
{
  GtkWidget *menu;
  GSList *group;
  struct line_type {
    gchar *str;
    OBJECT_TYPE type;
  } types[] = { { N_("Solid"),   TYPE_SOLID },
                { N_("Dotted"),  TYPE_DOTTED },
                { N_("Dashed"),  TYPE_DASHED },
                { N_("Center"),  TYPE_CENTER },
                { N_("Phantom"), TYPE_PHANTOM } };
  gint i;

  menu  = gtk_menu_new ();
  group = NULL;
  
  for (i = 0; i < sizeof (types) / sizeof (struct line_type); i++) {
    GtkWidget *menuitem;
      
    menuitem = gtk_radio_menu_item_new_with_label (group, _(types[i].str));
    group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
    gtk_menu_append (GTK_MENU (menu), menuitem);
    gtk_object_set_data (GTK_OBJECT(menuitem), "linetype",
                         GINT_TO_POINTER (types[i].type));
    gtk_widget_show (menuitem);
  }

  return(menu);
}

/*! \brief Callback function for the linetype menu item in the line type dialog
 *  \par Function Description
 *  This Function is called when the user changes the line type selection.
 *  It sets the dash space/length entries either active or inactive.
 */
static gint line_type_dialog_linetype_change(GtkWidget *w, gpointer data)
{
  struct line_type_data *line_type_data = (struct line_type_data*) data;
  GtkWidget *menuitem;
  gboolean activate_length_entry, activate_space_entry;
  gint type;

  menuitem = gtk_menu_get_active (
    GTK_MENU (gtk_option_menu_get_menu (
                GTK_OPTION_MENU (line_type_data->line_type))));

  type = GPOINTER_TO_INT(
    gtk_object_get_data (GTK_OBJECT (menuitem), "linetype"));
  switch(type) {
      case(TYPE_SOLID):
        activate_length_entry = FALSE;
        activate_space_entry  = FALSE;
        break;
      case(TYPE_DOTTED):
        activate_length_entry = FALSE;
        activate_space_entry  = TRUE;
        break;
      case(TYPE_DASHED):
      case(TYPE_CENTER):
      case(TYPE_PHANTOM):
        activate_length_entry = TRUE;
        activate_space_entry  = TRUE;
        break;
      default:
        activate_length_entry = TRUE;
        activate_space_entry  = TRUE;
  }

  gtk_widget_set_sensitive (line_type_data->space_entry,
                            activate_space_entry);
  gtk_widget_set_sensitive (line_type_data->length_entry,
                            activate_length_entry);
    
  return(0);
}


/*! \brief Worker function for the line type and width dialog
 *  \par Function Description
 *  The function takes the properties of the dialog and applies
 *  them to the selected objects.
 */
static void line_type_dialog_ok(GtkWidget *w, gpointer data)
{
  struct line_type_data *line_type_data = (struct line_type_data*)data;
  TOPLEVEL *toplevel;
  GList *objects;
  const gchar *width_str, *length_str, *space_str;
  OBJECT_TYPE type;
    
  /* retrieve the list of objects and the toplevel */
  objects  = line_type_data->objects;
  toplevel = line_type_data->toplevel;
    
  /* get the new values from the text entries of the dialog */
  width_str   = gtk_entry_get_text (GTK_ENTRY (
                                      line_type_data->width_entry));
  length_str  = gtk_entry_get_text (GTK_ENTRY (
                                      line_type_data->length_entry));
  space_str   = gtk_entry_get_text (GTK_ENTRY (
                                      line_type_data->space_entry));
  type = GPOINTER_TO_INT(
    gtk_object_get_data (
      GTK_OBJECT (
        gtk_menu_get_active (
          GTK_MENU (gtk_option_menu_get_menu (
                      GTK_OPTION_MENU (
                        line_type_data->line_type))))), "linetype"));
    
  /* are there several objects concerned? */
  if (g_list_next (objects) == NULL) {
    /* no, there is only one object */
    OBJECT *o_current = (OBJECT*) objects->data;
    gint width, length, space;
        
    width  = atoi (width_str);
    length = atoi (length_str);
    space  = atoi (space_str);

    /* apply the new line options to object */
    o_erase_single (toplevel, o_current);
    o_set_line_options (toplevel, o_current,
                        o_current->line_end, 
                        type,
                        width,
                        length,
                        space);
    o_object_recalc (toplevel, o_current);
    o_redraw_single (toplevel, o_current);
      
  } else {
    /* more than one object in the list */
    GList *object;
    gint width, length, space;

    /* get the new line options */
    width  = g_strcasecmp (width_str,
                           _("*unchanged*")) ? atoi (width_str)  : -1;
    length = g_strcasecmp (length_str,
                           _("*unchanged*")) ? atoi (length_str) : -1;
    space  = g_strcasecmp (space_str,
                           _("*unchanged*")) ? atoi (space_str)  : -1;

    /* apply changes to each object */
    object = objects;
    while (object != NULL) {
      OBJECT *o_current = (OBJECT*)object->data;

      o_erase_single (toplevel, o_current);
      o_set_line_options (toplevel, o_current,
                          o_current->line_end, 
                          type   == -1 ? o_current->line_type : type,
                          width  == -1 ? o_current->line_width  : width,
                          length == -1 ? o_current->line_length : length,
                          space  == -1 ? o_current->line_space  : space);
      o_object_recalc (toplevel, o_current);
      o_redraw_single (toplevel, o_current);
          
      object = object->next;
    }
  }

  toplevel->page_current->CHANGED = 1;
}

/*! \brief response function for the line type and width dialog
 *  \par Function Description
 *  This function takes the user input and applies it to selected 
 *  objects.
 *  After that it kills the dialog.
 */
void line_type_dialog_response(GtkWidget *widget, gint response, 
			       struct line_type_data *line_type_data)
{
  switch (response) {
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    /* void */
    break;
  case GTK_RESPONSE_ACCEPT:
    line_type_dialog_ok(widget, line_type_data);
    break;
  default:
    printf("line_type_dialog_response(): strange signal %d\n",response);
  }
    
  i_set_state (line_type_data->toplevel, SELECT);
  i_update_toolbar (line_type_data->toplevel);
  gtk_widget_destroy (line_type_data->dialog);
  
  /* get ride of the list of objects but not the objects */
  g_list_free (line_type_data->objects);
  g_free (line_type_data);
}

/*! \brief Creates the line type and width dialog
 *  \par Function Description
 *  This function creates and sets up a dialog for manipulating the 
 *  line width and the line type setting of objects.
 */
void line_type_dialog (TOPLEVEL *w_current, GList *objects)
{
  GtkWidget *dialog;
  GtkWidget *vbox;
  GtkWidget *optionmenu   = NULL;
  GtkWidget *length_entry = NULL;
  GtkWidget *space_entry  = NULL;
  GtkWidget *width_entry  = NULL;
  GtkWidget *table;
  GtkWidget *label;
  struct line_type_data *line_type_data;
  gchar *width_str, *space_str, *length_str;
  gint type;

  line_type_data = (struct line_type_data*) g_malloc (
    sizeof (struct line_type_data));

  dialog = gtk_dialog_new_with_buttons(_("Edit Line Width & Type"),
				       GTK_WINDOW(w_current->main_window),
				       GTK_DIALOG_MODAL,
				       GTK_STOCK_CANCEL,
				       GTK_RESPONSE_REJECT,
				       GTK_STOCK_OK,
				       GTK_RESPONSE_ACCEPT,
				       NULL);

  gtk_window_position(GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);

  gtk_signal_connect(GTK_OBJECT(dialog), "response",
		     GTK_SIGNAL_FUNC(line_type_dialog_response),
		     line_type_data);

  gtk_container_border_width(GTK_CONTAINER(dialog), 
			     DIALOG_BORDER_SPACING);
  vbox = GTK_DIALOG(dialog)->vbox;
  gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_ELEMENT_SPACING);


  label = gtk_label_new(_("Line Properties:"));
  gtk_misc_set_alignment(GTK_MISC(label),0,0);
  gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);

  table = gtk_table_new (4, 2, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_ELEMENT_SPACING);
  gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_ELEMENT_SPACING);
  gtk_box_pack_start(GTK_BOX(vbox), table, FALSE, FALSE, 0);

  label = gtk_label_new (_("Width:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,0,1, GTK_FILL,0,0,0);

  label = gtk_label_new (_("Type:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,1,2, GTK_FILL,0,0,0);

  label = gtk_label_new (_("Dash Length:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,2,3, GTK_FILL,0,0,0);

  label = gtk_label_new (_("Dash Space:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,3,4, GTK_FILL,0,0,0);

  width_entry = gtk_entry_new();
  gtk_entry_set_activates_default (GTK_ENTRY(width_entry), TRUE);
  gtk_editable_select_region(GTK_EDITABLE(width_entry), 0, -1);
  gtk_table_attach_defaults(GTK_TABLE(table), width_entry,
			    1,2,0,1);
  
  optionmenu = gtk_option_menu_new ();
  gtk_option_menu_set_menu(GTK_OPTION_MENU(optionmenu),
                           create_menu_linetype (w_current));
  gtk_table_attach_defaults(GTK_TABLE(table), optionmenu,
			    1,2,1,2);

  gtk_signal_connect(GTK_OBJECT (optionmenu), "changed",
                     (GtkSignalFunc) line_type_dialog_linetype_change,
                     line_type_data);
  
  length_entry = gtk_entry_new();
  gtk_entry_set_activates_default (GTK_ENTRY(length_entry), TRUE);
  gtk_editable_select_region(GTK_EDITABLE(length_entry), 0, -1);
  gtk_table_attach_defaults(GTK_TABLE(table), length_entry,
			    1,2,2,3);

  space_entry = gtk_entry_new();
  gtk_entry_set_activates_default (GTK_ENTRY(space_entry), TRUE);
  gtk_editable_select_region(GTK_EDITABLE(space_entry), 0, -1);
  gtk_table_attach_defaults(GTK_TABLE(table), space_entry,
			    1,2,3,4);

  /* populate the data structure */
  line_type_data->dialog = dialog;
  line_type_data->width_entry  = width_entry;
  line_type_data->line_type    = optionmenu;
  line_type_data->length_entry = length_entry;
  line_type_data->space_entry  = space_entry;
  
  line_type_data->toplevel = w_current;
  line_type_data->objects  = objects;

  /* fill in the fields of the dialog */
  if (g_list_next (objects) == NULL) {
    /* only one object in object list */
    OBJECT *o_current = (OBJECT*) objects->data;
      
    width_str  = g_strdup_printf ("%d", o_current->line_width);
    space_str  = g_strdup_printf ("%d", o_current->line_space);
    length_str = g_strdup_printf ("%d", o_current->line_length);
    type = o_current->line_type;
  } else {
    GtkWidget *menuitem;
    GtkWidget *menu;
      
    width_str   = g_strdup_printf (_("*unchanged*"));
    space_str   = g_strdup_printf (_("*unchanged*"));
    length_str  = g_strdup_printf (_("*unchanged*"));
    type = TYPE_PHANTOM + 1;
      
    /* add a new menuitem to option menu for line type */
    menu = gtk_option_menu_get_menu (GTK_OPTION_MENU (optionmenu));
    menuitem = gtk_radio_menu_item_new_with_label (
      gtk_radio_menu_item_get_group (
        GTK_RADIO_MENU_ITEM (gtk_menu_get_active (GTK_MENU (menu)))),
      _("*unchanged*"));
    gtk_menu_append (menu, menuitem);
    gtk_object_set_data (GTK_OBJECT (menuitem),
                         "linetype",
                         GINT_TO_POINTER (-1));
    gtk_widget_show (menuitem);
  }
  
  gtk_entry_set_text (GTK_ENTRY (width_entry), width_str);
  gtk_entry_select_region (GTK_ENTRY (width_entry), 0, strlen (width_str));
  gtk_option_menu_set_history (GTK_OPTION_MENU (optionmenu), type);
  gtk_entry_set_text (GTK_ENTRY (space_entry), space_str);
  gtk_entry_set_text (GTK_ENTRY (length_entry), length_str);

  /* calling it once will set the dash space/length activity */
  line_type_dialog_linetype_change(optionmenu, line_type_data);
  
  gtk_widget_grab_focus(width_entry);
  gtk_grab_add (dialog);
  
  g_free (width_str);
  g_free (space_str);
  g_free (length_str);
  
  gtk_widget_show_all (dialog);
}

/***************** End of Line Type / Width dialog box ****************/

/***************** Start of Fill Type dialog box **********************/

/*! \brief Create a menu with fill types for the line type dialog
 *  \par Function Description
 *  This function creates a GtkMenu with the different fill types.
 */
static GtkWidget *create_menu_filltype (TOPLEVEL *w_current)
{
  GtkWidget *menu;
  GSList *group;
  struct fill_type {
    gchar *str;
    OBJECT_FILLING type;
  } types[] = { { N_("Hollow"), FILLING_HOLLOW },
                { N_("Filled"), FILLING_FILL },
                { N_("Mesh"),   FILLING_MESH },
                { N_("Hatch"),  FILLING_HATCH } };
  gint i;

  menu  = gtk_menu_new ();
  group = NULL;

  for (i = 0; i < sizeof (types) / sizeof (struct fill_type); i++) {
    GtkWidget *menuitem;
      
    menuitem = gtk_radio_menu_item_new_with_label (group, _(types[i].str));
    group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
    gtk_menu_append (GTK_MENU (menu), menuitem);
    gtk_object_set_data (GTK_OBJECT(menuitem), "filltype",
                         GINT_TO_POINTER (types[i].type));
    gtk_widget_show (menuitem);
  }

  return menu;
}

/*! \brief Callback function for the filltype menu in the filltype dialog
 *  \par Function Description
 *  This function sets the entry activity according to the selected 
 *  filltype of the filltype dialog.
 */
static gint fill_type_dialog_filltype_change(GtkWidget *w, gpointer data)
{
  struct fill_type_data *fill_type_data = (struct fill_type_data*) data;
  GtkWidget *menuitem;
  gboolean activate_width_entry;
  gboolean activate_anglepitch1_entries;
  gboolean activate_anglepitch2_entries;
  gint type;

  menuitem = gtk_menu_get_active (
    GTK_MENU (gtk_option_menu_get_menu (
                GTK_OPTION_MENU (fill_type_data->fill_type))));

  type = GPOINTER_TO_INT(
    gtk_object_get_data (GTK_OBJECT (menuitem), "filltype"));
  switch(type) {
      case(FILLING_HOLLOW):
      case(FILLING_FILL):
        activate_width_entry = FALSE;
        activate_anglepitch1_entries = FALSE;
        activate_anglepitch2_entries = FALSE;
        break;
      case(FILLING_HATCH):
        activate_width_entry = TRUE;
        activate_anglepitch1_entries = TRUE;
        activate_anglepitch2_entries = FALSE;
        break;
      case(FILLING_MESH):
        activate_width_entry = TRUE;
        activate_anglepitch1_entries = TRUE;
        activate_anglepitch2_entries = TRUE;
        break;
      default:
        activate_width_entry = TRUE;
        activate_anglepitch1_entries = TRUE;
        activate_anglepitch2_entries = TRUE;
  }

  gtk_widget_set_sensitive (fill_type_data->width_entry,
                            activate_width_entry);
  gtk_widget_set_sensitive (fill_type_data->angle1_entry,
                            activate_anglepitch1_entries);
  gtk_widget_set_sensitive (fill_type_data->pitch1_entry,
                            activate_anglepitch1_entries);
  gtk_widget_set_sensitive (fill_type_data->angle2_entry,
                            activate_anglepitch2_entries);
  gtk_widget_set_sensitive (fill_type_data->pitch2_entry,
                            activate_anglepitch2_entries);
    
  return(0);
}

/*! \brief Apply the settings of the filltype dialog to the selection
 *  \par Function Description
 *  This function applies the settings of the filltype dialog to the 
 *  selected objects
 */
static void fill_type_dialog_ok(GtkWidget *w, gpointer data)
{
  struct fill_type_data *fill_type_data = (struct fill_type_data*)data;
  TOPLEVEL *toplevel;
  GList *objects;
  const gchar *width_str, *angle1_str, *pitch1_str, *angle2_str, *pitch2_str;
  OBJECT_FILLING type;
  
  /* retrieve the list of objects and the toplevel */
  objects  = fill_type_data->objects;
  toplevel = fill_type_data->toplevel;

  /* get the new values from the text entries of the dialog */
  width_str  = gtk_entry_get_text (GTK_ENTRY (
                                     fill_type_data->width_entry));
  angle1_str = gtk_entry_get_text (GTK_ENTRY (
                                     fill_type_data->angle1_entry));
  pitch1_str = gtk_entry_get_text (GTK_ENTRY (
                                     fill_type_data->pitch1_entry));
  angle2_str = gtk_entry_get_text (GTK_ENTRY (
                                     fill_type_data->angle2_entry));
  pitch2_str = gtk_entry_get_text (GTK_ENTRY (
                                     fill_type_data->pitch2_entry));
  type = GPOINTER_TO_INT(
    gtk_object_get_data (
      GTK_OBJECT (
        gtk_menu_get_active (
          GTK_MENU (gtk_option_menu_get_menu (
                      GTK_OPTION_MENU (
                        fill_type_data->fill_type))))), "filltype"));

  /* are there several objects concerned? */
  if (g_list_next (objects) == NULL) {
    /* no, there is only one object */
    OBJECT *o_current = (OBJECT*) objects->data;
    gint width, angle1, pitch1, angle2, pitch2;
        
    width  = atoi (width_str);
    angle1 = atoi (angle1_str);
    pitch1 = atoi (pitch1_str);
    angle2 = atoi (angle2_str);
    pitch2 = atoi (pitch2_str);

    /* apply the new line options to object */
    o_erase_single (toplevel, o_current);
    o_set_fill_options(toplevel, o_current,
                       type, width,
                       pitch1, angle1,
                       pitch2, angle2);
    o_object_recalc (toplevel, o_current);
    o_redraw_single (toplevel, o_current);
      
  } else {
    /* more than one object in the list */
    GList *object;
    gint width, angle1, pitch1, angle2, pitch2;

    /* get the new line options */
    width  = g_strcasecmp (width_str,
                           _("*unchanged*")) ? atoi (width_str)  : -1;
    angle1 = g_strcasecmp (angle1_str,
                           _("*unchanged*")) ? atoi (angle1_str) : -1;
    pitch1 = g_strcasecmp (pitch1_str,
                           _("*unchanged*")) ? atoi (pitch1_str) : -1;
    angle2 = g_strcasecmp (angle2_str,
                           _("*unchanged*")) ? atoi (angle2_str) : -1;
    pitch2 = g_strcasecmp (pitch2_str,
                           _("*unchanged*")) ? atoi (pitch2_str) : -1;

    /* apply changes to each object */
    object = objects;
    while (object != NULL) {
      OBJECT *o_current = (OBJECT*)object->data;

      o_erase_single (toplevel, o_current);
      o_set_fill_options (toplevel, o_current,
                          type   == -1 ? o_current->fill_type   : type,
                          width  == -1 ? o_current->fill_width  : width,
                          pitch1 == -1 ? o_current->fill_pitch1 : pitch1,
                          angle1 == -1 ? o_current->fill_angle1 : angle1,
                          pitch2 == -1 ? o_current->fill_pitch2 : pitch2,
                          angle2 == -1 ? o_current->fill_angle2 : angle2);
      o_object_recalc (toplevel, o_current);
      o_redraw_single (toplevel, o_current);
          
      object = object->next;
    }
  }
  toplevel->page_current->CHANGED = 1;
}

/*! \brief response function for the filltype dialog
 *  \par Function Description
 *  This function handles the user response to the filltype dialog.
 *  It destroys the dialog after that.
 */
void fill_type_dialog_response(GtkWidget *widget, gint response,
			       struct fill_type_data *fill_type_data)
{
  switch (response) {
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    /* void */
    break;
  case GTK_RESPONSE_ACCEPT:
    fill_type_dialog_ok(widget, fill_type_data);
    break;
  default:
    printf("line_type_dialog_response(): strange signal %d\n",response);
  }
  
  i_set_state (fill_type_data->toplevel, SELECT);
  i_update_toolbar (fill_type_data->toplevel);
  
  gtk_grab_remove (fill_type_data->dialog);
  gtk_widget_destroy (fill_type_data->dialog);

  /* get ride of the list of objects but not the objects */
  g_list_free (fill_type_data->objects);
  g_free (fill_type_data);
} 

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void fill_type_dialog(TOPLEVEL *w_current, GList *objects)
{
  GtkWidget *dialog;
  GtkWidget *vbox;
  GtkWidget *optionmenu = NULL;
  GtkWidget *width_entry = NULL;
  GtkWidget *angle1_entry = NULL;
  GtkWidget *pitch1_entry = NULL;
  GtkWidget *angle2_entry = NULL;
  GtkWidget *pitch2_entry = NULL;
  GtkWidget *label;
  GtkWidget *table;
  struct fill_type_data *fill_type_data;
  gchar *width_str, *angle1_str, *pitch1_str, *angle2_str, *pitch2_str;
  gint type;

  fill_type_data = (struct fill_type_data*) g_malloc (
    sizeof (struct fill_type_data));

  dialog = gtk_dialog_new_with_buttons(_("Edit Fill Type"),
				       GTK_WINDOW(w_current->main_window),
				       GTK_DIALOG_MODAL,
				       GTK_STOCK_CANCEL,
				       GTK_RESPONSE_REJECT,
				       GTK_STOCK_OK,
				       GTK_RESPONSE_ACCEPT,
				       NULL);

  gtk_window_position(GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  
  gtk_dialog_set_default_response(GTK_DIALOG(dialog),
				  GTK_RESPONSE_ACCEPT);

  gtk_signal_connect(GTK_OBJECT(dialog), "response",
		     GTK_SIGNAL_FUNC(fill_type_dialog_response), fill_type_data);

  gtk_container_border_width(GTK_CONTAINER(dialog), DIALOG_BORDER_SPACING);
  vbox = GTK_DIALOG(dialog)->vbox;
  gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_ELEMENT_SPACING);
  

  label = gtk_label_new(_("Fill Properties:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_box_pack_start(GTK_BOX(vbox),label, FALSE, FALSE, 0);

  table = gtk_table_new (6, 2, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_ELEMENT_SPACING);
  gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_ELEMENT_SPACING);
  gtk_box_pack_start(GTK_BOX(vbox), table, FALSE, FALSE, 0);

  label = gtk_label_new (_("Fill Type:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,0,1, GTK_FILL,0,0,0);

  label = gtk_label_new (_("Line Width:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,1,2, GTK_FILL,0,0,0);

  label = gtk_label_new (_("Angle 1:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,2,3, GTK_FILL,0,0,0);

  label = gtk_label_new (_("Pitch 1:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,3,4, GTK_FILL,0,0,0);

  label = gtk_label_new (_("Angle 2:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,4,5, GTK_FILL,0,0,0);

  label = gtk_label_new (_("Pitch 2:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,5,6, GTK_FILL,0,0,0);


  optionmenu = gtk_option_menu_new ();
  gtk_option_menu_set_menu(GTK_OPTION_MENU(optionmenu),
                           create_menu_filltype (w_current));
  gtk_table_attach_defaults(GTK_TABLE(table), optionmenu,
			    1,2,0,1);

  gtk_signal_connect(GTK_OBJECT (optionmenu), "changed",
                     (GtkSignalFunc) fill_type_dialog_filltype_change,
                     fill_type_data);

  width_entry = gtk_entry_new();
  gtk_entry_set_activates_default (GTK_ENTRY(width_entry), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), width_entry,
			    1,2,1,2);

  angle1_entry = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(angle1_entry), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), angle1_entry,
			    1,2,2,3);

  pitch1_entry = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(pitch1_entry), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), pitch1_entry,
			    1,2,3,4);

  angle2_entry = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(angle2_entry), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), angle2_entry,
			    1,2,4,5);

  pitch2_entry = gtk_entry_new ();
  gtk_entry_set_activates_default (GTK_ENTRY(pitch2_entry), TRUE);
  gtk_table_attach_defaults(GTK_TABLE(table), pitch2_entry,
			    1,2,5,6);

  /* populate the data structure */
  fill_type_data->dialog = dialog;
  fill_type_data->fill_type    = optionmenu;
  fill_type_data->width_entry  = width_entry;
  fill_type_data->angle1_entry = angle1_entry;
  fill_type_data->pitch1_entry = pitch1_entry;
  fill_type_data->angle2_entry = angle2_entry;
  fill_type_data->pitch2_entry = pitch2_entry;
  
  fill_type_data->toplevel = w_current;
  fill_type_data->objects  = objects;

  /* fill in the fields of the dialog */
  if (g_list_next (objects) == NULL) {
    /* only one object in object list */
    OBJECT *o_current = (OBJECT*) objects->data;
      
    type = o_current->fill_type;
    width_str  = g_strdup_printf ("%d", o_current->fill_width);
    angle1_str = g_strdup_printf ("%d", o_current->fill_angle1);
    pitch1_str = g_strdup_printf ("%d", o_current->fill_pitch1);
    angle2_str = g_strdup_printf ("%d", o_current->fill_angle2);
    pitch2_str = g_strdup_printf ("%d", o_current->fill_pitch2);
  } else {
    GtkWidget *menuitem;
    GtkWidget *menu;
      
    width_str  = g_strdup_printf (_("*unchanged*"));
    angle1_str = g_strdup_printf (_("*unchanged*"));
    pitch1_str = g_strdup_printf (_("*unchanged*"));
    angle2_str = g_strdup_printf (_("*unchanged*"));
    pitch2_str = g_strdup_printf (_("*unchanged*"));
    type = FILLING_HATCH + 1;
      
    /* add a new menuitem to option menu for line type */
    menu = gtk_option_menu_get_menu (GTK_OPTION_MENU (optionmenu));
    menuitem = gtk_radio_menu_item_new_with_label (
      gtk_radio_menu_item_get_group (
        GTK_RADIO_MENU_ITEM (gtk_menu_get_active (GTK_MENU (menu)))),
      _("*unchanged*"));
    gtk_menu_append (menu, menuitem);
    gtk_object_set_data (GTK_OBJECT (menuitem),
                         "filltype",
                         GINT_TO_POINTER (-1));
    gtk_widget_show (menuitem);
  }

  gtk_option_menu_set_history (GTK_OPTION_MENU (optionmenu), type);
  gtk_entry_set_text (GTK_ENTRY (width_entry), width_str);
  gtk_entry_select_region (GTK_ENTRY (width_entry), 0, strlen (width_str));
  gtk_entry_set_text (GTK_ENTRY (angle1_entry), angle1_str);
  gtk_entry_select_region (GTK_ENTRY (angle1_entry), 0, strlen (angle1_str));
  gtk_entry_set_text (GTK_ENTRY (pitch1_entry), pitch1_str);
  gtk_entry_select_region (GTK_ENTRY (pitch1_entry), 0, strlen (pitch1_str));
  gtk_entry_set_text (GTK_ENTRY (angle2_entry), angle2_str);
  gtk_entry_select_region (GTK_ENTRY (angle2_entry), 0, strlen (angle2_str));
  gtk_entry_set_text (GTK_ENTRY (pitch2_entry), pitch2_str);
  gtk_entry_select_region (GTK_ENTRY (pitch2_entry), 0, strlen (pitch2_str));
  
  /* Set the widget activity according to the current filltype */
  fill_type_dialog_filltype_change(optionmenu, fill_type_data);

  gtk_widget_grab_focus(width_entry);
  gtk_grab_add (dialog);
  
  g_free (width_str);
  g_free (angle1_str);
  g_free (pitch1_str);
  g_free (angle2_str);
  g_free (pitch2_str);
  
  gtk_widget_show_all (dialog);
  
}

/***************** End of Fill Type dialog box ***********************/

/***************** Start of Arc dialog box ***************************/

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int arc_angles_dialog_keypress(GtkWidget * widget, GdkEventKey * event, 
	              TOPLEVEL * w_current)
{
   if (strcmp(gdk_keyval_name(event->keyval), "Escape") == 0) {
	arc_angles_dialog_cancel(NULL, w_current);	
        return TRUE;
   }

   return FALSE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void arc_angles_dialog_ok(GtkWidget *w, TOPLEVEL *w_current)
{
  char *string_start = NULL;
  char *string_sweep = NULL;

  string_start = (char *) gtk_entry_get_text(GTK_ENTRY(w_current->aaentry_start));
  string_sweep = (char *) gtk_entry_get_text(GTK_ENTRY(w_current->aaentry_sweep));

  if ( (string_start[0] != '\0') && (string_sweep[0] != '\0') ) {
    /*! \todo put error detection */
    /* pb20011125 - o_arc_end4 accepts the final angles as param */
    o_arc_end4(w_current, atoi(string_start), atoi(string_sweep));
  }

  gtk_grab_remove(w_current->aawindow);
  gtk_widget_destroy(w_current->aawindow);
  w_current->aawindow = NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void arc_angles_dialog_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
  gtk_grab_remove(w_current->aawindow);
  gtk_widget_destroy(w_current->aawindow);
  w_current->aawindow = NULL;

  w_current->event_state = DRAWARC;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void arc_angle_dialog (TOPLEVEL *w_current)
{
  GtkWidget *label = NULL;
  GtkWidget *buttonok     = NULL;
  GtkWidget *buttoncancel = NULL;
  GtkWidget *vbox, *action_area;

  if (!w_current->aawindow) {
    w_current->aawindow = x_create_dialog_box(&vbox, &action_area);

    gtk_window_position(GTK_WINDOW(w_current->aawindow),
                        GTK_WIN_POS_MOUSE);

    gtk_signal_connect(GTK_OBJECT(w_current->aawindow),
                       "destroy",
                       GTK_SIGNAL_FUNC(destroy_window),
                       &w_current->aawindow);

    gtk_signal_connect(GTK_OBJECT(w_current->aawindow),
                     "key_press_event",
                     (GtkSignalFunc) arc_angles_dialog_keypress, w_current);

#if 0 /* removed because it was causing the dialog box to not close */
    gtk_signal_connect(GTK_OBJECT(w_current->aawindow),
                       "delete_event",
                       GTK_SIGNAL_FUNC(destroy_window),
                       &w_current->aawindow);
#endif

    gtk_window_set_title(GTK_WINDOW(w_current->aawindow),
                         _("Arc Params"));
    gtk_container_border_width(GTK_CONTAINER(w_current->aawindow),
                               10);

    label = gtk_label_new (_("Start Angle"));
    gtk_box_pack_start(
                       GTK_BOX(vbox),
                       label, TRUE, TRUE, 0);
    gtk_widget_show (label);

    w_current->aaentry_start = gtk_entry_new_with_max_length (4);
    gtk_editable_select_region(
                               GTK_EDITABLE(w_current->aaentry_start), 0, -1);
    gtk_box_pack_start(
                       GTK_BOX(vbox),
                       w_current->aaentry_start, FALSE, FALSE, 5);
    gtk_widget_show(w_current->aaentry_start);
    gtk_widget_grab_focus(w_current->aaentry_start);

    label = gtk_label_new(_("Degrees of Sweep"));
    gtk_box_pack_start(
                       GTK_BOX(vbox),
                       label, TRUE, TRUE, 0);
    gtk_widget_show(label);

    w_current->aaentry_sweep = gtk_entry_new_with_max_length (4);
    gtk_editable_select_region(
                               GTK_EDITABLE(w_current->aaentry_sweep), 0, -1);
    gtk_box_pack_start(GTK_BOX(vbox),
                       w_current->aaentry_sweep, FALSE, FALSE, 5);
    gtk_signal_connect(GTK_OBJECT(w_current->aaentry_sweep),
                       "activate",
                       GTK_SIGNAL_FUNC(arc_angles_dialog_ok),
                       w_current);
    gtk_widget_show(w_current->aaentry_sweep);

    buttoncancel = gtk_button_new_from_stock (GTK_STOCK_CANCEL);
    GTK_WIDGET_SET_FLAGS (buttoncancel, GTK_CAN_DEFAULT);
    gtk_box_pack_start (GTK_BOX (action_area),
                        buttoncancel, TRUE, TRUE, 0);
    gtk_signal_connect (GTK_OBJECT (buttoncancel), "clicked",
                        GTK_SIGNAL_FUNC(arc_angles_dialog_cancel),
                        w_current);
    gtk_widget_show (buttoncancel);

    buttonok = gtk_button_new_from_stock (GTK_STOCK_OK);
    GTK_WIDGET_SET_FLAGS (buttonok, GTK_CAN_DEFAULT);
    gtk_box_pack_start (
			GTK_BOX(action_area),
			buttonok, TRUE, TRUE, 0);
    gtk_signal_connect(GTK_OBJECT (buttonok), "clicked",
                       GTK_SIGNAL_FUNC(arc_angles_dialog_ok),
                       w_current);
    gtk_widget_show (buttonok);
    gtk_widget_grab_default (buttonok);

  }

  if (!GTK_WIDGET_VISIBLE (w_current->aawindow)) {
    gtk_widget_show (w_current->aawindow);
    gtk_grab_add (w_current->aawindow);
  }
}

/***************** End of Arc dialog box *****************************/

/***************** Start of Translate dialog box *********************/

/*! \brief response function for the translate dialog
 *  \par Function Description
 *  This function takes the user action and applies it.
 *  \todo improve put error detection
 */
void translate_dialog_response(GtkWidget *widget, gint response,
			       TOPLEVEL *w_current)
{
  GtkWidget *textentry;
  gchar *string;
 
  switch (response) {
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    /* void */
    break;
  case GTK_RESPONSE_ACCEPT:
    textentry = g_object_get_data(G_OBJECT(w_current->trwindow),"textentry");
    string = (gchar*) gtk_entry_get_text(GTK_ENTRY(textentry));
    if (strlen(string) != 0) {
      o_complex_translate_all(w_current, atoi(string));
    }
    break;
  default:
    printf("slot_edit_dialog_response(): strange signal %d\n",response);
  }

  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);
  gtk_widget_destroy(w_current->trwindow);
  w_current->trwindow=NULL;
}


/*! \brief Create the translate dialog
 *  \par Function Description
 *  Create the dialog to translate symbols.
 */
void translate_dialog (TOPLEVEL *w_current)
{
  GtkWidget *label;
  GtkWidget *textentry;
  GtkWidget *vbox;

  if (!w_current->trwindow) {
    w_current->trwindow = gtk_dialog_new_with_buttons(_("Translate"),
						      GTK_WINDOW(w_current->main_window),
						      GTK_DIALOG_MODAL,
						      GTK_STOCK_CANCEL,
						      GTK_RESPONSE_REJECT,
						      GTK_STOCK_OK,
						      GTK_RESPONSE_ACCEPT,
						      NULL);
    gtk_window_position(GTK_WINDOW (w_current->trwindow),
                        GTK_WIN_POS_MOUSE);

    gtk_signal_connect(GTK_OBJECT (w_current->trwindow), "response",
                       GTK_SIGNAL_FUNC(translate_dialog_response), w_current);

    gtk_dialog_set_default_response(GTK_DIALOG(w_current->trwindow),
				    GTK_RESPONSE_ACCEPT);

    gtk_container_border_width(GTK_CONTAINER(w_current->trwindow), DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(w_current->trwindow)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_ELEMENT_SPACING);

    label = gtk_label_new(_("Offset to translate?\n(0 for origin)"));
    gtk_misc_set_padding(GTK_MISC (label), 10, 10);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);
    
    textentry = gtk_entry_new_with_max_length (10);
    gtk_entry_set_text(GTK_ENTRY(textentry), "0");
    gtk_editable_select_region(GTK_EDITABLE(textentry), 0, -1);
    gtk_entry_set_activates_default(GTK_ENTRY(textentry), TRUE);
    gtk_box_pack_start(GTK_BOX(vbox),textentry, FALSE, FALSE, 0);

    GLADE_HOOKUP_OBJECT(w_current->trwindow, textentry, "textentry");
  }

  if (!GTK_WIDGET_VISIBLE (w_current->trwindow)) {
    gtk_widget_show_all (w_current->trwindow);
  }
}

/***************** End of Translate dialog box ***********************/

/***************** Start of Text size dialog box *********************/

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int text_size_dialog_keypress(GtkWidget * widget, GdkEventKey * event, 
			      TOPLEVEL * w_current)
{
   if (strcmp(gdk_keyval_name(event->keyval), "Escape") == 0) {
	text_size_dialog_cancel(NULL, w_current);	
        return TRUE;
   }

   return FALSE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void text_size_dialog_ok(GtkWidget *w, TOPLEVEL *w_current)
{
  char *string = NULL;
  int size;

  string = (char *) gtk_entry_get_text(GTK_ENTRY(w_current->tsentry));

  if ((string[0] != '\0')) {
    size = atoi(string);
    if (size) {
      w_current->text_size = size;
      w_current->page_current->CHANGED=1;
      o_undo_savestate(w_current, UNDO_ALL);
    }
  }

  gtk_grab_remove(w_current->tswindow);
  gtk_widget_destroy(w_current->tswindow);
  w_current->tswindow = NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void text_size_dialog_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);
  gtk_grab_remove(w_current->tswindow);
  gtk_widget_destroy(w_current->tswindow);
  w_current->tswindow = NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void text_size_dialog (TOPLEVEL *w_current)
{
  char *string;
  int len;
  GtkWidget *label = NULL;
  GtkWidget *buttonok = NULL;
  GtkWidget *buttoncancel = NULL;
  GtkWidget *vbox, *action_area;

  if (!w_current->tswindow) {
    w_current->tswindow = x_create_dialog_box(&vbox, &action_area);

    gtk_window_position(GTK_WINDOW(w_current->tswindow),
                        GTK_WIN_POS_MOUSE);

    gtk_signal_connect(GTK_OBJECT(w_current->tswindow),
                       "destroy",
                       GTK_SIGNAL_FUNC(destroy_window),
                       &w_current->tswindow);

    gtk_signal_connect(GTK_OBJECT(w_current->tswindow),
                     "key_press_event",
                     (GtkSignalFunc) text_size_dialog_keypress, w_current);

#if 0 /* removed because it was causing the dialog box to not close */
    gtk_signal_connect(GTK_OBJECT (w_current->tswindow),
                       "delete_event",
                       GTK_SIGNAL_FUNC(destroy_window),
                       &w_current->tswindow);
#endif

    gtk_window_set_title(GTK_WINDOW (w_current->tswindow),
                         _("Text Size"));
    gtk_container_border_width(GTK_CONTAINER(w_current->tswindow),
                               10);

    label = gtk_label_new (_("Enter new text size"));
    gtk_misc_set_padding (GTK_MISC (label), 10, 10);
    gtk_box_pack_start(
                       GTK_BOX(vbox),
                       label, TRUE, TRUE, 0);
    gtk_widget_show (label);

    w_current->tsentry = gtk_entry_new_with_max_length (10);
    gtk_editable_select_region(
                               GTK_EDITABLE(w_current->tsentry), 0, -1);
    gtk_box_pack_start(GTK_BOX(vbox),
                       w_current->tsentry, FALSE, FALSE, 5);
    gtk_signal_connect(GTK_OBJECT(w_current->tsentry), "activate",
                       GTK_SIGNAL_FUNC(text_size_dialog_ok),
                       w_current);
    gtk_widget_show (w_current->tsentry);
    gtk_widget_grab_focus(w_current->tsentry);

    buttoncancel = gtk_button_new_from_stock (GTK_STOCK_CANCEL);
    GTK_WIDGET_SET_FLAGS (buttoncancel, GTK_CAN_DEFAULT);
    gtk_box_pack_start(
                       GTK_BOX(action_area),
                       buttoncancel, TRUE, TRUE, 0);
    gtk_signal_connect(GTK_OBJECT (buttoncancel), "clicked",
                       GTK_SIGNAL_FUNC(text_size_dialog_cancel),
                       w_current);
    gtk_widget_show (buttoncancel);

    buttonok = gtk_button_new_from_stock (GTK_STOCK_OK);
    GTK_WIDGET_SET_FLAGS (buttonok, GTK_CAN_DEFAULT);
    gtk_box_pack_start(
                       GTK_BOX(action_area),
                       buttonok, TRUE, TRUE, 0);
    gtk_signal_connect(GTK_OBJECT (buttonok), "clicked",
                       GTK_SIGNAL_FUNC(text_size_dialog_ok),
                       w_current);
    gtk_widget_show (buttonok);
    gtk_widget_grab_default (buttonok);
  }

  if (!GTK_WIDGET_VISIBLE (w_current->tswindow)) {
    string = g_strdup_printf("%d", w_current->text_size);
    len = strlen(string);
    gtk_entry_set_text(GTK_ENTRY(w_current->tsentry), string);
    gtk_entry_select_region(GTK_ENTRY(w_current->tsentry), 0, len);
    gtk_widget_show (w_current->tswindow);
    gtk_grab_add(w_current->tswindow);
    g_free(string);
  }
}

/***************** End of Text size dialog box ***********************/

/***************** Start of Snap size dialog box *********************/

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int snap_size_dialog_keypress(GtkWidget * widget, GdkEventKey * event, 
			      TOPLEVEL * w_current)
{
   if (strcmp(gdk_keyval_name(event->keyval), "Escape") == 0) {
	snap_size_dialog_cancel(NULL, w_current);	
        return TRUE;
   }

   return FALSE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void snap_size_dialog_ok(GtkWidget *w, TOPLEVEL *w_current)
{
  char *string = NULL;
  int size;

  string = (char *) gtk_entry_get_text(GTK_ENTRY(w_current->tsentry));

  if ((string[0] != '\0')) {
    size = atoi(string);
    if (size) {
      w_current->snap_size = size;
    }
  }

  gtk_grab_remove(w_current->tswindow);
  gtk_widget_destroy(w_current->tswindow);
  w_current->tswindow = NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void snap_size_dialog_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);
  gtk_grab_remove(w_current->tswindow);
  gtk_widget_destroy(w_current->tswindow);
  w_current->tswindow = NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void snap_size_dialog (TOPLEVEL *w_current)
{
  char *string;
  int len;
  GtkWidget *label = NULL;
  GtkWidget *buttonok     = NULL;
  GtkWidget *buttoncancel = NULL;
  GtkWidget *vbox, *action_area;

  if (!w_current->tswindow) {
    w_current->tswindow = x_create_dialog_box(&vbox, &action_area);

    gtk_window_position(GTK_WINDOW (w_current->tswindow),
                        GTK_WIN_POS_MOUSE);

    gtk_signal_connect(GTK_OBJECT(w_current->tswindow), "destroy",
                       GTK_SIGNAL_FUNC(destroy_window),
                       &w_current->tswindow);

    gtk_signal_connect(GTK_OBJECT(w_current->tswindow),
                     "key_press_event",
                     (GtkSignalFunc) snap_size_dialog_keypress, w_current);

#if 0 /* removed because it was causing the dialog box to not close */
    gtk_signal_connect(GTK_OBJECT(w_current->tswindow),
                       "delete_event",
                       GTK_SIGNAL_FUNC(destroy_window),
                       &w_current->tswindow);
#endif

    gtk_window_set_title(GTK_WINDOW (w_current->tswindow),
                         _("Snap Grid"));
    gtk_container_border_width(GTK_CONTAINER(w_current->tswindow),
                               10);

    label = gtk_label_new(_("Enter new snap grid spacing"));
    gtk_misc_set_padding(GTK_MISC (label), 10, 10);
    gtk_box_pack_start(
                       GTK_BOX(vbox),
                       label, TRUE, TRUE, 0);
    gtk_widget_show(label);

    w_current->tsentry = gtk_entry_new_with_max_length (10);
    gtk_editable_select_region(GTK_EDITABLE(w_current->tsentry),
                               0, -1);
    gtk_box_pack_start(GTK_BOX(vbox),
                       w_current->tsentry, FALSE, FALSE, 5);
    gtk_signal_connect(GTK_OBJECT(w_current->tsentry), "activate",
                       GTK_SIGNAL_FUNC(snap_size_dialog_ok),
                       w_current);
    gtk_widget_show(w_current->tsentry);
    gtk_widget_grab_focus(w_current->tsentry);

    buttoncancel = gtk_button_new_from_stock (GTK_STOCK_CANCEL);
    GTK_WIDGET_SET_FLAGS (buttoncancel, GTK_CAN_DEFAULT);
    gtk_box_pack_start(
                       GTK_BOX(action_area),
                       buttoncancel, TRUE, TRUE, 0);
    gtk_signal_connect(GTK_OBJECT (buttoncancel), "clicked",
                       GTK_SIGNAL_FUNC(snap_size_dialog_cancel),
                       w_current);
    gtk_widget_show(buttoncancel);

    buttonok = gtk_button_new_from_stock (GTK_STOCK_OK);
    GTK_WIDGET_SET_FLAGS (buttonok, GTK_CAN_DEFAULT);
    gtk_box_pack_start(
                       GTK_BOX(action_area),
                       buttonok, TRUE, TRUE, 0);
    gtk_signal_connect(GTK_OBJECT (buttonok), "clicked",
                       GTK_SIGNAL_FUNC(snap_size_dialog_ok),
                       w_current);
    gtk_widget_show (buttonok);
    gtk_widget_grab_default (buttonok);
  }

  if (!GTK_WIDGET_VISIBLE (w_current->tswindow)) {
    string = g_strdup_printf("%d", w_current->snap_size);
    len = strlen(string);
    gtk_entry_set_text(GTK_ENTRY(w_current->tsentry), string);
    gtk_entry_select_region(GTK_ENTRY(w_current->tsentry), 0, len);
    gtk_widget_show (w_current->tswindow);
    gtk_grab_add (w_current->tswindow);
    g_free(string);
  }
}

/***************** End of Snap size dialog box ***********************/

/***************** Start of slot edit dialog box *********************/

/*! \todo response function for the slot edit dialog
 *  \brief
 *  \par Function Description
 *  The function takes the dialog entry and applies the new slot to the 
 *  symbol.
 */
void slot_edit_dialog_response(GtkWidget *widget, gint response, TOPLEVEL *w_current)
{
  GtkWidget *textentry;
  int len;
  gchar *string = NULL;

  switch (response) {
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    /* void */
    break;
  case GTK_RESPONSE_ACCEPT:
    textentry = g_object_get_data(G_OBJECT(w_current->sewindow),"textentry");
    string = (gchar*) gtk_entry_get_text(GTK_ENTRY(textentry));
    len = strlen(string);
    if (len != 0) {
      o_slot_end(w_current, string, len);
    }
    break;
  default:
    printf("slot_edit_dialog_response(): strange signal %d\n",response);
  }
  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);
  gtk_widget_destroy(w_current->sewindow);
  w_current->sewindow = NULL;
}
    

/*! \brief Create the slot entry dialog
 *  \par Function Description
 *  This function creates the slot edit dialog.
 */
void slot_edit_dialog (TOPLEVEL *w_current, char *string)
{
  GtkWidget *label = NULL;
  GtkWidget *textentry;
  GtkWidget *vbox;

  if (!w_current->sewindow) {
    w_current->sewindow = gtk_dialog_new_with_buttons(_("Edit slot number"),
						      GTK_WINDOW(w_current->main_window),
						      GTK_DIALOG_MODAL,
						      GTK_STOCK_CANCEL,
						      GTK_RESPONSE_REJECT,
						      GTK_STOCK_OK,
						      GTK_RESPONSE_ACCEPT,
						      NULL);

    gtk_window_position(GTK_WINDOW(w_current->sewindow),
                        GTK_WIN_POS_MOUSE);

    gtk_dialog_set_default_response (GTK_DIALOG (w_current->sewindow), 
				     GTK_RESPONSE_ACCEPT);

    gtk_signal_connect(GTK_OBJECT(w_current->sewindow), "response",
		       GTK_SIGNAL_FUNC(slot_edit_dialog_response),
		       w_current);

    gtk_container_border_width(GTK_CONTAINER(w_current->sewindow), 
			       DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(w_current->sewindow)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_ELEMENT_SPACING);

    label = gtk_label_new (_("Edit slot number:"));
    gtk_misc_set_alignment(GTK_MISC(label),0,0);
    gtk_box_pack_start(GTK_BOX (vbox), label, FALSE, FALSE, 0);

    textentry = gtk_entry_new();
    gtk_box_pack_start( GTK_BOX(vbox),
                       textentry, FALSE, FALSE, 0);
    gtk_entry_set_max_length(GTK_ENTRY(textentry), 80);
    gtk_entry_set_activates_default (GTK_ENTRY(textentry),TRUE);

    GLADE_HOOKUP_OBJECT(w_current->sewindow, textentry, "textentry");
    gtk_widget_show_all (w_current->sewindow);
  }

  if (string != NULL) {
    textentry = g_object_get_data(G_OBJECT(w_current->sewindow),"textentry");
    gtk_entry_set_text(GTK_ENTRY(textentry), string);
    gtk_entry_select_region(GTK_ENTRY(textentry),
			    strlen("slot="), strlen(string));
  }

  if (!GTK_WIDGET_VISIBLE (w_current->sewindow)) {
    gtk_widget_show_all (w_current->sewindow);
    gtk_window_activate_focus (GTK_WINDOW(w_current->sewindow));
  }
}

/***************** End of Slot Edit dialog box ***********************/

/***************** Start of help/about dialog box ********************/

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
int about_dialog_keypress(GtkWidget * widget, GdkEventKey * event, 
			  TOPLEVEL * w_current)
{
   if (strcmp(gdk_keyval_name(event->keyval), "Escape") == 0) {
	about_dialog_close(NULL, w_current);	
        return TRUE;
   }

   return FALSE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void about_dialog_close(GtkWidget *w, TOPLEVEL *w_current)
{
  gtk_widget_destroy(w_current->abwindow);
  w_current->abwindow = NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void about_dialog (TOPLEVEL *w_current)
{
  GtkWidget *label = NULL;
  GtkWidget *buttonclose = NULL;
  GtkWidget *vbox, *action_area;
  char *string;

  if (!w_current->abwindow) {
    w_current->abwindow = x_create_dialog_box(&vbox, &action_area);

    gtk_window_position (GTK_WINDOW (w_current->abwindow),
                         GTK_WIN_POS_MOUSE);

    gtk_window_set_title (GTK_WINDOW (w_current->abwindow),
                          _("About..."));
    gtk_container_border_width (GTK_CONTAINER (
                                               w_current->abwindow), 5);

    gtk_signal_connect (GTK_OBJECT (w_current->abwindow),
                        "destroy", GTK_SIGNAL_FUNC(destroy_window),
                        &w_current->abwindow);

    gtk_signal_connect(GTK_OBJECT(w_current->abwindow),
                     "key_press_event",
                     (GtkSignalFunc) about_dialog_keypress, w_current);

#if 0 /* removed because it was causing the dialog box to not close */
    gtk_signal_connect (GTK_OBJECT (w_current->abwindow),
                        "delete_event",
                        GTK_SIGNAL_FUNC(destroy_window),
                        &w_current->abwindow);
#endif

    string = g_strdup_printf( _("gEDA : GPL Electronic Design Automation"));
    label = gtk_label_new (string);
    g_free(string);
    gtk_box_pack_start(
                       GTK_BOX(vbox),
                       label, TRUE, TRUE, 5);
    gtk_widget_show (label);

    string = g_strdup_printf(_("gschem version %s%s"), VERSION, CUSTOM_VERSION);
    label = gtk_label_new (string);
    g_free(string);
    gtk_box_pack_start(
                       GTK_BOX(vbox),
                       label, TRUE, TRUE, 5);
    gtk_widget_show (label);

    string = g_strdup_printf( _("Written by:\nAles V. Hvezda\nahvezda@geda.seul.org\nAnd many others (See AUTHORS file)"));
    label = gtk_label_new (string);
    g_free(string);
    gtk_box_pack_start(
                       GTK_BOX(vbox),
                       label, TRUE, TRUE, 5);
    gtk_widget_show (label);

    buttonclose = gtk_button_new_from_stock (GTK_STOCK_CLOSE);
    GTK_WIDGET_SET_FLAGS (buttonclose, GTK_CAN_DEFAULT);
    gtk_box_pack_start(
                       GTK_BOX(action_area),
                       buttonclose, TRUE, TRUE, 0);
    gtk_signal_connect(GTK_OBJECT (buttonclose), "clicked",
                       GTK_SIGNAL_FUNC(about_dialog_close),
                       w_current);
    gtk_widget_show(buttonclose);

  }

  if (!GTK_WIDGET_VISIBLE(w_current->abwindow)) {
    gtk_widget_show(w_current->abwindow);
  }
}

/***************** End of help/about dialog box *********************/

/***************** Start of coord dialog box ************************/

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int coord_dialog_keypress(GtkWidget * widget, GdkEventKey * event, 
			  TOPLEVEL * w_current)
{
   if (strcmp(gdk_keyval_name(event->keyval), "Escape") == 0) {
	coord_dialog_close(NULL, w_current);	
        return TRUE;
   }

   return FALSE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void coord_dialog_close(GtkWidget *w, TOPLEVEL *w_current)
{
  gtk_widget_destroy(w_current->cowindow);
  w_current->cowindow = NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void coord_display_update(TOPLEVEL *w_current, int x, int y)
{
  char *string;
  int world_x, world_y;

  string = g_strdup_printf("(%d, %d)", x, y);
  gtk_label_set_text(GTK_LABEL(w_current->coord_screen), string );
  g_free(string);

  SCREENtoWORLD(w_current, x, y, &world_x, &world_y);

  string = g_strdup_printf("(%d, %d)", world_x, world_y);
  gtk_label_set_text(GTK_LABEL(w_current->coord_world), string );
  g_free(string);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void coord_dialog (TOPLEVEL *w_current, int x, int y)
{
  GtkWidget *buttonclose = NULL;
  GtkWidget *frame;
  GtkWidget *vbox2;

  if (!w_current->cowindow) {
    w_current->cowindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_window_position (GTK_WINDOW (w_current->cowindow),
                         GTK_WIN_POS_MOUSE);

    gtk_window_set_title (GTK_WINDOW (w_current->cowindow),
                          _("Coords"));
    gtk_container_border_width (GTK_CONTAINER (
                                               w_current->cowindow), 5);

    gtk_signal_connect (GTK_OBJECT (w_current->cowindow),
                        "destroy", GTK_SIGNAL_FUNC(destroy_window),
                        &w_current->cowindow);

#if 0 /* removed because it was causing the dialog box to not close */
    gtk_signal_connect (GTK_OBJECT (w_current->cowindow),
                        "delete_event",
                        GTK_SIGNAL_FUNC(destroy_window),
                        &w_current->cowindow);
#endif

    gtk_signal_connect(GTK_OBJECT(w_current->cowindow),
                     "key_press_event",
                     (GtkSignalFunc) coord_dialog_keypress, w_current);


    vbox2 = gtk_vbox_new (FALSE, 5);
    gtk_container_add (GTK_CONTAINER (w_current->cowindow), vbox2);
    gtk_widget_show(vbox2);

    frame = gtk_frame_new (_("Screen"));
    w_current->coord_screen =
      gtk_label_new("(########, ########)");
    gtk_label_set_justify(
                          GTK_LABEL(w_current->coord_screen), GTK_JUSTIFY_LEFT);
    gtk_misc_set_padding(GTK_MISC(w_current->coord_screen),
                         10, 10);
    gtk_container_add(GTK_CONTAINER (frame),
                      w_current->coord_screen);
    gtk_box_pack_start(GTK_BOX (vbox2), frame, FALSE, FALSE, 0);
    gtk_widget_show(w_current->coord_screen);
    gtk_widget_show(frame);

    frame = gtk_frame_new (_("World"));
    w_current->coord_world =
      gtk_label_new ("(########, ########)");
    gtk_misc_set_padding(GTK_MISC(w_current->coord_world), 10, 10);
    gtk_label_set_justify(GTK_LABEL(w_current->coord_world),
                          GTK_JUSTIFY_LEFT);
    gtk_container_add(GTK_CONTAINER (frame),
                      w_current->coord_world);
    gtk_box_pack_start(GTK_BOX (vbox2), frame, FALSE, FALSE, 0);
    gtk_widget_show(w_current->coord_world);
    gtk_widget_show(frame);

    buttonclose = gtk_button_new_from_stock (GTK_STOCK_CLOSE);
    GTK_WIDGET_SET_FLAGS (buttonclose, GTK_CAN_DEFAULT);
    gtk_box_pack_start(GTK_BOX ( vbox2 ),
                       buttonclose, TRUE, TRUE, 0);
    gtk_signal_connect(GTK_OBJECT (buttonclose), "clicked",
                       GTK_SIGNAL_FUNC(coord_dialog_close),
                       w_current);
    gtk_widget_show(buttonclose);
    gtk_widget_grab_default (buttonclose);

  }

  if (!GTK_WIDGET_VISIBLE (w_current->cowindow)) {
    gtk_widget_show (w_current->cowindow);
    coord_display_update(w_current, x, y);
  } else {
    gdk_window_raise(w_current->cowindow->window);
  }
}

/***************** End of coord dialog box **************************/

/***************** Start of color edit dialog box *******************/

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint color_set(GtkWidget *w, gpointer data)
{
  int index;

  /* 
   * here we really are passing an int sized piece of data, the index rather
   * than a pointer and we shouldn't have issues as long as
   * sizeof(void *) >= sizeof(int)
   */
  index = GPOINTER_TO_INT( data );

  /* hate to use this here... but I have to... */
  global_window_current->edit_color = index;
  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \warning
 *  Caller must free returned character string.
 *
 */
char *index2functionstring(int index)
{
  char *string;

  switch(index) {
    case(BACKGROUND_COLOR):
      string = g_strdup ("background");
      break;
    case(PIN_COLOR):
      string = g_strdup ("pin");
      break;
    case(NET_ENDPOINT_COLOR):
      string = g_strdup ("net endpoint");
      break;
    case(GRAPHIC_COLOR):
      string = g_strdup ("graphic");
      break;
    case(NET_COLOR):
      string = g_strdup ("net");
      break;
    case(ATTRIBUTE_COLOR):
      string = g_strdup ("attribute");
      break;
    case(LOGIC_BUBBLE_COLOR):
      string = g_strdup ("logic bubble");
      break;
    case(GRID_COLOR):
      string = g_strdup ("grid point");
      break;
    case(DETACHED_ATTRIBUTE_COLOR):
      string = g_strdup ("detached attribute");
      break;
    case(TEXT_COLOR):
      string = g_strdup ("text");
      break;
    case(BUS_COLOR):
      string = g_strdup ("bus");
      break;
    case(SELECT_COLOR):
      string = g_strdup ("select");
      break;
    case(BOUNDINGBOX_COLOR):
      string = g_strdup ("bounding box");
      break;
    case(ZOOM_BOX_COLOR):
      string = g_strdup ("zoom box");
      break;
    case(STROKE_COLOR):
      string = g_strdup ("stroke");
      break;
    case(LOCK_COLOR):
      string = g_strdup ("lock");
      break;
    case(OUTPUT_BACKGROUND_COLOR):
      string = g_strdup ("output background");
      break;
    default:
      string = g_strdup ("unknown");
      break;
  }
  return(string);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  This is from gtktest.c
 */
static GtkWidget *create_color_menu (TOPLEVEL * w_current, int * select_index)
{
  GtkWidget *menu;
  GtkWidget *menuitem;
  GSList *group;
  int index=0;
  char *buf; 
  char *menu_string;
  char *temp=NULL;

  /* first lets see if we have a selected object, if so select its color */
  int select_col = -1;
  int item_index = 0;
  SELECTION *s_current = NULL;
  OBJECT *object = NULL;

  menu = gtk_menu_new ();
  group = NULL;

  /* skip over head */
  s_current = w_current->page_current->selection2_head->next;

  if (s_current != NULL) {

    object = s_current->selected_object;
    if (object == NULL) {
      fprintf(stderr, "no object selected - WHEE!\n");
    }else{
      select_col = object->saved_color;
      /* fprintf(stderr, "setting object color %d\n", select_col); */
    }
  }else /*fprintf(stderr, "no object selected\n")*/;

  for (index=0; index < MAX_COLORS;index++) {
    
    if ((buf=x_color_get_name(index)) != NULL) {
      temp = index2functionstring(index);
      menu_string = g_strdup_printf("%d | %s | %s", index, 
				    temp,
				    buf);
      g_free(temp);
      temp = NULL;
      g_free(buf);
      buf = NULL;
      menuitem = gtk_radio_menu_item_new_with_label (group, 
                                                     menu_string);
      g_free(menu_string);
      menu_string = NULL;
      
      group = gtk_radio_menu_item_group(GTK_RADIO_MENU_ITEM(
                                                            menuitem));
      
      gtk_menu_append (GTK_MENU (menu), menuitem);
      
      gtk_signal_connect (GTK_OBJECT (menuitem), 
                          "activate", 
                          (GtkSignalFunc) color_set,
                          GINT_TO_POINTER( index ));
      
      /* I have no idea if doing the above cast is valid,
       * since index isn't a pointer, it's just being
       * treated as one, it's then cast to an int in
       * color_set.  This should be ok as long as
       * sizeof(void *) >= sizeof(int)
       */

      if (select_col == -1){
	/* set the default to the current color */
        if (index == global_window_current->edit_color) {
          gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menuitem), TRUE);
          /*fprintf(stderr, "checking item %d\n", index); */
	  *select_index = item_index;
        }
      }else{
        if (index == select_col){
          gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menuitem), TRUE);
          /* fprintf(stderr, "checking selected item %d\n", index); */
	  *select_index = item_index;
        }
      } 
      gtk_widget_show(menuitem);
      item_index++;
    }
  }
  return menu;
}

/*! \brief Apply a color change to selected objects
 *  \par Function Description
 *  This function applies a color change to the currently selected objects.
 */
void color_edit_dialog_apply(GtkWidget *w, TOPLEVEL *w_current)
{
  SELECTION *s_current = NULL;
  OBJECT *object = NULL;

  /* skip over head */
  s_current = w_current->page_current->selection2_head->next;

  while(s_current != NULL) {

    object = s_current->selected_object;
    if (object == NULL) {
      fprintf(stderr, _("ERROR: NULL object in color_edit_dialog_apply!\n"));
      exit(-1);
    }

    switch(object->type) {
      case(OBJ_LINE):
      case(OBJ_BOX):
      case(OBJ_CIRCLE):
      case(OBJ_NET):
      case(OBJ_BUS):
      case(OBJ_PIN):
      case(OBJ_ARC):
        object->saved_color = w_current->edit_color;
        w_current->page_current->CHANGED = 1;
        break;

      case(OBJ_TEXT):
        object->saved_color = w_current->edit_color;	
        o_complex_set_saved_color_only(
                                       object->text->prim_objs,
                                       w_current->edit_color);
        w_current->page_current->CHANGED = 1;
        break;
    }

    s_current = s_current->next;
  }
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \brief response function for the color edit dialog
 *  \par Function Description
 *  This function takes the user response from the color edit dialog
 */
void color_edit_dialog_response(GtkWidget *widget, gint response, TOPLEVEL *w_current)
{
  switch (response) {
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    gtk_widget_destroy(w_current->clwindow);
    w_current->clwindow = NULL;
    break;
  case GTK_RESPONSE_ACCEPT:
    color_edit_dialog_apply(widget, w_current);
    break;
  default:
    printf("ERROR: color_edit_dialog_response(): strange signal %d\n",response);
  }
}


/*! \brief Create the color edit dialog
 *  \par Function Description
 *  This function creates the color edit dialog
 */
void color_edit_dialog (TOPLEVEL *w_current)
{
  GtkWidget *optionmenu;
  GtkWidget *label;
  GtkWidget *vbox;
  int select_index = 0;

  if (!w_current->clwindow) {
    w_current->clwindow = gtk_dialog_new_with_buttons(_("Color Edit"),
						      GTK_WINDOW(w_current->main_window),
						      0, /* nonmodal dialog */
						      GTK_STOCK_CLOSE,
						      GTK_RESPONSE_REJECT,
						      GTK_STOCK_APPLY,
						      GTK_RESPONSE_ACCEPT,
						      NULL);

    gtk_window_position (GTK_WINDOW (w_current->clwindow),
                         GTK_WIN_POS_MOUSE);

    gtk_dialog_set_default_response (GTK_DIALOG (w_current->clwindow), 
				     GTK_RESPONSE_ACCEPT);

    gtk_signal_connect(GTK_OBJECT(w_current->clwindow), "response",
		       GTK_SIGNAL_FUNC(color_edit_dialog_response),
		       w_current);

    gtk_container_border_width(GTK_CONTAINER(w_current->clwindow), 
			       DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(w_current->clwindow)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_ELEMENT_SPACING);

    label = gtk_label_new(_("Object color:"));
    gtk_misc_set_alignment(GTK_MISC(label),0,0);
    gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);

    optionmenu = gtk_option_menu_new ();
    gtk_option_menu_set_menu(GTK_OPTION_MENU(optionmenu),
                             create_color_menu (w_current, &select_index));
    gtk_option_menu_set_history(GTK_OPTION_MENU (optionmenu), select_index);
    gtk_box_pack_start(GTK_BOX(vbox),
                       optionmenu, FALSE, FALSE, 0);
    gtk_widget_show_all(w_current->clwindow);
  }

  if (w_current->clwindow) {
    gtk_widget_show_all(w_current->clwindow);
    gtk_window_activate_focus(GTK_WINDOW(w_current->clwindow));
  }
}

/***************** End of color edit dialog box *********************/

/***************** Start of help/keymapping dialog box **************/

#define MAX_HOTKEY_BUFFER  256
static char *hotkey_strings[MAX_HOTKEY_BUFFER];
static int hotkey_counter=0;

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int x_dialog_hotkeys_keypress(GtkWidget * widget, GdkEventKey * event, 
			      TOPLEVEL * w_current)
{
   if (strcmp(gdk_keyval_name(event->keyval), "Escape") == 0) {
	x_dialog_hotkeys_close(NULL, w_current);	
        return TRUE;
   }

   return FALSE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_dialog_hotkeys_close (GtkWidget *w, TOPLEVEL *w_current)
{
  gtk_widget_destroy(w_current->hkwindow);
  w_current->hkwindow = NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_dialog_hotkeys_free_all(void)
{
  int i;

  for (i = 0 ; i < hotkey_counter; i++) {
    if (hotkey_strings[i]) {
      g_free(hotkey_strings[i]);
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_dialog_hotkeys_fill(char *string) 
{

  if (hotkey_counter > MAX_HOTKEY_BUFFER-1) {
    printf(_("Ran out of space in the hotkey buffer...\n"));
    return;
  }	

  hotkey_strings[hotkey_counter] = (char *) g_malloc(sizeof(char)*(
                                                                 strlen(string)+1));
  ;
  strcpy(hotkey_strings[hotkey_counter], string);
  hotkey_counter++;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_dialog_hotkeys (TOPLEVEL *w_current)
{
  GtkWidget *buttonclose = NULL;
  GtkWidget *vbox, *action_area, *scrolled_win, *list;
  GtkWidget *item;
  int i;

  if (!w_current->hkwindow) {


    w_current->hkwindow = x_create_dialog_box(&vbox, &action_area);

    gtk_window_position (GTK_WINDOW (w_current->hkwindow),
                         GTK_WIN_POS_MOUSE);

    gtk_window_set_title (GTK_WINDOW (w_current->hkwindow),
                          _("Hotkeys..."));
    gtk_container_border_width (GTK_CONTAINER (
                                               w_current->hkwindow), 5);

    gtk_widget_set_usize(w_current->hkwindow, 300,300);

    gtk_signal_connect (GTK_OBJECT (w_current->hkwindow),
                        "destroy", GTK_SIGNAL_FUNC(destroy_window),
                        &w_current->hkwindow);

    gtk_signal_connect(GTK_OBJECT(w_current->hkwindow),
                     "key_press_event",
                     (GtkSignalFunc) x_dialog_hotkeys_keypress, w_current);

#if 0 /* removed because it was causing the dialog box to not close */
    gtk_signal_connect (GTK_OBJECT (w_current->hkwindow),
                        "delete_event",
                        GTK_SIGNAL_FUNC(destroy_window),
                        &w_current->hkwindow);
#endif

    scrolled_win = gtk_scrolled_window_new (NULL, NULL);
    gtk_container_set_border_width (GTK_CONTAINER (scrolled_win), 5);
    gtk_widget_set_usize (scrolled_win, -1, 300);
    gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_win),
                                    GTK_POLICY_AUTOMATIC,
                                    GTK_POLICY_AUTOMATIC);
    gtk_widget_show (scrolled_win);

    list = gtk_list_new ();
    gtk_list_set_selection_mode (GTK_LIST (list), GTK_SELECTION_SINGLE);
    gtk_scrolled_window_add_with_viewport
      (GTK_SCROLLED_WINDOW (scrolled_win), list);
    gtk_container_set_focus_vadjustment
      (GTK_CONTAINER (list),
       gtk_scrolled_window_get_vadjustment
       (GTK_SCROLLED_WINDOW (scrolled_win)));
    gtk_container_set_focus_hadjustment
      (GTK_CONTAINER (list),
       gtk_scrolled_window_get_hadjustment
       (GTK_SCROLLED_WINDOW (scrolled_win)));
    gtk_widget_show(list);

    item = gtk_list_item_new_with_label (
                                         _("Function : keystroke(s)"));
    gtk_container_add (GTK_CONTAINER (list), item);
    gtk_widget_show(item);

    item = gtk_list_item_new_with_label (" ");
    gtk_container_add (GTK_CONTAINER (list), item);
    gtk_widget_show(item);

    for (i = 0 ; i < hotkey_counter; i++) {

      if (hotkey_strings[i]) {	
        item = gtk_list_item_new_with_label (
                                             hotkey_strings[i]);
        gtk_container_add (GTK_CONTAINER (list), item);
        gtk_widget_show(item);
      }
    }

    buttonclose = gtk_button_new_from_stock (GTK_STOCK_CLOSE);
    GTK_WIDGET_SET_FLAGS (buttonclose, GTK_CAN_DEFAULT);
    gtk_box_pack_start(
                       GTK_BOX(action_area),
                       buttonclose, TRUE, TRUE, 0);
    gtk_signal_connect(GTK_OBJECT (buttonclose), "clicked",
                       GTK_SIGNAL_FUNC(x_dialog_hotkeys_close),
                       w_current);
    gtk_widget_show(buttonclose);

  }

  if (!GTK_WIDGET_VISIBLE(w_current->hkwindow)) {
    gtk_widget_show(w_current->hkwindow);
  } else {
    gdk_window_raise(w_current->hkwindow->window);
  }
}

/***************** End of help/keymapping dialog box ****************/

/*********** Start of misc support functions for dialog boxes *******/
extern GtkWidget *stwindow;

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_dialog_raise_all(TOPLEVEL *w_current)
{

#if 0 /* don't raise the log window */
  if (stwindow) {
    gdk_window_raise(stwindow->window);
  }
#endif

  if(w_current->fowindow) {
    gdk_window_raise(w_current->fowindow->window);
  }
  if(w_current->fswindow) {
    gdk_window_raise(w_current->fswindow->window);
  }
  if(w_current->sowindow) {
    gdk_window_raise(w_current->sowindow->window);
  }
  if(w_current->aswindow) {
    gdk_window_raise(w_current->aswindow->window);
  }
  if(w_current->cswindow) {
    gdk_window_raise(w_current->cswindow->window);
  }

#if 0 /* don't raise these windows ever */ 
  if(w_current->fileselect[FILESELECT].xfwindow) {
    gdk_window_raise(w_current->fileselect[FILESELECT].xfwindow->window);
  }
  if(w_current->fileselect[COMPSELECT].xfwindow) {
    gdk_window_raise(w_current->fileselect[COMPSELECT].xfwindow->window);
  }
#endif

  if(w_current->iwindow) {
    gdk_window_raise(w_current->iwindow->window);
  }

#if 0 /* don't raise the page manager window */
  if(w_current->pswindow) {
    gdk_window_raise(w_current->pswindow->window);
  }
#endif

  if(w_current->tiwindow) {
    gdk_window_raise(w_current->tiwindow->window);
  }
  if(w_current->tewindow) {
    gdk_window_raise(w_current->tewindow->window);
  }
  if(w_current->sewindow) {
    gdk_window_raise(w_current->sewindow->window);
  }
  if(w_current->exwindow) {
    gdk_window_raise(w_current->exwindow->window);
  }
  if(w_current->aawindow) {
    gdk_window_raise(w_current->aawindow->window);
  }
  if(w_current->mawindow) {
    gdk_window_raise(w_current->mawindow->window);
  }
  if(w_current->aewindow) {
    gdk_window_raise(w_current->aewindow->window);
  }
  if(w_current->trwindow) {
    gdk_window_raise(w_current->trwindow->window);
  }
  if(w_current->tswindow) {
    gdk_window_raise(w_current->tswindow->window);
  }
  if(w_current->abwindow) {
    gdk_window_raise(w_current->abwindow->window);
  }
  if(w_current->hkwindow) {
    gdk_window_raise(w_current->hkwindow->window);
  }
  if(w_current->cowindow) {
    gdk_window_raise(w_current->cowindow->window);
  }
  if(w_current->clwindow) {
    gdk_window_raise(w_current->clwindow->window);
  }
  if(w_current->ltwindow) {
    gdk_window_raise(w_current->ltwindow->window);
  }
  if(w_current->ftwindow) {
    gdk_window_raise(w_current->ftwindow->window);
  }

}

/*********** End of misc support functions for dialog boxes *******/

/***************** Start of generic message dialog box *******************/

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void generic_msg_dialog (const char *msg)
{
  GtkWidget *dialog;
  
  dialog = gtk_message_dialog_new (NULL,


                                   GTK_DIALOG_MODAL
                                   | GTK_DIALOG_DESTROY_WITH_PARENT,
                                   GTK_MESSAGE_INFO,
                                   GTK_BUTTONS_OK,
                                   msg);

  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);

}

/***************** End of generic message dialog box *********************/

/***************** Start of generic confirm dialog box *******************/

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int generic_confirm_dialog (const char *msg)
{
  GtkWidget *dialog;
  gint r;

  dialog = gtk_message_dialog_new (NULL,


                                   GTK_DIALOG_MODAL
                                   | GTK_DIALOG_DESTROY_WITH_PARENT,
                                   GTK_MESSAGE_INFO,
                                   GTK_BUTTONS_OK_CANCEL,
                                   msg);

  r = gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);

  if (r ==  GTK_RESPONSE_OK)
    return 1;
  else 
    return 0;
}

/***************** End of generic confirm dialog box *********************/

/***************** Start of generic file select dialog box ***************/
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \warning
 *   Caller must free returned character string.
 */
char *generic_filesel_dialog (const char *msg, const char *templ, gint flags)
{
  GtkWidget *dialog;
  gchar *result = NULL, *folder, *seed;
  char *title;
  static gchar *path = NULL;
  static gchar *shortcuts = NULL;

  /* Default to load if not specified.  Maybe this should cause an error. */ 
  if (! (flags & (FSB_LOAD | FSB_SAVE))) {
    flags = flags | FSB_LOAD;
  }

  if (flags & FSB_LOAD) {
    title = g_strdup_printf("%s: Open", msg);
    dialog = gtk_file_chooser_dialog_new (title,
					  NULL,
					  GTK_FILE_CHOOSER_ACTION_OPEN,
					  GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					  GTK_STOCK_OPEN, GTK_RESPONSE_OK,
					  NULL);
    /* Since this is a load dialog box, the file must exist! */
    flags = flags | FSB_MUST_EXIST;

  } else {
    title = g_strdup_printf("%s: Save", msg);
    dialog = gtk_file_chooser_dialog_new (title,
					  NULL,
					  GTK_FILE_CHOOSER_ACTION_SAVE,
					  GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
					  GTK_STOCK_OPEN, GTK_RESPONSE_OK,
					  NULL);
  }

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_OK);

  /* Pick the current default folder to look for files in */
  if (path && *path) {
    gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (dialog), path);
  }


  /* Pick the current template (*.rc) or default file name */
  if (templ && *templ) {
    if (flags & FSB_SAVE)  {
      gtk_file_chooser_set_current_name (GTK_FILE_CHOOSER (dialog), templ);
    } else {
      gtk_file_chooser_select_filename (GTK_FILE_CHOOSER (dialog), templ);
    }
  }

  
  if (shortcuts && *shortcuts) {
    printf ("shortcuts = \"%s\"\n", shortcuts);
    folder = g_strdup (shortcuts);
    seed = folder;
    while ((folder = strtok (seed, ":")) != NULL) {
      gtk_file_chooser_add_shortcut_folder (GTK_FILE_CHOOSER (dialog),
					    folder, NULL);
      seed = NULL;
    }
  
    g_free (folder);
  }

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_OK) {
    result = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (dialog));
    folder = gtk_file_chooser_get_current_folder (GTK_FILE_CHOOSER (dialog));
    /*! \bug FIXME
    if (folder && path) {
      dup_string (path, folder);
      g_free (folder);
    }
    */
  }
  gtk_widget_destroy (dialog);

  g_free (title);

  return result;

}

/***************** End of generic file select dialog box *****************/

/*********** Start of generic text input dialog box *******/
char generic_textstring[256] = "refdes=R";

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void generic_text_input_ok(GtkWidget * w, TOPLEVEL * w_current)
{
  char *string = NULL;

  string = (char *) gtk_entry_get_text(GTK_ENTRY(w_current->tsentry));
  strncpy(generic_textstring, string, 256);

  gtk_grab_remove(w_current->tswindow);
  gtk_widget_destroy(w_current->tswindow);
  w_current->tswindow = NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void generic_text_input_dialog(TOPLEVEL * w_current)
{
  int len;
  GtkWidget *label = NULL;
  GtkWidget *buttonok = NULL;
  GtkWidget *vbox, *action_area;

  if (!w_current->tswindow) {
    w_current->tswindow = x_create_dialog_box(&vbox, &action_area);

    gtk_window_position(GTK_WINDOW(w_current->tswindow),
			GTK_WIN_POS_MOUSE);

    gtk_signal_connect(GTK_OBJECT(w_current->tswindow),
		       "destroy",
		       GTK_SIGNAL_FUNC(destroy_window),
		       &w_current->tswindow);


#if 0				/* removed because it was causing the dialog box to not close */
    gtk_signal_connect(GTK_OBJECT(w_current->tswindow),
		       "delete_event",
		       GTK_SIGNAL_FUNC(destroy_window),
		       &w_current->tswindow);
#endif

    gtk_window_set_title(GTK_WINDOW(w_current->tswindow),
			 _("Generic String"));
    gtk_container_border_width(GTK_CONTAINER(w_current->tswindow), 10);

    label = gtk_label_new(_("Enter new string."));
    gtk_misc_set_padding(GTK_MISC(label), 20, 20);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);
    gtk_widget_show(label);

    w_current->tsentry = gtk_entry_new_with_max_length(20);
    gtk_editable_select_region(GTK_EDITABLE(w_current->tsentry), 0, -1);
    gtk_box_pack_start(GTK_BOX(vbox), w_current->tsentry, FALSE, FALSE, 5);
    gtk_signal_connect(GTK_OBJECT(w_current->tsentry), "activate",
		       GTK_SIGNAL_FUNC(generic_text_input_ok), w_current);
    gtk_widget_show(w_current->tsentry);
    gtk_widget_grab_focus(w_current->tsentry);

    buttonok = gtk_button_new_from_stock (GTK_STOCK_OK);
    GTK_WIDGET_SET_FLAGS(buttonok, GTK_CAN_DEFAULT);
    gtk_box_pack_start(GTK_BOX(action_area), buttonok, TRUE, TRUE, 0);
    gtk_signal_connect(GTK_OBJECT(buttonok), "clicked",
		       GTK_SIGNAL_FUNC(generic_text_input_ok), w_current);
    gtk_widget_show(buttonok);
    gtk_widget_grab_default(buttonok);
  }

  if (!GTK_WIDGET_VISIBLE(w_current->tswindow)) {
    len = strlen(generic_textstring);
    gtk_entry_set_text(GTK_ENTRY(w_current->tsentry), generic_textstring);
    gtk_entry_select_region(GTK_ENTRY(w_current->tsentry), 0, len);
    gtk_widget_show(w_current->tswindow);
    gtk_grab_add(w_current->tswindow);
  }
}

/*********** End of generic text input dialog box *******/

/*********** Start of find text dialog box *******/
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int find_text_keypress(GtkWidget * widget, GdkEventKey * event, 
		       TOPLEVEL * w_current)
{
   if (strcmp(gdk_keyval_name(event->keyval), "Escape") == 0) {
	find_text_done(NULL, w_current);	
        return TRUE;
   }

   return FALSE;
}

GtkWidget *checkdescend = NULL;
int start_find;
OBJECT *remember_page;

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void find_text_ok(GtkWidget * w, TOPLEVEL * w_current)
{
  char *string = NULL;
  int done;

  string = (char *) gtk_entry_get_text(GTK_ENTRY(w_current->tsentry));

  /* descend = gtk_object_get_data(GTK_OBJECT(w_current->tsentry),"descend");*/

  strncpy(generic_textstring, string, 256);

  while (remember_page != w_current->page_current->object_head) {
    s_hierarchy_up(w_current, w_current->page_current->up);
  }
  done =
      o_edit_find_text(w_current, remember_page, string,
         gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON
          (checkdescend)),
         !start_find);
  if (done) {
    o_redraw_all_fast(w_current);
    gtk_grab_remove(w_current->tswindow);
    gtk_widget_destroy(w_current->tswindow);
    w_current->tswindow = NULL;
  }
  start_find = 0;

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void find_text_done(GtkWidget * w, TOPLEVEL * w_current)
{
  gtk_grab_remove(w_current->tswindow);
  gtk_widget_destroy(w_current->tswindow);
  w_current->tswindow = NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void find_text_dialog(TOPLEVEL * w_current)
{
  int len;
  GtkWidget *label = NULL;
  GtkWidget *buttonok = NULL;
  GtkWidget *buttondone = NULL;
  GtkWidget *vbox, *action_area;
  OBJECT *object = NULL;

  start_find = 1;
  remember_page = w_current->page_current->object_head;
  if ((object = o_select_return_first_object(w_current)) != NULL) {
    if (object->type == OBJ_TEXT) {
      strncpy(generic_textstring, object->text->string, 256);
    }
  }



  if (!w_current->tswindow) {
    w_current->tswindow = x_create_dialog_box(&vbox, &action_area);

    gtk_window_position(GTK_WINDOW(w_current->tswindow),
			GTK_WIN_POS_MOUSE);

    gtk_signal_connect(GTK_OBJECT(w_current->tswindow),
		       "destroy",
		       GTK_SIGNAL_FUNC(destroy_window),
		       &w_current->tswindow);

    gtk_signal_connect(GTK_OBJECT(w_current->tswindow),
                     "key_press_event",
                     (GtkSignalFunc) find_text_keypress, w_current);

#if 0				/* removed because it was causing the dialog box to not close */
    gtk_signal_connect(GTK_OBJECT(w_current->tswindow),
		       "delete_event",
		       GTK_SIGNAL_FUNC(destroy_window),
		       &w_current->tswindow);
#endif

    gtk_window_set_title(GTK_WINDOW(w_current->tswindow), _("Find text"));
    gtk_container_border_width(GTK_CONTAINER(w_current->tswindow), 10);
    gtk_box_set_spacing(GTK_BOX(vbox),10);

    label = gtk_label_new(_("Text to find:"));
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);
    gtk_widget_show(label);

    w_current->tsentry = gtk_entry_new_with_max_length(20);
    gtk_editable_select_region(GTK_EDITABLE(w_current->tsentry), 0, -1);
    gtk_box_pack_start(GTK_BOX(vbox), w_current->tsentry, FALSE, FALSE, 5);
    gtk_signal_connect(GTK_OBJECT(w_current->tsentry), "activate",
		       GTK_SIGNAL_FUNC(find_text_ok), w_current);
    gtk_widget_show(w_current->tsentry);
    gtk_widget_grab_focus(w_current->tsentry);

    checkdescend =
	gtk_check_button_new_with_label(_("descend into hierarchy"));
    /*          gtk_object_set_data (GTK_OBJECT (w_current->tswindow), "descend", w_current->preview_checkbox);*/
    gtk_box_pack_start(GTK_BOX(vbox), checkdescend, TRUE, TRUE, 0);

    buttondone = gtk_button_new_from_stock (GTK_STOCK_CLOSE);
    GTK_WIDGET_SET_FLAGS(buttondone, GTK_CAN_DEFAULT);
    gtk_box_pack_start(GTK_BOX(action_area), buttondone, TRUE, TRUE, 0);
    gtk_signal_connect(GTK_OBJECT(buttondone), "clicked",
		       GTK_SIGNAL_FUNC(find_text_done), w_current);

    buttonok = gtk_button_new_from_stock (GTK_STOCK_FIND);
    GTK_WIDGET_SET_FLAGS(buttonok, GTK_CAN_DEFAULT);
    gtk_box_pack_start(GTK_BOX(action_area), buttonok, TRUE, TRUE, 0);
    gtk_signal_connect(GTK_OBJECT(buttonok), "clicked",
		       GTK_SIGNAL_FUNC(find_text_ok), w_current);



    gtk_widget_show(buttonok);
    gtk_widget_show(buttondone);
    gtk_widget_show(checkdescend);
    gtk_widget_grab_default(buttonok);
  }

  if (!GTK_WIDGET_VISIBLE(w_current->tswindow)) {
    len = strlen(generic_textstring);
    gtk_entry_set_text(GTK_ENTRY(w_current->tsentry), generic_textstring);
    gtk_entry_select_region(GTK_ENTRY(w_current->tsentry), 0, len);
    gtk_widget_show(w_current->tswindow);
    gtk_grab_add(w_current->tswindow);
  }
}

/*********** End of find text dialog box *******/

/*********** Start of hide text dialog box *******/

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int hide_text_keypress(GtkWidget * widget, GdkEventKey * event, 
		       TOPLEVEL * w_current)
{
   if (strcmp(gdk_keyval_name(event->keyval), "Escape") == 0) {
	hide_text_done(NULL, w_current);	
        return TRUE;
   }

   return FALSE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void hide_text_ok(GtkWidget * w, TOPLEVEL * w_current)
{
  char *string = NULL;

  string = (char *) gtk_entry_get_text(GTK_ENTRY(w_current->tsentry));
  strncpy(generic_textstring, string, 256);

  o_edit_hide_specific_text(w_current,
			    w_current->page_current->object_head, string);

  gtk_grab_remove(w_current->tswindow);
  gtk_widget_destroy(w_current->tswindow);
  w_current->tswindow = NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void hide_text_done(GtkWidget * w, TOPLEVEL * w_current)
{
  gtk_grab_remove(w_current->tswindow);
  gtk_widget_destroy(w_current->tswindow);
  w_current->tswindow = NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void hide_text_dialog(TOPLEVEL * w_current)
{
  int len;
  GtkWidget *label = NULL;
  GtkWidget *buttonok = NULL;
  GtkWidget *vbox, *action_area;

  if (!w_current->tswindow) {
    w_current->tswindow = x_create_dialog_box(&vbox, &action_area);

    gtk_window_position(GTK_WINDOW(w_current->tswindow),
			GTK_WIN_POS_MOUSE);

    gtk_signal_connect(GTK_OBJECT(w_current->tswindow),
		       "destroy",
		       GTK_SIGNAL_FUNC(destroy_window),
		       &w_current->tswindow);

    gtk_signal_connect(GTK_OBJECT(w_current->tswindow),
                     "key_press_event",
                     (GtkSignalFunc) hide_text_keypress, w_current);

#if 0				/* removed because it was causing the dialog box to not close */
    gtk_signal_connect(GTK_OBJECT(w_current->tswindow),
		       "delete_event",
		       GTK_SIGNAL_FUNC(destroy_window),
		       &w_current->tswindow);
#endif

    gtk_window_set_title(GTK_WINDOW(w_current->tswindow), _("Hide text"));
    gtk_container_border_width(GTK_CONTAINER(w_current->tswindow), 10);

    label = gtk_label_new(_("Hide text starting with:"));
    gtk_misc_set_padding(GTK_MISC(label), 20, 20);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);
    gtk_widget_show(label);

    w_current->tsentry = gtk_entry_new_with_max_length(20);
    gtk_editable_select_region(GTK_EDITABLE(w_current->tsentry), 0, -1);
    gtk_box_pack_start(GTK_BOX(vbox), w_current->tsentry, FALSE, FALSE, 5);
    gtk_signal_connect(GTK_OBJECT(w_current->tsentry), "activate",
		       GTK_SIGNAL_FUNC(hide_text_ok), w_current);
    gtk_widget_show(w_current->tsentry);
    gtk_widget_grab_focus(w_current->tsentry);

    buttonok = gtk_button_new_from_stock (GTK_STOCK_OK);
    GTK_WIDGET_SET_FLAGS(buttonok, GTK_CAN_DEFAULT);
    gtk_box_pack_start(GTK_BOX(action_area), buttonok, TRUE, TRUE, 0);
    gtk_signal_connect(GTK_OBJECT(buttonok), "clicked",
		       GTK_SIGNAL_FUNC(hide_text_ok), w_current);
    gtk_widget_show(buttonok);
    gtk_widget_grab_default(buttonok);
  }

  if (!GTK_WIDGET_VISIBLE(w_current->tswindow)) {
    len = strlen(generic_textstring);
    gtk_entry_set_text(GTK_ENTRY(w_current->tsentry), generic_textstring);
    gtk_entry_select_region(GTK_ENTRY(w_current->tsentry), 0, len);
    gtk_widget_show(w_current->tswindow);
    gtk_grab_add(w_current->tswindow);
  }
}

/*********** End of hide text dialog box *******/

/*********** Start of show text dialog box *******/

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int show_text_keypress(GtkWidget * widget, GdkEventKey * event, 
		       TOPLEVEL * w_current)
{
   if (strcmp(gdk_keyval_name(event->keyval), "Escape") == 0) {
	show_text_done(NULL, w_current);	
        return TRUE;
   }

   return FALSE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void show_text_ok(GtkWidget * w, TOPLEVEL * w_current)
{
  char *string = NULL;

  string = (char *) gtk_entry_get_text(GTK_ENTRY(w_current->tsentry));
  strncpy(generic_textstring, string, 256);

  o_edit_show_specific_text(w_current,
			    w_current->page_current->object_head, string);

  gtk_grab_remove(w_current->tswindow);
  gtk_widget_destroy(w_current->tswindow);
  w_current->tswindow = NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void show_text_done(GtkWidget * w, TOPLEVEL * w_current)
{
  gtk_grab_remove(w_current->tswindow);
  gtk_widget_destroy(w_current->tswindow);
  w_current->tswindow = NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void show_text_dialog(TOPLEVEL * w_current)
{
  int len;
  GtkWidget *label = NULL;
  GtkWidget *buttonok = NULL;
  GtkWidget *vbox, *action_area;

  if (!w_current->tswindow) {
    w_current->tswindow = x_create_dialog_box(&vbox, &action_area);

    gtk_window_position(GTK_WINDOW(w_current->tswindow),
			GTK_WIN_POS_MOUSE);

    gtk_signal_connect(GTK_OBJECT(w_current->tswindow),
		       "destroy",
		       GTK_SIGNAL_FUNC(destroy_window),
		       &w_current->tswindow);

    gtk_signal_connect(GTK_OBJECT(w_current->tswindow),
                     "key_press_event",
                     (GtkSignalFunc) show_text_keypress, w_current);

#if 0				/* removed because it was causing the dialog box to not close */
    gtk_signal_connect(GTK_OBJECT(w_current->tswindow),
		       "delete_event",
		       GTK_SIGNAL_FUNC(destroy_window),
		       &w_current->tswindow);
#endif

    gtk_window_set_title(GTK_WINDOW(w_current->tswindow), _("Show text"));
    gtk_container_border_width(GTK_CONTAINER(w_current->tswindow), 10);

    label = gtk_label_new(_("Show text starting with:"));
    gtk_misc_set_padding(GTK_MISC(label), 20, 20);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);
    gtk_widget_show(label);

    w_current->tsentry = gtk_entry_new_with_max_length(20);
    gtk_editable_select_region(GTK_EDITABLE(w_current->tsentry), 0, -1);
    gtk_box_pack_start(GTK_BOX(vbox), w_current->tsentry, FALSE, FALSE, 5);
    gtk_signal_connect(GTK_OBJECT(w_current->tsentry), "activate",
		       GTK_SIGNAL_FUNC(show_text_ok), w_current);
    gtk_widget_show(w_current->tsentry);
    gtk_widget_grab_focus(w_current->tsentry);

    buttonok = gtk_button_new_from_stock (GTK_STOCK_OK);
    GTK_WIDGET_SET_FLAGS(buttonok, GTK_CAN_DEFAULT);
    gtk_box_pack_start(GTK_BOX(action_area), buttonok, TRUE, TRUE, 0);
    gtk_signal_connect(GTK_OBJECT(buttonok), "clicked",
		       GTK_SIGNAL_FUNC(show_text_ok), w_current);
    gtk_widget_show(buttonok);
    gtk_widget_grab_default(buttonok);
  }

  if (!GTK_WIDGET_VISIBLE(w_current->tswindow)) {
    len = strlen(generic_textstring);
    gtk_entry_set_text(GTK_ENTRY(w_current->tsentry), generic_textstring);
    gtk_entry_select_region(GTK_ENTRY(w_current->tsentry), 0, len);
    gtk_widget_show(w_current->tswindow);
    gtk_grab_add(w_current->tswindow);
  }
}

/*********** End of show text dialog box *******/

/*********** Start of autonumber text dialog box *******/

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int autonumber_text_keypress(GtkWidget * widget, GdkEventKey * event, 
			     TOPLEVEL * w_current)
{
   if (strcmp(gdk_keyval_name(event->keyval), "Escape") == 0) {
	autonumber_text_done(NULL, w_current);	
        return TRUE;
   }
   if (strcmp(gdk_keyval_name(event->keyval), "Return") == 0) {
	autonumber_text_ok(NULL, w_current);	
        return TRUE;
   }
   return FALSE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void autonumber_text_ok(GtkWidget * w, TOPLEVEL * w_current)
{
  char *searchtext = NULL;
  char *startnumber = NULL;
  int sortorder, searchfocus, unnumbered, startnumber_i=1;
  GtkWidget * widget;
  
  widget = g_object_get_data (G_OBJECT (w_current->tswindow), "searchtext");
  searchtext = (char *) gtk_entry_get_text(GTK_ENTRY(widget));
  strncpy(generic_textstring, searchtext, 256); /* ???????? */

  widget = g_object_get_data (G_OBJECT (w_current->tswindow), "startnumber");
  startnumber = (char *) gtk_entry_get_text(GTK_ENTRY(widget));
  sscanf(startnumber," %d",&startnumber_i);

  widget = g_object_get_data (G_OBJECT (w_current->tswindow), "searchfocus");
  searchfocus = (int) gtk_option_menu_get_history (GTK_OPTION_MENU(widget));

  widget = g_object_get_data (G_OBJECT (w_current->tswindow), "unnumbered");
  unnumbered = (int) gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget));

  widget = g_object_get_data (G_OBJECT (w_current->tswindow), "sortorder");
  sortorder = (int) gtk_option_menu_get_history(GTK_OPTION_MENU(widget));

  if (startnumber_i < 0) {  /* disallow negativ numbers */
    fprintf(stderr, _("Warning: negative numbers not allowed in the "
		      "autonumber_text dialog\n"));
    return;
  }

  /*  printf("autonumber_text_ok: searchtext: %s\n"
	 "   startnumber: %i\n   searchfocus: %i\n"
	 "   unnumbered: %i\n   sortorder: %i\n" ,searchtext,
	 startnumber_i, searchfocus, unnumbered, sortorder); */
  o_edit_autonumber_text(w_current, searchtext, startnumber_i, searchfocus, 
			 unnumbered, sortorder);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void autonumber_text_done(GtkWidget * w, TOPLEVEL * w_current)
{
  gtk_grab_remove(w_current->tswindow);
  gtk_widget_destroy(w_current->tswindow);
  w_current->tswindow = NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void autonumber_text_dialog(TOPLEVEL * w_current)
{
  GtkWidget *vbox, *action_area;
  GtkWidget *frame1; /* selection frame */
  GtkWidget *label1;
  GtkWidget *table1;
  GtkWidget *label1_1, *label1_2;
  GtkWidget *combo1;
  GList *combo1_items = NULL;
  GtkWidget *combo_entry1;

  GtkWidget *optionmenu1_2, *menu1_2;
  GtkWidget *option_selection1, *option_selection2, *option_selection3;
  GtkWidget *hbox1;  /* contains radio buttons */
  GtkWidget *radiobutton1;
  GtkWidget *radiobutton2;

  GtkWidget *frame2; /* option frame */
  GtkWidget *label2;
  GtkWidget *table2;
  GtkWidget *label2_1, *label2_2;
  GtkWidget *entry2;
  GtkWidget *optionmenu2_1, *menu2_1;
  GtkWidget *option_order1, *option_order2, *option_order3, *option_order4;

  GtkWidget *buttonok = NULL;
  GtkWidget *buttoncancel = NULL;

  if (!w_current->tswindow) {
    w_current->tswindow = gtk_dialog_new();
    vbox=GTK_DIALOG(w_current->tswindow)->vbox;
    action_area=GTK_DIALOG(w_current->tswindow)->action_area;

    gtk_window_position(GTK_WINDOW(w_current->tswindow),
			GTK_WIN_POS_MOUSE);

    gtk_signal_connect(GTK_OBJECT(w_current->tswindow),
		       "destroy",
		       GTK_SIGNAL_FUNC(destroy_window),
		       &w_current->tswindow);

    gtk_signal_connect(GTK_OBJECT(w_current->tswindow),
                       "key_press_event",
                       (GtkSignalFunc) autonumber_text_keypress, 
		       w_current);

#if 0	   /* removed because it was causing the dialog box to not close */
    gtk_signal_connect(GTK_OBJECT(w_current->tswindow),
		       "delete_event",
		       GTK_SIGNAL_FUNC(destroy_window),
		       &w_current->tswindow);
#endif

    gtk_window_set_title(GTK_WINDOW(w_current->tswindow),
			 _("Autonumber text"));

    /* selection frame */
    frame1 = gtk_frame_new(NULL);
    gtk_widget_show (frame1);
    gtk_box_pack_start (GTK_BOX(vbox), frame1, TRUE, TRUE, 0);
    label1 = gtk_label_new (_("selection"));
    gtk_widget_show (label1);
    gtk_frame_set_label_widget (GTK_FRAME(frame1), label1);

    table1 = gtk_table_new(2,3,FALSE);
    gtk_widget_show (table1);
    gtk_container_add (GTK_CONTAINER(frame1), table1);

    /* search text section */
    label1_1 = gtk_label_new (_("search text"));
    gtk_widget_show (label1_1);
    gtk_table_attach (GTK_TABLE (table1), label1_1, 0, 1, 0, 1,
		      (GtkAttachOptions) (GTK_FILL|GTK_EXPAND),
		      (GtkAttachOptions) (0), 0, 0);

    combo1 = gtk_combo_new ();
    /* g_object_set_data (G_OBJECT (GTK_COMBO (combo1)->popwin),
       "GladeParentKey", combo1);*/
    gtk_widget_show (combo1);
    gtk_table_attach (GTK_TABLE (table1), combo1, 1, 2, 0, 1,
		      (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		      (GtkAttachOptions) (0), 0, 0);
    /* gtk_combo_set_value_in_list (GTK_COMBO (combo1), TRUE, TRUE);*/
    combo1_items = g_list_append (combo1_items, (gpointer) "refdes=C");
    combo1_items = g_list_append (combo1_items, (gpointer) "refdes=Q");
    combo1_items = g_list_append (combo1_items, (gpointer) "refdes=R");
    combo1_items = g_list_append (combo1_items, (gpointer) "refdes=U");
    combo1_items = g_list_append (combo1_items, (gpointer) "refdes=X");
    gtk_combo_set_popdown_strings (GTK_COMBO (combo1), combo1_items);
    g_list_free (combo1_items);

    combo_entry1 = GTK_COMBO (combo1)->entry;
    gtk_widget_show (combo_entry1);
    gtk_entry_set_text (GTK_ENTRY (combo_entry1), "refdes=C");	
    GLADE_HOOKUP_OBJECT(w_current->tswindow,combo_entry1,"searchtext");

    /* search focus section */
    label1_2 = gtk_label_new (_("search focus"));
    gtk_widget_show (label1_2);
    gtk_table_attach (GTK_TABLE (table1), label1_2, 0, 1, 1, 2,
		      (GtkAttachOptions) (GTK_FILL|GTK_EXPAND),
		      (GtkAttachOptions) (0), 0, 0);

    optionmenu1_2 = gtk_option_menu_new ();
    gtk_widget_show (optionmenu1_2);
    gtk_table_attach (GTK_TABLE (table1), optionmenu1_2, 1, 2, 1, 2,
		      (GtkAttachOptions) (GTK_FILL|GTK_EXPAND),
		      (GtkAttachOptions) (0), 0, 0);

    menu1_2 = gtk_menu_new();
    option_selection1 = gtk_menu_item_new_with_label (_("selected objects"));
    gtk_widget_show (option_selection1);
    gtk_container_add (GTK_CONTAINER (menu1_2), option_selection1);
    option_selection2 = gtk_menu_item_new_with_label (_("current sheet"));
    gtk_widget_show (option_selection2);
    gtk_container_add (GTK_CONTAINER (menu1_2), option_selection2);
    option_selection3 = gtk_menu_item_new_with_label (_("hierarchical sheets"));
    gtk_widget_show (option_selection3);
    gtk_container_add (GTK_CONTAINER (menu1_2), option_selection3);
    gtk_option_menu_set_menu (GTK_OPTION_MENU (optionmenu1_2), menu1_2);
    GLADE_HOOKUP_OBJECT(w_current->tswindow,optionmenu1_2,"searchfocus");

    /* radio buttons */
    hbox1=gtk_hbox_new (FALSE,0);
    gtk_widget_show (hbox1);
    gtk_table_attach (GTK_TABLE (table1), hbox1, 0, 2, 2, 3,
		      (GtkAttachOptions) (GTK_FILL|GTK_EXPAND),
		      (GtkAttachOptions) (0), 0, 0);
    radiobutton1 = gtk_radio_button_new_with_label (NULL, _("unnumbered"));
    gtk_widget_show (radiobutton1);
    gtk_box_pack_start (GTK_BOX (hbox1), radiobutton1, TRUE, TRUE, 0);
    radiobutton2 = gtk_radio_button_new_with_label_from_widget(
		     GTK_RADIO_BUTTON (radiobutton1), _("all"));
    gtk_widget_show (radiobutton2);
    gtk_box_pack_start (GTK_BOX (hbox1), radiobutton2, TRUE, TRUE, 0);
    GLADE_HOOKUP_OBJECT(w_current->tswindow,radiobutton1,"unnumbered");

    /* option frame */
    frame2=gtk_frame_new(NULL);
    gtk_widget_show(frame2);
    gtk_box_pack_start (GTK_BOX(vbox), frame2, TRUE, TRUE, 0);
    label2 = gtk_label_new (_("options"));
    gtk_widget_show (label2);
    gtk_frame_set_label_widget (GTK_FRAME(frame2), label2);
    
    table2 = gtk_table_new(2,2,FALSE);
    gtk_widget_show(table2);
    gtk_container_add (GTK_CONTAINER (frame2),table2);
    label2_1=gtk_label_new(_("start number"));
    gtk_widget_show(label2_1);
    gtk_table_attach (GTK_TABLE (table2), label2_1, 0, 1, 0, 1,
		      (GtkAttachOptions) (GTK_FILL|GTK_EXPAND),
		      (GtkAttachOptions) (0), 0, 0);
    entry2=gtk_entry_new();
    gtk_entry_set_text(GTK_ENTRY(entry2), "1");
    gtk_widget_show(entry2);
    gtk_table_attach (GTK_TABLE (table2), entry2, 1, 2, 0, 1,
		      (GtkAttachOptions) (GTK_FILL|GTK_EXPAND),
		      (GtkAttachOptions) (0), 0, 0);
    GLADE_HOOKUP_OBJECT(w_current->tswindow,entry2,"startnumber");

    label2_2=gtk_label_new(_("sort order"));
    gtk_widget_show(label2_2);
    gtk_table_attach (GTK_TABLE (table2), label2_2, 0, 1, 1, 2,
		      (GtkAttachOptions) (GTK_FILL|GTK_EXPAND),
		      (GtkAttachOptions) (0), 0, 0);

    optionmenu2_1 = gtk_option_menu_new ();
    gtk_widget_show (optionmenu2_1);
    gtk_table_attach (GTK_TABLE (table2), optionmenu2_1, 1, 2, 1, 2,
		      (GtkAttachOptions) (GTK_FILL|GTK_EXPAND),
		      (GtkAttachOptions) (0), 0, 0);
    menu2_1 = gtk_menu_new();
    option_order1 = gtk_menu_item_new_with_label (_("file order"));
    gtk_widget_show (option_order1);
    gtk_container_add (GTK_CONTAINER (menu2_1), option_order1);
    option_order2 = gtk_menu_item_new_with_label (_("top down"));
    gtk_widget_show (option_order2);
    gtk_container_add (GTK_CONTAINER (menu2_1), option_order2);
    option_order3 = gtk_menu_item_new_with_label (_("left right"));
    gtk_widget_show (option_order3);
    gtk_container_add (GTK_CONTAINER (menu2_1), option_order3);
    option_order4 = gtk_menu_item_new_with_label (_("diagonal"));
    gtk_widget_show (option_order4);
    gtk_container_add (GTK_CONTAINER (menu2_1), option_order4);
    gtk_option_menu_set_menu (GTK_OPTION_MENU (optionmenu2_1), menu2_1);
    GLADE_HOOKUP_OBJECT(w_current->tswindow,optionmenu2_1,"sortorder");
    
    /* Why is the entry attached to w_current ?? (Werner) */
    w_current->tsentry = combo_entry1;

    buttoncancel = gtk_button_new_from_stock (GTK_STOCK_CLOSE);
    gtk_box_pack_start(GTK_BOX(action_area), buttoncancel, TRUE, TRUE, 0);
    gtk_signal_connect(GTK_OBJECT(buttoncancel), "clicked",
		       GTK_SIGNAL_FUNC(autonumber_text_done), w_current);
    gtk_widget_show(buttoncancel);
    
    buttonok = gtk_button_new_from_stock (GTK_STOCK_APPLY);
    GTK_WIDGET_SET_FLAGS(buttonok, GTK_CAN_DEFAULT);
    gtk_box_pack_start(GTK_BOX(action_area), buttonok, TRUE, TRUE, 0);
    gtk_signal_connect(GTK_OBJECT(buttonok), "clicked",
		       GTK_SIGNAL_FUNC(autonumber_text_ok), w_current);
    gtk_widget_show(buttonok);
    gtk_widget_grab_default(buttonok);
  }

  if (!GTK_WIDGET_VISIBLE(w_current->tswindow)) {
    gtk_entry_set_text(GTK_ENTRY(w_current->tsentry), generic_textstring);
    gtk_entry_select_region(GTK_ENTRY(w_current->tsentry),
			    0,strlen(generic_textstring));
    gtk_widget_show(w_current->tswindow);
    gtk_grab_add(w_current->tswindow);
  }
}

/*********** End of autonumber text dialog box *******/

/*********** Start of some Gtk utils  *******/

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void select_all_text_in_textview(GtkTextView *textview) 
{
  GtkTextBuffer *textbuffer;
  GtkTextIter start, end;
  GtkTextMark *mark;
	
  textbuffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(textview));
  gtk_text_buffer_get_bounds (textbuffer, &start, &end);
  gtk_text_buffer_place_cursor(textbuffer, &start);
  mark = gtk_text_buffer_get_selection_bound(textbuffer);
  gtk_text_buffer_move_mark(textbuffer, mark, &end);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int text_view_calculate_real_tab_width(GtkTextView *textview, int tab_size) 
{
  PangoLayout *layout;
  gchar *tab_string;
  gint counter = 0;
  gint tab_width = 0;

  if (tab_size == 0)
  return -1;

  tab_string = g_malloc (tab_size + 1);

  while (counter < tab_size) {
    tab_string [counter] = ' ';
    counter++;
  }

  tab_string [tab_size] = 0;

  layout = gtk_widget_create_pango_layout (
                                           GTK_WIDGET (textview), 
                                           tab_string);
  g_free (tab_string);

  if (layout != NULL) {
    pango_layout_get_pixel_size (layout, &tab_width, NULL);
    g_object_unref (G_OBJECT (layout));
  } else
  tab_width = -1;

  return tab_width;

}

/*********** End of some Gtk utils *******/

/*********** Start of major symbol changed dialog box *******/

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void major_changed_dialog(TOPLEVEL* w_current)
{
  GtkWidget* dialog;
  char* refdes_string = NULL;
  char* tmp;

  if (w_current->major_changed_refdes) {

    GList* current = w_current->major_changed_refdes;
    while (current)
    {
      char *value = (char*) current->data;

      if (!refdes_string)
      {
        refdes_string = g_strdup (value);
      } else {
        tmp = g_strconcat (refdes_string, "\n", value, NULL);
        g_free(refdes_string);
        refdes_string = tmp;
      }
      
      current = current->next;
    }

    tmp = g_strconcat (refdes_string, 
                       "\n\nBe sure to verify each of these symbols!", 
                       NULL);
    g_free(refdes_string);
    refdes_string = tmp;

    dialog = gtk_message_dialog_new ((GtkWindow*) w_current->main_window,
                                     GTK_DIALOG_DESTROY_WITH_PARENT,
                                     GTK_MESSAGE_ERROR,
                                     GTK_BUTTONS_CLOSE,
                        "Major symbol changes detected in refdes:\n\n%s\n",
                                     refdes_string);

    gtk_widget_show(dialog);

    g_signal_connect_swapped (dialog, "response",
                              G_CALLBACK (gtk_widget_destroy),
                              dialog);

    if (refdes_string) g_free(refdes_string);
  }
}

/*********** End of major symbol changed dialog box *******/

/***************** Start of Close Confirmation dialog box ************/

#define TYPE_CLOSE_CONFIRMATION_DIALOG            (close_confirmation_dialog_get_type ())
#define CLOSE_CONFIRMATION_DIALOG(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_CLOSE_CONFIRMATION_DIALOG, CloseConfirmationDialog))
#define CLOSE_CONFIRMATION_DIALOG_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), TYPE_CLOSE_CONFIRMATION_DIALOG, CloseConfirmationDialogClass))
#define IS_CLOSE_CONFIRMATION_DIALOG(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_CLOSE_CONFIRMATION_DIALOG))
#define IS_CLOSE_CONFIRMATION_DIALOG_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), TYPE_CLOSE_CONFIRMATION_DIALOG))
#define CLOSE_CONFIRMATION_DIALOG_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj),TYPE_CLOSE_CONFIRMATION_DIALOG, CloseConfirmationDialogClass))


typedef struct _CloseConfirmationDialog      CloseConfirmationDialog;
typedef struct _CloseConfirmationDialogClass CloseConfirmationDialogClass;

struct _CloseConfirmationDialog 
{
	GtkDialog parent;

  GtkListStore *store_unsaved_pages;
  
};

struct _CloseConfirmationDialogClass 
{
	GtkDialogClass parent_class;
  
};


enum {
  PROP_UNSAVED_PAGE=1,
  PROP_UNSAVED_PAGES,
  PROP_SELECTED_PAGES,
};

enum {
  COLUMN_SAVE,
  COLUMN_PAGE,
  NUM_COLUMNS
};


static gpointer close_confirmation_dialog_parent_class = NULL;


static void close_confirmation_dialog_class_init (CloseConfirmationDialogClass *klass);
static void close_confirmation_dialog_init (CloseConfirmationDialog *self);
static void close_confirmation_dialog_set_property (GObject      *object,
                                                    guint         property_id,
                                                    const GValue *value,
                                                    GParamSpec   *pspec);
static void close_confirmation_dialog_get_property (GObject      *object,
                                                    guint         property_id,
                                                    GValue       *value,
                                                    GParamSpec   *pspec);
static GObject* close_confirmation_dialog_constructor (GType type,
                                                       guint n_construct_properties,
                                                       GObjectConstructParam *construct_params);

GList *close_confirmation_dialog_get_selected_pages (CloseConfirmationDialog *dialog);



GType
close_confirmation_dialog_get_type ()
{
  static GType close_confirmation_dialog_type = 0;
  
  if (!close_confirmation_dialog_type) {
    static const GTypeInfo close_confirmation_dialog_info = {
      sizeof(CloseConfirmationDialogClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) close_confirmation_dialog_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof(CloseConfirmationDialog),
      0,    /* n_preallocs */
      (GInstanceInitFunc) close_confirmation_dialog_init,
    };
                
    close_confirmation_dialog_type =
      g_type_register_static (GTK_TYPE_DIALOG,
                              "CloseConfirmationDialog",
                              &close_confirmation_dialog_info, 0);
  }
  
  return close_confirmation_dialog_type;
}

static void
close_confirmation_dialog_class_init (CloseConfirmationDialogClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

  close_confirmation_dialog_parent_class = g_type_class_peek_parent (klass);
 
  gobject_class->constructor  = close_confirmation_dialog_constructor;
  gobject_class->set_property = close_confirmation_dialog_set_property;
  gobject_class->get_property = close_confirmation_dialog_get_property;

  g_object_class_install_property (
    gobject_class, PROP_UNSAVED_PAGE,
    g_param_spec_pointer ("unsaved-page",
                          "",
                          "",
                          G_PARAM_CONSTRUCT_ONLY | G_PARAM_WRITABLE));
  g_object_class_install_property (
    gobject_class, PROP_UNSAVED_PAGES,
    g_param_spec_pointer ("unsaved-pages",
                          "",
                          "",
                          G_PARAM_CONSTRUCT_ONLY | G_PARAM_WRITABLE));
  g_object_class_install_property (
    gobject_class, PROP_SELECTED_PAGES,
    g_param_spec_pointer ("selected-pages",
                          "",
                          "",
                          G_PARAM_READABLE));

}

static void
close_confirmation_dialog_init (CloseConfirmationDialog *self)
{
  /* create model for treeview and populate */
  self->store_unsaved_pages = gtk_list_store_new (NUM_COLUMNS,
                                                  G_TYPE_BOOLEAN,  /* save? */
                                                  G_TYPE_POINTER); /* page */
  
}

/*! \brief Returns the number of pages in the model.
 *  \par Function Description
 *  This function determines the number of pages with unsaved changes
 *  from the model.
 *
 *  \param in model The tree model.
 *  \returns The number of pages with unsaved changes.
 */
static gint
count_pages (GtkTreeModel *model)
{
  GtkTreeIter iter;
  gint n_pages;
  
  gtk_tree_model_get_iter_first (model, &iter);
  for (n_pages = 1;
       gtk_tree_model_iter_next (model, &iter);
       n_pages++);

  return n_pages;
}

/*! \brief Returns the name to use for the given page in the model.
 *  \par Function Description
 *  This function determines the text to be used to identify a
 *  specific page from the model of pages with unsaved changes.
 *
 *  If <B>piter</B> is NULL, the name for the first page of the model
 *  is returned. Otherwise, it returns the name for the page defined
 *  by the pointed iterator.
 *
 *  The returned value must be freed by caller.
 *
 *  \param in model The tree model.
 *  \param in piter A pointer on a GtkTreeIter of model or NULL.
 *  \returns The name for the page.
 */
static gchar*
get_page_name (GtkTreeModel *model, GtkTreeIter *piter)
{
  GtkTreeIter iter;
  PAGE *page;
  
  g_return_val_if_fail (GTK_IS_TREE_MODEL (model), NULL);

  if (piter == NULL) {
    gtk_tree_model_get_iter_first (model, &iter);
  } else {
    iter = *piter;
  }

  gtk_tree_model_get (model, &iter,
                      COLUMN_PAGE, &page,
                      -1);
  g_assert (page != NULL && page->page_filename != NULL);
  return g_path_get_basename (page->page_filename);
}

/*! \brief Sets the contents of the name cell in the treeview of dialog.
 *  \par Function Description
 *  This functions sets the cell of the treeview with the short name
 *  of the page obtained with <B>get_page_name()</B>.
 *
 *  \param in tree_column A GtkTreeColumn.
 *  \param in cell        The GtkCellRenderer that is being rendered by
 *                        tree_column.
 *  \param in tree_model  The GtkTreeModel being rendered.
 *  \param in iter        A GtkTreeIter of the current row rendered.
 *  \param in data        .
 */
static void
close_confirmation_dialog_set_page_name (GtkTreeViewColumn *tree_column,
                                         GtkCellRenderer   *cell,
                                         GtkTreeModel      *tree_model,
                                         GtkTreeIter       *iter,
                                         gpointer           data)
{
  gchar *page_name;

  page_name = get_page_name (tree_model, iter);
  g_object_set (cell,
                "text", page_name,
                NULL);
  g_free (page_name);
  
}

/*! \brief Callback function for the toggled signal of check box in treeview.
 *  \par Function Description
 *  This functions changes the value of the save column in the model
 *  for the affected row when user toggles the check box in the
 *  treeview.
 *
 *  \param in cell_renderer The GtkCellRendererToggle.
 *  \param in path          The GtkTreePath to the concerned row in model.
 *  \param in user          The dialog as user data.
 */
static void
close_confirmation_dialog_callback_renderer_toggled (GtkCellRendererToggle *cell_renderer,
                                                     gchar                 *path,
                                                     gpointer               user_data)
{
  CloseConfirmationDialog *dialog = CLOSE_CONFIRMATION_DIALOG (user_data);
  GtkTreeModel *model;
	GtkTreeIter iter;
  gboolean save;

  model = GTK_TREE_MODEL (dialog->store_unsaved_pages);

  gtk_tree_model_get_iter_from_string (model, &iter, path);
	gtk_tree_model_get (model, &iter,
                      COLUMN_SAVE, &save,
                      -1);
	gtk_list_store_set (GTK_LIST_STORE (model), &iter,
                      COLUMN_SAVE, (save != TRUE),
                      -1);
  
}

/*! \brief Adds a treeview to confirmation dialog for selecting of pages.
 *  \par Function Description
 *  This function adds a treeview and caption to display the content
 *  of the dialog model of pages with unsaved changes.
 *
 *  The treeview displays the page names with check boxes.
 *
 *  \param in dialog The dialog.
 *  \returns A pointer on the GtkVBox to add to dialog.
 */
static GtkWidget*
close_confirmation_dialog_build_page_list (CloseConfirmationDialog *dialog)
{
  GtkWidget *vbox, *scrolled_window, *treeview, *label;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  const gchar *text;

  /* place the treeview and its caption into their own box */
  vbox = GTK_WIDGET (g_object_new (GTK_TYPE_VBOX,
                                   /* GtkBox */
                                   "homogeneous", FALSE,
                                   "spacing",     8,
                                   NULL));

  /* the list of pages with changes */
  /*  - scrolled window as container for the treeview first */
  scrolled_window = GTK_WIDGET (g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                                              /* GtkScrolledWindow */
                                              "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                                              "vscrollbar-policy", GTK_POLICY_AUTOMATIC,
                                              "shadow-type",       GTK_SHADOW_IN,
                                              NULL));
  /*  - then the treeview */
  /* create model for treeview and populate */
  treeview = GTK_WIDGET (g_object_new (GTK_TYPE_TREE_VIEW,
                                       /* GtkTreeView */
                                       "enable-search",   FALSE,
                                       "headers-visible", FALSE,
                                       "model",           dialog->store_unsaved_pages,
                                       NULL));
  renderer = gtk_cell_renderer_toggle_new ();
  g_signal_connect (renderer, "toggled",
                    G_CALLBACK (
                      close_confirmation_dialog_callback_renderer_toggled),
                    dialog);
  column   = gtk_tree_view_column_new_with_attributes ("Save?",
                                                       renderer,
                                                       "active", COLUMN_SAVE,
                                                       NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);
  
  renderer = gtk_cell_renderer_text_new ();
  column = GTK_TREE_VIEW_COLUMN (
    g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                  /* GtkTreeViewColumn */
                  "title", _("Name"),
                  NULL));
  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_set_cell_data_func (column, renderer,
                                           close_confirmation_dialog_set_page_name,
                                           NULL, NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);
  
	gtk_container_add (GTK_CONTAINER (scrolled_window), treeview);
  
	gtk_box_pack_end (GTK_BOX (vbox), scrolled_window,
                    TRUE, TRUE, 0);

  /* the caption label above the list of pages */
  label = GTK_WIDGET (g_object_new (GTK_TYPE_LABEL,
                                    /* GtkMisc */
                                    "xalign",          0.0,
                                    "yalign",          0.0,
                                    /* GtkLabel */
                                    "wrap",            TRUE,
                                    "mnemonic-widget", treeview,
                                    NULL));
  text = _("S_elect the schematics you want to save:");
  gtk_label_set_text_with_mnemonic (GTK_LABEL (label), text);
  gtk_label_set_mnemonic_widget (GTK_LABEL (label), treeview);
	gtk_box_pack_start (GTK_BOX (vbox), label,
                      FALSE, FALSE, 0);
  
  return vbox;
}

static GObject*
close_confirmation_dialog_constructor (GType type,
                                       guint n_construct_properties,
                                       GObjectConstructParam *construct_params)
{
  GObject *object;
  CloseConfirmationDialog *dialog;
  GtkWidget *hbox, *image, *vbox, *label;
  GtkTreeIter iter;
  gboolean ret, single_page;
  gchar *tmp, *str;

  /* chain up to constructor of parent class */
  object =
    G_OBJECT_CLASS (close_confirmation_dialog_parent_class)->constructor (
      type,
      n_construct_properties,
      construct_params);
  dialog = CLOSE_CONFIRMATION_DIALOG (object);

  g_object_set (dialog,
                /* GtkDialog */
                "has-separator",     FALSE,
                /* GtkWindow */
                "resizable",         FALSE,
                "skip-taskbar-hint", TRUE,
                /* GtkContainer */
                "border-width",      5,
                NULL);
  g_object_set (GTK_DIALOG (dialog)->vbox,
                /* GtkBox */
                "spacing", 14,
                NULL);
  g_object_set (GTK_DIALOG (dialog)->action_area,
                /* GtkBox */
                "spacing",      6,
                /* GtkContainer */
                "border-width", 5,
                NULL);
                
  /* check if there is one or more than one page with changes */
  ret = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (
                                         dialog->store_unsaved_pages),
                                       &iter);
  g_assert (ret);
  single_page = !gtk_tree_model_iter_next (GTK_TREE_MODEL (
                                             dialog->store_unsaved_pages),
                                           &iter);

  /* here starts the layout of the dialog */
	hbox = GTK_WIDGET (g_object_new (GTK_TYPE_HBOX,
                                   /* GtkContainer */
                                   "border-width", 5,
                                   /* GtkBox */
                                   "homogeneous",  FALSE,
                                   "spacing",      12,
                                   NULL));

  /* warning image */
	image = g_object_new (GTK_TYPE_IMAGE,
                        /* GtkMisc */
                        "xalign",    0.5,
                        "yalign",    0.0,
                        /* GtkImage */
                        "stock",     GTK_STOCK_DIALOG_WARNING,
                        "icon-size", GTK_ICON_SIZE_DIALOG,
                        NULL);
	gtk_box_pack_start (GTK_BOX (hbox), image,
                      FALSE, FALSE, 0);

  /* vertical box on the right hand side of the dialog */
  vbox = GTK_WIDGET (g_object_new (GTK_TYPE_VBOX,
                                   /* GtkBox */
                                   "homogeneous", FALSE,
                                   "spacing",     12,
                                   NULL));

  /* primary label */
  if (single_page) {
    /* single page */
    gchar *page_name;
    
    page_name = get_page_name (GTK_TREE_MODEL (dialog->store_unsaved_pages),
                               NULL);
    tmp = g_strdup_printf (
      _("Save the changes to schematic \"%s\" before closing?"),
      page_name);
    g_free (page_name);
  } else {
    /* multi page */
    tmp = g_strdup_printf (
      _("There is %d schematics with unsaved changes. "
        "Save changes before closing?"),
      count_pages (GTK_TREE_MODEL (dialog->store_unsaved_pages)));
  }
  str = g_strconcat ("<big><b>", tmp, "</b></big>", NULL);
  g_free (tmp);
  label = GTK_WIDGET (g_object_new (GTK_TYPE_LABEL,
                                    /* GtkMisc */
                                    "xalign",     0.0,
                                    "yalign",     0.0,
                                    "selectable", TRUE,
                                    /* GtkLabel */
                                    "wrap",       TRUE,
                                    "use-markup", TRUE,
                                    "label",      str,
                                    NULL));
  g_free (str);
	gtk_box_pack_start (GTK_BOX (vbox), label,
                      FALSE, FALSE, 0);

  if (!single_page) {
    /* more than one page with changes, display each page and offer */
    /* the opportunity to save them before exiting */
    gtk_box_pack_start (GTK_BOX (vbox),
                        close_confirmation_dialog_build_page_list (dialog),
                        FALSE, FALSE, 0);
  }
  
  /* secondary label */
  str = _("If you don't save, all your changes will be permanently lost.");
  label = GTK_WIDGET (g_object_new (GTK_TYPE_LABEL,
                                    /* GtkMisc */
                                    "xalign",     0.0,
                                    "yalign",     0.0,
                                    "selectable", TRUE,
                                    /* GtkLabel */
                                    "wrap",       TRUE,
                                    "label",      str,
                                    NULL));
	gtk_box_pack_start (GTK_BOX (vbox), label,
                      FALSE, FALSE, 0);

  
	gtk_box_pack_start (GTK_BOX (hbox), vbox,
                      FALSE, FALSE, 0);


  /* add buttons to dialog action area */
  gtk_dialog_add_buttons (GTK_DIALOG (dialog),
                          _("_Close without saving"), GTK_RESPONSE_NO,
                          GTK_STOCK_CANCEL,           GTK_RESPONSE_CANCEL,
                          GTK_STOCK_SAVE,             GTK_RESPONSE_YES,
                          NULL);

  /* all done, let's show the contents of the dialog */
	gtk_widget_show_all (hbox);

	gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), hbox, 
	                    FALSE, FALSE, 0);
  
  return object;
}

static void
close_confirmation_dialog_set_property (GObject      *object,
                                        guint         property_id,
                                        const GValue *value,
                                        GParamSpec   *pspec)
{
  CloseConfirmationDialog *dialog = CLOSE_CONFIRMATION_DIALOG (object);
  GtkTreeIter iter;
  gpointer data;
  GList *p_current;

  switch(property_id) {
    case PROP_UNSAVED_PAGE:
      data = g_value_get_pointer (value);
      if (data != NULL) {
        /* add single page to model */
        gtk_list_store_append (dialog->store_unsaved_pages,
                               &iter);
        gtk_list_store_set (dialog->store_unsaved_pages,
                            &iter,
                            COLUMN_SAVE, TRUE,
                            COLUMN_PAGE, data,
                            -1);
      }
      break;
      
    case PROP_UNSAVED_PAGES:
      data = g_value_get_pointer (value);
      /* add set of pages to model */
      for (p_current = (GList*)data;
           p_current != NULL;
           p_current = g_list_next (p_current)) {
        gtk_list_store_append (dialog->store_unsaved_pages,
                               &iter);
        gtk_list_store_set (dialog->store_unsaved_pages,
                            &iter,
                            COLUMN_SAVE, TRUE,
                            COLUMN_PAGE, p_current->data,
                            -1);
      }
      break;
      
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
  
}

static void
close_confirmation_dialog_get_property (GObject    *object,
                                        guint       property_id,
                                        GValue     *value,
                                        GParamSpec *pspec)
{
  CloseConfirmationDialog *dialog = CLOSE_CONFIRMATION_DIALOG (object);

  switch(property_id) {
    case PROP_SELECTED_PAGES:
      g_value_set_pointer (
        value,
        close_confirmation_dialog_get_selected_pages (dialog));
      break;
      
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
  
}

/*! \brief Helps building a list of selected page to save.
 *  \par Function Description
 *  This is the <B>GtkTreeModelForeachFunc</B> for function
 *  <B>close_confirmation_dialog_get_selected_pages()</B>.
 *
 *  It builds from the tree model a list of PAGEs for which a save
 *  action has been requested. Each selected page is appended to the
 *  GList pointed by <B>data</B>
 *
 *  \param in model The tree model.
 *  \param in path  .
 *  \param in iter  .
 *  \param in data  A pointer on a GList* to fill.
 *  \returns FALSE to continue walking the tree.
 */
static gboolean
get_selected_pages (GtkTreeModel *model,
                    GtkTreePath  *path,
                    GtkTreeIter  *iter,
                    gpointer     data)
{
  PAGE *page;
  gboolean save;
  
  gtk_tree_model_get (model, iter,
                      COLUMN_SAVE, &save,
                      COLUMN_PAGE, &page,
                      -1);
  if (save) {
    g_assert (page != NULL);
    *(GList**)data = g_list_append (*(GList**)data, page);
  }

  return FALSE;
}

/*! \brief Returns a list of the selected pages with changes to save.
 *  \par Function Description
 *  This function returns the pages that the user has selected in the
 *  confirmation dialog.
 *
 *  The returned list must be freed.
 *
 *  \param in dialog The dialog.
 *  \returns A GList of selected PAGE* in dialog.
 */
GList*
close_confirmation_dialog_get_selected_pages (CloseConfirmationDialog *dialog)
{
  GList *selected = NULL;

  gtk_tree_model_foreach (GTK_TREE_MODEL (dialog->store_unsaved_pages),
                          (GtkTreeModelForeachFunc)get_selected_pages,
                          &selected);
  
  return selected;
}


/*! \brief Asks for confirmation before closing a changed page.
 *  \par Function Description
 *  This function asks the user to confirm its closing order for
 *  page <B>page</B> while it still has unsaved changes.
 *
 *  It displays a message dialog inviting the user to cancel the
 *  closing, or to discard the changes or to save the changes to a
 *  file.
 *
 *  \param in toplevel The toplevel environment.
 *  \param in page     The page to close.
 */
void
x_dialog_close_changed_page (TOPLEVEL *toplevel, PAGE *page)
{
	GtkWidget *dialog;

  g_return_if_fail (page != NULL && page->CHANGED);
  
  dialog = GTK_WIDGET (g_object_new (TYPE_CLOSE_CONFIRMATION_DIALOG,
                                     "unsaved-page", page,
                                     NULL));
  switch (gtk_dialog_run (GTK_DIALOG (dialog))) {
      case GTK_RESPONSE_NO:
        /* action selected: close without saving */
        /* close the page, discard changes */
        x_window_close_page (toplevel, page);
        break;
        
      case GTK_RESPONSE_CANCEL:
        /* action selected: cancel */
        /* nothing to do */
        break;

      case GTK_RESPONSE_YES:
        /* action selected: save */
        /* prompts user for the filename and ultimate confirmation */
        s_page_goto (toplevel, page);
        x_fileselect_save (toplevel);
        /* has the page been really saved? */
        if (!page->CHANGED) {
          x_window_close_page (toplevel, page);
        }
        /* no, user has cancelled the save and page has changes */
        /* do not close page */
        break;
        
      default:
        g_assert_not_reached ();
  }
  gtk_widget_destroy (dialog);

}

/*! \brief Asks for confirmation before closing a window.
 *  \par Function Description
 *  This function asks the user to confirm its closing order for
 *  the given window.
 *
 *  The user is given the possibility to save the pages that currently
 *  have unsaved changes, if any.
 *
 *  It returns TRUE if the user really accepts the close of the
 *  window. Otherwise the user has somehow cancelled and the window
 *  must not be closed.
 *  
 *  \param in toplevel The toplevel environment.
 *  \returns TRUE if the window can be closed, FALSE otherwise.
 */
gboolean
x_dialog_close_window (TOPLEVEL *toplevel)
{
	GtkWidget *dialog;
  PAGE *p_current;
  GList *unsaved_pages, *p_unsaved;
  gboolean ret = FALSE;

  /* build a list of unsaved pages */
  g_assert (toplevel->page_head != NULL &&
            toplevel->page_head->next != NULL);
  for (p_current = toplevel->page_head->next, unsaved_pages = NULL;
       p_current != NULL;
       p_current = p_current->next) {
    if (p_current->CHANGED) {
      unsaved_pages = g_list_append (unsaved_pages, (gpointer)p_current);
    }
  }

  if (unsaved_pages == NULL) {
    /* no page with unsaved changes, close window */
    return TRUE;
  }

  dialog = GTK_WIDGET (g_object_new (TYPE_CLOSE_CONFIRMATION_DIALOG,
                                     "unsaved-pages", unsaved_pages,
                                     NULL));
  g_list_free (unsaved_pages);
  switch (gtk_dialog_run (GTK_DIALOG (dialog))) {
      case GTK_RESPONSE_NO:
        /* action selected: close without saving */
        /* discard changes, ok to close window */
        ret = TRUE;
        break;
        
      case GTK_RESPONSE_CANCEL:
        /* action selected: cancel */
        /* do not close window */
        ret = FALSE;
        break;

      case GTK_RESPONSE_YES:
        /* action selected: save */
        /* prompts user for the filename and ultimate confirmation for */
        /* each selected page */
        g_object_get (dialog,
                      "selected-pages", &unsaved_pages,
                      NULL);
        for (p_unsaved = unsaved_pages, ret = TRUE;
             p_unsaved != NULL;
             p_unsaved = g_list_next (p_unsaved)) {
          p_current = (PAGE*)p_unsaved->data;
          
          s_page_goto (toplevel, p_current);
          x_fileselect_save (toplevel);
          /* if user cancelled previous, do not close window */
          ret &= !p_current->CHANGED;
        }
        g_list_free (unsaved_pages);
        break;
        
      default:
        g_assert_not_reached ();
  }
  gtk_widget_destroy (dialog);

  return ret;
}

/***************** End of Close Confirmation dialog box **************/

