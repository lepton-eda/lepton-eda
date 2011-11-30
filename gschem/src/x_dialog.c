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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */
/*! \todo STILL NEED to clean up line lengths in aa and tr */
#include <config.h>
#include <version.h>
#include <missing.h>

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define GLADE_HOOKUP_OBJECT(component,widget,name) \
  g_object_set_data_full (G_OBJECT (component), name, \
    gtk_widget_ref (widget), (GDestroyNotify) gtk_widget_unref)

static GtkWidget* create_menu_linetype (GSCHEM_TOPLEVEL *w_current);
static gint line_type_dialog_linetype_change (GtkWidget *w, gpointer data);
static void line_type_dialog_ok (GtkWidget *w, gpointer data);

static GtkWidget* create_menu_filltype (GSCHEM_TOPLEVEL *w_current);
static gint fill_type_dialog_filltype_change(GtkWidget *w, gpointer data);
static void fill_type_dialog_ok(GtkWidget *w, gpointer data);


struct line_type_data {
  GtkWidget *dialog;
  GtkWidget *width_entry;
  GtkWidget *line_type;
  GtkWidget *length_entry;
  GtkWidget *space_entry;

  GSCHEM_TOPLEVEL *w_current;
};

struct fill_type_data {
  GtkWidget *dialog;
  GtkWidget *fill_type;
  GtkWidget *width_entry;
  GtkWidget *angle1_entry;
  GtkWidget *pitch1_entry;
  GtkWidget *angle2_entry;
  GtkWidget *pitch2_entry;

  GSCHEM_TOPLEVEL *w_current;
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

/* TODO: This string is used by the dialogs: show_text, find_text and hide_text
 * I think it should be removed. (Werner Hoch)
 */
char generic_textstring[256] = "refdes=R";

/***************** Start of Text Input dialog box *********************/

/*! \brief worker function for the text entry dialog
 *  \par Function Description
 *  This function applies the text from the text entry dialog.
 */
void text_input_dialog_apply(GtkWidget *w, GSCHEM_TOPLEVEL *w_current)
{
  gchar *string = NULL;
  gchar *tmp = NULL;
  GtkWidget *tientry;
  GtkTextBuffer *textbuffer;
  GtkTextIter start, end;

  tientry = gtk_object_get_data(GTK_OBJECT(w_current->tiwindow),"tientry");

  textbuffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(tientry));
  gtk_text_buffer_get_bounds (textbuffer, &start, &end);
  string =  gtk_text_iter_get_text (&start, &end);

  if (string[0] == '\0' )
    return;

  switch(w_current->text_caps) {
    case(LOWER):
      tmp = g_utf8_strdown (string, -1);
      break;

    case(UPPER):
      tmp = g_utf8_strup (string, -1);
      break;

    case(BOTH):
    default:
      /* do nothing */
      break;
  }

  /* select the text, so you can continue immediatly writing the next text */
  select_all_text_in_textview(GTK_TEXT_VIEW(tientry));
  gtk_widget_grab_focus(tientry);

  w_current->toplevel->page_current->CHANGED=1;

  o_text_prepare_place (w_current, tmp == NULL ? string : tmp);
  g_free (string);
  g_free (tmp);
}

/*! \brief response function for the text entry dialog
 *  \par Function Description
 *  Callback function for the text entry dialog.
 */
void text_input_dialog_response(GtkWidget * widget, gint response, GSCHEM_TOPLEVEL *w_current)
{
  switch(response) {
  case GTK_RESPONSE_ACCEPT:
    text_input_dialog_apply(widget, w_current);
    break;
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    i_set_state(w_current, SELECT);
    i_update_toolbar(w_current);
    gtk_widget_destroy(w_current->tiwindow);
    w_current->tiwindow=NULL;
    break;
  default:
    printf("text_edit_dialog_response(): strange signal %d\n", response);
  }
}


/*! \brief create or present the text entry dialog
 *  \par Function Description
 *  This function creates or raises the modal text entry dialog
 */
void text_input_dialog (GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *label = NULL;
  GtkWidget *tientry = NULL;
  GtkWidget *vbox;
  GtkWidget *viewport1 = NULL;
  GtkWidget *scrolled_window = NULL;
  PangoTabArray *tab_array;
  int real_tab_width;

  if (!w_current->tiwindow) { /* dialog not created yet */
    w_current->tiwindow = gschem_dialog_new_with_buttons(_("Text Entry..."),
                                                         GTK_WINDOW(w_current->main_window),
                                                         0, /* NON_MODAL */
                                                         "text-entry", w_current,
                                                         GTK_STOCK_CLOSE,
                                                         GTK_RESPONSE_REJECT,
                                                         GTK_STOCK_APPLY,
                                                         GTK_RESPONSE_ACCEPT,
                                                         NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->tiwindow),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_window_position(GTK_WINDOW (w_current->tiwindow),
                        GTK_WIN_POS_NONE);

    g_signal_connect (G_OBJECT (w_current->tiwindow), "response",
                      G_CALLBACK (text_input_dialog_response),
                      w_current);

    gtk_dialog_set_default_response(GTK_DIALOG(w_current->tiwindow),
                                    GTK_RESPONSE_ACCEPT);

    gtk_container_border_width(GTK_CONTAINER (w_current->tiwindow),
                               DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(w_current->tiwindow)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox),DIALOG_V_SPACING);

    label = gtk_label_new (_("Enter text, click apply,\n"
                             "move cursor into window, click to place text.\n"
                             "Middle button to rotate while placing."));
    gtk_misc_set_alignment(GTK_MISC(label),0,0);
    gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);

    viewport1 = gtk_viewport_new (NULL, NULL);
    gtk_widget_show (viewport1);

    scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
                                   GTK_POLICY_AUTOMATIC,
                                   GTK_POLICY_AUTOMATIC);
    gtk_container_add (GTK_CONTAINER (viewport1), scrolled_window);
    gtk_box_pack_start( GTK_BOX(vbox), viewport1, TRUE, TRUE, 0);

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

    gtk_object_set_data(GTK_OBJECT(w_current->tiwindow),
                        "tientry",tientry);

    gtk_widget_show_all (w_current->tiwindow);
  }
  else { /* dialog already created */
    gtk_window_present (GTK_WINDOW(w_current->tiwindow));
  }

  /* always select the text in the entry */
  tientry = gtk_object_get_data(GTK_OBJECT(w_current->tiwindow),"tientry");
  select_all_text_in_textview(GTK_TEXT_VIEW(tientry));
  gtk_widget_grab_focus(tientry);
}

/***************** End of Text Input dialog box ***********************/

/***************** Start of Text Edit dialog box **********************/
/*! \brief CAllback for a text aligment change
 *  \par Function Description
 *  This function stores a change of the text alignment in the
 *  <b>GSCHEM_TOPLEVEL</b> struct.
 *  \todo Remove that function. Only the OK-Button should set any
 *  properties in the GSCHEM_TOPLEVEL struct.
 */
gint change_alignment(GtkComboBox *w, GSCHEM_TOPLEVEL *w_current)
{
  GtkTreeIter iter;
  GtkTreeModel *model;
  gint value;
  if( gtk_combo_box_get_active_iter(w, &iter))
  {
    model = gtk_combo_box_get_model(w);
    gtk_tree_model_get(model, &iter, 1, &value, -1);
    w_current->text_alignment = value;
  }

  /*w_current->page_current->CHANGED=1; I don't think this belongs */
  /* o_undo_savestate(w_current, UNDO_ALL); I don't think this belongs */

  return 0;
}

/*! \brief Create the alignment combo box list store for the text
*   property dialog
 *  \par Function Description
 *  This function creates a GtkListStore with nine different alignment
 *  entries.
 */
static GtkListStore *create_menu_alignment (GSCHEM_TOPLEVEL *w_current)
{
  GtkListStore *store;
  GtkTreeIter   iter;

  store = gtk_list_store_new(2, G_TYPE_STRING, G_TYPE_INT);

  gtk_list_store_append(store, &iter);
  gtk_list_store_set(store, &iter, 0, _("Upper Left"), -1);
  gtk_list_store_set(store, &iter, 1, 2, -1);
  gtk_list_store_append(store, &iter);
  gtk_list_store_set(store, &iter, 0, _("Upper Middle"), -1);
  gtk_list_store_set(store, &iter, 1, 5, -1);
  gtk_list_store_append( store, &iter);
  gtk_list_store_set(store, &iter, 0, _("Upper Right"), -1);
  gtk_list_store_set(store, &iter, 1, 8, -1);

  gtk_list_store_append(store, &iter);
  gtk_list_store_set(store, &iter, 0, _("Middle Left"), -1);
  gtk_list_store_set(store, &iter, 1, 1, -1);
  gtk_list_store_append(store, &iter);
  gtk_list_store_set(store, &iter, 0, _("Middle Middle"), -1);
  gtk_list_store_set(store, &iter, 1, 4, -1);
  gtk_list_store_append(store, &iter);
  gtk_list_store_set(store, &iter, 0, _("Middle Right"), -1);
  gtk_list_store_set(store, &iter, 1, 7, -1);

  gtk_list_store_append(store, &iter);
  gtk_list_store_set(store, &iter, 0, _("Lower Left"), -1);
  gtk_list_store_set(store, &iter, 1, 0, -1);
  gtk_list_store_append(store, &iter);
  gtk_list_store_set(store, &iter, 0, _("Lower Middle"), -1);
  gtk_list_store_set(store, &iter, 1, 3, -1);
  gtk_list_store_append(store, &iter);
  gtk_list_store_set(store, &iter, 0, _("Lower Right"), -1);
  gtk_list_store_set(store, &iter, 1, 6, -1);

  return store;
}

/* we reuse the color menu so we need to declare it */
static GtkWidget *create_color_menu(GSCHEM_TOPLEVEL * w_current);

/*! \brief Apply the settings from the text property dialog
 *  \par Function Description
 *  This function applies the user settings to the selected text objects
 *  and closes the dialog
 */
void text_edit_dialog_ok(GtkWidget *w, GSCHEM_TOPLEVEL *w_current)
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

  num_selected = g_list_length( geda_list_get_glist( w_current->toplevel->page_current->selection_list ));

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
}

/*! \brief Response function for the text property dialog
 *  \par Function Description
 *  This function receives the user response of the text property dialog.
 *  The response is either <b>OK</b>, <b>Cancel</b> or delete.
 *
 */
void text_edit_dialog_response(GtkWidget * widget, gint response, GSCHEM_TOPLEVEL *w_current)
{
  switch(response) {
  case GTK_RESPONSE_ACCEPT:
    text_edit_dialog_ok(widget, w_current);
    break;
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    /* void */
    break;
  default:
    printf("text_edit_dialog_response(): strange signal %d\n", response);
  }
  /* clean up */
  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);
  gtk_widget_destroy(w_current->tewindow);
  w_current->tewindow = NULL;
}

/*! \brief Create the edit text properties dialog
 *  \par Function Description
 *  This Function creates the dialog to edit text properties.
 *  \todo Check why there's no color in the calling parameters
 *  \todo If more than one text element is selected, add an unchanged option
 */
void text_edit_dialog (GSCHEM_TOPLEVEL *w_current, const char *string, int text_size,
                       int text_alignment)
{
  GtkWidget *label;
  GtkWidget *table;
  GtkWidget *vbox;
  GtkWidget *optionmenu;
  GtkWidget *combobox;
  GtkListStore *align_menu_model;
  GtkCellRenderer *cell;
  GtkWidget *viewport1;
  GtkWidget *textentry;
  GtkWidget *sizeentry;
  GtkWidget *alignment;
  GtkWidget *scrolled_window;
  GtkTextBuffer *textbuffer;
  char *text_size_string;
  int num_selected;
  /* Lookup table for quickly translating between alignment values and the
     combo box list indices, index is alignment value, value is list index */
  static int alignment_lookup[] = {6, 3, 0, 7, 4, 1, 8, 5, 2};

  if (!w_current->tewindow) {
    w_current->tewindow = gschem_dialog_new_with_buttons(_("Edit Text Properties"),
                                                         GTK_WINDOW(w_current->main_window),
                                                         GTK_DIALOG_MODAL,
                                                         "text-edit", w_current,
                                                         GTK_STOCK_CANCEL,
                                                         GTK_RESPONSE_REJECT,
                                                         GTK_STOCK_OK,
                                                         GTK_RESPONSE_ACCEPT,
                                                         NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->tewindow),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_dialog_set_default_response(GTK_DIALOG(w_current->tewindow),
                                    GTK_RESPONSE_ACCEPT);

    g_signal_connect (G_OBJECT (w_current->tewindow), "response",
                      G_CALLBACK (text_edit_dialog_response),
                      w_current);

    gtk_window_position(GTK_WINDOW (w_current->tewindow),
                        GTK_WIN_POS_MOUSE);


    vbox = GTK_DIALOG(w_current->tewindow)->vbox;
    gtk_container_set_border_width(GTK_CONTAINER(w_current->tewindow),DIALOG_BORDER_SPACING);
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    /* add a text box if only one object is selected */
    num_selected = g_list_length( geda_list_get_glist( w_current->toplevel->page_current->selection_list ));

    if (num_selected == 1) {
      label = gtk_label_new (_("<b>Text Content</b>"));
      gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
      gtk_misc_set_alignment(GTK_MISC(label),0,0);
      gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);

      alignment = gtk_alignment_new(0,0,1,1);
      gtk_alignment_set_padding(GTK_ALIGNMENT(alignment), 0, 0,
                                DIALOG_INDENTATION, 0);
      gtk_box_pack_start(GTK_BOX(vbox), alignment, TRUE, TRUE, 0);

      viewport1 = gtk_viewport_new (NULL, NULL);
      gtk_widget_set_size_request(GTK_WIDGET(viewport1),-1,75);

      scrolled_window = gtk_scrolled_window_new(NULL, NULL);
      gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window),
                                     GTK_POLICY_AUTOMATIC,
                                     GTK_POLICY_AUTOMATIC);
      gtk_container_add (GTK_CONTAINER (viewport1), scrolled_window);
      gtk_container_add( GTK_CONTAINER(alignment), viewport1);

      textentry = gtk_text_view_new();
      gtk_text_view_set_editable (GTK_TEXT_VIEW (textentry), TRUE);
      if (string != NULL) {
        textbuffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (textentry));
        gtk_text_buffer_set_text (GTK_TEXT_BUFFER (textbuffer), string, -1);
        select_all_text_in_textview (GTK_TEXT_VIEW (textentry));
      }

      /*! \bug FIXME: Set tab's width in the textview widget. */
      /* See first the code in text_input_dialog and get it working before adding it here. */

      gtk_container_add(GTK_CONTAINER(scrolled_window), textentry);
      gtk_widget_grab_focus(textentry);
      GLADE_HOOKUP_OBJECT(w_current->tewindow, textentry,"textentry");
    }

    label = gtk_label_new(_("<b>Text Properties</b>"));
    gtk_label_set_use_markup(GTK_LABEL(label), TRUE);
    gtk_misc_set_alignment(GTK_MISC(label),0,0);
    gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);

    alignment = gtk_alignment_new(0,0,1,1);
    gtk_alignment_set_padding(GTK_ALIGNMENT(alignment), 0, 0,
                              DIALOG_INDENTATION, 0);
    gtk_box_pack_start(GTK_BOX(vbox), alignment, FALSE, FALSE, 0);

    table = gtk_table_new (3, 2, FALSE);
    gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
    gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
    gtk_container_add(GTK_CONTAINER(alignment), table);

    label = gtk_label_new(_("Color:"));
    gtk_misc_set_alignment(GTK_MISC(label),0,0);
    gtk_table_attach(GTK_TABLE(table), label, 0,1,0,1, GTK_FILL,0,0,0);

    optionmenu = create_color_menu (w_current);
    gtk_table_attach_defaults(GTK_TABLE(table), optionmenu, 1,2,0,1);

    label = gtk_label_new(_("Size:"));
    gtk_misc_set_alignment(GTK_MISC(label),0,0);
    gtk_table_attach(GTK_TABLE(table), label, 0,1,1,2, GTK_FILL,0,0,0);

    sizeentry = gtk_entry_new_with_max_length (10);
    gtk_editable_select_region(GTK_EDITABLE (sizeentry), 0, -1);
    gtk_table_attach_defaults(GTK_TABLE(table), sizeentry, 1,2,1,2);
    gtk_entry_set_activates_default(GTK_ENTRY(sizeentry), TRUE);

    label = gtk_label_new(_("Alignment:"));
    gtk_misc_set_alignment(GTK_MISC(label),0,0);
    gtk_table_attach(GTK_TABLE(table), label, 0,1,2,3, GTK_FILL,0,0,0);

    align_menu_model = create_menu_alignment(w_current);
    combobox = gtk_combo_box_new_with_model(GTK_TREE_MODEL(align_menu_model));
    gtk_combo_box_set_wrap_width(GTK_COMBO_BOX(combobox), 3);
    cell = gtk_cell_renderer_text_new();
    gtk_cell_layout_pack_start(GTK_CELL_LAYOUT(combobox), cell, TRUE);
    gtk_cell_layout_set_attributes(GTK_CELL_LAYOUT(combobox), cell,
                                   "text", 0, NULL);
    gtk_combo_box_set_active(GTK_COMBO_BOX(combobox),
                             alignment_lookup[text_alignment]);
    w_current->text_alignment = text_alignment;
    g_object_unref (align_menu_model);
    gtk_table_attach_defaults(GTK_TABLE(table), combobox, 1,2,2,3);
    g_signal_connect(G_OBJECT(combobox), "changed",
                      G_CALLBACK(change_alignment), w_current);

    GLADE_HOOKUP_OBJECT(w_current->tewindow, sizeentry,"sizeentry");
    gtk_widget_show_all(w_current->tewindow);
  }

  else { /* dialog already there */
    gtk_window_present(GTK_WINDOW(w_current->tewindow));
  }

  text_size_string = g_strdup_printf("%d", text_size);
  sizeentry = g_object_get_data (G_OBJECT (w_current->tewindow), "sizeentry");
  gtk_entry_set_text(GTK_ENTRY(sizeentry),
                     text_size_string);
  g_free(text_size_string);
}

/***************** End of Text Edit dialog box ************************/

/***************** Start of Line Type/width dialog box ****************/

/*! \brief Create a line type menu for the line type dialog
 *  \par Function Description
 *  This function creates a GtkMenu with the different linetypes.
 */
static GtkWidget *create_menu_linetype (GSCHEM_TOPLEVEL *w_current)
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
                { N_("Phantom"), TYPE_PHANTOM },
                { N_("*unchanged*"), TYPE_ERASE } };
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

/*! \brief get the linetype data from selected objects
 *  \par Function Description
 *  Get linetype information over all selected objects.
 *  If a object property is different to the other objects, then
 *  return -2 in that variable.
 *  \param [in]   selection the selection list
 *  \param [out]  end       OBJECT_END type
 *  \param [out]  type      OBJECT_FILLING type
 *  \param [out]  width     line width
 *  \param [out]  length    length of each line
 *  \param [out]  space     space between points and lines
 *  \returns TRUE if linetype found, FALSE otherwise
 */
static gboolean selection_get_line_type(GList *selection,
                                        OBJECT_END *end, OBJECT_TYPE *type,
                                        gint *width, gint *length, gint *space)
{
  GList *iter;
  OBJECT *object;
  gboolean found = FALSE;
  OBJECT_END oend;
  OBJECT_TYPE otype;
  gint owidth, olength, ospace;

  for (iter = selection; iter != NULL; iter = g_list_next(iter)) {
    object = (OBJECT *) iter->data;
    if (! o_get_line_options(object, &oend, &otype, 
                             &owidth, &olength, &ospace))
      continue;

    if (found == FALSE) {  /* first object with filltype */
      found = TRUE;
      *end = oend;
      *type = otype;
      *width = owidth;
      *length = olength;
      *space = ospace;
    } else {
      /* indicate different values with the value -2 */
      if (*end != oend) *end = -2;
      if (*type != otype) *type = -2;
      if (*width != owidth) *width = -2;
      if (*length != olength) *length = -2;
      if (*space != ospace) *space = -2;
    }
  }

  return found;
}
 

/*! \brief set the linetype in the linetype dialog
 *  \par Function Description
 *  Set all widgets in the linetype dialog. Variables marked with the
 *  invalid value -2 are set to *unchanged*.
 *  \param [in]   line_type_data dialog structure
 *  \param [in]   end       OBJECT_END type (currently not used)
 *  \param [in]   type      OBJECT_FILLING type
 *  \param [in]   width     fill width.
 *  \param [in]   length    length of each line
 *  \param [in]   space     space between points and lines
 */
static void line_type_dialog_set_values(struct line_type_data *line_type_data,
                                        OBJECT_END end, OBJECT_TYPE type,
                                        gint width, gint length, gint space)
{
  gchar *text;
  GtkWidget *menu, *menuitem;

  if (type == -2)
    type = TYPE_ERASE;
  gtk_option_menu_set_history(GTK_OPTION_MENU(line_type_data->line_type), type);
  menu = gtk_option_menu_get_menu(GTK_OPTION_MENU(line_type_data->line_type));
  menuitem = gtk_menu_get_active(GTK_MENU(menu));
  gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menuitem), TRUE);

  if (width == -2)
    text = g_strdup(_("*unchanged*"));
  else
    text = g_strdup_printf ("%d", width);
  gtk_entry_set_text (GTK_ENTRY (line_type_data->width_entry), text);
  gtk_entry_select_region (GTK_ENTRY (line_type_data->width_entry), 
                           0, strlen (text));
  g_free(text);

  if (length == -2)
    text = g_strdup(_("*unchanged*"));
  else
    text = g_strdup_printf ("%d", length);
  gtk_entry_set_text (GTK_ENTRY (line_type_data->length_entry), text);
  gtk_entry_select_region (GTK_ENTRY (line_type_data->length_entry), 
                           0, strlen (text));
  g_free(text);

  if (space == -2)
    text = g_strdup(_("*unchanged*"));
  else
    text = g_strdup_printf ("%d", space);
  gtk_entry_set_text (GTK_ENTRY (line_type_data->space_entry), text);
  gtk_entry_select_region (GTK_ENTRY (line_type_data->space_entry),
                           0, strlen (text));
  g_free(text);
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
  GSCHEM_TOPLEVEL *w_current = line_type_data->w_current;
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *selection, *iter;
  OBJECT *object;
  const gchar *width_str, *length_str, *space_str;
  OBJECT_TYPE type;
  gint width, length, space;
  OBJECT_TYPE otype;
  OBJECT_END oend;
  gint owidth, olength, ospace;

  /* get the selection */
  if (! o_select_selected(w_current))
    return;
  selection = 
    geda_list_get_glist(w_current->toplevel->page_current->selection_list);

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
  if (type == TYPE_ERASE)
    type = -1;
  
  /* convert the options to integers (-1 means unchanged) */
  width =  g_strcasecmp (width_str,
                         _("*unchanged*")) ? atoi (width_str)  : -1;
  length = g_strcasecmp (length_str,
                         _("*unchanged*")) ? atoi (length_str) : -1;
  space  = g_strcasecmp (space_str,
                         _("*unchanged*")) ? atoi (space_str)  : -1;

  for (iter = selection; iter != NULL; iter = g_list_next(iter)) {
    object = (OBJECT *) iter->data;
    if (! o_get_line_options(object, &oend, &otype,
                             &owidth, &olength, &ospace))
      continue;

    /* oend is not in the dialog, yet */
    otype = type == -1 ? otype : type;
    owidth = width  == -1 ? owidth : width;
    olength = length == -1 ? olength : length;
    ospace = space  == -1 ? ospace : space;

    /* set all not required options to -1 and 
       set nice parameters if not provided by the user */
    switch (otype) {
    case (TYPE_SOLID):
      olength = ospace = -1;
      break;
    case (TYPE_DOTTED):
      olength = -1;
      if (ospace < 1) ospace = 100;
      break;
    case (TYPE_DASHED):
    case (TYPE_CENTER):
    case (TYPE_PHANTOM):
      if (ospace < 1) ospace = 100;
      if (olength < 1) olength = 100;
      break;
    default:
      g_assert_not_reached();
    }

    o_set_line_options (toplevel, object,
                        oend, otype, owidth, olength, ospace);
  }

  toplevel->page_current->CHANGED = 1;
  o_undo_savestate(w_current, UNDO_ALL);
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

  i_set_state (line_type_data->w_current, SELECT);
  i_update_toolbar (line_type_data->w_current);
  gtk_widget_destroy (line_type_data->dialog);

  g_free (line_type_data);
}

/*! \brief Creates the line type and width dialog
 *  \par Function Description
 *  This function creates and sets up a dialog for manipulating the
 *  line width and the line type setting of objects.
 */
void line_type_dialog (GSCHEM_TOPLEVEL *w_current)
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
  GList *selection;
  OBJECT_END end;
  OBJECT_TYPE type=TYPE_SOLID;
  gint width=1, length=-1, space=-1;

  if (! o_select_selected(w_current))
    return;

  selection = 
    geda_list_get_glist(w_current->toplevel->page_current->selection_list);

  if (! selection_get_line_type(selection, &end, &type,
                                &width, &length, &space))
    return;

  line_type_data = (struct line_type_data*) g_malloc (
    sizeof (struct line_type_data));

  dialog = gschem_dialog_new_with_buttons(_("Edit Line Width & Type"),
                                          GTK_WINDOW(w_current->main_window),
                                          GTK_DIALOG_MODAL,
                                          "line-type", w_current,
                                          GTK_STOCK_CANCEL,
                                          GTK_RESPONSE_REJECT,
                                          GTK_STOCK_OK,
                                          GTK_RESPONSE_ACCEPT,
                                          NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GTK_RESPONSE_ACCEPT,
                                          GTK_RESPONSE_REJECT,
                                          -1);

  gtk_window_position(GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);

  g_signal_connect (G_OBJECT (dialog), "response",
                    G_CALLBACK (line_type_dialog_response),
                    line_type_data);

  gtk_container_border_width(GTK_CONTAINER(dialog),
                             DIALOG_BORDER_SPACING);
  vbox = GTK_DIALOG(dialog)->vbox;
  gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

  /*  Don't know whether to set the headline or not (Werner) */
  /*  label = gtk_label_new(_("Line Properties:"));
      gtk_misc_set_alignment(GTK_MISC(label),0,0);
      gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0); */

  table = gtk_table_new (4, 2, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
  gtk_box_pack_start(GTK_BOX(vbox), table, FALSE, FALSE, 0);

  label = gtk_label_new (_("Type:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,0,1, GTK_FILL,0,0,0);

  label = gtk_label_new (_("Width:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,1,2, GTK_FILL,0,0,0);

  label = gtk_label_new (_("Dash Length:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,2,3, GTK_FILL,0,0,0);

  label = gtk_label_new (_("Dash Space:"));
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_table_attach(GTK_TABLE(table), label, 0,1,3,4, GTK_FILL,0,0,0);

  optionmenu = gtk_option_menu_new ();
  gtk_option_menu_set_menu(GTK_OPTION_MENU(optionmenu),
                           create_menu_linetype (w_current));
  gtk_table_attach_defaults(GTK_TABLE(table), optionmenu,
                            1,2,0,1);

  width_entry = gtk_entry_new();
  gtk_entry_set_activates_default (GTK_ENTRY(width_entry), TRUE);
  gtk_editable_select_region(GTK_EDITABLE(width_entry), 0, -1);
  gtk_table_attach_defaults(GTK_TABLE(table), width_entry,
                            1,2,1,2);

  g_signal_connect(G_OBJECT (optionmenu), "changed",
                   G_CALLBACK (line_type_dialog_linetype_change),
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

  line_type_data->w_current = w_current;

  /* fill in the fields of the dialog */
  line_type_dialog_set_values(line_type_data, end, type,
                              width, length, space);

  /* calling it once will set the dash space/length activity */
  line_type_dialog_linetype_change(optionmenu, line_type_data);

  gtk_widget_grab_focus(width_entry);
  gtk_widget_show_all (dialog);
}

/***************** End of Line Type / Width dialog box ****************/

/***************** Start of Fill Type dialog box **********************/

/*! \brief Create a menu with fill types for the line type dialog
 *  \par Function Description
 *  This function creates a GtkMenu with the different fill types.
 */
static GtkWidget *create_menu_filltype (GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *menu;
  GSList *group;
  struct fill_type {
    gchar *str;
    OBJECT_FILLING type;
  } types[] = { { N_("Hollow"), FILLING_HOLLOW },
                { N_("Filled"), FILLING_FILL },
                { N_("Mesh"),   FILLING_MESH },
                { N_("Hatch"),  FILLING_HATCH },
                { N_("*unchanged*"), FILLING_VOID } };
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

/*! \brief get the filltype data from selected objects
 *  \par Function Description
 *  Get filltype information over all selected objects.
 *  If a object property is different to the other objects, then
 *  return -2 in that variable.
 *  \param [in]   selection the selection list
 *  \param [out]  type      OBJECT_FILLING type
 *  \param [out]  width     fill width.
 *  \param [out]  pitch1    cross hatch line distance
 *  \param [out]  angle1    cross hatch angle
 *  \param [out]  pitch2    cross hatch line distance
 *  \param [out]  angle2    cross hatch angle
 *  \returns TRUE if filltype found, FALSE otherwise
 */
static gboolean selection_get_fill_type(GList *selection,
                                        OBJECT_FILLING *type, gint *width,
                                        gint *pitch1, gint *angle1,
                                        gint *pitch2, gint *angle2)
{
  GList *iter;
  OBJECT *object;
  gboolean found = FALSE;
  OBJECT_FILLING otype;
  gint owidth, opitch1, oangle1, opitch2, oangle2;


  for (iter = selection; iter != NULL; iter = g_list_next(iter)) {
    object = (OBJECT *) iter->data;
    if (! o_get_fill_options(object, &otype, &owidth, 
                             &opitch1, &oangle1, &opitch2, &oangle2))
      continue;

    if (found == FALSE) {  /* first object with filltype */
      found = TRUE;
      *type = otype;
      *width = owidth;
      *pitch1 = opitch1;
      *angle1 = oangle1;
      *pitch2 = opitch2;
      *angle2 = oangle2;
    } else {
      /* indicate different values with the value -2 */
      if (*type != otype) *type = -2;
      if (*width != owidth) *width = -2;
      if (*pitch1 != opitch1) *pitch1 = -2;
      if (*angle1 != oangle1) *angle1 = -2;
      if (*pitch2 != opitch2) *pitch2 = -2;
      if (*angle2 != oangle2) *angle2 = -2;
    }
  }

  return found;
}


/*! \brief set the filltype in the filltype dialog
 *  \par Function Description
 *  Set all widgets in the filltype dialog. Variables marked with the
 *  invalid value -2 are set to *unchanged*.
 *  \param [in]   fill_type_data dialog structure
 *  \param [in]   type      OBJECT_FILLING type
 *  \param [in]   width     fill width.
 *  \param [in]   pitch1    cross hatch line distance
 *  \param [in]   angle1    cross hatch angle
 *  \param [in]   pitch2    cross hatch line distance
 *  \param [in]   angle2    cross hatch angle
 */
static void fill_type_dialog_set_values(struct fill_type_data *fill_type_data,
                                        OBJECT_FILLING type, gint width,
                                        gint pitch1, gint angle1,
                                        gint pitch2, gint angle2)
{
  gchar *text;
  GtkWidget *menu, *menuitem;

  if (type == -2)
    type = FILLING_VOID;
  gtk_option_menu_set_history(GTK_OPTION_MENU(fill_type_data->fill_type), type);
  menu = gtk_option_menu_get_menu(GTK_OPTION_MENU(fill_type_data->fill_type));
  menuitem = gtk_menu_get_active(GTK_MENU(menu));
  gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menuitem), TRUE);

  if (width == -2)
    text = g_strdup(_("*unchanged*"));
  else
    text = g_strdup_printf ("%d", width);
  gtk_entry_set_text (GTK_ENTRY (fill_type_data->width_entry), text);
  gtk_entry_select_region (GTK_ENTRY (fill_type_data->width_entry), 
                           0, strlen (text));
  g_free(text);

  if (pitch1 == -2)
    text = g_strdup(_("*unchanged*"));
  else
    text = g_strdup_printf ("%d", pitch1);
  gtk_entry_set_text (GTK_ENTRY (fill_type_data->pitch1_entry), text);
  gtk_entry_select_region (GTK_ENTRY (fill_type_data->pitch1_entry), 
                           0, strlen (text));
  g_free(text);

  if (angle1 == -2)
    text = g_strdup(_("*unchanged*"));
  else
    text = g_strdup_printf ("%d", angle1);
  gtk_entry_set_text (GTK_ENTRY (fill_type_data->angle1_entry), text);
  gtk_entry_select_region (GTK_ENTRY (fill_type_data->angle1_entry), 
                           0, strlen (text));
  g_free(text);

  if (pitch2 == -2)
    text = g_strdup(_("*unchanged*"));
  else
    text = g_strdup_printf ("%d", pitch2);
  gtk_entry_set_text (GTK_ENTRY (fill_type_data->pitch2_entry), text);
  gtk_entry_select_region (GTK_ENTRY (fill_type_data->pitch2_entry), 
                           0, strlen (text));
  g_free(text);

  if (angle2 == -2)
    text = g_strdup(_("*unchanged*"));
  else
    text = g_strdup_printf ("%d", angle2);
  gtk_entry_set_text (GTK_ENTRY (fill_type_data->angle2_entry), text);
  gtk_entry_select_region (GTK_ENTRY (fill_type_data->angle2_entry), 
                           0, strlen (text));
  g_free(text);
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
  case(FILLING_VOID):
    activate_width_entry = TRUE;
    activate_anglepitch1_entries = TRUE;
    activate_anglepitch2_entries = TRUE;
    break;
  default:
    g_assert_not_reached ();
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
  struct fill_type_data *fill_type_data = (struct fill_type_data*) data;
  GSCHEM_TOPLEVEL *w_current = fill_type_data->w_current;
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *selection, *iter;
  OBJECT *object;
  const gchar *width_str, *angle1_str, *pitch1_str, *angle2_str, *pitch2_str;
  OBJECT_FILLING type;
  gint width, angle1, pitch1, angle2, pitch2;
  OBJECT_FILLING otype;
  gint owidth, oangle1, opitch1, oangle2, opitch2;

  /* get the selection */
  if (! o_select_selected(w_current))
    return;
  selection = 
    geda_list_get_glist(w_current->toplevel->page_current->selection_list);

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
  if (type == FILLING_VOID)
    type = -1;

  /* convert the options to integers (-1 means unchanged) */
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

  for (iter = selection; iter != NULL; iter = g_list_next(iter)) {
    object = (OBJECT *) iter->data;
    if (! o_get_fill_options(object, &otype, &owidth,
                             &opitch1, &oangle1, &opitch2, &oangle2))
      continue;

    otype = type == -1 ? otype : type;
    owidth = width == -1 ? owidth : width;
    opitch1 = pitch1 == -1 ? opitch1 : pitch1;
    oangle1 = angle1 == -1 ? oangle1 : angle1;
    opitch2 = pitch2 == -1 ? opitch2 : pitch2;
    oangle2 = angle2 == -1 ? oangle2 : angle2;
    
    /* set all not required options to -1 and 
       set nice parameters if not provided by the user */
    switch (otype) {
    case (FILLING_HOLLOW):
    case (FILLING_FILL):
      owidth = opitch1 = oangle1 = opitch2 = oangle2 = -1;
      break;
    case (FILLING_HATCH):
      if (owidth < 1) owidth = 1;
      if (opitch1 < 1) opitch1 = 100;
      opitch2 = oangle2 = -1;
      break;
    case (FILLING_MESH):
      if (owidth < 1) owidth = 1;
      if (opitch1 < 1) opitch1 = 100;
      if (opitch2 < 1) opitch2 = 100;
      break;
    default:
      g_assert_not_reached();
    }
    
    o_set_fill_options (toplevel, object, otype, owidth,
                        opitch1, oangle1, opitch2, oangle2);
  }

  toplevel->page_current->CHANGED = 1;
  o_undo_savestate(w_current, UNDO_ALL);
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

  i_set_state (fill_type_data->w_current, SELECT);
  i_update_toolbar (fill_type_data->w_current);

  gtk_grab_remove (fill_type_data->dialog);
  gtk_widget_destroy (fill_type_data->dialog);

  g_free (fill_type_data);
}

/*! \brief Creates the fill type dialog
 *  \par Function Description
 *  This function creates the fill type dialog.
 *  It uses the selection to set it's initial values.
 */
void fill_type_dialog(GSCHEM_TOPLEVEL *w_current)
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
  GList *selection;
  OBJECT_FILLING type=FILLING_VOID;
  gint width=0, pitch1=0, angle1=0, pitch2=0, angle2=0;

  if (! o_select_selected(w_current))
    return;

  selection = 
    geda_list_get_glist(w_current->toplevel->page_current->selection_list);

  if (! selection_get_fill_type(selection, &type, &width,
                                &pitch1, &angle1, &pitch2, &angle2))
    return;

  fill_type_data = (struct fill_type_data*) g_malloc (
    sizeof (struct fill_type_data));

  dialog = gschem_dialog_new_with_buttons(_("Edit Fill Type"),
                                          GTK_WINDOW(w_current->main_window),
                                          GTK_DIALOG_MODAL,
                                          "fill-type", w_current,
                                          GTK_STOCK_CANCEL,
                                          GTK_RESPONSE_REJECT,
                                          GTK_STOCK_OK,
                                          GTK_RESPONSE_ACCEPT,
                                          NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GTK_RESPONSE_ACCEPT,
                                          GTK_RESPONSE_REJECT,
                                          -1);

  gtk_window_position(GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);

  gtk_dialog_set_default_response(GTK_DIALOG(dialog),
                                  GTK_RESPONSE_ACCEPT);

  g_signal_connect (G_OBJECT (dialog), "response",
                    G_CALLBACK (fill_type_dialog_response),
                    fill_type_data);

  gtk_container_border_width(GTK_CONTAINER(dialog), DIALOG_BORDER_SPACING);
  vbox = GTK_DIALOG(dialog)->vbox;
  gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

  /*  Don't know whether to use the headline or not (Werner) */
  /*  label = gtk_label_new(_("Fill Properties:"));
      gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
      gtk_box_pack_start(GTK_BOX(vbox),label, FALSE, FALSE, 0);  */

  table = gtk_table_new (6, 2, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
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

  g_signal_connect (G_OBJECT (optionmenu), "changed",
                    G_CALLBACK (fill_type_dialog_filltype_change),
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

  fill_type_data->w_current = w_current;

  /* fill in the fields of the dialog */
  fill_type_dialog_set_values(fill_type_data, type, width,
                              pitch1, angle1, pitch2, angle2);

  /* Set the widget activity according to the current filltype */
  fill_type_dialog_filltype_change(optionmenu, fill_type_data);

  gtk_widget_grab_focus(width_entry);
  gtk_widget_show_all (dialog);
}

/***************** End of Fill Type dialog box ***********************/

/***************** Start of Arc dialog box ***************************/

/*! \brief response function for the arc angle dialog
 *  \par Function Description
 *  The response function of th arc angle dialog takes the content of
 *  the dialog and applies it on the current arc.
 *  If the dialog is closed or canceled the function destroys the dialog.
 */
void arc_angle_dialog_response(GtkWidget *w, gint response,
                               GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *spinentry;
  gint radius, start_angle, sweep_angle;
  OBJECT *arc_object = NULL;

  switch (response) {
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    /* void */
    break;
  case GTK_RESPONSE_ACCEPT:
    spinentry = g_object_get_data(G_OBJECT(w_current->aawindow),"radius");
    radius = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spinentry));
    spinentry = g_object_get_data(G_OBJECT(w_current->aawindow),"spin_start");
    start_angle = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spinentry));
    spinentry = g_object_get_data(G_OBJECT(w_current->aawindow),"spin_sweep");
    sweep_angle = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spinentry));
    arc_object = (OBJECT*) g_object_get_data(G_OBJECT(w_current->aawindow),"arc_object");

    if (arc_object != NULL) {
      o_arc_modify(w_current->toplevel, arc_object, radius, 0, ARC_RADIUS);
      o_arc_modify(w_current->toplevel, arc_object, start_angle, 0, ARC_START_ANGLE);
      o_arc_modify(w_current->toplevel, arc_object, sweep_angle, 0, ARC_END_ANGLE);
    } else {
      o_arc_end4(w_current, radius, start_angle, sweep_angle);
    }
    break;
  default:
    printf("arc_angle_dialog_response(): strange signal %d\n",response);
  }

  gtk_widget_destroy(w_current->aawindow);
  w_current->aawindow = NULL;
}

/*! \brief Creates the arc angle dialog
 *  \par Function Description
 *  This function creates the arc angle dialog. Depending on the 
 *  \a arc_object the entries are filled with the arc OBJECT properties
 *  or with some standard values.
 *
 *  \param [in] w_current   The GSCHEM_TOPLEVEL object
 *  \param [in] arc_object  an arc OBJECT if used to modify an arc
 *                          or NULL to create a new arc.
 */
void arc_angle_dialog (GSCHEM_TOPLEVEL *w_current, OBJECT *arc_object)
{
  GtkWidget *label = NULL;
  GtkWidget *vbox;
  GtkWidget *alignment, *table;
  GtkWidget *radius, *spin_start, *spin_sweep;

  if (!w_current->aawindow) {
    w_current->aawindow = gschem_dialog_new_with_buttons(_("Arc Params"),
                                                         GTK_WINDOW(w_current->main_window),
                                                         GTK_DIALOG_MODAL,
                                                         "arc-angle", w_current,
                                                         GTK_STOCK_CANCEL,
                                                         GTK_RESPONSE_REJECT,
                                                         GTK_STOCK_OK,
                                                         GTK_RESPONSE_ACCEPT,
                                                         NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->aawindow),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_window_position(GTK_WINDOW(w_current->aawindow),
                        GTK_WIN_POS_MOUSE);

    g_signal_connect (G_OBJECT (w_current->aawindow), "response",
                      G_CALLBACK (arc_angle_dialog_response),
                      w_current);

    gtk_dialog_set_default_response(GTK_DIALOG(w_current->aawindow),
                                    GTK_RESPONSE_ACCEPT);

    gtk_container_border_width(GTK_CONTAINER(w_current->aawindow), DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(w_current->aawindow)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);


    alignment = gtk_alignment_new(0,0,1,1);
    gtk_alignment_set_padding(GTK_ALIGNMENT(alignment), 0, 0,
                              0 /*DIALOG_INDENTATION */, 0);
    gtk_box_pack_start(GTK_BOX(vbox), alignment, FALSE, FALSE, 0);

    table = gtk_table_new (2, 3, FALSE);
    gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
    gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
    gtk_container_add(GTK_CONTAINER(alignment), table);

    label = gtk_label_new (_("Arc Radius:"));
    gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
    gtk_table_attach(GTK_TABLE(table), label, 0,1,0,1, GTK_FILL,0,0,0);

    radius = gtk_spin_button_new_with_range(1, 100000, 100);
    gtk_entry_set_activates_default(GTK_ENTRY(radius), TRUE);
    gtk_table_attach_defaults(GTK_TABLE(table), radius, 1,2,0,1);

    label = gtk_label_new (_("Start Angle:"));
    gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
    gtk_table_attach(GTK_TABLE(table), label, 0,1,1,2, GTK_FILL,0,0,0);

    spin_start = gtk_spin_button_new_with_range(-360,360,1);
    gtk_entry_set_activates_default(GTK_ENTRY(spin_start), TRUE);
    gtk_table_attach_defaults(GTK_TABLE(table), spin_start, 1,2,1,2);

    label = gtk_label_new(_("Degrees of Sweep:"));
    gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
    gtk_table_attach(GTK_TABLE(table), label, 0,1,2,3, GTK_FILL,0,0,0);

    spin_sweep = gtk_spin_button_new_with_range(-360,360,1);
    gtk_entry_set_activates_default(GTK_ENTRY(spin_sweep), TRUE);
    gtk_table_attach_defaults(GTK_TABLE(table), spin_sweep, 1,2,2,3);

    GLADE_HOOKUP_OBJECT(w_current->aawindow, radius, "radius");
    GLADE_HOOKUP_OBJECT(w_current->aawindow, spin_start,"spin_start");
    GLADE_HOOKUP_OBJECT(w_current->aawindow, spin_sweep,"spin_sweep");
    g_object_set_data(G_OBJECT(w_current->aawindow), "arc_object", arc_object);
    gtk_widget_show_all (w_current->aawindow);
  }

  else {  /* dialog already created */
    gtk_window_present (GTK_WINDOW(w_current->aawindow));
    radius = g_object_get_data(G_OBJECT(w_current->aawindow),"radius");
    spin_start = g_object_get_data(G_OBJECT(w_current->aawindow),"spin_start");
    spin_sweep = g_object_get_data(G_OBJECT(w_current->aawindow),"spin_sweep");
  }

  if (arc_object == NULL) {
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(radius), w_current->distance);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_start),0);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_sweep), 90);
  } else {
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(radius), 
			      arc_object->arc->width / 2);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_start),
			      arc_object->arc->start_angle);
    gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_sweep),
			      arc_object->arc->end_angle);
  }

  gtk_widget_grab_focus(radius);
}

/***************** End of Arc dialog box *****************************/

/***************** Start of Translate dialog box *********************/

/*! \brief response function for the translate dialog
 *  \par Function Description
 *  This function takes the user action and applies it.
 *  \todo improve error detection / use a spin button?
 */
void translate_dialog_response(GtkWidget *widget, gint response,
                               GSCHEM_TOPLEVEL *w_current)
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
    printf("translate_edit_dialog_response(): strange signal %d\n",response);
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
void translate_dialog (GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *label;
  GtkWidget *textentry;
  GtkWidget *vbox;

  if (!w_current->trwindow) {
    w_current->trwindow = gschem_dialog_new_with_buttons(_("Translate"),
                                                         GTK_WINDOW(w_current->main_window),
                                                         GTK_DIALOG_MODAL,
                                                         "translate", w_current,
                                                         GTK_STOCK_CANCEL,
                                                         GTK_RESPONSE_REJECT,
                                                         GTK_STOCK_OK,
                                                         GTK_RESPONSE_ACCEPT,
                                                         NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->trwindow),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_window_position(GTK_WINDOW (w_current->trwindow),
                        GTK_WIN_POS_MOUSE);

    g_signal_connect (G_OBJECT (w_current->trwindow), "response",
                      G_CALLBACK (translate_dialog_response),
                      w_current);

    gtk_dialog_set_default_response(GTK_DIALOG(w_current->trwindow),
                                    GTK_RESPONSE_ACCEPT);

    gtk_container_border_width(GTK_CONTAINER(w_current->trwindow),
                               DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(w_current->trwindow)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    label = gtk_label_new(_("Offset to translate?\n(0 for origin)"));
    gtk_misc_set_alignment(GTK_MISC (label), 0, 0);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

    textentry = gtk_entry_new_with_max_length (10);
    gtk_entry_set_text(GTK_ENTRY(textentry), "0");
    gtk_editable_select_region(GTK_EDITABLE(textentry), 0, -1);
    gtk_entry_set_activates_default(GTK_ENTRY(textentry), TRUE);
    gtk_box_pack_start(GTK_BOX(vbox),textentry, FALSE, FALSE, 0);

    GLADE_HOOKUP_OBJECT(w_current->trwindow, textentry, "textentry");
    gtk_widget_show_all (w_current->trwindow);
  }

  else  { /* dialog already created */
    gtk_window_present(GTK_WINDOW(w_current->trwindow));
  }
}

/***************** End of Translate dialog box ***********************/

/***************** Start of Text size dialog box *********************/

/*! \brief response function for the text size dialog
 *  \par Function Description
 *  This function takes the user input and applies it to gschem
 */
void text_size_dialog_response(GtkWidget *w, gint response,
                               GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *spin_size;
  gint size;

  switch (response) {
  case GTK_RESPONSE_ACCEPT:
    spin_size = g_object_get_data(G_OBJECT(w_current->tswindow),"spin_size");
    size = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spin_size));

    w_current->text_size = size;
    w_current->toplevel->page_current->CHANGED=1;
    o_undo_savestate(w_current, UNDO_ALL);
    break;
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    /* void */
    break;
  default:
    printf("text_size_dialog_response(): strange signal %d\n",response);
  }

  /* clean up */
  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);
  gtk_widget_destroy(w_current->tswindow);
  w_current->tswindow = NULL;
}

/*! \brief Create the text size dialog
 *  \par Function Description
 *  This function creates the text size dialog.
 */
void text_size_dialog (GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *label = NULL;
  GtkWidget *vbox;
  GtkWidget *spin_size;

  if (!w_current->tswindow) {
    w_current->tswindow = gschem_dialog_new_with_buttons(_("Text Size"),
                                                         GTK_WINDOW(w_current->main_window),
                                                         GTK_DIALOG_MODAL,
                                                         "text-size", w_current,
                                                         GTK_STOCK_CANCEL,
                                                         GTK_RESPONSE_REJECT,
                                                         GTK_STOCK_OK,
                                                         GTK_RESPONSE_ACCEPT,
                                                         NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->tswindow),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_window_position(GTK_WINDOW(w_current->tswindow),
                        GTK_WIN_POS_MOUSE);

    g_signal_connect (G_OBJECT (w_current->tswindow), "response",
                      G_CALLBACK (text_size_dialog_response),
                      w_current);
    gtk_dialog_set_default_response(GTK_DIALOG(w_current->tswindow),
                                    GTK_RESPONSE_ACCEPT);

    gtk_container_border_width(GTK_CONTAINER(w_current->tswindow),
                               DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(w_current->tswindow)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    label = gtk_label_new (_("Enter new text size:"));
    gtk_misc_set_alignment (GTK_MISC (label), 0, 0);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

    spin_size = gtk_spin_button_new_with_range(2,10000,2);
    gtk_editable_select_region( GTK_EDITABLE(spin_size), 0, -1);
    gtk_box_pack_start(GTK_BOX(vbox), spin_size, FALSE, FALSE, 0);
    gtk_entry_set_activates_default(GTK_ENTRY(spin_size), TRUE);
    gtk_widget_grab_focus(spin_size);

    GLADE_HOOKUP_OBJECT(w_current->tswindow, spin_size, "spin_size");
    gtk_widget_show_all(w_current->tswindow);
  }

  else { /* dialog already created */
    gtk_window_present(GTK_WINDOW(w_current->tswindow));
  }

  /* always set the current text size to the dialog */
  spin_size = g_object_get_data(G_OBJECT(w_current->tswindow),"spin_size");
  gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_size), w_current->text_size);
  gtk_editable_select_region(GTK_EDITABLE(spin_size), 0, -1);
}

/***************** End of Text size dialog box ***********************/

/***************** Start of Snap size dialog box *********************/

/*! \brief response function for the snap size dialog
 *  \par Function Description
 *  This is the response function for the snap size dialog.
 *  It sets the given snap size to gschem.
 */
void snap_size_dialog_response(GtkWidget *w, gint response,
                               GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *spin_size;
  gint size;

  switch (response) {
  case GTK_RESPONSE_ACCEPT:
    spin_size = g_object_get_data(G_OBJECT(w_current->tswindow),"spin_size");
    size = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spin_size));

    w_current->snap_size = size;
    i_update_grid_info (w_current);
    o_invalidate_all (w_current);
    w_current->toplevel->page_current->CHANGED=1;  /* maybe remove those two lines */
    o_undo_savestate(w_current, UNDO_ALL);
    break;
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    /* void */
    break;
  default:
    printf("snap_size_dialog_response(): strange signal %d\n",response);
  }

  /* clean up */
  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);
  gtk_widget_destroy(w_current->tswindow);
  w_current->tswindow = NULL;
}

/*! \brief Create the snap size dialog
 *  \par Function Description
 *  This function creates the snap size dialog.
 */
void snap_size_dialog (GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *label = NULL;
  GtkWidget *vbox;
  GtkWidget *spin_size;

  if (!w_current->tswindow) {
    w_current->tswindow = gschem_dialog_new_with_buttons(_("Snap Size"),
                                                         GTK_WINDOW(w_current->main_window),
                                                         GTK_DIALOG_MODAL,
                                                         "snap-size", w_current,
                                                         GTK_STOCK_CANCEL,
                                                         GTK_RESPONSE_REJECT,
                                                         GTK_STOCK_OK,
                                                         GTK_RESPONSE_ACCEPT,
                                                         NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->tswindow),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_window_position(GTK_WINDOW(w_current->tswindow),
                        GTK_WIN_POS_MOUSE);

    g_signal_connect (G_OBJECT (w_current->tswindow), "response",
                      G_CALLBACK (snap_size_dialog_response),
                      w_current);
    gtk_dialog_set_default_response(GTK_DIALOG(w_current->tswindow),
                                    GTK_RESPONSE_ACCEPT);

    gtk_container_border_width(GTK_CONTAINER(w_current->tswindow),
                               DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(w_current->tswindow)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    label = gtk_label_new (_("Enter new snap grid spacing:"));
    gtk_misc_set_alignment (GTK_MISC (label), 0, 0);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

    spin_size = gtk_spin_button_new_with_range(0,100000,5);
    gtk_editable_select_region( GTK_EDITABLE(spin_size), 0, -1);
    gtk_box_pack_start(GTK_BOX(vbox), spin_size, FALSE, FALSE, 0);
    gtk_entry_set_activates_default(GTK_ENTRY(spin_size), TRUE);
    gtk_widget_grab_focus(spin_size);

    GLADE_HOOKUP_OBJECT(w_current->tswindow, spin_size, "spin_size");
    gtk_widget_show_all(w_current->tswindow);
  }

  else {  /* dialog already there */
    gtk_window_present(GTK_WINDOW(w_current->tswindow));
  }

  /* always set the current gschem value to the dialog entry */
  spin_size = g_object_get_data(G_OBJECT(w_current->tswindow),"spin_size");
  gtk_spin_button_set_value(GTK_SPIN_BUTTON(spin_size), w_current->snap_size);
  gtk_editable_select_region(GTK_EDITABLE(spin_size), 0, -1);
}

/***************** End of Snap size dialog box ***********************/

/***************** Start of slot edit dialog box *********************/

/*! \brief response function for the slot edit dialog
 *  \par Function Description
 *  The function takes the dialog entry and applies the new slot to the
 *  symbol.
 */
void slot_edit_dialog_response(GtkWidget *widget, gint response, GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *textentry;
  char *slot_string;
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
      slot_string = g_strdup_printf ("slot=%s", string);
      o_slot_end (w_current, o_select_return_first_object (w_current),
                  slot_string);
      g_free (slot_string);
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
void slot_edit_dialog (GSCHEM_TOPLEVEL *w_current, const char *string)
{
  GtkWidget *label = NULL;
  GtkWidget *textentry;
  GtkWidget *vbox;

  if (!w_current->sewindow) {
    w_current->sewindow = gschem_dialog_new_with_buttons(_("Edit slot number"),
                                                         GTK_WINDOW(w_current->main_window),
                                                         GTK_DIALOG_MODAL,
                                                         "slot-edit", w_current,
                                                         GTK_STOCK_CANCEL,
                                                         GTK_RESPONSE_REJECT,
                                                         GTK_STOCK_OK,
                                                         GTK_RESPONSE_ACCEPT,
                                                         NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->sewindow),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_window_position(GTK_WINDOW(w_current->sewindow),
                        GTK_WIN_POS_MOUSE);

    gtk_dialog_set_default_response (GTK_DIALOG (w_current->sewindow),
                                     GTK_RESPONSE_ACCEPT);

    g_signal_connect (G_OBJECT (w_current->sewindow), "response",
                      G_CALLBACK (slot_edit_dialog_response),
                      w_current);

    gtk_container_border_width(GTK_CONTAINER(w_current->sewindow),
                               DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(w_current->sewindow)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

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

  else { /* dialog already created */
    gtk_window_present (GTK_WINDOW(w_current->sewindow));
  }

  /* always set the current text and select the number of the slot */
  if (string != NULL) {
    textentry = g_object_get_data(G_OBJECT(w_current->sewindow),"textentry");
    gtk_entry_set_text(GTK_ENTRY(textentry), string);
    gtk_editable_select_region (GTK_EDITABLE(textentry), 0, -1);
  }
}

/***************** End of Slot Edit dialog box ***********************/

/***************** Start of help/about dialog box ********************/

/*! \brief Create the about dialog and show it
 *  \par Function Description
 *  This function creates the about dialog.
 */
void about_dialog (GSCHEM_TOPLEVEL *w_current)
{
  char *version_string;
  char *logo_file;
  GdkPixbuf *logo;
  GError *error = NULL;

  version_string = g_strdup_printf (_("%s (g%.7s)"),
                                    PACKAGE_DOTTED_VERSION,
                                    PACKAGE_GIT_COMMIT);

  logo_file = g_strconcat (w_current->toplevel->bitmap_directory,
                           G_DIR_SEPARATOR_S, "gschem-about-logo.png", NULL);

  logo = gdk_pixbuf_new_from_file (logo_file, &error);
  g_free (logo_file);

  if (error != NULL) {
    g_assert (logo == NULL);
    s_log_message ("Could not load image at file: %s\n%s\n",
                   logo_file, error->message);
    g_error_free (error);
  }

  gtk_show_about_dialog (
      GTK_WINDOW (w_current->main_window),
      "version",        version_string,
      "logo",           logo,
      "title",          _("About gschem"),
      "comments",       _("gEDA: GPL Electronic Design Automation"),
      "copyright",      _("Copyright © 1998-2011 Ales Hvezda"
                            " <ahvezda@geda.seul.org>\n"
                          "Copyright © 1998-2011 gEDA Contributors"
                            " (see ChangeLog for details)"),
      "website",        "http://www.gpleda.org/",
      NULL);

  g_free (version_string);
  g_object_unref (logo);
}

/***************** End of help/about dialog box *********************/

/***************** Start of coord dialog box ************************/
/*! \brief Response function for the coord dialog
 *  \par Function Description
 *  This function destroys the coord dialog box and does some cleanup.
 */
void coord_dialog_response(GtkWidget *w, gint response, GSCHEM_TOPLEVEL *w_current)
{
  gtk_widget_destroy(w_current->cowindow);
  w_current->cowindow = NULL;
  w_current->coord_world = NULL;
  w_current->coord_screen = NULL;
}

/*! \brief Update the coordinates in the coord dialog box.
 *  \par Function Description
 *  This function takes the screen coordinates and prints the
 *  screen and the world coordinates in the coord dialog.
 */
void coord_display_update(GSCHEM_TOPLEVEL *w_current, int x, int y)
{
  char *string;
  int world_x, world_y;

  string = g_strdup_printf("(%d, %d)", x, y);
  gtk_label_set_text(GTK_LABEL(w_current->coord_screen), string );
  g_free(string);

  SCREENtoWORLD (w_current, x, y, &world_x, &world_y);
  world_x = snap_grid (w_current, world_x);
  world_y = snap_grid (w_current, world_y);

  string = g_strdup_printf("(%d, %d)", world_x, world_y);
  gtk_label_set_text(GTK_LABEL(w_current->coord_world), string );
  g_free(string);
}

/*! \brief Create the coord dialog
 *  \par Function Description
 *  This function creates the coord dialog box.
 */
void coord_dialog (GSCHEM_TOPLEVEL *w_current, int x, int y)
{
  GtkWidget *frame;
  GtkWidget *vbox;

  if (!w_current->cowindow) {
    w_current->cowindow = gschem_dialog_new_with_buttons(_("Coords"),
                                                         GTK_WINDOW(w_current->main_window),
                                                         0, /* Not modal GTK_DIALOG_MODAL */
                                                         "coord", w_current,
                                                         GTK_STOCK_CLOSE,
                                                         GTK_RESPONSE_REJECT,
                                                         NULL);

    gtk_window_position (GTK_WINDOW (w_current->cowindow),
                         GTK_WIN_POS_NONE);

    g_signal_connect (G_OBJECT (w_current->cowindow), "response",
                      G_CALLBACK (coord_dialog_response),
                      w_current);

    gtk_container_border_width (GTK_CONTAINER(w_current->cowindow),
                                DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(w_current->cowindow)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);


    frame = gtk_frame_new (_("Screen"));
    w_current->coord_screen = gtk_label_new("(########, ########)");
    gtk_label_set_justify( GTK_LABEL(w_current->coord_screen), GTK_JUSTIFY_LEFT);
    gtk_misc_set_padding(GTK_MISC(w_current->coord_screen),
                         DIALOG_H_SPACING, DIALOG_V_SPACING);
    gtk_container_add(GTK_CONTAINER (frame),
                      w_current->coord_screen);
    gtk_box_pack_start(GTK_BOX (vbox), frame, FALSE, FALSE, 0);

    frame = gtk_frame_new (_("World"));
    w_current->coord_world = gtk_label_new ("(########, ########)");
    gtk_misc_set_padding(GTK_MISC(w_current->coord_world),
                         DIALOG_H_SPACING, DIALOG_V_SPACING);
    gtk_label_set_justify(GTK_LABEL(w_current->coord_world),
                          GTK_JUSTIFY_LEFT);
    gtk_container_add(GTK_CONTAINER (frame),
                      w_current->coord_world);
    gtk_box_pack_start(GTK_BOX (vbox), frame, FALSE, FALSE, 0);

    gtk_widget_show_all(w_current->cowindow);
  }

  else { /* window already creatad  */
    gtk_window_present(GTK_WINDOW(w_current->cowindow));
  }

  /* always update the coords when the dialog is requested */
  coord_display_update(w_current, x, y);
}

/***************** End of coord dialog box **************************/

/***************** Start of color edit dialog box *******************/

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \warning
 *  Caller must g_free returned character string.
 *
 */
char *index2functionstring(int index)
{
  char *string;

  switch(index) {
    case(BACKGROUND_COLOR):
      string = g_strdup (_("Background"));
      break;
    case(PIN_COLOR):
      string = g_strdup (_("Pin"));
      break;
    case(NET_ENDPOINT_COLOR):
      string = g_strdup (_("Net endpoint"));
      break;
    case(GRAPHIC_COLOR):
      string = g_strdup (_("Graphic"));
      break;
    case(NET_COLOR):
      string = g_strdup (_("Net"));
      break;
    case(ATTRIBUTE_COLOR):
      string = g_strdup (_("Attribute"));
      break;
    case(LOGIC_BUBBLE_COLOR):
      string = g_strdup (_("Logic bubble"));
      break;
    case(DOTS_GRID_COLOR):
      string = g_strdup (_("Grid point"));
      break;
    case(DETACHED_ATTRIBUTE_COLOR):
      string = g_strdup (_("Detached attribute"));
      break;
    case(TEXT_COLOR):
      string = g_strdup (_("Text"));
      break;
    case(BUS_COLOR):
      string = g_strdup (_("Bus"));
      break;
    case(SELECT_COLOR):
      string = g_strdup (_("Selection"));
      break;
    case(BOUNDINGBOX_COLOR):
      string = g_strdup (_("Bounding box"));
      break;
    case(ZOOM_BOX_COLOR):
      string = g_strdup (_("Zoom box"));
      break;
    case(STROKE_COLOR):
      string = g_strdup (_("Stroke"));
      break;
    case(LOCK_COLOR):
      string = g_strdup (_("Lock"));
      break;
    case(OUTPUT_BACKGROUND_COLOR):
      string = g_strdup (_("Output background"));
      break;
    case(JUNCTION_COLOR):
      string = g_strdup (_("Net junction"));
      break;
    case(MESH_GRID_MAJOR_COLOR):
      string = g_strdup (_("Mesh grid major"));
      break;
    case(MESH_GRID_MINOR_COLOR):
      string = g_strdup (_("Mesh grid minor"));
      break;
    default:
      string = g_strdup (_("Unknown"));
      break;
  }
  return(string);
}

/*! \brief Cell layout data function for color combobox.
 *  \par Function Description
 *  Cell layout data function to support color swatches in the color
 *  combobox.
 *
 *  \param layout
 *  \param cell
 *  \param model
 *  \param iter
 *  \param data the current #GSCHEM_TOPLEVEL pointer.
 */
static void
color_menu_swatch_layout_data (GtkCellLayout *layout,
                               GtkCellRenderer *cell,
                               GtkTreeModel *model,
                               GtkTreeIter *iter,
                               gpointer data)
{
  /* GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL *) data; */
  GValue v = {0, };
  gint idx;

  /* Get the index of the color on this row */
  gtk_tree_model_get_value (model, iter, 1, &v);
  idx = g_value_get_int (&v);

  /* Set the cell's background color */
  g_object_set (cell, "background-gdk", x_get_color (idx), NULL);
}

/*! \brief Handle color combobox selection change event.
 *  \par Function Description
 *  Update application state to reflect color combobox selection
 *  changes.
 *
 *  \param widget
 *  \param data the current #GSCHEM_TOPLEVEL pointer.
 */
static void
color_menu_change_selection (GtkWidget *widget,
                             gpointer data)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL *) data;
  GtkComboBox *cbox = GTK_COMBO_BOX (widget);
  gint idx;
  GtkTreeIter iter;
  GValue v = {0, };

  if (!gtk_combo_box_get_active_iter (cbox, &iter)) {
    return; /* No color selected */
  }
  gtk_tree_model_get_value (gtk_combo_box_get_model (cbox),
                            &iter, 1, &v);
  idx = g_value_get_int (&v);

  /* Stash the selected color in the GSCHEM_TOPLEVEL.
   * FIXME this is ugly. */
  w_current->edit_color = idx;
}

/*! \brief Create a ComboBox with the gschem colors.
 *  \par Function Description
 *  Creates a #GtkComboBox with the color list and swatches showing
 *  each of the available colors.
 *
 *  The backing #GtkTreeModel is a #GtkListStore with two columns, the
 *  first holding the user-friendly name of the color, and the other
 *  the color map index.
 *
 *  \param [in] w_current    The current gschem context.
 */
static GtkWidget *
create_color_menu (GSCHEM_TOPLEVEL *w_current)
{
  GtkListStore *store;
  GtkComboBox *cbox;
  GtkCellLayout *layout;
  GtkCellRenderer *text_cell;
  GtkCellRenderer *color_cell;

  gint i;
  gchar *str;
  OBJECT *obj;
  GtkTreeIter iter;

  obj = o_select_return_first_object (w_current);
  if (obj != NULL)
    w_current->edit_color = obj->color;

  /* The columns are: name of color, index of color. */
  store = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_INT);
  cbox = GTK_COMBO_BOX (gtk_combo_box_new_with_model (GTK_TREE_MODEL (store)));
  layout = GTK_CELL_LAYOUT (cbox); /* For convenience */

  /* Renders the color swatch. Since this won't contain text, set a
   * minimum width. */
  color_cell = GTK_CELL_RENDERER (gtk_cell_renderer_text_new());
  g_object_set (color_cell, "width", 25, NULL);
  gtk_cell_layout_pack_start (layout, color_cell, FALSE);
  gtk_cell_layout_set_cell_data_func (layout, color_cell,
                                      color_menu_swatch_layout_data,
                                      (gpointer) w_current,
                                      NULL);

  /* Renders the name of the color */
  text_cell = GTK_CELL_RENDERER (gtk_cell_renderer_text_new());
  g_object_set (text_cell, "xpad", 5, NULL);
  gtk_cell_layout_pack_start (layout, text_cell, TRUE);
  gtk_cell_layout_add_attribute (layout, text_cell, "text", 0);

  /* Populate the list */
  for (i = 0; i < MAX_COLORS; i++) {
    /* Skip 'invalid' colors. */
    if (!x_color_display_enabled(i)) continue;

    str = index2functionstring (i);
    gtk_list_store_append (store, &iter);
    gtk_list_store_set (store, &iter, 0, str, 1, i, -1);
    if (i == w_current->edit_color)
      gtk_combo_box_set_active_iter (cbox, &iter);
  }

  g_signal_connect (cbox,
                    "changed",
                    GTK_SIGNAL_FUNC (color_menu_change_selection),
                    w_current);

  return GTK_WIDGET (cbox);
}

/*! \brief Apply a color change to selected objects
 *  \par Function Description
 *  This function applies a color change to the currently selected objects.
 */
void color_edit_dialog_apply(GtkWidget *w, GSCHEM_TOPLEVEL *w_current)
{
  GList *s_current = NULL;
  OBJECT *object = NULL;

  s_current = geda_list_get_glist( w_current->toplevel->page_current->selection_list );

  while(s_current != NULL) {

    object = (OBJECT *) s_current->data;
    if (object == NULL) {
      fprintf(stderr, _("ERROR: NULL object in color_edit_dialog_apply!\n"));
      exit(-1);
    }

    o_set_color (w_current->toplevel, object, w_current->edit_color);
    w_current->toplevel->page_current->CHANGED = 1;

    s_current = g_list_next(s_current);
  }
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \brief response function for the color edit dialog
 *  \par Function Description
 *  This function takes the user response from the color edit dialog
 */
void color_edit_dialog_response(GtkWidget *widget, gint response, GSCHEM_TOPLEVEL *w_current)
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
void color_edit_dialog (GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *optionmenu;
  GtkWidget *label;
  GtkWidget *vbox;

  if (!w_current->clwindow) {
    w_current->clwindow = gschem_dialog_new_with_buttons(_("Color Edit"),
                                                         GTK_WINDOW(w_current->main_window),
                                                         0, /* nonmodal dialog */
                                                         "color-edit", w_current,
                                                         GTK_STOCK_CLOSE,
                                                         GTK_RESPONSE_REJECT,
                                                         GTK_STOCK_APPLY,
                                                         GTK_RESPONSE_ACCEPT,
                                                         NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->clwindow),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_window_position (GTK_WINDOW (w_current->clwindow),
                         GTK_WIN_POS_MOUSE);

    gtk_dialog_set_default_response (GTK_DIALOG (w_current->clwindow),
                                     GTK_RESPONSE_ACCEPT);

    g_signal_connect (G_OBJECT (w_current->clwindow), "response",
                      G_CALLBACK (color_edit_dialog_response),
                      w_current);

    gtk_container_border_width(GTK_CONTAINER(w_current->clwindow),
                               DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(w_current->clwindow)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    label = gtk_label_new(_("Object color:"));
    gtk_misc_set_alignment(GTK_MISC(label),0,0);
    gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);

    optionmenu = create_color_menu (w_current);
    gtk_box_pack_start(GTK_BOX(vbox),
                       optionmenu, FALSE, FALSE, 0);
    gtk_widget_show_all(w_current->clwindow);
  }

  else { /* dialog already created */
    gtk_window_present(GTK_WINDOW(w_current->clwindow));
  }
}

/***************** End of color edit dialog box *********************/

/***************** Start of help/keymapping dialog box **************/

/*! \brief Response function for the hotkey dialog
 *  \par Function Description
 *  This function destroys the hotkey dialog and does some cleanup.
 */
void x_dialog_hotkeys_response(GtkWidget *w, gint response,
                               GSCHEM_TOPLEVEL *w_current)
{
  switch(response) {
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    /* void */
    break;
  default:
    printf("x_dialog_hotkeys_response(): strange signal %d\n", response);
  }
  /* clean up */
  gtk_widget_destroy(w_current->hkwindow);
  w_current->hkwindow = NULL;
}

/*! \brief Creates the hotkeys dialog
 *  \par Function Description
 *  This function creates the hotkey dialog and puts the list of hotkeys
 *  into it.
 */
void x_dialog_hotkeys (GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *vbox, *scrolled_win;
  GtkListStore *store;
  GtkWidget *treeview;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;

  if (!w_current->hkwindow) {
    w_current->hkwindow = gschem_dialog_new_with_buttons(_("Hotkeys"),
                                                         GTK_WINDOW(w_current->main_window),
                                                         0, /* not modal */
                                                         "hotkeys", w_current,
                                                         GTK_STOCK_CLOSE,
                                                         GTK_RESPONSE_REJECT,
                                                         NULL);

    gtk_window_position (GTK_WINDOW (w_current->hkwindow),
                         GTK_WIN_POS_NONE);

    g_signal_connect (G_OBJECT (w_current->hkwindow), "response",
                      G_CALLBACK (x_dialog_hotkeys_response),
                      w_current);

    gtk_dialog_set_default_response(GTK_DIALOG(w_current->hkwindow),
                                    GTK_RESPONSE_ACCEPT);

    gtk_container_border_width (GTK_CONTAINER (w_current->hkwindow),
                                DIALOG_BORDER_SPACING);
    gtk_widget_set_usize(w_current->hkwindow, 300,300);

    vbox = GTK_DIALOG(w_current->hkwindow)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    scrolled_win = gtk_scrolled_window_new (NULL, NULL);
    gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);
    gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_win),
                                    GTK_POLICY_AUTOMATIC,
                                    GTK_POLICY_AUTOMATIC);

    /* the model */
    store = g_keys_to_list_store ();

    /* the tree view */
    treeview = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
    gtk_container_add(GTK_CONTAINER(scrolled_win), treeview);

    /* the columns */
    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes (_("Function"),
                                                       renderer,
                                                       "text",
                                                       0,
                                                       NULL);
    gtk_tree_view_append_column (GTK_TREE_VIEW(treeview), column);
    renderer = gtk_cell_renderer_text_new ();
    column = gtk_tree_view_column_new_with_attributes (_("Keystroke(s)"),
                                                       renderer,
                                                       "text",
                                                       1,
                                                       NULL);
    gtk_tree_view_append_column (GTK_TREE_VIEW(treeview), column);

    /* show all recursively */
    gtk_widget_show_all(w_current->hkwindow);
  }

  else { /* dialog already created */
    gtk_window_present(GTK_WINDOW(w_current->hkwindow));
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
void x_dialog_raise_all(GSCHEM_TOPLEVEL *w_current)
{
  if(w_current->sowindow) {
    gdk_window_raise(w_current->sowindow->window);
  }
  if(w_current->cswindow) {
    gdk_window_raise(w_current->cswindow->window);
  }
  if(w_current->iwindow) {
    gdk_window_raise(w_current->iwindow->window);
  }
  if(w_current->tiwindow) {
    gdk_window_raise(w_current->tiwindow->window);
  }
  if(w_current->tewindow) {
    gdk_window_raise(w_current->tewindow->window);
  }
  if(w_current->sewindow) {
    gdk_window_raise(w_current->sewindow->window);
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
  if(w_current->hkwindow) {
    gdk_window_raise(w_current->hkwindow->window);
  }
  if(w_current->cowindow) {
    gdk_window_raise(w_current->cowindow->window);
  }
  if(w_current->clwindow) {
    gdk_window_raise(w_current->clwindow->window);
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
                                   GTK_DIALOG_MODAL |
                                     GTK_DIALOG_DESTROY_WITH_PARENT,
                                   GTK_MESSAGE_INFO,
                                   GTK_BUTTONS_OK,
                                   "%s", msg);

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
                                   GTK_DIALOG_MODAL |
                                     GTK_DIALOG_DESTROY_WITH_PARENT,
                                   GTK_MESSAGE_INFO,
                                   GTK_BUTTONS_OK_CANCEL,
                                   "%s", msg);

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
 *   Caller must g_free returned character string.
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

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GTK_RESPONSE_OK,
                                          GTK_RESPONSE_CANCEL,
                                          -1);

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

/*********** Start of find text dialog box *******/

int start_find;
PAGE *remember_page;

/*! \brief response function for the find text dialog
 *  \par Function Description
 *  This function takes the string the user likes to find and searches it
 *  in the schematic.
 */
void find_text_dialog_response(GtkWidget *w, gint response,
                               GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GtkWidget *textentry;
  GtkWidget *checkdescend;
  gchar *string;
  gint done=0, close=0;

  switch (response) {
  case GTK_RESPONSE_ACCEPT:
    textentry = g_object_get_data(G_OBJECT(w_current->tfindwindow),"textentry");
    string = (gchar*) gtk_entry_get_text(GTK_ENTRY(textentry));
    checkdescend = g_object_get_data(G_OBJECT(w_current->tfindwindow),"checkdescend");

    strncpy(generic_textstring, string, 256);

    if (remember_page != toplevel->page_current) {
      s_page_goto(toplevel, remember_page);
    }
    done =
      o_edit_find_text (w_current, s_page_objects (remember_page), string,
                        gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON
                                                     (checkdescend)),
                        !start_find);
    if (done) {
      o_invalidate_all (w_current);
      close = 1;
    }
    start_find = 0;
    break;
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    close = 1;
    break;
  default:
    printf("find_text_dialog_response(): strange signal %d\n", response);
  }
  if (close) {
    gtk_widget_destroy(w_current->tfindwindow);
    w_current->tfindwindow = NULL;
  }
}

/*! \brief Create the text find dialog
 *  \par Function Description
 *  This function creates the text find dialog.
 */
void find_text_dialog(GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *label = NULL;
  GtkWidget *vbox;
  GtkWidget *checkdescend;
  GtkWidget *textentry;
  OBJECT *object = NULL;

  start_find = 1;
  remember_page = w_current->toplevel->page_current;
  if ((object = o_select_return_first_object(w_current)) != NULL) {
    if (object->type == OBJ_TEXT) {
      strncpy (generic_textstring,
               o_text_get_string (w_current->toplevel, object),
               256);
    }
  }

  if (!w_current->tfindwindow) {
    w_current->tfindwindow = gschem_dialog_new_with_buttons(_("Find Text"),
                                                            GTK_WINDOW(w_current->main_window),
                                                            0, /* not modal GTK_DIALOG_MODAL */
                                                            "find-text", w_current,
                                                            GTK_STOCK_CLOSE,
                                                            GTK_RESPONSE_REJECT,
                                                            GTK_STOCK_FIND,
                                                            GTK_RESPONSE_ACCEPT,
                                                            NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->tfindwindow),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_window_position(GTK_WINDOW(w_current->tfindwindow),
                        GTK_WIN_POS_MOUSE);

    g_signal_connect (G_OBJECT (w_current->tfindwindow), "response",
                      G_CALLBACK (find_text_dialog_response),
                      w_current);

    gtk_dialog_set_default_response(GTK_DIALOG(w_current->tfindwindow),
                                     GTK_RESPONSE_ACCEPT);

    gtk_container_border_width(GTK_CONTAINER(w_current->tfindwindow),
                               DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(w_current->tfindwindow)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    label = gtk_label_new(_("Text to find:"));
    gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

    textentry = gtk_entry_new_with_max_length(20);
    gtk_editable_select_region(GTK_EDITABLE(textentry), 0, -1);
    gtk_box_pack_start(GTK_BOX(vbox), textentry, FALSE, FALSE, 0);
    gtk_entry_set_activates_default(GTK_ENTRY(textentry), TRUE);
    gtk_widget_grab_focus(textentry);

    checkdescend = gtk_check_button_new_with_label(_("descend into hierarchy"));
    gtk_box_pack_start(GTK_BOX(vbox), checkdescend, TRUE, TRUE, 0);

    GLADE_HOOKUP_OBJECT(w_current->tfindwindow, textentry, "textentry");
    GLADE_HOOKUP_OBJECT(w_current->tfindwindow, checkdescend, "checkdescend");

    gtk_widget_show_all(w_current->tfindwindow);
  }

  else { /* dialog already created */
    gtk_window_present(GTK_WINDOW(w_current->tfindwindow));
  }

  /* always select the text string in the entry */
  textentry = g_object_get_data (G_OBJECT (w_current->tfindwindow), "textentry");
  gtk_entry_set_text(GTK_ENTRY(textentry), generic_textstring);
  gtk_entry_select_region(GTK_ENTRY(textentry), 0, -1);
}

/*********** End of find text dialog box *******/

/*********** Start of hide text dialog box *******/

/*! \brief Response function for the hide text dialog
 *  \par Function Description
 *  This is the response function of the hide text dialog. It takes the user input
 *  and hides all text elements that starts with the searchtext.
 */
void hide_text_dialog_response(GtkWidget *w, gint response,
                               GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *textentry;
  gchar *string;

  switch (response) {
  case GTK_RESPONSE_ACCEPT:
    textentry = g_object_get_data(G_OBJECT(w_current->thidewindow),"textentry");
    string = (gchar*) gtk_entry_get_text(GTK_ENTRY(textentry));

    strncpy(generic_textstring, string, 256);
    o_edit_hide_specific_text (w_current,
                               s_page_objects (w_current->toplevel->page_current),
                               string);
    break;
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    gtk_widget_destroy(w_current->thidewindow);
    w_current->thidewindow = NULL;
    break;
  default:
    printf("show_text_dialog_response(): strange signal %d\n",response);
  }
}

/*! \brief Creates the hide text dialog
 *  \par Function Description
 *  This function creates the hide text dialog.
 */
void hide_text_dialog(GSCHEM_TOPLEVEL * w_current)
{
  GtkWidget *label = NULL;
  GtkWidget *textentry;
  GtkWidget *vbox;

  if (!w_current->thidewindow) {
    w_current->thidewindow = gschem_dialog_new_with_buttons(_("Hide Text"),
                                                           GTK_WINDOW(w_current->main_window),
                                                           0, /* not modal GTK_DIALOG_MODAL, */
                                                           "hide-text", w_current,
                                                           GTK_STOCK_CLOSE,
                                                           GTK_RESPONSE_REJECT,
                                                           GTK_STOCK_APPLY,
                                                           GTK_RESPONSE_ACCEPT,
                                                           NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->thidewindow),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_window_position(GTK_WINDOW(w_current->thidewindow),
                        GTK_WIN_POS_MOUSE);

    g_signal_connect (G_OBJECT (w_current->thidewindow), "response",
                      G_CALLBACK (hide_text_dialog_response),
                      w_current);

    gtk_dialog_set_default_response(GTK_DIALOG(w_current->thidewindow),
                                    GTK_RESPONSE_ACCEPT);

    gtk_container_border_width(GTK_CONTAINER(w_current->thidewindow),
                               DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(w_current->thidewindow)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    label = gtk_label_new(_("Hide text starting with:"));
    gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

    textentry = gtk_entry_new_with_max_length(20);
    gtk_box_pack_start(GTK_BOX(vbox), textentry, FALSE, FALSE, 0);
    gtk_entry_set_activates_default(GTK_ENTRY(textentry), TRUE);
    gtk_widget_grab_focus(textentry);

    GLADE_HOOKUP_OBJECT(w_current->thidewindow, textentry, "textentry");
    gtk_widget_show_all(w_current->thidewindow);
  }

  else { /* dialog already created, just select it */
    gtk_window_present(GTK_WINDOW(w_current->thidewindow));
  }

  /* always select the text in the search entry */
  textentry = g_object_get_data (G_OBJECT (w_current->thidewindow), "textentry");
  gtk_entry_set_text(GTK_ENTRY(textentry), generic_textstring);
  gtk_entry_select_region(GTK_ENTRY(textentry), 0, -1);
}

/*********** End of hide text dialog box *******/

/*********** Start of show text dialog box *******/

/*! \brief Response function for the show text dialog
 *  \par Function Description
 *  This function takes the users input and searches all strings starting with
 *  the given search text and hides those text objects.
 */
void show_text_dialog_response(GtkWidget *widget, gint response,
                               GSCHEM_TOPLEVEL *w_current)
{
  GtkWidget *textentry;
  gchar *string;

  switch (response) {
  case GTK_RESPONSE_ACCEPT:
    textentry = g_object_get_data(G_OBJECT(w_current->tshowwindow),"textentry");
    string = (gchar*) gtk_entry_get_text(GTK_ENTRY(textentry));

    strncpy(generic_textstring, string, 256);
    o_edit_show_specific_text (w_current,
                               s_page_objects (w_current->toplevel->page_current),
                               string);
    break;
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    gtk_widget_destroy(w_current->tshowwindow);
    w_current->tshowwindow = NULL;
    break;
  default:
    printf("show_text_dialog_response(): strange signal %d\n",response);
  }
}

/*! \brief Create the show text dialog.
 *  \par Function Description
 *  This function creates the show text dialog.
 */
void show_text_dialog(GSCHEM_TOPLEVEL * w_current)
{
  GtkWidget *label = NULL;
  GtkWidget *textentry;
  GtkWidget *vbox;

  if (!w_current->tshowwindow) {
    w_current->tshowwindow = gschem_dialog_new_with_buttons(_("Show Text"),
                                                            GTK_WINDOW(w_current->main_window),
                                                            0, /* not modal GTK_DIALOG_MODAL, */
                                                            "show-text", w_current,
                                                            GTK_STOCK_CLOSE,
                                                            GTK_RESPONSE_REJECT,
                                                            GTK_STOCK_APPLY,
                                                            GTK_RESPONSE_ACCEPT,
                                                            NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
    gtk_dialog_set_alternative_button_order(GTK_DIALOG(w_current->tshowwindow),
                                            GTK_RESPONSE_ACCEPT,
                                            GTK_RESPONSE_REJECT,
                                            -1);

    gtk_window_position(GTK_WINDOW(w_current->tshowwindow),
                        GTK_WIN_POS_MOUSE);

    g_signal_connect (G_OBJECT (w_current->tshowwindow), "response",
                      G_CALLBACK (show_text_dialog_response),
                      w_current);

    gtk_dialog_set_default_response(GTK_DIALOG(w_current->tshowwindow),
                                    GTK_RESPONSE_ACCEPT);

    gtk_container_border_width(GTK_CONTAINER(w_current->tshowwindow),
                               DIALOG_BORDER_SPACING);
    vbox = GTK_DIALOG(w_current->tshowwindow)->vbox;
    gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

    label = gtk_label_new(_("Show text starting with:"));
    gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
    gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

    textentry = gtk_entry_new_with_max_length(20);
    gtk_box_pack_start(GTK_BOX(vbox), textentry, FALSE, FALSE, 0);
    gtk_entry_set_activates_default(GTK_ENTRY(textentry), TRUE);
    gtk_widget_grab_focus(textentry);

    GLADE_HOOKUP_OBJECT(w_current->tshowwindow, textentry, "textentry");
    gtk_widget_show_all(w_current->tshowwindow);
  }

  else { /* dialog already created. Show it */
    gtk_window_present(GTK_WINDOW(w_current->tshowwindow));
  }

  /* always select the text in the entry */
  textentry = g_object_get_data (G_OBJECT (w_current->tshowwindow), "textentry");
  gtk_entry_set_text(GTK_ENTRY(textentry), generic_textstring);
  gtk_entry_select_region(GTK_ENTRY(textentry), 0, -1);
}

/*********** End of show text dialog box *******/

/*********** Start of some Gtk utils  *******/

/*! \brief Selects all text in a TextView widget
 *  \par Function Description
 *  The function selects all the text in a TextView widget.
 */
void select_all_text_in_textview(GtkTextView *textview)
{
  GtkTextBuffer *textbuffer;
  GtkTextIter start, end;

  textbuffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(textview));
  gtk_text_buffer_get_bounds (textbuffer, &start, &end);
  gtk_text_buffer_select_range(textbuffer, &start, &end);
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
  gint tab_width = 0;

  if (tab_size == 0)
  return -1;

  tab_string = g_strnfill (tab_size, ' ');

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
void major_changed_dialog(GSCHEM_TOPLEVEL* w_current)
{
  GtkWidget* dialog;
  char* refdes_string = NULL;
  char* tmp;

  if (w_current->toplevel->major_changed_refdes) {

    GList* current = w_current->toplevel->major_changed_refdes;
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

      current = g_list_next(current);
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

    g_free(refdes_string);
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
  PROP_SELECTED_PAGES
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
 *  \param [in] model The tree model.
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
 *  \param [in] model The tree model.
 *  \param [in] piter A pointer on a GtkTreeIter of model or NULL.
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
 *  \param [in] tree_column A GtkTreeColumn.
 *  \param [in] cell        The GtkCellRenderer that is being rendered by
 *                        tree_column.
 *  \param [in] tree_model  The GtkTreeModel being rendered.
 *  \param [in] iter        A GtkTreeIter of the current row rendered.
 *  \param [in] data        .
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
 *  \param [in] cell_renderer The GtkCellRendererToggle.
 *  \param [in] path          The GtkTreePath to the concerned row in model.
 *  \param [in] user_data     The dialog as user data.
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
 *  \param [in] dialog The dialog.
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
  const gchar *cstr;

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
      _("There are %d schematics with unsaved changes. "
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
  cstr = _("If you don't save, all your changes will be permanently lost.");
  label = GTK_WIDGET (g_object_new (GTK_TYPE_LABEL,
                                    /* GtkMisc */
                                    "xalign",     0.0,
                                    "yalign",     0.0,
                                    "selectable", TRUE,
                                    /* GtkLabel */
                                    "wrap",       TRUE,
                                    "label",      cstr,
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

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(dialog),
                                          GTK_RESPONSE_YES,
                                          GTK_RESPONSE_NO,
                                          GTK_RESPONSE_CANCEL,
                                          -1);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_YES);

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
 *  \param [in] model The tree model.
 *  \param [in] path  .
 *  \param [in] iter  .
 *  \param [in] data  A pointer on a GList* to fill.
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
 *  \param [in] dialog The dialog.
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
 *  \param [in] w_current The toplevel environment.
 *  \param [in] page      The page to close.
 */
void
x_dialog_close_changed_page (GSCHEM_TOPLEVEL *w_current, PAGE *page)
{
  GtkWidget *dialog;
  PAGE *keep_page;

  g_return_if_fail (page != NULL && page->CHANGED);

  keep_page = w_current->toplevel->page_current;

  dialog = GTK_WIDGET (g_object_new (TYPE_CLOSE_CONFIRMATION_DIALOG,
                                     "unsaved-page", page,
                                     NULL));
  /* set default response signal. This is usually triggered by the
     "Return" key */
  gtk_dialog_set_default_response(GTK_DIALOG(dialog),
                                  GTK_RESPONSE_YES);

  switch (gtk_dialog_run (GTK_DIALOG (dialog))) {
      case GTK_RESPONSE_NO:
        /* action selected: close without saving */
        /* close the page, discard changes */
        x_window_close_page (w_current, page);
        break;


      case GTK_RESPONSE_YES:
        /* action selected: save */
        s_page_goto (w_current->toplevel, page);
        i_callback_file_save(w_current, 0, NULL);
        /* has the page been really saved? */
        if (!page->CHANGED) {
          x_window_close_page (w_current, page);
        }
        /* no, user has cancelled the save and page has changes */
        /* do not close page */
        break;

      case GTK_RESPONSE_CANCEL:
        /* action selected: cancel */
        /* fall through */
      default:
        /* Hit when the user breaks out of the dialog with the escape key
         * or otherwise destroys the dialog window without a proper response */
        /* nothing to do */
        break;
  }
  gtk_widget_destroy (dialog);

  /* Switch back to the page we were on if it wasn't the one being closed */
  g_return_if_fail (keep_page != NULL);
  if (keep_page != page)
    s_page_goto (w_current->toplevel, keep_page);
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
 *  \param [in] w_current The toplevel environment.
 *  \returns TRUE if the window can be closed, FALSE otherwise.
 */
gboolean
x_dialog_close_window (GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *iter;
  GtkWidget *dialog;
  PAGE *p_current;
  PAGE *keep_page;
  GList *unsaved_pages, *p_unsaved;
  gboolean ret = FALSE;

  keep_page = toplevel->page_current;

  for ( iter = geda_list_get_glist( toplevel->pages ), unsaved_pages = NULL;
        iter != NULL;
        iter = g_list_next( iter ) ) {

    p_current = (PAGE*)iter->data;

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

  gtk_window_set_transient_for (GTK_WINDOW (dialog),
                                GTK_WINDOW (w_current->main_window));

  g_list_free (unsaved_pages);
  switch (gtk_dialog_run (GTK_DIALOG (dialog))) {
      case GTK_RESPONSE_NO:
        /* action selected: close without saving */
        /* discard changes, ok to close window */
        ret = TRUE;
        break;

      case GTK_RESPONSE_YES:
        /* action selected: save */
        g_object_get (dialog,
                      "selected-pages", &unsaved_pages,
                      NULL);
        for (p_unsaved = unsaved_pages, ret = TRUE;
             p_unsaved != NULL;
             p_unsaved = g_list_next (p_unsaved)) {
          p_current = (PAGE*)p_unsaved->data;

          s_page_goto (toplevel, p_current);
          i_callback_file_save(w_current, 0, NULL);
          /* if user cancelled previous, do not close window */
          ret &= !p_current->CHANGED;
        }
        g_list_free (unsaved_pages);
        break;

      case GTK_RESPONSE_CANCEL:
        /* action selected: cancel */
        /* fall through */
      default:
        /* Hit when the user breaks out of the dialog with the escape key
         * or otherwise destroys the dialog window without a proper response */
        ret = FALSE;
        break;
  }
  gtk_widget_destroy (dialog);

  /* Switch back to the page we were on */
  g_return_val_if_fail (keep_page != NULL, ret);
  s_page_goto (toplevel, keep_page);

  return ret;
}

/***************** End of Close Confirmation dialog box **************/


/***************** Start of misc helper dialog boxes **************/
/*! \brief Validate the input attribute
 *  \par Function Description
 *  This function validates the attribute and if it isn't valid
 *  pops up an error message box.
 *
 *  \param parent The parent window which spawned this dialog box.
 *  \param attribute The attribute to be validated.
 *  \returns TRUE if the attribute is valid, FALSE otherwise.
 */
int x_dialog_validate_attribute(GtkWindow* parent, char *attribute)
{
  GtkWidget* message_box;

  /* validate the new attribute */
  if (!o_attrib_string_get_name_value (attribute, NULL, NULL)) {
      message_box = gtk_message_dialog_new_with_markup (parent,
                                  GTK_DIALOG_DESTROY_WITH_PARENT,
                                  GTK_MESSAGE_ERROR,
                                  GTK_BUTTONS_CLOSE,
                                  _("<span weight=\"bold\" size=\"larger\">The input attribute \"%s\" is invalid\nPlease correct in order to continue</span>\n\nThe name and value must be non-empty.\nThe name cannot end with a space.\nThe value cannot start with a space."),
                                  attribute);
     gtk_window_set_title(GTK_WINDOW(message_box), _("Invalid Attribute"));
     gtk_dialog_run (GTK_DIALOG (message_box));
     gtk_widget_destroy (message_box);
     return FALSE;
  }
  return TRUE;
}
/***************** End of misc helper dialog boxes **************/

/*! \brief Edit the type of a pin (bus or net)
 *  \par Function Description
 *  This function presents an app modal dialog to edit the type of a pin
 */

void x_dialog_edit_pin_type (GSCHEM_TOPLEVEL *w_current, const GList *obj_list)
{
  GtkWidget *dialog;
  GtkWidget *vbox;
  GtkWidget *radio1;
  GtkWidget *radio2;
  const GList *iter;
  int new_type;
  int found_pins = FALSE;
  int changed_anything = FALSE;

  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *object = iter->data;
    if (object->type == OBJ_PIN) {
      found_pins = TRUE;
      break;
    }
  }

  if (!found_pins)
    return;

  dialog = gschem_dialog_new_with_buttons(_("Pin type"),
                                          GTK_WINDOW(w_current->main_window),
                                          GTK_DIALOG_MODAL,
                                          "pin-type-edit", w_current,
                                          GTK_STOCK_CANCEL,
                                          GTK_RESPONSE_CANCEL,
                                          GTK_STOCK_OK,
                                          GTK_RESPONSE_OK,
                                          NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order (GTK_DIALOG (dialog),
                                           GTK_RESPONSE_OK,
                                           GTK_RESPONSE_CANCEL,
                                           -1);

  gtk_window_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_CANCEL);

  gtk_container_border_width (GTK_CONTAINER (dialog), DIALOG_BORDER_SPACING);
  vbox = GTK_DIALOG (dialog)->vbox;
  gtk_box_set_spacing (GTK_BOX(vbox), DIALOG_V_SPACING);

  radio1 = gtk_radio_button_new_with_label (NULL, _("Net pin"));
  radio2 = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (radio1),
                                                        _("Bus pin (graphical)"));
  /* Pack them into a box, then show all the widgets */
  gtk_box_pack_start (GTK_BOX (vbox), radio1, TRUE, TRUE, 2);
  gtk_box_pack_start (GTK_BOX (vbox), radio2, TRUE, TRUE, 2);
  gtk_widget_show_all (vbox);

  switch (gtk_dialog_run (GTK_DIALOG (dialog))) {
    case GTK_RESPONSE_OK:
      new_type = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (radio1)) ?
                   PIN_TYPE_NET : PIN_TYPE_BUS;
      for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
        OBJECT *object = iter->data;

        if (object->type == OBJ_PIN &&
            object->pin_type != new_type) {
          changed_anything = TRUE;
          s_conn_remove_object (w_current->toplevel, object);
          o_pin_set_type (w_current->toplevel, object, new_type);
          s_conn_update_object (w_current->toplevel, object);
        }
      }
      if (changed_anything)
        o_undo_savestate (w_current, UNDO_ALL);
      break;

    case GTK_RESPONSE_CANCEL:
    default:
      /* Do nothing */
      break;
  }

  gtk_widget_destroy (dialog);
}

/***************** End of pin type edit dialog box *********************/
