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

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"
#include "../include/x_print.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

enum
  {
  PROP_FILENAME = 1,
  PROP_COMMAND,
  PROP_PAPERSIZE,
  PROP_ORIENTATION,
  PROP_TYPE,
  PROP_USEFILE
  };

/* Private functions */

static gboolean print_dialog_action_keypress (GtkWidget * widget,
                                             GdkEventKey * event,
                                             PrintDialog * dialog);
static void print_dialog_action_radio_toggled (GtkWidget * w,
                                              PrintDialog * dialog);

static void print_dialog_init (PrintDialog * dialog);
static void print_dialog_init_paper_combobox (PrintDialog * d);
static void print_dialog_init_type_combobox (PrintDialog * d);
static void print_dialog_init_orient_combobox (PrintDialog * d);
static void print_dialog_set_property (GObject * object, guint property_id,
                                      const GValue * value,
                                      GParamSpec * pspec);
static void print_dialog_set_property_comboboxes (PrintDialog *dialog,
						  GtkComboBox *cbox,
						  const GValue * value);
static void print_dialog_get_property (GObject * object, guint property_id,
                                      GValue * value, GParamSpec * pspec);
static void print_dialog_get_property_comboboxes (PrintDialog * dialog,
						  GtkComboBox * cbox,
						  GValue * value);
static void print_dialog_class_init (PrintDialogClass * class);



/*!
 *  \brief Callback function to show file chooser dialog
 *
 *  \par Shows file chooser dialog for user to select PostScript file
 *  to print to.
 *  \par Private callback function, should not be called by any code
 *  outside x_print.c
 */
static void
print_dialog_action_choosefile (GtkWidget * w, PrintDialog * dialog)
{
  GtkWidget *filechooser;
  const gchar *filename;
  const gchar *newfilename;
  filechooser = gtk_file_chooser_dialog_new (_("Select PostScript Filename..."),
					     GTK_WINDOW (dialog),
					     GTK_FILE_CHOOSER_ACTION_SAVE,
					     GTK_STOCK_CANCEL,
					     GTK_RESPONSE_CANCEL,
					     GTK_STOCK_OK,
					     GTK_RESPONSE_ACCEPT, NULL);

  filename = gtk_entry_get_text (GTK_ENTRY (dialog->fnfield));
  gtk_file_chooser_set_filename (GTK_FILE_CHOOSER (filechooser), filename);

  gtk_dialog_set_default_response(GTK_DIALOG(filechooser),
				  GTK_RESPONSE_ACCEPT);

  if (gtk_dialog_run (GTK_DIALOG (filechooser)) == GTK_RESPONSE_ACCEPT)
    {
      newfilename =
	gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (filechooser));
      gtk_entry_set_text (GTK_ENTRY (dialog->fnfield), newfilename);
    }

  gtk_widget_destroy (filechooser);

}

/*!
 *  \brief Handle keypress events caught by the print dialog.
  *
 *  \par Private callback function, should not be called by any code
 *  outside x_print.c
 */
static gboolean
print_dialog_action_keypress (GtkWidget * widget, GdkEventKey * event,
                              PrintDialog * dialog)
{
  if (widget == GTK_WIDGET (dialog))
    {
      
      if (strcmp (gdk_keyval_name (event->keyval), "Escape") == 0)
	{
	  gtk_dialog_response (GTK_DIALOG (dialog),
			       GTK_RESPONSE_REJECT);
	  return TRUE;
	}
      if (strcmp (gdk_keyval_name (event->keyval), "Return") == 0)
	{
	  gtk_dialog_response (GTK_DIALOG (dialog),
			       GTK_RESPONSE_ACCEPT);
	  return TRUE;
	}
    }
  return FALSE;
  
}

/*!
 *  \brief Create, initialize and populate a combobox for selecting
 *  what paper size to print to.
 *  \par Private function, should not be
 *  called by any code outside x_print.c
 */
static void
print_dialog_init_paper_combobox (PrintDialog * d)
{
  GtkComboBox *combobox;
  gchar *string;
  gint i;

  combobox = GTK_COMBO_BOX (gtk_combo_box_new_text ());
  gtk_combo_box_set_active (combobox, -1);

  /* Populate combo box with available paper sizes */
  i = 0;
  string = (gchar *) s_papersizes_get (i);
  while (string != NULL)
    {
      gtk_combo_box_insert_text (GTK_COMBO_BOX (combobox), i, string);
      
      i++;
      string = (gchar *) s_papersizes_get (i);
    }

  d->papercbox = combobox;
}

/*!
 *  \brief Create, initialize and populate a combobox for selecting
 *  the type of printout to produce.
 *  \par Private function, should not be called by any code
 *  outside x_print.c  
 */
static void
print_dialog_init_type_combobox (PrintDialog * d)
{
  GtkListStore *model;
  GtkTreeIter iter;
  GtkCellRenderer *renderer;

  GtkWidget *combobox;
  
  model = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_INT);

  gtk_list_store_append (model, &iter);
  gtk_list_store_set (model, &iter, 
		      0, _("Extents with margins"),
		      1, EXTENTS,
		      -1);
  
  gtk_list_store_append (model, &iter);
  gtk_list_store_set (model, &iter,
		      0, _("Extents no margins"),
		      1, EXTENTS_NOMARGINS,
		      -1);
  
  gtk_list_store_append (model, &iter);
  gtk_list_store_set (model, &iter,
		      0, _("Current Window"),
		      1, WINDOW,
		      -1);

  combobox = gtk_combo_box_new_with_model (GTK_TREE_MODEL (model));
  
  renderer = gtk_cell_renderer_text_new ();
  gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (combobox),
			      renderer, TRUE);
  gtk_cell_layout_add_attribute (GTK_CELL_LAYOUT (combobox),
				 renderer, "text", 0);

  d->typecbox = GTK_COMBO_BOX (combobox);
}

/*!
 *  \brief Create, initialize and populate a combobox for selecting
 *  paper orientation.
 *  \par Private function, should not be called by any code
 *  outside x_print.c  
 */
static void
print_dialog_init_orient_combobox (PrintDialog * d)
{
  GtkListStore *model;
  GtkTreeIter iter;
  GtkCellRenderer *renderer;

  GtkWidget *combobox;
  
  model = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_INT);

  gtk_list_store_append (model, &iter);
  gtk_list_store_set (model, &iter, 
		      0, _("Landscape"),
		      1, LANDSCAPE,
		      -1);
  
  gtk_list_store_append (model, &iter);
  gtk_list_store_set (model, &iter,
		      0, _("Portrait"),
		      1, PORTRAIT,
		      -1);

  combobox = gtk_combo_box_new_with_model (GTK_TREE_MODEL (model));
  
  renderer = gtk_cell_renderer_text_new ();
  gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (combobox),
			      renderer, TRUE);
  gtk_cell_layout_add_attribute (GTK_CELL_LAYOUT (combobox),
				 renderer, "text", 0);

  d->orientcbox = GTK_COMBO_BOX (combobox);
}

/*!
 *  \brief Handle the user clicking on radio buttons to select print
 *  destination.
 *
 *  \par Private callback function, should not be called by any code
 *  outside x_print.c
 */
static void
print_dialog_action_radio_toggled (GtkWidget * w, PrintDialog * dialog)
{
  if (w == GTK_WIDGET (dialog->cmdradio))
    {
      gtk_widget_set_sensitive (GTK_WIDGET (dialog->cmdfield),
                               gtk_toggle_button_get_active
                               (GTK_TOGGLE_BUTTON (w)));
    }
  else if (w == GTK_WIDGET (dialog->fileradio))
    {
      gtk_widget_set_sensitive (GTK_WIDGET (dialog->fnfield),
                               gtk_toggle_button_get_active
                               (GTK_TOGGLE_BUTTON (w)));
      gtk_widget_set_sensitive (GTK_WIDGET (dialog->saveasbutton),
                               gtk_toggle_button_get_active
                               (GTK_TOGGLE_BUTTON (w)));
    }
}


/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
print_dialog_init (PrintDialog * dialog)
{
  GtkWidget *box;
  GtkWidget *frame;
  GtkWidget *settingstable, *desttable;
  GtkWidget *label;

  /* Initialize properties */
  g_object_set (G_OBJECT (dialog),
		/* GtkWindow */
		"title", _("Print..."),
		"modal", TRUE, "destroy-with-parent", TRUE, NULL);

  /* Connect key-press event */
  g_signal_connect (dialog,
		    "key_press_event",
		    GTK_SIGNAL_FUNC (print_dialog_action_keypress), dialog);

  /* Setup hbox for two main panes */
  box = gtk_vbox_new (FALSE, 2);
  gtk_container_add (GTK_CONTAINER (GTK_DIALOG (dialog)->vbox), box);

  /* Upper frame */
  frame = gtk_frame_new (_("Settings"));
  gtk_container_set_border_width (GTK_CONTAINER (frame), 3);
  gtk_container_add (GTK_CONTAINER (box), frame);

  /* Upper table with drop-down menus & labels 
   * Left-hand column contains labels, right-hand contains comboboxes*/
  settingstable = gtk_table_new (2, 3, FALSE);
  gtk_table_set_col_spacings (GTK_TABLE (settingstable), 5);
  gtk_table_set_row_spacings (GTK_TABLE (settingstable), 5);
  gtk_container_set_border_width (GTK_CONTAINER (settingstable), 5);
  gtk_container_add (GTK_CONTAINER (frame), settingstable);

  label = gtk_label_new (_("Output paper size:"));
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0);
  gtk_table_attach (GTK_TABLE (settingstable),
		    label,
		    0, 1, 0, 1, GTK_EXPAND | GTK_FILL, GTK_EXPAND, 0, 0);

  print_dialog_init_paper_combobox (dialog);
  gtk_table_attach (GTK_TABLE (settingstable),
		    GTK_WIDGET (dialog->papercbox),
		    1, 2, 0, 1, GTK_FILL, 0, 0, 0);

  label = gtk_label_new (_("Type:"));
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0);
  gtk_table_attach (GTK_TABLE (settingstable),
		    label,
		    0, 1, 1, 2, GTK_EXPAND | GTK_FILL, GTK_EXPAND, 0, 0);

  print_dialog_init_type_combobox (dialog);
  gtk_table_attach (GTK_TABLE (settingstable),
		    GTK_WIDGET (dialog->typecbox),
		    1, 2, 1, 2, GTK_FILL, 0, 0, 0);

  label = gtk_label_new (_("Orientation:"));
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0);
  gtk_table_attach (GTK_TABLE (settingstable),
		    label,
		    0, 1, 2, 3, GTK_EXPAND | GTK_FILL, GTK_EXPAND, 0, 0);

  print_dialog_init_orient_combobox (dialog);
  gtk_table_attach (GTK_TABLE (settingstable),
		    GTK_WIDGET (dialog->orientcbox),
		    1, 2, 2, 3, GTK_FILL, 0, 0, 0);

  /* Lower frame */
  frame = gtk_frame_new (_("Destination"));
  gtk_container_set_border_width (GTK_CONTAINER (frame), 3);
  gtk_container_add (GTK_CONTAINER (box), frame);

  /* Table with destination selectors */
  desttable = gtk_table_new (3, 2, FALSE);
  gtk_table_set_col_spacings (GTK_TABLE (desttable), 5);
  gtk_table_set_row_spacings (GTK_TABLE (desttable), 5);
  gtk_container_set_border_width (GTK_CONTAINER (desttable), 5);
  gtk_container_add (GTK_CONTAINER (frame), desttable);

  /* Widgets for printing to file */
  dialog->fileradio =
    GTK_RADIO_BUTTON (gtk_radio_button_new_with_label (NULL, _("File:")));
  gtk_table_attach (GTK_TABLE (desttable),
		    GTK_WIDGET (dialog->fileradio),
		    0, 1, 0, 1, GTK_FILL, GTK_EXPAND, 0, 0);
  g_signal_connect (dialog->fileradio,
		    "toggled",
		    GTK_SIGNAL_FUNC (print_dialog_action_radio_toggled),
		    dialog);

  dialog->fnfield = GTK_ENTRY (gtk_entry_new ());
  gtk_table_attach (GTK_TABLE (desttable),
		    GTK_WIDGET (dialog->fnfield),
		    1, 2, 0, 1, GTK_EXPAND | GTK_FILL, 0, 0, 0);

  dialog->saveasbutton = GTK_BUTTON(gtk_button_new());
  gtk_container_add(GTK_CONTAINER(dialog->saveasbutton),
		    gtk_image_new_from_stock(GTK_STOCK_OPEN,
					     GTK_ICON_SIZE_SMALL_TOOLBAR));
  gtk_button_set_relief(GTK_BUTTON(dialog->saveasbutton), GTK_RELIEF_NONE);

  gtk_table_attach (GTK_TABLE (desttable),
		    GTK_WIDGET (dialog->saveasbutton), 2, 3, 0, 1,
		    GTK_FILL, 0, 0, 0);
  g_signal_connect (dialog->saveasbutton,
		    "clicked",
		    GTK_SIGNAL_FUNC (print_dialog_action_choosefile), dialog);

  /* Widgets for printing to command */
  dialog->cmdradio =
    GTK_RADIO_BUTTON (gtk_radio_button_new_with_label_from_widget
		      (dialog->fileradio, _("Command:")));
  gtk_table_attach (GTK_TABLE (desttable),
		    GTK_WIDGET (dialog->cmdradio),
		    0, 1, 1, 2,  GTK_FILL, GTK_EXPAND, 0, 0);
  g_signal_connect (dialog->cmdradio,
		    "toggled",
		    GTK_SIGNAL_FUNC (print_dialog_action_radio_toggled),
		    dialog);

  dialog->cmdfield = GTK_ENTRY (gtk_entry_new ());
  gtk_table_attach (GTK_TABLE (desttable), GTK_WIDGET (dialog->cmdfield),
		    1, 3, 1, 2, GTK_EXPAND | GTK_FILL, 0, 0, 0);

  /* Add "Cancel" and "Print" buttons */
  gtk_dialog_add_buttons (GTK_DIALOG (dialog),
			  GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT,
			  GTK_STOCK_PRINT, GTK_RESPONSE_ACCEPT,
			  NULL);

  /* Set initial radiobutton selection */
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (dialog->cmdradio), TRUE);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
print_dialog_set_property (GObject * object,
                          guint property_id,
                          const GValue * value, GParamSpec * pspec)
{
  PrintDialog *dialog = PRINT_DIALOG (object);
  gboolean file_active = FALSE;

  switch (property_id)
    {
    case PROP_FILENAME:
      gtk_entry_set_text (dialog->fnfield,
                         (char *) g_value_get_string (value));
      return;

    case PROP_COMMAND:
      gtk_entry_set_text (dialog->cmdfield,
                         (char *) g_value_get_string (value));
      return;

    case PROP_PAPERSIZE:
      gtk_combo_box_set_active (dialog->papercbox,
                               g_value_get_int (value));
      return;

    case PROP_ORIENTATION:
      print_dialog_set_property_comboboxes (dialog, 
					    dialog->orientcbox,
					    value);
      return;

    case PROP_TYPE:
      print_dialog_set_property_comboboxes (dialog, 
					    dialog->typecbox,
					    value);
      return;

    case PROP_USEFILE:
      file_active = g_value_get_boolean (value);
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (dialog->fileradio),
                                   file_active);
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (dialog->cmdradio),
                                   !file_active);
      return;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
    }
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
print_dialog_set_property_comboboxes (PrintDialog * dialog,
				      GtkComboBox * cbox,
				      const GValue * value)
{
  GtkTreeIter iter;
  GtkTreeModel *model;

  model = gtk_combo_box_get_model (cbox);
  gtk_tree_model_get_iter_first (model, &iter);

  do {
    GValue temp_value = {0, }; /* Make sure it's blank*/
    gtk_tree_model_get_value (model, &iter, 1, &temp_value);

    if (g_value_get_int (&temp_value) == g_value_get_int (value))
      {
	gtk_combo_box_set_active_iter (cbox, &iter);
	return;
      }

  } while (gtk_tree_model_iter_next (model, &iter));

  gtk_combo_box_set_active (cbox, 0);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
print_dialog_get_property (GObject * object,
                          guint property_id,
                          GValue * value, GParamSpec * pspec)
{
  PrintDialog *dialog = PRINT_DIALOG (object);
  gboolean file_active = FALSE;

  switch (property_id)
    {
    case PROP_FILENAME:
      g_value_set_string (value,
			  gtk_entry_get_text (GTK_ENTRY (dialog->fnfield)));
      return;
 
    case PROP_COMMAND:
      g_value_set_string (value,
			  gtk_entry_get_text (GTK_ENTRY (dialog->cmdfield)));
      return;
 
    case PROP_PAPERSIZE:
      g_value_set_int (value, gtk_combo_box_get_active (dialog->papercbox));
      return;
 
    case PROP_ORIENTATION:
      print_dialog_get_property_comboboxes (dialog,
					    dialog->orientcbox,
					    value);
      return;
      
    case PROP_TYPE:
      print_dialog_get_property_comboboxes (dialog,
					    dialog->typecbox,
					    value);
      return;
 
    case PROP_USEFILE:
      file_active = 
        gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (dialog->fileradio));
      g_value_set_boolean (value, file_active);
      return;
            
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
    }
} 

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
print_dialog_get_property_comboboxes (PrintDialog * dialog,
				      GtkComboBox * cbox,
				      GValue * value)
{
  GValue temp_value = {0, };
  GtkTreeModel *model;
  GtkTreeIter iter;

  model = gtk_combo_box_get_model (cbox);

  gtk_combo_box_get_active_iter (cbox, &iter);
  gtk_tree_model_get_value (model, &iter, 1, &temp_value);
  g_value_copy (&temp_value, value);
  g_value_unset (&temp_value);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *  \bug Hardcoded 'magic' numbers in this function
 *
 */
static void
print_dialog_class_init (PrintDialogClass * class)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (class);

  gobject_class->set_property = print_dialog_set_property;
  gobject_class->get_property = print_dialog_get_property;

  g_object_class_install_property (gobject_class, PROP_FILENAME,
				   g_param_spec_string ("filename",
							"", "", "",
							 G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_COMMAND,
				   g_param_spec_string ("command",
							"", "", "lpr",
							 G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_PAPERSIZE,
				   g_param_spec_int ("papersize",
						     "", "", 0, G_MAXINT, 0,
						     G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_ORIENTATION,
				   g_param_spec_int ("orientation",
						     "", "", 0, G_MAXINT, 0,
						     G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_TYPE,
				   g_param_spec_int ("type",
						     "", "", 0, G_MAXINT, 0,
						     G_PARAM_READWRITE));

  g_object_class_install_property (gobject_class, PROP_USEFILE,
				   g_param_spec_boolean ("usefile",
							 "", "", FALSE,
							 G_PARAM_READWRITE));
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
GType
print_dialog_get_type ()
{
  static GType print_dialog_type = 0;
 
  if (!print_dialog_type)
    {
      static const GTypeInfo print_dialog_info = {
	sizeof (PrintDialogClass),
	NULL,			/* base_init */
	NULL,			/* base_finalize */
	(GClassInitFunc) print_dialog_class_init,
	NULL,			/* class_finalize */
	NULL,			/* class_data */
	sizeof (PrintDialog),
	0,			/* n_preallocs */
	(GInstanceInitFunc) print_dialog_init,
      };
      print_dialog_type = g_type_register_static (GTK_TYPE_DIALOG,
						  "PrintDialog",
						  &print_dialog_info, 0);
    }

  return print_dialog_type;
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
void
x_print_setup (TOPLEVEL * w_current, char *filename)
{
  gchar * command = w_current->print_command;
  gint orient = w_current->print_orientation;
  gint type = w_current->print_output_type;
  gint paperidx, x, y, result;
  gchar *string, *destination;
  gboolean usefile = FALSE;
  GtkDialog *dialog; 
  GtkWidget *popup_message;

  /* Work out current paper size by iterating through available paper
   * sizes.  Set the default paper size as the active selection */

  /* FIXME: ought to have a TOPLEVEL property containing
   * default paper size name, this is somewhat hackish. No
   * better way of doing it with current implementation of
   * varying paper size though. */
  paperidx = 0;
  while (TRUE)
    {
      string = (gchar *) s_papersizes_get (paperidx);
      s_papersizes_get_size (string, &x, &y);

      if ((x == w_current->paper_width)
	  && (y == w_current->paper_height))
	{
	  break;
	}
      if (string == NULL)
	{
	  paperidx = 0;
	  break;
	}
      paperidx++;
    }
 
  /* Create a print dialog, find out whether the user clicks Print or
     Cancel, and then print or return accordingly */
  dialog = GTK_DIALOG (g_object_new (TYPE_PRINT_DIALOG,
						"command", command,
						"filename", filename,
						"papersize", paperidx,
						"orientation", orient,
						"type", type,
						"usefile", usefile,
						 NULL));
  gtk_widget_show_all (GTK_WIDGET (dialog));

  gtk_dialog_set_default_response(GTK_DIALOG(dialog),
				  GTK_RESPONSE_ACCEPT);
  gtk_window_set_transient_for(GTK_WINDOW(dialog),
			       GTK_WINDOW(w_current->main_window));

  result = gtk_dialog_run (dialog);
  
  if (result == GTK_RESPONSE_ACCEPT)
    {
      /* Extract values from dialog and set the paper size */
      g_object_get (dialog,
		    "command", &command,
		    "filename", &filename,
		    "papersize", &paperidx,
		    "orientation", &w_current->print_orientation,
		    "type", &w_current->print_output_type,
		    "usefile", &usefile,
		    NULL);

      s_papersizes_get_size (s_papersizes_get (paperidx),
			     &w_current->paper_width,
			     &w_current->paper_height);
		
      /* de select everything first */
      o_selection_unselect_list( w_current,
                                 &w_current->page_current->selection_list );

      if (usefile && filename[0])
	/* Print to file */
	{
	  destination = filename;
	  result = f_print_file (w_current, filename);
	}
      else if (command[0])
	/* Print to command and save command for later use. */
	{
	  destination = command;
	  result = f_print_command (w_current, command);
	  
	  g_free (w_current->print_command);
	  w_current->print_command = g_strdup (command);
	}
      else
	{
	  s_log_message (_("No print destination specified\n"));
	  return;
	}

      /* Check whether it worked */
      if (result)
	{
	  s_log_message (_("Cannot print current schematic to [%s]\n"), 
			 destination);

	  /* Pop up a message warning the user */
	  popup_message = 
	    gtk_message_dialog_new (GTK_WINDOW(dialog),
				    GTK_DIALOG_DESTROY_WITH_PARENT,
				    GTK_MESSAGE_ERROR,
				    GTK_BUTTONS_CLOSE,
				    _("Error printing to file '%s'\n"
				      "Check the log window for more information"),
				    destination);
	  gtk_dialog_run (GTK_DIALOG (popup_message));	  
	}
      else
	{
	  s_log_message (_("Printed current schematic to [%s]\n"), 
			 destination);
	}
    }
   
  /* We don't need the dialog any more */
  gtk_widget_destroy (GTK_WIDGET (dialog));

}
