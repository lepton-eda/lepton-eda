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

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/gschem_struct.h"
#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#include "../include/gschem_dialog.h"
#include "../include/x_log.h"

static void x_log_callback_response (GtkDialog *dialog,
                                     gint arg1,
                                     gpointer user_data);

static GtkWidget *log_dialog = NULL;

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void x_log_open ()
{
  if (log_dialog == NULL) {
    gchar *contents;
    
    log_dialog = GTK_WIDGET (g_object_new (TYPE_LOG,
                                           /* GschemDialog */
                                           "settings-name", "log",
                                           /* "toplevel", TOPEVEL * */
                                           NULL));

    g_signal_connect (log_dialog,
                      "response",
                      G_CALLBACK (x_log_callback_response),
                      NULL);

    /* make it read the content of the current log file */
    /* and add its contents to the dialog */
    contents = s_log_read ();

    /* s_log_read can return NULL if the log file cannot be written to */
    if (contents == NULL)
    {
      return;
    }

    log_message (LOG (log_dialog), contents);
    g_free (contents);

    x_log_update_func = x_log_message;
   
    if( auto_place_mode )
	gtk_widget_set_uposition( log_dialog, 10, 10); 
    gtk_widget_show (log_dialog);
  } else {
    g_assert (IS_LOG (log_dialog));
    gtk_window_present ((GtkWindow*)log_dialog);
  }

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void x_log_close ()
{
  if (log_dialog) {
    g_assert (IS_LOG (log_dialog));
    gtk_widget_destroy (log_dialog);
    x_log_update_func = NULL;
    log_dialog = NULL;
  }

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void x_log_message (const gchar *log_domain, GLogLevelFlags log_level,
                    const gchar *message)
{
  g_return_if_fail (log_dialog != NULL);
  
  g_assert (IS_LOG (log_dialog));
  log_message ((Log*)log_dialog, message);
    
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
static void x_log_callback_response (GtkDialog *dialog,
				     gint arg1,
				     gpointer user_data)
{
  switch (arg1) {
    case GTK_RESPONSE_DELETE_EVENT:
    case LOG_RESPONSE_CLOSE:
    g_assert (GTK_WIDGET (dialog) == log_dialog);
    x_log_close ();
    break;
    default:
    g_assert_not_reached ();
  }
  
}

static void log_class_init (LogClass *class);
static void log_init       (Log *log);

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
void log_message (Log *log, const gchar *message)
{
  GtkTextBuffer *buffer;
  GtkTextIter iter;
  GtkTextMark *mark;
  
  g_return_if_fail (IS_LOG (log));

  buffer = gtk_text_view_get_buffer (log->textview);
  gtk_text_buffer_get_end_iter (buffer, &iter);
  gtk_text_buffer_insert(buffer, &iter, message, -1);

  mark = gtk_text_buffer_create_mark(buffer, NULL, &iter, FALSE);
  gtk_text_view_scroll_to_mark (log->textview, mark, 0, TRUE, 0, 1);
  gtk_text_buffer_delete_mark (buffer, mark);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
GType log_get_type ()
{
  static GType log_type = 0;
  
  if (!log_type) {
    static const GTypeInfo log_info = {
      sizeof(LogClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) log_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof(Log),
      0,    /* n_preallocs */
      (GInstanceInitFunc) log_init,
    };
		
    log_type = g_type_register_static (GSCHEM_TYPE_DIALOG,
                                       "Log",
                                       &log_info, 0);
  }
  
  return log_type;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
static void log_class_init (LogClass *klass)
{
/*   GObjectClass *gobject_class = G_OBJECT_CLASS (klass); */
	
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 * 
 */
static void log_init (Log *log)
{
  GtkWidget *scrolled_win, *text_view;
  GtkTextBuffer *text_buffer;
  GtkTextMark *mark;

  /* dialog initialization */
  g_object_set (G_OBJECT (log),
                /* GtkContainer */
                "border-width",    0,
                /* GtkWindow */
                "type",            GTK_WINDOW_TOPLEVEL,
                "title",           _("Status"),
                "default-width",   600,
                "default-height",  200,
                "modal",           FALSE,
                "window-position", GTK_WIN_POS_NONE,
                "type-hint",       GDK_WINDOW_TYPE_HINT_NORMAL,
                /* GtkDialog */
                "has-separator",   TRUE,
                NULL);

  /* create a scrolled window for the textview */
  scrolled_win = GTK_WIDGET (
    g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                  /* GtkContainer */
                  "border-width",      5,
                  /* GtkScrolledWindow */
                  "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                  "vscrollbar-policy", GTK_POLICY_AUTOMATIC,
                  "shadow-type",       GTK_SHADOW_ETCHED_IN,
                  NULL));
  /* create the text buffer */
  text_buffer = GTK_TEXT_BUFFER (g_object_new (GTK_TYPE_TEXT_BUFFER,
                                               NULL));
  /* create the text view and attach the buffer to it */
  text_view = GTK_WIDGET (g_object_new (GTK_TYPE_TEXT_VIEW,
                                        /* GtkTextView */
/* unknown property in GTK 2.2, use gtk_text_view_set_buffer() instead */
/*                                         "buffer",   text_buffer, */
                                        "editable", FALSE,
                                        NULL));
  gtk_text_view_set_buffer (GTK_TEXT_VIEW (text_view), text_buffer);

  /* add the text view to the scrolled window */
  gtk_container_add (GTK_CONTAINER (scrolled_win), text_view);
  /* set textview of log */
  log->textview = GTK_TEXT_VIEW (text_view);

  /* add the scrolled window to the dialog vbox */
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (log)->vbox), scrolled_win,
                      TRUE, TRUE, 0);
  gtk_widget_show_all (scrolled_win);

  /* now add the close button to the action area */
  gtk_dialog_add_button (GTK_DIALOG (log),
                         GTK_STOCK_CLOSE, LOG_RESPONSE_CLOSE);

  /* scroll to the end of the buffer */
  mark = gtk_text_buffer_get_insert (text_buffer);
  gtk_text_view_scroll_to_mark (GTK_TEXT_VIEW (text_view), mark, 0.0, TRUE, 0.0, 1.0);
}
