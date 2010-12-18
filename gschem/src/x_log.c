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

/*
 * \file x_log.c
 * \brief GType class and functions to support the gschem log window.
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

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

static void x_log_callback_response (GtkDialog *dialog,
                                     gint arg1,
                                     gpointer user_data);
static void log_message (Log *log, 
                         const gchar *message, 
                         const gchar *style);

static void log_class_init (LogClass *class);
static void log_init       (Log *log);

static GtkWidget *log_dialog = NULL;

/*!
 *  \brief Open the Log window
 *
 *  \par Function Description
 *  If the log dialog instance doesn't exist, create it, and read
 *  the current log file contents (if they exist) and insert
 *  them into the log dialog.
 * 
 *  If the log dialog instance does exist, present it to the user.
 */
void x_log_open ()
{
  if (log_dialog == NULL) {
    gchar *contents;
    
    log_dialog = GTK_WIDGET (g_object_new (TYPE_LOG,
                                           /* GtkWindow */
                                           "type", GTK_WINDOW_TOPLEVEL,
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

    log_message (LOG (log_dialog), contents, "old");
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

/*!
 *  \brief Close the Log window
 *  \par Function Description
 *  If the log window exists, destroy it.
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

/*!
 *  \brief Add a message to the Log window
 *  \par Function Description
 *  Add a message to the Log window.
 *  Calls log_message() to do the actual logging.
 *  \param [in] log_domain
 *  \param [in] log_level The severity of the message
 *  \param [in] message   The message to be displayed
 */
void x_log_message (const gchar *log_domain, GLogLevelFlags log_level,
                    const gchar *message)
{
  gchar *style;
  g_return_if_fail (log_dialog != NULL);

  if (log_level & (G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_ERROR)) {
    style = "critical";
  } else if (log_level & G_LOG_LEVEL_WARNING) {
    style = "warning";
  } else {
    style = "message";
  }

  log_message (LOG(log_dialog), message, style);
}

/*!
 *  \brief Log window callback function
 *
 *  \par Function Description
 *  Callback function for the Log window. Only used to close the window.
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

/*!
 *  \brief Add a message to the log window
 *
 *  \par Function Description
 *  \param [in] log The log instance
 *  \param [in] message The message to be logged
 *  \param [in] style   The style to use in the text rendering
 */
static void log_message (Log *log, const gchar *message, 
                         const gchar *style)
{
  GtkTextBuffer *buffer;
  GtkTextIter iter;
  GtkTextMark *mark;
  
  g_return_if_fail (IS_LOG (log));

  buffer = gtk_text_view_get_buffer (log->textview);
  gtk_text_buffer_get_end_iter (buffer, &iter);
  /* Apply the "plain" tag before the level-specific tag in order to
   * reset the formatting */

  if (g_utf8_validate (message, -1, NULL)) {
    gtk_text_buffer_insert_with_tags_by_name (buffer, &iter, message, -1,
                                              "plain", style, NULL);
  } else {
    /* If UTF-8 wasn't valid (due to a system locale encoded filename or
     * other string being included by mistake), log a warning, and print
     * the original message to stderr, where it may be partly intelligible */
    gtk_text_buffer_insert_with_tags_by_name (buffer, &iter,
      _("** Invalid UTF-8 in log message. See stderr or gschem.log.\n"),
                                              -1, "plain", style, NULL);
    fprintf (stderr, "%s", message);
  }

  mark = gtk_text_buffer_create_mark(buffer, NULL, &iter, FALSE);
  gtk_text_view_scroll_to_mark (log->textview, mark, 0, TRUE, 0, 1);
  gtk_text_buffer_delete_mark (buffer, mark);
}

/*!
 *  \brief Get the Log class type
 *
 *  \par Function Description
 *
 * On first call, registers the Log class with the GType dynamic type system.
 * On subsequent calls, returns the saved value from first execution.
 * \returns the type identifier for the Log class
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

/*!
 *  \brief Log class initialization function
 *
 *  \par Function Description
 *
 * Class initialization function for the Log class. Currently
 * does nothing.
 */
static void log_class_init (LogClass *klass)
{
/*   GObjectClass *gobject_class = G_OBJECT_CLASS (klass); */
	
}

/*!
 *  \brief Log class instance initialization function
 *
 *  \par Function Description
 *
 * Instance initialization for the Log class. Sets up the log parameters,
 * creates a scrollable text view with highlighting and a Close button.
 *
 * \param log the instance of the class to initialize
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

  /* Add some tags for highlighting log messages to the buffer */
  gtk_text_buffer_create_tag (text_buffer, "plain",
                              "foreground", "black",
                              "foreground-set", TRUE,
                              "weight", PANGO_WEIGHT_NORMAL,
                              "weight-set", TRUE,
                              NULL);
  /* The default "message" style is plain */
  gtk_text_buffer_create_tag (text_buffer, "message", NULL);
  /* "old" messages are in dark grey */
  gtk_text_buffer_create_tag (text_buffer, "old",
                              "foreground", "#404040",
                              "foreground-set", TRUE,
			      NULL);
  /* "warning" messages are printed in red */
  gtk_text_buffer_create_tag (text_buffer, "warning",
                              "foreground", "red",
                              "foreground-set", TRUE,
                              NULL);
  /* "critical" messages are bold red */
  gtk_text_buffer_create_tag (text_buffer, "critical",
                              "foreground", "red",
                              "foreground-set", TRUE,
                              "weight", PANGO_WEIGHT_BOLD,
                              "weight-set", TRUE,
                              NULL);

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
