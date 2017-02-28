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
/*!
 * \file gschem_log_widget.c
 *
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


static void
changed_cb (GtkTextBuffer *buffer, GschemLogWidget *widget);

static void
class_init (GschemLogWidgetClass *klass);

static GtkTextBuffer*
create_text_buffer();

static void
instance_init (GschemLogWidget *log);

static void
log_message (GschemLogWidgetClass *klass, const gchar *message, const gchar *style);


/*!
 *  \brief Get the Log class type
 *
 *  \par Function Description
 *
 * On first call, registers the Log class with the GType dynamic type system.
 * On subsequent calls, returns the saved value from first execution.
 * \returns the type identifier for the Log class
 */
GType
gschem_log_widget_get_type ()
{
  static GType type = 0;

  if (type == 0) {
    static const GTypeInfo info = {
      sizeof(GschemLogWidgetClass),
      NULL,                                 /* base_init */
      NULL,                                 /* base_finalize */
      (GClassInitFunc) class_init,
      NULL,                                 /* class_finalize */
      NULL,                                 /* class_data */
      sizeof(GschemLogWidget),
      0,                                    /* n_preallocs */
      (GInstanceInitFunc) instance_init,
    };

    type = g_type_register_static (GSCHEM_TYPE_BIN,
                                   "GschemLogWidget",
                                   &info,
                                   (GTypeFlags) 0);
  }

  return type;
}


/*! \brief create a new status log widget
 *
 *  \return a new status log widget
 */
GschemLogWidget*
gschem_log_widget_new ()
{
  return GSCHEM_LOG_WIDGET (g_object_new (GSCHEM_TYPE_LOG_WIDGET, NULL));
}


/*! \brief Add a message to the status log
 *
 *  \param [in] log_domain
 *  \param [in] log_level The severity of the message
 *  \param [in] message   The message to be displayed
 */
void
x_log_message (const gchar *log_domain, GLogLevelFlags log_level, const gchar *message)
{
  GschemLogWidgetClass *klass = GSCHEM_LOG_WIDGET_CLASS (g_type_class_peek_static (GSCHEM_TYPE_LOG_WIDGET));
  gchar *style;

  if (log_level & (G_LOG_LEVEL_CRITICAL | G_LOG_LEVEL_ERROR)) {
    style = "critical";
  } else if (log_level & G_LOG_LEVEL_WARNING) {
    style = "warning";
  } else {
    style = "message";
  }

  log_message (klass, message, style);
}


/*!
 *  \brief Open the Log window
 *
 *  Selects the notebook tab that contains the status log widget
 */
void
x_log_open (GschemToplevel *w_current)
{
  int page;

  g_return_if_fail (w_current != NULL);
  g_return_if_fail (w_current->bottom_notebook != NULL);
  g_return_if_fail (w_current->log_widget != NULL);

  page = gtk_notebook_page_num (GTK_NOTEBOOK (w_current->bottom_notebook),
                                GTK_WIDGET (w_current->log_widget));

  if (page >= 0) {
    int current = gtk_notebook_get_current_page (GTK_NOTEBOOK (w_current->bottom_notebook));

    if (page != current) {
      gtk_notebook_set_current_page (GTK_NOTEBOOK (w_current->bottom_notebook), page);
    }
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
static void
log_message (GschemLogWidgetClass *klass, const gchar *message, const gchar *style)
{
  GtkTextIter iter;

  g_return_if_fail (klass != NULL);
  g_return_if_fail (klass->buffer != NULL);

  gtk_text_buffer_get_end_iter (klass->buffer, &iter);
  /* Apply the "plain" tag before the level-specific tag in order to
   * reset the formatting */

  if (g_utf8_validate (message, -1, NULL)) {
    gtk_text_buffer_insert_with_tags_by_name (klass->buffer, &iter, message, -1,
                                              "plain", style, NULL);
    gtk_text_buffer_insert (klass->buffer, &iter, "\n", -1);
  } else {
    /* If UTF-8 wasn't valid (due to a system locale encoded filename or
     * other string being included by mistake), log a warning, and print
     * the original message to stderr, where it may be partly intelligible */
    gtk_text_buffer_insert_with_tags_by_name (klass->buffer, &iter,
      _("** Invalid UTF-8 in log message. See stderr or gschem.log.\n"),
                                              -1, "plain", style, NULL);
    fprintf (stderr, "%s", message);
  }
}


/*! \brief callback for changes in the text buffer
 *
 *  Changes in the contents of the buffer cause all text view widgets to scroll
 *  to the bottom.
 *
 *  \param [in] buffer the text buffer triggering the event
 *  \param [in] widget the widget to scroll to the bottom
 */
static void
changed_cb (GtkTextBuffer *buffer, GschemLogWidget *widget)
{
  GtkTextIter iter;

  g_return_if_fail (buffer != NULL);
  g_return_if_fail (widget != NULL);
  g_return_if_fail (widget->viewer != NULL);

  gtk_text_buffer_get_end_iter (buffer, &iter);
  gtk_text_view_scroll_to_iter (widget->viewer, &iter, 0.0, TRUE, 0.0, 1.0);
}


/*! \brief initialize class
 */
static void
class_init (GschemLogWidgetClass *klass)
{
  gchar *contents;
/*   GObjectClass *gobject_class = G_OBJECT_CLASS (klass); */

  klass->buffer = create_text_buffer ();

  /* make it read the content of the current log file */
  /* and add its contents to the dialog */
  contents = s_log_read ();

  /* s_log_read can return NULL if the log file cannot be written to */
  if (contents != NULL) {
    log_message (klass, contents, "old");
    g_free (contents);

    x_log_update_func = x_log_message;
  }
}


/*! \brief create the text buffer for storing the status log contents
 *
 *  \return a GtkTextBuffer for storing the status log
 */
static GtkTextBuffer*
create_text_buffer()
{
  GtkTextBuffer *buffer = gtk_text_buffer_new (NULL);

  /* Add some tags for highlighting log messages to the buffer */
  gtk_text_buffer_create_tag (buffer,
                              "plain",
                              "foreground", "black",
                              "foreground-set", TRUE,
                              "weight", PANGO_WEIGHT_NORMAL,
                              "weight-set", TRUE,
                              NULL);

  /* The default "message" style is plain */
  gtk_text_buffer_create_tag (buffer, "message", NULL);

  /* "old" messages are in dark grey */
  gtk_text_buffer_create_tag (buffer,
                              "old",
                              "foreground", "#404040",
                              "foreground-set", TRUE,
                              NULL);

  /* "warning" messages are printed in red */
  gtk_text_buffer_create_tag (buffer,
                              "warning",
                              "foreground", "red",
                              "foreground-set", TRUE,
                              NULL);

  /* "critical" messages are bold red */
  gtk_text_buffer_create_tag (buffer,
                              "critical",
                              "foreground", "red",
                              "foreground-set", TRUE,
                              "weight", PANGO_WEIGHT_BOLD,
                              "weight-set", TRUE,
                              NULL);

  return buffer;
}


/*! \brief initialize instance
 *
 *  \param [in] widget an instance of the widget
 */
static void
instance_init (GschemLogWidget *widget)
{
  GtkTextIter iter;
  GschemLogWidgetClass *klass = GSCHEM_LOG_WIDGET_GET_CLASS (widget);
  GtkWidget *scrolled;

  g_return_if_fail (klass != NULL);
  g_return_if_fail (klass->buffer != NULL);
  g_return_if_fail (widget != NULL);

  scrolled = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_add (GTK_CONTAINER (widget), scrolled);

  widget->viewer = GTK_TEXT_VIEW (g_object_new (GTK_TYPE_TEXT_VIEW,
                                                /* GtkTextView */
                                                "buffer",   klass->buffer,
                                                "editable", FALSE,
                                                NULL));

  gtk_container_add (GTK_CONTAINER (scrolled), GTK_WIDGET (widget->viewer));

  g_signal_connect (klass->buffer,
                    "changed",
                    G_CALLBACK (&changed_cb),
                    widget);

  gtk_text_buffer_get_end_iter (klass->buffer, &iter);
  gtk_text_view_scroll_to_iter (widget->viewer, &iter, 0.0, TRUE, 0.0, 1.0);
}
