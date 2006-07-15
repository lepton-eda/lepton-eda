/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2004 Ales V. Hvezda
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */



typedef enum {
  LOG_RESPONSE_CLOSE  = 1
} LogResponseType;


#define TYPE_LOG         (log_get_type())
#define LOG(obj)         (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_LOG, Log))
#define LOG_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), TYPE_LOG, LogClass))
#define IS_LOG(obj)      (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_LOG))


typedef struct _LogClass LogClass;
typedef struct _Log      Log;


struct _LogClass {
  GtkDialogClass parent_class;
};

struct _Log {
  GtkDialog parent_instance;

  GtkTextView *textview;

  guint handler;
  
};


GType log_get_type (void);

void log_message (Log *log, const gchar *message);
