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


#ifndef __X_PREVIEW_H__
#define __X_PREVIEW_H__


/*
 * Preview
 */

#define TYPE_PREVIEW         (preview_get_type())
#define PREVIEW(obj)         (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_PREVIEW, Preview))
#define PREVIEW_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), TYPE_PREVIEW, PreviewClass))
#define IS_PREVIEW(obj)      (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_PREVIEW))


typedef struct _PreviewClass PreviewClass;
typedef struct _Preview      Preview;


struct _PreviewClass {
  GtkDrawingAreaClass parent_class;
  
};

struct _Preview {
  GtkDrawingArea parent_instance;

  GSCHEM_TOPLEVEL *preview_w_current;

  gchar *filename;
  gchar *buffer;

  gboolean active;
  
};


GType preview_get_type (void);


#endif /* __X_PREVIEW_H__ */
