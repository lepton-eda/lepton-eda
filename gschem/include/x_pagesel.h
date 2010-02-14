/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
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
  PAGESEL_RESPONSE_CLOSE  = 1,
  PAGESEL_RESPONSE_UPDATE = 2
} PageselResponseType;


#define TYPE_PAGESEL         (pagesel_get_type())
#define PAGESEL(obj)         (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_PAGESEL, Pagesel))
#define PAGESEL_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), TYPE_PAGESEL, PageselClass))
#define IS_PAGESEL(obj)      (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_PAGESEL))


typedef struct _PageselClass PageselClass;
typedef struct _Pagesel      Pagesel;


struct _PageselClass {
  GschemDialogClass parent_class;
};

struct _Pagesel {
  GschemDialog parent_instance;

  GtkTreeView *treeview;
};


GType pagesel_get_type (void);

void pagesel_update (Pagesel *pagesel);
