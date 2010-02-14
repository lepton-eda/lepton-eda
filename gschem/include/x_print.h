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


#ifndef __X_PRINT_H__
#define __X_PRINT_H__

/*
 * PrintDialog class
 */

#define TYPE_PRINT_DIALOG         (print_dialog_get_type())
#define PRINT_DIALOG(obj)         (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_PRINT_DIALOG, PrintDialog))
#define PRINT_DIALOG_CLASS(class) (G_TYPE_CHECK_CLASS_CAST ((class), TYPE_PRINT_DIALOG))
#define IS_PRINT_DIALOG(obj)      (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_PRINT_DIALOG))

typedef struct _PrintDialogClass PrintDialogClass;
typedef struct _PrintDialog PrintDialog;

struct _PrintDialogClass
{
  GschemDialogClass parent_class;
};

struct _PrintDialog
{
  GschemDialog parent_instance;

  GtkEntry *fnfield, *cmdfield;
  GtkRadioButton *fileradio, *cmdradio;
  GtkButton *saveasbutton;
  GtkComboBox *orientcbox, *typecbox, *papercbox;
};

GType print_dialog_get_type ();

#endif /* !__X_PRINT_H__ */
