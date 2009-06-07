/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2009 gEDA Contributors (see ChangeLog for details)
 *
 * Code based on GTK 2.14.5 gtk/gtkaccellabel.h (LGPL)
 *
 * GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * GschemAccelLabel: GtkLabel with accelerator monitoring facilities.
 * Copyright (C) 1998 Tim Janik
 *
 * Modified by the GTK+ Team and others 1997-2001.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/.
 *
 *  Adapted for gEDA by Peter Clifton <pcjc2@cam.ac.uk>
 *
 *  THIS FILE IS LGPL LICENSED, gEDA AS A WHOLE IS GPL LICENSED
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef __GSCHEM_ACCEL_LABEL_H__
#define __GSCHEM_ACCEL_LABEL_H__

G_BEGIN_DECLS

#define GSCHEM_TYPE_ACCEL_LABEL            (gschem_accel_label_get_type ())
#define GSCHEM_ACCEL_LABEL(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_ACCEL_LABEL, GschemAccelLabel))
#define GSCHEM_ACCEL_LABEL_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GSCHEM_TYPE_ACCEL_LABEL, GschemAccelLabelClass))
#define GSCHEM_IS_ACCEL_LABEL(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_ACCEL_LABEL))
#define GSCHEM_IS_ACCEL_LABEL_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GSCHEM_TYPE_ACCEL_LABEL))
#define GSCHEM_ACCEL_LABEL_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GSCHEM_TYPE_ACCEL_LABEL, GschemAccelLabelClass))


typedef struct _GschemAccelLabel            GschemAccelLabel;
typedef struct _GschemAccelLabelClass  GschemAccelLabelClass;

struct _GschemAccelLabel
{
  GtkAccelLabel label;

  guint          accel_padding;
  gchar         *accel_string;
  guint16        accel_string_width;
};

struct _GschemAccelLabelClass
{
  GtkAccelLabelClass  parent_class;
};


GType      gschem_accel_label_get_type          (void) G_GNUC_CONST;
GtkWidget* gschem_accel_label_new               (const gchar      *string);
guint      gschem_accel_label_get_accel_width   (GschemAccelLabel *accel_label);
void       gschem_accel_label_set_accel_string  (GschemAccelLabel *accel_label,
                                                 const gchar      *accel_string);
gboolean   gschem_accel_label_refetch           (GschemAccelLabel *accel_label);

/* private */
gchar *    _gschem_accel_label_class_get_accelerator_label (GschemAccelLabelClass *klass,
                                                            guint                  accelerator_key,
                                                            GdkModifierType        accelerator_mods);

G_END_DECLS

#endif /* __GSCHEM_ACCEL_LABEL_H__ */
