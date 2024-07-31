/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2011 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
 *
 * Code based on GTK 2.14.5 gtk/gtkaccellabel.h (LGPL)
 *
 * GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * SchematicAccelLabel: GtkLabel with accelerator monitoring facilities.
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
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA
 */

#ifndef __GSCHEM_ACCEL_LABEL_H__
#define __GSCHEM_ACCEL_LABEL_H__

G_BEGIN_DECLS

#ifdef ENABLE_GTK3

#define SCHEMATIC_TYPE_ACCEL_LABEL            (schematic_accel_label_get_type ())
#define SCHEMATIC_ACCEL_LABEL(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), SCHEMATIC_TYPE_ACCEL_LABEL, SchematicAccelLabel))
#define SCHEMATIC_ACCEL_LABEL_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), SCHEMATIC_TYPE_ACCEL_LABEL, SchematicAccelLabelClass))
#define SCHEMATIC_IS_ACCEL_LABEL(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), SCHEMATIC_TYPE_ACCEL_LABEL))
#define SCHEMATIC_IS_ACCEL_LABEL_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), SCHEMATIC_TYPE_ACCEL_LABEL))
#define SCHEMATIC_ACCEL_LABEL_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), SCHEMATIC_TYPE_ACCEL_LABEL, SchematicAccelLabelClass))


typedef struct _SchematicAccelLabel SchematicAccelLabel;
typedef struct _SchematicAccelLabelClass SchematicAccelLabelClass;
typedef struct _SchematicAccelLabelPrivate SchematicAccelLabelPrivate;

struct _SchematicAccelLabel
{
  GtkAccelLabel label;
  SchematicAccelLabelPrivate *priv;
};

struct _SchematicAccelLabelClass
{
  GtkLabelClass  parent_class;
};


GType      schematic_accel_label_get_type       (void) G_GNUC_CONST;
GtkWidget* schematic_accel_label_new            (const gchar      *string);
guint      schematic_accel_label_get_accel_width   (SchematicAccelLabel *accel_label);
void       schematic_accel_label_set_accel_string  (SchematicAccelLabel *accel_label,
                                                    const gchar *accel_string);
gboolean   schematic_accel_label_refetch           (SchematicAccelLabel *accel_label);


#else /* GTK2 */

#define SCHEMATIC_TYPE_ACCEL_LABEL            (schematic_accel_label_get_type ())
#define SCHEMATIC_ACCEL_LABEL(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), SCHEMATIC_TYPE_ACCEL_LABEL, SchematicAccelLabel))
#define SCHEMATIC_ACCEL_LABEL_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), SCHEMATIC_TYPE_ACCEL_LABEL, SchematicAccelLabelClass))
#define SCHEMATIC_IS_ACCEL_LABEL(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), SCHEMATIC_TYPE_ACCEL_LABEL))
#define SCHEMATIC_IS_ACCEL_LABEL_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), SCHEMATIC_TYPE_ACCEL_LABEL))
#define SCHEMATIC_ACCEL_LABEL_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), SCHEMATIC_TYPE_ACCEL_LABEL, SchematicAccelLabelClass))

typedef struct _SchematicAccelLabel            SchematicAccelLabel;
typedef struct _SchematicAccelLabelClass  SchematicAccelLabelClass;

struct _SchematicAccelLabel
{
  GtkAccelLabel label;
  guint          accel_padding;
  gchar         *accel_string;
  guint16        accel_string_width;
};

struct _SchematicAccelLabelClass
{
  GtkAccelLabelClass  parent_class;
};


GType      schematic_accel_label_get_type       (void) G_GNUC_CONST;
GtkWidget* schematic_accel_label_new            (const gchar      *string);
guint      schematic_accel_label_get_accel_width   (SchematicAccelLabel *accel_label);
void       schematic_accel_label_set_accel_string  (SchematicAccelLabel *accel_label,
                                                    const gchar *accel_string);
gboolean   schematic_accel_label_refetch           (SchematicAccelLabel *accel_label);

/* private */
gchar *    _gschem_accel_label_class_get_accelerator_label (SchematicAccelLabelClass *klass,
                                                            guint                  accelerator_key,
                                                            GdkModifierType        accelerator_mods);
#endif /* ENABLE_GTK3 */

G_END_DECLS

#endif /* __GSCHEM_ACCEL_LABEL_H__ */
