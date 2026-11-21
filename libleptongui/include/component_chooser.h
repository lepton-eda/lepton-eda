/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2026 Lepton EDA Contributors
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

/*!
 * \file component_chooser.h
 *
 * \brief The component selection dialog
 */

#ifndef __COMPONENT_CHOOSER_H__
#define __COMPONENT_CHOOSER_H__


/*
 * CompselectBehavior
 */

typedef enum {
  COMPSELECT_BEHAVIOR_REFERENCE,
  COMPSELECT_BEHAVIOR_EMBED,
  COMPSELECT_BEHAVIOR_INCLUDE
} CompselectBehavior;


GType compselect_behavior_get_type (void);
#define COMPSELECT_TYPE_BEHAVIOR  (compselect_behavior_get_type ())


/*
 * Compselect
 */

#define TYPE_COMPSELECT         (compselect_get_type())
#define COMPSELECT(obj)         (G_TYPE_CHECK_INSTANCE_CAST ((obj), TYPE_COMPSELECT, Compselect))
#define COMPSELECT_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), TYPE_COMPSELECT, CompselectClass))
#define IS_COMPSELECT(obj)      (G_TYPE_CHECK_INSTANCE_TYPE ((obj), TYPE_COMPSELECT))
#define COMPSELECT_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), TYPE_COMPSELECT, CompselectClass))

typedef struct _CompselectClass CompselectClass;
typedef struct _Compselect      Compselect;


struct _CompselectClass {
  SchematicDialogClass parent_class;

  guint behavior_changed_signal_id;

};

struct _Compselect {
  SchematicDialog parent_instance;

  GtkWidget   *hpaned, *vpaned, *attrframe;
  GtkTreeView *libtreeview, *inusetreeview, *attrtreeview;
  GtkNotebook *viewtabs;
  SchematicPreview *preview;
  GtkEntry    *entry_filter;
  GtkButton   *button_clear;
  guint        filter_timeout;
  GtkComboBox *combobox_behaviors;
};


GType compselect_get_type (void);

/* Response IDs for special dialog buttons */
typedef enum {
  COMPSELECT_RESPONSE_PLACE = 1,
  COMPSELECT_RESPONSE_HIDE = 2,
  COMPSELECT_RESPONSE_REFRESH = 3
} CompselectResponseType;


G_BEGIN_DECLS

void
x_compselect_open (GtkWidget *cswindow);

void
x_compselect_deselect (SchematicWindow *w_current);

void
x_compselect_callback_response (GtkDialog *dialog,
                                gint arg1,
                                gpointer user_data);
GtkWidget*
schematic_compselect_new (SchematicWindow *w_current);

GtkWidget*
schematic_component_chooser_get_attrframe (Compselect *chooser);

void
schematic_component_chooser_set_attrframe (Compselect *chooser,
                                           GtkWidget *widget);
GtkComboBox*
schematic_component_chooser_get_combobox_behaviors (Compselect *chooser);

void
schematic_component_chooser_set_combobox_behaviors (Compselect *chooser,
                                                    GtkComboBox *combobox);
GtkWidget*
schematic_component_chooser_get_hpaned (Compselect *chooser);

void
schematic_component_chooser_set_hpaned (Compselect *chooser,
                                        GtkWidget *hpaned);
GtkNotebook*
schematic_component_chooser_get_notebook (Compselect *chooser);

void
schematic_component_chooser_set_notebook (Compselect *chooser,
                                          GtkNotebook *notebook);
GtkWidget*
schematic_component_chooser_get_preview (GtkWidget *chooser);

void
schematic_component_chooser_set_preview (GtkWidget *chooser,
                                         GtkWidget *preview);
GtkWidget*
schematic_component_chooser_get_vpaned (Compselect *chooser);

void
schematic_component_chooser_set_vpaned (Compselect *chooser,
                                        GtkWidget *vpaned);
void
schematic_component_chooser_callback_behavior_changed (GtkComboBox *widget,
                                                       gpointer user_data);
GtkWidget*
schematic_component_chooser_create_attributes_treeview (Compselect *compselect);

GtkWidget*
schematic_component_chooser_create_behaviors_combo_box (void);

GtkWidget*
schematic_component_chooser_create_inuse_treeview (Compselect *compselect);

GtkWidget *
schematic_component_chooser_create_lib_treeview (Compselect *compselect);

void
schematic_component_chooser_setup (GtkWidget *chooser,
                                   GtkWidget *hpaned,
                                   GtkWidget *vpaned);
G_END_DECLS

#endif /* __COMPONENT_CHOOSER_H__ */
