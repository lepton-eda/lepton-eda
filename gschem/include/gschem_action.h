/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2009 Ales V. Hvezda
 * Copyright (C) 1998-2009 gEDA Contributors (see ChangeLog for details)
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


#ifndef __GSCHEM_ACTION_H__
#define __GSCHEM_ACTION_H__


#define GSCHEM_TYPE_ACTION           (gschem_action_get_type())
#define GSCHEM_ACTION(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_ACTION, GschemAction))
#define GSCHEM_ACTION_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_ACTION, GschemActionClass))
#define GSCHEM_IS_ACTION(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_ACTION))
#define GSCHEM_ACTION_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj),  GSCHEM_TYPE_ACTION, GschemActionClass))

typedef struct _GschemActionClass GschemActionClass;
typedef struct _GschemAction      GschemAction;


struct _GschemActionClass {
  GtkActionClass parent_class;

};

struct _GschemAction {
  GtkAction parent_instance;

  gchar *multikey_accel;
};


GType gschem_action_get_type (void);

GschemAction *gschem_action_new           (const gchar *name,
                                           const gchar *label,
                                           const gchar *tooltip,
                                           const gchar *stock_id,
                                           const gchar *multikey_accel);

#endif /* __GSCHEM_ACTION_H__ */
