/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's Library
 * Copyright (C) 2012 Peter TB Brett <peter@peter-b.co.uk>
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

#ifndef __EDA_OBJECTSET_H__
#define __EDA_OBJECTSET_H__

G_BEGIN_DECLS

/* ---------------------------------------------------------------- */

/*! \class EdaObjectset edaobjectset.h "libgeda/edaobjectset.h"
 * \brief Ordered set of schematic objects.
 *
 * The #EdaObjectset represents an ordered set of schematic objects.
 */

#define EDA_TYPE_OBJECTSET (eda_object_set_get_type ())
#define EDA_OBJECTSET(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), EDA_TYPE_OBJECTSET, EdaObjectset))
#define EDA_OBJECTSET_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), EDA_TYPE_OBJECTSET, EdaObjectsetClass))
#define EDA_IS_OBJECTSET(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EDA_TYPE_OBJECTSET))
#define EDA_IS_OBJECTSET_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((obj), EDA_TYPE_OBJECTSET))
#define EDA_OBJECTSET_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), EDA_TYPE_OBJECTSET, EdaObjectsetClass))

#define EDA_TYPE_OBJECT_ITER (eda_object_iter_get_type ())

typedef struct _EdaObjectsetClass EdaObjectsetClass;
typedef struct _EdaObjectset EdaObjectset;
typedef struct _EdaObjectsetPrivate EdaObjectsetPrivate;
typedef struct _EdaObjectIter EdaObjectIter;

struct _EdaObjectsetClass
{
  GObjectClass parent_class;

  /* virtual public methods */
  gboolean (*get_iter_nth)(EdaObjectset *set, EdaObjectIter *iter, gint index);
  gboolean (*iter_next)(EdaObjectset *set, EdaObjectIter *iter);
  gboolean (*find)(EdaObjectset *set, EdaObjectIter *iter);
  OBJECT *(*get)(EdaObjectset *set, EdaObjectIter *iter);
  void (*append)(EdaObjectset *set, OBJECT *object);
  void (*insert)(EdaObjectset *set, EdaObjectIter *iter, OBJECT *object);
  void (*remove)(EdaObjectset *set, EdaObjectIter *iter);

  /* signals */
  GCallback object_inserted;
  GCallback object_removed;
};

struct _EdaObjectset
{
  GObject parent_instance;

  /* Private members */
  EdaObjectsetPrivate *priv;
};

GType eda_objectset_get_type (void) G_GNUC_CONST;

/* Convenience functions */
EdaObjectset *eda_objectset_new (void) G_GNUC_WARN_UNUSED_RESULT;
void eda_objectset_foreach (EdaObjectset *set, GCallback func, gpointer user_data);

/* Getting iterators */
gboolean eda_objectset_get_iter_nth (EdaObjectset *set, EdaObjectIter *iter, gint index);
gboolean eda_objectset_iter_next (EdaObjectset *set, EdaObjectIter *iter);
gboolean eda_objectset_find (EdaObjectset *set, EdaObjectIter *iter, OBJECT *object);

/* Obtaining values */
OBJECT *eda_object_set_get (EdaObjectset *set, EdaObjectIter *iter);

/* Modifying the set */
void eda_objectset_append (EdaObjectset *set, OBJECT *object);
void eda_objectset_insert (EdaObjectset *set, EdaObjectIter *iter, OBJECT *object);
void eda_objectset_remove (EdaObjectset *set, EdaObjectIter *iter);

/* Iterator basics */
struct _EdaObjectIter
{
  gint stamp;
  gpointer user_data;
  gpointer user_data2;
  gpointer user_data3;
}

GType eda_object_iter_get_type (void) G_GNUC_CONST;

EdaObjectIter *eda_object_iter_copy (EdaObjectIter *iter);
void eda_object_iter_free (EdaObjectIter *iter);

G_END_DECLS

#endif /* !__EDA_OBJECTSET_H__ */
