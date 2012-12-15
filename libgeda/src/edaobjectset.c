/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's Library
 * Copyright (C) 2012 gEDA Contributors (see ChangeLog for details)
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

#include <config.h>

#include <libgeda_priv.h>

/*! \private \memberof EdaObjectset
 * Private data for object set. */
struct _EdaObjectsetPrivate
{
  gint stamp;
  GList *lst;
};

static void     eda_objectset_dispose (GObject *object);
static void     eda_objectset_finalize (GObject *object);

static gboolean eda_objectset_default_get_iter_nth  (EdaObjectSet *set,
                                                     EdaObjectIter *iter,
                                                     gint index);
static gboolean eda_objectset_default_iter_next     (EdaObjectSet *set,
                                                     EdaObjectIter *iter);
static gboolean eda_objectset_default_find          (EdaObjectSet *set,
                                                     EdaObjectIter *iter,
                                                     OBJECT *object);
static OBJECT * eda_objectset_default_get           (EdaObjectSet *set,
                                                     EdaObjectIter *iter);
static void     eda_objectset_default_append        (EdaObjectSet *set,
                                                     OBJECT *object);
static void     eda_objectset_default_insert        (EdaObjectSet *set,
                                                     EdaObjectIter *iter,
                                                     OBJECT *object);
static void     eda_objectset_default_remove        (EdaObjectSet *set,
                                                     EdaObjectIter *iter);

static void cclosure_marshal_VOID__BOXED_POINTER (GClosure *closure,
                                                  GValue *return_value,
                                                  guint n_param_values,
                                                  const GValue *param_values,
                                                  gpointer invocation_hint,
                                                  gpointer marshal_data);

/* ---------------------------------------------------------------- */

/*! Magic helpful GObject macro */
G_DEFINE_BOXED_TYPE (EdaObjectIter, eda_object_iter,
                     eda_object_iter_copy, eda_object_iter_free);

/*! \brief Copy an object iterator.
 * \par Function Description
 * Copy an #EdaObjectIter.  You will not normally need to do this;
 * just copy the structs by value.
 *
 * \code
 * EdaObjectIter new_iter = iter;
 * \endcode
 *
 * You must free the result with eda_object_iter_free().
 */
EdaObjectIter *
eda_object_iter_copy (EdaObjectIter *iter)
{
  return g_memdup (iter, sizeof(EdaObjectIter));
}

/*! \brief Free an object iterator.
 * \par Function Description
 * Free an #EdaObjectIter allocated with eda_object_iter_copy().
 */
void
eda_object_iter_free (EdaObjectIter *iter)
{
  g_free (iter);
}

/* ---------------------------------------------------------------- */

/*! Magic helpful GObject macro */
G_DEFINE_TYPE (EdaObjectset, eda_objectset, G_TYPE_OBJECT);

/*! Initialise EdaObjectset class. */
static void
eda_objectset_class_init (EdaObjectsetClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

  g_type_class_add_private (gobject_class, sizeof (EdaObjectsetPrivate));

  /* Register functions with base class */
  gobject_class->dispose  = eda_objectset_dispose;
  gobject_class->finalize = eda_objectset_finalize;

  /* Default signal handlers */
  klass->object_inserted = NULL;
  klass->object_removed  = NULL;

  /* Default implementation functions */
  klass->get_iter_nth  = eda_objectset_default_get_iter_nth;
  klass->iter_next     = eda_objectset_default_iter_next;
  klass->find          = eda_objectset_default_find;
  klass->get           = eda_objectset_default_get;
  klass->append        = eda_objectset_default_append;
  klass->insert        = eda_objectset_default_insert;
  klass->remove        = eda_objectset_default_remove;

  /* Create signals */
  g_signal_new ("object-inserted", /* signal name */
                G_TYPE_FROM_CLASS (gobject_class), /* type */
                G_SIGNAL_RUN_FIRST, /* flags */
                G_STRUCT_OFFSET (EdaObjectsetClass, object_inserted), /* class offset */
                NULL, /* accumulator */
                NULL, /* accumulator data */
                cclosure_marshal_VOID__BOXED_POINTER, /* c_marshaller */
                G_TYPE_NONE, /* return type */
                2, /* no. of params */
                EDA_TYPE_OBJECT_ITER, G_TYPE_POINTER);
  g_signal_new ("object-removed", /* signal name */
                G_TYPE_FROM_CLASS (gobject_class), /* type */
                G_SIGNAL_RUN_FIRST, /* flags */
                G_STRUCT_OFFSET (EdaObjectsetClass, object_removed), /* class offset */
                NULL, /* accumulator */
                NULL, /* accumulator data */
                cclosure_marshal_VOID__BOXED_POINTER, /* c_marshaller */
                G_TYPE_NONE, /* return type */
                2, /* no. of params */
                G_TYPE_POINTER);
}

/*! Initialise EdaObjectset instance. */
static void
eda_objectset_init (EdaObjectset *set)
{
  set->priv = G_TYPE_INSTANCE_GET_PRIVATE (set,
                                           EDA_TYPE_OBJECTSET,
                                           EdaObjectsetPrivate);
  set->priv->lst = NULL;
}

/*! Dispose of an EdaObjectset instance. Drop all references to other
 * GObjects, but keep the instance otherwise intact. May be run
 * multiple times (due to reference loops).
 */
static void
eda_objectset_dispose (GObject *object)
{
  /* Chain up to the parent class */
  G_OBJECT_CLASS (eda_objectset_parent_class)->dispose (object);
}

/*! Finalize an EdaConfig instance. Free all resources held by the
 * instance. */
static void
eda_objectset_finalize (GObject *object)
{
  EdaObjectset *set = EDA_OBJECTSET (object);
  g_list_free (set->priv->lst);
  set->priv->lst = NULL;
}

/* ---------------------------------------------------------------- */

#define ITER_INVALIDATE(x) ((x)->stamp = -1)
#define ITER_IS_VALID(s,x) ((s)->priv->stamp == (x)->stamp)
#define ITER_LST_PTR(x) ((x)->user_data)

gboolean
eda_objectset_get_iter (EdaObjectset *set, EdaObjectIter *iter, gint index)
{
  g_return_val_if_fail (EDA_IS_OBJECTSET (set), FALSE);
  return EDA_OBJECTSET_GET_CLASS (set)->get_iter (set, iter, index);
}

static gboolean
eda_objectset_default_get_iter_nth (EdaObjectset *set, EdaObjectIter *iter, gint index)
{
  GList *lst_iter;
  gint i = 0;
  g_return_val_if_fail (iter != NULL, FALSE);

  ITER_INVALIDATE (iter);

  ITER_LST_PTR (iter) = set->priv->lst;
  for (lst_iter = set->priv->lst, i = 0;
       lst_iter != NULL && i < index;
       lst_iter = g_list_next (lst_iter), ++i)
    ITER_LST_PTR (iter) = lst_iter;

  if (i == index) {
    iter->stamp = set->priv->stamp;
    return TRUE;
  }
  return FALSE;
}

gboolean
eda_objectset_iter_next (EdaObjectset *set, EdaObjectIter *iter)
{
  g_return_val_if_fail (EDA_IS_OBJECTSET (set), FALSE);
  return EDA_OBJECTSET_GET_CLASS (set)->iter_next (set, iter);
}

static gboolean
eda_objectset_default_iter_next (EdaObjectset *set, EdaObjectIter *iter)
{
  g_return_val_if_fail (iter != NULL, FALSE);
  g_return_val_if_fail (ITER_IS_VALID (iter), FALSE);

  ITER_LST_PTR (iter) = g_list_next (ITER_LST_PTR (iter));
  if (ITER_LST_PTR (iter) != NULL) return TRUE;

  ITER_INVALIDATE (iter);
  return FALSE;

}

