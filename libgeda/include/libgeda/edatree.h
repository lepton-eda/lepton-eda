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

#ifndef __EDA_RTREE_H__
#define __EDA_RTREE_H__

G_BEGIN_DECLS

#define EDA_TYPE_RTREE (eda_rtree_get_type ())
#define EDA_RTREE(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), EDA_TYPE_RTREE, EdaRtree))
#define EDA_RTREE_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), EDA_TYPE_RTREE, EdaRtreeClass))
#define EDA_IS_RTREE(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EDA_TYPE_RTREE))
#define EDA_IS_RTREE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((obj), EDA_TYPE_RTREE))
#define EDA_RTREE_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), EDA_TYPE_RTREE, EdaRtreeClass))

typedef struct _EdaRtreeClass EdaRtreeCLass;
typedef struct _EdaRtree EdaRtree;
typedef struct _EdaRtreePrivate EdaRtreePrivate;

struct _EdaRtreeCLass
{
  GObjectClass parent_class;
};

struct _EdaRtree
{
  GObject parent_instance;

  /* Private members */
  EdaRtreePrivate *priv;
};

GType eda_rtree_get_type (void) G_GNUC_CONST;

/* ---------------------------------------------------------------- */

EdaRtree *eda_rtree_new (void) G_GNUC_WARN_UNUSED_RESULT;
void eda_rtree_insert (EdaRtree *tree, const EdaRectangle *bounds, gconstpointer data);
void eda_rtree_remove (EdaRtree *tree, const EdaRectangle *bounds, gconstpointer data);
GList *eda_rtree_search (EdaRtree *tree, const EdaRectangle *query) G_GNUC_WARN_UNUSED_RESULT;

G_END_DECLS

#endif /* !__EDA_RTREE_H__ */
