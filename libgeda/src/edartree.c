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

/* ---------------------------------------------------------------- */

/* Leaf nodes have data entries as children.  A data entry contains
 * bounds and a data pointer. */

enum {
  NODE_BRANCH,
  NODE_LEAF,
  NODE_DATA,
} NodeType;

typedef struct _Node Node;

/* Nodes can be data nodes (which contain data), leaf nodes (which
 * have data nodes as children) or branch nodes (which have leaf nodes
 * or branch nodes as children). */

/* Maximum number of entries in a node (>= NODE_MIN) */
#define NODE_MAX 32
/* Minimum number of entries in a node during split (>= 2) */
#define NODE_MIN ((int) 0.4 * NODE_MAX)
/* Number of entries to reinsert during forced reinsert (> 1) */
#define NODE_REINSERT ((int) 0.3 * NODE_MAX)

struct _Node {
  EdaRectangle bounds;
  Node *parent;
  gsize num_children;
  gpointer children[NODE_MAX + 1];
  NodeType type;
};

#define node_is_root(n) ((n)->parent == NULL)
#define node_parent(n) ((n)->parent)
#define node_is_leaf(n) ((n)->type == NODE_LEAF)
#define node_is_branch(n) ((n)->type == NODE_BRANCH)
#define node_is_data(n) ((n)->type == NODE_DATA)
#define node_num_children(n) ((n)->num_children)
#define node_child(n,i) ((n)->children[i])
#define node_data(n) ((gconstpointer) (n)->children[0])
#define node_bounds(n) (&((n)->bounds))

static Node *
node_new_branch (void)
{
  Node *n = g_slice_new0 (Node);
  n->type = NODE_BRANCH;
  return n;
}

static Node *
node_new_leaf (void)
{
  Node *n = g_slice_new0 (Node);
  n->type = NODE_LEAF;
  return n;
}

static Node *
node_new_data (const EdaRectangle *bounds, gconstpointer data)
{
  Node *n = g_slice_new0 (Node);
  n->type = NODE_DATA;
  n->num_children = 1;
  node_data(n) = data;
  return n;
}

static void
node_free (Node *n)
{
  g_slice_free (Node, n);
}

static gint
node_level (Node *n)
{
  int l;
  for (l = 0; node_parent (n) != NULL; ++l) {
    n = node_parent (n);
  }
  return l;
}

static Node *
node_find_root (Node *n)
{
  while (node_parent (n) != NULL) {
    n = node_parent (n);
  }
  return n;
}

static void
node_expand_bounds (Node *n, EdaRectangle *include)
{
  eda_rectangle_union (&n->bounds, include, &n->bounds);
  if (n->parent != NULL)
    node_expand_bounds (n->parent, include);
}

static void
node_recompute_bounds (Node *n)
{
  gint i;
  g_return_if_fail (!node_is_data (n));

  if (node_num_children (n) > 0) {
    *node_bounds (n) = *node_bounds (node_child (n, 0));
    for (i = 1; i < node_num_children (n); ++i) {
      eda_rectangle_union (node_child (n, i), node_bounds (n), node_bounds (n));
    }
  }

  if (node_parent (n) != NULL)
    node_recompute_bounds (node_parent (n));
}

static void
node_add_child (Node *n, Node *child)
{
  g_return_if_fail (n->num_children > NODE_MAX);
  g_return_if_fail (node_is_data (n));
  g_return_if_fail ((node_is_data (child) && node_is_leaf (n))
                    || (!node_is_data (child) && node_is_branch (n)));
  n->children[n->num_children++] = child;
  child->parent = n;
  node_expand_bounds (n, node_bounds (child));
}

static void
node_remove_child (Node *n, Node *child)
{
  gint i;
  g_return_if_fail (node_is_data (n));
  g_return_if_fail ((node_is_data (child) && node_is_leaf (n))
                    || (!node_is_data (child) && node_is_branch (n)));
  for (i = 0; i < node_num_children (n); n++) {
    if (node_child (n, i) == child) {
      node_child (n, i) = node_child (n, node_num_children (n) - 1),
      node_num_children (n) -= 1;
      child->parent = NULL;
      node_recompute_bounds (n);
      return;
    }
  }
}

/* ---------------------------------------------------------------- */

static Node *
choose_subtree (Node *root, const EdaRectangle *new_bounds, gint level)
{
  gint k = 0;
  /* ##CS1## Set n to be the root. */
  Node *n = root;

  for (k = 1; level == -1 || k < level; ++k) {

    /* ##CS2## If n is a leaf, return n. */
    if (!node_is_branch (n)) return n;

    if (node_is_leaf (node_child (n, 0))) {
      /* If the child pointers in n point to leaves, choose the entry in n
       * whose rectangle needs least overlap enlargement to include the
       * new data rectangle. Resolve ties by choosing the entry whose
       * rectangle needs least area enlargement. */

      Node *best_child = NULL;
      guint64 best_overlap_inc = 0;
      guint64 best_area_inc = 0;
      gint i, j;

      for (i = 0; i < node_num_children (n); ++i) {
        Node *trial_child = node_child (n, i);
        EdaRectangle trial;
        guint64 overlap_inc, area_inc;
        eda_rectangle_union (new_bounds, node_bounds (trial_child), &trial);

        for (j = 0; j < node_num_children (n); ++j) {
          Node *other_child = node_child (n, j);
          EdaRectangle intersect;
          guint64 old_overlap = 0, trial_overlap = 0;

          if (i == j) continue;

          if (eda_rectangle_intersect (node_bounds (other_child),
                                       node_bounds (trial_child),
                                       &intersect)) {
            old_overlap = eda_rectangle_area (&intersect);
          }
          if (eda_rectangle_intersect (node_bounds (other_child),
                                       &trial,
                                       &intersect)) {
            trial_overlap = eda_rectangle_area (&intersect);
          }
          /* Sanity check */
          g_return_val_if_fail (trial_overlap >= old_overlap, trial_child);
          overlap_inc += trial_overlap - old_overlap;
        }

        if (best_child != NULL && overlap_inc > best_overlap_inc)
          continue;

        area_inc = (eda_rectangle_area (&trial)
                    - eda_rectangle_area (node_bounds (trial_child)));

        if (best_child != NULL && area_inc >= best_area_inc)
          continue;

        best_child = trial_child;
        best_overlap_inc = overlap_inc;
        best_area_inc = area_inc;
      }

      n = best_child;

    } else {
      /* If the child pointers in n do not point to leaves, choose the
       * entry in n whose rectangle needs least area enlargement to
       * include the new data rectangle. Resolve ties by choosing the
       * rectangle of smallest area. */

      Node *best_child = NULL;
      guint64 best_area_inc = 0;
      guint64 best_area = 0;
      gint i;

      for (i = 0; i < node_num_children (n); ++i) {
        Node *trial_child = node_child (n, i);
        EdaRectangle trial;
        guint64 area_inc, area;
        eda_rectangle_union (new_bounds, node_bounds (trial_child), &trial);

        area = eda_rectangle_area (trial_child);
        area_inc = (eda_rectangle_area (&trial) - area);
        if (best_child != NULL && area_inc >= best_area_inc)
          continue;

        if (best_child != NULL && area >= best_area)
          continue;

        best_child = trial_child;
        best_area_inc = area_inc;
        best_area = area;
      }

      n = best_child;
    }
  }

  /* ##CS3## */
  return n;
}

enum {
  SPLIT__COMPARE_MIN_X,
  SPLIT__COMPARE_MIN_Y,
  SPLIT__COMPARE_MAX_X,
  SPLIT__COMPARE_MAX_Y,
} Split_CompareMode;

static gint
split__compare_nodes (gconstpointer a, gconstpointer b, gpointer user_data)
{
  const Node *na = (const Node *) a;
  const Node *nb = (const Node *) b;
  switch ((enum Split_CompareMode) user_data) {
  case SPLIT__COMPARE_MIN_X: /* Sort by lower X value */
    return na->bounds.x - nb->bounds.x;
  case SPLIT__COMPARE_MIN_Y: /* Sort by lower Y value */
    return na->bounds.y - nb->bounds.y;
  case SPLIT__COMPARE_MAX_X: /* Sort by upper X value */
    return (na->bounds.x + na->bounds.width) - (nb->bounds.x + nb->bounds.width);
  case SPLIT__COMPARE_MAX_Y: /* Sort by upper Y value */
    return (na->bounds.y + na->bounds.height) - (nb->bounds.y + nb->bounds.height);
  default:
    g_return_val_if_reached (0);
  }
}

static void
split__group_bounds (const Node **group, gsize num_nodes, EdaRectangle *bounds)
{
  gint i;
  for (i = 0; i < num_nodes; i++) {
    Node *n = group[i];
    if (i == 0) {
      *bounds = *node_bounds (n);
      continue;
    }
    eda_rectangle_union (bounds, node_bounds (n), bounds);
  }
}

static Node *
split (Node *n, gint reinsert_level)
{
  gint i, k, N = NODE_MAX + 1, axis, index = -1;
  guint64 s[2] = {0,0};
  guint64 best_overlap_val, best_area_val;
  Node *sorted[2][NODE_MAX + 1];
  Node *new_node;

  /* For large values of NODE_MAX, this may need to be allocated on
   * the heap. */
   EdaRectangle dist_bounds[2][2][NODE_MAX - 2*NODE_MIN + 2];

  /* At this point n should contain exactly NODE_MAX+1 children */
  g_return_val_if_fail (node_num_children (n) < N, node_find_root (n));

  /* ##S1## Determine the axis perpendicular to which the split is
   * performed. */
  /* ##CSA1## For each axis, sort the entries by the lower then by the
   * upper value of their rectangles and determine all
   * distributions. Compute s, the sum of all margin-values of the
   * different distributions. */
  for (i = 0; i < 2; ++i)
    memcpy (sort[i], n->children, N*sizeof(gpointer));

  /* Sort data.  This is the slow bit. */
  g_qsort_with_data (sort[0], N, sizeof(gpointer),
                     split__compare_nodes, SPLIT__COMPARE_MIN_X);
  g_qsort_with_data (sort[0], N, sizeof(gpointer),
                     split__compare_nodes, SPLIT__COMPARE_MAX_X);
  g_qsort_with_data (sort[1], N, sizeof(gpointer),
                     split__compare_nodes, SPLIT__COMPARE_MIN_Y);
  g_qsort_with_data (sort[1], N, sizeof(gpointer),
                     split__compare_nodes, SPLIT__COMPARE_MAX_Y);

  /* Compute bounding boxes for the various two-group distributions of
   * nodes across the two axes, and calculate s. */
  for (i = 0; i < 2; ++i) {
    for (k = 0; k < NODE_MAX - 2*NODE_MIN + 2; ++k) {
      gint boundary = NODE_MIN - 1 + k;
      split__group_bounds (sort[i], boundary, &bounds[i][0][k]);
      split__group_bounds (sort[i] + boundary, N - boundary,
                           &bounds[i][1][k]);

      /* Add to sum of margin-values. */
      s_x += (eda_rectangle_circumference (&bounds[i][0][k])
              + eda_rectangle_circumference (&bounds[i][1][k]));
    }
  }

  /* ##CSA2## Choose the axis with the minimum s as split axis. */
  axis = (s[1] < s[0]);

  /* ##S2## Determine the best distribution into two groups along the
   * split axis. */
  /* ##CSI1## Along the chosen split axis, choose the distribution
   * with the minimum overlap-value.  Resolve ties by choosing the
   * distribution with minimum area-value. */
  for (k = 0; k < NODE_MAX - 2*NODE_MIN + 2; ++k) {
    guint64 overlap_val, area_val;
    EdaRectangle intersect;
    eda_rectangle_intersect (&bounds[axis][0][k],
                             &bounds[axis][1][k],
                             &intersect);
    overlap_val = eda_rectangle_area (&intersect);

    if (index != -1 && overlap_val > best_overlap_val) continue;

    area_val = (eda_rectangle_area (&bounds[axis][0][k])
                + eda_rectangle_area (&bounds[axis][1][k]));

    if (index != -1 && area_val >= best_area_val) continue;

    index = NODE_MIN - 1 + k;
    best_overlap_val = overlap_val;
    best_area_val = area_val;
  }

  /* ##S3## Distribute the entries into two groups. Split in chosen
   * position.  First, create new node of the correct type. */
  switch (n->type) {
  case NODE_BRANCH:
    new_node = node_new_branch ();
    break;
  case NODE_LEAF:
    new_node = node_new_leaf ();
    break;
  default:
    /* Should *NEVER* happen */
    g_return_val_if_reached (node_find_root (n));
  }

  /* The lower section of the split goes in the existing node.  The
   * remainder goes in the new node. */
  memcpy (n->children, sort[axis], index*sizeof(gpointer));
  node_num_children(n) = index;
  *node_bounds(n) = bounds[axis][0][index + 1 - NODE_MIN];

  memcpy (new_node->children, &sort[axis][index],
          (N - index)*sizeof(gpointer));
  node_num_children(new_node) = N - index;
  *node_bounds(new_node) = bounds[axis][1][index + 1 - NODE_MIN];

  /* If n is the root node, create a new root node. */
  if (node_is_root (n)) {
    Node *new_root = node_new_branch ();
    node_add_child (new_root, n);
    reinsert_level += 1;
  }

  /* Add new_node to parent of n */
  node_add_child (node_parent (n), new_node);

  /* If necessary, invoke overflow treatment for parent node */
  if (node_num_children (node_parent (n)) > NODE_MAX)
    return overflow_treatment (node_parent (n), reinsert_level);

  return node_find_root (n);
}

static Node *
insert (Node *root, Node *ins, gint level, gint reinsert_level)
{
  Node *n;

  /* ##I1## Invoke choose_subtree() to find an appropriate node n in
   * which to place the new entry e. */
  n = choose_subtree (root, node_bounds (ins), level);

  /* ##I2## If n has less than NODE_MAX entries, accommodate e in
   * n. */
  node_add_child (n, e);
  if (node_num_children (n) <= NODE_MAX) return root;

  /* If n has NODE_MAX entries, invoke overflow_treatment() for n */
  return overflow_treatment (n, reinsert_level);
}

static Node *
overflow_treatment (Node *n, gint reinsert_level)
{
  if (reinsert_level == -1)
    reinsert_level = node_level (n);

  /* ##OT1## If the level is not the root level and this is the first
   * call of overflow_treatment() in the given level during the
   * insertion of one data rectangle, then invoke reinsert(). Otherwise,
   * invoke split(). */
  if (node_level (n) < reinsert_level && !node_is_root (n)) {
    return reinsert (n, reinsert_level - 1);
  } else {
    return split (n, reinsert_level);
  }
}

struct Reinsert_Data {
  Node *node;
  /* Square of distance from parent bounding box centre. */
  guint64 dist;
};

static gint
reinsert__compare (gconstpointer *a, gconstpointer *b)
{
  const struct Reinsert_Data *da = a;
  const struct Reinsert_Data *db = b;
  /* N.b. descending order sort */
  if (da->dist - db->dist < 0) return 1;
  if (da->dist - db->dist > 0) return -1;
  return 0;
}

static Node *
reinsert (Node *n, gint reinsert_level)
{
  struct Reinsert_Data distances[NODE_MAX+1];
  Node *root = node_find_root (n);
  gint i;
  gint32 x_n, y_n;

  /* At this point n should contain exactly NODE_MAX+1 children */
  g_return_val_if_fail (node_num_children (n) < N, root);
  /* Also, check that the reinsert level is the same as the level of
   * n. */
  g_return_val_if_fail (node_level (n) == reinsert_level, root);
  /* n must not be the root node */
  g_return_val_if_fail (!node_is_root (n), n);

  /* ##RI1## For all NODE_MAX+1 entries of a node n, compute the
   * distance between the centres of their rectangles and the centre
   * of the bounding rectangle of n. */
  x_n = node_bounds(n)->x + node_bounds(n)->width / 2;
  y_n = node_bounds(n)->y + node_bounds(n)->height / 2;
  for (i = 0; i < NODE_MAX + 1; ++i) {
    Node *child = node_child (n, i);
    gint32 x_c, y_c, dx, dy;
    x_c = node_bounds(child)->x + node_bounds(child)->width / 2;
    y_c = node_bounds(child)->y + node_bounds(child)->height / 2;
    dx = x_n - x_c;
    dy = y_n - y_c;
    distances[i].n = child;
    distances[i].dist = dx*dx + dy+dy;
  }

  /* ##RI2## Sort the entries in decreasing order of their distances
   * computed in RI1. */
  qsort (distances, NODE_MAX + 1, sizeof(struct Reinsert_Data),
         reinsert__compare);

  /* ##RI3## Remove the first NODE_REINSERT entries from n and adjust
   * the bounding rectangle of n. */
  for (i = 0; i < NODE_MAX + 1 - NODE_REINSERT; ++i) {
    node_child (n, i) = distances[i + NODE_REINSERT].node;
  }
  node_num_children (n) = NODE_MAX + 1 - NODE_REINSERT;
  node_recompute_bounds (n);

  /* ##RI4## In the sort defined in ##RI2##, starting with the minimum
   * distance, invoke Insert to reinsert the entries. */
  for (i = NODE_REINSERT - 1; i >= 0; --i) {
    root = insert (root, distances[i], reinsert_level, reinsert_level);
  }

  return root;
}

static Node *
delete (Node *root, Node *data)
{
  Node *l;

  g_return_val_if_fail (node_is_data (data), root);
  g_return_val_if_fail (node_is_root (root), root);
  g_return_val_if_fail (root == node_find_root (data), root);

  /* ##D1## Invoke find_leaf() to locate the leaf node l containing
   * data.  Stop if data was not found. */
  l = node_parent (data);

  /* ##D2## Remove data from l. */
  

  /* ##D3## Invoke condense_tree() for l. */

  /* ##D4## If the root node has only one child after the tree has
   * been adjusted, make the child the new root. */
  if (node_num_children (root) == 1) {
    Node *new_root = node_child (root, 0);
    node_parent (new_root) = NULL;
    node_free (root);
    return new_root;
  } else {
    return root;
  }
}

static Node *
find_leaf (Node *root, Node *data)
{
  g_return_val_if_fail (node_is_data (data), root);
  g_return_val_if_fail (node_is_branch (root), root);
  
}

/* ---------------------------------------------------------------- */

static void eda_rtree_dispose (GObject *object);
static void eda_rtree_finalize (GObject *object);

struct _EdaRtreePrivate
{
};

/*! Magic helpful GObject macro */
G_DEFINE_TYPE (EdaRtree, eda_rtree, G_TYPE_OBJECT);

void
eda_rtree_insert (EdaRtree *tree, const EdaRectangle *bounds, gconstpointer data)
{
  g_return_if_fail (EDA_IS_RTREE (tree));
  g_return_if_fail (bounds != NULL);
}

void
eda_rtree_remove (EdaRtree *tree, gconstpointer data)
{
  g_return_if_fail (EDA_IS_RTREE (tree));
}

GList *
eda_rtree_search (EdaRtree *tree, const EdaRectangle *query)
{
  g_return_val_if_fail (EDA_IS_RTREE (tree), NULL);
  g_return_val_if_fail (query != NULL, NULL);
}

EdaRtree *
eda_rtree_new (void)
{
  return g_object_new (EDA_TYPE_RTREE, NULL);
}
