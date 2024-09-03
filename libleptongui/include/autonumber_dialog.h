/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2025 Lepton EDA Contributors
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#ifndef AUTONUMBER_DIALOG_H
#define AUTONUMBER_DIALOG_H

G_BEGIN_DECLS

/* Autonumber widget structs and enums. */
enum {
  AUTONUMBER_SORT_DIAGONAL,
  AUTONUMBER_SORT_YX,
  AUTONUMBER_SORT_YX_REV,
  AUTONUMBER_SORT_XY,
  AUTONUMBER_SORT_XY_REV,
  AUTONUMBER_SORT_FILE
};

enum {
  AUTONUMBER_IGNORE,
  AUTONUMBER_RENUMBER,
  AUTONUMBER_RESPECT
};

enum {
  SCOPE_SELECTED,
  SCOPE_PAGE,
  SCOPE_HIERARCHY
};

typedef struct autonumber_text_t SchematicAutonumber;

/** @brief Stored state of the autonumber text dialog */
struct autonumber_text_t {
  /** @brief Search text history */
  GList *scope_text;

  /** @brief Scope for searching existing numbers */
  gint scope_skip;

  /** @brief Scope for autonumbering text */
  gint scope_number;

  /** @brief Overwrite existing numbers in scope */
  gboolean scope_overwrite;

  /** @brief Sort order */
  gint order;

  /** @brief Starting number for automatic numbering */
  gint startnum;

  /** @brief Remove numbers instead of automatic numbering */
  gboolean removenum;

  /** @brief Automatic assignments of slots */
  gboolean slotting;

  /** @brief Pointer to the dialog */
  GtkWidget *dialog;

  /** @brief Pointer to the SchematicWindow struct */
  SchematicWindow *w_current;

  /* variables used while autonumbering */
  gchar * current_searchtext;
  gint root_page;      /* flag whether its the root page or not */
  GList *used_numbers; /* list of used numbers */
  GList *free_slots;   /* list of FREE_SLOT objects */
  GList *used_slots;   /* list of USED_SLOT objects */
};

typedef struct autonumber_slot_t SchematicAutonumberSlot;

struct autonumber_slot_t {
  gchar *symbolname;     /* or should I use the device name? (Werner) */
  gint number;           /* usually this is the refdes number */
  gint slotnr;      /* just the number of the free slot */
};

/* Construction, destruction */

SchematicAutonumber*
schematic_autonumber_new ();

GtkWidget*
schematic_autonumber_dialog_new (SchematicWindow *w_current);

/* Accessors */

SchematicAutonumber*
schematic_autonumber_get_autotext ();

void
schematic_autonumber_set_autotext (SchematicAutonumber *val);

char*
schematic_autonumber_get_autotext_current_searchtext (SchematicAutonumber *autotext);

void
schematic_autonumber_set_autotext_current_searchtext (SchematicAutonumber *autotext,
                                                      char *val);
GtkWidget*
schematic_autonumber_get_autotext_dialog (SchematicAutonumber *autotext);

void
schematic_autonumber_set_autotext_dialog (SchematicAutonumber *autotext,
                                          GtkWidget *widget);
GList*
schematic_autonumber_get_autotext_free_slots (SchematicAutonumber *autotext);

void
schematic_autonumber_set_autotext_free_slots (SchematicAutonumber *autotext,
                                              GList *val);
gboolean
schematic_autonumber_get_autotext_removenum (SchematicAutonumber *autotext);

void
schematic_autonumber_set_autotext_removenum (SchematicAutonumber *autotext,
                                             gboolean val);
int
schematic_autonumber_get_autotext_root_page (SchematicAutonumber *autotext);

void
schematic_autonumber_set_autotext_root_page (SchematicAutonumber *autotext,
                                             int val);
int
schematic_autonumber_get_autotext_scope_number (SchematicAutonumber *autotext);

void
schematic_autonumber_set_autotext_scope_number (SchematicAutonumber *autotext,
                                                int val);
gboolean
schematic_autonumber_get_autotext_scope_overwrite (SchematicAutonumber *autotext);

void
schematic_autonumber_set_autotext_scope_overwrite (SchematicAutonumber *autotext,
                                                   gboolean val);
int
schematic_autonumber_get_autotext_scope_skip (SchematicAutonumber *autotext);

void
schematic_autonumber_set_autotext_scope_skip (SchematicAutonumber *autotext,
                                              int val);
GList*
schematic_autonumber_get_autotext_scope_text (SchematicAutonumber *autotext);

void
schematic_autonumber_set_autotext_scope_text (SchematicAutonumber *autotext,
                                              GList *val);
gboolean
schematic_autonumber_get_autotext_slotting (SchematicAutonumber *autotext);

void
schematic_autonumber_set_autotext_slotting (SchematicAutonumber *autotext,
                                            gboolean val);
int
schematic_autonumber_get_autotext_startnum (SchematicAutonumber *autotext);

void
schematic_autonumber_set_autotext_startnum (SchematicAutonumber *autotext,
                                            int val);
int
schematic_autonumber_get_autotext_sort_order (SchematicAutonumber *autotext);

void
schematic_autonumber_set_autotext_sort_order (SchematicAutonumber *autotext,
                                              int val);
GList*
schematic_autonumber_get_autotext_used_numbers (SchematicAutonumber *autotext);

void
schematic_autonumber_set_autotext_used_numbers (SchematicAutonumber *autotext,
                                                GList *val);
GList*
schematic_autonumber_get_autotext_used_slots (SchematicAutonumber *autotext);

void
schematic_autonumber_set_autotext_used_slots (SchematicAutonumber *autotext,
                                              GList *val);
SchematicWindow*
schematic_autonumber_get_autotext_window (SchematicAutonumber *autotext);

void
schematic_autonumber_set_autotext_window (SchematicAutonumber *autotext,
                                          SchematicWindow *w_current);

int
schematic_autonumber_slot_get_number (SchematicAutonumberSlot *slot);

void
schematic_autonumber_slot_set_number (SchematicAutonumberSlot *slot,
                                      int number);
int
schematic_autonumber_slot_get_slot_number (SchematicAutonumberSlot *slot);

void
schematic_autonumber_slot_set_slot_number (SchematicAutonumberSlot *slot,
                                           int number);
char*
schematic_autonumber_slot_get_symbol_name (SchematicAutonumberSlot *slot);

void
schematic_autonumber_slot_set_symbol_name (SchematicAutonumberSlot *slot,
                                           char *name);
/* Methods */

void
schematic_autonumber_clear_database (SchematicAutonumber *autotext);

void
schematic_autonumber_collect_used_objects (SchematicAutonumber *autotext,
                                           SchematicWindow *w_current,
                                           GList *pages);
GtkWidget*
schematic_autonumber_dialog_lookup_widget (GtkWidget *widget,
                                           const gchar *widget_name);
void
schematic_autonumber_get_new_numbers (SchematicAutonumber *autotext,
                                      LeptonObject *o_current,
                                      gint *number,
                                      gint *slot);
void
schematic_autonumber_get_used (SchematicWindow *w_current,
                               SchematicAutonumber *autotext);
GList*
schematic_autonumber_history_add (GList *history,
                                  gchar *text);
int
schematic_autonumber_match (SchematicAutonumber *autotext,
                            LeptonObject *o_current,
                            int *number);
void
schematic_autonumber_remove_number (SchematicAutonumber *autotext,
                                    LeptonObject *o_current);
void
schematic_autonumber_run (SchematicAutonumber *autotext,
                          SchematicWindow *w_current,
                          GList *pages,
                          gchar *text_template,
                          gint scope_number);
int
schematic_autonumber_scope_from_string (char *s);

const char*
schematic_autonumber_scope_to_string (int scope);

int
schematic_autonumber_sort_order_from_string (char *s);

const char*
schematic_autonumber_sort_order_to_string (int sort_order);

int
schematic_autonumber_sort_diagonal (gconstpointer a,
                                    gconstpointer b);
int
schematic_autonumber_sort_xy (gconstpointer a,
                              gconstpointer b);
gint
schematic_autonumber_sort_xy_rev (gconstpointer a,
                                  gconstpointer b);
int
schematic_autonumber_sort_yx (gconstpointer a,
                              gconstpointer b);
int
schematic_autonumber_sort_yx_rev (gconstpointer a,
                                  gconstpointer b);
void
schematic_autonumber_sort_order_widget_init (GtkWidget *sort_order);

G_END_DECLS

#endif /* AUTONUMBER_DIALOG_H */
