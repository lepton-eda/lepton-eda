/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 2013 Ales Hvezda
 * Copyright (C) 2013 gEDA Contributors (see ChangeLog for details)
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
/*!
 * \file gschem_options.h
 *
 * \brief
 */

/*! \brief The initial snap size
 *
 *  This value will get replaced by user settings.
 */
#define DEFAULT_SNAP_SIZE (100)


/*! \brief The maximum snap size, inclusive
 *
 *  An even power of two multiplied by 100 ensures the snap size does not get
 *  off track when the user scales to the limit.
 */
#define MAXIMUM_SNAP_SIZE (102400)


/*! \brief The minimum snap size, inclusive
 *
 *  A value of 5 might be better for the minimum. Using the spin widget can
 *  allow the snap size to get off track.
 */
#define MINIMUM_SNAP_SIZE (1)


#define GSCHEM_TYPE_OPTIONS           (gschem_options_get_type ())
#define GSCHEM_OPTIONS(obj)           (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_OPTIONS, GschemOptions))
#define GSCHEM_OPTIONS_CLASS(klass)   (G_TYPE_CHECK_CLASS_CAST ((klass),  GSCHEM_TYPE_OPTIONS, GschemOptionsClass))
#define IS_GSCHEM_OPTIONS(obj)        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_OPTIONS))
#define GSCHEM_OPTIONS_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj),  GSCHEM_TYPE_OPTIONS, GschemOptionsClass))

typedef struct _GschemOptionsClass GschemOptionsClass;
typedef struct _GschemOptions GschemOptions;

struct _GschemOptionsClass {
  GObjectClass parent_class;
};

struct _GschemOptions {
  GObject parent;

  int        grid_mode;
  SNAP_STATE snap_mode;
  int        snap_size;
};

void
gschem_options_cycle_grid_mode (GschemOptions *options);

void
gschem_options_cycle_snap_mode (GschemOptions *options);

GRID_MODE
gschem_options_get_grid_mode (GschemOptions *options);

SNAP_STATE
gschem_options_get_snap_mode (GschemOptions *options);

int
gschem_options_get_snap_size (GschemOptions *options);

GType
gschem_options_get_type ();

GschemOptions*
gschem_options_new ();

void
gschem_options_scale_snap_down (GschemOptions *options);

void
gschem_options_scale_snap_up (GschemOptions *options);

void
gschem_options_set_grid_mode (GschemOptions *options, GRID_MODE grid_mode);

void
gschem_options_set_snap_mode (GschemOptions *options, SNAP_STATE snap_mode);

void
gschem_options_set_snap_size (GschemOptions *options, int snap_size);
