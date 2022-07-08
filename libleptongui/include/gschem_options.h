/* Lepton EDA Schematic Capture
 * Copyright (C) 2013 Ales Hvezda
 * Copyright (C) 2013-2014 gEDA Contributors
 * Copyright (C) 2017-2022 Lepton EDA Contributors
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

/*! \brief The initial mode for magnetic nets
 *
 *  TRUE = enabled
 *  FALSE = disabled
 *
 *  Loading a configuration will overwrite this value
 */
#define DEFAULT_MAGNETIC_NET_MODE (TRUE)


/*! \brief The initial mode for rubber band nets
 *
 *  TRUE = enabled
 *  FALSE = disabled
 *
 *  Loading a configuration will overwrite this value
 */
#define DEFAULT_NET_RUBBER_BAND_MODE (TRUE)


/*! \brief The initial grid mode
 *
 *  This value will get replaced by user settings.
 */
#define DEFAULT_GRID_MODE (GRID_MODE_MESH)


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
  gboolean   magnetic_net_mode;
  gboolean   net_rubber_band_mode;
  SchematicSnapMode snap_mode;
  int        snap_size;
};

G_BEGIN_DECLS

void
gschem_options_cycle_grid_mode (GschemOptions *options);

void
gschem_options_cycle_magnetic_net_mode (GschemOptions *options);

void
gschem_options_cycle_net_rubber_band_mode (GschemOptions *options);

void
gschem_options_cycle_snap_mode (GschemOptions *options);

GRID_MODE
gschem_options_get_grid_mode (GschemOptions *options);

gboolean
gschem_options_get_magnetic_net_mode (GschemOptions *options);

gboolean
gschem_options_get_net_rubber_band_mode (GschemOptions *options);

SchematicSnapMode
gschem_options_get_snap_mode (GschemOptions *options);

int
gschem_options_get_snap_size (GschemOptions *options);

GType
gschem_options_get_type ();

GschemOptions*
gschem_options_new ();

void
gschem_options_set_grid_mode (GschemOptions *options, GRID_MODE grid_mode);

void
gschem_options_set_magnetic_net_mode (GschemOptions *options, gboolean enabled);

void
gschem_options_set_net_rubber_band_mode (GschemOptions *options, gboolean enabled);

void
gschem_options_set_snap_mode (GschemOptions *options, SchematicSnapMode snap_mode);

void
gschem_options_set_snap_size (GschemOptions *options, int snap_size);

SchematicSnapMode
schematic_snap_mode_from_string (char *s);

const char*
schematic_snap_mode_to_string (SchematicSnapMode mode);

G_END_DECLS
