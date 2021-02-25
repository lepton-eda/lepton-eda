#ifndef LIBLEPTON_PRIV_H_
#define LIBLEPTON_PRIV_H_

/* System headers */
#include <glib.h>
#include <glib-object.h>
#include <libguile.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <glib/gstdio.h>

/* Public headers */
#define __LIBLEPTON_INTERNAL_API__ 1

#include "defines.h"

#include "geda_color.h"
#include "geda_fill_type.h"
#include "geda_line_type.h"
#include "geda_line_cap_type.h"
#include "geda_point.h"
#include "geda_string.h"

#include "geda_angle.h"
#include "geda_arc.h"
#include "geda_bezier.h"
#include "geda_bounds.h"
#include "geda_box.h"
#include "geda_circle.h"
#include "geda_component.h"
#include "geda_coord.h"
#include "geda_line.h"
#include "geda_path.h"
#include "geda_picture.h"
#include "geda_text.h"
#include "geda_transform.h"

#include "geda_forward.h"

#include "struct.h"
#include "object.h"
#include "object_list.h"
#include "geda_page.h"
#include "geda_toplevel.h"
#include "geda_undo.h"

#include "component_object.h"
#include "arc_object.h"
#include "box_object.h"
#include "bus_object.h"
#include "circle_object.h"
#include "line_object.h"
#include "net_object.h"
#include "path_object.h"
#include "picture_object.h"
#include "pin_object.h"
#include "text_object.h"

#include "globals.h"
#include "o_types.h"
#include "prototype.h"
#include "funcs.h"
#include "edaconfig.h"
#include "edaerrors.h"
#include "edapaths.h"

#include "geda_list.h"

/* Private headers */
#include "defines_priv.h"
#include "prototype_priv.h"
#include "i_vars_priv.h"

/* Gettext translation */
#include "gettext_priv.h"

#endif /* LIBLEPTON_PRIV_H_ */
