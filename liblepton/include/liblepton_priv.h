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

#include "color.h"
#include "fill.h"
#include "point.h"
#include "str.h"
#include "stroke.h"

#include "angle.h"
#include "arc.h"
#include "bezier.h"
#include "bounds.h"
#include "box.h"
#include "circle.h"
#include "component.h"
#include "coord.h"
#include "line.h"
#include "path.h"
#include "picture.h"
#include "sch2pcb.h"
#include "text.h"
#include "transform.h"

#include "forward.h"

#include "struct.h"
#include "object.h"
#include "object_list.h"
#include "page.h"
#include "toplevel.h"
#include "undo.h"

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

#include "list.h"

/* Private headers */
#include "defines_priv.h"
#include "prototype_priv.h"
#include "i_vars_priv.h"

/* Gettext translation */
#include "gettext_priv.h"

#endif /* LIBLEPTON_PRIV_H_ */
