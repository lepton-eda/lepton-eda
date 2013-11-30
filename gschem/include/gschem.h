/* System headers which gschem headers rely on */
#include <glib.h>
#include <gtk/gtk.h>
#include <libguile.h>
#include <libgeda/libgeda.h>
#include <libgeda/libgedaguile.h>
#include <libgedacairo/libgedacairo.h>

/* forward declaration, until everyone stops referencing it */
typedef struct st_gschem_toplevel GschemToplevel;

/* gschem headers */
#include "gschem_defines.h"
#include "gschem_bottom_widget.h"
#include "gschem_macro_widget.h"
#include "gschem_page_geometry.h"
#include "gschem_page_view.h"
#include "gschem_main_window.h"
#include "gschem_selection_adapter.h"
#include "gschem_struct.h"
#include "gschem_toplevel.h"
#include "gschem_accel_label.h"
#include "gschem_action.h"
#include "gschem_dialog.h"
#include "gschemhotkeystore.h"
#include "i_vars.h"
#include "x_preview.h"
#include "x_compselect.h"
#include "x_dialog.h"
#include "x_editlprop.h"
#include "x_log.h"
#include "x_pagesel.h"
#include "x_states.h"
#include "gschem_swatch_column_renderer.h"
#include "globals.h"
#include "prototype.h"
#include "gschem_integer_combo_box.h"

/* Gettext translation */
#include "gettext.h"
