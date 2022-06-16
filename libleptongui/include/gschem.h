#ifndef LEPTON_MAIN_HEADER_H_
#define LEPTON_MAIN_HEADER_H_

/* System headers which gschem headers rely on */
#include <glib.h>
#include <gtk/gtk.h>
#include <libguile.h>
#include <liblepton/liblepton.h>
#include <liblepton/libleptonguile.h>
#include <liblepton/edarenderer.h>
#include <liblepton/edacairo.h>


/* forward declaration, until everyone stops referencing it */
typedef struct st_gschem_toplevel GschemToplevel;

/* gschem headers */
#include "action_mode.h"
#include "grid_mode.h"
#include "snap_mode.h"
#include "gschem_defines.h"
#include "gschem_bin.h"
#include "gschem_bottom_widget.h"
#include "gschem_find_text_state.h"
#include "gschem_find_text_widget.h"
#include "gschem_log_widget.h"
#include "gschem_macro_widget.h"
#include "gschem_page_geometry.h"
#include "gschem_page_view.h"
#include "gschem_pin_type_combo.h"
#include "gschem_main_window.h"
#include "gschem_selection_adapter.h"
#include "gschem_show_hide_text_widget.h"
#include "gschem_struct.h"
#include "gschem_options.h"
#include "gschem_toplevel.h"
#include "gschem_accel_label.h"
#include "gschem_action.h"
#include "gschem_dialog.h"
#include "gschem_dialog_misc.h"
#include "gschemhotkeystore.h"
#include "i_vars.h"
#include "gschem_preview.h"
#include "x_compselect.h"
#include "x_dialog.h"
#include "x_tabs.h"
#include "gschem_swatch_column_renderer.h"
#include "gschem_fill_swatch_cell_renderer.h"
#include "globals.h"
#include "prototype.h"
#include "gschem_integer_combo_box.h"
#include "gschem_binding.h"
#include "gschem_binding_integer.h"

#include "gschem_object_properties_widget.h"
#include "gschem_options_widget.h"
#include "gschem_text_properties_widget.h"
#include "gschem_translate_widget.h"

#include "color_edit_widget.h"
#include "font_select_widget.h"
#include "page_select_widget.h"
#include "toolbar.h"

/* Gettext translation */
#include "gettext.h"


#endif /* LEPTON_MAIN_HEADER_H_ */
