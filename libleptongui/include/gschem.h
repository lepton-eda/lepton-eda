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
typedef struct st_schematic_window SchematicWindow;

/* gschem headers */
#include "action.h"
#include "action_mode.h"
#include "grid_mode.h"
#include "gtk_helper.h"
#include "snap_mode.h"
#include "schematic_defines.h"
#include "bin.h"
#include "bottom_widget.h"
#include "gschem_find_text_state.h"
#include "gschem_find_text_widget.h"
#include "gschem_log_widget.h"
#include "gschem_macro_widget.h"
#include "viewport.h"
#include "canvas.h"
#include "gschem_pin_type_combo.h"
#include "gschem_main_window.h"
#include "gschem_selection_adapter.h"
#include "gschem_show_hide_text_widget.h"
#include "gschem_struct.h"
#include "options.h"
#include "accel_label.h"
#include "gschem_dialog.h"
#include "dialog_misc.h"
#include "gschemhotkeystore.h"
#include "i_vars.h"
#include "preview_widget.h"
#include "x_compselect.h"
#include "x_dialog.h"
#include "x_tabs.h"
#include "gschem_swatch_column_renderer.h"
#include "gschem_fill_swatch_cell_renderer.h"
#include "globals.h"
#include "prototype.h"
#include "gschem_integer_combo_box.h"
#include "binding.h"
#include "binding_integer.h"
#include "window.h"

#include "gschem_object_properties_widget.h"
#include "options_widget.h"
#include "gschem_text_properties_widget.h"
#include "gschem_translate_widget.h"

#include "color_edit_widget.h"
#include "font_select_widget.h"
#include "page_select_widget.h"
#include "toolbar.h"

/* Gettext translation */
#include "gettext.h"


#endif /* LEPTON_MAIN_HEADER_H_ */
