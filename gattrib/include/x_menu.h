/* This is the stuff to define the graphical widgets used 
 * by gattrib.  I had to separate them off from the other
 * structs in order to define the callbacks first.  The
 * include order required is:
   #include <libgeda/libgeda.h>     
   #include "../include/struct.h"   
   #include "../include/prototype.h"
   #include "../include/globals.h"  
   #include "../include/x_menu.h"   
 */


#ifndef X_MENU_H
#define X_MENU_H

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkkeysyms.h>

#include <glib.h>
#ifdef HAS_GTK22
#include <glib-object.h>
#endif
 

/* -------  Includes needed to make the GTK stuff work  ------ */

#ifdef HAS_GTK22
#include "gtksheet_2_2.h"
#include "gtkitementry_2_2.h"
#else
#include "gtksheet_1_2.h"
#include "gtkitementry_1_2.h"
#endif

/* #include "pixmaps.h" */

/* ----------  Menu definition -- copied from the GTK tutorial file  ---------- */
/* ----------  and edited/adapted by SDB for gattrib.  ---------- */
/* This is the GtkItemFactoryEntry structure used to generate new menus.
   Item 1: The menu path. The letter after the underscore indicates an
           accelerator key once the menu is open.
   Item 2: The accelerator key for the entry
   Item 3: The callback function.
   Item 4: The callback action.  This changes the parameters with
           which the function is called.  The default is 0.
   Item 5: The item type, used to define what kind of an item it is.
           Here are the possible values:

           NULL               -> "<Item>"
           ""                 -> "<Item>"
           "<Title>"          -> create a title item
           "<Item>"           -> create a simple item
           "<CheckItem>"      -> create a check item
           "<ToggleItem>"     -> create a toggle item
           "<RadioItem>"      -> create a radio item
           <path>             -> path of a radio item to link against
           "<Separator>"      -> create a separator
           "<Branch>"         -> create an item to hold sub items (optional)
           "<LastBranch>"     -> create a right justified branch 
*/

static GtkItemFactoryEntry menu_items[] = {
  /* ------- File menu ------- */
  { "/_File", 
    NULL,
    NULL, 0, 
    "<Branch>" 
  },
  { "/File/_Open",
    "<control>O", 
    s_toplevel_menubar_file_open, 
    0, 
    NULL 
  },
  { "/File/_Save",
    "<control>S", 
    s_toplevel_menubar_file_save, 
    0, 
    NULL },
  { "/File/sep1",    
    NULL,    
    NULL,
    0, 
    "<Separator>"
  },
  { "/File/Print setup",
    NULL,
    s_toplevel_menubar_unimplemented_feature,
    0, 
    NULL 
  },
  { "/File/Print",
    NULL,
    s_toplevel_menubar_unimplemented_feature,
    0, 
    NULL },
  { "/File/sep1",    
    NULL,    
    NULL,
    0, 
    "<Separator>"
  },
  { "/File/Quit",   
    "<control>Q", 
    gattrib_really_quit,
    0, 
    NULL
  },
  /* ------- Edit menu ------- */
  { "/_Edit",    
    NULL,    
    NULL, 
    0, 
    "<Branch>" 
  },
  { "/Edit/Add new attrib column",      
    NULL,  
    s_toplevel_menubar_edit_newattrib, 
    0, 
    NULL
  },
  { "/Edit/Delete attrib column",  
    NULL,  
    s_toplevel_menubar_edit_delattrib,
    0,
    NULL 
  },
  { "/Edit/Search for attrib value",
    NULL,
    s_toplevel_menubar_unimplemented_feature,
    0, 
    NULL 
  },
  { "/Edit/Search and replace attrib value", 
    NULL, 
    s_toplevel_menubar_unimplemented_feature,
    0, 
    NULL 
  },
  { "/Edit/Search for refdes", 
    NULL,
    s_toplevel_menubar_unimplemented_feature,
    0,
    NULL 
  },
  /* ------- Options menu ------- */
  { "/_Options",    
    NULL,
    NULL,
    0, 
    "<Branch>" 
  },
  { "/Options/Set attrib visibility",
    NULL,
    NULL,
    0,
    "<Branch>"
  },
  { "/Options/Set attrib visibility/invisible",
    NULL,
    s_toplevel_menubar_unimplemented_feature,
    0,
    NULL
  },
  { "/Options/Set attrib visibility/value only",
    NULL,
    s_toplevel_menubar_unimplemented_feature,
    0,
    NULL
  },
  { "/Options/Set attrib visibility/name only",
    NULL,
    s_toplevel_menubar_unimplemented_feature,
    0, 
    NULL
  },
  { "/Options/Set attrib visibility/name and value visible", 
    NULL,
    s_toplevel_menubar_unimplemented_feature,
    0,
    NULL
  },
  /* ------- Help menu ------- */
  { "/_Help",         NULL,         NULL, 0, "<LastBranch>" },
  { "/_Help/About",   NULL,         x_dialog_about_dialog, 0, NULL },
};


#endif


