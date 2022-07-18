/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2016 Peter Brett <peter@peter-b.co.uk>
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

#include <config.h>
#include "gschem.h"


/*! \brief Creates a new GtkImage displaying a GTK stock icon if available.
 *  \par Function Description
 *
 * GTK3: Returns a GtkImage pixmap by the stock name \a stock.
 *
 * GTK2: The same as above though if a stock GTK icon with the
 * requested name was not found, this function falls back to the
 * bitmap icons provided in the distribution.
 *
 * \param stock Name of the stock icon ("new", "open", etc.)
 * \return Pointer to the new GtkImage object.
 */
static GtkWidget*
get_stock_pixmap (const char *stock)
{
  GtkWidget *wpixmap = NULL;
#ifdef ENABLE_GTK3
  /* Look up the icon in the icon theme. */
  wpixmap = gtk_image_new_from_icon_name (stock,
                                          GTK_ICON_SIZE_LARGE_TOOLBAR);
#else
  GtkStockItem item;

  gchar *stockid=g_strconcat("gtk-", stock, NULL);

  /* First check if GTK knows this icon */
  if(gtk_stock_lookup(stockid, &item)) {
    wpixmap = gtk_image_new_from_stock(stockid,
                                       GTK_ICON_SIZE_LARGE_TOOLBAR);
  } else {
    /* Look up the icon in the icon theme */
    wpixmap = gtk_image_new_from_icon_name (stock,
                                            GTK_ICON_SIZE_LARGE_TOOLBAR);
  }

  g_free(stockid);
#endif

  return wpixmap;
}


/*! \brief Set button icon.
 *  \par Function Description
 *
 * Sets the image file name to set the button pixmap from.
 *
 * \param [in] button The button widget pointer.
 * \param [in] icon_widget The filename of the image.
 */
void
schematic_toolbar_button_set_icon_widget (GtkWidget *button,
                                          const gchar *icon_name)
{
  g_return_if_fail (button != NULL);

  GtkWidget *icon = get_stock_pixmap (icon_name);
  gtk_tool_button_set_icon_widget (GTK_TOOL_BUTTON (button), icon);
}


/*! \brief Set button label.
 *  \par Function Description
 *
 * Sets the button label text to the given \a label.
 *
 * \param button [in] The button widget pointer.
 * \param label [in] The new label.
 */
void
schematic_toolbar_button_set_label (GtkWidget *button,
                                    const gchar *label)
{
  g_return_if_fail (button != NULL);

  gtk_tool_button_set_label (GTK_TOOL_BUTTON (button), label);
}


/*! \brief Set button tooltip.
 *  \par Function Description
 *
 * Sets the button tooltip text to the given \a tooltip.
 *
 * \param [in] button The button widget pointer.
 * \param [in] tooltip The new tooltip text.
 */
void
schematic_toolbar_button_set_tooltip_text (GtkWidget *button,
                                           const gchar *tooltip)
{
  g_return_if_fail (button != NULL);

  gtk_widget_set_tooltip_text (button, tooltip);
}


/*! \brief Create a new toolbar button.
 *  \par Function Description
 *
 * Returns a newly created toolbar button widget.
 *
 * \return The created button widget.
 */
GtkWidget*
schematic_toolbar_button_new ()
{
  return GTK_WIDGET (gtk_tool_button_new (NULL, NULL));
}


GtkWidget*
schematic_toolbar_radio_button_new ()
{
  return GTK_WIDGET (gtk_radio_tool_button_new (NULL));
}


GSList*
schematic_toolbar_radio_button_get_group (GtkWidget *button)
{
  return gtk_radio_tool_button_get_group (GTK_RADIO_TOOL_BUTTON (button));
}


void
schematic_toolbar_radio_button_set_group (GtkWidget *button,
                                          GSList *group)
{
  return gtk_radio_tool_button_set_group (GTK_RADIO_TOOL_BUTTON (button), group);
}


void
schematic_toolbar_insert_button (GtkWidget *toolbar,
                                 GtkToolButton *button,
                                 gint pos)
{
  gtk_toolbar_insert (GTK_TOOLBAR (toolbar), GTK_TOOL_ITEM (button), pos);
}


void
schematic_toolbar_insert_separator (GtkWidget *toolbar,
                                    gint pos)
{
  gtk_toolbar_insert (GTK_TOOLBAR (toolbar),
                      GTK_TOOL_ITEM (gtk_separator_tool_item_new ()),
                      pos);
}


GtkWidget*
schematic_toolbar_new (GschemToplevel *w_current,
                       GtkWidget *main_box)
{
  if (w_current->toolbars == 0)
  {
    return NULL;
  }

  GtkWidget *toolbar = gtk_toolbar_new ();

  gtk_orientable_set_orientation (GTK_ORIENTABLE (toolbar),
                                  GTK_ORIENTATION_HORIZONTAL);
  gtk_toolbar_set_style (GTK_TOOLBAR (toolbar), GTK_TOOLBAR_ICONS);

#ifdef ENABLE_GTK3
  gtk_box_pack_start (GTK_BOX (main_box), toolbar, FALSE, FALSE, 0);
#else
  if (w_current->handleboxes)
  {
    GtkWidget *handlebox = gtk_handle_box_new ();
    gtk_box_pack_start (GTK_BOX (main_box), handlebox, FALSE, FALSE, 0);
    gtk_container_add (GTK_CONTAINER (handlebox), toolbar);
  }
  else
  {
    gtk_box_pack_start (GTK_BOX (main_box), toolbar, FALSE, FALSE, 0);
  }
#endif
  return toolbar;
}


void
schematic_window_set_toolbar_net (GschemToplevel *w_current,
                                  GtkWidget *button)
{
  w_current->toolbar_net = button;
}


void
schematic_window_set_toolbar_bus (GschemToplevel *w_current,
                                  GtkWidget *button)
{
  w_current->toolbar_bus = button;
}


void
schematic_window_set_toolbar_select (GschemToplevel *w_current,
                                     GtkWidget *button)
{
  w_current->toolbar_select = button;
}


void
schematic_toolbar_activate_button (GtkWidget *button)
{
  gtk_toggle_tool_button_set_active (GTK_TOGGLE_TOOL_BUTTON (button), TRUE);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \param [in] w_current GschemToplevel structure
 *
 */
void
schematic_toolbar_update (GschemToplevel *w_current)
{
  switch (schematic_window_get_action_mode (w_current))
  {
    case(SELECT):
      gtk_toggle_tool_button_set_active (GTK_TOGGLE_TOOL_BUTTON (w_current->toolbar_select),
                                         TRUE);
      break;

    case(NETMODE):
      gtk_toggle_tool_button_set_active (GTK_TOGGLE_TOOL_BUTTON (w_current->toolbar_net),
                                         TRUE);
      break;

    case(BUSMODE):
      gtk_toggle_tool_button_set_active (GTK_TOGGLE_TOOL_BUTTON (w_current->toolbar_bus),
                                         TRUE);
      break;

    case(ARCMODE): /*! \todo */
    case(BOXMODE): /*! \todo */
    case(CIRCLEMODE): /*! \todo */
    case(LINEMODE): /*! \todo */
    case(PICTUREMODE): /*! \todo */
    case(PINMODE): /*! \todo */
    case(PAN): /*! \todo */
    case(COPYMODE): /*! \todo */
    case(MCOPYMODE): /*! \todo */
    case(MOVEMODE): /*! \todo */
    case(COMPMODE): /*! \todo */
    case(ROTATEMODE): /*! \todo */
    case(TEXTMODE): /*! \todo */
    case(MIRRORMODE): /*! \todo */
    case(ZOOMBOX): /*! \todo */
    case(PASTEMODE): /*! \todo */
    case(GRIPS): /*! \todo */
    default:
      gtk_toggle_tool_button_set_active (GTK_TOGGLE_TOOL_BUTTON (w_current->toolbar_select),
                                         TRUE);
      break;
  }
}
