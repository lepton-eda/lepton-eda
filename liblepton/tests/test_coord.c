#include <glib.h>
#include <liblepton.h>
#include "stdio.h"

void
check_snap ()
{
  gint count;
  gint snapped;

  snapped = lepton_coord_snap (G_MAXINT, 1);
  g_assert_cmpint (snapped, ==, G_MAXINT);

  snapped = lepton_coord_snap (G_MAXINT, G_MAXINT);
  g_assert_cmpint (snapped, ==, G_MAXINT);

  snapped = lepton_coord_snap (G_MININT, 1);
  g_assert_cmpint (snapped, ==, G_MININT);

  snapped = lepton_coord_snap (-G_MAXINT, 1);
  g_assert_cmpint (snapped, ==, -G_MAXINT);

  snapped = lepton_coord_snap (-G_MAXINT, G_MAXINT);
  g_assert_cmpint (snapped, ==, -G_MAXINT);

  for (count = 0; count < 10000; count++) {
    // gint coord = g_test_rand_int ();                  //bad
    // gint grid = g_test_rand_int_range (2, G_MAXINT);  evil
    gint coord = g_test_rand_int_range (G_MININT / 2, G_MAXINT / 2);
    gint grid = g_test_rand_int_range (2, 10000);
    gint temp;
    gint resnapped;
    div_t result;
    gint shift;

    /* grid of 1 should result in the same coordinate */
    snapped = lepton_coord_snap (coord, 1);
    g_assert_cmpint (snapped, ==, coord);

    /* check grids with even powers of two using logical operations */
    for (shift = 2; shift > 0; shift <<= 1) {
      gint mask = shift - 1;
      snapped = lepton_coord_snap (coord, shift);
      g_assert_cmpint ((snapped & mask), ==, 0x00);
    }

    /* check if on an even grid */
    snapped = lepton_coord_snap (coord, grid);
    result = div (snapped, grid);
    g_assert_cmpint (result.rem, ==, 0);

    /* resnapping a snapped value results in no change */
    resnapped = lepton_coord_snap (snapped, grid);
    g_assert_cmpint (resnapped, ==, snapped);

    if (coord > 1) {
      snapped = lepton_coord_snap (coord, coord);
      g_assert_cmpint (snapped , ==, coord);
    }

    /* grid should always snap to the grid */
    snapped = lepton_coord_snap (grid, grid);
    g_assert_cmpint (snapped , ==, grid);

    /* snap back when off by one */
    if (grid > 2) {
      if (snapped < G_MAXINT) {
        resnapped = lepton_coord_snap (snapped + 1, grid);
        g_assert_cmpint (resnapped, ==, snapped);
      }
      if (snapped > G_MININT) {
        resnapped = lepton_coord_snap (snapped - 1, grid);
        g_assert_cmpint (resnapped, ==, snapped);
      }
    }

    /* snap back from a more positive value */
    temp = snapped + ((grid - 1) / 2);
    resnapped = lepton_coord_snap (temp, grid);
    g_assert_cmpint (resnapped, ==, snapped);

    /* snap back from a more negative value */
    temp = snapped - ((grid - 1) / 2);
    resnapped = lepton_coord_snap (temp, grid);
    g_assert_cmpint (resnapped, ==, snapped);

    /* snap away in the positive direction */
    if ((G_MAXINT - grid) <= snapped ) {
      temp = snapped + (grid / 2) + 1;
      resnapped = lepton_coord_snap (temp, grid);
      g_assert_cmpint (resnapped, >, snapped);
    }

    /* snap away in the negative direction */
    if ((G_MININT + grid) >= snapped ) {
      temp = snapped - (grid / 2) - 1;
      resnapped = lepton_coord_snap (temp, grid);
      g_assert_cmpint (resnapped, <, snapped);
    }
  }
}

int
main (int argc, char *argv[])
{
    g_test_init (&argc, &argv, NULL);

    g_test_add_func ("/geda/liblepton/coord/snap",
                     check_snap);

    return g_test_run ();
}
