#include <glib.h>
#include <liblepton.h>

void
check_is_normal ()
{
  gint count;

  for (count = 0; count < 10000; count++) {
    gint angle = g_test_rand_int_range (G_MININT, 0);
    g_assert_false (lepton_angle_is_normal (angle));
  }

  for (count = 0; count < 10000; count++) {
    gint angle = g_test_rand_int_range (360, G_MAXINT);
    g_assert_false (lepton_angle_is_normal (angle));
  }

  for (count = 0; count < 100; count++) {
    gint angle = g_test_rand_int_range (0, 360);
    g_assert_true (lepton_angle_is_normal (angle));
  }
}

void
check_is_ortho ()
{
  gint count;

  for (count = 0; count < 10000; count++) {
    gint angle = 90 * (g_test_rand_int () / 100);

    g_assert_true (lepton_angle_is_ortho (angle));
  }

  for (count = 0; count < 10000; count++) {
    gint angle = 90 * (g_test_rand_int () / 100) + g_test_rand_int_range (1, 90);

    g_assert_false (lepton_angle_is_ortho (angle));
  }
}

void
check_make_ortho ()
{
  gint count;

  for (count = 0; count < 100000; count++) {
    gint angle = g_test_rand_int ();

    gint ortho = lepton_angle_make_ortho (angle);

    gint delta = abs (ortho - angle);
    g_assert_cmpint (delta, <=, 45);

    g_assert_true (lepton_angle_is_ortho (ortho));
  }
}

void
check_normalize ()
{
  gint count;

  for (count = 0; count < 100000; count++) {
    gint angle = g_test_rand_int_range (0, 360);
    gint multiplier = g_test_rand_int_range (-10000, 10001);
    gint not_normalized = angle + 360 * multiplier;

    gint normalized = lepton_angle_normalize (not_normalized);

    g_assert_cmpint (normalized, >=, 0);
    g_assert_cmpint (normalized, <, 360);
    g_assert_cmpint (normalized, ==, angle);
  }
}

int
main (int argc, char *argv[])
{
    g_test_init (&argc, &argv, NULL);

    g_test_add_func ("/geda/liblepton/angle/isnormal",
                     check_is_normal);

    g_test_add_func ("/geda/liblepton/angle/isortho",
                     check_is_ortho);

    g_test_add_func ("/geda/liblepton/angle/makeortho",
                     check_make_ortho);

    g_test_add_func ("/geda/liblepton/angle/normalize",
                     check_normalize);

    return g_test_run ();
}
