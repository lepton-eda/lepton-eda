#include <liblepton.h>
#include <math.h>

void
check_rotate ()
{
  gint count;

  for (count = 0; count < 10000; count++) {
    gint angle = g_test_rand_int ();
    gint x0 = g_test_rand_int_range (-80000, 80001);
    gint y0 = g_test_rand_int_range (-80000, 80001);
    gint x1;
    gint y1;

    lepton_point_rotate (x0, y0, angle, &x1, &y1);

    /* distance to origin (center of rotation) should be the same */
    gdouble d0 = hypot (x0, y0);
    gdouble d1 = hypot (x1, y1);
    gdouble derr = fabs (d1 - d0);
    g_assert_cmpfloat (derr, <=, 2.0);

    /* rotating a point at the origin results in the same point */
    if ((x0 == 0) && (y0 == 0)) {
      g_assert_cmpint (x1, ==, 0);
      g_assert_cmpint (y1, ==, 0);
    }

    /* check for angle error */
    /* ignore close to origin becaue of rounding error */
    if (d0 > 100.0) {
      gdouble a0 = atan2 (y0, x0);
      gdouble a1 = atan2 (y1, x1);
      /* gint normalized = geda_angle_normalize (angle); */

      gdouble dd = 180.0 * (a1 - a0) / G_PI;
      while (dd < 0) {
        dd += 360.0;
      }

      //g_assert_cmpfloat (fabs (normalized - dd), <=, 2.0);
    }
  }
}

void
check_rotate_90 ()
{
  gint count;

  for (count = 0; count < 10000; count++) {
    gint angle = geda_angle_normalize (geda_angle_make_ortho (g_test_rand_int ()));
    gint x0 = g_test_rand_int_range (-80000, 80001);
    gint y0 = g_test_rand_int_range (-80000, 80001);
    gint x1;
    gint y1;

    lepton_point_rotate_90 (x0, y0, angle, &x1, &y1);

    /* distance to origin (center of rotation) should be the same */
    gdouble d0 = hypot (x0, y0);
    gdouble d1 = hypot (x1, y1);
    gdouble derr = fabs (d1 - d0);
    g_assert_cmpfloat (derr, <=, 2.0);

    /* rotating a point at the origin results in the same point */
    if ((x0 == 0) && (y0 == 0)) {
      g_assert_cmpint (x1, ==, 0);
      g_assert_cmpint (y1, ==, 0);
      continue;
    }

    /* check for angle error */
    /* ignore close to origin becaue of rounding error */
    if (d0 > 100.0) {
      gdouble a0 = atan2 (y0, x0);
      gdouble a1 = atan2 (y1, x1);
      /* gint normalized = geda_angle_normalize (angle); */

      gdouble dd = 180.0 * (a1 - a0) / G_PI;
      while (dd < 0) {
        dd += 360.0;
      }

      // fixme : 359 and 1 are only 2 degrees apart
      // g_assert_cmpfloat (fabs (normalized - dd), <=, 2.0);
    }
  }
}

int
main (int argc, char *argv[])
{
    g_test_init (&argc, &argv, NULL);

    g_test_add_func ("/geda/liblepton/point/rotate",
                     check_rotate);

    g_test_add_func ("/geda/liblepton/point/rotate_90",
                     check_rotate_90);

    return g_test_run ();
}
