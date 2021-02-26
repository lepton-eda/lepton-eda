#include <glib.h>
#include <liblepton.h>

void
check_equal ()
{
  gint count;

  for (count = 0; count < 100; count++) {
    GedaBounds a;
    GedaBounds b;
    gint x0 = g_test_rand_int ();
    gint y0 = g_test_rand_int ();
    gint x1 = g_test_rand_int ();
    gint y1 = g_test_rand_int ();

    lepton_bounds_init_with_points (&a, x0, y0, x1, y1);
    lepton_bounds_init_with_points (&b, x0, y0, x1, y1);
    g_assert_true (lepton_bounds_equal (&a, &b));

    lepton_bounds_init_with_points (&b, ~x0, y0, x1, y1);
    g_assert_false (lepton_bounds_equal (&a, &b));

    lepton_bounds_init_with_points (&b, x0, ~y0, x1, y1);
    g_assert_false (lepton_bounds_equal (&a, &b));

    lepton_bounds_init_with_points (&b, x0, y0, ~x1, y1);
    g_assert_false (lepton_bounds_equal (&a, &b));

    lepton_bounds_init_with_points (&b, x0, y0, x1, ~y1);
    g_assert_false (lepton_bounds_equal (&a, &b));
  }
}

void
check_expand ()
{
  gint count;
  GedaBounds empty;
  lepton_bounds_init (&empty);

  for (count = 0; count < 100000; count++) {
    GedaBounds bounds;
    GedaBounds result;
    gint x = g_test_rand_int_range (-80000, 80001);
    gint y = g_test_rand_int_range (-80000, 80001);
    gint x0 = g_test_rand_int_range (-80000, 80001);
    gint y0 = g_test_rand_int_range (-80000, 80001);
    gint x1 = g_test_rand_int_range (-80000, 80001);
    gint y1 = g_test_rand_int_range (-80000, 80001);

    /* ===== empty bounds ===== */
    /* any operation on an empty bounds yields an empty bounds */
    lepton_bounds_expand (&result, &empty, x, y);
    g_assert_true (lepton_bounds_empty (&result));

    lepton_bounds_expand (&result, NULL, x, y);
    g_assert_true (lepton_bounds_empty (&result));

    /* ===== bounds of the largest box ===== */
    lepton_bounds_init_with_points (&bounds, G_MININT, G_MININT, G_MAXINT, G_MAXINT);
    g_assert_false (lepton_bounds_empty (&bounds));

    /* expanding by zero results in a copy */
    lepton_bounds_expand (&result, &bounds, 0, 0);
    g_assert_cmpint (result.min_x, ==, bounds.min_x);
    g_assert_cmpint (result.min_y, ==, bounds.min_y);
    g_assert_cmpint (result.max_x, ==, bounds.max_x);
    g_assert_cmpint (result.max_y, ==, bounds.max_y);

    /* shrink the bounds to empty */
    lepton_bounds_expand (&result, &bounds, G_MININT, y);
    g_assert_true (lepton_bounds_empty (&result));

    lepton_bounds_expand (&result, &bounds, x, G_MININT);
    g_assert_true (lepton_bounds_empty (&result));

    /* ===== bounds of a random box ===== */
    lepton_bounds_init_with_points (&bounds, x0, y0, x1, y1);
    g_assert_false (lepton_bounds_empty (&bounds));

    /* expanding by zero results in a copy */
    lepton_bounds_expand (&result, &bounds, 0, 0);
    g_assert_cmpint (result.min_x, ==, bounds.min_x);
    g_assert_cmpint (result.min_y, ==, bounds.min_y);
    g_assert_cmpint (result.max_x, ==, bounds.max_x);
    g_assert_cmpint (result.max_y, ==, bounds.max_y);

    /* expand the bounds and check the width */
    lepton_bounds_expand (&result, &bounds, abs(x), abs(y));
    g_assert_false (lepton_bounds_empty (&bounds));
    g_assert_cmpint ((result.max_x - result.min_x), ==, (bounds.max_x - bounds.min_x + 2*abs(x)));
    g_assert_cmpint ((result.max_y - result.min_y), ==, (bounds.max_y - bounds.min_y + 2*abs(y)));

    /* expand/shrink the bounds and check the width */
    lepton_bounds_expand (&result, &bounds, x, y);
    if (!lepton_bounds_empty (&result)) {
      g_assert_cmpint ((result.max_x - result.min_x), ==, (bounds.max_x - bounds.min_x + 2*x));
      g_assert_cmpint ((result.max_y - result.min_y), ==, (bounds.max_y - bounds.min_y + 2*y));
    }

    /* shrink the bounds to empty */
    lepton_bounds_expand (&result, &bounds, -200000, y);
    g_assert_true (lepton_bounds_empty (&result));

    lepton_bounds_expand (&result, &bounds, x, -200000);
    g_assert_true (lepton_bounds_empty (&result));

    /* ===== bounds of a random single point ===== */
    lepton_bounds_init_with_points (&bounds, x0, y0, x0, y0);
    g_assert_false (lepton_bounds_empty (&bounds));

    /* expanding by zero results in a copy */
    lepton_bounds_expand (&result, &bounds, 0, 0);
    g_assert_cmpint (result.min_x, ==, bounds.min_x);
    g_assert_cmpint (result.min_y, ==, bounds.min_y);
    g_assert_cmpint (result.max_x, ==, bounds.max_x);
    g_assert_cmpint (result.max_y, ==, bounds.max_y);

    /* expand the bounds and check the width */
    lepton_bounds_expand (&result, &bounds, abs(x), abs(y));
    g_assert_false (lepton_bounds_empty (&bounds));
    g_assert_cmpint ((result.max_x - result.min_x), ==, (bounds.max_x - bounds.min_x + 2*abs(x)));
    g_assert_cmpint ((result.max_y - result.min_y), ==, (bounds.max_y - bounds.min_y + 2*abs(y)));

    /* expand/shrink the bounds and check the width */
    lepton_bounds_expand (&result, &bounds, x, y);
    if (!lepton_bounds_empty (&result)) {
      g_assert_cmpint ((result.max_x - result.min_x), ==, (bounds.max_x - bounds.min_x + 2*x));
      g_assert_cmpint ((result.max_y - result.min_y), ==, (bounds.max_y - bounds.min_y + 2*y));
    }

    /* shrink the bounds to empty */
    lepton_bounds_expand (&result, &bounds, -1, y);
    g_assert_true (lepton_bounds_empty (&result));

    lepton_bounds_expand (&result, &bounds, x, -1);
    g_assert_true (lepton_bounds_empty (&result));
  }
}

void
check_init ()
{
  gint count;
  GedaBounds bounds;

  lepton_bounds_init (&bounds);
  g_assert_true (lepton_bounds_empty (&bounds));

  for (count = 0; count < 100000; count++) {
    GedaBounds result;
    gint x0 = g_test_rand_int ();
    gint y0 = g_test_rand_int ();
    gint x1 = g_test_rand_int ();
    gint y1 = g_test_rand_int ();

    lepton_bounds_init_with_points (&bounds, x0, y0, x1, y1);
    g_assert_false (lepton_bounds_empty (&bounds));
    g_assert_true (lepton_bounds_interior_point (&bounds, x0, y0));
    g_assert_true (lepton_bounds_interior_point (&bounds, x1, y1));

    lepton_bounds_init_with_points (&result, x1, y1, x0, y0);
    g_assert_true (lepton_bounds_equal (&result, &bounds));

    lepton_bounds_expand (&result, &bounds, 0, -1);
    g_assert_false (lepton_bounds_interior_point (&result, x0, y0));
    g_assert_false (lepton_bounds_interior_point (&result, x1, y1));

    lepton_bounds_expand (&result, &bounds, -1, 0);
    g_assert_false (lepton_bounds_interior_point (&result, x0, y0));
    g_assert_false (lepton_bounds_interior_point (&result, x1, y1));
  }
}


void
check_interior_point ()
{
  static const GedaPoint dirs[] =
  {
      { -1, -1 },
      { -1,  0 },
      { -1,  1 },
      {  0, -1 },
      {  0,  1 },
      {  1, -1 },
      {  1,  0 },
      {  1,  1 }
  };

  static const GedaPoint points[] =
  {
      {  8000,  1850 },
      { -1111,   720 },
      {   800, -1024 },
      {  -256, -2001 }
  };

  int dcount = sizeof (dirs) / sizeof (GedaPoint);
  int pcount = sizeof (points) / sizeof (GedaPoint);
  int pindex;

  for (pindex = 0; pindex < pcount; pindex++) {
    GedaBounds bounds;
    gint dindex;
    gboolean interior;

    lepton_bounds_init (&bounds);
    lepton_bounds_of_points (&bounds, &points[pindex], 1);

    interior = lepton_bounds_interior_point (&bounds, points[pindex].x, points[pindex].y);
    g_assert_true (interior);

    interior = inside_region (bounds.min_x, bounds.min_y, bounds.max_x, bounds.max_y, points[pindex].x, points[pindex].y);
    g_assert_true (interior);

    for (dindex = 0; dindex < dcount; dindex++) {
      GedaPoint point = points[pindex];

      point.x += dirs[dindex].x;
      point.y += dirs[dindex].y;

      interior = lepton_bounds_interior_point (&bounds, point.x, point.y);
      g_assert_false (interior);

      interior = inside_region (bounds.min_x, bounds.min_y, bounds.max_x, bounds.max_y, point.x, point.y);
      g_assert_false (interior);
    }
  }
}

void
check_union ()
{
  gint count;

  for (count = 0; count < 100000; count++) {
    GedaBounds bounds[2];
    gint index;
    GedaPoint points[4];
    GedaBounds result;

    for (index = 0; index < 4; index++) {
      points[index].x = g_test_rand_int ();
      points[index].y = g_test_rand_int ();
    }

    for (index = 0; index < 2; index++) {
      lepton_bounds_init (&bounds[index]);
      lepton_bounds_of_points (&bounds[index], &points[2*index], 2);
    }

    lepton_bounds_union (&result, &bounds[0], &bounds[1]);
    for (index = 0; index < 4; index++) {
      g_assert_true (lepton_bounds_interior_point (&result, points[index].x, points[index].y));
    }

    lepton_bounds_union (&result, &bounds[1], &bounds[0]);
    for (index = 0; index < 4; index++) {
      g_assert_true (lepton_bounds_interior_point (&result, points[index].x, points[index].y));
    }

    lepton_bounds_init (&bounds[1]);

    lepton_bounds_union (&result, &bounds[0], &bounds[1]);
    g_assert_cmpint (result.min_x, ==, bounds[0].min_x);
    g_assert_cmpint (result.min_y, ==, bounds[0].min_y);
    g_assert_cmpint (result.max_x, ==, bounds[0].max_x);
    g_assert_cmpint (result.max_y, ==, bounds[0].max_y);

    lepton_bounds_union (&result, &bounds[1], &bounds[0]);
    g_assert_cmpint (result.min_x, ==, bounds[0].min_x);
    g_assert_cmpint (result.min_y, ==, bounds[0].min_y);
    g_assert_cmpint (result.max_x, ==, bounds[0].max_x);
    g_assert_cmpint (result.max_y, ==, bounds[0].max_y);

    lepton_bounds_union (&result, &bounds[0], NULL);
    g_assert_cmpint (result.min_x, ==, bounds[0].min_x);
    g_assert_cmpint (result.min_y, ==, bounds[0].min_y);
    g_assert_cmpint (result.max_x, ==, bounds[0].max_x);
    g_assert_cmpint (result.max_y, ==, bounds[0].max_y);

    lepton_bounds_union (&result, NULL, &bounds[0]);
    g_assert_cmpint (result.min_x, ==, bounds[0].min_x);
    g_assert_cmpint (result.min_y, ==, bounds[0].min_y);
    g_assert_cmpint (result.max_x, ==, bounds[0].max_x);
    g_assert_cmpint (result.max_y, ==, bounds[0].max_y);

    lepton_bounds_init (&bounds[0]);

    lepton_bounds_union (&result, &bounds[0], &bounds[1]);
    g_assert_true (lepton_bounds_empty (&result));

    lepton_bounds_union (&result, NULL, NULL);
    g_assert_true (lepton_bounds_empty (&result));
  }
}


int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/geda/liblepton/bounds/equal",
                   check_equal);

  g_test_add_func ("/geda/liblepton/bounds/expand",
                     check_expand);

  g_test_add_func ("/geda/liblepton/bounds/init",
                   check_init);

  g_test_add_func ("/geda/liblepton/bounds/interior_point",
                   check_interior_point);

  g_test_add_func ("/geda/liblepton/bounds/union",
                     check_union);

  return g_test_run ();
}
