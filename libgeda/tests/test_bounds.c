#include <glib.h>
#include <libgeda.h>

void
check_init ()
{
  GedaBounds bounds;
  gboolean empty;

  geda_bounds_init (&bounds);

  empty = geda_bounds_empty (&bounds);
  g_assert_true (empty);
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

    geda_bounds_init (&bounds);
    geda_bounds_of_points (&bounds, &points[pindex], 1);

    interior = geda_bounds_interior_point (&bounds, points[pindex].x, points[pindex].y);
    g_assert_true (interior);

    interior = inside_region (bounds.min_x, bounds.min_y, bounds.max_x, bounds.max_y, points[pindex].x, points[pindex].y);
    g_assert_true (interior);

    for (dindex = 0; dindex < dcount; dindex++) {
      GedaPoint point = points[pindex];

      point.x += dirs[dindex].x;
      point.y += dirs[dindex].y;

      interior = geda_bounds_interior_point (&bounds, point.x, point.y);
      g_assert_false (interior);

      interior = inside_region (bounds.min_x, bounds.min_y, bounds.max_x, bounds.max_y, point.x, point.y);
      g_assert_false (interior);
    }
  }
}


int
main (int argc, char *argv[])
{
    g_test_init (&argc, &argv, NULL);

    g_test_add_func ("/geda/libgeda/bounds/init",
                     check_init);

    g_test_add_func ("/geda/libgeda/bounds/interior_point",
                     check_interior_point);

    return g_test_run ();
}
