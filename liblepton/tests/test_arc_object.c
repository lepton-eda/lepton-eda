#include <liblepton.h>
#include <version.h>

void
check_construction ()
{
  gint count;
  LeptonToplevel *toplevel = lepton_toplevel_new ();

  for (count = 0; count < 1000; count++) {
    gint center_x = g_test_rand_int ();
    gint center_y = g_test_rand_int ();
    gint color = g_test_rand_int_range (0, colors_count());
    gint radius = g_test_rand_int_range (0, G_MAXINT);
    gint start_angle = g_test_rand_int_range (0, 359);
    gint sweep_angle = g_test_rand_int_range (0, 360);

    LeptonObject *object0 = lepton_arc_object_new (color,
                                                   center_x,
                                                   center_y,
                                                   radius,
                                                   start_angle,
                                                   sweep_angle);

    g_assert (object0 != NULL);
    g_assert_cmpint (OBJ_ARC, ==, lepton_object_get_type (object0));

    g_assert_cmpint (center_x, ==, lepton_arc_object_get_center_x (object0));
    g_assert_cmpint (center_y, ==, lepton_arc_object_get_center_y (object0));
    g_assert_cmpint (color, ==, lepton_object_get_color (object0));
    g_assert_cmpint (radius, ==, lepton_arc_object_get_radius (object0));
    g_assert_cmpint (start_angle, ==, lepton_arc_object_get_start_angle (object0));
    g_assert_cmpint (sweep_angle, ==, lepton_arc_object_get_sweep_angle (object0));

    LeptonObject *object1 = lepton_arc_object_copy (object0);

    g_assert (object1 != NULL);
    g_assert (object1 != object0);
    g_assert_cmpint (OBJ_ARC, ==, lepton_object_get_type (object1));

    lepton_object_delete (object0);

    g_assert_cmpint (center_x, ==, lepton_arc_object_get_center_x (object1));
    g_assert_cmpint (center_y, ==, lepton_arc_object_get_center_y (object1));
    g_assert_cmpint (color, ==, lepton_object_get_color (object1));
    g_assert_cmpint (radius, ==, lepton_arc_object_get_radius (object1));
    g_assert_cmpint (start_angle, ==, lepton_arc_object_get_start_angle (object1));
    g_assert_cmpint (sweep_angle, ==, lepton_arc_object_get_sweep_angle (object1));

    lepton_object_delete (object1);
  }

  lepton_toplevel_delete (toplevel);
}

void
check_accessors ()
{
  gint count;
  LeptonToplevel *toplevel = lepton_toplevel_new ();

  for (count = 0; count < 1000; count++) {
    gint center_x = g_test_rand_int ();
    gint center_y = g_test_rand_int ();
    gint color = g_test_rand_int_range (0, colors_count());
    gint radius = g_test_rand_int_range (0, G_MAXINT);
    gint start_angle = g_test_rand_int_range (0, 359);
    gint sweep_angle = g_test_rand_int_range (0, 360);

    LeptonObject *object0 = lepton_arc_object_new (color,
                                               center_x,
                                               center_y,
                                               radius,
                                               start_angle,
                                               sweep_angle);

    g_assert (object0 != NULL);
    g_assert_cmpint (OBJ_ARC, ==, lepton_object_get_type (object0));

    center_x = g_test_rand_int ();
    center_y = g_test_rand_int ();
    color = g_test_rand_int_range (0, colors_count());
    radius = g_test_rand_int_range (0, G_MAXINT);
    start_angle = g_test_rand_int_range (0, 359);
    sweep_angle = g_test_rand_int_range (0, 360);

    lepton_arc_object_set_center_x (object0, center_x);
    lepton_arc_object_set_center_y (object0, center_y);
    lepton_object_set_color (object0, color);
    lepton_arc_object_set_radius (object0, radius);
    lepton_arc_object_set_start_angle (object0, start_angle);
    lepton_arc_object_set_sweep_angle (object0, sweep_angle);

    g_assert_cmpint (center_x, ==, lepton_arc_object_get_center_x (object0));
    g_assert_cmpint (center_y, ==, lepton_arc_object_get_center_y (object0));
    g_assert_cmpint (color, ==, lepton_object_get_color (object0));
    g_assert_cmpint (radius, ==, lepton_arc_object_get_radius (object0));
    g_assert_cmpint (start_angle, ==, lepton_arc_object_get_start_angle (object0));
    g_assert_cmpint (sweep_angle, ==, lepton_arc_object_get_sweep_angle (object0));

    lepton_object_delete (object0);
  }

  lepton_toplevel_delete (toplevel);
}

void
check_serialization ()
{
  gint count;
  gint converted;
  LeptonToplevel *toplevel = lepton_toplevel_new ();
  guint version;

  converted = sscanf (PACKAGE_DATE_VERSION, "%u", &version);
  g_assert_cmpuint (converted, ==, 1);

  for (count = 0; count < 1000; count++) {
    gint center_x = g_test_rand_int ();
    gint center_y = g_test_rand_int ();
    gint color = g_test_rand_int_range (0, colors_count());
    gint radius = g_test_rand_int_range (0, G_MAXINT);
    gint start_angle = g_test_rand_int_range (0, 359);
    gint sweep_angle = g_test_rand_int_range (0, 360);

    LeptonObject *object0 = lepton_arc_object_new (color,
                                               center_x,
                                               center_y,
                                               radius,
                                               start_angle,
                                               sweep_angle);

    g_assert (object0 != NULL);

    gchar *buffer0 = lepton_arc_object_to_buffer (object0);
    lepton_object_delete (object0);
    g_assert (buffer0 != NULL);

    LeptonObject *object1 = lepton_arc_object_read (buffer0,
                                                    version,
                                                    FILEFORMAT_VERSION,
                                                    NULL);

    g_assert (object1 != NULL);

    g_assert_cmpint (center_x, ==, lepton_arc_object_get_center_x (object1));
    g_assert_cmpint (center_y, ==, lepton_arc_object_get_center_y (object1));
    g_assert_cmpint (color, ==, lepton_object_get_color (object1));
    g_assert_cmpint (radius, ==, lepton_arc_object_get_radius (object1));
    g_assert_cmpint (start_angle, ==, lepton_arc_object_get_start_angle (object1));
    g_assert_cmpint (sweep_angle, ==, lepton_arc_object_get_sweep_angle (object1));

    gchar *buffer1 = lepton_arc_object_to_buffer (object1);
    lepton_object_delete (object1);
    g_assert (buffer1 != NULL);

    g_assert_cmpstr (buffer0, ==, buffer1);
    g_free (buffer0);
    g_free (buffer1);
  }

  lepton_toplevel_delete (toplevel);
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/geda/liblepton/arc_object/construction",
                   check_construction);

  g_test_add_func ("/geda/liblepton/arc_object/check_accessors",
                   check_accessors);

  g_test_add_func ("/geda/liblepton/arc_object/serialization",
                   check_serialization);

  return g_test_run ();
}
