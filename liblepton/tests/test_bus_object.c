#include <liblepton.h>
#include <version.h>

void
check_construction ()
{
  gint count;
  LeptonToplevel *toplevel = lepton_toplevel_new ();

  for (count = 0; count < 1000; count++) {
    gint x0 = g_test_rand_int ();
    gint y0 = g_test_rand_int ();
    gint x1 = g_test_rand_int ();
    gint y1 = g_test_rand_int ();
    gint color = g_test_rand_int_range (0, colors_count());
    gint ripper = g_test_rand_int_range (-1, 2);

    LeptonObject *object0 = lepton_bus_object_new (color,
                                                   x0,
                                                   y0,
                                                   x1,
                                                   y1,
                                                   ripper);

    g_assert (object0 != NULL);
    g_assert_cmpint (OBJ_BUS, ==, lepton_object_get_type (object0));

    g_assert_cmpint (x0, ==, lepton_bus_object_get_x0 (object0));
    g_assert_cmpint (y0, ==, lepton_bus_object_get_y0 (object0));
    g_assert_cmpint (x1, ==, lepton_bus_object_get_x1 (object0));
    g_assert_cmpint (y1, ==, lepton_bus_object_get_y1 (object0));
    g_assert_cmpint (color, ==, lepton_object_get_color (object0));
    g_assert_cmpint (ripper, ==, lepton_bus_object_get_ripper_direction (object0));

    LeptonObject *object1 = lepton_bus_object_copy (object0);

    g_assert (object1 != NULL);
    g_assert (object1 != object0);
    g_assert_cmpint (OBJ_BUS, ==, lepton_object_get_type (object1));

    lepton_object_delete (object0);

    g_assert_cmpint (x0, ==, lepton_bus_object_get_x0 (object1));
    g_assert_cmpint (y0, ==, lepton_bus_object_get_y0 (object1));
    g_assert_cmpint (x1, ==, lepton_bus_object_get_x1 (object1));
    g_assert_cmpint (y1, ==, lepton_bus_object_get_y1 (object1));
    g_assert_cmpint (color, ==, lepton_object_get_color (object1));
    g_assert_cmpint (ripper, ==, lepton_bus_object_get_ripper_direction (object1));

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
    gint x0 = g_test_rand_int ();
    gint y0 = g_test_rand_int ();
    gint x1 = g_test_rand_int ();
    gint y1 = g_test_rand_int ();
    gint color = g_test_rand_int_range (0, colors_count());
    gint ripper = g_test_rand_int_range (-1, 2);

    LeptonObject *object0 = lepton_bus_object_new (color,
                                                   x0,
                                                   y0,
                                                   x1,
                                                   y1,
                                                   ripper);

    g_assert (object0 != NULL);
    g_assert_cmpint (OBJ_BUS, ==, lepton_object_get_type (object0));

    x0 = g_test_rand_int ();
    y0 = g_test_rand_int ();
    x1 = g_test_rand_int ();
    y1 = g_test_rand_int ();
    color = g_test_rand_int_range (0, colors_count());
    ripper = g_test_rand_int_range (-1, 2);

    lepton_bus_object_set_x0 (object0, x0);
    lepton_bus_object_set_y0 (object0, y0);
    lepton_bus_object_set_x1 (object0, x1);
    lepton_bus_object_set_y1 (object0, y1);
    lepton_object_set_color (object0, color);
    lepton_bus_object_set_ripper_direction (object0, ripper);

    g_assert_cmpint (x0, ==, lepton_bus_object_get_x0 (object0));
    g_assert_cmpint (y0, ==, lepton_bus_object_get_y0 (object0));
    g_assert_cmpint (x1, ==, lepton_bus_object_get_x1 (object0));
    g_assert_cmpint (y1, ==, lepton_bus_object_get_y1 (object0));
    g_assert_cmpint (color, ==, lepton_object_get_color (object0));
    g_assert_cmpint (ripper, ==, lepton_bus_object_get_ripper_direction (object0));

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
    gint x0 = g_test_rand_int ();
    gint y0 = g_test_rand_int ();
    gint x1 = g_test_rand_int ();
    gint y1 = g_test_rand_int ();
    gint color = g_test_rand_int_range (0, colors_count());
    gint ripper = g_test_rand_int_range (-1, 2);

    LeptonObject *object0 = lepton_bus_object_new (color,
                                                   x0,
                                                   y0,
                                                   x1,
                                                   y1,
                                                   ripper);

    g_assert (object0 != NULL);

    gchar *buffer0 = lepton_bus_object_to_buffer (object0);
    lepton_object_delete (object0);
    g_assert (buffer0 != NULL);

    LeptonObject *object1 = o_bus_read (buffer0,
                                      version,
                                      FILEFORMAT_VERSION,
                                      NULL);

    g_assert (object1 != NULL);

    g_assert_cmpint (x0, ==, lepton_bus_object_get_x0 (object1));
    g_assert_cmpint (y0, ==, lepton_bus_object_get_y0 (object1));
    g_assert_cmpint (x1, ==, lepton_bus_object_get_x1 (object1));
    g_assert_cmpint (y1, ==, lepton_bus_object_get_y1 (object1));
    g_assert_cmpint (color, ==, lepton_object_get_color (object1));
    g_assert_cmpint (ripper, ==, lepton_bus_object_get_ripper_direction (object1));

    gchar *buffer1 = lepton_bus_object_to_buffer (object1);
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

  g_test_add_func ("/geda/liblepton/bus_object/construction",
                   check_construction);

  g_test_add_func ("/geda/liblepton/bus_object/check_accessors",
                   check_accessors);

  g_test_add_func ("/geda/liblepton/bus_object/serialization",
                   check_serialization);

  return g_test_run ();
}
