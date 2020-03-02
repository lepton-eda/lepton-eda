#include <liblepton.h>
#include <version.h>

void
check_construction ()
{
  gint count;

  for (count = 0; count < 1000; count++) {
    gint x0 = g_test_rand_int ();
    gint y0 = g_test_rand_int ();
    gint x1 = g_test_rand_int ();
    gint y1 = g_test_rand_int ();
    gint color = g_test_rand_int_range (0, colors_count());
    gint type = g_test_rand_int_range (0, 2);
    gint which = g_test_rand_int_range (0, 2);

    LeptonObject *object0 = lepton_pin_object_new (color,
                                                   x0,
                                                   y0,
                                                   x1,
                                                   y1,
                                                   type,
                                                   which);

    g_assert (object0 != NULL);
    g_assert_cmpint (OBJ_PIN, ==, lepton_object_get_type (object0));

    g_assert_cmpint (x0, ==, lepton_pin_object_get_x0 (object0));
    g_assert_cmpint (y0, ==, lepton_pin_object_get_y0 (object0));
    g_assert_cmpint (x1, ==, lepton_pin_object_get_x1 (object0));
    g_assert_cmpint (y1, ==, lepton_pin_object_get_y1 (object0));
    g_assert_cmpint (color, ==, lepton_object_get_color (object0));

    LeptonObject *object1 = lepton_pin_object_copy (object0);

    g_assert (object1 != NULL);
    g_assert (object1 != object0);
    g_assert_cmpint (OBJ_PIN, ==, lepton_object_get_type (object1));

    lepton_object_delete (object0);

    g_assert_cmpint (x0, ==, lepton_pin_object_get_x0 (object1));
    g_assert_cmpint (y0, ==, lepton_pin_object_get_y0 (object1));
    g_assert_cmpint (x1, ==, lepton_pin_object_get_x1 (object1));
    g_assert_cmpint (y1, ==, lepton_pin_object_get_y1 (object1));
    g_assert_cmpint (color, ==, lepton_object_get_color (object1));

    lepton_object_delete (object1);
  }
}

void
check_accessors ()
{
  gint count;

  for (count = 0; count < 1000; count++) {
    gint x0 = g_test_rand_int ();
    gint y0 = g_test_rand_int ();
    gint x1 = g_test_rand_int ();
    gint y1 = g_test_rand_int ();
    gint color = g_test_rand_int_range (0, colors_count());
    gint type = g_test_rand_int_range (0, 2);
    gint which = g_test_rand_int_range (0, 2);

    LeptonObject *object0 = lepton_pin_object_new (color,
                                                   x0,
                                                   y0,
                                                   x1,
                                                   y1,
                                                   type,
                                                   which);

    g_assert (object0 != NULL);
    g_assert_cmpint (OBJ_PIN, ==, lepton_object_get_type (object0));

    x0 = g_test_rand_int ();
    y0 = g_test_rand_int ();
    x1 = g_test_rand_int ();
    y1 = g_test_rand_int ();
    color = g_test_rand_int_range (0, colors_count());

    lepton_pin_object_set_x0 (object0, x0);
    lepton_pin_object_set_y0 (object0, y0);
    lepton_pin_object_set_x1 (object0, x1);
    lepton_pin_object_set_y1 (object0, y1);
    lepton_object_set_color (object0, color);

    g_assert_cmpint (x0, ==, lepton_pin_object_get_x0 (object0));
    g_assert_cmpint (y0, ==, lepton_pin_object_get_y0 (object0));
    g_assert_cmpint (x1, ==, lepton_pin_object_get_x1 (object0));
    g_assert_cmpint (y1, ==, lepton_pin_object_get_y1 (object0));
    g_assert_cmpint (color, ==, lepton_object_get_color (object0));

    lepton_object_delete (object0);
  }
}

void
check_serialization ()
{
  gint count;
  gint converted;
  guint version;

  converted = sscanf (PACKAGE_DATE_VERSION, "%u", &version);
  g_assert_cmpuint (converted, ==, 1);

  for (count = 0; count < 1000; count++) {
    gint x0 = g_test_rand_int ();
    gint y0 = g_test_rand_int ();
    gint x1 = g_test_rand_int ();
    gint y1 = g_test_rand_int ();
    gint color = g_test_rand_int_range (0, colors_count());
    gint type = g_test_rand_int_range (0, 2);
    gint which = g_test_rand_int_range (0, 2);

    LeptonObject *object0 = lepton_pin_object_new (color,
                                                   x0,
                                                   y0,
                                                   x1,
                                                   y1,
                                                   type,
                                                   which);

    g_assert (object0 != NULL);

    gchar *buffer0 = lepton_pin_object_to_buffer (object0);
    lepton_object_delete (object0);
    g_assert (buffer0 != NULL);

    LeptonObject *object1 = lepton_pin_object_read (buffer0,
                                                    version,
                                                    FILEFORMAT_VERSION,
                                                    NULL);

    g_assert (object1 != NULL);

    g_assert_cmpint (x0, ==, lepton_pin_object_get_x0 (object1));
    g_assert_cmpint (y0, ==, lepton_pin_object_get_y0 (object1));
    g_assert_cmpint (x1, ==, lepton_pin_object_get_x1 (object1));
    g_assert_cmpint (y1, ==, lepton_pin_object_get_y1 (object1));
    g_assert_cmpint (color, ==, lepton_object_get_color (object1));

    gchar *buffer1 = lepton_pin_object_to_buffer (object1);
    lepton_object_delete (object1);
    g_assert (buffer1 != NULL);

    g_assert_cmpstr (buffer0, ==, buffer1);
    g_free (buffer0);
    g_free (buffer1);
  }
}

int
main (int argc, char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/geda/liblepton/pin_object/construction",
                   check_construction);

  g_test_add_func ("/geda/liblepton/pin_object/check_accessors",
                   check_accessors);

  g_test_add_func ("/geda/liblepton/pin_object/serialization",
                   check_serialization);

  return g_test_run ();
}
