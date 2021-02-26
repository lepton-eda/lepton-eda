#include <liblepton.h>

struct _TestData
{
  gchar *input;
  gchar *expected;
};

void
check_get_first_line ()
{
  static const struct _TestData test_data[] =
  {
    { "",       ""  },
    { "a",      "a" },
    { "a\n",    "a" },
    { "a\r",    "a" },
    { "\na",    ""  },
    { "\ra",    ""  },
    { "\n\n",   ""  },
    { "a\nb\n", "a" }
  };

  gint count = sizeof (test_data) / sizeof (struct _TestData);
  gint index;

  for (index = 0; index < count; index++) {
    gchar *actual;
    gchar *expected = test_data[index].expected;
    gchar *input = g_strdup (test_data[index].input);

    actual = lepton_str_get_first_line (input);
    g_assert_cmpstr (actual, ==, expected);
  }
}

void
check_remove_ending_newline ()
{
  static const struct _TestData test_data[] =
  {
    { "",       ""     },
    { "a",      "a"    },
    { "a\n",    "a"    },
    { "a\r",    "a"    },
    { "\na",    "\na"  },
    { "\ra",    "\ra"  },
    { "\n\n",   "\n"   },
    { "a\nb\n", "a\nb" }
  };

  gint count = sizeof (test_data) / sizeof (struct _TestData);
  gint index;

  for (index = 0; index < count; index++) {
    gchar *actual;
    gchar *expected = test_data[index].expected;
    gchar *input = g_strdup (test_data[index].input);

    actual = lepton_str_remove_ending_newline (input);
    g_assert_cmpstr (actual, ==, expected);
  }
}

int
main (int argc, char *argv[])
{
    g_test_init (&argc, &argv, NULL);

    g_test_add_func ("/geda/liblepton/string/get_first_line",
                     check_get_first_line);

    g_test_add_func ("/geda/liblepton/string/remove_ending_newline",
                     check_remove_ending_newline);

    return g_test_run ();
}
