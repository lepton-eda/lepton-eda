
check_PROGRAMS = \
	test_angle \
	test_arc \
	test_arc_object \
	test_bounds \
	test_box \
	test_bus_object \
	test_circle \
	test_circle_object \
	test_coord \
	test_line \
	test_line_object \
	test_net_object \
	test_pin_object \
	test_point \
	test_string

TESTS = \
	test_angle \
	test_arc \
	test_arc_object \
	test_bounds \
	test_box \
	test_bus_object \
	test_circle \
	test_circle_object \
	test_coord \
	test_line \
	test_line_object \
	test_net_object \
	test_pin_object \
	test_point \
	test_string

AM_CPPFLAGS = -DLOCALEDIR=\"$(localedir)\"  $(DATADIR_DEFS) \
	-I$(srcdir)/../include -I$(srcdir)/../include/libgeda -I$(top_srcdir)

AM_CFLAGS = -DWOW \
	$(GCC_CFLAGS) $(MINGW_CFLAGS) $(GUILE_CFLAGS) $(GLIB_CFLAGS) \
	$(GDK_PIXBUF_CFLAGS)

AM_LDFLAGS = -version-info $(LIBGEDA_SHLIB_VERSION) \
	$(WINDOWS_LIBTOOL_FLAGS) $(MINGW_LDFLAGS) $(GUILE_LIBS) \
	$(GLIB_LIBS) $(GDK_PIXBUF_LIBS) \
	$(top_builddir)/libgedacairo/libgedacairo.la \
	$(top_builddir)/libgeda/src/libgeda.la \
	@LIBINTL@
