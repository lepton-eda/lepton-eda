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
	test_cpp \
	test_line \
	test_line_object \
	test_net_object \
	test_pin_object \
	test_point \
	test_string \
	test_text_object

test_cpp_SOURCES = test_cpp.cc

TESTS = $(check_PROGRAMS)

AM_CPPFLAGS = -DWOW -DLOCALEDIR=\"$(localedir)\"  $(DATADIR_DEFS) \
	-I$(srcdir)/../include -I$(srcdir)/../include/liblepton -I$(top_srcdir)

AM_CFLAGS = \
	$(GCC_CFLAGS) $(MINGW_CFLAGS) $(GUILE_CFLAGS) $(GLIB_CFLAGS) \
	$(GDK_PIXBUF_CFLAGS)
AM_CXXFLAGS =  $(MINGW_CLAGS) $(GUILE_CFLAGS) $(GLIB_CFLAGS) \
	$(GDK_PIXBUF_CFLAGS)

AM_LDFLAGS = \
	$(WINDOWS_LIBTOOL_FLAGS) $(MINGW_LDFLAGS) $(GUILE_LIBS) \
	$(GLIB_LIBS) $(GDK_PIXBUF_LIBS) \
	$(top_builddir)/libleptonrenderer/libleptonrenderer.la \
	$(top_builddir)/liblepton/src/liblepton.la
