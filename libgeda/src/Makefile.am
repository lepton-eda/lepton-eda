
# Build a libtool library for installation in libdir.
lib_LTLIBRARIES = libgeda.la

BUILT_SOURCES = \
	scheme_init.x \
	scheme_smob.x \
	scheme_toplevel.x \
	scheme_object.x \
	scheme_complex.x \
	scheme_page.x \
	scheme_attrib.x \
	scheme_os.x \
	scheme_config.x \
	scheme_closure.x \
	scheme_log.x \
	scheme_deprecated.x

scheme_api_sources = \
	scheme_init.c \
	scheme_smob.c \
	scheme_toplevel.c \
	scheme_object.c \
	scheme_complex.c \
	scheme_os.c \
	scheme_page.c \
	scheme_attrib.c \
	scheme_config.c \
	scheme_closure.c \
	scheme_deprecated.c \
	scheme_log.c \
	edascmhookproxy.c \
	edascmvaluetypes.c

libgeda_la_SOURCES = \
	$(scheme_api_sources) \
	a_basic.c \
	edapaths.c \
	edaconfig.c \
	edaerrors.c \
	f_basic.c \
	g_basic.c \
	geda_angle.c \
	geda_arc.c \
	geda_arc_object.c \
	geda_bounds.c \
	geda_box.c \
	geda_box_object.c \
	geda_bus_object.c \
	geda_color.c \
	geda_color_map.c \
	geda_circle.c \
	geda_circle_object.c \
	geda_complex_object.c \
	geda_coord.c \
	geda_fill_type.c \
	geda_line.c \
	geda_line_object.c \
	geda_list.c \
	geda_net_object.c \
	geda_object.c \
	geda_object_list.c \
	geda_page.c \
	geda_path.c \
	geda_path_object.c \
	geda_picture.c \
	geda_picture_object.c \
	geda_pin_object.c \
	geda_point.c \
	geda_string.c \
	geda_text_object.c \
	geda_toplevel.c \
	geda_transform.c \
	geda_undo.c \
	g_rc.c \
	g_register.c \
	i_vars.c \
	libgeda.c \
	m_hatch.c \
	m_polygon.c \
	o_attrib.c \
	o_embed.c \
	o_selection.c \
	s_attrib.c \
	s_basic.c \
	s_clib.c \
	s_conn.c \
	s_encoding.c \
	s_hierarchy.c \
	s_log.c \
	s_menu.c \
	s_slot.c \
	s_textbuffer.c \
	s_weakref.c

libgeda_la_CPPFLAGS = -DLOCALEDIR=\"$(localedir)\"  $(DATADIR_DEFS) \
	-I$(srcdir)/../include -I$(srcdir)/../include/libgeda -I$(top_srcdir)
libgeda_la_CFLAGS = \
	$(GCC_CFLAGS) $(MINGW_CFLAGS) $(GUILE_CFLAGS) $(GLIB_CFLAGS) \
	$(GDK_PIXBUF_CFLAGS)
libgeda_la_LDFLAGS = -version-info $(LIBGEDA_SHLIB_VERSION) \
	$(WINDOWS_LIBTOOL_FLAGS) $(MINGW_LDFLAGS) $(GUILE_LIBS) \
	$(GLIB_LIBS) $(GDK_PIXBUF_LIBS)
LIBTOOL=@LIBTOOL@ --silent

# This is used to generate boilerplate for defining Scheme functions
# in C.
SUFFIXES = .x
snarf_cpp_opts = $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) \
	$(libgeda_la_CPPFLAGS) $(AM_CFLAGS) $(libgeda_la_CFLAGS)
.c.x:
	$(AM_V_GEN) CPP="$(CPP)" $(GUILE_SNARF) -o $@ $< $(snarf_cpp_opts)

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~ $(BUILT_SOURCES)
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in

# Unfortunately, in order to test libtool thoroughly, we need access
# to its private directory.
objdir = `sed -n -e 's/^objdir=\"\(.*\)\"$$/\1/p' ./libtool`
