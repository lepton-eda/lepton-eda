# Build a libtool library for installation in libdir.
lib_LTLIBRARIES = liblepton.la

BUILT_SOURCES = \
	scheme_init.x \
	scheme_smob.x \
	scheme_toplevel.x \
	scheme_object.x \
	scheme_component.x \
	scheme_page.x \
	scheme_attrib.x \
	scheme_os.x \
	scheme_config.x \
	scheme_closure.x \
	scheme_deprecated.x \
	g_rc.x

scheme_api_sources = \
	scheme_init.c \
	scheme_smob.c \
	scheme_toplevel.c \
	scheme_object.c \
	scheme_component.c \
	scheme_os.c \
	scheme_page.c \
	scheme_attrib.c \
	scheme_config.c \
	scheme_closure.c \
	scheme_deprecated.c \
	edascmhookproxy.c \
	edascmvaluetypes.c \
	g_rc.c

liblepton_la_SOURCES = \
	$(scheme_api_sources) \
	a_basic.c \
	angle.c \
	arc.c \
	arc_object.c \
	bounds.c \
	box_object.c \
	bus_object.c \
	circle_object.c \
	component_object.c \
	edapaths.c \
	edaconfig.c \
	edaerrors.c \
	f_basic.c \
	g_basic.c \
	box.c \
	color.c \
	circle.c \
	coord.c \
	fill_type.c \
	line.c \
	geda_list.c \
	geda_page.c \
	geda_path.c \
	geda_picture.c \
	geda_string.c \
	geda_toplevel.c \
	geda_transform.c \
	geda_undo.c \
	g_register.c \
	i_vars.c \
	liblepton.c \
	line_object.c \
	net_object.c \
	m_hatch.c \
	m_polygon.c \
	o_attrib.c \
	o_embed.c \
	o_selection.c \
	object.c \
	object_list.c \
	path_object.c \
	picture_object.c \
	pin_object.c \
	point.c \
	s_attrib.c \
	s_basic.c \
	s_clib.c \
	s_conn.c \
	s_encoding.c \
	s_log.c \
	s_slot.c \
	s_textbuffer.c \
	s_weakref.c \
	text_object.c \
	version.c \
	edacairo.c \
	edapangorenderer.c \
	edarenderer.c

liblepton_la_CPPFLAGS = -DLOCALEDIR=\"$(localedir)\"  $(DATADIR_DEFS) \
	-I$(srcdir)/../include -I$(srcdir)/../include/liblepton -I$(top_srcdir)
liblepton_la_CFLAGS = \
	$(GCC_CFLAGS) $(MINGW_CFLAGS) $(GUILE_CFLAGS) $(GLIB_CFLAGS) $(GIO_CFLAGS) \
	$(GDK_CFLAGS) $(GDK_PIXBUF_CFLAGS) $(CFLAGS) $(CAIRO_CFLAGS) $(PANGO_CFLAGS)
liblepton_la_LDFLAGS = -version-info $(LIBLEPTON_SHLIB_VERSION) \
	$(WINDOWS_LIBTOOL_FLAGS) $(MINGW_LDFLAGS) $(GUILE_LIBS) $(GIO_LIBS) \
	$(GLIB_LIBS) $(GDK_LIBS) $(GDK_PIXBUF_LIBS) $(CAIRO_LIBS) $(PANGO_LIBS)
LIBTOOL=@LIBTOOL@ --silent

# This is used to generate boilerplate for defining Scheme functions
# in C.
SUFFIXES = .x
snarf_cpp_opts = $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) \
	$(liblepton_la_CPPFLAGS) $(AM_CFLAGS) $(liblepton_la_CFLAGS)
.c.x:
	$(AM_V_GEN) CPP="$(CPP)" $(GUILE_SNARF) -o $@ $< $(snarf_cpp_opts)

CLEANFILES = $(BUILT_SOURCES)

# Unfortunately, in order to test libtool thoroughly, we need access
# to its private directory.
objdir = `sed -n -e 's/^objdir=\"\(.*\)\"$$/\1/p' ./libtool`
