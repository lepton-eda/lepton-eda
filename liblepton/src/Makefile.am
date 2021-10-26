# Build a libtool library for installation in libdir.
lib_LTLIBRARIES = liblepton.la

BUILT_SOURCES = \
	scheme_init.x \
	scheme_toplevel.x \
	scheme_closure.x

scheme_api_sources = \
	scheme_init.c \
	scheme_smob.c \
	scheme_toplevel.c \
	scheme_config.c \
	scheme_closure.c \
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
	fill.c \
	line.c \
	list.c \
	page.c \
	path.c \
	picture.c \
	str.c \
	stroke.c \
	toplevel.c \
	transform.c \
	undo.c \
	i_vars.c \
	liblepton.c \
	line_object.c \
	net_object.c \
	m_hatch.c \
	m_polygon.c \
	o_attrib.c \
	o_selection.c \
	object.c \
	object_list.c \
	path_object.c \
	picture_object.c \
	pin_object.c \
	point.c \
	s_attrib.c \
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
	edarenderer.c \
	export.c

liblepton_la_CPPFLAGS = -DLOCALEDIR=\"$(localedir)\"  $(DATADIR_DEFS) \
	-I$(srcdir)/../include -I$(srcdir)/../include/liblepton -I$(top_srcdir)
liblepton_la_CFLAGS = \
	$(GCC_CFLAGS) $(MINGW_CFLAGS) $(GUILE_CFLAGS) $(GLIB_CFLAGS) $(GIO_CFLAGS) \
	$(GTK_CFLAGS)  $(CAIRO_PNG_CFLAGS) $(CAIRO_PDF_CLAGS) $(CAIRO_PS_CFLAGS) $(CAIRO_SVG_CFLAGS) \
	$(GDK_CFLAGS) $(GDK_PIXBUF_CFLAGS) $(CFLAGS) $(CAIRO_CFLAGS) $(PANGO_CFLAGS)
liblepton_la_LDFLAGS = -version-info $(LIBLEPTON_SHLIB_VERSION) \
	$(WINDOWS_LIBTOOL_FLAGS) $(MINGW_LDFLAGS) $(GUILE_LIBS) $(GIO_LIBS) \
	$(GLIB_LIBS) $(GDK_LIBS) $(GDK_PIXBUF_LIBS) $(CAIRO_LIBS) $(PANGO_LIBS) \
	$(GTK_LIBS) $(CAIRO_PNG_LIBS) $(CAIRO_PDF_CLAGS) $(CAIRO_PS_LIBS) $(CAIRO_SVG_LIBS)
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
