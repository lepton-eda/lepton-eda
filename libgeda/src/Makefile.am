
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
	scheme_deprecated.x

scheme_api_sources = \
	scheme_init.c \
	scheme_smob.c \
	scheme_toplevel.c \
	scheme_object.c \
	scheme_complex.c \
	scheme_page.c \
	scheme_attrib.c \
	scheme_deprecated.c

libgeda_la_SOURCES = \
	$(scheme_api_sources) \
	a_basic.c \
	f_basic.c \
	f_print.c \
	g_basic.c \
	geda_list.c \
	g_rc.c \
	g_register.c \
	i_vars.c \
	libgeda.c \
	m_basic.c \
	m_bounds.c \
	m_box.c \
	m_circle.c \
	m_hatch.c \
	m_line.c \
	m_polygon.c \
	m_transform.c \
	o_arc_basic.c \
	o_attrib.c \
	o_basic.c \
	o_box_basic.c \
	o_bus_basic.c \
	o_circle_basic.c \
	o_complex_basic.c \
	o_embed.c \
	o_line_basic.c \
	o_list.c \
	o_net_basic.c \
	o_path_basic.c \
	o_picture.c \
	o_pin_basic.c \
	o_selection.c \
	o_text_basic.c \
	s_attrib.c \
	s_basic.c \
	s_clib.c \
	s_color.c \
	s_conn.c \
	s_cue.c \
	s_encoding.c \
	s_hierarchy.c \
	s_log.c \
	s_menu.c \
	s_page.c \
	s_papersizes.c \
	s_path.c \
	s_slib.c \
	s_slot.c \
	s_textbuffer.c \
	s_tile.c \
	s_toplevel.c \
	s_undo.c \
	s_weakref.c \
	u_basic.c

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
	$(GUILE_SNARF) -o $@ $< $(snarf_cpp_opts)

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~ $(BUILT_SOURCES)
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in

# Unfortunately, in order to test libtool thoroughly, we need access
# to its private directory.
objdir = `sed -n -e 's/^objdir=\"\(.*\)\"$$/\1/p' ./libtool`
