SUBDIRS = po

bin_PROGRAMS = gaf

BUILT_SOURCES = shell.x

gaf_SOURCES = \
	builtins.h \
	gettext.h \
	gaf.c \
	export.c \
	shell.c

gaf_CPPFLAGS = \
	-I$(top_srcdir) -I$(top_srcdir)/libgeda/include -I$(includedir) \
	-I$(top_srcdir)/intl
gaf_CFLAGS = \
	$(GCC_CFLAGS) $(MINGW_CFLAGS) $(GUILE_CFLAGS) $(GTK_CFLAGS) \
	$(GDK_PIXBUF_CFLAGS) $(CAIRO_CFLAGS) $(CAIRO_PNG_CFLAGS) \
	$(CAIRO_PDF_CLAGS) $(CAIRO_PS_CFLAGS) $(CAIRO_SVG_CFLAGS)
gaf_LDFLAGS = $(GUILE_LIBS) $(GTK_LIBS) $(GDK_PIXBUF_LIBS) $(CAIRO_LIBS) \
	$(CAIRO_PNG_LIBS) $(CAIRO_PDF_CLAGS) $(CAIRO_PS_LIBS) $(CAIRO_SVG_LIBS)
gaf_LDADD = \
	$(top_builddir)/libgedacairo/libgedacairo.la \
	$(top_builddir)/libgeda/src/libgeda.la \
	@LIBINTL@

localedir = @datadir@/locale
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@

# This is used to generate boilerplate for defining Scheme functions
# in C.
SUFFIXES = .x
snarf_cpp_opts = $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) \
	$(gaf_CPPFLAGS) $(AM_CFLAGS) $(gaf_CFLAGS)
.c.x:
	CPP="$(CPP)" $(GUILE_SNARF) -o $@ $< $(snarf_cpp_opts)

CLEANFILES = $(BUILT_SOURCES)
