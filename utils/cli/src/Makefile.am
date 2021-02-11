bin_PROGRAMS = lepton-cli

BUILT_SOURCES = shell.x

lepton_cli_SOURCES = \
	builtins.h \
	gettext.h \
	lepton-cli.c \
	config.c \
	export.c \
	shell.c

lepton_cli_CPPFLAGS = \
	-I$(top_srcdir) -I$(top_srcdir)/liblepton/include -I$(includedir)
lepton_cli_CFLAGS = \
	$(GCC_CFLAGS) $(MINGW_CFLAGS) $(GUILE_CFLAGS) $(GTK_CFLAGS) \
	$(GDK_PIXBUF_CFLAGS) $(CAIRO_CFLAGS) $(CAIRO_PNG_CFLAGS) \
	$(CAIRO_PDF_CLAGS) $(CAIRO_PS_CFLAGS) $(CAIRO_SVG_CFLAGS) $(CFLAGS)
lepton_cli_LDFLAGS = $(GUILE_LIBS) $(GTK_LIBS) $(GDK_PIXBUF_LIBS) $(CAIRO_LIBS) \
	$(CAIRO_PNG_LIBS) $(CAIRO_PDF_CLAGS) $(CAIRO_PS_LIBS) $(CAIRO_SVG_LIBS)
lepton_cli_LDADD = \
	$(top_builddir)/liblepton/src/liblepton.la

localedir = @datadir@/locale
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@

# This is used to generate boilerplate for defining Scheme functions
# in C.
snarf_cpp_opts = $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) \
	$(lepton_cli_CPPFLAGS) $(AM_CFLAGS) $(lepton_cli_CFLAGS)
.c.x:
	$(AM_V_GEN) CPP="$(CPP)" $(GUILE_SNARF) -o $@ $< $(snarf_cpp_opts)

SUFFIXES = .x

CLEANFILES = $(BUILT_SOURCES)
