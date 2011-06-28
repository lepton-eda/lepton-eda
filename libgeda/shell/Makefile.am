noinst_PROGRAMS = geda-shell

BUILT_SOURCES = \
	shell.x

geda_shell_SOURCES = shell.c

geda_shell_CPPFLAGS = \
	-I$(srcdir)/../include -I$(top_srcdir) -I$(includedir) \
	-I$(top_srcdir)/intl
geda_shell_CFLAGS = \
	$(GCC_CFLAGS) $(MINGW_CFLAGS) $(GUILE_CFLAGS) $(GLIB_CFLAGS) \
	$(GDK_PIXBUF_CFLAGS)
geda_shell_LDFLAGS = $(GLIB_LIBS) $(GUILE_LIBS) $(GDK_PIXBUF_LIBS)
geda_shell_LDADD = ../src/libgeda.la @LIBINTL@

localedir = @datadir@/locale
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@

# This is used to generate boilerplate for defining Scheme functions
# in C.
SUFFIXES = .x
snarf_cpp_opts = $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) \
	$(geda_shell_CPPFLAGS) $(AM_CFLAGS) $(geda_shell_CFLAGS)
.c.x:
	CPP="$(CPP)" $(GUILE_SNARF) -o $@ $< $(snarf_cpp_opts)

CLEANFILES = $(BUILT_SOURCES)
