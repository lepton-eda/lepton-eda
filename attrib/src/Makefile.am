bin_PROGRAMS = lepton-attrib

lepton_attrib_SOURCES = lepton-attrib.c

lepton_attrib_CPPFLAGS = -I$(top_srcdir)/libleptonattrib/include \
	-I$(top_srcdir)/liblepton/include \
	-I$(srcdir)/../include -I$(top_srcdir) -I$(includedir)
lepton_attrib_CFLAGS = $(GCC_CFLAGS) $(GLIB_CFLAGS) \
	$(MINGW_CFLAGS) \
	$(GTK_CFLAGS) $(GTHREAD_CFLAGS) $(GUILE_CFLAGS)
lepton_attrib_LDFLAGS = $(GLIB_LIBS) $(GTK_LIBS) $(GTHREAD_LIBS) \
	$(GUILE_LIBS) $(MINGW_GUI_LDFLAGS)
lepton_attrib_LDADD = \
	$(top_builddir)/libleptonattrib/src/libleptonattrib.la \
	$(top_builddir)/liblepton/src/liblepton.la


localedir = @datadir@/locale
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@
