bin_PROGRAMS = lepton-export

lepton_export_SOURCES = \
	lepton-export.c

lepton_export_CPPFLAGS = \
	-I$(top_srcdir) -I$(top_srcdir)/liblepton/include -I$(includedir)
lepton_export_CFLAGS = \
	$(GCC_CFLAGS) $(MINGW_CFLAGS) $(GUILE_CFLAGS) $(GTK_CFLAGS) \
	$(GDK_PIXBUF_CFLAGS) $(CAIRO_CFLAGS) $(CAIRO_PNG_CFLAGS) \
	$(CAIRO_PDF_CLAGS) $(CAIRO_PS_CFLAGS) $(CAIRO_SVG_CFLAGS) $(CFLAGS)
lepton_export_LDFLAGS = $(GUILE_LIBS) $(GTK_LIBS) $(GDK_PIXBUF_LIBS) $(CAIRO_LIBS) \
	$(CAIRO_PNG_LIBS) $(CAIRO_PDF_CLAGS) $(CAIRO_PS_LIBS) $(CAIRO_SVG_LIBS)
lepton_export_LDADD = \
	$(top_builddir)/liblepton/src/liblepton.la

localedir = @datadir@/locale
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@
