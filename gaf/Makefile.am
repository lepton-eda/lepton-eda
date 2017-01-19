SUBDIRS = po

bin_PROGRAMS = gaf

EXTRA_DIST = $(html_man_files) gaf.1.in ChangeLog

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	( \
	  cd $(srcdir) && \
	  $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- . || \
	  echo "WARNING: ChangeLog information not available from git" >&2 ; \
	) > $@
endif HAVE_GIT_REPO

BUILT_SOURCES = shell.x

gaf_SOURCES = \
	builtins.h \
	gettext.h \
	gaf.c \
	config.c \
	export.c \
	shell.c

gaf_CPPFLAGS = \
	-I$(top_srcdir) -I$(top_srcdir)/libgeda/include -I$(includedir)
gaf_CFLAGS = \
	$(GCC_CFLAGS) $(MINGW_CFLAGS) $(GUILE_CFLAGS) $(GTK_CFLAGS) \
	$(GDK_PIXBUF_CFLAGS) $(CAIRO_CFLAGS) $(CAIRO_PNG_CFLAGS) \
	$(CAIRO_PDF_CLAGS) $(CAIRO_PS_CFLAGS) $(CAIRO_SVG_CFLAGS)
gaf_LDFLAGS = $(GUILE_LIBS) $(GTK_LIBS) $(GDK_PIXBUF_LIBS) $(CAIRO_LIBS) \
	$(CAIRO_PNG_LIBS) $(CAIRO_PDF_CLAGS) $(CAIRO_PS_LIBS) $(CAIRO_SVG_LIBS)
gaf_LDADD = \
	$(top_builddir)/libgedacairo/libgedacairo.la \
	$(top_builddir)/libgeda/src/libgeda.la

localedir = @datadir@/locale
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@

# This is used to generate boilerplate for defining Scheme functions
# in C.
snarf_cpp_opts = $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) \
	$(gaf_CPPFLAGS) $(AM_CFLAGS) $(gaf_CFLAGS)
.c.x:
	$(AM_V_GEN) CPP="$(CPP)" $(GUILE_SNARF) -o $@ $< $(snarf_cpp_opts)

.1.in.1:
	d=`$(GUILE) -c '(setlocale LC_ALL "C") \
	  (display (strftime "%B %-d, %Y" \
	  (car (strptime "%Y%m%d" (cadr (program-arguments))))))' $(DATE_VERSION)` ; \
	v="$(DOTTED_VERSION).$(DATE_VERSION)" ; \
	y=`echo $(DATE_VERSION)| sed -e"s/\(....\).*/\1/"`; \
	sed -e"s:[@]DATE@:$$d:" -e"s:[@]VERSION@:$$v:" -e"s:[@]YEAR@:$$y:" < $< > $@

# Rules for converting man->html
dist_man_MANS = gaf.1
html_man_files = gaf.html

docsmandir=$(docdir)/man
docsman_DATA=$(html_man_files)
if ENABLE_GROFF_HTML
.1.html:
	rm -rf $@.tmp && \
	LC_NUMERIC=C $(GROFF) -man -T html  $< >$@.tmp && \
	mv -f $@.tmp $@
else
.1.html:
	@echo "No working groff available: cannot create $@"
	@! :
endif

SUFFIXES = .x .1.in .1 .html

CLEANFILES = $(BUILT_SOURCES)
MAINTAINERCLEANFILES = ChangeLog
