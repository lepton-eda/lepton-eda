## Process this file with automake to produce Makefile.in

bin_PROGRAMS = gmk_sym smash_megafile convert_sym sarlacc_schem olib \
	       gsch2pcb grenum

# don't forget all *.h files */
gmk_sym_SOURCES = gmk_sym.c char_width.c
smash_megafile_SOURCES = smash_megafile.c
grenum_SOURCES = grenum.c grenum.h

olib_SOURCES = olib.l geda_sym_format.h
olib_LDFLAGS = -lm

convert_sym_SOURCES = convert_sym.c char_width.c
convert_sym_LDFLAGS = -lm

gsch2pcb_SOURCES = gsch2pcb.c
gsch2pcb_CPPFLAGS = \
	-DPCBDATADIR=\"$(PCBDATADIR)\" \
	-DPCBLIBPATH=\"$(PCBLIBPATH)\"
gsch2pcb_CFLAGS = $(GLIB_CFLAGS)
gsch2pcb_LDFLAGS = $(GLIB_LIBS)

sarlacc_schem_SOURCES = sarlacc_schem.c
sarlacc_schem_CFLAGS = $(GLIB_CFLAGS) $(GUILE_CFLAGS) $(GDK_PIXBUF_CFLAGS)
sarlacc_schem_LDFLAGS = $(GLIB_LIBS) $(GUILE_LIBS) $(GDK_PIXBUF_LIBS)
sarlacc_schem_LDADD = $(top_builddir)/libgeda/src/libgeda.la

AM_CFLAGS = $(GCC_CFLAGS) $(MINGW_CFLIGS)
AM_CPPFLAGS = -I$(top_srcdir)/libgeda/include -I$(srcdir)/../include \
	-I$(top_srcdir) -I$(includedir)

MOSTLYCLEANFILES = *.log *.ps core FILE *~ olib.c
CLEANFILES = *.log *.ps core FILE *~  olib.c
DISTCLEANFILES = *.log core FILE *~ prototype.bak olib.c
MAINTAINERCLEANFILES = *.log *.ps core FILE *~  Makefile.in configure olib.c
