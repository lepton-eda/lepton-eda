## Process this file with automake to produce Makefile.in

bin_PROGRAMS = gmk_sym smash_megafile convert_sym sarlacc_schem olib

# don't forget all *.h files */
gmk_sym_SOURCES = gmk_sym.c char_width.c 
smash_megafile_SOURCES = smash_megafile.c 
convert_sym_SOURCES = convert_sym.c char_width.c
sarlacc_schem_SOURCES = sarlacc_schem.c
olib_SOURCES = olib.l geda_sym_format.h

INCLUDES = $(LIBGEDA_CFLAGS)

gmk_sym_LDADD = @MINGW_LIBS@ 
smash_megafile_LDADD = @MINGW_LIBS@
convert_sym_LDADD = @MINGW_LIBS@ -lm
sarlacc_schem_LDADD = @MINGW_LIBS@
olib_LDADD = @LEXLIB@ @MINGW_LIBS@ -lm

EXTRA_DIST = 

MOSTLYCLEANFILES = *.log *.ps core FILE *~ olib.c
CLEANFILES = *.log *.ps core FILE *~  olib.c
DISTCLEANFILES = *.log core FILE *~ prototype.bak olib.c
MAINTAINERCLEANFILES = *.log *.ps core FILE *~  Makefile.in configure olib.c

