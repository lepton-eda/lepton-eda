## Process this file with automake to produce Makefile.in

bin_PROGRAMS = gmk_sym smash_megafile convert_sym sarlacc_schem
bin_SCRIPTS = sarlacc_sym

# don't forget all *.h files */
gmk_sym_SOURCES = gmk_sym.c char_width.c 
smash_megafile_SOURCES = smash_megafile.c 
convert_sym_SOURCES = convert_sym.c char_width.c
sarlacc_schem_SOURCES = sarlacc_schem.c

INCLUDES = $(LIBGEDA_CFLAGS)

gmk_sym_LDADD = $(LIBERTY)
smash_megafile_LDADD = 
convert_sym_LDADD = $(LIBERTY) -lm
sarlacc_schem_LDADD = 

EXTRA_DIST = convert_sym.awk gpstoimage mk_char_tab.pl sarlacc_sym

MOSTLYCLEANFILES = *.log *.ps core FILE *~
CLEANFILES = *.log *.ps core FILE *~ 
DISTCLEANFILES = *.log core FILE *~ prototype.bak
MAINTAINERCLEANFILES = *.log *.ps core FILE *~  Makefile.in configure

