## Process this file with automake to produce Makefile.in

bin_PROGRAMS = gmk_sym smash_megafile convert_sym sarlacc_schem

# don't forget all *.h files */
gmk_sym_SOURCES = gmk_sym.c char_width.c 
smash_megafile_SOURCES = smash_megafile.c 
convert_sym_SOURCES = convert_sym.c 
sarlacc_schem_SOURCES = sarlacc_schem.c

INCLUDES =

gmk_sym_LDADD = 
smash_megafile_LDADD = 
convert_sym_LDADD = -lm
sarlacc_schem_LDADD = 

EXTRA_DIST = 7474.txt 8031.txt convert_sym.awk gpstoimage mk_char_tab.pl \
	     test.txt block.txt sarlacc_sym

MOSTLYCLEANFILES = *.log *.ps core FILE *~
CLEANFILES = *.log *.ps core FILE *~ 
DISTCLEANFILES = *.log *.ps core FILE *~ 
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ 

