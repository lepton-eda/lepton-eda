## Process this file with automake to produce Makefile.in

bin_PROGRAMS = gmk_sym smash_megafile convert_sym sarlacc_schem olib \
	       gsch2pcb grenum

if CCISGCC
AM_CFLAGS = -Wall
endif

# don't forget all *.h files */
gmk_sym_SOURCES = gmk_sym.c char_width.c 
smash_megafile_SOURCES = smash_megafile.c 
convert_sym_SOURCES = convert_sym.c char_width.c
sarlacc_schem_SOURCES = sarlacc_schem.c
olib_SOURCES = olib.l geda_sym_format.h
gsch2pcb_SOURCES = gsch2pcb.c
grenum_SOURCES = grenum.c grenum.h

gmk_sym_CFLAGS = @MINGW_CFLAGS@
smash_megafile_CFLAGS = @MINGW_CFLAGS@
convert_sym_CFLAGS = @GSCHLAS_CFLAGS@
sarlacc_schem_CFLAGS = @GSCHLAS_CFLAGS@
olib_CFLAGS = @MINGW_CFLAGS@
gsch2pcb_CFLAGS = @GSCH2PCB_CFLAGS@
grenum_CFLAGS = @GRENUM_CFLAGS@

convert_sym_LDADD = -lm
olib_LDADD = -lm
gsch2pcb_LDADD = @GSCH2PCB_LDFLAGS@ 
grenum_LDADD = @GRENUM_LDFLAGS@

MOSTLYCLEANFILES = *.log *.ps core FILE *~ olib.c
CLEANFILES = *.log *.ps core FILE *~  olib.c
DISTCLEANFILES = *.log core FILE *~ prototype.bak olib.c
MAINTAINERCLEANFILES = *.log *.ps core FILE *~  Makefile.in configure olib.c

