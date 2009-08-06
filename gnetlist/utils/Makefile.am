## Process this file with automake to produce Makefile.in

bin_PROGRAMS = mk_verilog_syms

mk_verilog_syms_SOURCES = \
		mk_verilog_syms.c

mk_verilog_syms_CFLAGS = $(GCC_CFLAGS) $(MINGW_CFLIGS)
mk_verilog_syms_LDFLAGS = -lm

MOSTLYCLEANFILES = *.log *.ps core FILE *~ prototype.bak
CLEANFILES = *.log *.ps core FILE *~ prototype.bak
DISTCLEANFILES = *.log core FILE *~ prototype.bak
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ prototype.bak prototype.h Makefile.in configure

