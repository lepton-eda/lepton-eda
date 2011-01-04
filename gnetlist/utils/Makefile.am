## Process this file with automake to produce Makefile.in

noinst_PROGRAMS = mk_verilog_syms

mk_verilog_syms_SOURCES = \
		mk_verilog_syms.c

mk_verilog_syms_CFLAGS = $(GCC_CFLAGS) $(MINGW_CFLIGS)
mk_verilog_syms_LDFLAGS = -lm

MOSTLYCLEANFILES = *.log *.ps core FILE *~
CLEANFILES = *.log *.ps core FILE *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ Makefile.in

