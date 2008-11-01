## Process this file with automake to produce Makefile.in

bin_PROGRAMS = mk_verilog_syms

if CCISGCC
AM_CFLAGS = -Wall
endif

mk_verilog_syms_SOURCES = \
		mk_verilog_syms.c

INCLUDES = 

mk_verilog_syms_LDADD = -lm 

MOSTLYCLEANFILES = *.log *.ps core FILE *~ prototype.bak
CLEANFILES = *.log *.ps core FILE *~ prototype.bak
DISTCLEANFILES = *.log core FILE *~ prototype.bak
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ prototype.bak prototype.h Makefile.in configure

