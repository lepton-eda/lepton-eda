## Process this file with automake to produce Makefile.in

bin_PROGRAMS = gschlas 

# don't forget all *.h files */
gschlas_SOURCES = \
		i_vars.c g_rc.c g_register.c globals.c \
		gschlas.c parsecmd.c 


INCLUDES = @GSCHLAS_CFLAGS@ 
gschlas_LDADD = @GSCHLAS_LDFLAGS@

EXTRA_DIST = create_proto 

CPROTO =        cproto
CPROTOCFLAGS =  $(INCLUDES) -I. -I.. -I../include
ALES_HACK =     -D__GNUC__

proto: prototype.h

prototype.h: $(gschem_SOURCES)
	$(CPROTO) $(ALES_HACK) $(CPROTOCFLAGS) *.c > new_proto
	mv new_proto ../include/prototype.h

MOSTLYCLEANFILES = *.log *.ps core FILE *~ prototype.bak
CLEANFILES = *.log *.ps core FILE *~ prototype.bak
DISTCLEANFILES = *.log core FILE *~ prototype.bak
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ prototype.bak prototype.h Makefile.in configure

