## Process this file with automake to produce Makefile.in

EXTRA_DIST = 1217.geda bottom.sch bottom.sym gnetlistrc gschemrc gschlasrc \
             hierarchy.geda middle.sch middle.sym rock.sch rock.sym \
	     top.sch

check_SCRIPTS = tests

tests:
	rm -f new_*
	gnetlist -o new_hierarchy.geda -g geda ./top.sch
	diff hierarchy.geda new_hierarchy.geda


MOSTLYCLEANFILES = new_* core *.log FILE *.ps *~
CLEANFILES = new_* core *.log FILE *.ps *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak
MAINTAINERCLEANFILES = new_* core *.log FILE *.ps *~ Makefile.in configure



