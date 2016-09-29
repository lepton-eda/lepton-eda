## Process this file with automake to produce Makefile.in

SUBDIRS = hierarchy hierarchy2 drc2 common

EXTRA_DIST = runtest.sh README stack_1.geda gnetlistrc.orig

check_SCRIPTS = tests

BUILDDIR=$(builddir)
SRCDIR=$(srcdir)
GNETLIST=$(builddir)/../src/gnetlist

tests:

# cleanup
	rm -f $(BUILDDIR)/new_*
	rm -rf $(BUILDDIR)/devfiles
	mkdir $(BUILDDIR)/devfiles
	cp -f $(srcdir)/gnetlistrc.orig $(builddir)/gnetlistrc

# stack gEDA
	$(SRCDIR)/runtest.sh $(SRCDIR)/../examples/stack_1.sch geda \
		$(BUILDDIR) $(SRCDIR)


MOSTLYCLEANFILES = new_* gnetlistrc

mostlyclean-local:
	rm -rf devfiles
