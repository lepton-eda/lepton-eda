
AM_CFLAGS = -g -O2 

SUBDIRS = src include lib design docs data po

INTLTOOL_FILES = intltool-extract.in \
                 intltool-merge.in \
                 intltool-update.in

EXTRA_DIST = BUGS NOTES README ChangeLog ChangeLog-1.0 AUTHORS \
	COPYING INSTALL autogen.sh ToDos \
	icon-theme-installer $(INTLTOOL_FILES)

distclean-local:
	-rm -rf autom4te*.cache

maintainer-clean-local:
	-rm -rf autom4te*.cache

MOSTLYCLEANFILES = *.log core FILE *~ #*#
CLEANFILES = *.log core FILE *~ #*#
DISTCLEANFILES = *.log core FILE *~ prototype.bak \
                 intltool-extract \
                 intltool-merge \
                 intltool-update \
                 po/.intltool-merge-cache
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in configure \
                       config.status aclocal.m4 \
                       $(INTLTOOL_FILES) #*#

DISTCHECK_CONFIGURE_FLAGS = --disable-update-desktop-database
