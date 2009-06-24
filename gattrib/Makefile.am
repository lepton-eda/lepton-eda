
AM_CFLAGS = -g -O2 

SUBDIRS = po src include lib design docs data

EXTRA_DIST = BUGS NOTES README ChangeLog ChangeLog-1.0 AUTHORS \
	COPYING INSTALL autogen.sh ToDos \
	icon-theme-installer desktop-i18n

distclean-local:
	-rm -rf autom4te*.cache

maintainer-clean-local:
	-rm -rf autom4te*.cache

MOSTLYCLEANFILES = *.log core FILE *~ #*#
CLEANFILES = *.log core FILE *~ #*#
DISTCLEANFILES = *.log core FILE *~ prototype.bak
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in configure \
                       config.status aclocal.m4 #*#

DISTCHECK_CONFIGURE_FLAGS = --disable-update-desktop-database

ACLOCAL_AMFLAGS = -I m4
