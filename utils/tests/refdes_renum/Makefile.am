SUBDIRS= inputs outputs

AM_TESTS_ENVIRONMENT = PERL=${PERL} top_srcdir=${srcdir}/../..

check-local:
	$(AM_TESTS_ENVIRONMENT) srcdir=$(srcdir) $(SHELL) $(srcdir)/run_tests.sh

EXTRA_DIST = run_tests.sh tests.list README.txt
