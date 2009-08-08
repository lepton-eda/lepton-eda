
SUBDIRS= inputs outputs

TESTS_ENVIRONMENT= PERL=${PERL} top_srcdir=${srcdir}/../..

RUN_TESTS= run_tests.sh

check_SCRIPTS= ${RUN_TESTS}

TESTS= ${RUN_TESTS}

EXTRA_DIST= ${RUN_TESTS} tests.list README.txt

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in
