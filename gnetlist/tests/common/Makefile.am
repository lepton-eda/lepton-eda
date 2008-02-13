## $Id$
##

SUBDIRS= inputs outputs

TESTS_ENVIRONMENT= PERL=${PERL}

RUN_TESTS= run_tests.sh

check_SCRIPTS= ${RUN_TESTS}

TESTS= ${RUN_TESTS}

EXTRA_DIST= run_backend_tests.sh ${RUN_TESTS} tests.list backends.list always-copy.list
CLEANFILES= *.log
