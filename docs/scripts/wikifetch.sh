#!/bin/bash

#
# Retrieve wiki pages as html pages, creating a local tree of documents
# mirroring the original wiki structure. Wiki URLs are converted to local
# directories/filenames based on namespace/wiki-page naming conventions.
#
# Check calling arguments.

# website to fetch
site=http://geda.seul.org/wiki

if [ ! -d ../wip ]
then
	mkdir ../wip
fi

if [ ! -d ../wip ]
then
	echo "cannot create wip directory"
	exit 1
fi

# 
pavuk -logfile ../wip/pavuk_logfile.txt \
      -slogfile ../wip/pavuk_slogfile.txt \
      -cdir ../wip \
      -dont_leave_site \
      -noRobots \
      -index_name "index.html" \
      -httpad "+X_DOKUWIKI_DO: export_xhtml" \
      -cookie_file ../wip/cookies.txt \
      -cookie_send \
      -url_pattern http://geda.seul.org/wiki/\* \
      -skip_rpattern "(.*\?do=(_export|diff|revisions|backlink|index|export_.*))|_export|feed\.php.*" \
      -tr_chr_chr "?&*:" _ \
      -post_update \
      -sleep 1 \
      -fnrules F "*" "%h/%d/%b%E" "$site"

