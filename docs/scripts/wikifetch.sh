#!/bin/bash
#
# Retrieve wiki pages as html pages, creating a local tree of documents
# mirroring the original wiki structure. Wiki URLs are converted to local
# directories/filenames based on namespace/wiki-page naming conventions.
#
# Check calling arguments.
if [ $# -ne 1 ] ; then
  echo " "
  echo " Error: Incorrect arguments"
  echo " "
  echo "   Usage:  sh pavuk.sh {wiki's URL}"
  echo "   where: (wiki's URL}       -- The URL of the wiki's start page"
  echo "                                (e.g., http://geda.seul.org/wiki)."
  echo " "
  exit 1
fi

# 
pavuk -logfile /tmp/pavuk/pavuk_logfile.txt \
      -slogfile /tmp/pavuk/pavuk_slogfile.txt \
      -cdir /tmp/pavuk \
      -dont_leave_site \
      -noRobots \
      -index_name "index.html" \
      -httpad "+X_DOKUWIKI_DO: export_xhtml" \
      -cookie_file /tmp/pavuk/cookies.txt \
      -cookie_send \
      -url_pattern http://geda.seul.org/wiki/\* \
      -skip_rpattern "(.*\?do=(diff|revisions|backlink|index|export_.*))|feed\.php.*" \
      -tr_chr_chr "?&*:" _ \
      -post_update \
      -fnrules F "*" "%h/%d/%b%E" "$1"

