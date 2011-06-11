
/* This file contains preprocessor macros which provide substitutes
 * for missing functions or other definitions based on the results of
 * configure. */

/* We need to be able to pass UTF-8 strings to and from libguile.  The
 * most forward-compatible way to do this is to explicitly use the
 * "utf8" API for doing so, but this API is only available from Guile
 * 2.0 onwards.
 *
 * In Guile 2.0 there is a similar "locale" API which encodes/decodes
 * strings differently based on the locale, so we need to avoid it in
 * case the user decides to set a non-UTF-8 locale.  However, the
 * "locale" API *is* present in Guile 1.8, in which it doesn't attempt
 * to encode/decode the strings its passed, so we can use it as a
 * direct replacement for the "utf8" API.
 *
 * Confused yet?
 */

#ifndef HAVE_SCM_FROM_UTF8_STRINGN
#  define scm_from_utf8_stringn scm_from_locale_stringn
#endif
#ifndef HAVE_SCM_FROM_UTF8_STRING
#  define scm_from_utf8_string(x) scm_from_utf8_stringn ((x), -1)
#endif
#ifndef HAVE_SCM_TO_UTF8_STRINGN
#  define scm_to_utf8_stringn scm_to_locale_stringn
#endif
#ifndef HAVE_SCM_TO_UTF8_STRING
#  define scm_to_utf8_string(x) scm_to_utf8_stringn ((x), NULL)
#endif

#ifndef HAVE_SCM_FROM_UTF8_SYMBOLN
#  define scm_from_utf8_symboln scm_from_locale_symboln
#endif
#ifndef HAVE_SCM_FROM_UTF8_SYMBOL
#  define scm_from_utf8_symbol scm_from_locale_symbol
#endif
