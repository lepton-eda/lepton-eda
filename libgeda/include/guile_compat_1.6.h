#if !HAVE_DECL_SCM_IS_TRUE
#define scm_is_true(x)    SCM_NFALSEP(x)
#endif

#if !HAVE_DECL_SCM_IS_FALSE
#define scm_is_false(x)   SCM_FALSEP(x)
#endif

#if !HAVE_DECL_SCM_IS_INTEGER
#define scm_is_integer(x) SCM_INUMP(x)
#endif

#if !HAVE_DECL_SCM_TO_INT
#define scm_to_int(x)     SCM_INUM(x)
#endif

#if !HAVE_DECL_SCM_FROM_INT
#define scm_from_int(x)   SCM_MAKINUM(x)
#endif

#if !HAVE_DECL_SCM_IS_STRING
#define scm_is_string(x) SCM_STRINGP(x)
#endif

#if !HAVE_DECL_SCM_TO_LOCALE_STRING
#define scm_to_locale_string(x)   strdup(SCM_STRING_CHARS(x))
#endif

#if !HAVE_DECL_SCM_FROM_LOCALE_STRING
#define scm_from_locale_string(x) scm_makfrom0str(x)
#endif

#if !HAVE_DECL_SCM_FROM_LOCALE_SYMBOL
#define scm_from_locale_symbol(x) \
  scm_string_to_symbol (scm_from_locale_string (x))
#endif

#if !HAVE_DECL_SCM_CAR
#define scm_car(x) SCM_CAR(x)
#endif

#if !HAVE_DECL_SCM_CDR
#define scm_cdr(x) SCM_CDR(x)
#endif

#if !HAVE_DECL_SCM_CADDR
#define scm_caddr(x) SCM_CADDR(x)
#endif

#if !HAVE_DECL_SCM_CADR
#define scm_cadr(x) SCM_CADR(x)
#endif

