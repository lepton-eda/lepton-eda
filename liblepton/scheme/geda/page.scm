;;; Deprecated module.

(define-module (geda page)
  #:use-module (geda deprecated)
  #:use-module (lepton page)

  #:re-export (object-page
               page?
               active-pages
               make-page
               close-page!
               page-filename
               set-page-filename!
               page-contents
               page-dirty?
               page->string
               string->page
               page-append!
               page-remove!
               set-page-dirty!))

(deprecated-module-log-warning! "(lepton page)")
