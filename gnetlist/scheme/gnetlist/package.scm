(define-module (gnetlist package)
  #:use-module (srfi srfi-9)
  #:use-module (gnetlist traverse)
  #:export (make-package package?
            package-id set-package-id!
            package-refdes set-package-refdes!
            package-tag set-package-tag!
            package-composite? set-package-composite!
            package-object set-package-object!
            package-attribs set-package-attribs!
            package-pins set-package-pins!))

(define-record-type <package>
  (make-package id refdes tag composite object attribs pins)
  package?
  (id package-id set-package-id!)
  (refdes package-refdes set-package-refdes!)
  (tag package-tag set-package-tag!)
  (composite package-composite? set-package-composite!)
  (object package-object set-package-object!)
  (attribs package-attribs set-package-attribs!)
  (pins package-pins set-package-pins!))
