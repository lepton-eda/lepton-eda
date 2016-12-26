(define-module (gnetlist package-pin)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (make-package-pin package-pin?
            package-pin-id set-package-pin-id!
            package-pin-object set-package-pin-object!
            package-pin-type set-package-pin-type!
            package-pin-number set-package-pin-number!
            package-pin-name set-package-pin-name!
            package-pin-label set-package-pin-label!
            package-pin-attribs set-package-pin-attribs!
            package-pin-nets set-package-pin-nets!))

(define-record-type <package-pin>
  (make-package-pin id object type number name label attribs nets)
  package-pin?
  (id package-pin-id set-package-pin-id!)
  (object package-pin-object set-package-pin-object!)
  (type package-pin-type set-package-pin-type!)
  (number package-pin-number set-package-pin-number!)
  (name package-pin-name set-package-pin-name!)
  (label package-pin-label set-package-pin-label!)
  (attribs package-pin-attribs set-package-pin-attribs!)
  (nets package-pin-nets set-package-pin-nets!))

(set-record-type-printer!
 <package-pin>
 (lambda (record port) (format port "#<geda-package-pin ~A>" (package-pin-id record))))
