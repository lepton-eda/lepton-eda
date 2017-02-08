(define-module (gnetlist pin-net)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (make-pin-net pin-net?
            pin-net-id set-pin-net-id!
            pin-net-priority set-pin-net-priority!
            pin-net-name set-pin-net-name!
            pin-net-connection-package set-pin-net-connection-package!
            pin-net-connection-pinnumber set-pin-net-connection-pinnumber!))

(define-record-type <pin-net>
  (make-pin-net id priority name connection-package connection-pinnumber)
  pin-net?
  (id pin-net-id set-pin-net-id!)
  (priority pin-net-priority set-pin-net-priority!)
  (name pin-net-name set-pin-net-name!)
  (connection-package pin-net-connection-package set-pin-net-connection-package!)
  (connection-pinnumber pin-net-connection-pinnumber set-pin-net-connection-pinnumber!))

(set-record-type-printer!
 <pin-net>
 (lambda (record port) (format port "#<pin-net ~A>" (pin-net-id record))))
