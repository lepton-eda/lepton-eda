(define-module (gnetlist traverse)

  ; Import C procedures and variables
  #:use-module (gnetlist core traverse)

  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (geda attrib)
  #:use-module (geda object)
  #:use-module (gnetlist config)
  #:use-module (gnetlist package)
  #:use-module (gnetlist package-pin)
  #:use-module (gnetlist pin-net)
  #:use-module (gnetlist verbose)
  #:export (traverse))

;;; Temporary function to deal with gnetlist's net->connected_to
;;; entries.
(define (split-string-by-char s ch)
  (let ((space-pos (string-index s ch)))
    (and space-pos
         (cons (string-take s space-pos)
               (string-drop s (1+ space-pos))))))

(define (list->nets ls)
  (define package car)
  (define pinnumber cdr)
  (define (list->net ls)
    (match ls
      ((-1 . rest)
       #f)
     ((id priority name connection)
      (let ((conn-pair (if connection
                           (split-string-by-char connection #\space)
                           '(#f . #f))))
        (make-pin-net id
                      priority
                      name
                      (package conn-pair)
                      (pinnumber conn-pair))))
     (_ #f)))
  (filter-map list->net ls))

(define (list->pins ls)
  (define (make-pin-attrib-list object)
    (define (add-attrib attrib)
      (cons (string->symbol (attrib-name attrib))
            (attrib-value attrib)))

    (map add-attrib (object-attribs object)))

  (define (list->pin ls)
    (match ls
      ((#f . rest)
       #f)
      ((object number name label nets)
       (let ((attribs (make-pin-attrib-list object)))
         (make-package-pin (object-id object)
                           object
                           'net-pin
                           number
                           name
                           label
                           attribs
                           (list->nets nets))))
      (_ #f)))
  (filter-map list->pin ls))


(define (list->packages ls)
  ;; Makes attribute list of OBJECT using getter GET-ATTRIBS.
  (define (make-attrib-list get-attribs object)
    (define (add-attrib ls attrib)
      (let* ((name (string->symbol (attrib-name attrib)))
             (prev-value (assq-ref ls name))
             (new-value (attrib-value attrib)))
        (if prev-value
            (assq-set! ls name (cons new-value prev-value))
            (acons name (list new-value) ls))))

    (let loop ((in (get-attribs object))
               (out '()))
      (if (null? in)
          out
          (loop (cdr in)
                (add-attrib out (car in))))))

  (define (list->package ls)
    (match ls
      ((refdes tag composite #f pins)
       #f)
      ((refdes tag composite object pins)
       (make-package (object-id object)
                     refdes
                     tag
                     composite
                     object
                     (make-attrib-list inherited-attribs object)
                     (make-attrib-list object-attribs object)
                     (list->pins pins)))
      (_ #f)))
  (filter-map list->package ls))


(define (remove-refdes-mangling netlist)
  ;; Separator is yet always '/'
  (define (base-refdes refdes legend)
    (and refdes
         (verbose-print legend)
         (if (gnetlist-config-ref 'reverse-refdes-order)
             (let ((pos (string-index refdes #\/)))
               (if pos
                   (string-take refdes pos)
                   refdes))
             (let ((pos (string-rindex refdes #\/)))
               (if pos
                   (string-drop refdes (1+ pos))
                   refdes)))))

  (define (fix-net-connections net)
    (set-pin-net-connection-package! net
                                     (base-refdes (pin-net-connection-package net)
                                                  "U")))

  (define (fix-pin-connections pin)
    (for-each fix-net-connections (package-pin-nets pin)))

  (define (fix-package package)
    (set-package-refdes! package
                         (base-refdes (package-refdes package) "u"))
    (for-each fix-pin-connections (package-pins package))
    package)

  (verbose-print "- Removing refdes mangling:\n")
  (for-each fix-package netlist)
  (verbose-done)
  netlist)


(define (traverse netlist-mode)
  (let ((cwd (getcwd))
        (netlist (list->packages (%traverse netlist-mode))))
    ;; Change back to the directory where we started.  This is
    ;; done because (%traverse) can change the current working
    ;; directory.
    (chdir cwd)
    (if (gnetlist-config-ref 'mangle-refdes)
        netlist
        (remove-refdes-mangling netlist))))
