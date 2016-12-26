(define-module (gnetlist schematic)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (gnetlist attrib compare)
  #:use-module (gnetlist traverse)
  #:use-module (gnetlist package)
  #:use-module (geda page)
  #:use-module (geda object)
  #:export (make-schematic schematic?
            schematic-id set-schematic-id!
            schematic-toplevel-pages set-schematic-toplevel-pages!
            schematic-tree set-schematic-tree!
            schematic-netlist set-schematic-netlist!
            schematic-non-unique-packages set-schematic-non-unique-packages!
            make-toplevel-schematic))

(define-record-type <schematic>
  (make-schematic id toplevel-pages tree netlist non-unique-packages)
  schematic?
  (id schematic-id set-schematic-id!)
  (toplevel-pages schematic-toplevel-pages set-schematic-toplevel-pages!)
  (tree schematic-tree set-schematic-tree!)
  (netlist schematic-netlist set-schematic-netlist!)
  (non-unique-packages schematic-non-unique-packages set-schematic-non-unique-packages!))

(set-record-type-printer!
 <schematic>
 (lambda (record port) (format port "#<geda-schematic ~A>" (schematic-id record))))


(define next-schematic-id #f)

;;; Closure to return a new schematic id.
(let ((id 0))
  (set! next-schematic-id
        (lambda () (set! id (1+ id)) (1- id))))


;;; For now, we just make a schematic SXML tree from the list of
;;; pages given on the command line.
(define (schematic->sxml netlist toplevel-pages)
  (define (page->sxml page)
    (define (source-page source)
      (let loop ((pages (active-pages)))
        (and (not (null? pages))
             (if (string=? source (basename (page-filename (car pages))))
                 (page->sxml (car pages))
                 (loop (cdr pages))))))

    ;; Given a list of strings, some of which may contain commas,
    ;; splits comma separated strings and returns the new combined
    ;; list
    (define (comma-separated->list ls)
      (append-map (lambda (s) (string-split s #\,)) ls))

    (define (composite->sxml p)
      (let ((sources (and=> (package-attributes p 'source)
                            comma-separated->list)))
        (if sources
            `(package ,p ,@(map source-page sources))
            `(package ,p))))

    (define (non-composite->sxml p)
      `(package ,p))

    (define (component->sxml p)
      (and (string=? (basename (page-filename (object-page (package-object p))))
                     (basename (page-filename page)))
           ((if (package-composite? p) composite->sxml non-composite->sxml) p)))

    `(page ,page ,@(filter-map component->sxml netlist)))


  `(*TOP* (package "TOPLEVEL"
                   ,@(map page->sxml toplevel-pages))))

;;; Gets non unique set of package refdeses.
;;; Backward compatibility procedure for old backends.
(define (non-unique-packages netlist)
  (sort (filter-map package-refdes netlist) refdes<?))


(define (make-toplevel-schematic toplevel-pages)
  "Creates a new schematic record based on TOPLEVEL-PAGES which
must be a list of pages."
  (let* ((id (next-schematic-id))
         (netlist (traverse))
         (tree (schematic->sxml netlist toplevel-pages))
         (nu-packages (non-unique-packages netlist)))
    (make-schematic id toplevel-pages tree netlist nu-packages)))
