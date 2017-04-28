(define-module (symbol check path)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (geda object)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)

  #:export (path-info
            check-path))

(define (path-info path)
  "Returns info on all elements of PATH."
  (let ((len (path-length path)))
    (let loop ((i 0)
               (info '()))
      (if (< i len)
          (loop (1+ i) (cons (path-ref path i) info))
          (reverse info)))))


(define (check-path object)
  "Checks path OBJECT.
  * Checks if it has zero length elements."
  (fold
   (lambda (next prev)
     (when
         (match `(,@prev ,@next)
           ;; Next line or move to the same coord C.
           (((or 'moveto 'lineto) c (or 'moveto 'lineto) c) #t)
           ;; The same if the previous element was a curve.
           (('curveto c1 c2 c (or 'moveto 'lineto) c) #t)
           ;; All points of the next curve have the same coord C
           ;; as for the previous line or move.
           (((or 'moveto 'lineto) c 'curveto c c c) #t)
           ;; The same for two curves.
           (('curveto c1 c2 c 'curveto c c c) #t)
           (_ #f))
       (blame-object object
                     'error
                     (format #f
                             (_ "Zero length path element: ~A")
                             next)))
     next)
   '()
   (path-info object)))
