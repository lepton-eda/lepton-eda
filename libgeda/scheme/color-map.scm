;                                                         -*-Scheme-*-

;; Map between color index numbers and symbolic color names
(define %color-name-map
  '((background . 0)
    (pin . 1)
    (net-endpoint . 2)
    (graphic . 3)
    (net . 4)
    (attribute . 5)
    (logic-bubble . 6)
    (dots-grid . 7)
    (detached-attribute . 8)
    (text . 9)
    (bus . 10)
    (select . 11)
    (bounding-box . 12)
    (zoom-box . 13)
    (stroke . 14)
    (lock . 15)
    (output-background . 16)
    (freestyle1 . 17)
    (freestyle2 . 18)
    (freestyle3 . 19)
    (freestyle4 . 20)
    (junction . 21)
   ))

;; Look up the internal system ID for a symbolic color
(define (color-map-name-to-index val)
  (if (symbol? val)
      (apply + (map (lambda (x)
                      (if (eqv? (car x) val) (cdr x) 0))
                    %color-name-map))
      val))


;; Look up the symbolic color for an internal system ID
(define (color-map-name-from-index idx)
  (define (impl lst idx)
    (if (null? lst)
        idx ;; Fall back to the index if no symbol found
        (let ((entry (car lst)))
          (if (eq? idx (cdr entry))
              (car entry)
              (impl (cdr lst) idx)))))
  (impl %color-name-map idx))

;; Convert a color map to use system IDs
(define (color-map-to-symbolic colormap)
  (map (lambda (entry)
         (list (color-map-name-from-index (car entry))
               (cadr entry)))
       colormap))

;; Convert a color map to use symbolic color names
(define (color-map-from-symbolic colormap)
  (map (lambda (entry)
         (list (color-map-name-to-index (car entry))
               (cadr entry)))
       colormap))

;; Given a color map function (e.g. print-color-map), return an
;; equivalent function that uses symbolic colors.
(define (color-map-friendlier-function map-function)
  (lambda rest
    (if (null? rest)
        (color-map-to-symbolic (map-function))
        (map-function (apply color-map-from-symbolic rest)))))

;; Converts a standard color map function into one which understands
;; symbolic colors
(define-macro (color-map-make-friendly func)
  `(define ,func (color-map-friendlier-function ,func)))
