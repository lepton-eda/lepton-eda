; -*-Scheme-*-
(define (build-path first . rest)
  (if (null? rest) first
      (apply build-path 
	     (append (list (string-append first path-sep (car rest))) 
		     (cdr rest)))))
