;; Printing out current key bindings for gEDA (gschem)
; Stefan Petersen 1999-04-04 (spe@stacken.kth.se)
; Free for all use. Just don't blame me when your house burns up.

(define (print-mapped-keys mapped-keys)
  (display (car mapped-keys))
  (display " = ")
  (for-each (lambda (key)
	      (cond ((not (null? key))
		     (display key)
		     (display " "))))
	    (cdr mapped-keys))
  (newline))

(define (mapping-keys keymap keys)
  (for-each (lambda (mapped-key) ; Receives a pair
	      (let ((action (eval (cdr mapped-key))))
		(cond ((list? action)
		       (mapping-keys action (append keys (car mapped-key))))
		      (else
		       (print-mapped-keys (list 
					   (cdr mapped-key)
					   keys 
					   (car mapped-key)))))))
	    keymap))

; You need call this after you call any rc file function
(gschem-use-rc-values)

; filename is specified on the command line
(mapping-keys global-keymap '())

;(gschem-exit)
