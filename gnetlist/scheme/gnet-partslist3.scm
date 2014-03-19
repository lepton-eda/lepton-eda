; Copyright (C) 2001-2010 MIYAMOTO Takanori
; gnet-partslist3.scm
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(load-from-path "partslist-common.scm")

(define (partslist3:write-top-header)
  (display ".START\n")
  (display "..device\tvalue\tfootprint\tquantity\trefdes\n"))

(define (partslist3:write-partslist ls)
  (if (null? ls)
      '()
      (begin (write-one-row (cdar ls) "\t" "\t")
             (write-one-row (caar ls) " " "\n")
             (partslist3:write-partslist (cdr ls)))))

(define (partslist3:write-bottom-footer)
  (display ".END")
  (newline))

(define (count-same-parts ls)
  (if (null? ls)
      (append ls)
      (let* ((parts-table-no-uref (let ((result '()))
                                    (for-each (lambda (l) (set! result (cons (cdr l) result))) (reverse ls))
                                    (append result)))
             (first-ls (car parts-table-no-uref))
             (match-length (length (member first-ls (reverse parts-table-no-uref))))
             (rest-ls (list-tail ls match-length))
             (match-ls (list-tail (reverse ls) (- (length ls) match-length)))
             (uref-ls (let ((result '()))
                        (for-each (lambda (l) (set! result (cons (car l) result))) match-ls)
                        (append result))))
        (cons (cons uref-ls (append first-ls  (list match-length))) (count-same-parts rest-ls)))))

(define (partslist3 output-filename)
  (set-current-output-port (gnetlist:output-port output-filename))
  (let ((parts-table (marge-sort-with-multikey (get-parts-table packages) '(1 2 3 0))))
    (set! parts-table (count-same-parts parts-table))
    (partslist3:write-top-header)
    (partslist3:write-partslist parts-table)
    (partslist3:write-bottom-footer))
  (close-output-port (current-output-port)))
