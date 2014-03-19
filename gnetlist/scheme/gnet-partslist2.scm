; Copyright (C) 2001-2010 MIYAMOTO Takanori
; gnet-partslist2.scm
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

(define (partslist2:write-top-header)
  (display ".START\n")
  (display "..refdes\tdevice\tvalue\tfootprint\tquantity\n"))

(define (partslist2:write-partslist ls)
  (if (null? ls)
      '()
      (begin (write-one-row (append (car ls) (list 1)) "\t" "\n")
             (partslist2:write-partslist (cdr ls)))))

(define (partslist2:write-bottom-footer)
  (display ".END")
  (newline))

(define (partslist2 output-filename)
  (set-current-output-port (gnetlist:output-port output-filename))
  (let ((parts-table (marge-sort-with-multikey (get-parts-table packages) '(1 2 3 0))))
    (partslist2:write-top-header)
    (partslist2:write-partslist parts-table)
    (partslist2:write-bottom-footer))
  (close-output-port (current-output-port)))
