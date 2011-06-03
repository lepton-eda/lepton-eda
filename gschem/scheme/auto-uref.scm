;; gEDA - GPL Electronic Design Automation
;; gschem - gEDA Schematic Capture
;; Copyright (C) 1998-2010 Ales Hvezda
;; Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(use-modules (ice-9 regex) (srfi srfi-1))

;; Modify attributes of an object to assign next unused refdes value
(define (auto-uref attribs)

  ; Return (prefix . number) on match or #f on failure
  (define (split-value value)
    (let ((match (string-match "^([A-Za-z]+)([0-9]+)$" value)))
      (if match
        (cons (match:substring match 1)
              (string->number (match:substring match 2)))
        #f)))

  ; Extract prefix from a refdes attribute value
  (define (get-prefix value)
    (let ((prefix (string-match "^[A-Za-z]+" value)))
      (if (= 0 (match:end prefix))
	  #f
	  (match:substring prefix))))

  ; Filter non-inherited refdes values
  (define (refdes-attrs attribs)
    (filter (lambda (a)
              (and
                (not (attrib-inherited? a))
                (string=? "refdes" (car (get-attribute-name-value a)))))
            attribs))

  ; Extract numbers from refdeses that have given prefix
  (define (extract-numbers object prefix)
    (let* ((refdeses (refdes-attrs (get-object-attributes object)))
           (vals (map (lambda (a)
                        (cdr (get-attribute-name-value a)))
                      refdeses))
           (prefix-numbers (filter-map split-value vals))
           (numbers (filter-map (lambda (n.v)
                                  (if (string=? prefix (car n.v))
                                      (cdr n.v)
                                      #f))
                                prefix-numbers)))
      numbers))

  ; Collect all numbers associated with prefix on current page
  (define (collect-all-numbers prefix)
    (let ((objects (get-objects-in-page (get-current-page))))
      (concatenate (map (lambda (o)
                          (extract-numbers o prefix))
                        objects))))

  ; Return first number not present in used greater or equal to minimum
  (define (find-first-unused used minimum)
    (define (go n xs)
      (cond ((null? xs) n)
            ((< n (car xs)) n)
            ((= n (car xs)) (go (1+ n) (cdr xs)))
            (else (go n (cdr xs)))))
    (go minimum used))

  ; Do the work - first check if attributes contain refdes with prefix
  (let* ((refdeses (refdes-attrs attribs))
         (refdes (if (null? refdeses)
                     #f
                     (car refdeses)))
         (prefix (if refdes
                     (get-prefix (cdr (get-attribute-name-value refdes)))
                     #f)))
    (if prefix
        (let* ((used-nums (sort-list (collect-all-numbers prefix) <))
               (next-num (find-first-unused used-nums 1)))
          ;(simple-format #t "~A: ~A -> ~A~%"
          ;                  prefix
          ;                  used-nums
          ;                  next-num)
          (set-attribute-value!
            refdes
            (string-append prefix (number->string next-num)))))))

