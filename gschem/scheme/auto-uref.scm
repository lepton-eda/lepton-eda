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

;; Two level associative list - page at first level, refdes prefix at second
(define page-prefix-list '())

;; Modify attributes of an object to assign next unused refdes value
(define (auto-uref attribs)

  ; Map of refdes prefix and next available number for current page
  (define refdes-map
    (let ((old (assoc-ref page-prefix-list (get-current-page))))
      (if old old '())))

  ; Retrieve next available number for given refdes prefix
  ; Update refdes-map to track used refdeses
  (define (get-next-uref prefix)
    (let* ((old (assoc-ref refdes-map prefix))
           (new (if old (1+ old) 1)))
      (set! refdes-map (assoc-set! refdes-map prefix new))
      new))
  
  ; Extract prefix from a refdes attribute value
  (define (get-prefix value)
    (let ((prefix (string-match "^[A-Z]*" value)))
      (if (= 0 (match:end prefix))
	  #f
	  (match:substring prefix))))

  ; Process object attributes
  (for-each 
    (lambda (attrib)
      (let* ((name-value (get-attribute-name-value attrib))
             (name (car name-value))
             (value (cdr name-value))
             (prefix (get-prefix value)))
        ; If get-prefix fails (returns #f) there is no ? in the string
        (if (and prefix
                 (string=? name "refdes")
                 (not (attrib-inherited? attrib)))
          (set-attribute-value! attrib
                                (string-append
                                  prefix
                                  (number->string
                                    (get-next-uref prefix)))))))
    attribs)

  ; Update global map with modified map for current page
  (set! page-prefix-list (assoc-set! page-prefix-list
                                     (get-current-page)
                                     refdes-map)))


;; Scan for existing refdeses in the page and initialise page-prefix-list
(define (auto-uref-init-page page)

  ; Return (prefix . number) on match or #f on failure
  (define (split-attr value)
    (let ((match (string-match "^([A-Z]+)([0-9]+)$" value)))
      (if match
        (cons (match:substring match 1)
              (string->number (match:substring match 2)))
        #f)))

  ; Update refdes map with given prefix-num pair
  (define (update-refdes-map prefix-num)
    (let* ((prefix (car prefix-num))
           (value (cdr prefix-num))
           (old-value (assoc-ref refdes-map prefix))
           (new-value (if old-value (max old-value value) value)))
    (set! refdes-map (assoc-set! refdes-map prefix new-value))))

  ; Execute update for a single object
  (define (handle-object object)
    (let* ((all-attribs (get-object-attributes object))
           (own-attribs (filter (lambda (a)
                                  (not (attrib-inherited? a))) all-attribs))
           (name-vals (map get-attribute-name-value own-attribs))
           (refdeses (filter (lambda (a)
                               (string=? "refdes" (car a)))
                             name-vals))
           (prefix-pairs (filter-map (lambda (a)
                                       (split-attr (cdr a)))
                                     refdeses)))
      (for-each update-refdes-map prefix-pairs)))

  ; Clear refdes map for given page
  (define (refdes-map) '())

  ; Update refdes maps for objects in given page
  (for-each handle-object (get-objects-in-page page))

  ; Overwrite map for given page
  (set! page-prefix-list (assoc-set! page-prefix-list page refdes-map)))
