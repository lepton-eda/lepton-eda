;;; gEDA - GNU Electronic Design Automation
;;; gnetlist - GNU Netlist
;;; Copyright (C) 1998 Ales V. Hvezda
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; Support functions

;; get all packages for a particular schematic page 
;; eventually placeholder will be either the hierarchical level or something 
;; of the sort
(define packages 
  (gnetlist:get-packages "placeholder"))

;; return a list of all unique the nets in the design
(define all-unique-nets
  (gnetlist:get-all-unique-nets "placeholder"))


;; return a list of all the nets in the design
;; Might return duplicates
(define all-nets
  (gnetlist:get-all-nets "placeholder"))

;;
;; Given a uref, returns the device attribute value (unknown if not defined)
;;
(define get-device
   (lambda (package)
      (gnetlist:get-package-attribute package "device")))

;; return all pins for a particular package 
(define pins
   (lambda (package)
      (gnetlist:get-pins package)))

;; not very useful, but amusing 
(define all-pins
   (map gnetlist:get-pins packages))

;; this is really crude, but I'm tired... :)
(define display-nl
   (lambda (list)
      (display list) 
      (newline)))


;; ah.. wonder what use this is...
(define display-pin
   (lambda (pin-list)
      (for-each display-nl pin-list)))


;; ha. I'm playing with scheme here.. don't mind me
(define display-all-pins
   (lambda ()
      (for-each display-pin all-pins)))


;; another misc function
(define print-packages
   (lambda (plist)
      (for-each display-nl plist)))

;; ETTUS
;; find-device
;; Usage:  (find-device packages devicename)
;; Returns the first package which matches the devicename
(define find-device
   (lambda (components devicename)
      (if (not (null? components))       
         (if (string=? devicename (get-device (car components)))
            (car components)
            (find-device (cdr components) devicename))))) 


;; ETTUS
;; find-devices
;; Usage:  (find-devices packages devicename '())
;; Returns a list of packages which match the device name
(define find-devices
   (lambda (components devicename list)
      (if (not (null? components))
         (if (string=? devicename (get-device (car components)))
            (find-devices (cdr components)
                                devicename
                                (cons (car components) list))
            (find-devices (cdr components)
                                devicename
                                list))
         list)))

;; ETTUS
;; strip1
;; Usage (strip1 list)
;; Cuts off the last element
(define strip1
  (lambda (ls)
    (if (or (null? ls)
            (null? (cdr ls)))
        '()        
        (cons (car ls) (strip1 (cdr ls))))))

;; ETTUS
;; contains?
;; Usage (contains? list item)
;; True if the list contains the item, according to string=?
(define contains?
   (lambda (ls item)
      (cond
         ((null? ls) #f)
         ((string=? item (car ls)) #t)
         (#t (contains? (cdr ls) item)))))


