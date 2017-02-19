;; gEDA - GPL Electronic Design Automation
;; libgeda - gEDA's library - Scheme API
;; Copyright (C) 2011-2012 Peter Brett <peter@peter-b.co.uk>
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
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
;;

(define-module (geda config)

  ; Import C procedures
  #:use-module (geda core smob)
  #:use-module (geda core config))

(define-public config? %config?)
(define-public default-config-context %default-config-context)
(define-public system-config-context %system-config-context)
(define-public user-config-context %user-config-context)
(define-public path-config-context %path-config-context)
(define-public config-filename %config-filename)
(define-public config-load! %config-load!)
(define-public config-loaded? %config-loaded?)
(define-public config-save! %config-save!)
(define-public config-changed? %config-changed?)
(define-public config-parent %config-parent)
(define-public set-config-parent! %set-config-parent!)
(define-public config-trusted? %config-trusted?)
(define-public set-config-trusted! %set-config-trusted!)

(define-public (config-trusted-context cfg)
  (cond
   ((not cfg) #f)
   ((config-trusted? cfg) cfg)
   (else (config-trusted-context (config-parent cfg)))))

(define-public config-groups %config-groups)
(define-public config-has-group? %config-has-group?)
(define-public config-keys %config-keys)

(define-public (config-has-key? cfg group key)
  (false-if-exception
   (begin (config-source cfg group key)
          #t)))

(define-public (config-inherited? cfg group key)
  (not (equal? cfg (config-source cfg group key))))

(define-public config-source %config-source)
(define-public config-string %config-string)
(define-public config-boolean %config-boolean)
(define-public config-int %config-int)
(define-public config-real %config-real)
(define-public config-string-list %config-string-list)
(define-public config-boolean-list %config-boolean-list)
(define-public config-int-list %config-int-list)
(define-public config-real-list %config-real-list)
(define-public set-config! %set-config!)
(define-public add-config-event! %add-config-event!)
(define-public remove-config-event! %remove-config-event!)
