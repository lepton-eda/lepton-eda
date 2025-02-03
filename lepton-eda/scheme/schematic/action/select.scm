;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2025-2026 Lepton EDA Contributors
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not write to the Free Software
;;; Foundation Inc. 51 Franklin Street Fifth Floor Boston MA 02110-1301 USA.


(define-module (schematic action select)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (system foreign)

  #:use-module (lepton attrib)
  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi check-args)
  #:use-module (lepton ffi glib)
  #:use-module (lepton ffi)
  #:use-module (lepton object foreign)

  #:use-module (schematic action-mode)
  #:use-module (schematic ffi)
  #:use-module (schematic window foreign)

  #:export (continue-box-selection
            continue-selection
            find-object
            finish-box-selection
            finish-selection
            start-box-selection
            start-selection))


;;; Definitions from schematic_defines.h
(define SINGLE 0)
(define MULTIPLE 1)


;;; Select all nets connected to *NET in *WINDOW.  Depending on
;;; the state of the net selection mode of the window and
;;; selection state of the current net this function will either
;;; select the single net, all directly connected nets or all nets
;;; connected with netname attribute.
(define (select-connected-nets *window *net)
  (define *active-page (schematic_window_get_active_page *window))
  (define *objects
    (glist->list (lepton_page_objects *active-page) identity))

  (define (netname-value *object)
    (let ((netname-attribs
           (filter (lambda (o) (string= (attrib-name o) "netname"))
                   (object-attribs (pointer->object *object)))))
      (and (not (null? netname-attribs))
           (attrib-value (car netname-attribs)))))

  ;; Get all the nets of the stacked netnames.
  (define (netname-stack->net-stack *netname-stack)
    (let loop ((ls *objects)
               (*net-stack %null-pointer))
      (if (null? ls)
          *net-stack
          (let ((*object (car ls)))
            (loop (cdr ls)
                  (let ((*attachment (lepton_object_get_attached_to *object)))
                    (if (and (true? (lepton_object_is_text *object))
                             (not (null-pointer? *attachment))
                             (true? (lepton_object_is_net *attachment)))
                        (let ((netname (netname-value *attachment)))
                          (if netname
                              (let ((netname-ls (glist->list *netname-stack pointer->string)))
                                (if (member netname netname-ls)
                                    (g_list_prepend *net-stack *attachment)
                                    *net-stack))
                              *net-stack))
                        *net-stack)))))))
  (define (select-next-nets *net-stack
                            *netname-stack
                            net-selection-state
                            count
                            netname-list)
    (let* ((*new-netname-stack
            (o_select_connected_nets *window
                                     *net-stack
                                     *netname-stack
                                     net-selection-state
                                     count))
           (new-netname-list (glist->list *new-netname-stack
                                          pointer->string)))
      (if (equal? netname-list new-netname-list)
          ;; No new netnames in the stack, free it and exit.
          (schematic_selection_free_netname_stack *new-netname-stack)
          (let ((*new-net-stack
                 (netname-stack->net-stack *new-netname-stack)))
            (select-next-nets *new-net-stack
                              *new-netname-stack
                              net-selection-state
                              1
                              new-netname-list)))))

  ;; If either Shift or Ctrl are pressed, behave exactly the same
  ;; as a single object selection.  This makes it possible to
  ;; <mouse-1> on a net segment to select it and then
  ;; Shift+<mouse-1> on it to deselect it.
  (if (or (true? (schematic_window_get_shift_key_pressed *window))
          (true? (schematic_window_get_control_key_pressed *window)))
      (o_select_object *window *net SINGLE 0)
      (let ((net-selection-state
             (schematic_window_get_net_selection_state *window)))
        (unless (true? (lepton_object_get_selected *net))
          (schematic_window_set_net_selection_state *window 1))
        ;; The current net is the startpoint for the net stack.
        (let ((*netstack (g_list_prepend %null-pointer *net)))
          (select-next-nets *netstack
                            %null-pointer
                            net-selection-state
                            0
                            '()))
        (let ((net-selection-mode
               (schematic_window_get_net_selection_mode *window)))
          (schematic_window_set_net_selection_state *window
                                                    (1+ net-selection-state))
          (when (> (schematic_window_get_net_selection_state *window)
                   net-selection-mode)
            (schematic_window_set_net_selection_state *window 1))))))


;;; Test if *OBJECT in *WINDOW was hit at the given world
;;; coordinates (X . Y) taking into account the given pixel slack
;;; SLACK.  To be hit, the object has to be selectable and visible
;;; as well.  If it is either not around that coordinate, is not
;;; selectable (locked), or invisible and not being rendered, this
;;; function will return #f.  If the object was hit, it returns
;;; #t.
(define (is-hit? *window *object x y slack)
  (define (bv->int bv)
    (bytevector-sint-ref bv 0 (native-endianness) (sizeof int)))

  (define show_hidden_text
    (schematic_window_get_show_hidden_text *window))

  (and (true? (lepton_object_get_selectable *object))
       (or (false? (lepton_object_is_text *object))
           (true? (lepton_text_object_is_visible *object))
           ;; We can't hit invisible (text) objects unless
           ;; show_hidden_text is active.
           (true? show_hidden_text))

       ;; Do a coarse test first to avoid computing distances for
       ;; objects ouside of the hit range.
       (let ((xmin-bv (make-bytevector (sizeof int) 0))
             (ymin-bv (make-bytevector (sizeof int) 0))
             (xmax-bv (make-bytevector (sizeof int) 0))
             (ymax-bv (make-bytevector (sizeof int) 0)))

         ;; First off, we have to be able to get bounds of the
         ;; object.
         (and (true? (lepton_object_calculate_visible_bounds
                      *object
                      show_hidden_text
                      (bytevector->pointer xmin-bv)
                      (bytevector->pointer ymin-bv)
                      (bytevector->pointer xmax-bv)
                      (bytevector->pointer ymax-bv)))

              (let ((xmin (bv->int xmin-bv))
                    (ymin (bv->int ymin-bv))
                    (xmax (bv->int xmax-bv))
                    (ymax (bv->int ymax-bv)))
                ;; And the position has to be inside the object
                ;; rectangle bounds or at least closer to it than
                ;; the given slack.
                (and (<= (- xmin slack) x (+ xmax slack))
                     (<= (- ymin slack) y (+ ymax slack))))))
       ;; And eventually, an object may be a circle, and the
       ;; corners of its bounding box may be a bit far from its
       ;; circumference to be selected.  Let's check this.
       (< (lepton_object_shortest_distance *object
                                           x
                                           y
                                           show_hidden_text)
          slack)))


;;; Test if *OBJECT in *WINDOW was hit at the given coordinates (X
;;; . Y) taken into account the permissible pixel slack SLACK.
;;; If so, the function changes selection as appropriate for the
;;; found object and saves a pointer to it so future find
;;; operations resume after this object.  Return TRUE if the
;;; LeptonObject was hit, otherwise FALSE.
(define (find-single-object *window *object x y slack)
  (if (not (is-hit? *window *object x y slack))
      FALSE

      (begin
        (if (and (true? (lepton_object_is_net *object))
                 (true? (schematic_window_get_net_selection_mode *window)))
            (select-connected-nets *window *object)
            ;; 0 is count.
            (o_select_object *window *object SINGLE 0))

        (schematic_window_set_object_lastplace *window *object)
        (i_update_menus *window)
        TRUE)))


(define (find-object *window x y)
  "Find and select an object in *WINDOW at the given world
coordinates (X . Y) taking into account the number of slack pixels
around the object defining the area in which it still can be
selected,and update the page selection.  Find operations resume
searching after the last object which was found before, so
multiple find operations at the same point will cycle through any
objects on top of each other at this location.  The function
returns TRUE if the object was hit at the given coordinate,
otherwise FALSE."
  (define *canvas (schematic_window_get_current_canvas *window))

  ;; Rotate the object list LS starting from *OBJECT so its head
  ;; before *OBJECT is appended to its tail after *OBJECT and the
  ;; object is removed.  If *OBJECT is not in the list, return the
  ;; list as is.
  (define (remove-object/rotate-ls *object ls)
    (let ((index
           (list-index (lambda (x) (equal? x *object)) ls)))
      (if index
          (let-values (((head tail) (split-at ls index)))
            (append (cdr tail) head))
          ls)))

  (when (null-pointer? *canvas)
    (error "NULL canvas."))

  (let ((slack (schematic_canvas_WORLDabs
                *canvas
                (schematic_window_get_select_slack_pixels *window)))
        (*objects (lepton_page_objects
                   (schematic_window_get_active_page *window)))
        (*last-found-object
         (schematic_window_get_object_lastplace *window)))

    ;; Iterate over all objects starting at the last found object.
    ;; If there is more than one object around the (X . Y)
    ;; position, this will select the next object near the
    ;; position point.  You can change the selected object by
    ;; clicking at the same place multiple times.
    (let loop ((ls (remove-object/rotate-ls
                    *last-found-object
                    (glist->list *objects identity))))
      (if (null? ls)
          (begin
            ;; Didn't find anything.... reset lastplace.
            (schematic_window_set_object_lastplace *window
                                                   %null-pointer)
            ;; Deselect everything only if Shift key isn't pressed.
            (unless (true? (schematic_window_get_shift_key_pressed
                            *window))
              (o_select_unselect_all *window))

            (i_update_menus *window)
            FALSE)

          (if (true? (find-single-object *window (car ls) x y slack))
              TRUE
              (loop (cdr ls)))))))


;;; Invalidate the area of the box selection in WINDOW.
(define (invalidate-selection-box window)
  (define *window (check-window window 1))
  (define *canvas (schematic_window_get_current_canvas *window))

  (when (null-pointer? *canvas)
    (error "NULL canvas."))

  (schematic_canvas_invalidate_world_rect
   *canvas
   (schematic_window_get_first_wx *window)
   (schematic_window_get_first_wy *window)
   (schematic_window_get_second_wx *window)
   (schematic_window_get_second_wy *window)))


(define (start-box-selection window x y)
  "Start the process of box selection in WINDOW.  (X . Y) is the
current coordinate."
  (define *window (check-window window 1))
  (define *canvas (schematic_window_get_current_canvas *window))

  (check-integer x 2)
  (check-integer y 3)

  (when (null-pointer? *canvas)
    (error "NULL canvas."))

  ;; If we are still close to the button press location, then
  ;; don't enter the selection box mode.
  (let* ((diff-x (abs (- (schematic_window_get_first_wx *window) x)))
         (diff-y (abs (- (schematic_window_get_first_wy *window) y)))
         (dist (schematic_canvas_SCREENabs *canvas (max diff-x diff-y))))
    (when (>= dist 10)
      (schematic_window_set_second_wx *window x)
      (schematic_window_set_second_wy *window y)

      (set-action-mode! 'box-select-mode #:window window)
      (i_action_start *window))))


(define (continue-box-selection window x y)
  "Continue box selection in WINDOW.  (X . Y) is the current
coordinate."
  (define *window (check-window window 1))

  (check-action-state window)

  (when (true? (schematic_window_get_rubber_visible *window))
    (invalidate-selection-box window))

  (schematic_window_set_second_wx *window x)
  (schematic_window_set_second_wy *window y)

  (invalidate-selection-box window)
  (schematic_window_set_rubber_visible *window 1))


(define (search-visible-objects window)
  (define *window (check-window window 1))

  (define shift-key-pressed?
    (true? (schematic_window_get_shift_key_pressed *window)))
  (define control-key-pressed?
    (true? (schematic_window_get_control_key_pressed *window)))

  (define show_hidden_text
    (schematic_window_get_show_hidden_text *window))

  (define wx1 (schematic_window_get_first_wx *window))
  (define wy1 (schematic_window_get_first_wy *window))
  (define wx2 (schematic_window_get_second_wx *window))
  (define wy2 (schematic_window_get_second_wy *window))

  (define left (min wx1 wx2))
  (define right (max wx1 wx2))
  (define top (min wy1 wy2))
  (define bottom (max wy1 wy2))

  (define *active-page (schematic_window_get_active_page *window))
  (define *objects (lepton_page_objects *active-page))

  (define object-bv-left (make-bytevector (sizeof int) 0))
  (define object-bv-right (make-bytevector (sizeof int) 0))
  (define object-bv-top (make-bytevector (sizeof int) 0))
  (define object-bv-bottom (make-bytevector (sizeof int) 0))

  (define (test-object-bounds *object count)
    ;; Only select visible objects.
    (let ((visible-object?
           (or (false? (lepton_object_is_text *object))
               (true? (lepton_text_object_is_visible *object))
               (true? show_hidden_text))))
      (if (and visible-object?
               (true? (lepton_object_calculate_visible_bounds
                       *object
                       show_hidden_text
                       (bytevector->pointer object-bv-left)
                       (bytevector->pointer object-bv-top)
                       (bytevector->pointer object-bv-right)
                       (bytevector->pointer object-bv-bottom)))
               (>= (bytevector-sint-ref object-bv-left
                                        0
                                        (native-endianness)
                                        (sizeof int))
                   left)
               (<= (bytevector-sint-ref object-bv-right
                                        0
                                        (native-endianness)
                                        (sizeof int))
                   right)
               (>= (bytevector-sint-ref object-bv-top
                                        0
                                        (native-endianness)
                                        (sizeof int))
                   top)
               (<= (bytevector-sint-ref object-bv-bottom
                                        0
                                        (native-endianness)
                                        (sizeof int))
                   bottom))
          (begin
            (o_select_object *window *object MULTIPLE count)
            (1+ count))
          count)))

  (define (select-objects)
    (let loop ((*object-ls (glist->list *objects identity))
               ;; Object count.
               (count 0))
      (if (null? *object-ls)
          count
          (loop (cdr *object-ls)
                (test-object-bounds (car *object-ls) count)))))

  (let ((count (select-objects)))
    ;; If there were no objects to be found in select box, count
    ;; will be zero, and you need to deselect anything remaining
    ;; (except when the Shift or Control keys are pressed).
    (when (and (zero? count)
               (not shift-key-pressed?)
               (not control-key-pressed?))
      (o_select_unselect_all *window)))

  (i_update_menus *window))


(define (finish-box-selection window x y)
  "Finish the process of box selection in WINDOW.  (X . Y) is the
unused value of the current world coordinate."
  (define *window (check-window window 1))

  (check-action-state window)

  (invalidate-selection-box window)
  (schematic_window_set_rubber_visible *window 0)

  (search-visible-objects window)

  (set-action-mode! 'select-mode #:window window)
  (i_action_stop *window))


(define (start-selection window x y)
  "Choose the way of how to start the selection process.  If no grip
was found at the given world coordinate (X . Y) the function
starts an action in WINDOW in order to force other function to
decide that.  Otherwise, it switches on the grips mode for working
with the grip found.  The function is intended to be called by
pressing the left mouse button."
  (define *window (check-window window 1))

  (check-integer x 2)
  (check-integer y 3)

  ;; Look for grips or fall through if not enabled.
  (o_grips_start *window x y)

  (unless (eq? (action-mode window) 'grips-mode)
    ;; Now go into normal select mode.
    (i_action_start *window)
    (schematic_window_set_first_wx *window x)
    (schematic_window_set_first_wy *window y)
    (schematic_window_set_second_wx *window x)
    (schematic_window_set_second_wy *window y)))


;;; Test if there is a selected object under cursor in *WINDOW at
;;; the coordinate (X . Y).  Return TRUE on success, otherwise
;;; return FALSE.
(define (find-selected-object *window x y)
  (define *canvas (schematic_window_get_current_canvas *window))

  (when (null-pointer? *canvas)
    (error "NULL canvas."))

  (let ((slack
         (schematic_canvas_WORLDabs *canvas
                                    (schematic_window_get_select_slack_pixels *window)))
        (*selection (schematic_window_get_selection_list *window)))

    (let loop ((*selected-objects (glist->list (lepton_list_get_glist *selection)
                                               identity)))
      (if (null? *selected-objects)
          FALSE
          (if (is-hit? *window (car *selected-objects) x y slack)
              TRUE
              (loop (cdr *selected-objects)))))))


(define (continue-selection window x y)
  "Continue selection at world coordinate point (X . Y) in WINDOW.
The function determines whether objects have to be selected or
moved.  Checks if the Shift or Control keys are pressed, (that
means the user definitely wants to drag out a selection box), or
there are no selected objects under the cursor.  In that case the
function starts drawing the selection box.  Otherwise, it looks
for the objects that have been or could be selected and starts
moving them.  The function is intended to be called by motion of
the mouse while the left mouse button is pressed."
  (define *window (check-window window 1))
  (define wx1 (schematic_window_get_first_wx *window))
  (define wy1 (schematic_window_get_first_wy *window))

  (check-integer x 2)
  (check-integer y 3)
  (check-action-state window)

  ;; Check if a mod key is pressed or there is no selected object
  ;; under the cursor.
  (if (or (true? (schematic_window_get_shift_key_pressed *window))
          (true? (schematic_window_get_control_key_pressed *window))
          (and (false? (find-selected-object *window wx1 wy1))
               (or (false? (find-object *window wx1 wy1))
                   (false? (o_select_selected *window)))))
      ;; Start drawing a selection box to select objects.
      (start-box-selection window x y)

      ;; Start moving the selected object(s).
      (o_move_start *window wx1 wy1)))


(define (finish-selection window x y)
  "Finish the process of selection in WINDOW at the world
coordinate (X . Y) where the function tries to find an object and
select it.  The function is intended to be called by releasing the
left mouse button."
  (define *window (check-window window 1))

  (check-integer x 2)
  (check-integer y 3)
  (check-action-state window)

  ;; Look for objects to select.
  (find-object *window x y)
  (i_action_stop *window))
