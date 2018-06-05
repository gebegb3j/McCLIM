;;;; This application is meant to demonstrate output recording at the
;;;; very lowest level.  Every CLIM stream pane has an OUTPUT HISTORY
;;;; which is a special type of output record.  Other output records
;;;; are descendants of the output history.  Output records are used
;;;; by CLIM so that when a window has been obscured and then exposed,
;;;; CLIM can draw the contents the way it appeared before the window
;;;; was obscured.
;;;;
;;;; This application is a bit unusual in that we define our own
;;;; output history class, our own output record type, and our own
;;;; pane type.  There are several reasons for doing it this way.
;;;; First, we want to illustrate that the application programmer is
;;;; not obliged to use the output record types that are supplied by
;;;; CLIM.  In fact, for high-performance applications for which the
;;;; contents of a pane can be very large, and for which that contents
;;;; is edited as part of the application logic, this technique might
;;;; be the only reasonable one for obtaining the required
;;;; performance.  Second, we want to show a simplified version of the
;;;; machinery used by CLIM in order to manage output records.  The
;;;; built-in machinery is designed for high performance, but it is
;;;; also much harder to understand than the simplified machinery in
;;;; this example application.

;;; Make sure we are in the COMMON-LISP-USER package when we start
;;; defining new classes and new functions.
(cl:in-package #:common-lisp-user)

;;; Define the package to be used for this application.  Notice that
;;; we :USE the CLIM-LISP package rather than the COMMON-LISP package.
;;; Notice also that we do not :USE the CLIM package.  Instead we use
;;; explicit package prefixes for all CLIM symbols.
(defpackage #:output-record-example-1
  (:use #:clim-lisp))

(in-package #:output-record-example-1)

;;; This is the class that we are going to use for the output history
;;; of our pane.  It has a simple list of CHILDREN which are going to
;;; be custom output records.
(defclass list-output-history (clim:stream-output-history-mixin)
  ((%children :initform '() :accessor children)))

;;; CLIM requires us to define a method on BOUNDING-RECTANGLE,
;;; specialized to our output-history class.  We just use a fixed
;;; position and a fixed size for our output history.
(defmethod clim:bounding-rectangle* ((region list-output-history))
  (values 0 0 100 100))

;;; CLIM requires us to define a method on CLEAR-OUTPUT-RECORD,
;;; specialized to our output-history class.  We set the list of
;;; children to be the empty list.
(defmethod clim:clear-output-record ((record list-output-history))
  (setf (children record) '()))

;;; CLIM requires us to define a method on REPLAY-OUTPUT-RECORD,
;;; specialized to our output-history class.  For a high-performance
;;; application, it would be wise to organize the output records by
;;; position, but we just have an unordered list of children, so we
;;; just traverse that list and call REPLAY-OUTPUT-RECORD recursively
;;; on each child.
(defmethod clim:replay-output-record ((record list-output-history) stream
                                      &optional region x-offset y-offset)
  (declare (ignore region x-offset y-offset))
  (loop for child in (children record)
        do (clim:replay-output-record child stream)))

(defmethod clim:map-over-output-records-containing-position
    (function (record list-output-history) x y
     &optional x-offset y-offset
     &rest function-args)
  (declare (ignore x-offset y-offset))
  (loop for child in (children record)
        do (clim:with-bounding-rectangle* (min-x min-y max-x max-y) child
             (when (and (<= min-x x max-x) (<= min-y y max-y))
               (funcall function function-args)))))

(defclass circle-output-record (clim:graphics-displayed-output-record)
  ((%center-x :initarg :center-x :accessor center-x)
   (%center-y :initarg :center-y :accessor center-y)))

(defmethod clim:bounding-rectangle* ((region circle-output-record))
  (let ((center-x (center-x region))
        (center-y (center-y region)))
    (values (- center-x 20)
            (- center-y 20)
            (+ center-x 20)
            (+ center-y 20))))

(defmethod clim:replay-output-record ((record circle-output-record) stream
                                      &optional region x-offset y-offset)
  (declare (ignore region x-offset y-offset))
  (clim:with-bounding-rectangle* (min-x min-y max-x max-y) record
    (clim:draw-ellipse* stream
                        (/ (+ min-x max-x) 2) (/ (+ min-y max-y) 2)
                        (/ (- max-x min-x) 3) 0
                        0 (/ (- max-y min-y) 3)
                        :filled t
                        :ink clim:+red+)))

(defclass pane (clim:application-pane)
  ()
  (:default-initargs
   :output-record (make-instance 'list-output-history)
   :display-time nil))

(clim:define-application-frame output-record-example-1 ()
  ()
  (:panes (application (clim:make-pane 'pane :scroll-bars nil))
          (interactor :interactor))
  (:layouts (default (clim:vertically ()
                       (7/10 application)
                       (3/10 interactor)))))

(define-output-record-example-1-command (com-quit :name t) ()
  (clim:frame-exit clim:*application-frame*))

(define-output-record-example-1-command (com-insert-circle :name t)
    ((x 'integer) (y 'integer))
  (let ((pane (clim:find-pane-named clim:*application-frame* 'application )))
    (push (make-instance 'circle-output-record
            :center-x x
            :center-y y
            :parent (clim:stream-output-history pane))
          (children (clim:stream-output-history pane)))))

(define-output-record-example-1-command (com-move :name t)
    ((x 'integer) (y 'integer))
  (let* ((pane (clim:find-pane-named clim:*application-frame* 'application ))
         (history (clim:stream-output-history pane))
         (first (first (children history))))
    (setf (center-x first) x
          (center-y first) y)))

(defun output-record-example-1 ()
  (clim:run-frame-top-level
   (clim:make-application-frame 'output-record-example-1)))
  
(defmethod clim:redisplay-frame-panes :after
    ((frame output-record-example-1) &key force-p)
  (declare (ignore force-p))
  (let ((pane (clim:find-pane-named clim:*application-frame* 'application )))
    (clim:with-output-recording-options (pane :record nil :draw t)
      (clim:draw-rectangle* pane 0 0 1000 1000
                            :filled t
                            :ink clim:+background-ink+))
    (clim:replay (clim:stream-output-history pane) pane)))