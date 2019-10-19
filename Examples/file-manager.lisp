(cl:in-package #:clim-demo)

;;; Model

(defclass file ()
  ((%name      :initarg  :name
               :reader   name)
   (%directory :initarg  :directory
               :accessor directory*)))

(defun make-file (name)
  (make-instance 'file :name name))

(defclass directory* (file)
  ((%children :accessor children
              :initform '())))

(defmethod shared-initialize :after ((instance   directory*)
                                     (slot-names t)
                                     &key
                                       (children nil children-supplied-p))
  (when children-supplied-p
    (setf (children instance) '())
    (map nil (alexandria:curry #'adopt instance) children)))

(defun make-directory (name &rest children)
  (make-instance 'directory* :name name :children children))

(defmethod adopt ((parent directory*) (child file))
  (setf (directory* child) parent)
  (push child (children parent)))

(defmethod disown ((parent directory*) (child file))
  (setf (directory* child) nil)
  (alexandria:removef (children parent) child))

(defclass root (directory*) ())

(defun make-root (&rest children)
  (make-instance 'root :name "<root>" :children children))

;;; Presentation methods

(defclass file-manager-view (view) ())

(define-presentation-method present ((object file)
                                     (type   file)
                                     (stream extended-output-stream)
                                     (view   file-manager-view)
                                     &key)
  (surrounding-output-with-border (stream :shape :rectangle :background +yellow+)
    (princ (name object) stream)))

(defun print-directory-contents (items stream)
  (formatting-item-list (stream)
    (map nil (lambda (child)
               (formatting-cell (stream)
                 (present child (presentation-type-of child)
                          :stream stream :single-box t)))
         (sort (copy-list items) #'string< :key #'name))))

(define-presentation-method present ((object directory*)
                                     (type   directory*)
                                     (stream extended-output-stream)
                                     (view   file-manager-view)
                                     &key)
  (surrounding-output-with-border (stream :shape :rectangle :background +beige+)
    (princ (name object) stream)
    (with-translation (stream (+ (stream-cursor-position stream) 8) 0)
      (draw-rectangle* stream 0 0 16 16))
    (terpri stream)
    (print-directory-contents (children object) stream)))

(define-presentation-method present ((object root)
                                     (type   root)
                                     (stream extended-output-stream)
                                     (view   file-manager-view)
                                     &key)
  (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region stream)
    (draw-rectangle* stream (+ x1 4) (+ y1 4) (- x2 4) (- y2 4) :filled nil))
  (stream-increment-cursor-position stream 8 8)
  (print-directory-contents (children object) stream))

;;; Application

(define-application-frame file-manager ()
  ((%root :initarg  :root
          :reader   root
          :initform (make-root
                     (make-directory "Trash")
                     (make-directory "Directory 1")
                     (make-directory "Directory 2"
                                     (make-file "baz")
                                     (make-file "fez"))
                     (make-file "whoop"))))
  (:panes
   (files      :application :display-function (lambda (frame pane)
                                                (let ((root (root frame)))
                                                  (present root (presentation-type-of root)
                                                           :stream     pane
                                                           :single-box t)))
                            :default-view     (make-instance 'file-manager-view))
   (interactor :interactor))
  (:layouts
   (:default
    (vertically ()
      (7/8 files)
      (make-pane 'clime:box-adjuster-gadget)
      (1/8 interactor))))
  (:menu-bar              nil)
  (:pointer-documentation t)
  (:default-initargs
   :width 800 :height 600))

(defmethod frame-standard-output ((frame file-manager))
  (frame-standard-input frame))

;;; Commands

(defun drag-file-feedback (frame from-presentation stream x0 y0 x1 y1 state mode)
  (case state
    (:highlight
     (with-output-recording-options (stream :record nil :draw t)
       (multiple-value-bind (old-x old-y) (output-record-position from-presentation)
         (let ((offset-x (- old-x x0))
               (offset-y (- old-y y0))
               (width    (bounding-rectangle-width from-presentation))
               (height   (bounding-rectangle-height from-presentation)))
                                        ; (erase-output-record from-presentation stream)
           (setf (output-record-position from-presentation)
                 (values (+ x1 offset-x) (+ y1 offset-y)))
                                        ; (add-output-record from-presentation (stream-output-history stream))
           (replay-output-record from-presentation stream)
           (setf (output-record-position from-presentation) (values old-x old-y))
           #+no (draw-rectangle* stream x1 y1 (+ x1 width) (+ y1 height)
                                 :filled nil :line-dashes '(4 4))
           (let ((text (string-downcase mode)))
             (draw-text* stream text (+ x1 offset-x width 4) (+ y1 offset-y height 4)))))))
    (:unhighlight
     (multiple-value-bind (old-x old-y) (output-record-position from-presentation)
       (let* ((offset-x (- old-x x0))
              (offset-y (- old-y y0))
              (width      (bounding-rectangle-width from-presentation))
              (height     (bounding-rectangle-height from-presentation))
              (text       (string-downcase mode))
              (text-width (text-size stream text)))
         (repaint-sheet stream (make-rectangle* (+ x1 offset-x -1)
                                                (+ y1 offset-y -1)
                                                (+ x1 offset-x width 2 4 text-width)
                                                (+ y1 offset-y height 2 4))))))))

(defun drag-file-feedback/copy (frame from-presentation stream x0 y0 x1 y1 state)
  (drag-file-feedback frame from-presentation stream x0 y0 x1 y1 state :copy))

(defun drag-file-feedback/move (frame from-presentation stream x0 y0 x1 y1 state)
  (drag-file-feedback frame from-presentation stream x0 y0 x1 y1 state :move))

(defun drag-file-feedback/invalid (frame from-presentation stream x0 y0 x1 y1 state)
  (drag-file-feedback frame from-presentation stream x0 y0 x1 y1 state :invalid))

(define-command (com-new-file :name t :command-table file-manager)
    ((directory directory*)
     (name string))
  (adopt directory (make-file name)))

(define-presentation-to-command-translator directory->com-new-file
    (directory* com-new-file file-manager
     :gesture :select :priority -1
     :documentation ((object stream) (format stream "New file")))
    (directory)
  `(,directory ,(accept 'string :prompt "name")))

(define-command (com-new-directory :name t :command-table file-manager)
    ((directory directory*)
     (name      string))
  (adopt directory (make-directory name)))

(define-presentation-to-command-translator directory->com-new-directory
    (directory* com-new-directory file-manager
     :gesture :select :priority -1
     :documentation ((object stream) (format stream "New directory")))
    (directory)
  `(,directory ,(accept 'string :prompt "name")))

(define-command (com-copy-file :command-table file-manager)
    ((from file) (to directory*))
  (format t "Copying ~A to ~A~%" (name from) (name to))
  (adopt to (make-file (name from))))

(define-drag-and-drop-translator drag-file/invalid
    (file command directory* file-manager
     :gesture t
     :priority -1
     :feedback drag-file-feedback/invalid
     :pointer-documentation ((object destination-object stream event)
                                        ; (setf (clouseau:root-object *inspector* :run-hook-p t) event)
                             (format stream "~A ~A ~A~@[ to ~A ~A~] with ~D"
                                     "Cannot drag"
                                     (type-of object) (name object)
                                     (when destination-object
                                       (type-of destination-object))
                                     (when destination-object
                                       (name destination-object))
                                     (event-modifier-state event))
                             (force-output stream)))
    (object destination-object)
  nil)

(define-drag-and-drop-translator drag-file/copy
    (file command directory* file-manager
     :gesture t
     :feedback drag-file-feedback/copy
     :tester ((object event)
              t #+no (if event
                  (plusp (event-modifier-state event))
                  t))
     :destination-tester ((object destination-object event)
                          (print (event-modifier-state event) *trace-output*)
                          (and (= (event-modifier-state event) 1024)
                               (not (or (eq object destination-object)
                                        (eq (directory* object) destination-object)))))
     :pointer-documentation ((object destination-object stream)
                             ; (setf (clouseau:root-object *inspector* :run-hook-p t) event)
                             (format stream "~A ~A ~A~@[ to ~A ~A~]"
                                     "Copy"
                                     (type-of object) (name object)
                                     (when destination-object
                                       (type-of destination-object))
                                     (when destination-object
                                       (name destination-object)))
                             (force-output stream)))
    (object destination-object)
  `(com-copy-file ,object ,destination-object))

(define-command (com-move-file :command-table file-manager)
    ((from file) (to directory*))
  (format t "Moving ~A to ~A~%" (name from) (name to))
  (disown (directory* from) from)
  (adopt to from))

; (setf *inspector* (nth-value 1 (clouseau:inspect 1 :new-process t)))

(define-drag-and-drop-translator drag-file/move
    (file command directory* file-manager
          :gesture t
          :tester ((object event)
                   t #+no (if event
                       (zerop (event-modifier-state event))
                       t))
          :destination-tester ((object destination-object event)
                               (and (zerop (event-modifier-state event))
                                    (not (or (eq object destination-object)
                                             (eq (directory* object) destination-object)))))
          :feedback drag-file-feedback/move
          :pointer-documentation ((object destination-object stream)
                                  ; (setf (clouseau:root-object *inspector* :run-hook-p t) event)
                                  (format stream "~A ~A ~A~@[ to ~A ~A~]"
                                          "Move"
                                          #+no (cond
                                                 ((plusp (event-modifier-state event))
                                                  "Copy")
                                                 (t
                                                  "Move"))
                                          (type-of object) (name object)
                                          (when destination-object
                                            (type-of destination-object))
                                          (when destination-object
                                            (name destination-object)))
                                  (force-output stream)))
    (object destination-object #+no event)
  (let ((command 'com-move-file #+no (if (plusp (event-modifier-state event))
                     'com-copy-file
                     'com-move-file)))
    `(,command ,object ,destination-object)))

;;;

(defun run-file-manager ()
  (run-frame-top-level (make-application-frame 'file-manager)))
