;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2001 by Alexey Dejneka (adejneka@comail.ru)
;;;  (c) copyright 2003 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; TODO:
;;;
;;; - Check types: RATIONAL, COORDINATE, REAL?
;;; - Check default values of unsupplied arguments.
;;; - Better error detection.
;;; - 
;;; - Multiple columns:
;;; - - all columns are assumed to have the same width;
;;; - - all columns have the same number of rows; they should have the
;;;     same height.
;;; - :MOVE-CURSOR T support.
;;; - All types of widths, heights.
;;;   width, height too?
;;; - Item list formatting: what is :EQUALIZE-COLUMN-WIDTHS?!
;;;
;;; - We should think about inserting actual gutter into the output
;;;   records so that the bounding box of a row or a cell it its
;;;   logical dimension.

;;; - I would prefer if the INITIALIZE-INSTANCE would grok spacing and
;;;   all. Hmm, is that correct?

;;; The question araise if we need to support something like:
;;; (formatting-row ()
;;;   (with-output-as-presentation ()
;;;     (formatting-cell ())
;;;     (formatting-cell ())))

;;; Further: Should this table be somehow dynamic? That is when cell
;;; contents change also rerun the layout protocol? Or is that somehow
;;; covered by the incremental redisplay?

;;; Guess, we just do, it couldn't hurt.

(in-package :clim-internals)

;;; Cell formatting
(define-protocol-class cell-output-record (output-record))

;;; STANDARD-CELL-OUTPUT-RECORD class
(defclass standard-cell-output-record (cell-output-record
                                       standard-sequence-output-record)
  ((align-x    :initform :left :initarg :align-x    :reader cell-align-x)
   (align-y    :initform :baseline :initarg :align-y    :reader cell-align-y)
   (min-width  :initform 0   :initarg :min-width  :reader cell-min-width)
   (min-height :initform 0   :initarg :min-height :reader cell-min-height)))

(defun table-cell-allowed-as-child-of (record)
  "Internal. A predicate which decides whether 'record' can take a
table cell as argument."
  ;; We allow a cell output record here if any ancestor is a
  ;; table-row, column or list-item. Note: This is probably not 100%
  ;; correct, as I have no idea what happens when you nest graphs and
  ;; tables ...
  (or (row-output-record-p record)
      (column-output-record-p record)
      (item-list-output-record-p record)
      (and (output-record-parent record)
           (table-cell-allowed-as-child-of (output-record-parent record)))))

(defun assert-table-cell-allowed (record)
  "Internal. Assert that the a table cell is allowed here and if not
   barf."
  (unless (table-cell-allowed-as-child-of record)
    (error "Sorry ~S not within ~S, ~S or ~S."
           'formatting-cell
           'formatting-row
           'formatting-column
           'formatting-item-list)))

(defmethod invoke-formatting-cell (stream cont
                                   &key (align-x :left)
                                        (align-y :baseline)
                                        (min-width 0)
                                        (min-height 0)
                                        (record-type 'standard-cell-output-record)
                                   &allow-other-keys)
  (invoke-with-new-output-record
   stream
   (lambda (stream record)
     (declare (ignore record))
     (letf (((stream-cursor-position stream) (values 0 0)))
       (funcall cont stream)))
   record-type
   :align-x align-x
   :align-y align-y
   :min-width (parse-space stream min-width :horizontal)
   :min-height (parse-space stream min-height :vertical)))

(defgeneric invoke-formatting-cell (stream cont
                                    &key align-x align-y min-width min-height record-type
                                    &allow-other-keys))

(defmacro formatting-cell ((&optional (stream t)
                            &rest more
                            &key align-x align-y
                                 min-width min-height
                                 (record-type ''standard-cell-output-record))
                           &body body)
  (declare (ignore align-x align-y min-width min-height record-type))
  (setf stream (stream-designator-symbol stream))
  (gen-invoke-trampoline 'invoke-formatting-cell
                         (list stream)
                         more
                         body))


;;; Generic block formatting
(defclass block-output-record-mixn ()
  ()
  (:documentation "The class representing one-dimensional blocks of cells."))

(defgeneric map-over-block-cells (function block)
  (:documentation "Applies the FUNCTION to all cells in the BLOCK."))

(defmethod map-over-block-cells (function (block block-output-record-mixn))
  ;; ### we need to do better
  (labels ((foo (row-record)
             (map-over-output-records
              (lambda (record)
                (foo record)
                (when (cell-output-record-p record)
                  (funcall function record)))
              row-record)))
    (foo block)))


;;; Row formatting
(define-protocol-class row-output-record (output-record))

(defgeneric map-over-row-cells (function row-record)
  (:documentation "Applies FUNCTION to all the cells in the row
ROW-RECORD, skipping intervening non-table output record structures.
FUNCTION is a function of one argument, an output record corresponding
to a table cell within the row."))

;;; STANDARD-ROW-OUTPUT-RECORD class
(defclass standard-row-output-record (row-output-record
                                      block-output-record-mixn
                                      standard-sequence-output-record)
  ())

(defmethod map-over-row-cells (function
                               (row-record standard-row-output-record))
  (map-over-block-cells function row-record))

(defmacro formatting-row ((&optional (stream t)
                           &key (record-type ''standard-row-output-record))
                           &body body)
  (setf stream (stream-designator-symbol stream))
  (gen-invoke-trampoline 'invoke-formatting-row (list stream) (list record-type) body))

(defgeneric invoke-formatting-row (stream cont record-type &rest initargs))

(defmethod invoke-formatting-row (stream cont record-type &rest initargs)
  (apply #'invoke-with-new-output-record stream (lambda (s r) r (funcall cont s)) record-type initargs))


;;; Column formatting
(define-protocol-class column-output-record (output-record))

(defgeneric map-over-column-cells (function column-record)
  (:documentation "Applies FUNCTION to all the cells in the column
COLUMN-RECORD, skipping intervening non-table output record
structures. FUNCTION is a function of one argument, an output record
corresponding to a table cell within the column."))

;;; STANDARD-COLUMN-OUTPUT-RECORD class
(defclass standard-column-output-record (column-output-record
                                         block-output-record-mixn
                                         standard-sequence-output-record)
  ())

(defmethod map-over-column-cells (function (column-record standard-column-output-record))
  (map-over-block-cells function column-record))

(defmacro formatting-column ((&optional (stream t)
                                        &key (record-type ''standard-column-output-record))
                             &body body)
  (setf stream (stream-designator-symbol stream))
  (gen-invoke-trampoline 'invoke-formatting-column (list stream) (list record-type) body))

(defgeneric invoke-formatting-column (stream cont record-type &rest initargs))

(defmethod invoke-formatting-column (stream cont record-type &rest initargs)
  (apply #'invoke-with-new-output-record stream (lambda (s r) r (funcall cont s)) record-type initargs))


;;; Table formatting

(define-protocol-class table-output-record (output-record))

(defgeneric map-over-table-elements (function table-record type)
  (:documentation "Applies FUNCTION to all the rows or columns of
TABLE-RECORD that are of type TYPE. TYPE is one of :ROW, :COLUMN or
:ROW-OR-COLUMN. FUNCTION is a function of one argument. The function
skips intervening non-table output record structures."))
(defgeneric adjust-table-cells (table-record stream))
(defgeneric adjust-multiple-columns (table-record stream))

;;; STANDARD-TABLE-OUTPUT-RECORD class
(defclass standard-table-output-record (table-output-record
                                        standard-sequence-output-record)
  (;; standard slots
   (x-spacing :initarg :x-spacing)
   (y-spacing :initarg :y-spacing)
   (multiple-columns :initarg :multiple-columns)
   (multiple-columns-x-spacing :initarg :multiple-columns-x-spacing)
   (equalize-column-widths :initarg :equalize-column-widths)
   ;; book keeping -- communication from adjust-table-cells to
   ;; adjust-multiple-columns
   (widths)
   (heights)                            ;needed?
   (rows)
   ))

(defmethod initialize-instance :after ((table standard-table-output-record)
                                       &rest initargs)
  (declare (ignore initargs))
  (unless (slot-boundp table 'multiple-columns-x-spacing)
    (setf (slot-value table 'multiple-columns-x-spacing)
          (slot-value table 'x-spacing))))

(defmacro formatting-table ((&optional (stream t)
                                       &rest args
                                       &key x-spacing y-spacing
                                            multiple-columns
                                            multiple-columns-x-spacing
                                            equalize-column-widths (move-cursor t)
                                            (record-type ''standard-table-output-record)
                                       &allow-other-keys)
                            &body body)
  (declare (ignore x-spacing y-spacing multiple-columns
                   multiple-columns-x-spacing
                   equalize-column-widths move-cursor record-type))
  (gen-invoke-trampoline 'invoke-formatting-table
                         (list (stream-designator-symbol stream))
                         args
                         body))

(defun invoke-formatting-table
    (stream continuation
     &key x-spacing y-spacing
     multiple-columns
     multiple-columns-x-spacing
     equalize-column-widths
     (move-cursor t)
     (record-type 'standard-table-output-record)
     &allow-other-keys)
  (setq x-spacing (parse-space stream (or x-spacing #\Space) :horizontal))
  (setq y-spacing (parse-space stream (or y-spacing
                                          (stream-vertical-spacing stream))
                               :vertical))
  (setq multiple-columns-x-spacing
        (if multiple-columns-x-spacing
            (parse-space stream multiple-columns-x-spacing :horizontal)
            x-spacing))
  (with-new-output-record
      (stream record-type table
              :x-spacing x-spacing
              :y-spacing y-spacing
              :multiple-columns multiple-columns
              :multiple-columns-x-spacing multiple-columns-x-spacing
              :equalize-column-widths equalize-column-widths)
    (multiple-value-bind (cursor-old-x cursor-old-y)
        (stream-cursor-position stream)
      (with-output-recording-options (stream :record t :draw nil)
        (funcall continuation stream)
        (finish-output stream))
      (adjust-table-cells table stream)
      (when multiple-columns (adjust-multiple-columns table stream))
      #+NIL
      (setf (output-record-position table)
            (values cursor-old-x cursor-old-y))
      (if move-cursor
          ;; FIXME!!!
          ;; Yeah, fix me -- what is wrong with that?
          (setf (stream-cursor-position stream)
                (values (bounding-rectangle-max-x table)
                        (bounding-rectangle-max-y table)))
          (setf (stream-cursor-position stream)
                (values cursor-old-x cursor-old-y)))
      (replay table stream))))

(defmethod map-over-table-elements (function
                                    (table-record standard-table-output-record)
                                    type)
  (labels ((foo (table-record)
             (map-over-output-records
              (lambda (record)
                (foo record)
                (when (or
                       (and (row-output-record-p record)
                            (member type '(:row :row-or-column)))
                       (and (column-output-record-p record)
                            (member type '(:column :row-or-column))))
                  (funcall function record))
                )
              table-record)))
    (foo table-record)))


;;; Item list formatting

(define-protocol-class item-list-output-record ()
  ())

(defgeneric map-over-item-cells (function item-list-record)
  )
(defgeneric adjust-item-list-cells (item-list-record stream))

(defclass standard-item-list-output-record (item-list-output-record
                                            block-output-record-mixn
                                            standard-sequence-output-record)
  ((x-spacing :initarg :x-spacing)
   (y-spacing :initarg :y-spacing)
   (initial-spacing :initarg :initial-spacing)
   (row-wise :initarg :row-wise)
   (n-rows :initarg :n-rows)
   (n-columns :initarg :n-columns)
   (max-width :initarg :max-width)
   (max-height :initarg :max-height)))

(defmethod map-over-item-cells (function
                                (item-list-record standard-item-list-output-record))
  (map-over-block-cells function item-list-record))

(defun invoke-format-item-list (stream continuation
                                &key x-spacing y-spacing n-columns n-rows
                                     max-width max-height
                                     initial-spacing (row-wise t) (move-cursor t)
                                (record-type
                                 'standard-item-list-output-record))
  (setq x-spacing (parse-space stream (or x-spacing #\Space) :horizontal))
  (setq y-spacing (parse-space stream (or y-spacing
                                          (stream-vertical-spacing stream))
                               :vertical))
  (with-new-output-record
      (stream record-type item-list
              :x-spacing x-spacing
              :y-spacing y-spacing
              :initial-spacing initial-spacing
              :row-wise row-wise
              :n-rows n-rows
              :n-columns n-columns
              :max-width max-width
              :max-height max-height)
    (multiple-value-bind (cursor-old-x cursor-old-y)
        (stream-cursor-position stream)
      (with-output-recording-options (stream :record t :draw nil)
        (funcall continuation stream)
        (finish-output stream))
      (adjust-item-list-cells item-list stream)
      (if move-cursor
          ;; FIXME!!!
          #+ignore
          (setf (stream-cursor-position stream)
                (values cursor-new-x cursor-new-y))
          #-ignore
          nil
          (setf (stream-cursor-position stream)
                (values cursor-old-x cursor-old-y)))
      (replay item-list stream)
      item-list)))

(defun format-items (items
                     &rest args
                     &key (stream *standard-output*)
                          printer presentation-type
                          cell-align-x cell-align-y
                     &allow-other-keys)
  (let ((printer (if printer
                     (if presentation-type
                         #'(lambda (item stream)
                             (with-output-as-presentation (stream item presentation-type)
                               (funcall printer item stream)))
                         printer)
                     (if presentation-type
                         #'(lambda (item stream)
                             (present item presentation-type :stream stream))
                         #'prin1))))
    (with-keywords-removed (args (:stream :printer :presentation-type
                                  :cell-align-x :cell-align-y))
      (apply #'invoke-format-item-list
             stream
             #'(lambda (stream)
                 (map nil
                      #'(lambda (item)
                          (formatting-cell (stream :align-x cell-align-x
                                                   :align-y cell-align-y)
                                           (funcall printer item stream)))
                      items))
             args))))

(defmacro formatting-item-list ((&optional (stream t)
                                 &rest args
                                 &key x-spacing y-spacing n-columns n-rows
                                      stream-width stream-height
                                      max-width max-height
                                      initial-spacing (row-wise t) (move-cursor t)
                                      record-type &allow-other-keys)
                                &body body)
  (declare (ignore x-spacing y-spacing n-columns n-rows
                   stream-width stream-height
                   max-width max-height
                   initial-spacing row-wise move-cursor
                   record-type))
  (setf stream (stream-designator-symbol stream))
  (gen-invoke-trampoline 'invoke-format-item-list
                         (list stream)
                         args
                         body))

(defmethod adjust-table-cells ((table-record standard-table-output-record)
                               stream)
  (with-slots (x-spacing y-spacing equalize-column-widths) table-record
    ;; Note: for the purpose of layout it is pretty much irrelevant if
    ;;       this is a table by rows or a table by columns

    (let (rows columns)
      (map-over-table-elements (lambda (thing)
                                 (cond ((row-output-record-p thing)
                                        (push thing rows))
                                       ((column-output-record-p thing)
                                        (push thing columns))
                                       (t
                                        (error "Something is wrong."))))
                               table-record
                               :row-or-column)
      ;;
      (when (and rows columns)
        (error "Make up your mind: Either a table of rows or a table of columns."))
      ;;
      ;; A table is, well, a table.
      ;;
      (setf rows
            (reverse
             (mapcar #'(lambda (row &aux res)
                         (map-over-row-cells (lambda (cell) (push cell res))
                                             row)
                         (reverse res))
                     rows)))
      (setf columns
            (reverse
             (mapcar #'(lambda (column &aux res)
                         (map-over-column-cells (lambda (cell) (push cell res))
                                                column)
                         (reverse res))
                     columns)))
      ;; Since we have :baseline vertical alignment (and no :char
      ;; horizontal alignment like in HTML), we always work from rows.
      (when columns
        ;; rework this into rows, humble but effective.
        (loop for col in columns do
              (labels ((foo (col rows)
                         (cond ((null col) rows)
                               ((cons (cons (car col) (car rows))
                                      (foo (cdr col) (cdr rows)))))))
                (setf rows (foo col rows)))))

      (let ((nrows (length rows))
            (ncols (reduce #'max (mapcar #'length rows) :initial-value 0)))
        (let ((widthen  (make-array ncols :initial-element 0))
              (heights  (make-array nrows :initial-element 0))
              (ascents  (make-array nrows :initial-element 0))
              (descents (make-array nrows :initial-element 0)))

          ;; collect widthen, heights
          (loop for row in rows
                for i from 0 do
                (loop for cell in row
                      for j from 0 do
                      ;; we have cell at row i col j at hand.
                      ;; witdh:
                      (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* cell)
                        (maxf (aref widthen j)
                              (max (- x2 x1) (cell-min-width cell)))
                        (maxf (aref heights i)
                              (max (- y2 y1) (cell-min-height cell)))
                          
                        (when (eq (cell-align-y cell) :baseline)
                          (multiple-value-bind (baseline) (output-record-baseline cell)
                            (maxf (aref ascents i) (- baseline y1))
                            (maxf (aref descents i) (- y2 baseline)))))))

          ;; baseline aligned cells can force the row to be taller.
          (loop for i from 0 below nrows do
                (maxf (aref heights i) (+ (aref ascents i) (aref descents i))))

          (when (slot-value table-record 'equalize-column-widths)
            (setf widthen (make-array ncols :initial-element (reduce #'max widthen :initial-value 0))))

          (setf (slot-value table-record 'widths) widthen
                (slot-value table-record 'heights) heights
                (slot-value table-record 'rows) rows)
          
          ;; Finally just put the cells where they belong.

          (multiple-value-bind (cx cy) (stream-cursor-position stream)
            (loop for row in rows
                  for y = cy then (+ y h y-spacing)
                  for h across heights
                  for ascent across ascents
                  do
                  (loop for cell in row
                        for x = cx then (+ x w x-spacing) 
                        for w across widthen do
                        (adjust-cell* cell x y w h ascent)))))))))

(defmethod adjust-multiple-columns ((table standard-table-output-record) stream)
  (with-slots (widths heights rows
               multiple-columns multiple-columns-x-spacing x-spacing y-spacing)
      table
    (let* ((mcolumn-width
            ;; total width of a column of the "meta" table.
            (+ (reduce #'+ widths)
               (* (1- (length widths)) x-spacing)
               multiple-columns-x-spacing))
           (n-columns
            (max 1
                 (if (eq multiple-columns t)
                     (floor (+ (- (stream-text-margin stream)
                                  (stream-cursor-position stream))
                               multiple-columns-x-spacing)
                            (+ mcolumn-width multiple-columns-x-spacing))
                     multiple-columns)))
           (column-size (ceiling (length rows) n-columns)) )
      (let ((y 0) (dy 0))
        (loop for row in rows
              for h across heights
              for i from 0
              do
              (multiple-value-bind (ci ri) (floor i column-size)
                (when (zerop ri)
                    (setf dy (- y)))
                (let ((dx (* ci mcolumn-width)))
                  (loop for cell in row do
                        (multiple-value-bind (x y) (output-record-position cell)
                          (setf (output-record-position cell)
                                (values (+ x dx) (+ y dy))))))
                (incf y h)
                (incf y y-spacing)))) )))

(defmethod adjust-item-list-cells ((item-list standard-item-list-output-record)
                                   stream)
  (with-slots (x-spacing y-spacing initial-spacing row-wise) item-list
    ;;
    ;; What we do:
    ;;
    ;; First we collect the items, then we figure out the width of the
    ;; single initial column. While doing so we collect the heights.
    (let ((items nil)
          (width 0)
          (heights nil))
      ;;
      (map-over-item-cells (lambda (item)
                             (push item items))
                           item-list)
      (setf items (reverse items))
      (setf heights (make-array (length items)))
      ;;
      (loop for item in items
            for i from 0
            do
            (with-bounding-rectangle* (x1 y1 x2 y2) item
              (maxf width (- x2 x1))
              (setf (aref heights i) (- y2 y1))))
      ;;
      ;; Now figure out the number of rows and the number of columns to
      ;; layout to.
      ;;
      (let ((stream-width (- (stream-text-margin stream) (stream-cursor-position stream)
                             (if initial-spacing
                                 x-spacing
                                 0)))
            (N (length items))
            (column-width
             (+ width x-spacing)))
        ;; ### note that the floors below are still not correct
        (multiple-value-bind (n-columns n-rows)
            (with-slots (n-columns n-rows max-width max-height) item-list
              (cond (n-columns
                     (values n-columns (ceiling N n-columns)))
                    (n-rows
                     (values (ceiling N n-rows) n-rows))
                    (max-width
                     (let ((n-columns (max 1 (floor (+ max-width x-spacing)
                                                    (+ column-width x-spacing)))))
                       (values n-columns (ceiling N n-columns))))
                    (max-height
                     ;; difficult
                     ;; ###
                     (let ((n-rows (max 1 (floor (+ max-height y-spacing)
                                                 (+ (reduce #'max heights :initial-value 0)
                                                    (* (1- (length heights)) y-spacing)
                                                    y-spacing)))))
                       (values (ceiling N n-rows) n-rows)))
                    (t
                     (let ((n-columns (max 1 (floor (+ stream-width x-spacing)
                                                    (+ column-width x-spacing)))))
                       (values n-columns (ceiling N n-columns))))))
          ;;
          (cond (row-wise
                 ;; Here is the catch: When this is row-wise, this not
                 ;; not so different from a table.
                 ;; ### do the cells put alongside expose baseline adjust?
                 (let ((y 0))
                   (loop for yi below n-rows
                         while items
                         do
                         (let ((h 0))
                           (loop for xi below n-columns
                                 for x = (if initial-spacing (floor x-spacing 2) 0)
                                 then (+ x width x-spacing)
                                 while items
                                 do
                                 (let ((item (pop items)))
                                   (maxf h (bounding-rectangle-height item))
                                   (adjust-cell* item x y width
                                                 (bounding-rectangle-height item)
                                                 (output-record-baseline item))))
                           (incf y h)))))
                (t
                 ;; This is somewhat easier ...
                 (let (h)
                   (loop for xi below n-columns
                         for x = (if initial-spacing (floor x-spacing 2) 0)
                         then (+ x width x-spacing)
                         while items
                         do
                         (loop for yi below n-rows
                               for y = 0 then (+ y y-spacing h)
                               while items do
                               (let ((item (pop items)))
                                 (setf h (bounding-rectangle-height item))
                                 (adjust-cell* item x y width h (output-record-baseline item)))))))))))))

(defun adjust-cell* (cell x y w h ascent)
  (setf (output-record-position cell)
        (values
         (case (cell-align-x cell)
           ((nil :left)   x)            ;###
           (:center (+ x (/ (- w (bounding-rectangle-width cell)) 2)))
           (:right  (- (+ x w) (bounding-rectangle-width cell))))
         (case (cell-align-y cell)
           (:top    y)
           (:bottom (- (+ y h) (bounding-rectangle-height cell)))
           (:center (+ y (/ (- h (bounding-rectangle-height cell)) 2)))
           (:baseline
            (multiple-value-bind (baseline) (output-record-baseline cell)
              ;; make (+ y ascents) line up with (+ y1 b)
              ;; that is y+a = y1+b -> y1= y+a-b
              (+ y (- ascent baseline))))))))

