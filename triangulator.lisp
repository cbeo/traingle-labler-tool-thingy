;;;; triangulator.lisp

(in-package #:triangulator)

(defclass point ()
  ((raw :initarg :raw :reader raw-point)))

(defclass exportable-point ()
  ((x :initarg :x :initform 0 :accessor point-x)
   (y :initarg :y :initform 0 :accessor point-y)))

(defgeneric make-exportable (pt))

(defclass tracking-point (point)
  ((label :accessor label :initform "" :initarg :label)
   (behavior :accessor behavior :initform "" :initarg :behavior)))

(defclass exportable-tracking-point (exportable-point)
  ((label :accessor label :initform "" :initarg :label)
   (behavior :accessor behavior :initform "" :initarg :behavior))  )

(defmethod make-exportable ((pt tracking-point))
  (make-instance 'exportable-tracking-point
                 :x (point-x pt)
                 :y (point-y pt)
                 :label (label pt)
                 :behavior (behavior pt)))

(defun make-tracking-point (x y &key (label "") (behavior ""))
  (make-instance 'tracking-point
                 :raw (sdl2:make-point x y)
                 :label label
                 :behavior behavior))

(defclass vertex (point)
  ((tracking :accessor tracking-point :initform ""
             :initarg :tracking-point)))

(defclass exportable-vertex (exportable-point)
  ((tracking
    :accessor tracking-point
    :initform ""
    :initarg :tracking-point)))

(defmethod make-exportable ((pt vertex))
  (make-instance 'exportable-vertex
                 :x (point-x pt) :y (point-y pt)
                 :tracking-point (tracking-point pt)))

(defmethod j:%to-json ((v vertex))
  (j:with-object
    (j:write-key-value "point" (list (point-x v) (point-y v)))
    (j:write-key-value "tracking" (tracking-point v))))

(defmethod j:%to-json ((v exportable-vertex))
  (j:with-object
    (j:write-key-value "point" (list (point-x v) (point-y v)))
    (j:write-key-value "tracking" (tracking-point v))))

(defmethod j:%to-json ((pt exportable-tracking-point))
  (j:with-object
    (j:write-key-value "point" (list (point-x pt) (point-y pt)))
    (j:write-key-value "label" (label pt))
    (j:write-key-value "behavior" (behavior pt))))

(defmethod j:%to-json ((pt tracking-point))
  (j:with-object
    (j:write-key-value "point" (list (point-x pt) (point-y pt)))
    (j:write-key-value "label" (label pt))
    (j:write-key-value "behavior" (behavior pt))))

(defun make-vertex (x y &key (tracking-point ""))
  (make-instance 'vertex
                 :raw (sdl2:make-point x y)
                 :tracking-point tracking-point))

(defun scale (pt sx sy)
  (setf (point-x pt) (* sx (point-x pt))
        (point-y pt) (* sy (point-y pt)))
  pt)

(defun translate (pt dx dy)
  (incf (point-x pt) dx)
  (incf (point-y pt) dy)
  pt)


(defgeneric edit-point (point)
  (:documentation "interactive edit string features of this point"))

(defmethod edit-point ((pt vertex))
  (with-slots (tracking) pt
    (when (y-or-n-p "Set the tracking label?")
      (princ "new tracking> ") (force-output)
      (setf tracking (read-line)))))

(defmethod edit-point ((pt tracking-point))
  (with-slots (label behavior) pt
    (when (y-or-n-p "Change the label?")
      (princ "new label> ") (force-output)
      (setf label (read-line)))
    (when (y-or-n-p "Change the behavior?")
      (princ "new behavior> ") (force-output)
      (setf behavior (read-line)))))

(defmethod print-object ((vertex vertex) stream)
  (with-slots (tracking) vertex
    (format stream "vertex~%x = ~a~%y = ~a~%tracking = ~a~%"
            (point-x vertex) (point-y vertex) tracking)))

(defmethod print-object ((pt tracking-point) stream)
  (with-slots (label behavior) pt
    (format stream "tracking point~%x = ~a~%y = ~a~%label = ~a~%behavior = ~a~%"
            (point-x pt) (point-y pt) label behavior)    ))

(defgeneric point-x (point))
(defmethod point-x ((point point))
  (with-slots (raw) point
    (sdl2:point-x raw)))

(defmethod (setf point-x) (val (point point))
  (with-slots (raw) point
    (setf (sdl2:point-x raw) val))) 

(defmethod point-y ((point point))
  (with-slots (raw) point
    (sdl2:point-y raw)))

(defmethod (setf point-y) (val (point point))
  (with-slots (raw) point
    (setf (sdl2:point-y raw) val))) 


(defclass model ()
  ((label
    :accessor label
    :initarg :label
    :initform "")
   (path
    :accessor model-path
    :initform  nil)
   (tracking-points
    :accessor tracking-points
    :initform nil)
   (triangles
    :accessor triangles
    :initform nil)))

(defun model-bounding-box (m)
  (let ((box (sdl2:make-rect (point-x (first (model-path m)))
                             (point-y (first (model-path m)))
                             0 0)))
    (dolist (tri (triangles m) box)
      (dolist (pt tri)
        (setf (sdl2:rect-x box) (min (sdl2:rect-x box)
                                     (point-x pt))
              (sdl2:rect-y box) (min (sdl2:rect-y box)
                                     (point-y pt)))
        (setf (sdl2:rect-width box) (max (sdl2:rect-width box)
                                         (- (point-x pt)
                                            (sdl2:rect-x box)))
              (sdl2:rect-height box) (max (sdl2:rect-height box)
                                          (- (point-y pt)
                                             (sdl2:rect-y box))))))))

(defun normalized-model (model)
  (let* ((box
           (model-bounding-box model))
         (tx
           (* -1 (+ (sdl2:rect-x box) (* 0.5 (sdl2:rect-width box)))))
         (ty
           (* -1 (+ (sdl2:rect-y box) (* 0.5 (sdl2:rect-height box)))))
         (scale
           (if (< (sdl2:rect-width box) (sdl2:rect-height box))
               (/ 2 (sdl2:rect-height box))
               (/ 2 (sdl2:rect-width  box))))
         (normalizer
           (lambda (pt) (scale
                         (translate
                          (make-exportable pt)
                          tx ty)
                         scale scale)))
         (new-model
           (make-instance 'model :label (label model))))
    (setf (triangles new-model)
          (mapcar (lambda (tri) (mapcar normalizer tri)) (triangles model))
          (tracking-points new-model)
          (mapcar normalizer (tracking-points model)))
    new-model))

(defun argmin (fn xs)
  (when xs
    (let ((mx (car xs))
          (val (funcall fn (car xs))))
      (dolist (x (cdr xs) mx)
        (let ((tval (funcall fn x)))
          (when (< tval val)
            (setf mx x
                  val tval)))))))

(defun point-dist (a b)
  (let ((dx (- (point-x b) (point-x a)))
        (dy (- (point-y b) (point-y a))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun auto-track ()
  (when (and  *current-model*
              (model-path *current-model*)
              (tracking-points *current-model*))
    (dolist (pt (model-path *current-model*))
      (setf (tracking-point pt)
            (label
             (argmin (lambda (xy) (point-dist pt xy))
                     (tracking-points *current-model*)))))))

(defun pre-process-tringles (triangles)
  (labels ((map-point (pt)
             (typecase pt
               (tracking-point
                (list :|tracking| (label pt)
                      :|point| (list (point-x pt) (point-y pt))))
               (exportable-tracking-point
                (list :|tracking| (label pt)
                      :|point| (list (point-x pt) (point-y pt))))
               (t pt))))
    (loop :for tri :in triangles
          :collect (mapcar #'map-point tri))))

(defmethod j:%to-json ((model model))
  (let ((normal (normalized-model model)))
    (j:with-object
      (j:write-key-value
       "original"
       (j:with-object 
         (j:write-key-value "label" (label model))
         (j:write-key-value "path" (model-path model))
         (j:write-key-value "tracking" (tracking-points model))
         (j:write-key-value "triangles" (pre-process-tringles (triangles model)))))
      (j:write-key-value
       "normalized"
       (j:with-object 
         (j:write-key-value "label" (label normal))
         (j:write-key-value "tracking" (tracking-points normal))
         (j:write-key-value "triangles" (pre-process-tringles (triangles normal))))))))


(defun @> (ob &rest props)
  (let ((val ob))
    (dolist (p props val)
      (setf val (if (integerp p)
                    (elt val p)
                    (getf val p))))))

(defun load-model (json-file)
  (let ((ob
          (@> (j:parse (alexandria:read-file-into-string json-file))
              :|original|))
        (model (make-instance 'model)))
    (setf
     (label model) (@> ob :|label|)

     (tracking-points model)
     (mapcar (lambda (pt)
               (make-tracking-point
                (@> pt :|point| 0)
                (@> pt :|point| 1)
                :label (@> pt :|label|)
                :behavior (@> pt :|behavior|)))
             (@> ob :|tracking|))

     (model-path model)
     (mapcar (lambda (pt)
               (make-vertex
                (@> pt :|point| 0)
                (@> pt :|point| 1)
                :tracking-point (@> pt :|tracking|)))
             (@> ob :|path|)))

    (let* ((all-points
             (append (model-path model) (tracking-points model)))
           (find-pt
             (lambda (tri-pt)
               (find-if (lambda (pt)
                          (and (= (point-x pt) (@> tri-pt :|point| 0))
                               (= (point-y pt) (@> tri-pt :|point| 1))))
                        all-points))))
      (setf (triangles model)
            (mapcar (lambda (tri) (mapcar find-pt tri))
                    (@> ob :|triangles|))))
    model))

(defun load-into-current-model (json-file)
  (setf *current-model* (load-model json-file)))

(defun key (keysym)
  "Converts an sdl keysm into a list that looks like (scancode . modifiers)

Scancode is a keyword that looks like :scancode-a, :scancode-lshift, etc.

Modifiers is a possibly empty list of keywords that look like :lshift
, :rctrl, :ralt, etc
"
  (list* (sdl2:scancode keysym)
         (sdl2:mod-keywords (sdl2:mod-value keysym))))

(defvar *mode* :path
  "Modes include
:path            - for adding points to the end of a model's path
:tracking        - for add points to the model's tracking points 
:triangle        - for adding triangles to the model
:remove-triangle - for removing triangles by selecting their points
:remove-select   - for removing points (and triangles conaining them) by click
:select          - for selecting points for manipulation")

(defun make-queue ()
  (cons nil nil))

(defun enqueue (x q) 
  (push x (car q)))

(defun dequeue (q)
  (when (and (car q) (null (cdr q)))
    (setf (cdr q) (reverse (car q))
          (car q) nil))
  (when (cdr q) (pop (cdr q))))
             
(defun queue-empty-p (q)
  (and (null (car q))
       (null (cdr q))))


(defvar *all-models* (make-queue))
(defvar *current-model* nil)

(defun label-model ()
  (when *current-model*
    (when  (y-or-n-p "Set the model's label? (currently = ~s)"
                     (label *current-model*))
      (princ "new label> ")  (force-output)
      (setf (label *current-model*)
            (read-line)))))

(defun clear-models ()
  (setf *all-models* (make-queue)
        *current-model* nil))

(defun cycle-models (&optional new-p)
  (let ((new-model (if new-p (make-instance 'model)
                       (dequeue *all-models*))))
    (when *current-model*
      (enqueue *current-model* *all-models*))
    (setf *current-model* new-model)
    (setf *selected-pt* nil)
    (switch-mode :path)
    (format t "Cycled to model ~a~%" (label *current-model*))))

(defun switch-mode (m)
  (setf *mode* m)
  (format t "input mode switched to ~a~%" *mode*))

(defun cancel ()
  (case *mode*
    (:path (remove-path-point))
    (:triangle (remove-triangle))
    (:tracking (remove-tracking-point))))

(defun model-ready-for-export-p (m)
  (and m
       (plusp (length (label m)))
       (triangles m)
       (tracking-points m)
       (not  (some (lambda (pt) (or (string= "" (label pt))
                                    (string= "" (behavior pt))))
                   (tracking-points m)))
       (model-path m)
       (not (some (lambda (pt) (string= "" (tracking-point pt)))
                  (model-path m)))))

(defun export-model ()
  (cond
    ((model-ready-for-export-p *current-model*)
     (alexandria:write-string-into-file
      (j:to-json *current-model*)
      (concatenate 'string (label *current-model*) ".json")
      :if-exists :supersede)
     (format t "exported ~a~%" (label *current-model*)))
    (t (format t "modle not ready for export~%"))))

(defun handle-keydown (key)
  (trivia:match key
    ((list :scancode-a) (auto-track) (format t "ran AUTO-TRACK~%"))
    ((list :scancode-p) (switch-mode :path))
    ((list :scancode-t :rshift) (switch-mode :triangle))
    ((list :scancode-t :lshift) (switch-mode :triangle))
    ((list :scancode-t) (switch-mode :tracking))
    ((list :scancode-s) (switch-mode :select))
    ((list :scancode-x) (switch-mode :remove-select))
    ((list :scancode-x :lshift) (switch-mode :remove-triangle))
    ((list :scancode-x :rshift) (switch-mode :remove-triangle))
    ((list :scancode-n) (cycle-models))
    ((list :scancode-n :rshift) (cycle-models t))
    ((list :scancode-n :lshift) (cycle-models t))
    ((list :scancode-z :rshift) (cancel))
    ((list :scancode-z :lshift) (cancel))
    ((list :scancode-up) (move-selected 0 -5))
    ((list :scancode-down) (move-selected 0 5))
    ((list :scancode-left) (move-selected -5 0))
    ((list :scancode-right) (move-selected 5 0))
    ((list :scancode-tab) (next-selected-point))
    ;; ((list :scancode-l) (label-model))
    ;; ((list :scancode-x :rshift) (export-model))
    ;; ((list :scancode-x :lshift) (export-model))
    ;;(_ (print key) (force-output))
    ))

(defun begin-edit-loop (&key tracking-only-p)
  (when *current-model*
    (next-selected-point tracking-only-p)
    (when  (y-or-n-p "Edit?") (edit-selected))
    (let ((starting-point *selected-pt*))
      (next-selected-point tracking-only-p)
      (loop :until (eq *selected-pt* starting-point)
            :do
               (when (y-or-n-p "Edit?") (edit-selected))
               (next-selected-point tracking-only-p)))))

(defvar *selected-pt* nil)
(defun select-point-at (x y)
  (when-let (pt (point-at x y))
    (setf *selected-pt* pt)
    (format t "selected point ~a~%" pt)))

(defun move-selected (dx dy)
  (when *selected-pt*
    (incf (point-x *selected-pt*) dx)
    (incf (point-y *selected-pt*) dy)))

(defun edit-selected ()
  (when *selected-pt*
    (terpri)
    (edit-point *selected-pt*)
    (princ "updated to:")
    (terpri)
    (princ *selected-pt*)
    (terpri)))

(defun next-selected-point (&optional tracking-only-p)
  (when *current-model* 
    (cond
      (tracking-only-p
       (setf *selected-pt*
             (or (second (member *selected-pt* (tracking-points *current-model*)))
                 (first (tracking-points *current-model*)))))

      ((member *selected-pt* (model-path *current-model*))
       (setf *selected-pt*
             (or (second (member *selected-pt* (model-path *current-model*)))
                 (first (tracking-points *current-model*))
                 (first (model-path *current-model*)))))

      ((member *selected-pt* (tracking-points *current-model*))
       (setf *selected-pt*
             (or (second (member *selected-pt* (tracking-points *current-model*)))
                 (first (model-path *current-model*))
                 (first (tracking-points *current-model*)))))

      (t
       (setf *selected-pt* (first (model-path *current-model*)))))
    (when *selected-pt*
      (terpri)
      (princ *selected-pt*) (force-output))))


(defun insert-after (y x xs &optional back)
  (cond ((null xs)
         (reverse (cons y back)))

        ((eql x (car xs))
         (append (reverse back)
                 (list* (car xs) y  (cdr  xs))))

        (t
         (insert-after y x (cdr xs) (cons (car xs) back)))))


(defun add-path-point (x y)
  (when *current-model*
    (setf (model-path *current-model*)
          (insert-after (make-vertex x y)
                        *selected-pt*
                        (model-path *current-model*)))))

(defun remove-path-point ()
  (when *current-model*
    (pop (model-path *current-model*))))

(defun add-tracking-point (x y)
  (when *current-model* 
    (push (make-tracking-point x y)
          (tracking-points *current-model*))))

(defun remove-tracking-point ()
  (when *current-model*
    (pop (tracking-points *current-model*))))

(defun point-at (x y &key (radius 10))
  (flet ((lookup (pts)
           (find-if
            (lambda (pt)
              (and (<= (abs (- (point-x pt) x)) radius)
                   (<= (abs (- (point-y pt) y)) radius)))
            pts)))
    (when-let (pt (lookup (model-path *current-model*)))
      (return-from point-at (values pt :path)))
    (when-let (pt (lookup (tracking-points *current-model*)))
      (return-from point-at (values pt :tracking)))
    (values nil nil)))

(defvar *building-triangle* nil)

(defun add-point-to-triangle (x y)
  (when *current-model* 
    (multiple-value-bind (pt kind) (point-at x y)
      (declare (ignorable kind))
      (when pt
        (pushnew pt *building-triangle*))
      (when (= 3 (length *building-triangle*))
        (push *building-triangle* (triangles *current-model*))
        (setf *building-triangle* nil)))))

(defun remove-triangle-by-points (x y)
  (when *current-model*
    (let ((pt (point-at x y)))
      (when pt
        (push pt *building-triangle*))
      (when (= 3 (length *building-triangle*))
        (setf (triangles *current-model*)
              (remove-if
               (lambda (tri)
                 (every (lambda (tri-pt) (member tri-pt *building-triangle*))
                        tri))
               (triangles *current-model*)))
        (setf *building-triangle* nil)))))

(defun remove-triangles-involving (pt)
  (when *current-model*
    (setf (triangles *current-model*)
          (remove-if
           (lambda (tri) (member pt tri))
           (triangles *current-model*)))))

(defun remove-point-at (x y)
  (when *current-model*
    (multiple-value-bind (pt kind) (point-at x y)
      (when pt
        (remove-triangles-involving pt)
        (case kind
          (:path (setf (model-path *current-model*)
                       (remove pt (model-path *current-model*))))
          (:tracking (setf (tracking-points *current-model*)
                           (remove pt (tracking-points *current-model*)))))))))

(defun remove-triangle ()
  (when *current-model*
    (pop (triangles *current-model*))))

(defun handle-mousedown (&key x y clicks button)
  (declare (ignorable x y clicks button))
  (unless *current-model*
    (setf *current-model* (make-instance 'model))
    (format t "creating new model and setting it as current~%"))
  (case *mode*
    (:path (add-path-point x y))
    (:tracking (add-tracking-point x y))
    (:triangle (add-point-to-triangle x y))
    (:remove-select (remove-point-at x y))
    (:remove-triangle (remove-triangle-by-points x y))
    (:select (select-point-at x y))))


(let ((point-rect (sdl2:make-rect 0 0 20 20)))
  (defun draw-point (renderer point)
    (setf (sdl2:rect-x point-rect) (- (point-x point) 10)
          (sdl2:rect-y point-rect) (- (point-y point) 10))
    (sdl2:render-fill-rect renderer point-rect)))

(defun render (renderer)
  (sdl2:set-render-draw-color renderer 110 110 110 255)
  (sdl2:render-clear renderer)

  (when *current-model*
    (with-slots (path tracking-points triangles) *current-model* 
      (sdl2:set-render-draw-color renderer 0 0 255 255)
      (let ((path (mapcar #'raw-point
                          (when (car path) (cons (car (last path)) path)))))
        (multiple-value-bind (points num) (apply #'sdl2:points* (reverse path))
          (sdl2:render-draw-lines renderer points num)))

      (dolist (pt path) (draw-point renderer pt))
      (sdl2:set-render-draw-color renderer 255 0 0 255)

      (dolist (pt tracking-points) (draw-point renderer pt))
      (sdl2:set-render-draw-color renderer 0 255 0 255)

      (dolist (tri triangles)
        (let ((tri (mapcar #'raw-point (cons (car (last tri)) tri))))
          (multiple-value-bind (points num) (apply #'sdl2:points* tri)
            (sdl2:render-draw-lines renderer points num))))))

  (when *selected-pt*
    (sdl2:set-render-draw-color renderer 255 0 255 255)
    (draw-point renderer *selected-pt*))
  
  (when *building-triangle*
    (sdl2:set-render-draw-color renderer 255 255 0 255)
    (dolist (pt *building-triangle*) (draw-point renderer pt)))

  (sdl2:render-present renderer)
  (sdl2:delay (round (/ 1000 60))))


(defun start-debug ()
  (bt:make-thread (lambda () (swank:create-server :port 4006 :dont-close t)))
  (start :exit-after t))


(defun start (&key exit-after)
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "triangulator" :flags '(:shown))
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
        (sdl2:with-event-loop (:method :poll)
          (:keydown (:keysym keysym)
                    (if (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                        (sdl2:push-event :quit)
                        (handle-keydown (key keysym))))

          (:mousebuttondown (:x x :y y :clicks clicks :button button )
                            (handle-mousedown :x x :y y :clicks clicks :button button))

          (:idle () (render renderer))

          (:quit () t)))))
  (when exit-after
    (quit)))
