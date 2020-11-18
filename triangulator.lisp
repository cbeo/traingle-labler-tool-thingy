;;;; triangulator.lisp

(in-package #:triangulator)

(defclass point ()
  ((raw :initarg :raw :reader raw-point)))

(defclass tracking-point (point)
  ((label :accessor label :initform "")
   (behavior :accessor behavior :initform "")))

(defun make-tracking-point (x y)
  (make-instance 'tracking-point :raw (sdl2:make-point x y)))

(defclass vertex (point)
  ((tracking :accessor tracking-point :initform "")))

(defmethod j:%to-json ((v vertex))
  (j:with-object
    (j:write-key-value "point" (list (point-x v) (point-y v)))
    (j:write-key-value "tracking" (tracking-point v))))

(defmethod j:%to-json ((pt tracking-point))
  (j:with-object
    (j:write-key-value "point" (list (point-x pt) (point-y pt)))
    (j:write-key-value "label" (label pt))
    (j:write-key-value "behavior" (behavior pt))))

(defun make-vertex (x y)
  (make-instance 'vertex :raw (sdl2:make-point x y)))

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

(defun point-x (point)
  (with-slots (raw) point
    (sdl2:point-x raw)))

(defun (setf point-x) (val point)
  (with-slots (raw) point
    (setf (sdl2:point-x raw) val))) 

(defun point-y (point)
  (with-slots (raw) point
    (sdl2:point-y raw)))

(defun (setf point-y) (val point)
  (with-slots (raw) point
    (setf (sdl2:point-y raw) val))) 


(defclass model ()
  ((label
    :accessor label
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

(defmethod j:%to-json ((model model))
  (j:with-object
    (j:write-key-value "label" (label model))
    (j:write-key-value "tracking" (tracking-points model))
    (j:write-key-value "triangles" (triangles model))))

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
:path      - for adding points to the end of a model's path
:tracking  - for add points to the model's tracking points 
:triangle  - for adding triangles to the model
:select    - for selecting points for manipulation")

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
    (switch-mode :path)))

(defun switch-mode (m)
  (setf *mode* m)
  (format t "input mode switched to ~a~%" *mode*))

(defun cancel ()
  (case *mode*
    (:path (remove-path-point))
    (:triangle (remove-triangle))
    (:tracking (remove-tracking-point))))

(defun export-model ()
  (when *current-model*
    (alexandria:write-string-into-file
     (j:to-json *current-model*)
     (concatenate 'string (label *current-model*) ".json")
     :if-exists :supersede)
    (format t "exported ~a~%" (label *current-model*))))

(defun handle-keydown (key)
  (trivia:match key
    ((list :scancode-p) (switch-mode :path))
    ((list :scancode-t :rshift) (switch-mode :triangle))
    ((list :scancode-t :lshift) (switch-mode :triangle))
    ((list :scancode-t) (switch-mode :tracking))
    ((list :scancode-s) (switch-mode :select))
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
    ((list :scancode-e) (edit-selected))
    ((list :scancode-l) (label-model))
    ((list :scancode-x :rshift) (export-model))
    ((list :scancode-x :lshift) (export-model))
    ;;(_ (print key) (force-output))
    ))

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

(defun next-selected-point ()
  (when *current-model* 
    (cond 
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

(defun add-path-point (x y)
  (when *current-model*
    (push (make-vertex x y)
          (model-path *current-model*))))

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

(let ((building-triangle nil))
  (defun add-point-to-triangle (x y)
    (when *current-model* 
      (multiple-value-bind (pt kind) (point-at x y)
        (declare (ignorable kind))
        (when pt
          (push pt building-triangle))
        (when (= 3 (length building-triangle))
          (push building-triangle (triangles *current-model*))
          (setf building-triangle nil))))))

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
  
  (sdl2:render-present renderer)
  (sdl2:delay (round (/ 1000 60))))


(defun start-debug ()
  (bt:make-thread (lambda () (swank:create-server :port 4006 :dont-close t)))
  (start))


(defun start ()
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

          (:quit () t))))))
