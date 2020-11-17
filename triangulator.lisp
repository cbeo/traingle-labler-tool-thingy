;;;; triangulator.lisp

(in-package #:triangulator)

(defvar *some-rect* (sdl2:make-rect 0 0 100 100))

(defclass model ()
  ((path
    :accessor model-path
    :initform  nil)
   (tracking-points
    :accessor tracking-points
    :initform nil)
   (triangles
    :accessor triangles
    :initform nil)))

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
    (:path (remove-path-point))))

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
    ((list :scancode-up) (move-selected 0 -5))
    ((list :scancode-down) (move-selected 0 5))
    ((list :scancode-left) (move-selected -5 0))
    ((list :scancode-right) (move-selected 5 0))

    ))

(defvar *selected-pt* nil)
(defun select-point-at (x y)
  (when-let (pt (point-at x y))
    (setf *selected-pt* pt)
    (format t "selected point ~a~%" pt)))

(defun move-selected (dx dy)
  (when *selected-pt*
    (incf (sdl2:point-x *selected-pt*) dx)
    (incf (sdl2:point-y *selected-pt*) dy)))

(defun add-path-point (x y)
  (push (sdl2:make-point x y)
        (model-path *current-model*)))

(defun remove-path-point ()
  (pop (model-path *current-model*)))

(defun add-tracking-point (x y)
  (push (sdl2:make-point x y)
        (tracking-points *current-model*)))

(defun point-at (x y &key (radius 10))
  (flet ((lookup (pts)
           (find-if
            (lambda (pt)
              (and (<= (abs (- (sdl2:point-x pt) x)) radius)
                   (<= (abs (- (sdl2:point-y pt) y)) radius)))
            pts)))
    (when-let (pt (lookup (model-path *current-model*)))
      (return-from point-at (values pt :path)))
    (when-let (pt (lookup (tracking-points *current-model*)))
      (return-from point-at (values pt :tracking)))
    (values nil nil)))

(let ((building-triangle nil))
  (defun add-point-to-triangle (x y)
    (multiple-value-bind (pt kind) (point-at x y)
      (declare (ignorable kind))
      (when pt
        (push pt building-triangle))
      (when (= 3 (length building-triangle))
        (push building-triangle (triangles *current-model*))
        (setf building-triangle nil)))))

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


(let ((point-rect (sdl2:make-rect 0 0 10 10)))
  (defun draw-point (renderer point)
    (setf (sdl2:rect-x point-rect) (- (sdl2:point-x point) 5)
          (sdl2:rect-y point-rect) (- (sdl2:point-y point) 5))
    (sdl2:render-fill-rect renderer point-rect)))

(defun render (renderer)
  (sdl2:set-render-draw-color renderer 255 255 255 255)
  (sdl2:render-clear renderer)

  (when *current-model*
    (with-slots (path tracking-points triangles) *current-model* 
      (sdl2:set-render-draw-color renderer 0 0 255 255)
      (let ((path (when (car path) (cons (car (last path)) path))))
        (multiple-value-bind (points num) (apply #'sdl2:points* (reverse path))
          (sdl2:render-draw-lines renderer points num)))
      (dolist (pt path) (draw-point renderer pt))
      (sdl2:set-render-draw-color renderer 255 0 0 255)
      (dolist (pt tracking-points) (draw-point renderer pt))
      (sdl2:set-render-draw-color renderer 0 255 0 255)
      (dolist (tri triangles)
        (let ((tri (cons (car (last tri)) tri)))
          (multiple-value-bind (points num) (apply #'sdl2:points* tri)
            (sdl2:render-draw-lines renderer points num))))))

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
