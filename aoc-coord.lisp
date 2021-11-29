(defpackage :aoc-coord
  (:use :cl :iterate)
  (:import-from :serapeum :nlet)
  (:export :destructuring-coord
           :aref-coord
           :setf aref-coord
           :scan-matrix
           :make-coord
           :make-coord
           :get-x
           :get-y
           :*coord-origin*
           :*all-absolute-dirs*
           :coord+
           :coord-
           :coord=
           :coord<
           :coordp
           :next-coord
           :turn
           :manhattan-distance
           :manhattan-distance-from-origin
           :coord-angle
           :coord-array-get
           :coord-array-set
           :next-column
           :next-row
           :get-coords-limits
           :left :right :front :back
           :north :south :east :west))

(in-package :aoc-coord)

(defmacro destructuring-coord ((var-x var-y) coord &body body)
  (let ((c (gensym)))
    `(let* ((,c ,coord) (,var-x (pop ,c)) (,var-y ,c)) ,@body)))

(defmacro destructuring-2coords ((var-xa var-ya var-xb var-yb) coord-a coord-b &body body)
  (let ((ca (gensym)) (cb (gensym)))
    `(let* ((,ca ,coord-a) (,cb ,coord-b) (,var-xa (pop ,ca)) (,var-ya ,ca) (,var-xb (pop ,cb)) (,var-yb ,cb)) ,@body)))

(defun aref-coord (arr coord)
  (destructuring-coord (x y) coord
    (aref arr y x)))

(defun (setf aref-coord) (new-val arr coord)
  (destructuring-coord (x y) coord
    (setf (aref arr y x) new-val)))

(defun scan-matrix (func matrix &key initial-value)
  (destructuring-bind (mtx-height mtx-width) (array-dimensions matrix)
    (let ((nelems (* mtx-height mtx-width)))
      (nlet rec ((i 0) (acc initial-value))
        (if (= i nelems)
          acc
          (multiple-value-bind (y x) (floor i mtx-width)
            (let ((coord (make-coord x y)))
              (rec (1+ i) (funcall func matrix coord acc)))))))))

(defun make-coord (&optional (x 0) (y 0)) (cons x y))
(defun get-x (coord) (car coord))
(defun get-y (coord) (cdr coord))

(defvar *coord-origin* (make-coord 0 0))
(defparameter *all-absolute-dirs* '(north east south west))

(defun coord+ (a b)
  (destructuring-2coords (xa ya xb yb) a b
    (cons (+ xa xb) (+ ya yb))))

(defun coord- (a b)
  (destructuring-2coords (xa ya xb yb) a b
    (cons (- xa xb) (- ya yb))))

(defun coord= (a b)
  (destructuring-2coords (xa ya xb yb) a b
    (and (eq xa xb) (eq ya yb))))

(defun coord< (a b)
  (cond
    ((< (get-y a) (get-y b)) t)
    ((> (get-y a) (get-y b)) nil)
    (t
      (cond
        ((< (get-x a) (get-x b)) t)
        (t nil)))))

(defun coordp (c)
  (and
    (consp c)
    (numberp (get-x c))
    (numberp (get-y c))))

(defun next-coord (dir c &optional (steps 1))
  (destructuring-coord (x y) c
    (case dir
      (north (make-coord x (- y steps)))
      (south (make-coord x (+ y steps)))
      (west  (make-coord (- x steps) y))
      (east  (make-coord (+ x steps) y)))))

(defun turn (reldir absdir)
  (ccase reldir
         (left
           (case absdir
             (north 'west )
             (west  'south)
             (south 'east )
             (east  'north)))
         (right
           (case absdir
             (north 'east )
             (east  'south)
             (south 'west )
             (west  'north)))
         (back
           (case absdir
             (north 'south)
             (south 'north)
             (east  'west )
             (west  'east )))
         (front absdir)))

(defun manhattan-distance (p q)
  (+ (abs (- (get-x p) (get-x q))) (abs (- (get-y p) (get-y q)))))

(defun manhattan-distance-from-origin (p)
  (manhattan-distance *coord-origin* p))

(defun coord-angle (crd)
  (atan (cdr crd) (car crd)))

(defun next-column (crd) (cons (1+ (get-x crd)) (get-y crd)))
(defun next-row   (crd) (cons 0 (1+ (get-y crd))))

(defun coord-array-get (arr coord)
  (let ((x (get-x coord)) (y (get-y coord)))
    (unless (or (< x 0) (< y 0) (>= y (array-dimension arr 0)) (>= x (array-dimension arr 1)))
      (aref arr y x))))

(defun coord-array-set (arr coord val)
  (let ((x (get-x coord)) (y (get-y coord)))
    (unless (or (< x 0) (< y 0) (>= y (array-dimension arr 0)) (>= x (array-dimension arr 1)))
      (setf (aref arr y x) val))))

(defun get-coords-limits (list)
  (iterate
    (for coord in list)
    (for x-min first (get-x coord) then (min x-min (get-x coord)))
    (for y-min first (get-y coord) then (min y-min (get-y coord)))
    (for x-max first (get-x coord) then (max x-max (get-x coord)))
    (for y-max first (get-y coord) then (max y-max (get-y coord)))
    (finally (return (list :x-min x-min :y-min y-min :x-max x-max :y-max y-max )))))

