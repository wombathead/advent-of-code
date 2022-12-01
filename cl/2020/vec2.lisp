;;;; 2 dimensional vector operations

(defpackage :vec2
  (:use :cl)
  (:export :vec2
           :make-vec2
           :copy-vec2
           :vec2-x
           :vec2-y
           :copy-vec2
           :vec2-p
           
           :add
           :sub
           :mult
           
           :dot
           :l1
           :norm
           :normalize

           :rotate-90
           :rotate-90-about
           :rotate-270
           :rotate-270-about))

(in-package :vec2)

(defstruct vec2
  (x 0)
  (y 0))

(defun add (&rest vectors)
  (make-vec2 :x (apply #'+ (mapcar #'vec2-x vectors))
             :y (apply #'+ (mapcar #'vec2-y vectors))))

(defun sub (&rest vectors)
  (make-vec2 :x (apply #'- (mapcar #'vec2-x vectors))
             :y (apply #'- (mapcar #'vec2-y vectors))))

(defun mult (k v)
  (make-vec2 :x (* k (vec2-x v))
             :y (* k (vec2-y v))))

(defun dot (u v)
  (+ (* (vec2-x u) (vec2-x v)) (* (vec2-y u) (vec2-y v))))

(defun l1 (u v)
  (+ (abs (- (vec2-x u) (vec2-x v)))
     (abs (- (vec2-y u) (vec2-y v)))))

(defun norm (v)
  (let ((x (vec2-x v))
        (y (vec2-y v)))
    (sqrt (+ (* x x) (* y y)))))

(defun normalize (v)
  (let ((x (vec2-x v))
        (y (vec2-y v))
        (n (norm v)))
    (make-vec2 :x (/ x n) :y (/ y n))))

(defun rotate-90 (v)
  " rotate vector V 90 degrees counter-clockwise about the origin "
  (let ((x (vec2-x v))
        (y (vec2-y v)))
    (make-vec2 :x (- y) :y x)))

(defun rotate-270 (v)
  " rotate vector V 90 degrees clockwise about the origin "
  (let ((x (vec2-x v))
        (y (vec2-y v)))
    (make-vec2 :x y :y (- x))))

(defun rotate-90-about (u p)
  (let ((translated (sub u p)))
    (add p (rotate-90 translated))))

(defun rotate-270-about (u p)
  (let ((translated (sub u p)))
    (add p (rotate-270 translated))))

(defun rotate (v degrees)
  (let* ((x (vec2-x v))
         (y (vec2-y v))
         (rads (* degrees (/ pi 180))) 
         (cs (cos rads))
         (sn (sin rads)))
    (make-vec2 :x (- (* x cs) (* y sn))
               :y (+ (* x sn) (* y cs)))))
