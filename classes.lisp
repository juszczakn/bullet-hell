(in-package :bullet-hell)

(defclass point ()
  ((x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)))

(defclass overlay (point)
  ((id :initform (gensym) :reader id)
   (size :initarg :size :initform 1 :accessor size)
   (shape :initarg :shape :initform :circle :accessor shape)
   (color :initform :white :initarg :color :accessor color)))

(defclass bullet (overlay)
  ())

(defclass entity (overlay)
  ((name :initform nil :initarg :name :accessor name)
   (health :initform 100 :initarg :health :accessor health)
   (weapons :initform nil :initarg :weapons :accessor weapons)
   (shield :initarg :shield :initform 0 :accessor shield)))

(defclass player (entity)
  ())

;;square-enemy? circle-enemy?
(defclass enemy (entity)
  ())

(defclass weapon ()
  ((power :initarg :power :initform 1 :accessor power)
   (pattern :initarg :pattern :initform :single :accessor pattern)))

;;;;

(defgeneric move (p dx dy))

(defmethod move ((p point) dx dy)
  (incf (x p) dx)
  (incf (y p) dy))

;;;;

(defgeneric collision? (i j))

(defmethod collision? ((i overlay) (j overlay))
  (let ((ix (x i)) (iy (y i))
        (jx (x j)) (jy (y j))
        (isize (size i)) (jsize (size j))
        (ishape (shape i)) (jshape (shape j)))
    (cond
      ((and
        (eq ishape :circle)
        (eq jshape :circle)) (circle-collision?
                              (list ix iy isize)
                              (list jx jy jsize)))
      ;; todo: bother with other shapes later
      (t t))))
