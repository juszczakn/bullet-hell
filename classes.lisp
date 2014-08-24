(in-package :bullet-hell)

(defclass point ()
  ((x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)))

(defclass overlay (point)
  ((id :initform (gensym) :reader id)
   (size :initarg :size :initform 1 :accessor size)
   (shape :initarg :shape :initform :circle :accessor shape)))

(defclass star (overlay)
  ((move-speed :initarg :move-speed :initform 1 :accessor move-speed)))

(defclass bullet (overlay)
  ((power :initarg :power :initform 1 :accessor power)
   (dx :initarg :dx :initform 0 :accessor dx)
   (dy :initarg :dy :initform -10 :accessor dy)))

(defclass entity (overlay)
  ((name :initform nil :initarg :name :accessor name)
   (health :initform 100 :initarg :health :accessor health)
   (weapons :initform (make-instance 'weapon) :initarg :weapons :accessor weapons)
   (shield :initarg :shield :initform 0 :accessor shield)
   (dx :initarg :dx :initform 0 :accessor dx)
   (dy :initarg :dy :initform 0 :accessor dy)
   (shooting-p :initarg :shooting-p :initform nil :accessor shooting-p)))

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

(defgeneric shoot-bullet (e))

(defmethod shoot-bullet ((e entity))
  (with-slots (x y weapons) e
    (let ((bullet (shoot-weapon weapons)))
      (setf (x bullet) x)
      (setf (y bullet) y)
      (setf (dy bullet) 10)
      bullet)))

(defmethod shoot-bullet ((e player))
  (let ((bullet (call-next-method)))
    (with-slots (dx dy) bullet
      (setf dx (+ dx (ceiling (/ (* *player-speed-multiplier* (dx *player*)) 5))))
      (setf dy (+ dy (ceiling (/ (* *player-speed-multiplier* (dy *player*)) 5)))))
    (setf *player-bullets* (cons bullet *player-bullets*))))

(defgeneric shoot-weapon (w))
(defmethod shoot-weapon ((w weapon))
  (make-instance 'bullet))
