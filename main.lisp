(in-package :bullet-hell)

(defparameter *frame-width* 800)
(defparameter *frame-height* 600)

(defvar *player* (make-instance 'player :size 20 :x 100 :y 100))
(defvar *player-bullets* nil)
(defvar *enemy-bullets* nil)
(defvar *enemies* nil)
(defvar *stars* (loop for i from 0 to 100 collect
                     (make-instance 'star
                                    :x (random *frame-width*) :y (random *frame-height*)
                                    :size 1 :move-speed (+ 1 (random 2)))))

(defvar *last-time* 0)
(defparameter *frame-tick* (/ 1000 60))

(defun draw-enemies ()
  )

(defun deg-to-rad (deg)
  (let ((m (/ deg 180)))
    (* m pi)))

(defun draw-circle (x y size sides)
  (gl:begin :line-loop)
  (let ((elts (loop for i from 0 to (/ 360 sides) collect i)))
    (dolist (elt elts)
      (let ((heading (deg-to-rad (* sides elt))))
        (gl:vertex (+ x (* size (cos heading))) (+ y (* size (sin heading)))))))
  (gl:end))

(defun draw-bullets ()
  (gl:color 1 0 0)
  (loop for pbull in *player-bullets* do
       (draw-circle (x pbull) (y pbull) (size pbull) 3)))

(defun draw-player ()
  (gl:color 0 0 1)
  (draw-circle (x *player*) (y *player*) (size *player*) 30))

(defun draw-stars ()
  (gl:color 1 1 1)
  (loop for star in *stars* do
       (draw-circle (x star) (y star) (size star) 3)))

(defun ai-move ()
  (loop for star in *stars* do
       (with-slots (x y move-speed) star
         (incf y (- (move-speed star)))
         (when (< y 0)
           (setf x (random *frame-width*))
           (setf y (+ *frame-height* (random *frame-height*)))
           (setf move-speed (+ 1 (random 2))))))
  (loop for pbull in *player-bullets* do
       (with-slots (x y dx dy) pbull
         (move pbull dx dy)
         ;enemies/player collision
         (when (> y *frame-height*)
           (setf *player-bullets* (delete pbull *player-bullets*))))))

(defun draw-screen ()
  (draw-stars)
  (draw-player)
  (draw-bullets))

(defun start-window ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        (let ((controllers ())
              (counter 0))
          (setf *last-time* (sdl2:get-ticks))
          (sdl2:gl-make-current win gl-context)
          (gl:viewport 0 0 800 600)
          (gl:matrix-mode :projection)
          (gl:ortho 0 800 0 600 -1 1)
          (gl:matrix-mode :modelview)
          (gl:load-identity)
          (gl:clear-color 0 0 0 1)
          (sdl2:with-event-loop (:method :poll)
            (:keydown
             (:keysym keysym)
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-up)
               (incf (y *player*) 10))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-down)
               (incf (y *player*) -10))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-left)
               (incf (x *player*) -10))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-right)
               (incf (x *player*) 10))
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-space)
               (setf *player-bullets* (cons (make-instance
                                             'bullet :power 1 :x (x *player*) :y (y *player*) :dy 15)
                                            *player-bullets*))))
            (:keyup
             (:keysym keysym)
             (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
               (sdl2:push-event :quit)))
            (:idle
             ()
             (let* ((current-time (- (sdl2:get-ticks) *last-time*))
                    (delta-time (- current-time *last-time*)))
               (when (> delta-time *frame-tick*)
                 (ai-move)
                 (setf *last-time* current-time)
                 (gl:clear :color-buffer)
                 (draw-screen)
                 (gl:flush)
                 (sdl2:gl-swap-window win))))
            (:quit () t)))))))
