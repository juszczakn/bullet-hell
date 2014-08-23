(defsystem #:bullet-hell
  :name "bullet-hell"
  :depends-on (:cl-opengl :sdl2)
  :serial t
  :components ((:file "package")
               (:file "geometry")
               (:file "classes")
               (:file "main")))
