(defsystem #:bullet-hell
  :name "bullet-hell"
  :depends-on (:sdl2 :cl-opengl)
  :serial t
  :components ((:file "package")
               (:file "geometry")
               (:file "classes")
               (:file "main")))
