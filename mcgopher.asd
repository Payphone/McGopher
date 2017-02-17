;;;; mcgopher.asd

(asdf:defsystem #:mcgopher
  :description "A Gopher client"
  :author "Peyton Farrar <peyton@peytonfarrar.com>"
  :license "MIT"
  :depends-on (#:mcclim
               #:iolib
               #:cl-ppcre)
  :serial t
  :components ((:file "utils")
               (:file "gopher")
               (:file "package")
               (:file "mcgopher")))
