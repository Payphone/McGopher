;;;; mcgopher.asd

(asdf:defsystem #:mcgopher
  :description "A Gopher client"
  :author "Peyton Farrar <peyton@peytonfarrar.com>"
  :license "MIT"
  :depends-on (#:mcclim
               #:iolib
               #:files-and-folders
               #:cl-ppcre
               #:alexandria)
  :serial t
  :components ((:file "src/config")
               (:file "src/utils")
               (:file "src/gopher")
               (:file "src/gui")
               (:file "package")))
