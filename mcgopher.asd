;;;; mcgopher.asd

(asdf:defsystem #:mcgopher
  :description "A Gopher client"
  :author "Peyton Farrar <peyton@peytonfarrar.com>"
  :license "MIT"
  :depends-on (#:mcclim
               #:iolib
               #:files-and-folders
               #:peyton-utils
               #:cl-ppcre
               #:alexandria)
  :serial t
  :components ((:file "config")
               (:file "utils")
               (:file "gopher")
               (:file "gui")
               (:file "package")))
