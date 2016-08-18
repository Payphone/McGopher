;;;; mcgopher.asd

(asdf:defsystem #:mcgopher
  :description "A Gopher client"
  :author "Peyton Farrar <peyton@peytonfarrar.com>"
  :license "LLGPL"
  :depends-on (#:mcclim
               #:usocket)
  :serial t
  :components ((:file "utils")
               (:file "gopher")
               (:file "package")
               (:file "mcgopher")))
