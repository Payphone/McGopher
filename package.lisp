;;;; package.lisp

(defpackage #:mcgopher
  (:use #:clim
        #:clim-lisp
        #:mcgopher.utils
        #:mcgopher.gopher)
  (:export
   #:app-main))
