;;;; package.lisp

(defpackage #:mcgopher
  (:use #:clim
        #:clim-lisp
        #:peyton-utils
        #:files-and-folders
        #:mcgopher.config
        #:mcgopher.utils
        #:mcgopher.gopher)
  (:export
   #:main))
