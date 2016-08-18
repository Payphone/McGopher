(defpackage #:mcgopher.utils
  (:use #:cl)
  (:export #:aif
           #:it))

(in-package #:mcgopher.utils)

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))
