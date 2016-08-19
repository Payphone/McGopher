(defpackage #:mcgopher.utils
  (:use #:cl
        #:cl-ppcre)
  (:export #:aif
           #:it
           #:read-until
           #:tabs-to-spaces))

(in-package #:mcgopher.utils)

(defmacro aif (test then &optional else)
  "Anaphoric if, allows 'it' to be used in the body"
  `(let ((it ,test))
     (if it ,then ,else)))

(defun read-until (separator stream &optional output)
  "Reads from a stream until it reaches the separator character,
   then returns the collected characters as a string"
  (let ((character (read-char stream nil nil)))
    (if (or (not character) (char= separator character))
        (coerce (reverse output) 'string)
        (read-until separator stream
                    (cons character output)))))

(defun tabs-to-spaces (string)
  "Converts all tabs to four spaces in a string"
  (ppcre:regex-replace-all #\Tab string "    "))
