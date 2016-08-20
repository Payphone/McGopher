(defpackage #:mcgopher.utils
  (:use #:cl
        #:cl-ppcre)
  (:export #:aif
           #:asetf
           #:it
           #:read-until
           #:tabs-to-spaces
           #:queue
           #:make-queue
           #:queue-front
           #:queue-push
           #:queue-next))

(in-package #:mcgopher.utils)

(defmacro aif (test then &optional else)
  "Anaphoric if, allows 'it' to be used in the body."
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro asetf (value new-value)
  "Similar to setf, except it only takes a single pair and binds the symbol
   value to 'it'."
  `(let ((it ,value))
     (setf ,value ,new-value)))

(defun read-until (separator stream &optional output)
  "Reads from a stream until it reaches the separator character, then returns
   the collected characters as a string."
  (let ((character (read-char stream nil nil)))
    (if (or (not character) (char= separator character))
        (coerce (reverse output) 'string)
        (read-until separator stream
                    (cons character output)))))

(defun tabs-to-spaces (string)
  "Converts all tabs to four spaces in a string."
  (ppcre:regex-replace-all #\Tab string "    "))

(defstruct queue
  "A structure for a queue of elements with a maximum size. If the max size is
   exceeded, the last element is removed."
  (elements '() :type list)
  (max-size 10  :type fixnum))

(defun queue-front (queue)
  "Returns the first element in a queue."
  (car (queue-elements queue)))

(defun queue-push (element queue)
  "Pushes an element to the front of a queue, returning the new queue."
  (let ((new-queue queue))
    (if (< (length (queue-elements queue)) (queue-max-size queue))
        (asetf (queue-elements new-queue) (cons element it))
        (asetf (queue-elements new-queue) (cons element (butlast it))))
    new-queue))

(defun queue-next (queue)
  "Returns a new queue with the front element removed."
  (let ((new-queue queue))
    (if (cdr (queue-elements queue))
        (asetf (queue-elements new-queue) (cdr it)))
    new-queue))
