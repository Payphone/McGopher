(defpackage #:mcgopher.utils
  (:use #:cl
        #:peyton-utils)
  (:export #:tabs-to-spaces
           #:queue
           #:make-queue
           #:queue-elements
           #:queue-front
           #:queue-push
           #:queue-next
           #:queue-length))

(in-package #:mcgopher.utils)

(defun tabs-to-spaces (string)
  "Converts all tabs to four spaces in a string."
  (ppcre:regex-replace-all #\Tab string "    "))

;; Queues

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
  (let ((new-queue (copy-structure queue)))
    (if (< (length (queue-elements queue)) (queue-max-size queue))
        (asetf (queue-elements new-queue) (cons element it))
        (asetf (queue-elements new-queue) (cons element (butlast it))))
    new-queue))

(defun queue-next (queue)
  "Returns a new queue with the front element removed."
  (let* ((new-queue (copy-structure queue)))
    (if (cdr (queue-elements new-queue))
        (setf (queue-elements new-queue) (cdr (queue-elements new-queue))))
    new-queue))

(defun queue-length (queue)
  "Returns the length of the queue."
  (length (queue-elements queue)))
