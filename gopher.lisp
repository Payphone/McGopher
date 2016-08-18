;;; gopher.lisp

(defpackage #:mcgopher.gopher
  (:use #:cl
        #:mcgopher.utils
        #:usocket)
  (:export #:gopher-get
           #:gopher-follow
           #:gopher-item
           #:gopher-message
           #:gopher-category
           #:gopher-content
           #:gopher-location
           #:gopher-host
           #:gopher-port))

(in-package #:mcgopher.gopher)

(defclass gopher-item ()
  ((content :initarg :content :accessor gopher-content)
   (category :initarg :category :accessor gopher-category)
   (location :initarg :location :accessor gopher-location)
   (host :initarg :host :accessor gopher-host)
   (gopher-port :initarg :port :accessor gopher-port)))

(defun read-until (separator stream &optional output)
  (let ((character (read-char stream nil nil)))
    (if (or (not character) (char= separator character))
        (coerce (reverse output) 'string)
        (read-until separator stream
                    (cons character output)))))

(defun response-to-item (response)
  (if (peek-char nil response nil nil)
       (make-instance 'gopher-item
                      :category (read-char response)
                      :content (read-until #\Tab response)
                      :location (read-until #\Tab response)
                      :host (read-until #\Tab response)
                      :port (remove #\Return (read-until #\Newline response)))))

(defun response-to-list (response)
  (aif (response-to-item response)
       (cons it (response-to-list response))))

(defun response-to-string (response)
  (with-output-to-string (stream)
    (loop for line = (read-line response nil)
       while line do (format stream "~a~%" line))))

(defun gopher-get (host port location)
  "Sends a request to a gopher server returning a list of gopher items"
  (let* ((socket (socket-connect host port :timeout 15))
         (stream (socket-stream socket)))
    (format stream "~a~%" location)
    (force-output stream)
    (response-to-list stream)))

(defun gopher-follow (item)
  (gopher-get (gopher-host item)
              (gopher-port item)
              (gopher-location item)))
