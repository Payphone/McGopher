;;; gopher.lisp

(defpackage #:mcgopher.gopher
  (:use #:cl
        #:mcgopher.utils
        #:usocket)
  (:export #:gopher-goto
           #:gopher-item
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

(defun response-to-item (response)
  "Given a stream containing a Gopher response, returns a Gopher item."
  (if (peek-char nil response nil nil)
       (make-instance 'gopher-item
                      :category (read-char response)
                      :content (read-until #\Tab response)
                      :location (read-until #\Tab response)
                      :host (read-until #\Tab response)
                      :port (remove #\Return (read-until #\Newline response)))))

(defun response-to-list (response)
  "Reads from a stream until it reaches the end, consing Gopher items into a
   list along the way."
  (aif (response-to-item response)
       (cons it (response-to-list response))))

(defun response-to-string (response)
  "Reads from a stream, returning the contents as string."
  (with-output-to-string (stream)
    (loop for line = (read-line response nil)
       while line do (format stream "~a~%" line))))

(defun fix-formatting (string)
  "Removes tabs and #\Return from a string."
  (tabs-to-spaces (remove #\Return string)))

(defun gopher-get (&key host port location category)
  "Sends a request to a gopher server returning a list of gopher items."
  (with-connected-socket (socket (socket-connect host port :timeout 15))
    (let ((stream (socket-stream socket)))
      (format stream "~a~%" location)
      (force-output stream)
      (case category
        (#\0 (list (make-instance 'gopher-item
                                  :category #\i
                                  :content (fix-formatting
                                            (response-to-string stream)))))
        (t (response-to-list stream))))))

(defun gopher-goto (address)
  "Given an address, returns a list of Gopher items."
  (let* ((split-address (ppcre:split "[^a-zA-Z0-9_\\-.]" address :limit 3))
         (host (first split-address))
         (category (scan-character "[0-9ghilsT]+" (second split-address)))
         (location (third split-address)))
    ;; Unless otherwise specified, assume the category is a directory list
    (gopher-get :category (aif category it #\1)
                :host host
                :port 70
                :location (cat "/" location))))
