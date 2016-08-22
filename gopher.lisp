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
  "A single server response in the format of 'CATEGORY CONTENT #\Tab LOCATION
   #\Tab HOST #\Tab PORT #\Return #\Newline'."
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
  (let* ((socket (socket-connect host port :timeout 15))
         (stream (socket-stream socket)))
    (format stream "~a~%" location)
    (force-output stream)
    (case category
      (#\0 (list (make-instance 'gopher-item
                                :category #\i
                                :content (fix-formatting
                                          (response-to-string stream)))))
      (t (response-to-list stream)))))

(defun address-to-gopher-list (address)
  "Converts an address separated by '/' to a list of three elements. The first
   is the host, the second is the Gopher type, and third the location."
  (ppcre:split "[^a-zA-Z0-9_\\-.]"
               (ppcre:regex-replace "(.*?):\/\/" address "") :limit 3))

(defun gopher-goto (address)
  "Given an address, returns a list of Gopher items."
  (let* ((address (address-to-gopher-list address))
         (category-position (ppcre:scan "[0-9]+" (cadr address)))
         (category (aif category-position (char (cadr address) it) #\1))
         (location (aif (caddr address) it "")))
    (gopher-get :category category
                :host (car address)
                :port 70
                :location (format nil "/~A" location))))
