;;; gopher.lisp

(defpackage #:mcgopher.gopher
  (:use #:cl
        #:iolib
        :files-and-folders
        :peyton-utils
        #:mcgopher.config
        #:mcgopher.utils)
  (:export #:contents
           #:location
           #:host
           #:gopher-port

           #:plain-text
           #:directory-list
           #:cso-search-query
           #:page-error
           #:binhex-text
           #:binary-archive
           #:uuencoded-text
           #:search-query
           #:telnet-session-pointer
           #:binary-file
           #:gif-image
           #:html-file
           #:information
           #:unspecified-image
           #:audio
           #:tn3270-session-pointer

           #:gopher-goto
           #:download))

(in-package #:mcgopher.gopher)

;; Gopher content types

(defclass content ()
  ((contents :initarg :contents :accessor contents :initform nil)
   (location :initarg :location :accessor location)
   (host :initarg :host :accessor host)
   (gopher-port :initarg :gopher-port :accessor gopher-port)))

(macrolet ((generate-content-classes ()
             `(progn
                ,@(loop for item in *content-types*
                        for class = (symb (cdr item))
                        collect `(defclass ,class (content) ())))))
  (generate-content-classes))

(defun lookup (type)
  "Find the associated content type."
  (intern (string (cdr (assoc type *content-types*))) :mcgopher.gopher))

(defun read-item (stream)
  "Given a stream from a Gopher server, attempts to read a Gopher item."
  (let ((category (lookup (read-char stream nil))))
    (when category
      (make-instance category
                     :contents (aif (read-until #\Tab stream) it "")
                     :location (read-until #\Tab stream)
                     :host (read-until #\Tab stream)
                     :gopher-port (remove #\Return
                                          (read-until #\Newline stream))))))

(defun read-items (stream)
  "Reads gopher items from a stream"
  (loop for item = (read-item stream)
     until (null item)
     collect item))

(defun fix-formatting (string)
  "Removes tabs and #\Return from a string."
  (tabs-to-spaces (remove #\Return string)))

(defun gopher-goto (address &optional (port 70))
  "Given an address, returns a list of Gopher items."
  (let* ((split-address (ppcre:split "[^a-zA-Z0-9_\\-.]" address :limit 2))
         (host (first split-address))
         (location (second split-address)))
    (handler-case
        (with-open-socket (socket :connect :active
                                  :address-family :internet
                                  :type :stream
                                  :external-format '(:utf-8 :eol-style :crlf)
                                  :ipv6 nil)
          (connect socket (lookup-hostname host) :port port :wait t)
          (format socket "/~A~%" (or location "/"))
          (force-output socket)
          (read-items socket))
      (error () (list (make-instance 'page-error :contents
                                     "Unable to load page."))))))

(defun download (address &key name (port 70))
  (when address
    (let* ((split-address (ppcre:split "[^a-zA-Z0-9_\\-.]" address :limit 2))
           (host (first split-address))
           (location (second split-address)))
      (with-open-socket (socket :connect :active
                                :address-family :internet
                                :external-format '(:utf-8 :eol-style :crlf)
                                :type :stream
                                :ipv6 nil)
        (connect socket (lookup-hostname host) :port port :wait t)
        (format socket "~A~%" location)
        (force-output socket)
        (with-open-file (out (merge-paths *download-folder* (or name "untitled"))
                             :direction :output
                             :if-exists :supersede
                             :element-type '(unsigned-byte 8))
          (loop for byte = (read-byte socket nil)
             until (null byte) do
               (write-byte byte out)))
        (value name)))))
