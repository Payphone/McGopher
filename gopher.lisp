;;; gopher.lisp

(defpackage #:mcgopher.gopher
  (:use #:cl
        #:iolib
        :files-and-folders
        :peyton-utils
        #:mcgopher.config
        #:mcgopher.utils)
  (:export #:*content-types*
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
           #:category
           #:contents
           #:location
           #:host
           #:gopher-port

           #:gopher-goto
           #:download))

(in-package #:mcgopher.gopher)

;; Gopher content types

(defvar *content-types*
  '((#\0 . plain-text)
    (#\1 . directory-list)
    (#\2 . cso-search-query)
    (#\3 . page-error)
    (#\4 . binhex-text)
    (#\5 . binary-archive)
    (#\6 . uuencoded-text)
    (#\7 . search-query)
    (#\8 . telnet-session-pointer)
    (#\9 . binary-file)
    (#\g . gif-image)
    (#\h . html-file)
    (#\i . information)
    (#\I . unspecified-image)
    (#\s . audio)
    (#\T . tn3270-session-pointer)))

(defclass content ()
  ((contents :initarg :contents :accessor contents :initform nil)
   (location :initarg :location :accessor location)
   (host :initarg :host :accessor host)
   (gopher-port :initarg :gopher-port :accessor gopher-port)))

;; Not for the weak of heart!
#.`(progn
     ,@(loop for item in *content-types*
          for class = (cdr item)
          collect `(defclass ,class (content) ())
          collect `(defmethod print-object ((object ,class) stream)
                     (print-unreadable-object (object stream :type t :identity t)
                       (princ (contents object))))))

(defun lookup (type)
  "Find the associated content type."
  (cdr (assoc type *content-types*)))

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
        (with-open-file (out (merge-paths *downloads-folder* name)
                             :direction :output
                             :if-exists :supersede
                             :element-type '(unsigned-byte 8))
          (loop for byte = (read-byte socket nil)
             until (null byte) do
               (write-byte byte out)))))))
