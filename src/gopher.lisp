;;; gopher.lisp


(defpackage #:mcgopher.gopher
  (:use #:cl
        #:iolib
        #:alexandria
        #:split-sequence
        #:mcgopher.config
        #:mcgopher.utils)
  (:export ;; Gopher Content Type Classes
           #:content
           #:link
           #:downloadable
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
           ;; Content Methods
           #:contents
           #:location
           #:host
           #:gopher-port
           ;; Gopher Commands
           #:gopher-goto
           #:goto-text
           #:download
           #:content-address
           #:content->address))

(in-package #:mcgopher.gopher)

;;; Gopher Content Types

(defparameter *content-types*
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
    (#\T . tn3270-session-pointer))
  "Gopher content types organized in an association list in the format
  (char . content-type) where 'char' is the character the Gopher server
  associates with the following content type.")

(defclass content ()
  ((contents :initarg :contents :accessor contents :initform nil)
   (location :initarg :location :accessor location :initform nil)
   (host :initarg :host :accessor host :initform nil)
   (gopher-port :initarg :gopher-port :accessor gopher-port :initform nil))
  (:documentation "A generic Gopher content class."))

(defclass link () ()
  (:documentation "Used primarily by the GUI to identify which content types are
  'clickable'."))

(defclass downloadable (link) ()
  (:documentation "Used primarily by the GUI to identify which content types are
  downloadable."))

(defclass plain-text             (content downloadable) ())
(defclass directory-list         (content link) ())
(defclass cso-search-query       (content) ())
(defclass page-error             (content) ())
(defclass binhex-text            (content downloadable) ())
(defclass binary-archive         (content downloadable) ())
(defclass uuencoded-text         (content downloadable) ())
(defclass search-query           (content) ())
(defclass telnet-session-pointer (content) ())
(defclass binary-file            (content downloadable) ())
(defclass gif-image              (content downloadable) ())
(defclass html-file              (content downloadable) ())
(defclass information            (content) ())
(defclass unspecified-image      (content downloadable) ())
(defclass audio                  (content downloadable) ())
(defclass tn3270-session-pointer (content) ())

;;; Gopher Specific Utilities

(defun lookup (type)
  "Find the associated content type when given a character. Ex. #\g evaluates to
  gif-image."
  (cdr (assoc type *content-types*)))

(defun rlookup (class)
  "Given a content class name, returns the associated character. Ex. gif-image
  evaluates to #\g."
  (car (rassoc class *content-types* :test #'string=)))

(defun string->content (string)
  "Given a string from a Gopher server, attempts to create an instance of type
  category, where category is the content type.
  Ex. 'iInformative Information #\Tab null.host #\Tab 1' would create a new
  information instance with contents of 'Informative Information' and host
  'null.host'."
  (let* ((item (split-sequence:split-sequence #\Tab string))
         (category (lookup (first-elt (car item)))))
    (when category
      (make-instance category
                     :contents (remove-if (lambda (x) x) (nth 0 item) :end 1)
                     :location (nth 1 item)
                     :host (nth 2 item)
                     :gopher-port (remove #\Return (nth 3 item))))))

(defun infer-content-type (address)
  "Tries to guess the content type from the address string. If it can't find the
  type, it is assumed to be a directory listing."
  (let* ((split-address (ppcre:split "[^a-zA-Z0-9_\\-.]" address :limit 3))
         (type (second split-address))
         (class-type (and (= (length type) 1) (lookup (first-elt type)))))
    (if class-type
        (values class-type (remove type split-address :test #'string=))
        (values (lookup #\1) split-address))))

(defmethod content->address ((content content))
  "Address used for inferring the content type. This is not the address read by
  the Gopher server."
  (format nil "~A/~A~A" (host content) (rlookup (type-of content))
          (or (location content) "")))

(defmethod content-address ((content content))
  "Address as read by the Gopher server. Note: The content type is not needed."
  (format nil "~A/~A" (host content) (location content)))

;;; Interacting With The Gopher Server

(defmacro with-gopher-socket ((var address) &body body)
  "Sends an address to a gopher server and stores the response in 'var' for use
  in the body."
  `(let* ((split-address (ppcre:split "[^a-zA-Z0-9_\\-.]" ,address :limit 2))
          (host (first split-address))
          (location (second split-address)))
     (handler-case
         (with-open-socket (,var :connect :active
                                 :address-family :internet
                                 :type :stream
                                 :external-format '(:utf-8 :eol-style :crlf)
                                 :ipv6 nil)
           (connect ,var (lookup-hostname host) :port 70 :wait t)
           (format ,var "/~A~%" (or location "/"))
           (force-output ,var)
           ,@body)
       (hangup ()
         (make-instance 'page-error :contents "Server closed connection
                                                    when attempting to write."))
       (end-of-file ()
         (make-instance 'page-error :contents "Server closed connection
                                                    when attempting to read."))
       (socket-connection-reset-error ()
         (make-instance 'page-error :contentes "Connection reset by peer."))
       (socket-connection-refused-error ()
         (make-instance 'page-error :contents "Connection refused."))
       (error ()
         (make-instance 'page-error :contents "Unable to load page.")))))

(defmethod gopher-goto ((object plain-text))
  "Returns the contents of the plain text file."
  (with-gopher-socket (socket (content-address object))
    (fix-formatting (read-stream-content-into-string socket))))

(defmethod gopher-goto ((object directory-list))
  "Returns a list of content items associated with the directory list."
  (with-gopher-socket (socket (content-address object))
    (loop for item = (string->content (read-line socket))
       until (null item)
       collect item)))

(defmethod gopher-goto ((content link))
  "Attempts to download from the link's address."
  (download (content-address content))
  (make-instance 'information :contents (format nil "Saved file to ~A"
                                                *download-folder*)))

(defmethod gopher-goto ((address string))
  "Given an address, returns a list of Gopher items."
  (multiple-value-bind (type split-address)
      (infer-content-type address)
    (gopher-goto (make-instance type :host (car split-address)
                                     :location (format nil "~{~A~^ ~}"
                                                       (cdr split-address))))))

(defun download (address &optional file-name)
  "Saves the server reply to the downloads folder and returns the file name."
  (let* ((name (or file-name
                   (lastcar (ppcre:split "[^a-zA-Z0-9_\\-.]" address))))
         (path (merge-pathnames *download-folder* name)))
    (with-gopher-socket (socket address)
      (write-byte-vector-into-file (read-stream-content-into-byte-vector socket)
                                   path :if-exists :supersede))
    (values path)))
