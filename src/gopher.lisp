;;; gopher.lisp

(defpackage #:mcgopher.gopher
  (:use #:cl
        #:iolib
        #:alexandria
        #:split-sequence
        #:mcgopher.config
        #:mcgopher.utils)
  (:export
   ;; Gopher Content Type Classes
   #:gopher-address
   #:content
   #:link
   #:downloadable
   #:external
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
   #:urlp
   #:gopher-goto
   #:goto-text
   #:download
   #:content-address
   #:content->address))

(in-package #:mcgopher.gopher)

;;; Gopher Content Types

(defconstant +gopher-port+ 70 "The standard Gopher port.")

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

(defclass gopher-address ()
  ((gopher-address :reader gopher-address
                   :initarg :gopher-address
                   :type string
                   :documentation "The gopher address.")))

(defclass content ()
  ((contents :reader contents
             :initarg :contents
             :type string
             :documentation "The contents of a content type.")
   (location :reader location
             :initarg :location
             :type string
             :documentation "The selector or path of the content.")
   (host :reader host
         :initarg :host
         :type string
         :documentation "The hostname associated with the content.")
   (gopher-port :reader gopher-port
                :initarg :gopher-port
                :type integer
                :documentation "The port used by the host to deliver
                the content."))
  (:documentation "A generic Gopher content class."))

(defclass link () ()
  (:documentation "Used primarily by the GUI to identify which content types can
  be displayed in the browser."))

(defclass external () ()
  (:documentation "Used primarily by the GUI to identify which content types can
  be opened with external programs."))

(defclass downloadable () ()
  (:documentation "Used primarily by the GUI to identify which content types can
  be downloaded as a file."))

(defclass plain-text             (content link external downloadable) ())
(defclass directory-list         (content link) ())
(defclass cso-search-query       (content) ())
(defclass page-error             (content) ())
(defclass binhex-text            (content external downloadable) ())
(defclass binary-archive         (content downloadable) ())
(defclass uuencoded-text         (content external downloadable) ())
(defclass search-query           (content) ())
(defclass telnet-session-pointer (content) ())
(defclass binary-file            (content downloadable) ())
(defclass gif-image              (content external downloadable) ())
(defclass html-file              (content external) ())
(defclass information            (content) ())
(defclass unspecified-image      (content external downloadable) ())
(defclass audio                  (content external downloadable) ())
(defclass tn3270-session-pointer (content) ())

;;; Gopher Specific Utilities

(defun urlp (string)
  (string= (subseq string 0 4) "URL:"))

(defun lookup (type)
  "Find the associated content type when given a character. Ex. #\g evaluates to
  gif-image."
  (cdr (assoc type *content-types*)))

(defun rlookup (class)
  "Given a content class name, returns the associated character. Ex. gif-image
  evaluates to #\g."
  (car (rassoc class *content-types* :test #'string=)))

(defun ->content (string)
  "Given a string from a Gopher server, attempts to create an instance of type
  category, where category is the content type.
  Ex. 'iInformative Information 	null.host 	1' would create a new
  information instance with contents of 'Informative Information' and host
  'null.host'."
  (let* ((item (split-sequence:split-sequence #\Tab string))
         (category (lookup (first-elt (nth 0 item)))))
    (when category
      (make-instance category
                     :contents (subseq (nth 0 item) 1)
                     :location (nth 1 item)
                     :host (nth 2 item)
                     :gopher-port (parse-integer (nth 3 item) :junk-allowed t)))))

(defun infer-content-type (address)
  "Tries to guess the content type from the address string. If it can't find the
  type, it is assumed to be a directory listing."
  (let* ((split-address (ppcre:split "[^a-zA-Z0-9_\\-.]" address :limit 3))
         (type (second split-address))
         (class-type (and (= (length type) 1) (lookup (first-elt type)))))
    (if class-type
        (values class-type (remove type split-address :test #'string=))
        (values (lookup #\1) split-address))))

(defmethod contents ((string string)) string)
(defmethod contents ((null null)))

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
         (make-instance 'page-error :contents "Connection reset by peer."))
       (socket-connection-refused-error ()
         (make-instance 'page-error :contents "Connection refused."))
       (error ()
         (make-instance 'page-error :contents "Unable to load page.")))))

(defmethod gopher-goto ((object plain-text))
  "Returns the contents of the plain text file."
  (with-gopher-socket (socket (content-address object))
    (loop for line = (read-line socket nil nil) while line collect
         (fix-formatting line))))

(defmethod gopher-goto ((object directory-list))
  "Returns a list of content items associated with the directory list."
  (with-gopher-socket (socket (content-address object))
    (loop for line = (read-line socket nil nil) while line collect
         (->content line))))

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
