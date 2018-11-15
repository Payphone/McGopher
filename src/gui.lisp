;;;; mcgopher.lisp

(defpackage #:mcgopher.gui
  (:use #:clim
        #:clim-lisp
        :mcgopher.config
        #:mcgopher.utils
        #:mcgopher.gopher)
  (:import-from :alexandria
                :symbolicate
                :ensure-list)
  (:export
   #:main))

(in-package #:mcgopher.gui)

(define-application-frame mcgopher ()
  ((history :type queue
            :initform (make-queue :elements (list *homepage*)
                                  :max-size 10)
            :accessor page-history))
  (:menu-bar #.*menu-bar-p* :display-time t)
  (:pointer-documentation t)
  (:panes
   (back-button :push-button
                :display-time t
                :label "Back"
                :activate-callback #'(lambda (gadget) (declare (ignore gadget))
                                        (com-previous))
                :text-style *menu-font*
                :background *alt-background*
                :foreground *alt-foreground*)
   (refresh :push-button
            :display-time t
            :label "Refresh"
            :activate-callback #'(lambda (gadget) (declare (ignore gadget))
                                    (redisplay-frame-pane *application-frame*
                                                          'app))
            :text-style *menu-font*
            :background *alt-background*
            :foreground *alt-foreground*)
   (address :text-field
            :display-time t
            :text-style *menu-font*
            :value (queue-front (page-history *application-frame*))
            :activate-callback #'(lambda (gadget)
                                   (asetf (page-history *application-frame*)
                                          (queue-push (gadget-value gadget) it)))
            :background *background*
            :foreground *foreground*)
   (go-button :push-button
              :display-time t
              :label "Go"
              :activate-callback #'(lambda (gadget) (declare (ignore gadget))
                                      (activate-gadget-callback
                                       (find-pane-named *application-frame*
                                                        'address)))
              :text-style *menu-font*
              :background *alt-background*
              :foreground *alt-foreground*)
   (int :interactor)
   (app :application
        :display-function 'display-app
        :incremental-redisplay t
        :text-style *content-font*
        :background *background*
        :foreground *foreground*
        :height :compute))
  (:layouts
   (default
       (vertically ()
         (1/12 (horizontally (:spacing 5 :background *alt-background*)
                 (1/12 back-button) (1/12 refresh) (9/12 address)
                 (1/12 go-button)))
         (10/12 app)
         (1/12 int)))))

;;; Callbacks

(defun activate-gadget-callback (gadget)
  "Shorthand for activating a gadget's callback."
  (activate-callback gadget (gadget-client gadget) (gadget-id gadget)))

(defmethod (setf page-history) :after (history new-history)
  "When the page history changes, update the address bar and redraw the
   content pane with new content."
  (declare (ignore history new-history))
  (setf (gadget-value (find-pane-named *application-frame* 'address))
        (queue-front (page-history *application-frame*)))
  (redisplay-frame-pane *application-frame* 'app))

;;; Presentation Methods

(define-presentation-method present (object (type content) stream
                                            (view textual-view)
                                            &key acceptably)
  (declare (ignore acceptably))
  (with-text-face (stream :bold)
    (format stream "#<~A: ~A>~%" (type-of object) (contents object))))

(define-presentation-method present (object (type string) stream
                                            (view textual-view) &key acceptably)
  (declare (ignorable acceptably))
  (format stream "~A~%" object))

(define-presentation-method present (object (type directory-list) stream
                                            (view textual-view) &key acceptably)
  (declare (ignorable acceptably))
  (with-text-face (stream :bold)
    (format stream "~A~%" (contents object))))

(define-presentation-method present (object (type information) stream
                                            (view textual-view) &key acceptably)
  (declare (ignorable acceptably))
  (format stream "~A~%" (contents object)))

(define-presentation-method present (object (type gopher-address) stream
                                            (view textual-view) &key acceptably)
  (declare (ignorable acceptably))
  (format stream "~A~%" (gopher-address object)))

;;; Translators

(define-presentation-to-command-translator goto
    (link com-goto mcgopher
          :menu nil
          :gesture :select
          :documentation "Goto link"
          :pointer-documentation
          ((object stream)
           (format stream "~A" (content->address object))))
    (object)
  (list object))

(define-presentation-to-command-translator download
    (downloadable com-download mcgopher
                  :menu nil
                  :gesture :edit
                  :documentation "Download file"
                  :pointer-documentation
                  ((object stream)
                   (format stream "~A" (content-address object))))
    (object)
  (list object))

;;; Display Functions

(defun display-app (frame pane)
  "Presents the page at the top of the history."
  (let ((line-count 0))
    (loop for item in (ensure-list (gopher-goto (queue-front
                                                 (page-history frame)))) do
         (updating-output (pane :cache-value (contents item))
           (if *count-lines* (format pane "~A " (incf line-count)))
           (present item (presentation-type-of item) :stream pane)))))

;;; Commands

;; Menu Commands

(define-mcgopher-command (com-quit :menu "Quit" :name t :keystroke #.*key-quit*)
    ()
  "Exits the application."
  (frame-exit *application-frame*))

(define-mcgopher-command (com-history :name t :menu "History") ()
  "Shows previously visited links."
  (let ((choice
         (menu-choose
          (mapc #'(lambda (url) `(,url :value ,url))
                (queue-elements (page-history *application-frame*))))))
    (if (typep choice 'string)
        (asetf (page-history *application-frame*)
               (queue-push choice it)))))

(define-mcgopher-command (com-toggle-line-count :name t :menu "Line Count") ()
  (if *count-lines* (setf *count-lines* nil) (setf *count-lines* t))
  (com-refresh))

;;; Keystroke Commands

(define-mcgopher-command (com-refresh :name t :keystroke #.*key-refresh*) ()
  "Refreshes the page."
  (activate-gadget-callback (find-pane-named *application-frame* 'refresh)))

(define-mcgopher-command (com-previous :name t :keystroke #.*key-previous*) ()
  "Moves the history back one item."
  (if (> (queue-length (page-history *application-frame*)) 1)
      (asetf (page-history *application-frame*)
             (queue-next it))))

;;; Interactor Commands

(define-mcgopher-command (com-goto-address :name t) ((address string))
  "Opens the address."
  (asetf (page-history *application-frame*)
         (queue-push address it)))

(define-mcgopher-command (com-bookmark :name t) ()
  (let ((address (queue-front (page-history *application-frame*))))
    (if (not (search-file *bookmarks* address))
        (with-open-file (out *bookmarks* :direction :output :if-exists :append
                             :if-does-not-exist :create)
          (format out "~A~%" address)))))

(define-mcgopher-command (com-bookmarks :name t) ()
  (with-open-file (in *bookmarks*)
    (loop for bookmark = (read-line in nil) while bookmark do
         (present (make-instance 'gopher-address :gopher-address bookmark)
                  'gopher-address
                  :stream (find-pane-named *application-frame* 'int)))))


;;; Application Commands

(define-mcgopher-command com-goto ((object 'link :gesture :pointer-document))
  "Opens the object."
  (asetf (page-history *application-frame*)
         (queue-push (content->address object) it)))

(define-mcgopher-command com-goto-gopher-address ((object 'gopher-address
                                                          :gesture :select))
  "Opens the object."
  (asetf (page-history *application-frame*)
         (queue-push (gopher-address object) it)))

(define-mcgopher-command com-download ((object 'downloadable :gesture :edit))
  "Downloads the linked content."
  (download (content-address object)))

(define-mcgopher-command com-open-external
    ((object 'external :gesture :select :gesture edit))
  (uiop/run-program:run-program
   (format nil "xdg-open \"~A\"" (content-address object)))  )

(define-mcgopher-command com-open-external
    ((object 'html-file :gesture :select))
  (let ((location (location object)))
    (uiop/run-program:run-program
     (format nil "xdg-open \"~A\""
             (if (urlp location) (subseq location 4) location))))  )

;;; Main

(defun main ()
  "Main entry point to McGopher"
  (run-frame-top-level (make-application-frame 'mcgopher :pretty-name "McGopher"
                                               :width 800)))
