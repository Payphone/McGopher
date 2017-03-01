;;;; mcgopher.lisp

(defpackage #:mcgopher.gui
  (:use #:clim
        #:clim-lisp
        #:files-and-folders
        #:mcgopher.config
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
  (:pointer-documentation t)
  (:panes
   (back-button :push-button
                :label "Back"
                :activate-callback #'(lambda (gadget) (declare (ignore gadget))
                                             (com-previous))
                :background *alt-background*
                :foreground *alt-foreground*)
   (refresh :push-button
            :label "Refresh"
            :activate-callback #'(lambda (gadget) (declare (ignore gadget))
                                   (redisplay-frame-pane *application-frame*
                                                         'app :force-p t))
            :background *alt-background*
            :foreground *alt-foreground*)
   (address :text-field
            :text-style (make-text-style :fix :roman :large)
            :value (queue-front (page-history *application-frame*))
            :activate-callback #'(lambda (gadget)
                                   (asetf (page-history *application-frame*)
                                          (queue-push (gadget-value gadget) it)))
            :text-style (make-text-style :fix :roman *font-size*)
            :background *background*
            :foreground *foreground*)
   (go-button :push-button
              :label "Go"
              :activate-callback #'(lambda (gadget) (declare (ignore gadget))
                                     (activate-gadget-callback
                                      (find-pane-named *application-frame*
                                                       'address)))
              :background *alt-background*
              :foreground *alt-foreground*)
   (int :interactor)
   (app :application
        :incremental-redisplay t
        :display-function 'display-app
        :text-style (make-text-style :fix :roman *font-size*)
        :background *background*
        :foreground *foreground*))
  (:layouts
   (default
       (vertically ()
         (1/12 (horizontally (:spacing 5 :background *alt-background*)
                 back-button refresh address go-button))
         (10/12 app)
         (1/12 int)))))

;; Callbacks

(defun activate-gadget-callback (gadget)
  (activate-callback gadget (gadget-client gadget) (gadget-id gadget)))

(defmethod (setf page-history) :after (history new-history)
  "When the page history changes update the address bar and redraw the
   content pane."
  (declare (ignore history new-history))
  (setf (gadget-value (find-pane-named *application-frame* 'address))
        (queue-front (page-history *application-frame*)))
  (redisplay-frame-pane *application-frame* 'app))

;; Presentation Methods

(define-presentation-method present (object (type content) stream
                                            (view textual-view)
                                            &key acceptably)
  (declare (ignore acceptably))
  (with-text-face (stream :bold)
    (format stream "#<~A: ~A>~%" (type-of object) (contents object))))

(define-presentation-method present (object (type directory-list) stream
                                            (view textual-view) &key acceptably)
  (declare (ignorable acceptably))
  (with-text-face (stream :bold)
    (format stream "~A~%" (contents object))))

(define-presentation-method present (object (type information) stream
                                            (view textual-view) &key acceptably)
  (declare (ignorable acceptably))
  (format stream "~A~%" (contents object)))

;; Translators

(define-presentation-to-command-translator goto
    (link com-goto mcgopher
          :menu nil
          :gesture :select
          :documentation "Goto link"
          :pointer-documentation "Goto link")
    (object)
  (list object))

(define-presentation-to-command-translator download
    (link com-goto mcgopher
          :menu nil
          :gesture :edit
          :documentation "Download file"
          :pointer-documentation "Download file")
    (object)
  (list object))

;; Display Functions

(defun display-app (frame pane)
  "Presents items read frnom the gopher server. Either a directory listing or
  plain text."
  (loop for item in (ensure-list (gopher-goto (queue-front
                                               (page-history frame)))) do
       (updating-output (pane :unique-id item)
         (present item (presentation-type-of item) :stream pane))))

;; Commands

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

(define-mcgopher-command (com-refresh :name t :keystroke #.*key-refresh*) ()
  (activate-gadget-callback (find-pane-named *application-frame* 'refresh)))

(define-mcgopher-command com-goto ((object 'link :gesture :select))
  "Opens the object."
  (asetf (page-history *application-frame*)
         (queue-push (content->address object) it)))

(define-mcgopher-command (com-goto :name t) ((address string))
  (asetf (page-history *application-frame*)
         (queue-push address it)))

(define-mcgopher-command (com-previous :name t :keystroke #.*key-previous*) ()
  "Moves the history back one item."
  (if (> (queue-length (page-history *application-frame*)) 1)
      (asetf (page-history *application-frame*)
             (queue-next it))))

(define-mcgopher-command com-download ((object 'link :gesture :edit))
  "Downloads the linked content."
  (download (content-address object)))

(define-mcgopher-command (com-download :name t) ((address string))
  "Downloads the linked content."
  (download address))

(macrolet ((generate-external-commands ()
             "Creates commands for opening content in external programs."
             `(progn
                ,@(loop for item in *external-programs*
                     for class = (car item)
                     for program = (cdr item)
                     collect
                       `(define-mcgopher-command ,(symbolicate 'com-open- class)
                            ((object ',(symbolicate class) :gesture :select))
                          (let ((path (download (content-address object))))
                            (uiop/run-program:run-program
                             (format nil "~A ~A" ,program path))
                            (uiop/filesystem:delete-file-if-exists path)))))))
  (generate-external-commands))

;; Main

(defun main ()
  "Main entry point to McGopher"
  (run-frame-top-level (make-application-frame 'mcgopher :pretty-name "McGopher"
                                               :width 800)))
