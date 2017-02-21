;;;; mcgopher.lisp

(in-package #:mcgopher)

(define-application-frame superapp ()
  ((history :type queue
            :initform (make-queue :elements '("gopher.floodgap.com")
                                  :max-size 10)
            :accessor page-history))
  (:pointer-documentation t)
  (:panes
   (back-button :push-button
                :label "Back"
                :activate-callback #'(lambda (gadget) (declare (ignore gadget))
                                       (com-previous)))
   (refresh :push-button
            :label "Refresh"
            :activate-callback #'(lambda (gadget) (declare (ignore gadget))
                                   (redisplay-frame-pane *application-frame*
                                                         'app :force-p t)))
   (address :text-field
            :value (queue-front (page-history *application-frame*))
            :activate-callback #'(lambda (gadget)
                                   (asetf (page-history *application-frame*)
                                          (queue-push (gadget-value gadget) it)))
            :text-style (make-text-style :fix :roman *font-size*))
   (go-button :push-button
              :label "Go"
              :activate-callback #'(lambda (gadget) (declare (ignore gadget))
                                     (activate-gadget-callback
                                      (find-pane-named *application-frame*
                                                       'address))))
   (app :application
        :incremental-redisplay t
        :display-function 'display-app
        :text-style (make-text-style :fix :roman *font-size*)
        :background *background-color*
        :foreground *foreground-color*))
  (:layouts
   (default (vertically ()
              (1/12 (horizontally (:x-spacing 5)
                                  back-button refresh address go-button))
              (10/12 app)))))

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

;; Display Functions

;; Create presentation types from gopher content types and define the default
;; present method.

(macrolet
    ((generate-present-methods ()
       `(progn
          ,@(loop for item in *content-types*
               for class = (symb (cdr item))
               collect `(define-presentation-method present
                            (object (type ,class) stream (view textual-view)
                                    &key acceptably)
                          (declare (ignore acceptably))
                          (with-text-face (stream :bold)
                            (format stream "#<~A: ~A>~%" ',class
                                    (contents object))))))))

  (generate-present-methods))

(define-presentation-method present (object (type directory-list) stream
                                            (view textual-view) &key acceptably)
  (declare (ignorable acceptably))
  (with-text-face (stream :bold)
    (format stream "~A~%" (contents object))))

(define-presentation-method present (object (type information) stream
                                            (view textual-view) &key acceptably)
  (declare (ignorable acceptably))
  (format stream "~A~%" (contents object)))

(defun display-app (frame pane)
  "Draws Gopher items to the frame by pulling data from the current page."
  (loop for item in (gopher-goto (queue-front (page-history frame)))
     do (updating-output (pane :unique-id item :cache-value (contents item))
          (present item (presentation-type-of item) :stream pane))))

;; Commands

(define-superapp-command (com-quit :menu "Quit" :name t
                                   :keystroke (#\q :control)) ()
  "Exits the application."
  (frame-exit *application-frame*))

(define-superapp-command (com-history :menu "History") ()
  (let ((choice
         (menu-choose
          (mapc #'(lambda (url) `(,url :value ,url))
                (queue-elements (page-history *application-frame*))))))
    (if (typep choice 'string)
        (asetf (page-history *application-frame*)
               (queue-push choice it)))))

(define-superapp-command com-goto ((item 'directory-list :gesture :select))
  "Follows a Gopher item's URL."
  (asetf (page-history *application-frame*)
         (queue-push (mkstr (host item) (location item)) it)))

(define-superapp-command (com-previous :name t :keystroke (:left :meta)) ()
  "Moves the history back one item."
  (if (> (queue-length (page-history *application-frame*)) 1)
      (asetf (page-history *application-frame*)
             (queue-next it))))

;; Main

(defun main ()
  "Main entry point to McGopher"
  (run-frame-top-level (make-application-frame 'superapp :width 800)))
