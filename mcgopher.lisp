;;;; mcgopher.lisp

(in-package #:mcgopher)

(define-application-frame superapp ()
  ((history :type queue
            :initform (make-queue :elements '("gopher.floodgap.com/1")
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
                                         (redisplay-frame-pane (get-frame-pane
                                                          *application-frame*
                                                          'content))))
   (address :text-field
            :value (queue-front (page-history *application-frame*))
            :activate-callback #'(lambda (gadget)
                                   (asetf (page-history *application-frame*)
                                          (queue-push (gadget-value gadget) it)))
            :text-style (make-text-style :fix :roman :very-large))
   (go-button :push-button
              :label "Go"
              :activate-callback #'(lambda (gadget) (declare (ignore gadget))
                                           (activate-gadget-callback
                                            (find-pane-named *application-frame*
                                                             'address))))
   (content :application
        :incremental-redisplay t
        :display-function 'display-app
        :text-style (make-text-style :fix :roman :very-large))
   (int :interactor-pane))
  (:layouts
   (default (vertically ()
              (1/12 (horizontally (:x-spacing 5)
                      back-button refresh address go-button))
              (10/12 content)
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
  (redisplay-frame-pane *application-frame*
                        (get-frame-pane *application-frame* 'content)))

;; Display Functions

(define-presentation-type gopher-item ())

(defmethod display-item (pane (item gopher-item))
  "Displays a Gopher item based on the category."
  (case (gopher-category item)
    (#\i (format pane "~A~%" (gopher-content item)))
    (#\3 (with-drawing-options (pane :ink +red+)
           (format pane "~A~%" (gopher-content item))))
    (t (with-output-as-presentation (pane item 'gopher-item)
         (with-text-face (pane :bold)
           (format pane "~A~%" (gopher-content item)))))))

(defun display-app (frame pane)
  "Draws Gopher items to the frame by pulling data from the current page."
  (loop for item in (gopher-goto (queue-front (page-history frame)))
     do (updating-output (pane :unique-id item :cache-value (gopher-content item))
          (display-item pane item))))

;; Commands

(define-superapp-command (com-quit :menu "Quit" :name t) ()
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

(define-superapp-command com-goto ((item 'gopher-item :gesture :select))
  "Follows a Gopher item's URL."
  (asetf (page-history *application-frame*)
         (queue-push (gopher-item-to-address item) it)))

(define-superapp-command (com-previous :name t :keystroke (:left :meta)) ()
  "Moves the history back one item."
  (if (> (queue-length (page-history *application-frame*)) 1)
      (asetf (page-history *application-frame*)
             (queue-next it))))

;; Main

(defun main ()
  "Main entry point to McGopher"
  (run-frame-top-level (make-application-frame 'superapp :width 1200)))
