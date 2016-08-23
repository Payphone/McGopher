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
                                             (page-previous)))
   (address :text-field
            :value (queue-front (page-history *application-frame*))
            :activate-callback 'address-callback
            :text-style (make-text-style :fix :roman :very-large))
   (go-button :push-button
              :label "Go"
              :activate-callback 'go-callback)
   (app :application
        :incremental-redisplay t
        :display-function 'display-app
        :text-style (make-text-style :fix :roman :very-large))
   (int :interactor-pane))
  (:layouts
   (default (vertically ()
              (1/12 (horizontally (:x-spacing 5) back-button address go-button))
              (10/12 app)
              (1/12 int)))))

;; Callbacks

(defmethod (setf page-history) :after ((history queue) (new-history queue))
  (declare (ignore history new-history))
  (redisplay-frame-pane *application-frame*
                        (get-frame-pane *application-frame* 'app)
                        :force-p t))

(defun address-callback (gadget)
  "Sets the address gadget value and the frame history, then refreshes the
   frame."
  (asetf (page-history *application-frame*)
         (queue-push (gadget-value gadget) it)))

(defun go-callback (gadget)
  "Calls the address-callback."
  (declare (ignore gadget))
  (address-callback (find-pane-named *application-frame* 'address)))

(defmethod page-previous ()
  "Moves the history back one element and updates the address."
  (unless (string= (queue-front (page-history *application-frame*))
                   (queue-front (queue-next (page-history *application-frame*))))
    (asetf (page-history *application-frame*)
           (queue-next it))
    (setf (gadget-value (find-pane-named *application-frame* 'address))
          (queue-front (page-history *application-frame*)))))

;; Display Functions

(define-presentation-type gopher-item ())

(defmethod display-item (pane (item gopher-item))
  "Displays a Gopher item."
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

(define-superapp-command (com-quit :menu "Quit") ()
  "Exits the application."
  (frame-exit *application-frame*))

(define-superapp-command com-goto ((item 'gopher-item :gesture :select))
  "Follows a Gopher item's URL."
  (let ((frame *application-frame*))
    (asetf (page-history frame) (queue-push (gopher-item-to-address item) it))
    (setf (gadget-value (find-pane-named frame 'address))
          (queue-front (page-history frame)))))

(define-superapp-command (com-previous :name t :keystroke (:left :meta)) ()
  "Moves back one page."
  (page-previous))

;; Main

(defun app-main ()
  "Main entry point to McGopher"
  (run-frame-top-level (make-application-frame 'superapp :width 1200)))
