;;;; mcgopher.lisp

(in-package #:mcgopher)

(define-application-frame superapp ()
  ((history :initform (make-queue :elements '("gopher.floodgap.com/1")
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
            :text-style (make-text-style :fix :roman :huge))
   (go-button :push-button
              :label "Go"
              :activate-callback 'go-callback)
   (app :application
        :incremental-redisplay t
        :display-function 'display-app
        :text-style (make-text-style :sans-serif :roman :very-large))
   (int :interactor-pane))
  (:layouts
   (default (vertically ()
              (1/12 (horizontally (:x-spacing 5) back-button address go-button))
              (10/12 app) (1/12 int)))))

;; Callbacks

(defun address-callback (gadget)
  (asetf (page-history *application-frame*)
         (queue-push (gadget-value gadget) it))
  (redisplay-frame-pane *application-frame*
                        (get-frame-pane *application-frame* 'app)
                        :force-p t))

(defun go-callback (gadget)
  (declare (ignore gadget))
  (address-callback (find-pane-named *application-frame* 'address)))

(defun page-previous ()
  (unless (string= (queue-front (page-history *application-frame*))
                   (queue-front (queue-next (page-history *application-frame*))))
    (asetf (page-history *application-frame*)
           (queue-next it))
    (setf (gadget-value (find-pane-named *application-frame* 'address))
          (queue-front (page-history *application-frame*)))
    (redisplay-frame-pane *application-frame*
                          (get-frame-pane *application-frame* 'app)
                          :force-p t)))

;; Display Funcions

(define-presentation-type gopher-item ())

(defmethod display-item (pane (item gopher-item))
  (case (gopher-category item)
    (#\i (format pane "~A~%" (gopher-content item)))
    (#\3 (with-text-face (pane :italic)
           (format pane "~A~%" (gopher-content item))))
    (t (with-output-as-presentation (pane item 'gopher-item)
         (with-text-face (pane :bold)
           (format pane "~A~%" (gopher-content item)))))))

(defun display-app (frame pane)
  (loop for item in (gopher-goto (queue-front (page-history frame)))
     do (updating-output (pane :unique-id item)
          (display-item pane item))))

;; Commands

(define-superapp-command (com-quit :menu "Quit") ()
  (frame-exit *application-frame*))

(define-superapp-command com-goto ((item 'gopher-item :gesture :select))
  (let ((frame *application-frame*))
    (asetf (page-history frame) (queue-push
                                 (format nil "~A/~A~A"
                                        (gopher-host item)
                                        (gopher-category item)
                                        (gopher-location item))
                                 it))
    (setf (gadget-value (find-pane-named frame 'address))
          (queue-front (page-history frame)))))

(define-superapp-command (com-previous :name t :keystroke (:left :hyper)) ()
  (page-previous))

;; Main

(defun app-main ()
  (run-frame-top-level (make-application-frame 'superapp :width 1200)))
