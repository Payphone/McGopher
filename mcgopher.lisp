;;;; mcgopher.lisp

(in-package #:mcgopher)

(define-application-frame superapp ()
  ((page-host :initform "sdf.org" :accessor page-host)
   (page-location :initform "/" :accessor page-location))
  (:pointer-documentation t :menubar menubar-command-table)
  (:panes
   (address :text-field
            :value (page-location *application-frame*)
            :activate-callback 'address-callback)
   (app :application
        :incremental-redisplay t
        :display-function 'display-app
        :text-style (make-text-style :sans-serif :roman :very-large))
   (int :interactor-pane))
  (:layouts
   (default (vertically () (1/12 address) (10/12 app) (1/12 int)))))

(define-presentation-type gopher-item ())

(defmethod display-item (pane (item gopher-item))
  (case (gopher-category item)
    (#\i (format pane "~A~%" (gopher-content item)))
    (t (with-output-as-presentation (pane item 'gopher-item)
         (with-text-face (pane :bold)
           (format pane "~A~%" (gopher-content item)))))))

(defun app-main ()
  (run-frame-top-level (make-application-frame 'superapp :width 1200)))

(defun address-callback (gadget)
  (setf (page-location *application-frame*)
        (gadget-value gadget))
  (redisplay-frame-pane *application-frame*
                        (get-frame-pane *application-frame* 'app)
                        :force-p t))

(defun display-app (frame pane)
  (loop for item in (gopher-get (page-host frame)
                                70
                                (page-location frame))
     do (updating-output (pane :unique-id item)
          (display-item pane item))))

(define-superapp-command (com-set-url :name t) ()
  (format t "~A" (gadget-value (slot-value *application-frame* 'address))))

(define-superapp-command (com-quit :menu "Quit") ()
  (frame-exit *application-frame*))

(define-superapp-command com-goto ((item 'gopher-item :gesture :select))
  (let ((frame *application-frame*))
    (setf (page-host frame) (gopher-host item))
    (setf (page-port frame) (parse-integer (gopher-port item)))
    (setf (page-location frame) (gopher-location item))
    (setf (gadget-value (find-pane-named frame 'address))
          (gopher-location item))))

(define-superapp-command com-get-url ((item 'gopher-item :gesture :describe))
  (format t "~A~A" (gopher-host item) (gopher-location item)))
