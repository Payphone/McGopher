;; config.lisp

(defpackage #:mcgopher.config
  (:use #:clim #:clim-lisp)
  (:export #:*downloads-folder*
           #:*font-size*
           #:*background-color*
           #:*foreground-color*))

(in-package #:mcgopher.config)

(defvar *downloads-folder* #P"~/Downloads")
(defvar *font-size* :large)
(defvar *background-color* +white+)
(defvar *foreground-color* +black+)
(defvar *external-programs*
  '((gif . "feh")
    (wav . "mpv")
    (html . "firefox")
    ()))
