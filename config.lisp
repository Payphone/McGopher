;; config.lisp

(defpackage #:mcgopher.config
  (:use #:clim #:clim-lisp)
  (:export #:*download-folder*
           #:*font-size*
           #:*background-color*
           #:*foreground-color*
           #:*key-previous*
           #:*key-quit*
           #:*external-programs*))

(in-package #:mcgopher.config)

;; Appearance
(defvar *font-size* :large)
(defvar *background-color* +white+)
(defvar *foreground-color* +black+)

;; Keybindings
(defvar *key-previous* `(:left :meta))
(defvar *key-quit* `(#\q :control))

;; Misc
(defvar *download-folder* #P"~/Downloads")
(defvar *external-programs*
  '((gif-image         . "feh")
    (unspecified-image . "feh")
    (audio             . "mpv --no-video")))
