;; config.lisp

(defpackage #:mcgopher.config
  (:use #:clim #:clim-lisp)
  (:export ;; Appearance
           #:*font-size*
           #:*background*
           #:*foreground*
           #:*alt-background*
           #:*alt-foreground*
           ;; Keybindings
           #:*key-previous*
           #:*key-quit*
           #:*key-refresh*
           ;;Misc
           #:*download-folder*
           #:*homepage*
           #:*external-programs*))

(in-package #:mcgopher.config)

;; Appearance
(defvar *font-size* :large)
(defvar *background* +white+)
(defvar *foreground* +black+)
(defvar *alt-background* +light-gray+)
(defvar *alt-foreground* +black+)

;; Keybindings
(defvar *key-previous* `(:left :meta))
(defvar *key-quit* `(#\q :control))
(defvar *key-refresh* `(:F5))

;; Misc
(defvar *download-folder* #P"~/Downloads")
(defvar *homepage* "gopher.floodgap.com")

(defconstant *external-programs*
  '((gif-image         . "feh")
    (unspecified-image . "feh")
    (audio             . "mpv")))
