;;;; config.lisp

(defpackage #:mcgopher.config
  (:use #:clim #:clim-lisp)
  (:export
   ;; Appearance
   #:*menu-font*
   #:*content-font*
   #:*menu-bar-p*
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
   #:*homepage*))

(in-package #:mcgopher.config)

;;; Appearance

(defvar *menu-font* (make-text-style :fix :roman :large))
(defvar *content-font* (make-text-style :fix :roman :large))
(defvar *menu-bar-p* t)
(defvar *background* +white+)
(defvar *foreground* +black+)
(defvar *alt-background* +light-gray+)
(defvar *alt-foreground* +black+)

;;; Keybindings

(defvar *key-previous* `(:left :meta))
(defvar *key-quit* `(#\q :control))
(defvar *key-refresh* `(:F5))

;;; Misc

(defvar *download-folder* #P"~/Downloads/")
(defvar *homepage* "gopher.floodgap.com")
