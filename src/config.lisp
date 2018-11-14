;;;; config.lisp

(defpackage #:mcgopher.config
  (:use #:clim #:clim-lisp)
  (:export
   ;; Appearance
   #:*menu-font*
   #:*content-font*
   #:*content-bold-font*
   #:*menu-bar-p*
   #:*count-lines*
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
   #:*bookmarks*
   #:*homepage*))

(in-package #:mcgopher.config)

;;; Appearance

(defvar *menu-font* (make-text-style :fix :roman :normal))
(defvar *content-font* (make-text-style :fix :roman :normal))
(defvar *menu-bar-p* t)
(defvar *count-lines* nil)
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
(defvar *bookmarks* #P "~/.config/mcgopher/bookmarks")
(defvar *homepage* "gopher.floodgap.com")
