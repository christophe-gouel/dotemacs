;;; early-init.el --- Emacs early init -*- lexical-binding: t; -*-

;;; Commentary:

;; early-init.el is run before init.el, before package and UI initialization
;; happens.

;;; Code:

;; Defer garbage collection further back in the startup process
(setopt gc-cons-threshold 10000000
	gc-cons-percentage 0.6)

;; Default frame settings (faster to set them here before they are initialized)
(setopt default-frame-alist
	'((fullscreen . maximized) ; maximized screen, not full screen
	  (tool-bar-lines . 0) ; no tool bar
	  (vertical-scroll-bars) ; no vertical scroll bar
	  (undecorated . t))) ; no title bar

(unless (eq window-system 'ns)
  (push '(menu-bar-mode . 0) default-frame-alist)) ; no menu bar

(setopt frame-resize-pixelwise t) ; resize frame pixelwise

;;; early-init.el ends here
