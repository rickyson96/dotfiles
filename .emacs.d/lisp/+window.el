;;; +window.el --- Window Management in Emacs        -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is not part of GNU Emacs

;;; Commentary:

;;

;;; Code:

(elpaca ace-window
  ;; Karthink's blog post has a very useful tricks to use embark.
  ;; And I also learned how to configure ace-window from there!
  ;; https://karthinks.com/software/fifteen-ways-to-use-embark/

  ;; I have decided to use left-hand keys as window selector, and
  ;; right-hand keys for dispatch command.
  (setopt aw-keys '(?u ?e ?a ?p ?. ?, ?k ?j ?i ?y ?x)
		  aw-dispatch-alist '((?o aw-flip-window)    ; switch to previous window (belongs to left-hand)
							  (?\M-o aw-flip-window) ; for use with "M-o" keybind

							  (?g aw-delete-window "Delete Window")
							  (?w aw-swap-window "Swap Windows")
							  (?v aw-move-window "Move Window")
							  (?b aw-copy-window "Copy Window")

							  (?c aw-switch-buffer-in-window "Select Buffer")
							  (?r aw-switch-buffer-other-window "Switch Buffer Other Window")
							  (?t aw-switch-to-window "Switch to Window")
							  (?n aw-split-window-vert "Split Vert Window")
							  (?h aw-split-window-horz "Split Horz Window")

							  (?m delete-other-windows "Delete Other Windows")
							  (?? aw-show-dispatch-help))
		  aw-make-frame-char ?\;
		  aw-dispatch-always t)

  (eval-when-compile
	(defmacro ra/embark-ace-action (fn)
      `(defun ,(intern (concat "ra/embark-ace-" (symbol-name fn))) ()
		 (interactive)
		 (with-demoted-errors "%s"
		   (require 'ace-window)
		   (let ((aw-dispatch-always t))
			 (aw-switch-to-window (aw-select nil))
			 (call-interactively (symbol-function ',fn)))))))

  (with-eval-after-load 'embark
	(ra/keymap-set embark-file-map
	  "o" (ra/embark-ace-action find-file))
	(ra/keymap-set embark-buffer-map
	  "o" (ra/embark-ace-action switch-to-buffer))
	(ra/keymap-set embark-bookmark-map
	  "o" (ra/embark-ace-action bookmark-jump))))


(elpaca switch-window
  (setopt switch-window-shortcut-style 'qwerty
		  switch-window-qwerty-shortcuts '("u" "e" "o" "a" "h" "t" "n" "s" "p" "." "," "k" "j" "i" "y" "x")
		  switch-window-background t
		  switch-window-threshold 2)
  (with-eval-after-load 'switch-window
	(set-face-attribute 'switch-window-label nil :height 4.0)))

(setopt switch-to-buffer-obey-display-actions t ; Sane `switch-to-buffer'
		display-buffer-alist `((,(rx bol "*eshell" (0+ nonl) "*" eol)
								(display-buffer-in-direction)
								(direction . bottom)
								(window-height . 0.3))
							   (,(rx bol "*" (0+ nonl) "eat" (0+ nonl) "*" eol)
								(display-buffer-in-direction)
								(direction . bottom)
								(window-height . 0.3))))

(provide '+window)
;;; +window.el ends here
