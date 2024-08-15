;;; +window.el --- Window Management in Emacs        -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is not part of GNU Emacs

;;; Commentary:

;;

;;; Code:

;; NOTE: might need to use `windresize' when resizing becomes cumbersome

(setopt focus-follows-mouse t
		mouse-autoselect-window t
		;; split window should split horizontally on laptop's fullscreen
		split-width-threshold 120

		pixel-scroll-precision-large-scroll-height 30
		pixel-scroll-precision-use-momentum t
		pixel-scroll-precision-interpolate-page t
		pixel-scroll-precision-interpolation-factor 1.0
		scroll-margin 0
		scroll-conservatively 1000
		scroll-preserve-screen-position 1)

(pixel-scroll-precision-mode 1)

(elpaca ace-window
  ;; Karthink's blog post has a very useful tricks to use embark.
  ;; And I also learned how to configure ace-window from there!
  ;; https://karthinks.com/software/fifteen-ways-to-use-embark/

  ;; TODO: integrate this and switch from `switch-window'

  ;; I have decided to use left-hand keys as window selector, and
  ;; right-hand keys for dispatch command.
  (setopt aw-keys '(?u ?e ?a ?p ?. ?, ?j ?' ?q)
		  aw-dispatch-alist '((?o aw-flip-window)    ; switch to previous window (belongs to left-hand)

							  (?g aw-delete-window "Delete Window")
							  (?m aw-swap-window "Swap Windows")
							  (?f aw-transpose-frame "Transpose Frame")

							  (?s aw-switch-buffer-in-window "Select Buffer")
							  (?n aw-execute-command-other-window "Execute Command Other Window")
							  (?r aw-switch-buffer-other-window "Switch Buffer Other Window")
							  (?v aw-split-window-vert "Split Vert Window")
							  (?h aw-split-window-horz "Split Horz Window")

							  (?z delete-other-windows "Delete Other Windows")
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

;; Taken from nhttps://karthinks.com/software/emacs-window-management-almanac/
(defun ra/ace-window-prefix ()
  "Use `ace-window' to display the buffer of the next command.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Creates a new window before displaying the buffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
  (interactive)
  (display-buffer-override-next-command
   (lambda (buffer _)
     (let (window type)
       (setq
        window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
        type 'reuse)
       (cons window type)))
   nil "[ace-window]")
  (message "Use `ace-window' to display next command buffer..."))

(with-eval-after-load 'ace-window
  (set-face-attribute 'aw-leading-char-face nil :height 2.0))

(defvar ra/eldoc-window nil)
(defun ra/eldoc-window-delete ()
  (when (window-live-p ra/eldoc-window)
	(delete-window ra/eldoc-window)
	(setq ra/eldoc-window nil)
	t))

;; Shameless plug from https://karthinks.com/software/emacs-window-management-almanac/
(defvar ra/other-window-alt-direction 1
  "The current `ra/other-window-alternating' direction")

(defun ra/other-window-alternating (&optional arg)
  "Call `other-window', switching directions each time."
  (interactive)
  (if (equal last-command 'ra/other-window-alternating)
	  (other-window (* ra/other-window-alt-direction (or arg 1)))
    (setq ra/other-window-alt-direction (- ra/other-window-alt-direction))
    (other-window (* ra/other-window-alt-direction (or arg 1)))))

(setopt other-window-scroll-default
      (lambda ()
        (or (get-mru-window nil t 'not-this-one-dummy) ; Need to set DEDICATED to `t'
			(next-window)					 ; fall back to next window
			(next-window nil nil 'visible))))

(setopt switch-to-buffer-obey-display-actions t ; Sane `switch-to-buffer'
		switch-to-buffer-in-dedicated-window 'pop
		;; TODO function for `display-buffer-alist'
		;; something that use keys as parameters and set `display-buffer-alist' and `popper-reference-buffer'.
		display-buffer-alist `((,(rx bol "*helpful" (1+ nonl) eol)
								(display-buffer-reuse-mode-window)
								(mode . helpful-mode))
							   (,(rx bol "*Man " (1+ nonl) eol)
								(display-buffer-reuse-mode-window)
								(mode . Man-mode))
							   (,(rx bol "*" (0+ nonl) "eshell" (0+ nonl) "*" eol)
								(display-buffer-in-direction)
								(direction . bottom)
								(window-height . 0.35)
								(dedicated . t))
							   (,(rx bol "*" (0+ nonl) "eat" (0+ nonl) "*" eol)
								(display-buffer-in-direction)
								(direction . bottom)
								(window-height . 0.35))
							   (,(rx bol "*" (0+ nonl) "Async Shell Command" (0+ nonl) "*" eol)
								(display-buffer-in-direction)
								(direction . bottom)
								(window-height . 0.35)
								(dedicated . t))
							   (,(rx bol "*eldoc*" eol)
								(display-buffer-in-direction)
								(direction . bottom)
								(window-height . ,(lambda (w)
													(fit-window-to-buffer w 40 1)))
								(window-parameters . ((mode-line-format . none)))
								(body-function . ,(lambda (w)
													(setq ra/eldoc-window w))))
							   (,(rx bol "*compilation*" eol)
								(display-buffer-in-side-window)
								(direction . bottom)
								(window-height . 0.35))
							   (,(rx bol "*pair-tree*" eol)
								(display-buffer-in-direction)
								(direction . bottom)
								(window-atom . main))
							   (,(rx bol "*sexp*" eol)
								(display-buffer-in-atom-window)
								(side . below))
							   (,(rx bol "*HTTP Response" (0+ nonl) "*" eol)
								(display-buffer-in-side-window)
								(side . right)
								(slot . 2))
							   (,(rx bol "*HTTP Headers" (0+ nonl) "*" eol)
								(display-buffer-in-side-window)
								(side . right)
								(slot . 1)
								(mode-line-format . ""))))

(with-eval-after-load '+mappings
  (add-hook 'ra/keyboard-quit-hook #'ra/eldoc-window-delete))

(elpaca popper
  (ra/keymap-set (current-global-map)
	"M-`" #'popper-toggle
	"M-~" #'popper-cycle)
  (setopt popper-display-control nil
		  popper-reference-buffers `(,(rx "*Messages*")
									 ,(rx "Output*" eol)
									 ,(rx "*Async Shell Command*")
									 help-mode
									 compilation-mode
									 ,(rx "*" (0+ nonl) "eat" (0+ nonl) "*")
									 eat-mode
									 ,(rx "*" (0+ nonl) "eshell" (0+ nonl) "*")
									 eshell-mode
									 ,(rx bol "*eldoc*" eol)
									 ,(rx bol "*HTTP Response" (0+ nonl) "*" eol)
									 ,(rx bol "*HTTP Headers" (0+ nonl) "*" eol)))
  (popper-mode +1)
  (popper-echo-mode +1))

(winner-mode 1)

(provide '+window)
;;; +window.el ends here
