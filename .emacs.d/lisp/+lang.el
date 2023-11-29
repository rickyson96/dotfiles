;;; +lang.el --- Teach Emacs Various Languages       -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ricky Anderson

;; Author: Ricky Anderson <rickyson@thinky>
;; Keywords: local

;; This file is part of GNU Emacs

;;; Commentary:

;;

;;; Code:

(elpaca go-mode)
(elpaca gotest)
(elpaca go-tag)
(elpaca go-stacktracer)
(elpaca go-fill-struct)
(elpaca go-scratch)
(elpaca go-playground)

(elpaca nix-mode)

(elpaca macrostep)

;; sudo file
(elpaca etc-sudoers-mode)

(elpaca fish-mode)
(elpaca fish-completion)

;; literate calc
(elpaca literate-calc-mode)

(provide '+lang)
;;; +lang.el ends here
