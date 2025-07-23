;; -*- lexical-binding: t -*-

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file 'no-error 'no-message)

(org-babel-load-file "~/.emacs.d/emacs.org")
