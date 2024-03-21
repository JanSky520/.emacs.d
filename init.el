;; -*- lexical-binding: t -*-

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file 'no-error 'no-message)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'basic)
;(require 'plugin)
(require 'myfun)
