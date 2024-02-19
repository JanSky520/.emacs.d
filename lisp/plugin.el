(setq package-check-signature nil)
(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (unless (bound-and-true-p package--initialized)
    (package-initialize)))

;;设置补全
(use-package corfu
  :init
  (progn
    (setq corfu-auto t)
    (setq corfu-cycle t)
    (setq corfu-quit-at-boundary t)
    (setq corfu-quit-no-match t)
    (setq corfu-preview-current nil)
    (setq corfu-min-width 40)
    (setq corfu-max-width 50)
    (setq corfu-auto-delay 0.2)
    (setq corfu-auto-prefix 1)
    (setq corfu-on-exact-match nil)
    (global-corfu-mode)
    ))

;;增强minibuffer
(use-package vertico
  :init
  (vertico-mode))
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
(use-package marginalia
  :init
  (marginalia-mode))
(use-package consult
  :bind (("C-s" . consult-line)
         ("M-s" . consult-imenu)))

;;语法高亮
(use-package treesit-auto
  :demand t
  :config
  (progn
    (setq treesit-auto-install 'prompt)
    (setq treesit-font-lock-level 4))
  (global-treesit-auto-mode))

;;eglot配置
(use-package eglot
  :config (add-to-list 'eglot-server-programs '((c++-ts-mode c-ts-mode) "clangd"))
  :hook ((c++-ts-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)))

;;org配置
(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))







(provide 'plugin)
