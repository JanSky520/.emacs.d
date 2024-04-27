(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (unless (bound-and-true-p package--initialized)
  (package-initialize)))

;;语法高亮
(use-package treesit-auto
  :ensure t
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
(use-package flymake
  :hook (prog-mode . flymake-mode)
  :hook (flymake-mode . (lambda ()
                          (setq eldoc-documentation-functions
                                (cons 'flymake-eldoc-function
                                      (delq 'flymake-eldoc-function
                                            eldoc-documentation-functions)))))
  :init (setq elisp-flymake-byte-compile-load-path (cons "./" load-path)))

;;org配置
(use-package org-superstar
  :ensure t
  :after org
  :hook (org-mode . org-superstar-mode))

;;启动面板
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :init
  (setq dashboard-banner-logo-title "你若安好，便是晴天")
  (setq dashboard-init-info "中国科学院大学沈阳计算技术研究所")
  (setq dashboard-footer-messages '("落霞与孤鹜齐飞 秋水共长天一色"))
  (setq dashboard-startup-banner 'official)
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-items '((recents . 5))))

;;增强minibuffer
(use-package vertico
  :ensure t
  :init (vertico-mode))
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
(use-package marginalia
  :ensure t
  :init (marginalia-mode))
(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("M-s" . consult-imenu)
         ("C-x b" . consult-buffer)))
(use-package recentf
  :init (recentf-mode t)
  :config (setq recentf-max-menu-items 10))

;;设置补全
(use-package company
  :ensure t
  :init
  (global-company-mode t)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0))
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;;翻译插件
(use-package sdcv
  :ensure t
  :bind (("M-a" . sdcv-search-pointer+)
         ("M-r" . sdcv-search-input+))
  :config
  (setq sdcv-dictionary-simple-list
        '("简明英汉字典增强版")))

;;主题
(use-package nerd-icons
  :ensure t
  )
(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-monokai-classic t))
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode t))

;;终端
(use-package vterm
  :ensure t
  :config )
(use-package vterm-toggle
  :ensure t
  :bind ("<f2>" . vterm-toggle)
  :config )
(setq vterm-toggle-fullscreen-p nil)
(add-to-list 'display-buffer-alist
             '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                (display-buffer-reuse-window display-buffer-at-bottom)
                (reusable-frames . visible)
                (window-height . 0.23)))

(add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))


(provide 'plugin)
