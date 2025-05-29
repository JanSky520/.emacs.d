(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (unless (bound-and-true-p package--initialized)
    (package-initialize)))

;;代码高亮
(use-package treesit
  :config (setq treesit-font-lock-level 4)
  :init
  (setq treesit-language-source-alist
    '((elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
      (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
      (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
      (org        . ("https://github.com/milisims/tree-sitter-org"))
      (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
      (sql        . ("https://github.com/m-novikov/tree-sitter-sql"))
      (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))))
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode)))

;;eglot配置
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '((c++-ts-mode c-ts-mode) "clangd"))
  :hook
  (c++-ts-mode . eglot-ensure)
  (c-ts-mode . eglot-ensure))
(use-package nasm-mode
  :ensure t
  :mode ("\\.asm\\'" "\\.inc\\'")
  :config (define-key nasm-mode-map (kbd "C-c C-c") 'nasm-compile))
(use-package fish-mode
  :ensure t)

;;设置补全
(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :init
  (progn
    (setq corfu-auto t)
    (setq corfu-cycle t)
    (setq corfu-quit-at-boundary t)
    (setq corfu-quit-no-match t)
    (setq corfu-preview-current t)
    (setq corfu-min-width 40)
    (setq corfu-max-width 50)
    (setq corfu-auto-delay 0.2)
    (setq corfu-auto-prefix 1)
    (setq corfu-on-exact-match nil)
    (setq corfu-preselect 'promt)
    (corfu-popupinfo-mode)))
(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))
(use-package which-key
  :config
  (which-key-mode 1))

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
  (setq dashboard-items '((recents . 7))))

;;主题
(use-package nerd-icons
  :ensure t)
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))


;;org美化
(use-package org-modern
  :ensure t
  :hook (org-mode . org-modern-mode))



(provide 'plugin)
