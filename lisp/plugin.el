(require 'package)
(setq package-check-signature nil)
(setq package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

;;设置补全
(global-company-mode 1)
(global-set-key (kbd "<f2>") 'find-function)
(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0)
(package-install 'vertico)
(vertico-mode t)
(package-install 'orderless)
(setq completion-styles '(orderless))
(package-install 'marginalia)
(marginalia-mode t)

;;快捷键显示
(use-package which-key :ensure t :defer t
  :hook (after-init . which-key-mode))

;;加强搜索
(package-install 'consult)
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "<f1>") 'consult-imenu)

;;主题
(use-package all-the-icons
  :ensure t
  :defer t
  :init
  (set-frame-font "all-the-icons" t))
(package-install 'doom-themes)
(load-theme 'doom-monokai-octagon t)
(use-package nerd-icons)
(use-package simple
  :ensure nil
  :hook (after-init . size-indication-mode)
  :init
  (progn
    (setq column-number-mode t)
    ))
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode t))
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;;快速代码运行
(use-package quickrun
  :ensure t
  :commands (quickrun)
  :init
  (quickrun-add-command "c/gcc"
  '((:exec . ("%c  %o -o %e %s"
	      "%e %a")))
  :override t)
  )
(global-set-key (kbd "<f5>") 'quickrun)





(provide 'plugin)
