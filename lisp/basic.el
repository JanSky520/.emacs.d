(global-display-line-numbers-mode 1)
;;取消开始界面
(setq inhibit-startup-screen t)
;;关闭工具栏
(tool-bar-mode -1)
;;关闭菜单栏
(menu-bar-mode -1)
;;关闭文件滑动控件
(scroll-bar-mode -1)
;;更改光标的样式
(setq-default cursor-type 'bar)
;;括号匹配
(electric-pair-mode t)
;;关闭自动备份
(setq make-backup-files nil)
;;让鼠标滚动更好用
(setq mouse-wheel-scroll-amount '(3 ((shift) . 5) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;;括号匹配
(show-paren-mode t)
;;会将lambda等符号美化为λ
(add-hook 'prog-mode-hook 'prettify-symbols-mode)

;; 设置tab缩进
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;;最近打开文件
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)
(global-set-key (kbd "C-x C-b") 'consult-buffer)
;;全选加强
(delete-selection-mode 1)
;;开启C语言
(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eglot-ensure)
(global-set-key (kbd "C-/") 'eglot-format)

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                 (setq doom-modeline-icon t)
                (with-selected-frame frame
                  (efs/set-font-faces))))
    (efs/set-font-faces))

;;关掉哔哔声
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)
;;自动加载外部文件
(global-auto-revert-mode 1)
(setq auto-save-default nil)








(provide 'basic)
