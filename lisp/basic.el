(setq inhibit-startup-screen t)    ;;关闭开始界面
(set-face-attribute 'default nil :font "Sarasa Term SC Nerd 14")
(global-display-line-numbers-mode 1)    ;;显示行号
(setq display-line-numbers-width-start t)  ; 动态调整行号宽度
(setq display-line-numbers-grow-only t)    ; 行号宽度只增不减（减少抖动）
(tool-bar-mode -1)    ;;关闭工具栏
(menu-bar-mode -1)    ;;关闭菜单栏
(setq make-backup-files nil)    ;;关闭自动备份
(setq-default cursor-type 'bar)    ;;更改光标样式
(scroll-bar-mode -1)    ;;关闭文件滑动控件
(show-paren-mode t)    ;;括号匹配
(electric-pair-mode t)    ;;括号补全
(add-hook 'prog-mode-hook 'prettify-symbols-mode)    ;;会将lambda等符号美化为λ
(delete-selection-mode 1)    ;;全选加强
(setq ring-bell-function 'ignore)    ;;关掉哔哔声
(fset 'yes-or-no-p 'y-or-n-p)    ;;快速yes或no
(setq-default indent-tabs-mode nil)    ;;启用tab键
(setq-default tab-width 4)    ;;设置tab宽度
(global-hl-line-mode t)    ;;高亮当前行
(setq initial-frame-alist
      '((width . 135)
      (height . 40)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))


(provide 'basic)
