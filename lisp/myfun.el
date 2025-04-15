(defun my-update-time ()
  "设置modeline时间显示"
  (setq display-time-format "%Y年%m月%d日 %H:%M:%S %A")
  (setq display-time-default-load-average nil)
  (display-time-mode 1))
(run-with-timer 0 1 'my-update-time)

(defun ascii ()
  "展示ascii码表"
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (setq buffer-read-only nil)        ;; Not need to edit the content, just read mode (added)
  (local-set-key "q" 'bury-buffer)   ;; Nice to have the option to bury the buffer (added)
  (setq lower32 '("nul" "soh" "stx" "etx" "eot" "enq" "ack" "bel"
  		          "bs" "ht" "nl" "vt" "np" "cr" "so" "si"
  		          "dle" "dc1" "dc2" "dc3" "dc4" "nak" "syn" "etb"
  		          "can" "em" "sub" "esc" "fs" "gs" "rs" "us"
  		          ))
  (save-excursion (let ((i -1))
                    (insert "ASCII characters 0 thru 127.\n\n")
                    (insert " Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char\n")
                    (while (< i 31)
                      (insert (format "%4x %4d %4s | %4x %4d %4s | %4x %4d %4s | %4x %4d %4s\n"
                                      (setq i (+ 1  i)) i (elt lower32 i)
                                      (setq i (+ 32 i)) i (single-key-description i)
                                      (setq i (+ 32 i)) i (single-key-description i)
                                      (setq i (+ 32 i)) i (single-key-description i)))
                      (setq i (- i 96))))))

(defun auto-hexl ()
  "自动查看二进制文件"
  (add-to-list 'auto-mode-alist '("\\.bin\\'" . hexl-mode))
  (add-to-list 'auto-mode-alist '("\\.img\\'" . hexl-mode)))
(auto-hexl)

(defun nasm-compile ()
  "自动使用 make 编译汇编"
  (interactive)
  (unless (file-exists-p "Makefile")
    (error "No Makefile found in current directory"))
  (compile "make bochs"))





(provide 'myfun)
