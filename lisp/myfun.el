(defun my-update-time ()
  "设置modeline时间显示"
  (setq display-time-format "%Y年%m月%d日 %H:%M:%S %A")
  (display-time-mode 1))
(run-with-timer 0 1 'my-update-time)




(provide 'myfun)
