;;; my-dired-oermission-help.el --- Evil keybinding cheat sheet -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;; *Permission Help* を右サイドバーに固定表示
;; ----------------------------------------------------------
(add-to-list 'display-buffer-alist
             '("\\*Permission Help\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 40)
               (window-parameters . ((no-delete-other-windows . t)
                                     (mode-line-format . none)))))

(defun my-dired-permission-help ()
  "Show a quick permission reference."
  (interactive)
  (let ((help-window-select t))   ;; 表示後にヘルプウィンドウへフォーカスを移す
    (with-help-window "*Permission Help*"
      (princ "Permission Quick Reference\n")
      (princ "==========================\n\n")

      (princ "Permission values\n")
      (princ "-----------------\n")
      (princ "r = 4\n")
      (princ "w = 2\n")
      (princ "x = 1\n\n")

      (princ "Common combinations\n")
      (princ "-------------------\n")
      (princ "rwx = 7\n")
      (princ "rw- = 6\n")
      (princ "r-x = 5\n")
      (princ "r-- = 4\n")
      (princ "--- = 0\n\n")

      (princ "Common permissions\n")
      (princ "------------------\n")
      (princ "-rw-r--r--  = 644\n")
      (princ "-rw-------  = 600\n")
      (princ "-rw----r--  = 604\n")
      (princ "-rw----rw-  = 606\n")
      (princ "-rwxr-xr-x  = 755\n")
      (princ "-rwx------  = 700\n")
      (princ "-rwx---r-x  = 705\n\n")

      (princ "File type (first character)\n")
      (princ "---------------------------\n")
      (princ "-  regular file\n")
      (princ "d  directory\n")
      (princ "l  symbolic link\n")
      (princ "c  character device\n")
      (princ "b  block device\n")
      (princ "p  FIFO (named pipe)\n")
      (princ "s  socket\n\n")

      (princ "Examples\n")
      (princ "--------\n")
      (princ "drwxr-xr-x\n")
      (princ " d   rwx   r-x   r-x\n")
      (princ " |    7     5     5\n")
      (princ " +--> directory\n")
      (princ " => directory 755\n"))))

(provide 'my-dired-permission-help)
;;; my-dired-permission-help.el ends here

