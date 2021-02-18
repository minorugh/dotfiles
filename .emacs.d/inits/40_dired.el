;;; 40_dired.el --- Dired settings  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf dired
  :hook (dired-mode-hook . dired-my-append-buffer-name-hint)
  :config
  (bind-key "<left>" 'dired-up-alternate-directory dired-mode-map)
  (bind-key "<right>" 'dired-open-in-accordance-with-situation dired-mode-map)
  (bind-key "RET" 'dired-open-in-accordance-with-situation dired-mode-map)
  (bind-key "SPC" 'my:dired-toggle-mark dired-mode-map)
  (bind-key "C-g" 'my:dired-unmark-all dired-mode-map)
  (bind-key "f" 'counsel-find-file dired-mode-map)
  (bind-key "r" 'wdired-change-to-wdired-mode dired-mode-map)
  (bind-key "o" 'dired-open-file dired-mode-map)
  (bind-key "[" 'dired-hide-details-mode dired-mode-map)
  (bind-key "a" 'toggle-dired-listing-switches dired-mode-map)
  (bind-key "q" 'dired-dwim-quit-window dired-mode-map)
  (bind-key "t" 'counsel-tramp dired-mode-map)
  (bind-key "i" 'call-sxiv dired-mode-map)
  (bind-key "s" 'sudo-edit dired-mode-map)
  (bind-key "e" 'gedit-sudo-open dired-mode-map)
  (bind-key "." 'magit-status dired-mode-map)
  (bind-key "<" 'beginning-of-buffer dired-mode-map)
  (bind-key ">" 'end-of-buffer dired-mode-map)
  ;; Use dired as 2-screen filer
  (setq dired-dwim-target t)
  ;; Always to perform the delete/copy of directories recursively
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-listing-switches "-lgGhF")
  :init
  (leaf sudo-edit :ensure t)
  (leaf wdired :require t)
  (leaf dired-x :require t
    :config
    (setq dired-omit-mode t)
	(setq dired-omit-files "^\\desktop.ini"))
  ;; Show directory first
  (leaf ls-lisp
    :require t
    :config
    (setq ls-lisp-use-insert-directory-program nil ls-lisp-dirs-first t))

  ;; cus functions
  (defun toggle-dired-listing-switches ()
    "Toggle `dired-mode' switch between with and without 'A' option to show or hide dot files."
    (interactive)
    (progn
      (if (string-match "[Aa]" dired-listing-switches)
		  (setq dired-listing-switches "-lgGhF")
		(setq dired-listing-switches "-lgGhFA"))
	  (reload-current-dired-buffer)))

  (defun reload-current-dired-buffer ()
    "Reload current `dired-mode' buffer."
    (let* ((dir (dired-current-directory)))
      (progn (kill-buffer (current-buffer))
			 (dired dir))))

  (defun dired-my-append-buffer-name-hint ()
    "Append a auxiliary string [Dir] to a name of dired buffer."
    (when (eq major-mode 'dired-mode)
      (let* ((dir (expand-file-name list-buffers-directory))
			 ;; Add a drive letter for Windows
			 (drive (if (and (eq 'system-type 'windows-nt)
							 (string-match "^\\([a-zA-Z]:\\)/" dir))
						(match-string 1 dir) "")))
		(rename-buffer (concat (buffer-name) " [" drive "dir]") t))))

  (defun dired-dwim-quit-window ()
    "`quit-window 'according to screen division."
    (interactive)
    (quit-window (not (delq (selected-window) (get-buffer-window-list)))))

  ;; http://nishikawasasaki.hatenablog.com/entry/20120222/1329932699
  (defun dired-open-in-accordance-with-situation ()
    "Files are opened in separate buffers, directories are opened in the same buffer."
    (interactive)
    (let ((file (dired-get-filename)))
      (if (file-directory-p file)
		  (dired-find-alternate-file)
		(dired-find-file))))

  (defun dired-up-alternate-directory ()
    "Move to higher directory without make new buffer."
    (interactive)
    (let* ((dir (dired-current-directory))
		   (up (file-name-directory (directory-file-name dir))))
      (or (dired-goto-file (directory-file-name dir))
		  ;; Only try dired-goto-subdir if buffer has more than one dir.
		  (and (cdr dired-subdir-alist)
			   (dired-goto-subdir up))
		  (progn
			(find-alternate-file up)
			(dired-goto-file dir)))))

  ;; https://www.emacswiki.org/emacs/OperatingOnFilesInDired
  (defun dired-open-file ()
    "In dired, open the file in associated application."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (unless (getenv "WSLENV")
		(call-process "xdg-open" nil 0 nil file))
      ;; use wsl-utils:https://github.com/smzht/wsl-utils
      (when (getenv "WSLENV")
		(call-process "wslstart" nil 0 nil file))))

  (defun gedit-sudo-open ()
	"Get file and open gedit with sudo."
	(interactive)
	(let ((file (dired-get-filename nil t)))
	  (compile (concat "sudo gedit " file))))

  (defun my:dired-toggle-mark (arg)
    "Toggle the current next files."
    (interactive "p")
    (let ((dired-marker-char
		   (if (save-excursion (beginning-of-line)
							   (looking-at " "))
			   dired-marker-char ?\040)))
      (dired-mark arg)))

  (defun my:dired-unmark-all ()
	"Dired unmark all."
	(interactive)
	(call-interactively 'dired-unmark-all-marks)
	(call-interactively 'revert-buffer))

  (defun call-sxiv ()
	"Call sxiv for all imags in current dir."
	(interactive)
	(let ((image-files ;; List of image file names
		   (delq nil   ;; Is this area replaced with a filter macro around emacs26?
				 (mapcar
				  (lambda (f)
					(when (string-match
						   "\.\\(jpe?g\\|png\\|gif\\|bmp\\)$"
						   f )
					  f ))
				  (directory-files default-directory) )))) ;; List of filenames in the current directory
	  (start-process-shell-command ;; Asynchronous execution of shell commands
	   "sxiv"
	   nil	;; Do not open buffer for process
	   (format "sxiv -f -n %s %s"
			   (length image-files) ;; Number of image files = last image file
			   (mapconcat 'identity image-files " ") )))) ;; Concatenate lists separated by spaces

  ;; <enter> イメージモード と サムネールモード を切り替え

  ;; サムネールモードで左ダブルクリック　→　イメージモードへ
  ;; イメージモードで右クリック　→　サムネールモードへ

  ;; イメージモード
  ;; <n> 	n 	次の画像を表示する 	<space>
  ;; <n> 	p 	前の画像を表示する 	<backspace>
  ;; g 	最初のイメージへジャンプする
  ;; <n> 	G 	最後のイメージ あるいは <n>番めのイメージへジャンプする
  ;; + 	ズームイン
  ;; - 	ズームアウト
  ;; <n> 	= 	100％ あるいは <n>％ の大きさにする
  ;; W 	ウィンドウの大きさに合わせる
  ;; q 	終了
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 40_dired.el ends here
