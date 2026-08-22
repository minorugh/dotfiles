;;; 40-dired.el --- Dired configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; ============================================================
;;  Dired Core
;; ============================================================

(leaf dired
  :preface
  (autoload 'my-open-tig "my-tig-bridge" nil t)
  (autoload 'my-dired-permission-help "my-dired-permission-help" nil t)
  :hook ((dired-mode-hook . my-dired-omit-mode)
         (dired-mode-hook . my-dired-env-warning-face))
  :bind (:dired-mode-map
         ("<left>"   . my-dired-up)
         ("<right>"  . my-dired-open)
         ("RET" . my-dired-open)
         ("w"   . wdired-change-to-wdired-mode)
         ("a"   . dired-omit-mode)
         ("s"   . my-dired-sudo-rm)
         ("o"   . my-dired-open-xdg)
         ("v"   . my-dired-open-vim)
         ("n"   . my-dired-open-nano)
         ("["   . dired-hide-details-mode)
         ("t"   . my-open-tig)
         ("p"   . my-dired-permission-help)
         ("."   . xsrv-deploy-dired)    ; see 90-xsrv-deploy.el
         (","   . xsrv-download-dired)  ; see 90-xsrv-deploy.el
         ("i"   . my-sxiv))
  :config
  (setq global-auto-revert-non-file-buffers t)
  (setq dired-dwim-target         t)
  (setq dired-recursive-copies   'always)
  (setq dired-recursive-deletes  'always)
  (setq dired-listing-switches   "-AlhF --group-directories-first --no-group")
  (setq dired-omit-files         "^\\.$\\|^\\.[^\\.].*$\\|\\.elc$")
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Don't let Ivy override Emacs 30 file-name completion in dired copy.
  (add-to-list 'ivy-completing-read-handlers-alist
               '(dired-do-copy . completing-read-default)))


;; ============================================================
;;  Dired Extensions  (omit / navigation / file-ops / external / help)
;; ============================================================

(leaf *my-dired-extensions
  :after dired
  :config
  ;;  Omit Mode  (特定ディレクトリでは omit を無効化)
  ;; ----------------------------------------------------------
  (defun my-dired-omit-mode ()
    "Disable `dired-omit-mode' in specific directories; enable elsewhere."
    (let ((current (file-name-as-directory
                    (expand-file-name default-directory))))
      (dired-omit-mode
       (if (member current
                   (mapcar (lambda (d)
                             (file-name-as-directory (expand-file-name d)))
                           '("~/"
                             "~/.env_source/"
                             "~/src/github.com/minorugh/GH/"
                             "~/src/github.com/minorugh/minorugh.com/"
                             "~/src/github.com/minorugh/dotfiles/"
                             "~/src/github.com/minorugh/dotfiles/env/")))
           -1
         1))))

  ;;  Env Window Warning Face  (窓であることを視覚的に警告)
  ;; ----------------------------------------------------------
  (defun my-dired-env-warning-face ()
    "Set a background color in Dired buffers under `~/.env_source' or `dotfiles/env/'.
These directories hold the secrets repository and its bindfs-mounted
window, so the color distinguishes them from ordinary working directories."
    (let ((current (file-name-as-directory (expand-file-name default-directory))))
      (when (or (string-prefix-p (expand-file-name "~/.env_source/") current)
                (string-prefix-p (expand-file-name "~/src/github.com/minorugh/dotfiles/env/") current))
        (buffer-face-set '(:background "#3a1a1a")))))

  ;;  Navigation
  ;; ----------------------------------------------------------
  (defun my-dired-open ()
    "Open file or directory at point."
    (interactive)
    (let ((file (dired-get-filename)))
      (if (file-directory-p file)
          (find-alternate-file file)
        (find-file file))))

  (defun my-dired-up ()
    "Go to parent directory in the same buffer."
    (interactive)
    (find-alternate-file ".."))

  (defun my-dired-open-xdg ()
    "Open file at point with its associated application via xdg-open."
    (interactive)
    (let ((file (dired-get-filename nil t)))
      (call-process "xdg-open" nil 0 nil file)))

  (defun my-dired-open-vim ()
    "Open the file at point in gnome-terminal with Vim.
Placed on the external monitor when one is connected."
    (interactive)
    (let ((default-directory (dired-current-directory)))
      ;; my-launch-gnome-terminal は 07-functions.el で定義(外部モニター配置対応)
      (my-launch-gnome-terminal "--" "vim" (dired-get-file-for-visit))))

  (defun my-dired-open-nano ()
    "Open the file at point in a maximized gnome-terminal with GNU nano.
Placed on the external monitor when one is connected."
    (interactive)
    (let ((default-directory (dired-current-directory)))
      ;; my-launch-gnome-terminal は 07-functions.el で定義(外部モニター配置対応)
      (my-launch-gnome-terminal "--" "nano" (dired-get-file-for-visit))))

  ;;  File Operations
  ;; ----------------------------------------------------------
  (defun my-dired-sudo-rm ()
    "Delete dired-marked files with sudo."
    (interactive)
    (let ((files (dired-get-marked-files)))
      (when (y-or-n-p "Delete marked files with sudo?")
        (dolist (file files)
          (let ((result (call-process "sudo" nil t nil "rm" "-rf" file)))
            (unless (zerop result)
              (message "sudo rm failed for: %s" file))))
        (revert-buffer))))

  ;;  External Tools
  ;; ----------------------------------------------------------
  (defun my-dired-run-script ()
    "Run the executable script at point in an external gnome-terminal.
Intended for interactive scripts (e.g. check-backup.sh) that prompt
for input or launch vim/emacsclient, which don't work well inside
Emacs's own shell-command output.
Placed on the external monitor when one is connected."
    (interactive)
    (let ((default-directory (dired-current-directory)))
      ;; my-launch-gnome-terminal は 07-functions.el で定義(外部モニター配置対応)
      (my-launch-gnome-terminal "--" "bash" (dired-get-file-for-visit))))

  (defun my-sxiv ()
    "Open all images in the current directory with sxiv (fullscreen tiling)."
    (interactive)
    (let* ((files (directory-files default-directory nil
                                   "\\.\\(jpe?g\\|png\\|gif\\|bmp\\)$"))
           (cmd (format "sxiv -t -f %s"
                        (mapconcat #'shell-quote-argument files " "))))
      (start-process-shell-command "sxiv" nil cmd))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 40-dired.el ends here
