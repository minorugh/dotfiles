;;; 05-ivy-tools.el --- My Ivy-based tools -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Custom Ivy-based commands: project switcher, imenu
;;
;;; Code:

;; ─────────────────────────────────────────
;; Git プロジェクト切り替え
;; ─────────────────────────────────────────
(leaf my-switch-project
  :init
  (defcustom my-git-project-search-dirs
    '("~/projects" "~/work" "~/src")
    "Git プロジェクトを探すトップレベルディレクトリのリスト。"
    :type '(repeat directory)
    :group 'my)
  (defcustom my-git-project-search-depth 3
    "サブディレクトリを何段まで検索するか。"
    :type 'integer
    :group 'my)
  :config
  (require 'ivy)

  (defun my-find-git-projects ()
    (let (projects)
      (dolist (root my-git-project-search-dirs)
        (let ((expanded (expand-file-name root)))
          (when (file-directory-p expanded)
            (let* ((cmd (format
                         "find %s -maxdepth %d -name .git -type d 2>/dev/null"
                         (shell-quote-argument expanded) my-git-project-search-depth))
                   (raw (shell-command-to-string cmd)))
              (dolist (git-dir (split-string raw "\n" t))
                (push (file-name-directory git-dir) projects))))))
      (sort (delete-dups projects) #'string<)))

  (defun my-ivy-switch-git-project--transformer (s)
    (replace-regexp-in-string
     (concat "^" (regexp-quote (expand-file-name "~")) "/")
     "~/" s))

  (ivy-set-display-transformer
   'my-ivy-switch-git-project
   #'my-ivy-switch-git-project--transformer)

  (defun my-ivy-switch-git-project ()
    (interactive)
    (let ((projects (my-find-git-projects)))
      (if (null projects)
          (message "Git プロジェクトが見つかりません。`my-git-project-search-dirs` を確認してください。")
        (ivy-read "Git project: " projects
                  :action (lambda (dir)
                            (setq default-directory dir)
                            (message "移動しました: %s" dir)
                            (dired dir))
                  :caller 'my-ivy-switch-git-project))))

  (global-set-key (kbd "C-c p p") #'my-ivy-switch-git-project))

;; ─────────────────────────────────────────
;; imenu
;; ─────────────────────────────────────────
(leaf my-ivy-imenu
  :config
  (require 'ivy)
  (require 'imenu)

  (defun my-ivy-imenu--collect ()
    "imenu インデックスをフラットな候補リストに変換する。"
    (let ((index (imenu--make-index-alist t))
          (cands nil))
      (cl-labels
          ((flatten (alist prefix)
             (dolist (item alist)
               (cond
                ((imenu--subalist-p item)
                 (flatten (cdr item)
                          (concat prefix
                                  (propertize (car item)
                                              'face 'font-lock-type-face)
                                  "/")))
                ((and (cdr item) (not (equal (car item) "*Rescan*")))
                 (let* ((category-fmt (if (string= prefix "")
                                         ""
                                       (concat prefix " ")))
                        (name-fmt (propertize (car item)
                                              'face 'font-lock-function-name-face))
                        (display (concat category-fmt name-fmt)))
                   (push (cons display (cdr item)) cands)))))))
        (flatten index ""))
      (nreverse cands)))

  (defun my-ivy-imenu--goto (x)
    "候補 X の位置にジャンプして recenter する。"
    (let ((pos (cdr x)))
      (when (overlayp pos) (setq pos (overlay-start pos)))
      (when (markerp pos) (setq pos (marker-position pos)))
      (when pos
        (goto-char pos)
        (recenter))))

  (defun my-ivy-imenu ()
    "Ivy でバッファの imenu を ↑↓ プレビュー付きで表示する。"
    (interactive)
    (let* ((orig-buf   (current-buffer))
           (orig-point (point))
           (cands      (my-ivy-imenu--collect))
           (map        (copy-keymap ivy-minibuffer-map)))
      (if (not cands)
          (message "imenu エントリが見つかりません。")
        (define-key map (kbd "<down>") 'ivy-next-line-and-call)
        (define-key map (kbd "<up>")   'ivy-previous-line-and-call)
        (ivy-read "imenu: " cands
                  :keymap map
                  :action (lambda (x)
                            (with-current-buffer orig-buf
                              (my-ivy-imenu--goto x)))
                  :unwind (lambda ()
                            (unless (eq ivy-exit 'done)
                              (switch-to-buffer orig-buf)
                              (goto-char orig-point)
                              (recenter)))
                  :require-match t
                  :caller 'my-ivy-imenu)))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 05-ivy-tools.el ends here
