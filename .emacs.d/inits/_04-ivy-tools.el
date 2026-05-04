;;; 04-ivy-tools.el --- My Ivy-based describe tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf ivy
  :ensure t
  :hook (after-init-hook . ivy-mode)
  :chord (("df" . my-describe-command)
          ("fg" . my-describe-variable))
  :bind (:ivy-minibuffer-map
         ("<down>" . ivy-next-line-and-call)
         ("<up>"   . ivy-previous-line-and-call))
  :config
  (setq ivy-use-virtual-buffers      t)
  (setq ivy-use-selectable-prompt    t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-extra-directories        nil)

  (defvar my-describe-history nil "Variable to store history of describe-command.")

  (defun my-describe-command ()
    "Search and describe commands by keybinding or name using Ivy."
    (interactive)
    (let ((cands nil))
      (mapatoms
       (lambda (s)
         (when (commandp s)
           (let* ((name (symbol-name s))
                  (key (where-is-internal s nil t))
                  (key-desc (if key (key-description key) "")))
             (push (cons (format "%-18s %s" key-desc name) s) cands)))))
      (ivy-read "Find: " cands
                :action (lambda (x) (describe-function (cdr x)))
                :initial-input "^"
                :require-match t
                :history 'my-describe-history
                :caller 'my-describe-command)))

  (defun my-describe-variable ()
    "Search and describe variables using Ivy."
    (interactive)
    (let ((cands nil))
      (mapatoms (lambda (s) (when (boundp s) (push (symbol-name s) cands))))
      (ivy-read "Variable: " (sort cands #'string<)
                :action (lambda (x) (describe-variable (intern x)))
                :require-match t))))

;; ─────────────────────────────────────────
;; Git Project Switcher
;;
;; Provides `my-ivy-switch-git-project', an Ivy-based command to
;; jump to any Git project directory.
;;
;; How it works:
;;   1. Searches each directory listed in `my-git-project-search-dirs'
;;      up to `my-git-project-search-depth' levels deep, collecting
;;      every entry named ".git" (both directories and gitfiles).
;;   2. Applies `my-git-project-alias-map': any project whose real
;;      .git lives outside Dropbox (to avoid sync conflicts) is
;;      replaced by its Dropbox counterpart, which holds the actual
;;      data.  The canonical .git path is also excluded from the list
;;      so it does not appear twice.
;;   3. Presents the deduplicated, sorted list via Ivy and opens the
;;      chosen directory in Dired.
;; ─────────────────────────────────────────
(leaf ivy-git-project
  :vc (:url "https://github.com/minorugh/ivy-git-project")
  :bind ("C-x p" . ivy-git-project-switch)
  :config
  (setq ivy-git-project-search-dirs
        '("~/src/github.com/minorugh/" "~/Dropbox/GH/" "~/Dropbox/minorugh.com/" "~/.env_source/"))
  (setq ivy-git-project-search-depth 3)
  (setq ivy-git-project-alias-map
        '(("~/src/github.com/minorugh/GH/"          . "~/Dropbox/GH/")
          ("~/src/github.com/minorugh/minorugh.com/" . "~/Dropbox/minorugh.com/"))))

;; (leaf my-switch-project
;;   :after project ivy
;;   :config
;;   (defcustom my-git-project-search-dirs
;;     '("~/src/github.com/minorugh/" "~/Dropbox/GH/" "~/Dropbox/minorugh.com/" "~/.env_source/")
;;     "A list of top-level directories to look for Git projects."
;;     :type '(repeat directory)
;;     :group 'my)

;;   (defcustom my-git-project-search-depth 3
;;     "サブディレクトリを何段まで検索するか。"
;;     :type 'integer
;;     :group 'my)

;;   (defcustom my-git-project-alias-map
;;     '(("~/src/github.com/minorugh/GH/"           . "~/Dropbox/GH/")
;;       ("~/src/github.com/minorugh/minorugh.com/" . "~/Dropbox/minorugh.com/"))
;;     "実体 .git を持つパス → 実際に使いたいパスへのエイリアス対応表。"
;;     :type '(alist :key-type directory :value-type directory)
;;     :group 'my)

;;   (defun my--git-project-alias-keys ()
;;     "エイリアスマップのキー（除外対象）を展開済みリストで返す。"
;;     (mapcar (lambda (pair) (expand-file-name (car pair)))
;;             my-git-project-alias-map))

;;   (defun my--git-project-apply-alias (dir)
;;     "DIR がエイリアスマップのキーと一致すれば値に置換、なければ DIR をそのまま返す。"
;;     (let ((hit (cl-assoc (expand-file-name dir)
;;                          my-git-project-alias-map
;;                          :test (lambda (a b) (string= a (expand-file-name b))))))
;;       (if hit (expand-file-name (cdr hit)) dir)))

;;   (defun my-find-git-projects ()
;;     (let (projects
;;           (exclude-keys (my--git-project-alias-keys)))
;;       (dolist (root my-git-project-search-dirs)
;;         (let ((expanded (expand-file-name root)))
;;           (when (file-directory-p expanded)
;;             (let* ((cmd (format
;;                          "find %s -maxdepth %d -name .git \\( -type d -o -type f \\) 2>/dev/null"
;;                          (shell-quote-argument expanded) my-git-project-search-depth))
;;                    (raw (shell-command-to-string cmd)))
;;               (dolist (git-dir (split-string raw "\n" t))
;;                 (let ((proj (file-name-directory git-dir)))
;;                   (unless (member (expand-file-name proj) exclude-keys)
;;                     (push (my--git-project-apply-alias proj) projects))))))))
;;       (sort (delete-dups projects) #'string<)))

;;   (defun my-ivy-switch-git-project--transformer (s)
;;     (replace-regexp-in-string
;;      (concat "^" (regexp-quote (expand-file-name "~")) "/")
;;      "~/" s))

;;   (ivy-set-display-transformer
;;    'my-ivy-switch-git-project
;;    #'my-ivy-switch-git-project--transformer)

;;   (defun my-ivy-switch-git-project ()
;;     (interactive)
;;     (let ((projects (my-find-git-projects)))
;;       (if (null projects)
;;           (message "Git プロジェクトが見つかりません。`my-git-project-search-dirs` を確認してください。")
;;         (ivy-read "Git project: " projects
;;                   :action (lambda (dir)
;;                             (setq default-directory dir)
;;                             (message "移動しました: %s" dir)
;;                             (dired dir))
;;                   :caller 'my-ivy-switch-git-project)))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 04-ivy-tools.el ends here
