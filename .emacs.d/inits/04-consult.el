;;; 04-consult.el --- Consult/Vertico config. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Completion framework: vertico / orderless / marginalia / consult
;; Replaces: counsel / ivy-rich / swiper / amx
;;
;; Note: ivy package itself is retained for git-peek.el and my-makefile.el,
;; which call ivy-read directly.  ivy-mode is intentionally NOT enabled.
;;
;;; Code:
;; (setq debug-on-error t)

;;; ============================================================
;;; 1. vertico  — ミニバッファ候補表示
;;; ============================================================
(leaf vertico
  :ensure t
  :doc "Vertical completion UI for the default completion system."
  :hook (after-init-hook . vertico-mode)
  :config
  (setq vertico-cycle t)
  (setq vertico-count 10)
  (setq enable-recursive-minibuffers t)
  (custom-set-faces
   `(vertico-current ((t (:background ,(doom-color 'base4)
			      :foreground ,(doom-color 'fg)
			      :weight bold))))))

;;; ============================================================
;;; 2. orderless — スペース区切りで複数パターンを AND 検索
;;; ============================================================
(leaf orderless
  :ensure t
  :doc "Completion style for space-separated patterns."
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides
        '((file (styles basic partial-completion))))
  (setq orderless-matching-styles
        '(orderless-literal orderless-regexp)))

;;; ============================================================
;;; 3. marginalia — 候補の横にアノテーション表示 (ivy-rich の代替)
;;; ============================================================
(leaf marginalia
  :ensure t
  :doc "Annotate completion candidates (replaces ivy-rich)."
  :hook (after-init-hook . marginalia-mode))

;;; ============================================================
;;; 4. consult — 強化補完コマンド群 (counsel の代替)
;;; ============================================================
(leaf consult
  :ensure t
  :doc "Consulting completing-read (replaces counsel)."
  :commands (consult-buffer consult-line consult-git-grep
	     consult-mark consult-yank-pop)
  :bind (("C-x C-f" . find-file)
	 ("C-x b"   . consult-project-buffer)
         ("C-x f"   . project-find-file)
	 ("C-x m"   . consult-mark)
	 ("M-g f"   . consult-flymake)
	 ("M-g g"   . consult-goto-line)
	 ("M-g o"   . consult-outline)
	 ("M-g i"   . consult-imenu)
         ("M-y"     . consult-yank-pop)
         ("C-:"     . consult-buffer)
         ("C-s"     . consult-line-region)
         ("s-a"     . consult-git-grep)
         ("s-s"     . consult-line-thing-at-point))
  :config
  ;; swiper-thing-at-point 相当
  (defun consult-line-thing-at-point ()
    "Call `consult-line' with the symbol at point as initial input."
    (interactive)
    (consult-line (thing-at-point 'symbol)))

  ;; swiper-region 相当
  (defun consult-line-region ()
    "Call `consult-line' with the selected region as initial input,
or plain consult-line."
    (interactive)
    (consult-line
     (when (use-region-p)
       (buffer-substring-no-properties (region-beginning) (region-end))))))


(defvar my-describe-history nil "Variable to store history of `describe-command'.")

(defun my-describe-command ()
  "A definitive version of Ivy with learning features enabled."
  (interactive)
  (let ((cands nil))
    (mapatoms
     (lambda (s)
       (when (commandp s)
         (let* ((name (symbol-name s))
                (key (where-is-internal s nil t))
                (key-desc (if key (key-description key) "")))
           (push (format "%-40s %s" name key-desc) cands)))))
    (ivy-read "Command: " cands
              :action (lambda (x)
                        (describe-function (intern (car (split-string x)))))
              :require-match t
              :history 'my-describe-history
              :caller 'my-describe-command)))

(defun my-describe-variable ()
  "Describe-variable complete with variable refinement."
  (interactive)
  (let ((cands nil))
    (mapatoms
     (lambda (s)
       (when (boundp s)
         (push (symbol-name s) cands))))
    (ivy-read "Variable: " (sort cands #'string<)
              :action (lambda (x) (describe-variable (intern x)))
              :require-match t)))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 04-consult.el ends here
