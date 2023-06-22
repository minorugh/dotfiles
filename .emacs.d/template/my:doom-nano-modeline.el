;;; my:doom-nano-modeline.el --- User custom configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:
;; (setq debug-on-error t)

;; -----------------------------------------------------------------------------------------
;;
;; Change modeline display format
;;
;; -----------------------------------------------------------------------------------------

(defun doom-nano-modeline-buffer-name-vc-and-major-mode ()
  "Return the buffer name and the major mode."
  (let* ((buffer-name (cond
                       ((and (derived-mode-p 'org-mode)
                             (buffer-narrowed-p)
                             (buffer-base-buffer))
                        (format"%s [%s]" (buffer-base-buffer)
                               (org-link-display-format
                                (substring-no-properties (or (org-get-heading 'no-tags)
                                                             "-")))))
                       ((and (buffer-narrowed-p)
                             (buffer-base-buffer))
                        (format"%s [narrow]" (buffer-base-buffer)))
                       (t
                        (format-mode-line "%b"))))

         (buffer-modified (if (and buffer-file-name (buffer-modified-p)) "** " ""))

         (mode-name (format-mode-line mode-name))

         (vc-branch-name (doom-nano-modeline--get-vc-branch))

         (vc-branch (if vc-branch-name
                        `((vc-branch-name . nil))
                      nil)))

    `((,(concat buffer-modified buffer-name) . nil)
      (" " . nil)
      ;; (,(if vc-branch-name (concat "[" vc-branch-name "]") "") . doom-nano-modeline-vc-branch-name-face)
      ;; (,(if vc-branch-name " " "") . nil)
      ;; (,(concat "(" mode-name ")") . doom-nano-modeline-major-mode-face))))
	  (,(concat "(" mode-name "") . doom-nano-modeline-major-mode-face)
	  (,(if vc-branch-name (concat ", " vc-branch-name ")") "") . doom-nano-modeline-vc-branch-name-face)
	  (,(if vc-branch-name " " ")") . doom-nano-modeline-vc-branch-name-face))))


;; -----------------------------------------------------------------------------------------
;;
;; Custom Set Faces
;;
;; -----------------------------------------------------------------------------------------

(custom-set-faces
 '(region                                    ((t (:background "#6272a4"))))
 '(hl-line                                   ((t (:background "#3B4252" :extend t ))))
 '(doom-nano-modeline-active-face            ((t (:foreground "#f8f8f2" :background "#44475a" :weight bold))))
 '(doom-nano-modeline-cursor-position-face   ((t (:foreground "#b0b8d1" :background "#44475a"))))
 '(doom-nano-modeline-evil-emacs-state-face  ((t (:foreground "#ffa07a" :background "#6272a4" :weight bold))))
 '(doom-nano-modeline-evil-normal-state-face ((t (:foreground "#adff2f" :background "#4682b4" :weight bold))))
 '(doom-nano-modeline-evil-visual-state-face ((t (:foreground "#e0ffff" :background "#6495ed" :weight bold))))
 '(doom-nano-modeline-major-mode-face        ((t (:foreground "#b0b8d1" :background "#44475a"))))
 '(doom-nano-modeline-vc-branch-name-face    ((t (:foreground "#b0b8d1" :background "#44475a")))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; my:doom-nano-modeline.el ends here
