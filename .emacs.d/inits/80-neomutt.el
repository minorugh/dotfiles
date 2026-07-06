;;; 80-neomutt.el --- Neomuttt configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(define-derived-mode neomutt-mail-mode text-mode "NeoMutt"
  "Major mode for editing NeoMutt compose buffers via emacsclient.")
(add-to-list 'auto-mode-alist '("/neomutt-" . neomutt-mail-mode))

(leaf my-neomutt
  :doc "NeoMutt integration with emacsclient."
  :hook ((server-visit-hook . my-neomutt-setup)
         (server-done-hook  . my-neomutt-server-done))
  :bind (("C-x C-c" . server-edit))  ; same as C-x #
  :init
  (defun my-neomutt-setup ()
    "Prepare a NeoMutt compose buffer."
    (when (and buffer-file-name
               (string-match-p "neomutt" buffer-file-name))
      (neomutt-mail-mode)))

  (defun my-neomutt-server-done ()
    "Run after `server-edit' (C-x C-c) finishes."
    (when (derived-mode-p 'neomutt-mail-mode)
      (when (bound-and-true-p darkroom-mode)
        (my-darkroom-out))
      (let ((buf (current-buffer))
            (frame (selected-frame)))
        (kill-buffer buf)
        (when (frame-live-p frame)
          (iconify-frame frame))))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 80-neomutt.el ends here
