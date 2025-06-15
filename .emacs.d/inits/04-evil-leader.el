;;; 04-evil-leader.el --- Evil mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf evil-leader :ensure t
  :doc "Free keymap on evil-mode"
  :defun evil-leader/set-leader evil-emacs-state ad:switch-to-buffer
  :hook (after-init-hook . global-evil-leader-mode)
  :config
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "0" 'delete-window
    "1" 'delete-other-windows
    "2" 'split-window-below
    "3" 'split-window-right
    "n" 'make-frame
    "_" 'other-frame
    "/" 'delete-frame
    "S" 'window-swap-states
    "o" 'other-window-or-split
    "[" 'previous-buffer
    "]" 'next-buffer
    "l" 'recenter-top-bottom
    "h" 'hydra-diff/body
    "j" 'evil-join-whitespace
    "g" 'my:google-this
    "s" 'swiper-thing-at-point
    ":" 'counsel-switch-buffer
    "f" 'flymake-show-buffer-diagnostics
    "," 'org-capture
    "." 'terminal-open
    "?" 'vim-cheat-sheet
    "q" 'keyboard-quit
    "SPC" 'avy-goto-word-1)
  :hydra
  (hydra-diff
   (:color red :hint nil)
   "
    diff-hl-hunk  prev.next:_[_._]_  _s_how"
   ("]" diff-hl-next-hunk)
   ("[" diff-hl-previous-hunk)
   ("g" diff-hl-diff-goto-hunk)
   ("r" diff-hl-revert-hunk)
   ("s" diff-hl-show-hunk)
   ("<muhenkan>" nil))
  :init
  (defun vim-cheat-sheet ()
    "View vim cheat sheet online."
    (interactive)
    (browse-url "https://minorugh.github.io/vim-cheat/vim-cheat-sheet.html"))

  (defun ad:switch-to-buffer (&rest _arg)
    "Set buffer for automatic `evil-insert-state'."
    (when (member (buffer-name) '("COMMIT_EDITMSG"))
      (evil-emacs-state)))
  (advice-add 'switch-to-buffer :after #'ad:switch-to-buffer))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 04-evil-leader.el ends here
