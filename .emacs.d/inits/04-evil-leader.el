;;; 04-evil-leader.el --- Evil leader configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf evil-leader :ensure t
  :doc "Free keymap on evil-mode."
  :hook (after-init-hook . global-evil-leader-mode)
  :config
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "0" 'delete-window
    "1" 'delete-other-windows
    "2" 'split-window-below
    "3" 'split-window-right
    "m" 'make-frame
    "n" 'neomutt
    "_" 'other-frame
    "/" 'delete-frame
    "w" 'window-swap-states
    "o" 'other-window-or-split
    "[" 'previous-buffer
    "]" 'next-buffer
    "l" 'recenter-top-bottom
    "h" 'hydra-diff/body
    "j" 'evil-join-whitespace
    "g" 'my-google-this
    ":" 'thunar-open
    "f" 'flycheck-list-errors
    "," 'org-capture
    "." 'thunderbird
    "?" 'vim-cheat-sheet
    "c" 'org-capture
    "q" 'keyboard-quit
    "SPC" 'avy-goto-word-1)
  :init
  (defun vim-cheat-sheet ()
    "View vim cheat sheet online."
    (interactive)
    (browse-url "https://minorugh.github.io/vim-cheat/vim-cheat-sheet.html"))

  (defun thunderbird ()
    "Open thunderbird mail-client for Gmail."
    (interactive)
    (start-process "thunderbird" nil "thunderbird"))

  (defun neomutt ()
    "Open terminal and ssh to xsrv."
    (interactive)
    (start-process-shell-command "neomutt" nil "neomutt.sh"))
  (setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))

  (defun mattermost ()
    "Open mattermost-desktop."
    (interactive)
    (start-process "mattermost" nil "mattermost-desktop")))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 04-evil-leader.el ends here
