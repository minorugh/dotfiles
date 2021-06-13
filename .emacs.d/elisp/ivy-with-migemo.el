;;; ivy-with-migemo.el --- Use ivy/counsel/swiper with migemo.

;; Copyright (C) 2021 ballforest

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; In the past, many people used avy-migemo to support migemo in ivy and
;; counsel. However, avy-migemo has not been maintained for a long time, and
;; many attempts have been made to achieve migemo support without using
;; avy-migemo. I decided to write a new minor-mode to make commands with
;; ivy-based interface compatible with migemo.

;;; Installation:
;;
;; To use this package, add following code to your init file.
;;
;; (require 'ivy-with-migemo)
;; (global-ivy-with-migemo-mode +1)
;;
;; You can customize ivy-with-migemo-enable-command which is a list of commands
;; to use migemo-mode. For example,
;;
;; (setq ivy-with-migemo-enable-command
;;       '(swiper swiper-isearch counsel-find-file))

;;; Code:

(require 'ivy)
(require 'migemo)

(defgroup ivy-with-migemo nil
  "Ivy with migemo."
  :group 'ivy
  :prefix "ivy-with-migemo-")

(defcustom ivy-with-migemo-lighter " IWM" ; Ivy-With-Migemo
  "Lighter for `ivy-with-migemo-mode'."
  :type '(choice (const :tag "Not displayed." nil)
                 string)
  :group 'ivy-with-migemo)

(defcustom ivy-with-migemo-enable-command
  '(swiper swiper-isearch)
  "Commands to use migemo-mode."
  :type 'list
  :group 'ivy-with-migemo)

(defun ivy-with-migemo-get-pattern-shyly (word)
  (replace-regexp-in-string
   "\\\\("
   "\\\\(?:"
   (migemo-get-pattern word)))

(defun ivy-with-migemo--regex-migemo-pattern (word)
  (cond
   ((string-match "\\(.*\\)\\(\\[[^\0]+\\]\\)"  word)
    (concat (ivy-with-migemo-get-pattern-shyly (match-string 1 word))
            (match-string 2 word)))
   ((string-match "\\`\\\\([^\0]*\\\\)\\'" word)
    (match-string 0 word))
   (t
    (ivy-with-migemo-get-pattern-shyly word))))

(defun ivy-with-migemo--regex-migemo (str)
  (when (string-match-p "\\(?:[^\\]\\|^\\)\\\\\\'" str)
    (setq str (substring str 0 -1)))
  (setq str (ivy--trim-trailing-re str))
  (cdr (let ((subs (ivy--split str)))
         (if (= (length subs) 1)
             (cons
              (setq ivy--subexps 0)
              (if (string-match-p "\\`\\.[^.]" (car subs))
                  (concat "\\." (ivy-with-migemo--regex-migemo-pattern
                                 (substring (car subs) 1)))
                (ivy-with-migemo--regex-migemo-pattern (car subs))))
           (cons
            (setq ivy--subexps (length subs))
            (replace-regexp-in-string
             "\\.\\*\\??\\\\( "
             "\\( "
             (mapconcat
              (lambda (x)
                (if (string-match-p "\\`\\\\([^?][^\0]*\\\\)\\'" x)
                    x
                  (format "\\(%s\\)" (ivy-with-migemo--regex-migemo-pattern x))))
              subs
              ".*?")
             nil t))))))

(defun ivy-with-migemo--regex-migemo-plus (str)
  (cl-letf (((symbol-function 'ivy--regex) #'ivy-with-migemo--regex-migemo))
    (ivy--regex-plus str)))

(defvar ivy-with-migemo--search-default-mode-backup nil)
(defvar ivy-with-migemo--ivy-re-builders-alist-backup nil)

;;;###autoload
(define-minor-mode ivy-with-migemo-mode
  "Enable migemo under commands with ivy interface."
  :group      'ivy-with-migemo
  :init-value nil
  :global nil
  :lighter ivy-with-migemo-lighter
  (if ivy-with-migemo-mode
      (progn
        ;; enable
        (if (eq ivy-with-migemo--ivy-re-builders-alist-backup nil)
            (setq ivy-with-migemo--ivy-re-builders-alist-backup
                  (copy-alist ivy-re-builders-alist)))
        (if (and (eq ivy-with-migemo--search-default-mode-backup nil)
                 (not (eq search-default-mode nil)))
            (setq ivy-with-migemo--search-default-mode-backup
                  search-default-mode))
        (mapc (lambda (command)
                (setf (alist-get command ivy-re-builders-alist)
                      #'ivy-with-migemo--regex-migemo-plus))
              ivy-with-migemo-enable-command)
        (setq search-default-mode nil))

    ;; disable
    (setq ivy-re-builders-alist
          (copy-alist ivy-with-migemo--ivy-re-builders-alist-backup))
    (setq search-default-mode ivy-with-migemo--search-default-mode-backup)))

(defun ivy-with-migemo--turn-on ()
  (ivy-with-migemo-mode +1))

;;;###autoload
(define-globalized-minor-mode global-ivy-with-migemo-mode
  ivy-with-migemo-mode ivy-with-migemo--turn-on
  :group 'ivy-with-migemo)

(provide 'ivy-with-migemo)

;;; ivy-with-migemo.el ends here
