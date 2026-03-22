;;; deepl-translate.el --- Deepl translate configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(require 'request)

(defvar deepl-auth-key) ; この変数にdeeplから発行されるキーを設定する
(defvar deepl-confirmation-threshold 3000)
(defvar deepl-endpoint "api-free.deepl.com") ; 無料版は api-free.deepl.com

(cl-defun confirm-send-long-string (&key retry)
  (let ((send-it-p
         (read-from-minibuffer
          (if retry
              "Please answer with \"yes\" or \"no\". [yes/no]: "
            (format "It's over %S characters, do you really want to send it? [yes/no]: "
                    deepl-confirmation-threshold)))))
    (cond ((equal send-it-p "yes") t)
          ((equal send-it-p "no") nil)
          (t (confirm-send-long-string :retry t)))))

;; 2026-03-10 DeepL API 仕様変更に対応
;; 認証方式を POST ボディ (auth_key) から Authorization ヘッダーに変更
(cl-defun deepl-translate-internal (text source-lang target-lang success-callback)
  (when (and (> (length text) deepl-confirmation-threshold)
             (not (confirm-send-long-string)))
    (cl-return-from deepl-translate-internal))

  (request (format "https://%s/v2/translate" deepl-endpoint)
    :type "POST"
    :headers `(("Authorization" . ,(format "DeepL-Auth-Key %s" deepl-auth-key)))
    :data `(("text" . ,text)
            ("source_lang" . ,source-lang)
            ("target_lang" . ,target-lang))
    :parser 'json-read
    :success success-callback))

(cl-defun deepl--output-to-messages (&key data &allow-other-keys)
  (let ((translated-text (cdr (assoc 'text (aref (cdr (assoc 'translations data)) 0)))))
    (setq inhibit-message nil)
    (kill-new translated-text)
    (message translated-text)))

(defun deepl-ej (start end)
  (interactive "r")
  (let ((region (buffer-substring start end)))
    (deepl-translate-internal region "EN" "JA" #'deepl--output-to-messages)))

(defun deepl-je (start end)
  (interactive "r")
  (let ((region (buffer-substring start end)))
    (deepl-translate-internal region "JA" "EN" #'deepl--output-to-messages)))

(defun ja-char-p (char)
  (or (<= #x3041 char #x309f) ; hiragana
      (<= #x30a1 char #x30ff) ; katakana
      (<= #x4e01 char #x9faf) ; kanji
      ))

(defun ja-string-p (str)
  (>= (cl-count-if #'ja-char-p str) 3))

;;;###autoload
(defun deepl-translate (start end)
  (interactive "r")
  (let ((region (buffer-substring start end)))
    (if (ja-string-p region)
        (deepl-translate-internal region "JA" "EN" #'deepl--output-to-messages)
      (deepl-translate-internal region "EN" "JA" #'deepl--output-to-messages))))


(provide 'deepl-translate)
;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; byte-compile-warnings: (not free-vars)
;; End:
;;; deepl-translate.el ends here
