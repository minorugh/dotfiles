;;; my-csv-mode.el --- CSV/TSV column alignment only  -*- lexical-binding: t; -*-

(defvar-local my-csv-separator ?,)

(defvar my-csv-font-lock-keywords
  '(("^#.*" . font-lock-comment-face)))

(defun my-csv--parse-line ()
  (let ((eol (line-end-position))
        (beg (point))
        (inq nil)
        (fields nil))
    (while (< (point) eol)
      (let ((c (char-after)))
        (cond ((eq c ?\") (setq inq (not inq)))
              ((and (eq c my-csv-separator) (not inq))
               (push (cons beg (point)) fields)
               (setq beg (1+ (point))))))
      (forward-char 1))
    (push (cons beg eol) fields)
    (nreverse fields)))

(defun my-csv-align ()
  (interactive)
  (let ((rows nil)
        (widths nil))
    (remove-overlays (point-min) (point-max) 'my-csv t)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (unless (or (eolp) (eq (char-after) ?#))
          (let ((fields (my-csv--parse-line))
                (i 0))
            (push fields rows)
            (dolist (f fields)
              (let ((w (string-width (buffer-substring-no-properties (car f) (cdr f)))))
                (if (< i (length widths))
                    (setf (nth i widths) (max w (nth i widths)))
                  (setq widths (append widths (list w)))))
              (setq i (1+ i)))))
        (forward-line 1)))
    (dolist (fields rows)
      (let ((col 0)
            (i 0))
        (dolist (f (butlast fields))
          (setq col (+ col (nth i widths) 2))
          (setq i (1+ i))
          (let ((ov (make-overlay (cdr f) (1+ (cdr f)))))
            (overlay-put ov 'my-csv t)
            (if (eq my-csv-separator ?\t)
                (overlay-put ov 'display `(space :align-to ,col))
              (overlay-put ov 'after-string
                           (propertize " " 'display `(space :align-to ,col))))))))))

(defun my-csv-setup ()
  (add-hook 'after-save-hook #'my-csv-align nil t)
  (my-csv-align))

;;;###autoload
(define-derived-mode my-csv-mode prog-mode "CSV"
  "Minimal major mode for CSV column alignment."
  (setq-local font-lock-defaults '(my-csv-font-lock-keywords t))
  (setq-local truncate-lines t))

;;;###autoload
(define-derived-mode my-tsv-mode my-csv-mode "TSV"
  "Minimal major mode for TSV column alignment."
  (setq-local my-csv-separator ?\t))

(add-hook 'my-csv-mode-hook #'my-csv-setup)

(provide 'my-csv-mode)
;;; my-csv-mode.el ends here
